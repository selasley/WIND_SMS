$! File name  :  COMPILE_WIND
$! Purpose    :  Compile source code for WIND library
$!
$! Version :	 25-MAR-94
$!
$ ON ERROR THEN GOTO EXIT
$!
$ SET DEF SRC_DIR
$!
$ first_time := T
$!
$! Check to see if only a single routine is to be compiled
$! Compile everything
$!
$ module_list = -
                  "JULIAN_TO_GREGORIAN," + -
                  "CNVRT_TO_FLOAT_JULIAN," + -
                  "CNVRT_YRMONDAY_TO_JULIAN"
$ compile_any := F
$ target = p1
$ i = 0
$loop:
$     module_name = f$element(i,",",module_list)
$! write sys$output "module ''i' = ''module_name'"
$     if module_name .eqs. "," then goto endloop
$     if module_name .eqs. target .or. target .eqs. ""
$     then
$         if first_time then WRITE SYS$OUTPUT "Compiling for WIND_LIB"
$         call for_compile 'module_name'
$         compile_any := T
$         first_time := F
$     endif
$     i = i + 1
$     goto loop
$endloop:
$ if compile_any then exit
$ write sys$output -
         "Only the following modules are allowed:"
$ i = 0
$loop2:
$     module_name = f$element(i,",",module_list)
$     if module_name .eqs. "," then goto endloop2
$     write sys$output "    ",module_name
$     i = i + 1
$     goto loop2
$endloop2:
$exit:
$ set def COM_DIR
$ exit
$
$!========================================================================
$!			FOR_COMPILE Subroutine
$!
$FOR_COMPILE: SUBROUTINE
$!
$ ON WARNING THEN EXIT
$ IF P1.EQS."" THEN INQUIRE P1 FILE
$ SET DEF OBJ_DIR
$ WRITE SYS$OUTPUT "Compiling ",''P1'
$ FOR SRC_DIR:'P1'.FOR -
			/Warning=(GEN, DECLAR) -
			/NoOpt -
			/I4 -
			/NoD_Lines -
			/CHECK=ALL/SHOW=ALL -
			/Extend
$ LIB/REPLACE LIB_DIR:WIND_LIB.OLB 'P1'.OBJ
$ WRITE SYS$OUTPUT "*** ", P1, " included in WIND_LIB ***"
$ DEL 'P1'.OBJ;*
$!
$ EXIT
$ ENDSUBROUTINE		!End of FOR_COMPILE
