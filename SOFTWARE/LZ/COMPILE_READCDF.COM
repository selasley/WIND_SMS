$! File name  :  FORL.COM
$! Purpose    :  Compile source code for TEST
$!
$! Version :	 29-JUL-93
$!
$ ON ERROR THEN GOTO EXIT
$!
$ SET DEF SRC_DIR
$!
$! Compile everything
$ WRITE SYS$OUTPUT "Compiling"
$ CALL FOR_COMPILE 'P1'
$!
$EXIT:
$ SET DEF COM_DIR
$ EXIT
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
                        /LIST -
			/NoOpt -
			/I4 -
			/NoD_Lines -
			/CHECK=ALL/SHOW=ALL -
			/Extend
$ EXIT
