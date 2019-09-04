$ cdfdir = f$trnlnm("CDF$HOME")
$ if cdfdir .eqs. ""
$ then
$   cdfdir = f$extract(0,f$length(p1)-1,p1)
$ else
$   cdfdir = cdfdir - ".]" - "]"
$ endif
$
$ CC/NOOPT/INCLUDE='cdfdir'.SRC.INCLUDE] MAKESFDU
$ LINK/NOUSERLIBRARY MAKESFDU,-
      'cdfdir'.SRC.TOOLS]TOOLBOX1,-
      'cdfdir'.SRC.TOOLS]TOOLBOX3,-
      'cdfdir'.SRC.LIB]LIBCDF/LIB,SYS$LIBRARY:VAXCRTL/LIB
