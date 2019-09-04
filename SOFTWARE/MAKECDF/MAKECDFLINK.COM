
$CDFDIR = F$EXTRACT(0,F$LENGTH(P1)-1,P1)
$ if p2 .eqs. "LINK" then goto linkit
$cc/noopt/include='CDFDIR'.SRC.INCLUDE] makeCDF
$cc/noopt/include='CDFDIR'.SRC.INCLUDE] makeCDF2
$cc/noopt/include='CDFDIR'.SRC.INCLUDE] makeCDF3
$linkit:
$link makeCDF,makeCDF2,makeCDF3,-
            'CDFDIR'.src.tools]toolbox1,-
            'CDFDIR'.src.tools]toolbox3,-
            'CDFDIR'.src.lib]libcdf/lib
