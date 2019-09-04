$! FILE NAME:	MAKE_DATE.COM
$! PURPOSE :	Create and compile DATE.FOR to insert a TIME stamp
$!
$! This version:    7-NOV-1994
$!
$! Created :
$!	21-JUN-1994	H.BROER	University of Maryland, Physics Department,
$!				Space Physics Group
$!
$! Revision :
$!	27-JUN-1994	H.BROER   include WIND_OPEN_L1-file
$!	30-JUN-1994	H.BROER   include WIND_STORE_L1-file
$!	18-OCT-1994	L.Bleau	  added code to put DATA stmt in DATE.FOR to
$!			          initialize TIME_STAMP; changed TIME_STAMP to
$!			          CHAR*23 
$!       4-NOV-1994     L.Bleau   changed KB to TTOUT in WRITE statements
$!	 7-NOV-1994	L.Bleau	  removed LINK commands, renamed to MAKE_DATE
$!      18-JAN-1995     L.Bleau   use p1 instead of hardcoding DATE as filename
$!       6-FEB-1995     L.Bleau   check and delete previous versions of DATE.FOR
$!                                and DATE.OBJ independently to eliminate error msg
$!
$     ON ERROR THEN  GOTO EXIT
$!
$ if p1 .eqs. "" then p1 := DATE
$ if f$search("''p1'.FOR") .nes. "" then DELETE 'p1'.FOR;*
$ if f$search("''p1'.OBJ") .nes. "" then DELETE 'p1'.OBJ;*
$!
$ OPEN/WRITE OUTFILE 'p1'.FOR
$ ASCTIM = F$TIME()
$ IF F$EXTRACT(1,1,ASCTIM) .EQS. "-" THEN ASCTIM = " " + ASCTIM
$ WRITE OUTFILE "       SUBROUTINE  TIM_STAMP"
$ WRITE OUTFILE " "
$ WRITE OUTFILE "       COMMON /TIME_STAMP/  TIME_STAMP
$ WRITE OUTFILE " "
$ WRITE OUTFILE "       INCLUDE	'INC_DIR:WIND_LUN.INC/NOLIST'"
$ WRITE OUTFILE " "
$ WRITE OUTFILE "       LOGICAL*4	A"
$ WRITE OUTFILE " "
$ WRITE OUTFILE "       CHARACTER*23	TIME_STAMP"
$ WRITE OUTFILE "	DATA TIME_STAMP/'",ASCTIM,"'/"
$ WRITE OUTFILE " "
$ WRITE OUTFILE "      WRITE( TTOUT, 100) '+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+'"
$ WRITE OUTFILE "      WRITE( TTOUT, 100) '|                                 |'"
$ WRITE OUTFILE "      WRITE( TTOUT, 100) '+          WIND_CD_READ           +'"
$ WRITE OUTFILE "      WRITE( TTOUT, 100) '|                                 |'"
$ WRITE OUTFILE "      WRITE( TTOUT, 100) '+             created             +'"
$ WRITE OUTFILE "      WRITE( TTOUT, 100) '|                                 |'"
$ WRITE OUTFILE "      WRITE( TTOUT, 100) '+     ",ASCTIM,"     +'"
$ WRITE OUTFILE "      WRITE( TTOUT, 100) '|                                 |'"
$ WRITE OUTFILE "      WRITE( TTOUT, 100) '+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+'"
$ WRITE OUTFILE "      WRITE( TTOUT, 120) 'IS THIS THE VERSION YOU WANT ?'"
$ WRITE OUTFILE "      A	= .TRUE."
$ WRITE OUTFILE "      CALL TILOG( A, *999 )"
$ WRITE OUTFILE "      IF( .NOT.A )   GO TO 999"
$ WRITE OUTFILE " 100  FORMAT( 1H , T22, A35 )"
$ WRITE OUTFILE " 120  FORMAT( 1H , A, $ )"
$ WRITE OUTFILE "      RETURN"
$ WRITE OUTFILE " "
$ WRITE OUTFILE " 999  STOP 999"
$ WRITE OUTFILE "      END"
$ CLOSE OUTFILE
$ 
$ FORTRAN 'p1'.FOR -
			/Warning=(GEN, DECLAR) -
!                        /LIST -
			/NoOpt -
			/I4 -
			/NoD_Lines -
			/CHECK=ALL/SHOW=ALL -
			/Extend
$!
$EXIT:
$    EXIT
