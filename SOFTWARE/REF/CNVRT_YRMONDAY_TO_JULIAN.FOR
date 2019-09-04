      INTEGER*4 FUNCTION CNVRT_YRMONDAY_TO_JULIAN( IYY, IMM, IDY )
!
!	In this routine JULDAY returns the Julian Day Number which begins at
!	noon of the calender date specified by month IMM, day IDY, and year IYY,
!	all INTEGER*4 variables. Positive year signifies A.D.; negative B.C.
!	Remember thet the year after 1 B.C. was 1 A.D.  Note that this routine
!	will not run on systems with 2-byte integers.
!
!	Programmed after the sample in "NUMERICAL RECIPES IN PASCAL" by
!	William H. Press and others.
!
!	Version:	29-Mar_1994
!
!	USAGE:
!	JJ	= CNVRT_YRMONDAY_TO_JULIAN( IYY, IMM, IDY )
!
!	PARAMETERS:
!	INTEGER*4	JJ	! OUTPUT: Integer JULIAN day
!
!	INTEGER*4	IYY	! INPUT:Year	pos=A.D., neg=B.C., no zero
!			IMM	!	Month
!			IDY	!	Day
! 
!	Creation:
!	22-Mar-1994	H.Broer	UMD, Space Physics Group	
!
!	Revision:
!	25-Mar-1994	H.Broer	Correct for BC-years from Julian day 60.
!	29-Mar-1994	H.Broer Correct last day of Julian calender
!

      IMPLICIT NONE

      INTEGER*4	IGREG,		! variable
     :		IGREG1/588829/,	! Gregorian Calender was adopted on 15-OCT-1582
     :		IGREG2/588818/,	! the day before was 4-OCT-1582
     :		IDY,		! Input:	DAY OF MONTH
     :		IMM,		!		MONTH OF YEAR
     :		IYY,		!		YEAR
     :		JA,		!
     :		JM,		!
     :		JY,		!
     :		JUL

1000  IF( IYY .EQ. 0 ) THEN
          TYPE 100, ' THERE IS NO YEAR 0.'
  100 FORMAT( 1H , ' ***** ', A, ' ***** ' )
          TYPE 110, ' PLEASE ENTER YEAR;?'
  110 FORMAT( 1H , A, $ )
          ACCEPT 120, IYY
  120 FORMAT( I10 )
          GOTO 1000
      ENDIF

      IF( IYY .LT. 0 )	IYY = IYY + 1

      IF( IMM .GT. 2 )	THEN
          JY	= IYY
          JM	= IMM + 1
      ELSE
          JY	= IYY - 1
          JM	= IMM + 13
      ENDIF

      JUL	= INT( 365.25D0 * ( JY + 4712 ) )
     :		+ INT( 30.6001D0 * JM )
     :		+ IDY
     :		- 63

      IGREG	= IDY + 31*( IMM + 12*IYY )
      IF( IGREG .GE. IGREG1 )   THEN
          JA	= INT( 0.01 * JY )
          JUL	= JUL + 2 - JA + INT( 0.25 * JA )
      ELSE
          IF( IGREG .GT. IGREG2 )	JUL = 2299160
      ENDIF

      CNVRT_YRMONDAY_TO_JULIAN	= JUL

      RETURN

      END
