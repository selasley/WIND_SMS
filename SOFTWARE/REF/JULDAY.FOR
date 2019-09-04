      INTEGER*4 FUNCTION JULDAY( IYY, IMM, IDY )

C	In this routine JULDAY returns the Julian Day Number which begins at
C	noon of the calender date specified by month IMM, day IDY, and year IYY,
C	all INTEGER*4 variables. Positive year signifies A.D.; negative B.C.
C	Remember thet the year after 1 B.C. was 1 A.D.  Note that this routine
C	will not run on systems with 2-byte integers.
C
C	Programmed after the sample in "NUMERICAL RECIPES IN PASCAL" by
C	William H. Press and others.
C
C	Creation:
C	22-Mar-1994	H.Broer	UMD, Space Physics Group	
C
C	Revision:
C	24-Mar-1994	H.Broer	Corrections made for BC-years to Julian day 366.
C	 9-FEB-1995	L. Bleau	changed '!' to 'C' in program comments
C

      IMPLICIT NONE

      INTEGER*4	IGREG/588829/,	! Gregorian Calender was adopted on 15-OCT-1582
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

      IF( IDY + 31*( IMM + 12*IYY ) .GE. IGREG )   THEN
          JA	= INT( 0.01 * JY )
          JUL	= JUL + 2 - JA + INT( 0.25 * JA )
      ENDIF

      JULDAY	= JUL

      RETURN

      END
