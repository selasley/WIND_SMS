      SUBROUTINE JULIAN_TO_GREGORIAN( JUL, IYY, IMM, IDY, IHR, IMIN, ISEC, IMILLI, IMICRO )

!	In this routine JULIAN_TO_GREGORIAN the input Julian Day Number causes
!	the return of the calender date expressed in year IYY, month IMM, and
!	day IDY, on which the Julian Day started at noon with all variables in
!	INTEGER*4.  The fractional part converts to hours, minutes to micros.
!
!	Note that this routine will not run on systems with 2-byte integers.
!
!	Programmed after the sample in "NUMERICAL RECIPES IN PASCAL" by
!	William H. Press and others.
!
!	USAGE:
!	CALL JULIAN_TO_GREGORIAN( JUL, IYY, IMM, IDY,
!				  IHR, IMIN, ISEC, IMILLI, IMICRO )
!
!	REAL*8		JUL	! INPUT: JULIAN DAY	range 0. -> 1075.E+6
!
!	INTEGER*4	IYY	! OUT:	YEAR		e.g.: 1994, neg = BC
!			IMM	!	MONTH		range 1 -> 12
!			IDY	!	DAY		range 1 -> 31
!			IHR	!	HOUR		range 0 -> 23
!			IMIN	!	MINUTE		range 0 -> 59
!			ISEC	!	SECONDS		range 0 -> 59
!			IMILLI	!	MILLISECONDS	range 0 -> 999
!			IMICRO	!	MICROSECONDS	range 0 -> 999
!		
!	Creation:
!	21-Mar-1994	H.Broer	UMD, Space Physics Group	
!
!	Revision:
!	22-Mar-1994	H.Broer 	include calculation of hour,min,s,ms,us
!	25-Mar-1994	H.Broer		change all Floating numbers to REAL*8
!
!
      IMPLICIT NONE

      REAL*8	JUL,		! Input:	Julian Day
     :		F		! Fraction of Julian Day

      INTEGER*4	IGREG/2299161/,	! INTERNAL CROSSOVER FOR CORRECTION
     :		IDY,		! Output:	DAY OF MONTH
     :		IMM,		!		MONTH OF YEAR
     :		IYY,		!		YEAR
     :		IHR,		!		HOURS
     :		IMIN,		!		MINUTES
     :		ISEC,		!		SECONDS
     :		IMILLI,		!		MILLISECONDS
     :		IMICRO,		!		MICROSECONDS
     :		JA,		!
     :		JB,		!
     :		JC,		!
     :		JD,		!
     :		JE,		!
     :		JALPHA,		!
     :		JM,		!
     :		JY,		!
     :		Z		!

      F		= JUL + 5D-1		! CHANGE FROM NOON TO NEXT MIDNIGHT
      Z		= INT( F )		! GET INTEGER PART OF JULIAN DAY NUMBER
      F		= F - Z			! GET FRACTIONAL PART OF JULIAN DAY

      IF( Z .GE. IGREG )	THEN
          JALPHA	= INT( ( Z - 1867216.25D0 ) / 36524.25D0 )
          JA		= Z + 1 + JALPHA - INT( 0.25D0 * JALPHA )
      ELSE
          JA		= Z
      END IF

      JB	= JA + 1524
      JC	= INT( ( JB - 122.1D0 ) / 365.25D0 )
      JD	= INT( 365.25D0 * JC )
      JE	= INT( ( JB - JD ) / 30.6001D0 )

      IDY	= JB - JD - INT( 30.6001D0 * JE )
      IMM	= JE - 1
      IF( IMM .GT. 12 )		IMM = IMM - 12
      IYY	= JC - 4715
      IF( IMM .GT. 2 )		IYY = IYY - 1
      IF( IYY .LE. 0 )		IYY = IYY - 1	! NO YEAR ZERO neither AD nor BC

!  Convert fraction to TIMES

      IHR	= INT( 24.D0 * F )		!	HOURS
      F		= 24.D0 * F - IHR
      IMIN	= INT( 6.D1 * F )		!	MINUTES
      F		= 6.D1 * F - IMIN
      ISEC	= INT( 6.D1 * F )		!	SECONDS
      F		= 6.D1 * F - ISEC
      IMILLI	= INT( 1.D3 * F )		!	MILLISECONDS
      F		= 1.D3 * F - IMILLI
      IMICRO	= INT( 1.D3 * F )		!	MICROSECONDS

      RETURN

      END
