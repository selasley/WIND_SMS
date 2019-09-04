      SUBROUTINE JULIAN_TO_GREGORIAN( JUL, IYY, IMM, IDY, IHR, IMIN, ISEC, IMILLI, IMICRO )

C	In this routine JULIAN_TO_GREGORIAN the input Julian Day Number causes
C	the return of the calender date expressed in year IYY, month IMM, and
C	day IDY, on which the Julian Day started at noon with all variables in
C	INTEGER*4.  The fractional part converts to hours, minutes to micros.
C
C	Note that this routine will not run on systems with 2-byte integers.
C
C	Programmed after the sample in "NUMERICAL RECIPES IN PASCAL" by
C	William H. Press and others.
C
C	USAGE:
C	CALL JULIAN_TO_GREGORIAN( JUL, IYY, IMM, IDY,
C				  IHR, IMIN, ISEC, IMILLI, IMICRO )
C
C	REAL*8		JUL	! INPUT: JULIAN DAY	range 0. -> 1075.E+6
C
C	INTEGER*4	IYY	! OUT:	YEAR		e.g.: 1994, neg = BC
C			IMM	!	MONTH		range 1 -> 12
C			IDY	!	DAY		range 1 -> 31
C			IHR	!	HOUR		range 0 -> 23
C			IMIN	!	MINUTE		range 0 -> 59
C			ISEC	!	SECONDS		range 0 -> 59
C			IMILLI	!	MILLISECONDS	range 0 -> 999
C			IMICRO	!	MICROSECONDS	range 0 -> 999
C		
C	Creation:
C	21-Mar-1994	H.Broer	UMD, Space Physics Group	
C
C	Revision:
C	22-Mar-1994	H.Broer 	include calculation of hour,min,s,ms,us
C	25-Mar-1994	H.Broer		change all Floating numbers to REAL*8
C	19-APR-1995	L.Bleau		remove addition of 12 hours; see notes
C
C NOTE:
C
C This routine originally added 12 hours to the (floating point) Julian Day
C before decomposing it into its calendrical parts.  This was to reflect the
C fact that a Julian Day, by definition, starts at noon.  It also corrected for
C the 12 hours subtracted by the routine CNVRT_TO_FLOAT_JULIAN (see comments in
C WIND_GREG_TO_EPOCH.FOR).  The "Truncated Julian Day" used in the WIND Level 
C Zero file starts at midnight, however.  Since CNVRT_TO_FLOAT_JULIAN has been 
C changed to not add 12 hours the corresponding change must be made here also.  
C This, too, has been fixed.  JULIAN_TO_GREGORIAN now produces a value which 
C correctly reflects the date and time of the given Epoch value.
C
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

      F		= JUL
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
