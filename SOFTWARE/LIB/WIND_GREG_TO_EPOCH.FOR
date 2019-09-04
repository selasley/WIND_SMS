      SUBROUTINE WIND_GREG_TO_EPOCH( YEAR, DOY, MSEC, MICRO, EPOCH, STATUS )
C
C	This version:   20-APR-1995
C
C	This subroutine converts integer numbers representing Year, DayOfYear,
C	Milliseconds, and Microseconds into a REAL*8 EPOCH number.
C
C   USAGE:
C	CALL WIND_GREG_TO_EPOCH( YEAR, DOY, MSEC, MICRO, EPOCH, STATUS )
C
C   PARAMETERS:
C	NAME	TYPE	USE	Description
C	----	----	---	-----------
C	YEAR	I*4	 I	year, e.g.:1994, neg. = B.C.
C	DOY	I*4	 I	day-of-year
C	MSEC	I*4	 I	millisecond of day
C	MICRO	I*4	 I	microsecond of millisecond
C	EPOCH	R*8	 O	CDF Epoch format, milliseconds since 1-Jan-0001 B.C.
C				to be compatible with GEOTAIL
C	STATUS	I*4	 O	Status of conversion, 1 = good
C
C   FUNCTION OR SUBROUTINE CALLS:
C
C   CREATION:
C	 5-APR-1994	H.BROER	University of Maryland, SPACE PHYSICS GROUP
C
C Modification history:
C	19-APR-1995	L. Bleau	change code to use the CDF routine
C					compute_EPOCH, removed calls to
C					CNVRT_YRMONDAY_TO_JULIAN,
C					CNVRT_TO_FLOAT_JULIAN, and
C					WIND_JULIAN_TO_EPOCH
C	20-APR-1995     J.PAQUETTE      added call to CNVRT_MILLIS_TO_HRMNSMS
C                                       neccesary since msec input to CDF 
C                                       routine must be 0 < msec < 999
C
C NOTE:
C
C This is a change from the earlier version of WIND_GREG_TO_EPOCH in that
C previously all the work was done in the above-listed home-grown routines. 
C Now it is done mainly by the CDF routine compute_EPOCH.  This eliminates the
C 12 hour discrepancy between the CDF Epoch times and the Epoch times
C previously calculated by this routine.
C
C In addition, the routine CNVRT_TO_FLOAT_JULIAN subtracted 12 hours from the
C computed (floating point) Julian Day.  This was to reflect the fact that
C a Julian Day, by definition, starts at noon.  The "Truncated Julian Day" used
C in the WIND Level Zero file starts at midnight, however.  This had resulted 
C in the Epoch times produced by WIND_GREG_TO_EPOCH being 1/2 day less than the 
C correct value.  This, too, has been fixed.  WIND_GREG_TO_EPOCH now produces a 
C value which matches the CDF Epoch time value for the same day.
C 
C
      IMPLICIT NONE

      REAL*8	EPOCH		! TIME IN MILLISECONDS SINCE 1-JAN-0001 B.C.

      INTEGER*4	YEAR,DOY,MSEC,MICRO,STATUS
C
      STATUS	= 1				! GOOD STATUS
C
C In the call below, MSEC is milliseconds of day.  compute_EPOCH handles this
C correctly if the hour, minute, and second arguments are all zero.
C
      CALL compute_EPOCH( YEAR, 0, DOY, 0, 0, 0, MSEC, EPOCH )

      IF (EPOCH .EQ. -1) THEN
	STATUS = 0
	RETURN
      ENDIF

      EPOCH	= EPOCH + DFLOAT(MICRO) / 1D3

      RETURN

      END
