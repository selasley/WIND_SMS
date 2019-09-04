      SUBROUTINE WIND_EPOCH_TO_GREG( YEAR, DOY, MSEC, MICRO, EPOCH, STATUS )
C
C	This version:	19-APR-1995
C
C	This subroutine converts into integer numbers representing Year,
C	DayOfYear, Milliseconds, and Microseconds from a REAL*8 EPOCH number.
C
C   USAGE:
C	CALL WIND_EPOCH_TO_GREG( YEAR, DOY, MSEC, MICRO, EPOCH, STATUS )
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
C					EPOCH_breakdown, removed calls to
C					WIND_EPOCH_TO_JULIAN and
C					JULIAN_TO_GREGORIAN
C
C NOTE:
C
C This is a change from the earlier version of WIND_EPOCH_TO_GREG in that
C previously all the work was done in the above-listed home-grown routines. 
C Now it is done mainly by the CDF routine EPOCH_breakdown.  This eliminates
C the 12 hour discrepancy between the CDF Epoch times and the Epoch times.
C
C In addition, the routine WIND_EPOCH_TO_JULIAN added 12 hours to the time
C before determining the Julian Day.  This was to reflect the fact that
C a Julian Day, by definition, starts at noon.  It also corrected for the 12
C hours subtracted by the routine CNVRT_TO_FLOAT_JULIAN (see comments in
C WIND_GREG_TO_EPOCH.FOR).  The "Truncated Julian Day" used in the WIND Level 
C Zero file starts at midnight, however.  Since WIND_GREG_TO_EPOCH has been 
C changed to not add 12 hours the corresponding change must be made here also.  
C This, too, has been fixed.  WIND_EPOCH_TO_GREG now produces a value which 
C correctly reflects the date and time of the given Epoch value.
C
C
      IMPLICIT NONE

      REAL*8	EPOCH		! TIME IN MILLISECONDS SINCE 1-JAN-0001 B.C.

      INTEGER*4	YEAR,DOY,MSEC,MICRO,STATUS
      INTEGER*4	IMON,IDAY,IHR,IMIN,ISEC,IMILLI
C
      STATUS	= 1				! GOOD STATUS

      CALL EPOCH_breakdown( EPOCH, YEAR, IMON, IDAY, IHR, IMIN, ISEC, IMILLI )
      MICRO	= 0

      CALL CNVRT_YRMONDAY_TO_YEARDAY( YEAR, IMON, IDAY, DOY )	

      CALL CNVRT_HRMNSMS_TO_MILLIS( MSEC, IHR, IMIN, ISEC, IMILLI )

      RETURN

      END
