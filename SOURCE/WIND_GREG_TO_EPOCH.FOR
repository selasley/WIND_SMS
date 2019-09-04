      SUBROUTINE WIND_GREG_TO_EPOCH( YEAR, DOY, MSEC, MICRO, EPOCH, STATUS )
!
!	This version:	5-APR-1994
!
!	This subroutine converts integer numbers representing Year, DayOfYear,
!	Milliseconds, and Microseconds into a REAL*8 EPOCH number.
!
!   USAGE:
!	CALL WIND_GREG_TO_EPOCH( YEAR, DOY, MSEC, MICRO, EPOCH, STATUS )
!
!   PARAMETERS:
!	NAME	TYPE	USE	Description
!	----	----	---	-----------
!	YEAR	I*4	 I	year, e.g.:1994, neg. = B.C.
!	DOY	I*4	 I	day-of-year
!	MSEC	I*4	 I	millisecond of day
!	MICRO	I*4	 I	microsecond of millisecond
!	EPOCH	R*8	 O	CDF Epoch format, milliseconds since 3-Jan-0001 B.C.
!					to be compatible with GEOTAIL
!	STATUS	I*4	 O	Status of conversion, 1 = good = SS$_NORMAL
!
!   FUNCTION OR SUBROUTINE CALLS:
!
!   CREATION:
!	 5-APR-1994	H.BROER	University of Maryland, SPACE PHYSICS GROUP
!
!
      IMPLICIT NONE

      INCLUDE	'($SSDEF)/NOLIST'

      REAL*8	EPOCH,		! TIME IN MILLISECONDS SINCE 3-JAN-0001 B.C.
     :		RTEMP,		! internal
     :		STEMP,		! internal
     :		CNVRT_TO_FLOAT_JULIAN

      INTEGER*4	YEAR,		!
     :		DOY,		!
     :		MSEC,		!
     :		MICRO,		!
     :		STATUS,		!
     :		IMON,		!
     :		IDAY,		!
     :		IHR,		!
     :		IMIN,		!
     :		ISEC,		!
     :		IMILLI,		!
     :		IMICRO,		!
     :		ITEMP,		!
     :		CNVRT_YRMONDAY_TO_JULIAN

      STATUS	= SS$_NORMAL			! GOOD STATUS

      CALL CNVRT_YRDY_TO_YRMNDY(YEAR,
     :				DOY,
     :				IMON,
     :				IDAY )

      CALL CNVRT_MILLIS_TO_HRMNSMS( MSEC,
     :				IHR,
     :				IMIN,
     :				ISEC,
     :				IMILLI )

      ITEMP	= CNVRT_YRMONDAY_TO_JULIAN( YEAR,
     :				IMON,
     :				IDAY )	

      RTEMP	= CNVRT_TO_FLOAT_JULIAN( ITEMP,
     :				IHR,
     :				IMIN,
     :				ISEC,
     :				IMILLI,
     :				MICRO )

      CALL WIND_JULIAN_TO_EPOCH(STEMP,
     :				RTEMP,
     :				STATUS )

      EPOCH	= STEMP

      RETURN

      END
