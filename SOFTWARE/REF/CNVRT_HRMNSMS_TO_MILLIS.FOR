      SUBROUTINE CNVRT_HRMNSMS_TO_MILLIS( MILLI, IHR, IMIN, ISEC, MSEC )
!
!	This version:	5-APR-1994
!
!	This subroutine converts integer numbers representing Hour, Minutes,
!	Seconds, and Milliseconds to an integer number of milliseconds of day.
!
!   USAGE:
!	CALL SUBROUTINE CNVRT_HRMNSMS_TO_MILLIS( MILLI, IHR, IMIN, ISEC, MSEC )
!
!   PARAMETERS:
!	NAME	TYPE	USE	Description
!	----	----	---	-----------
!	MILLI	I*4	 O	Milliseconds of day
!	IHR	I*4	 I	hour of day
!	IMIN	I*4	 I	minute of hour
!	ISEC	I*4	 I	seconds of minute
!	MSEC	I*4	 I	millisecond of second
!
!   FUNCTION OR SUBROUTINE CALLS:
!
!   CREATION:
!	 5-APR-1994	H.BROER	University of Maryland, SPACE PHYSICS GROUP
!
!
      IMPLICIT NONE

      INTEGER*4	IHR,		!
     :		IMIN,		!
     :		ISEC,		!
     :		MILLI,		!
     :		MSEC		!

      MILLI	= ( ( IHR * 60 + IMIN ) * 60 + ISEC ) * 1000 + MSEC

      RETURN

      END
