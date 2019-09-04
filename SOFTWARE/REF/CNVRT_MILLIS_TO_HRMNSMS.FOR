      SUBROUTINE CNVRT_MILLIS_TO_HRMNSMS( MILLI, IHR, IMIN, ISEC, MSEC )
!
!	This version:	5-APR-1994
!
!	This subroutine converts to integer numbers representing Hour, Minutes,
!	Seconds, and Milliseconds from an integer number of milliseconds of day.
!
!   USAGE:
!	CALL CNVRT_MILLIS_TO_HRMNSMS( MILLI, IHR, IMIN, ISEC, MSEC )
!
!   PARAMETERS:
!	NAME	TYPE	USE	Description
!	----	----	---	-----------
!	MILLI	I*4	 I	Milliseconds of day
!	IHR	I*4	 O	hour of day
!	IMIN	I*4	 O	minute of hour
!	ISEC	I*4	 O	seconds of minute
!	MSEC	I*4	 O	millisecond of second
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
     :		MSEC,		!
     :		ITMP		! internal

      MSEC	= JMOD( MILLI, 1000 )	! milliseconds of second
      ITMP	= MILLI / 1000		! seconds of day
      ISEC	= JMOD( ITMP, 60 )	! seconds of minute
      ITMP	= ITMP / 60		! minutes of day
      IMIN	= JMOD( ITMP, 60 )	! minutes of hour
      IHR	= ITMP / 60		! hours of day
      
      RETURN

      END
