      SUBROUTINE WIND_JULIAN_TO_EPOCH( EPOCH, JULIAN, STATUS )
!	Routine to convert time expressed as EPOCH format (milliseconds AD)
!	into JULIAN day format (days since 4713 BC)
!
!	Version	 6-Apr-1994
!
!	USAGE:
!
!	CALL WIND_JULIAN_TO_EPOCH( EPOCH, JULIAN, STATUS )
!
!	PARAMETERS:
!
!	REAL*8	EPOCH	! OUTPUT:  milliseconds since 3-JAN-0001 B.C.
!				   to be compatible with GEOTAIL
!		JULIAN	! INPUT:   days since 4713 B.C.
!
!	INTEGER*4 STATUS ! OUTPUT:SS$_NORMAL ! 1 = SUCCESFUL OPERATION
!
!	Created:
!	 4-APR-1994	H.BROER	University of Maryland, Space Physics Group
!
      IMPLICIT NONE

      INCLUDE	'($SSDEF)'

      INTEGER*4 STATUS

      REAL*8	EPOCH,
     :		JULIAN
!
      STATUS	= SS$_NORMAL

      EPOCH	= ( JULIAN - 172106.D1 ) * 864.D5

      RETURN
      END
