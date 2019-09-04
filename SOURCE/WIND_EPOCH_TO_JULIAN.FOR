      SUBROUTINE WIND_EPOCH_TO_JULIAN( EPOCH, JULIAN, STATUS )
!	Routine to convert time expressed as EPOCH format (milliseconds AD)
!	into JULIAN day format (days since 4713 BC)
!
!	Version	 5-Apr-1994
!
!	USAGE:
!
!	CALL WIND_JULIAN_TO_EPOCH( EPOCH, JULIAN, STATUS )
!
!	PARAMETERS:
!
!	REAL*8	EPOCH	! INPUT:  milliseconds since 3-JAN-0001 B.C.
!		JULIAN	! OUTPUT: days since 4713 B.C.
!
!	INTEGER*4 STATUS ! OUTPUT:SS$_NORMAL ! 1 = SUCCESFUL OPERATION
!
!	Created:
!	 5-APR-1994	H.BROER	University of Maryland, Space Physics Group
!
      IMPLICIT NONE

      INCLUDE	'($SSDEF)'

      INTEGER*4 STATUS

      REAL*8	EPOCH,
     :		JULIAN
!
      STATUS	= SS$_NORMAL

      JULIAN	= EPOCH / 864.D5 + 172106.D1

      RETURN
      END
