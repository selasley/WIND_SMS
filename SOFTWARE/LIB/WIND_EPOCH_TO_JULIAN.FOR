      SUBROUTINE WIND_EPOCH_TO_JULIAN( EPOCH, JULIAN, STATUS )
C	Routine to convert time expressed as EPOCH format (milliseconds AD)
C	into JULIAN day format (days since 4713 BC)
C
C	Version	 5-Apr-1994
C
C	USAGE:
C
C	CALL WIND_JULIAN_TO_EPOCH( EPOCH, JULIAN, STATUS )
C
C	PARAMETERS:
C
C	REAL*8	EPOCH	! INPUT:  milliseconds since 1-JAN-0001 B.C.
C		JULIAN	! OUTPUT: days since 4713 B.C.
C
C	INTEGER*4 STATUS ! OUTPUT: 1 = SUCCESFUL OPERATION
C
C	Created:
C	 5-APR-1994	H.BROER	University of Maryland, Space Physics Group
C
C Modification history:
C	19-APR-1995	L. Bleau	changed comments
C
      IMPLICIT NONE

      INTEGER*4 STATUS

      REAL*8	EPOCH,JULIAN
C
      STATUS	= 1

      JULIAN	= EPOCH / 864.D5 + 172106.D1

      RETURN
      END
