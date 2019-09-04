      SUBROUTINE VOLTAGE_CHECK(STEP_MODE, SPIN_NUM, TELEMETRY_RATE, DVS,	!input 
     .                                              DVS_FLAG)           	!output
C This routine checks the DPPS voltage step to make sure it is correct depending
C on the Telemetry Rate, Stepping Mode, and EDB Spin Number. 
C
C Created 	 2-OCT-1996	by 	K.CHOTOO
C

      IMPLICIT NONE

      LOGICAL*1 TELEMETRY_RATE			!Telemetry bit rate: true=high, false = Low

      INTEGER*4 DVS,				!Deflection voltate step read from EDB
     .          DVS_FLAG,			!For incorrect voltage step, set to -1
     .          DVS_SEQUENCE(0:3,0:59),		!DPPS stepping sequences
     .		I,J,K,				!Array counters
     .          SPIN, SPIN_NUM,			!Spin Number 0-59
     .          STEP_MODE			!DPPS stepping sequence: 0=Fine, 1=Medium, 2=Coarse,
						!			 3=DPRAM2

C		**********  Start routine here ***********
      DVS_FLAG = 0 

C
C Check for valid stepping mode
C 
      IF((STEP_MODE .LT. 0) .OR. (STEP_MODE .GT. 3)) THEN
   	DVS_FLAG = -1
	GOTO 40
      ENDIF
C
C Check for valid spin number
C
      IF((SPIN_NUM  .LT. 0) .OR. (SPIN_NUM  .GT. 59)) THEN
	DVS_FLAG = -1 
	GOTO 40
      ENDIF
C
C In High Bit Rate, the DVS changes every spin so all 60 values used in a Science Record. 
C In Low Bit Rate DVS changes every other spin so only first 30 values used in a Science Record.
C
      DO J = 0, 29
        DVS_SEQUENCE(0,J) = 31 - J			!Fine mode
        DVS_SEQUENCE(0,J+30) = DVS_SEQUENCE(0,J)

        DVS_SEQUENCE(1,J) = 31 - (MOD(J,15) * 2)		!Medium mode
 	DVS_SEQUENCE(1,J+30) = DVS_SEQUENCE(1,J)

	DVS_SEQUENCE(2,J) = 31 - (MOD(J,10) * 3)		!Coarse mode
	DVS_SEQUENCE(2,J+30) = DVS_SEQUENCE(2,J)

        IF (J .LE. 12) DVS_SEQUENCE(3,J) = J * 2		!DPRAM2
 	IF ((J .EQ. 13) .OR. (J .EQ. 14)) DVS_SEQUENCE(3,J) = (J + 1) * 2
        IF ((J .GE. 15) .AND.(J .LE. 29)) DVS_SEQUENCE(3,J) = 31 - ((J - 14) * 2)
        DVS_SEQUENCE(3,J+30) = DVS_SEQUENCE(3,J)
      END DO

      IF (TELEMETRY_RATE) THEN
	 IF (DVS_SEQUENCE(STEP_MODE,SPIN_NUM) .NE. DVS) DVS_FLAG = -1
      ELSE
	 SPIN = SPIN_NUM / 2
         IF (DVS_SEQUENCE(STEP_MODE,SPIN) .NE. DVS) DVS_FLAG = -1
      ENDIF

  40  RETURN

      END

