C

      SUBROUTINE DPU_EQ(DPPSP,DPPSM,DVS,EQ)

C     This subroutine determines the e/q value based on the default dpu
C     algorithm and constants.

      REAL*4 D1/6.190722/,
     .       D2/1.1225857/      ! Default values used for E/Q calculation

      INTEGER*4 DPPSP,          !enable bit for dpps plus plate
     .          DPPSM,          !enable bit for dpps minus plate
     .          DVS             !dpps voltage step number

      REAL*4 EQ     !DPU value for e/q

          IF(IAND(DPPSP, DPPSM).EQ.1)THEN               !both plates on
            EQ = D1 * (D2**DVS)
          ELSE IF (DPPSP.EQ.1 .OR. DPPSM.EQ.1) THEN     !one plate on
            EQ = 0.5 * D1 * (D2**DVS)
          ELSE                                          !neither plate on
            EQ = 0
          ENDIF

      RETURN
      END
     
