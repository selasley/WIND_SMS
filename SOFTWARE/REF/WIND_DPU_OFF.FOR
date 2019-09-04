      SUBROUTINE WIND_DPU_OFF
C
C This subroutine handles the case in which HK data is present even though the
C experiment DPU is not producing sync words. It sets various quality flags to
C 8 (meaning "missing") preparatory to storing the data.
C
C     This version:    9-FEB-1995 
C
C     CREATION:
C        7-NOV-1994     J.PAQUETTE
C
C     REVISIONS:
C	 9-FEB-1995	L. Bleau	change include stmt to use EDBDEF.INC
C					and EDBVARS.INC instead of EDB.INC only
C
CDEC$ IDENT   '1.0'

      INCLUDE   'INC_DIR:EDBDEF.INC/NOLIST'
      INCLUDE   'INC_DIR:EDBVARS.INC/NOLIST'

      INTEGER II
C
      DO 200 II = 0, 59
        SCI( EDBset ).EDB( II ).Qual              = 8
        SCI( EDBset ).EDB( II ).CORE_Qual         = 8
        SCI( EDBset ).EDB( II ).SW_BR_Qual        = 8
        SCI( EDBset ).EDB( II ).SW_MR_Qual        = 8
        SCI( EDBset ).EDB( II ).SW_ME1_Qual       = 8
        SCI( EDBset ).EDB( II ).SW_ME2_Qual       = 8
        SCI( EDBset ).EDB( II ).SW_ER_Qual        = 8
        SCI( EDBset ).EDB( II ).SW_P_Qual         = 8
        SCI( EDBset ).EDB( II ).ST_HMR_Qual       = 8
        SCI( EDBset ).EDB( II ).ST_SMR_Qual       = 8
        SCI( EDBset ).EDB( II ).ST_BR_Qual        = 8
        SCI( EDBset ).EDB( II ).ST_OMR_Qual       = 8
        SCI( EDBset ).EDB( II ).ST_ER_Qual        = 8
        SCI( EDBset ).EDB( II ).ST_P_Qual         = 8
        SCI( EDBset ).EDB( II ).MA_UER_Qual       = 8
        SCI( EDBset ).EDB( II ).MA_SER_Qual       = 8
        SCI( EDBset ).EDB( II ).MA_BR_Qual        = 8
        SCI( EDBset ).EDB( II ).MA_MR_Qual        = 8
        SCI( EDBset ).EDB( II ).MA_P_Qual         = 8
200   CONTINUE
C
      RETURN
      END
