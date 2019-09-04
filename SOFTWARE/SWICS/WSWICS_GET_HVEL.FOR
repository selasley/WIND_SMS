        SUBROUTINE WSWICS_GET_HVEL(SWICS_RATE,SCI,HDB,     !input
     .                                 VEL_ION,T4)         !output
C**************************************************************************
C       this subroutine finds the proton peak in the fsr rate and converts
C       the voltage step to velocity.
C       
C       cmsc, 3 Jan 1995
C
C       alteration:                                        Date:
C       Added He velocity and Tk.  This is gotten            6 Feb 1995
C       from the TCR which peaks at He.                       cmsc
C
C       change getting of three points.  Now use             19 Feb 1995
C       WSWICS_GET_HPEAK routine to find half max             cmsc
C       voltage steps for H and He and use those
C       plus peak (in distribution) for 3 points sent
C       to MFIT routine
C
C**************************************************************************
C                            *Declarations*
        IMPLICIT NONE
C
        INCLUDE 'INC_DIR:EDBDEF.INC/NOLIST'
        INCLUDE 'INC_DIR:L1FMT.INC/NOLIST'
        INCLUDE '($RMSDEF)'
C
        INTEGER*4
     .    FSR,
     .    WIND_DECOMPRESS                                  !function
C
        RECORD
     .    /CORE/ SCI,                                      !basic data
     .    /HDBstr/ HDB,                                    !housekeeping
     .    /SW_RATE/ SWICS_RATE                             !rate data
C         
        LOGICAL
     .    CODE_C                                           !t=compress C
C
        REAL
     .    BCKGND,
     .    VEL(3), VEL0, SIGMA, X, 
     .    VEL_ION(2),T4(2)
C
        INTEGER
     .    ITEMP,                                           !temp value
     .    VSTEP,                                           !voltage step
     .    IMID, IMAX, IMIN,                                !limits
     .    MEMAX                                            !# of me rates
C
        REAL
     .    MASS(2)/1,4/,                                    
     .    MPQ(2)/1,2/,                                     !protons and alphas
     .    DISTRIB(3),                                          !pseudo dj/de
     .    EPQ,
     .    PEAK                                           !peak counts
C
        INTEGER
     .    PEAKS(2), HM1S(2), HM2S(2),                        !for hpeak routine
     .    I,J,K
C
        EXTERNAL
     .    MFIT
C
C***************************************************************************
C                         
C                            *Statements*
C
C
        ITEMP = HDB.BDH.DAT(189)
        CODE_C = BTEST(ITEMP,2)
C
        CALL WSWICS_GET_HPEAK(SWICS_RATE,SCI,HDB,PEAKS,HM1S,HM2S)
C
        DO K = 1, 2                              !protons then alphas
          DO I = 1, 3
            DISTRIB(I) = 0
          END DO
C
          IF (PEAKS(K).LT.0 .OR. PEAKS(K).EQ.59 .OR. PEAKS(K).EQ.0) THEN  
C                                                not 2 sig above bckgnd
            VEL_ION(K) = -1E10
            T4(K) = -1E10
            GOTO 1000
          END IF
          IF (HM1S(K).LT.0) HM1S(K) = PEAKS(K) - 1
          IF (HM2S(K).LT.0) HM2S(K) = PEAKS(K) + 1
C                                                          get hm dist vals
          IF (SCI.EDB(HM1S(K)).BITRATE) THEN                 !high bitrate
            MEMAX = 109                                !number of merates
          ELSE
            MEMAX = 55                                 !number of merates
          END IF
          PEAK = WIND_DECOMPRESS(SWICS_RATE.EDB(HM1S(K)).RATE(MEMAX),CODE_C)
          ITEMP = SWICS_RATE.EDB(HM1S(K)).RATE(MEMAX+6)
          VSTEP = IBITS(ITEMP,0,6)
          EPQ = 0.5 * 60.0**(VSTEP/59.0)
          DISTRIB(1) = PEAK*MPQ(K)/EPQ
          VEL(1) = 437.8*SQRT(EPQ/MPQ(K))
C
          IF (SCI.EDB(HM2S(K)).BITRATE) THEN                 !high bitrate
            MEMAX = 109                                !number of merates
          ELSE
            MEMAX = 55                                 !number of merates
          END IF
          PEAK = WIND_DECOMPRESS(SWICS_RATE.EDB(HM2S(K)).RATE(MEMAX),CODE_C)
          ITEMP = SWICS_RATE.EDB(HM2S(K)).RATE(MEMAX+6)
          VSTEP = IBITS(ITEMP,0,6)
          EPQ = 0.5 * 60.0**(VSTEP/59.0)
          DISTRIB(3) = PEAK*MPQ(K)/EPQ
          VEL(3) = 437.8*SQRT(EPQ/MPQ(K))
C
          IMIN = PEAKS(K) - 4
          IMAX = PEAKS(K) + 4
          IF (IMIN.LT.0) IMIN = 0
          IF (IMAX.GT.59) IMAX = 59
          DO I = IMIN, IMAX                                !loop over spins
C                                                get bit rate
            IF (SCI.EDB(I).BITRATE) THEN                 !high bitrate
              MEMAX = 109                                !number of merates
            ELSE
              MEMAX = 55                                 !number of merates
            END IF
            FSR = WIND_DECOMPRESS(SWICS_RATE.EDB(I).RATE(MEMAX),!fsr
     .                                           CODE_C)
            ITEMP = SWICS_RATE.EDB(I).RATE(MEMAX+6)
            VSTEP = IBITS(ITEMP,0,6)
            EPQ = 0.5 * 60.0**(VSTEP/59.0)
            IF (FSR*MPQ(K)/EPQ.GT.DISTRIB(2)) THEN
              DISTRIB(2) = FSR*MPQ(K)/EPQ
              VEL(2) = 437.8*SQRT(EPQ/MPQ(K))
            END IF
          END DO                                           !i = 0, 59
 500      IF (DISTRIB(1).GT.0 .AND. DISTRIB(2).GT.0 .AND. DISTRIB(3).GT.0) THEN
            IF (DISTRIB(2).GT.DISTRIB(3) .AND. VEL(2).GT.VEL(3) .AND.
     .            VEL(1).GT.VEL(2)) THEN
              CALL MFIT(VEL(1),DISTRIB(1),VEL(2),DISTRIB(2),VEL(3),DISTRIB(3),
     .                       X, VEL0, SIGMA)
              VEL_ION(K) = VEL0
              T4(K) = SIGMA*60.5E-4*MASS(K)                !sigma in [km/s]^2
            ELSE
              VEL_ION(K) = -1E10
              T4(K) = -1E10
            END IF
          END IF
          IF (VEL_ION(K).EQ.0) VEL_ION(K) = -1E10
          IF (T4(K).EQ.0) T4(K) = -1E10
 1000   END DO                                             !k = 1, 2
        RETURN
        END
