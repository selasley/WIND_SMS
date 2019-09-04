        SUBROUTINE WSWICS_GET_HPEAK(SWICS_RATE,SCI,HDB,     !input
     .                       PEAK_ION, HM1_ION, HM2_ION)            !output
C**************************************************************************
C       this subroutine finds the proton peak in the fsr rate and returns
C       the voltage step and those at the half max positions
C       
C       cmsc, 6 Jan 1995
C
C       Altered:                                           Date:
C       change routine to find both H and He                 19 Feb 1995
C       half max positions                                    cmsc
C       Now returns 'i' value instead of vs for 
C       peak and half max positions
C**************************************************************************
C                            *Declarations*
        IMPLICIT NONE
C
        INCLUDE 'INC_DIR:EDBDEF.INC/NOLIST'
        INCLUDE 'INC_DIR:L1FMT.INC/NOLIST'
        INCLUDE '($RMSDEF)'
C
C                                                **input**
        RECORD
     .    /CORE/ SCI,                                      !basic data
     .    /HDBstr/ HDB,                                    !housekeeping
     .    /SW_RATE/ SWICS_RATE                             !rate data
C
C                                                **output**
        INTEGER
     .    PEAK_ION(2), HM1_ION(2), HM2_ION(2)              !for H+
C
C                                                **internal**
        LOGICAL
     .    FOUND1, FOUND2,                                  !found hm's
     .    CODE_C                                           !t=compress C
C
        INTEGER
     .    RATE_USED,
     .    SHIFT(2)/0,2/,                                   !fsr and tcr
     .    WIND_DECOMPRESS,                                 !function
     .    ITEMP,                                           !temp value
     .    VSTEP,                                           !voltage step
     .    PEAK,                                          !peak counts
     .    TEMP_PEAK,
     .    VPEAK,                                           !step at peak
     .    HM1, HM2,                                        !half maxes
     .    MEMAX,                                           !# of me rates
     .    I,J,K,
     .    IMID                                             !temp var
C
        REAL
     .    BCKGND,                                          !background val
     .    MID                                              !1/2 max value
C
C***************************************************************************
C                         
C                            *Statements*
C
C
        DO K = 1, 2
          FOUND1 = .FALSE.
          FOUND2 = .FALSE.
          BCKGND = 1E9
          HM1 = -1
          HM2 = -1
          PEAK = 0
          DO I = 0, 59                                   !loop over spins
C                                                **get bit rate**
            IF (SCI.EDB(I).BITRATE) THEN                 !high bitrate
              MEMAX = 109                                !number of merates
            ELSE
              MEMAX = 55                                 !number of merates
            END IF
C                                                **get compression code & vs**
            ITEMP = HDB.BDH.DAT(189)
            CODE_C = BTEST(ITEMP,2)
            ITEMP = SWICS_RATE.EDB(I).RATE(MEMAX+6)
            VSTEP = IBITS(ITEMP,0,6)
C                                                **find peak**
            RATE_USED = WIND_DECOMPRESS(SWICS_RATE.EDB(I).RATE(MEMAX
     .                       +SHIFT(K)),CODE_C)       !fsr for h+, tcr for he++
            IF (RATE_USED.GT.PEAK) THEN
              PEAK = RATE_USED                               !get rate
              IMID = I
            END IF
            IF (RATE_USED.LT.BCKGND) THEN
              BCKGND = RATE_USED
            END IF
          END DO                                           !i = 0, 59
C
          IF (PEAK.LT.(BCKGND+2*SQRT(BCKGND))) THEN !not 2 sig above
            PEAK_ION(K) = -1
            HM1_ION(K) = -1
            HM2_ION(K) = -1
            GOTO 1000
          ELSE
            PEAK_ION(K) = IMID
          END IF
C                                                **find 1/2 maximums**
          IF (K.EQ.2)                            !find peak in fsr for he++
     .      PEAK = WIND_DECOMPRESS(SWICS_RATE.EDB(IMID).RATE(MEMAX),CODE_C)
          TEMP_PEAK = PEAK
          MID = PEAK/2.                                  !1/2 max
          MID = MAX(MID,BCKGND)
          DO I = 1, 10
            IF (IMID-I.GE.0 .AND. .NOT.FOUND1) THEN
              RATE_USED = WIND_DECOMPRESS(SWICS_RATE.EDB(IMID-I).RATE(MEMAX)
     .                       ,CODE_C)                      !both use fsr now
              IF (RATE_USED.LE.MID) THEN
                HM1 = IMID-I
                FOUND1 = .TRUE.
              END IF
            END IF
            IF (IMID+I.LE.59 .AND. .NOT.FOUND2) THEN
              RATE_USED = WIND_DECOMPRESS(SWICS_RATE.EDB(IMID+I).RATE(MEMAX),
     .                       CODE_C)
C                                                in case never find hm2 for he
C                                                take step at lowest rate 
              IF (K.EQ.2 .AND.RATE_USED.LT.TEMP_PEAK) THEN
                TEMP_PEAK = RATE_USED
                HM2 = IMID+I
              END IF
              IF (RATE_USED.LE.MID) THEN
                HM2 = IMID+I
                FOUND2 = .TRUE.
              END IF
            END IF                                         !imid+i.le.59
            IF (FOUND1 .AND. FOUND2) THEN
              HM1_ION(K) = HM1
              HM2_ION(K) = HM2
              GOTO 1000
            END IF
          END DO                                           !i = 1, 10
          IF (K.EQ.2.AND.FOUND1) THEN              !never found real hm2 for he
            HM1_ION(K) = HM1
            HM2_ION(K) = HM2
          ELSE
            HM1_ION(K) = -1
            HM2_ION(K) = -1
          END IF
 1000   END DO                                             !k = 1, 2
        RETURN
        END
