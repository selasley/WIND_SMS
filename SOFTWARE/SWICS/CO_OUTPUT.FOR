        PROGRAM CO_OUTPUT
C**************************************************************************
C       this subroutine increments boxes for C, O, C6, C5, O6, O7, and 
C       calculates/outputs ionization temperatures and C/O at appropriate
C       time intervals.
C
C       written by cmsc, 4 Jan 1996
C
C       Alterations
C         added restriction on processing for paps to be > 20 kV   2/1/96
C         added data quality flags and post gap flags              5/9/96
C	  write out error amounts for velocity, ratio, temps       6/14/96
C	  remove MOD from WRITE(30,xxx) to print 4-digit year,	   1/14/2000
C	  fix bug in leap year logic
C	  change call to DATE to DATE_AND_TIME for Y2K fix	   1/19/2000
C**************************************************************************
C                            *Declarations*
        IMPLICIT NONE
C
C                                      necessities for data
        INCLUDE 'INC_DIR:EDBDEF.INC/NOLIST'
        INCLUDE 'INC_DIR:L1FMT.INC/NOLIST'
        INCLUDE '($RMSDEF)'
C
        CHARACTER*80
     .    L1FILENAME /'DATA_DIR:WI*L1*.DAT'/               !data file name
C
        INTEGER*4
     .    LUNIT /10/,                                      !file unit num
     .    WIND_DECOMPRESS,                                 !function
     .    STATUS                                           !routine status
C
        RECORD
     .    /CORE/ SCI,                                      !basic data
     .    /HDBstr/ HDB,                                    !housekeeping
     .    /SW_PHA/ SWICS_PHA,                              !pha data
     .    /SW_RATE/ SWICS_RATE,                            !rate data
     .    /T/ START, END, LSCTIME,SCTIME,TIME_ARR(500)     !time data
C         
        INTEGER
     .    N, NN, CNTR                                      !from JP pgm
C
        LOGICAL
     .    CODE_C,                                          !t=compress C
     .    GOODSPIN                                         !from JP pgm
C
C                                      user input variables
        CHARACTER*80
     .   OUTFILE
C
        LOGICAL
     .    ANSWER,
     .    TSEARCH/.FALSE./                                 !t=time search done
C
        INTEGER*4
     .    DELTA_TIME,                             !output time scale in minutes
     .    STYY, STDOY, STHH, STMM,                         !start times
     .    SYY, SDOY, SHH, SMM                             !stop times
C
C                                      output variables
        REAL
     .    C_TEMP/-100/, O_TEMP/-100/,                !ionization temperatures
     .    CO_RATIO/-100/,                               !abundance ratio
     .    VEL_ARR(500),
     .    V_ALPHA_AVE/-100/                           !average of alpha velocity
C
        CHARACTER*8
     .    RUN_DT*9,
     .    RUN_TM
C
        INTEGER
     .    QUAL_FLAG_ARR(500), GAP_FLAG_ARR(500),
     .    QUAL_FLAG,       !denotes data quality, 0 = Good, 2 = Fair, 4 = Poor
     .    GAP_FLAG/0/                               !denotes type of data gap 
C                                      0 = none, 1 = bad instrument mode, 
C                                      2 = level 0 data missing, 
C                                      3 = level 0 data too noisy,
C                                      10 = not well defined He peak
C                                      internal variables
        INTEGER
     .    ARR_CNT/1/,
     .    COUNTER,                                         !for average
     .    CYCLE,                                           !cycle cntr
     .    VSTEP,                                           !voltage step
     .    PHANUMBER,                                       !#pha words
     .    PAPSL, ITEMP,                                    !to calc paps
     .    I,J,K
C
        REAL
     .    PAPS,                                            !paps voltage
     .    SUB_TIME,                                        !for sub_time rout
     .    VELOCITY(2), T4(2),                              !for hvel routine
     .    SUM_VELOCITY,                                    !for average
     .    EFFICIENCY(10080),
     .    MMPQ(2,10080),                                   !for pha routine
     .    ET(2,10080),
     .    EPQVAL(10080),
     .    C_BOX_MRANGE(3,2),                               !lo,hi m points
     .    C_BOX_MQRANGE(3,2),                              !lo, hi, m/q points
     .    O_BOX_MRANGE(3,2),                               !lo, hi m points
     .    O_BOX_MQRANGE(3,2),                              !lo, hi m/q points
     .    CO_BOX_MRANGE(2,2),                              !lo, hi m points
     .    CO_BOX_MQRANGE(2,2),                             !lo, hi m/q points
     .    OC_BOX_MRANGE(2,2),                              !lo, hi m points
     .    OC_BOX_MQRANGE(2,2),                             !lo, hi m/q points
     .    C_SPILLOVER/0.10/,                               !into O box
     .    O_SPILLOVER/0.20/,                               !into C box
     .    C_BOX_EFF/0.52/,                             !efficiency of box
     .    O_BOX_EFF/0.40/,                             !efficiency of box
     .    C_BOX(3)/0,0,0/,                              !boxes for C, C6, C5
     .    C_RAW/0/,                                        !raw counts
     .    O_BOX(3)/0,0,0/,                              !boxes for O, O7, O6
     .    O_RAW/0/,                                        !raw counts
     .    CO_BOX(2)/0,0/,                             !boxes for C at O7, O6
     .    OC_BOX(2)/0,0/,                              !boxes for O at C6, C5
     .    O_CORR,                                          !final O abund
     .    C_CORR,                                          !final C abund
     .    O7,                                          !final O abund
     .    O6,                                          !final O abund
     .    C6,                                          !final C abund
     .    C5,                                          !final C abund
     .    T6,                                 !ionization temperature
     .    CRATIOS(6)/-1.35,-0.58,0.030,0.55,0.99,1.36/, 
     .    CTEMPS(6)/5.9,6.0,6.1,6.2,6.3,6.4/,              !for t6 routine
     .    ORATIOS(6)/-3.13,-2.06,-1.22,-0.59,-0.09,0.31/, 
     .    OTEMPS(6)/5.9,6.0,6.1,6.2,6.3,6.4/
C
	REAL DELTA					!internal function
C
        LOGICAL
     .    FIRST/.TRUE./,                                   !first time
     .    EXCLUDE_H/.TRUE./,                               !for mmq routine
     .    EFFIC/.TRUE./,                                   !for mmq routine
     .    COMPARE_TIME_GE                                  !for time search
C
C***************************************************************************
C                         
C                            *Statements*
C
        DO J = 1, 3
          O_BOX_MRANGE(J,1) = 15
          O_BOX_MRANGE(J,2) = 17
          C_BOX_MRANGE(J,1) = 11
          C_BOX_MRANGE(J,2) = 13
          IF (J.LE.2) THEN
            OC_BOX_MRANGE(J,1) = 15
            OC_BOX_MRANGE(J,2) = 17
            CO_BOX_MRANGE(J,1) = 11
            CO_BOX_MRANGE(J,2) = 13
          END IF
        END DO
C
        O_BOX_MQRANGE(1,1) = 1.8
        O_BOX_MQRANGE(1,2) = 3.2
        O_BOX_MQRANGE(2,1) = 2.24
        O_BOX_MQRANGE(2,2) = 2.34
        O_BOX_MQRANGE(3,1) = 2.62
        O_BOX_MQRANGE(3,2) = 2.72
C
        C_BOX_MQRANGE(1,1) = 1.8
        C_BOX_MQRANGE(1,2) = 3.2
        C_BOX_MQRANGE(2,1) = 1.95
        C_BOX_MQRANGE(2,2) = 2.05
        C_BOX_MQRANGE(3,1) = 2.35
        C_BOX_MQRANGE(3,2) = 2.45
C
        OC_BOX_MQRANGE(1,1) = 1.95
        OC_BOX_MQRANGE(1,2) = 2.05
        OC_BOX_MQRANGE(2,1) = 2.35        
        OC_BOX_MQRANGE(2,2) = 2.45
C
        CO_BOX_MQRANGE(1,1) = 2.24
        CO_BOX_MQRANGE(1,2) = 2.34
        CO_BOX_MQRANGE(2,1) = 2.62
        CO_BOX_MQRANGE(2,2) = 2.72
C
 10     FORMAT (A,$)
 20     FORMAT (A,A)
C
C                                                user input
        WRITE(*,20) '        Wind/SWICS CO_OUTPUT Program'
        WRITE(*,*)
C
        WRITE(*,*)
        WRITE(*,10) ' Enter name of output file:  '
        READ(*,20) OUTFILE
        OPEN(UNIT=30, FILE=OUTFILE, STATUS='NEW', CARRIAGECONTROL='LIST')
        CALL DATE_AND_TIME(RUN_DT,RUN_TM)
        WRITE(30,20) RUN_DT(5:6)//'-'//RUN_DT(7:8)//'-'//RUN_DT(1:4)
        WRITE(30,20) RUN_TM(1:2)//':'//RUN_TM(3:4)//':'//RUN_TM(5:6)
C
        CALL WIND_GET_FILENAME(L1FILENAME, STATUS)
        WRITE(30,*)
        WRITE(30,20) ' Input file:  '//L1FILENAME
        WRITE(30,*)
        WRITE(30,20) '                            <Delta>   ',
     .               '       <Delta>           <Delta>           <Delta>'
        WRITE(30,20) '        Time      <He vel> <He vel>   ',
     .               '<C/O>    <C/O>     <Tc>     <Tc>     <To>     <To>'
C
        WRITE(*,*)
        WRITE(*,10) '  Do you want a time search?  '
        READ(*,20) ANSWER
        IF (ANSWER) THEN
          TSEARCH = .TRUE.
          WRITE(*,10) '   Enter start time [yyyy doy hh:mm]  '
          READ(*,30) STYY, STDOY, STHH, STMM
          START.YEAR=STYY
          START.DOY=STDOY
          START.HR=STHH
          START.MIN=STMM
          START.SEC=0
          WRITE(*,10) '   Enter stop time [yyyy doy hh:mm]  '
          READ(*,30) SYY, SDOY, SHH, SMM
          END.YEAR=SYY
          END.DOY=SDOY
          END.HR=SHH
          END.MIN=SMM
          END.SEC=0
          WRITE(30,*)
          WRITE(30,40) '  Time search:  ', STYY, STDOY, STHH, STMM,
     .                       SYY, SDOY, SHH, SMM
        END IF
C
 30     FORMAT (I4,1X,I3,1X,I2,':',I2)
 40     FORMAT (A,I4,1X,I3,1X,I2,':',I2.2,' - ',I4,1X,I3,1X,I2,':',I2.2)
C
        WRITE(*,*)
        WRITE(*,10) ' Enter output time interval in minutes:  '
        READ(*,*) DELTA_TIME
C
        WRITE(*,*)
        WRITE(*,20) '     Box defaults'
        WRITE(*,20) ' O:'
        WRITE(*,*) O_BOX_MRANGE
        WRITE(*,*) O_BOX_MQRANGE
        WRITE(*,20) ' C:'
        WRITE(*,*) C_BOX_MRANGE
        WRITE(*,*) C_BOX_MQRANGE
        WRITE(*,20) ' OC:'
        WRITE(*,*) OC_BOX_MRANGE
        WRITE(*,*) OC_BOX_MQRANGE
        WRITE(*,20) ' CO:'
        WRITE(*,*) CO_BOX_MRANGE
        WRITE(*,*) CO_BOX_MQRANGE
        WRITE(*,*)
        WRITE(*,10) '    Do you wish to change these?  '
        READ(*,20) ANSWER
        IF (ANSWER) THEN
          WRITE(*,10) '   Enter O, O7, O6 mass ranges:  '
          READ(*,*) O_BOX_MRANGE
          WRITE(*,10) '   Enter O, O7, O6 m/q ranges:  '
          READ(*,*) O_BOX_MQRANGE
          WRITE(*,10) '   Enter C, C6, C5 mass ranges:  '
          READ(*,*) C_BOX_MRANGE
          WRITE(*,10) '   Enter C, C6, C5 m/q ranges:  '
          READ(*,*) C_BOX_MQRANGE
          WRITE(*,10) '   Enter O in C6, C5 mass ranges:  '
          READ(*,*) OC_BOX_MRANGE
          WRITE(*,10) '   Enter O in C6, C5 m/q ranges:  '
          READ(*,*) OC_BOX_MQRANGE
          WRITE(*,10) '   Enter C in O7, O6 mass ranges:  '
          READ(*,*) CO_BOX_MRANGE
          WRITE(*,10) '   Enter C in O7, O6 m/q ranges:  '
          READ(*,*) CO_BOX_MQRANGE
        END IF                         !changing boxes
C
C                                      open data file and get data
        CALL WIND_L1_OPEN(LUNIT,L1FILENAME,L1_FILE_HEADER,STATUS)
        IF (STATUS.NE.1) THEN
          WRITE(*,20) ' problem opening file'
          STOP
        END IF
C
 100    CALL WIND_L1_READ_SR(LUNIT,CNTR,L1_SR_HEADER,STATUS)
        IF (STATUS.EQ.RMS$_EOF) GOTO 2000                  !end of this file
        CALL WIND_L1_GET_BASIC(LUNIT,SCI,STATUS)
        CALL WIND_L1_GET_HDB(LUNIT,HDB,STATUS)
        CALL WIND_L1_GET_SWICS(LUNIT,SWICS_RATE, SWICS_PHA, STATUS)
C                                                get first good spin
        N = 0
        GOODSPIN = .FALSE.
        DO WHILE (.NOT.GOODSPIN)
          K = N/32 + 1
          NN = MOD(N,31)
          GOODSPIN = BTEST(L1_SR_HEADER.PRESENT(K),NN)
          N = N + 1
          IF (N.GT.59) THEN
            GAP_FLAG = 2
            GOTO 100                  !all bad get next rec
          END IF
        END DO                                             !.not. goodspin
C
        SCTIME = SCI.EDB(0).W_TIME               !use wall time of first spin
        IF (SCTIME.DOY.EQ.0) THEN
	type *,'skipping this record, sctime is zero'
          GAP_FLAG = 2
          GOTO 100
        END IF
        IF (.NOT.TSEARCH .OR. (COMPARE_TIME_GE(SCTIME,START) .AND.
     .            .NOT.COMPARE_TIME_GE(SCTIME,END))) THEN  !in t range
          IF (FIRST) THEN
            GAP_FLAG = 0
            FIRST = .FALSE.
            LSCTIME = SCTIME
          END IF
C                                                          check paps
          ITEMP = HDB.BDH.DAT(61)
          CALL MVBITS(ITEMP,0,8,PAPSL,0) 
          PAPS = (190.96 - (117.15)*PAPSL)/(-1000)     !from BL 4/24/95
          IF (PAPS.LT.20) THEN                          !don't process
	type *,'skipping this sr, PAPS, PAPSL=',paps,papsl
            GAP_FLAG = 1
            GOTO 100
          END IF
C                                                          get velocity
          CALL WSWICS_GET_HVEL(SWICS_RATE, SCI, HDB, VELOCITY, T4)
C                                                          get m m/q data
          CALL WSWICS_GET_MMQ(SWICS_PHA,SWICS_RATE,SCI,HDB,EXCLUDE_H,
     .        EFFIC, EPQVAL, ET, MMPQ, EFFICIENCY, PHANUMBER)
C
          CYCLE = CYCLE + 1
c          IF (MOD(CYCLE,10).EQ.0) WRITE(*,'(A,I9)') 
c     .        '+  Working on cycle ', CYCLE
	type *,'Working on cycle ', CYCLE
        ELSE
          IF (TSEARCH.AND.COMPARE_TIME_GE(SCTIME,END)) THEN !done
            CALL WIND_L1_CLOSE(LUNIT,STATUS)               !close file
            GOTO 3000  
          END IF
        END IF                                             !tsearch
C
        IF (PHANUMBER.GT.0 .AND. VELOCITY(2).GE.200 .AND. 
     .          VELOCITY(2).LE.1000) THEN
          QUAL_FLAG = 0
C                                                check boxes
          DO I = 1, PHANUMBER
            DO J = 1, 3
              IF (MMPQ(1,I).GE.O_BOX_MRANGE(J,1) .AND. 
     .            MMPQ(1,I).LE.O_BOX_MRANGE(J,2) .AND.
     .            MMPQ(2,I).GE.O_BOX_MQRANGE(J,1) .AND.
     .            MMPQ(2,I).LE.O_BOX_MQRANGE(J,2)) THEN      !in box
                O_BOX(J) = O_BOX(J) + 1/EFFICIENCY(I)
                IF (J.EQ.1) O_RAW = O_RAW + 1
              END IF                                         !check box
              IF (MMPQ(1,I).GE.C_BOX_MRANGE(J,1) .AND. 
     .            MMPQ(1,I).LE.C_BOX_MRANGE(J,2) .AND.
     .            MMPQ(2,I).GE.C_BOX_MQRANGE(J,1) .AND.
     .            MMPQ(2,I).LE.C_BOX_MQRANGE(J,2)) THEN      !in box
                C_BOX(J) = C_BOX(J) + 1/EFFICIENCY(I)
                IF (J.EQ.1) C_RAW = C_RAW + 1
              END IF                                         !check box
            END DO                                 !J = 1, 3
            DO J = 1, 2
              IF (MMPQ(1,I).GE.OC_BOX_MRANGE(J,1) .AND. 
     .            MMPQ(1,I).LE.OC_BOX_MRANGE(J,2) .AND.
     .            MMPQ(2,I).GE.OC_BOX_MQRANGE(J,1) .AND.
     .            MMPQ(2,I).LE.OC_BOX_MQRANGE(J,2)) THEN      !in box
                OC_BOX(J) = OC_BOX(J) + 1/EFFICIENCY(I)
              END IF                                         !check box
              IF (MMPQ(1,I).GE.CO_BOX_MRANGE(J,1) .AND. 
     .            MMPQ(1,I).LE.CO_BOX_MRANGE(J,2) .AND.
     .            MMPQ(2,I).GE.CO_BOX_MQRANGE(J,1) .AND.
     .            MMPQ(2,I).LE.CO_BOX_MQRANGE(J,2)) THEN      !in box
                CO_BOX(J) = CO_BOX(J) + 1/EFFICIENCY(I)
              END IF                                         !check box
            END DO                                 !J = 1, 2
          END DO                                 !i = 1, phanumber
          IF (SUB_TIME(SCTIME,LSCTIME).LE.DELTA_TIME/2.) THEN
            WRITE(30,50) SCTIME.YEAR,SCTIME.DOY,SCTIME.HR,SCTIME.MIN,
     .      VELOCITY(2),DELTA(VELOCITY(2),.3), CO_RATIO,DELTA(CO_RATIO,.2),
     .      C_TEMP,DELTA(C_TEMP,.2), O_TEMP,DELTA(O_TEMP,.2), QUAL_FLAG,GAP_FLAG
          ELSE
            TIME_ARR(ARR_CNT) = SCTIME
            VEL_ARR(ARR_CNT) = VELOCITY(2)
            GAP_FLAG_ARR(ARR_CNT) = GAP_FLAG
            QUAL_FLAG_ARR(ARR_CNT) = QUAL_FLAG
            ARR_CNT = ARR_CNT + 1
          END IF
          SUM_VELOCITY = SUM_VELOCITY + VELOCITY(2)
          COUNTER = COUNTER + 1
          GAP_FLAG = 0
        ELSE
          IF (PHANUMBER.LT.0) GAP_FLAG = 1
          IF (VELOCITY(2).LT.200 .OR. VELOCITY(2).GT.1000) GAP_FLAG = 10
        END IF                                             !phanumber>0
C                                                check time scale
        IF (SUB_TIME(SCTIME,LSCTIME).GE.DELTA_TIME) THEN
C          V_ALPHA_AVE = SUM_VELOCITY/COUNTER
          IF (C_RAW.GT.1.AND. O_RAW.GT.1) THEN
            C_CORR = (C_BOX(1) - O_BOX(1)*O_SPILLOVER)/C_BOX_EFF
            O_CORR = (O_BOX(1) - C_BOX(1)*C_SPILLOVER)/O_BOX_EFF
            C6 = C_BOX(2) - OC_BOX(1)*O_SPILLOVER
            C5 = C_BOX(3) - OC_BOX(2)*O_SPILLOVER
            O7 = O_BOX(2) - CO_BOX(1)*C_SPILLOVER
            O6 = O_BOX(3) - CO_BOX(2)*C_SPILLOVER
            IF (C6.GT.0 .AND. C5.GT.0) C_TEMP = T6(C6/C5, CRATIOS, CTEMPS)/1E6
            IF (O7.GT.0 .AND. O6.GT.0) O_TEMP = T6(O7/O6, ORATIOS, OTEMPS)/1E6
            IF (O_CORR.GT.0 .AND. C_CORR.GT.0) CO_RATIO = C_CORR/O_CORR
            IF (C_RAW.LT.10 .OR. O_RAW.LT.10) QUAL_FLAG = 4
            IF (C_RAW.LT.40 .OR. O_RAW.LT.40) QUAL_FLAG = 2
            IF (C_RAW.GE.40 .OR. O_RAW.GE.40) QUAL_FLAG = 0
          END IF
          IF (ARR_CNT.GT.1) THEN
            WRITE(30,50) TIME_ARR(1).YEAR,TIME_ARR(1).DOY,TIME_ARR(1).HR,
     .       TIME_ARR(1).MIN, VEL_ARR(1),DELTA(VEL_ARR(1),.3), CO_RATIO,DELTA(CO_RATIO,.2),
     .       C_TEMP,DELTA(C_TEMP,.2), O_TEMP,DELTA(O_TEMP,.2),
     .       QUAL_FLAG_ARR(1), GAP_FLAG_ARR(1)
          ELSE
            WRITE(30,50) SCTIME.YEAR,SCTIME.DOY,SCTIME.HR,SCTIME.MIN,
     .      VELOCITY(2),DELTA(VELOCITY(2),.3), CO_RATIO,DELTA(CO_RATIO,.2),
     .      C_TEMP,DELTA(C_TEMP,.2), O_TEMP,DELTA(O_TEMP,.2), QUAL_FLAG,GAP_FLAG
          END IF
          LSCTIME = SCTIME
          COUNTER = 0
          SUM_VELOCITY = 0
          V_ALPHA_AVE = -100
          CO_RATIO = -100
          C_TEMP = -100
          O_TEMP = -100
          C_RAW = 0
          O_RAW = 0
          DO J = 1, 3
            O_BOX(J) = 0
            C_BOX(J) = 0
            IF (J.LE.2) THEN
              OC_BOX(J) = 0
              CO_BOX(J) = 0
            END IF
          END DO
          DO I = 2, ARR_CNT-1
            WRITE(30,50) TIME_ARR(I).YEAR,TIME_ARR(I).DOY,TIME_ARR(I).HR,
     .       TIME_ARR(I).MIN, VEL_ARR(I),DELTA(VEL_ARR(I),.3), CO_RATIO,CO_RATIO,
     .       C_TEMP,C_TEMP, O_TEMP,O_TEMP,
     .       QUAL_FLAG_ARR(I), GAP_FLAG_ARR(I)
          END DO
          ARR_CNT = 1
        END IF                                             !if delt > delta_time
C
   50   FORMAT (1X,I4,1X,I3,1X,2(I2,1X),2F9.2,6F9.3,2X,2I4)
C                                                get next record
        GOTO 100
C
 2000   CALL WIND_L1_CLOSE(LUNIT,STATUS)                   !close file
        WRITE(*,10) ' Do you wish to add another file?  '
        READ(*,20) ANSWER
        IF (ANSWER) THEN
          L1FILENAME = 'DATA_DIR:WI*L1*.DAT'
          CALL WIND_GET_FILENAME(L1FILENAME, STATUS)
          CALL WIND_L1_OPEN(LUNIT,L1FILENAME,L1_FILE_HEADER,STATUS)
          GOTO 100
        END IF
C
        IF (ARR_CNT.GT.1) THEN
          IF (C_RAW.GT.1 .AND. O_RAW.GT.1) THEN
            C_CORR = (C_BOX(1) - O_BOX(1)*O_SPILLOVER)/C_BOX_EFF
            O_CORR = (O_BOX(1) - C_BOX(1)*C_SPILLOVER)/O_BOX_EFF
            C6 = C_BOX(2) - OC_BOX(1)*O_SPILLOVER
            C5 = C_BOX(3) - OC_BOX(2)*O_SPILLOVER
            O7 = O_BOX(2) - CO_BOX(1)*C_SPILLOVER
            O6 = O_BOX(3) - CO_BOX(2)*C_SPILLOVER
            IF (C6.GT.0 .AND. C5.GT.0) C_TEMP = T6(C6/C5, CRATIOS, CTEMPS)/1E6
            IF (O7.GT.0 .AND. O6.GT.0) O_TEMP = T6(O7/O6, ORATIOS, OTEMPS)/1E6
            IF (O_CORR.GT.0 .AND. C_CORR.GT.0) CO_RATIO = C_CORR/O_CORR
            IF (C_RAW.LT.10 .OR. O_RAW.LT.10) QUAL_FLAG = 4
            IF (C_RAW.LT.40 .OR. O_RAW.LT.40) QUAL_FLAG = 2
            IF (C_RAW.GE.40 .OR. O_RAW.GE.40) QUAL_FLAG = 0
          END IF
          WRITE(30,50) TIME_ARR(1).YEAR,TIME_ARR(1).DOY,TIME_ARR(1).HR,
     .     TIME_ARR(1).MIN, VEL_ARR(1),DELTA(VEL_ARR(1),.3), CO_RATIO,DELTA(CO_RATIO,.2),
     .     C_TEMP,DELTA(C_TEMP,.2), O_TEMP,DELTA(O_TEMP,.2),
     .     QUAL_FLAG_ARR(1), GAP_FLAG_ARR(1)
          CO_RATIO = -100
          C_TEMP = -100
          O_TEMP = -100
          DO I = 2, ARR_CNT-1
            WRITE(30,50) TIME_ARR(I).YEAR,TIME_ARR(I).DOY,TIME_ARR(I).HR,
     .       TIME_ARR(I).MIN, VEL_ARR(I),DELTA(VEL_ARR(I),.3), CO_RATIO,CO_RATIO,
     .       C_TEMP,C_TEMP, O_TEMP,O_TEMP,
     .       QUAL_FLAG_ARR(I), GAP_FLAG_ARR(I)
          END DO
        END IF
C
 3000	CLOSE (UNIT=30)                                    !close ouput
        STOP
        END
C*****************************************************************************
        FUNCTION SUB_TIME(TIME, LTIME)
C
C       this function calculates the minutes between two times
C
        IMPLICIT NONE
C
        INCLUDE 'INC_DIR:EDBDEF.INC/NOLIST'
        INCLUDE 'INC_DIR:L1FMT.INC/NOLIST'
        INCLUDE '($RMSDEF)'
C
        RECORD
     .    /T/ TIME, LTIME
C
        REAL
     .    SUB_TIME
C
        INTEGER
     .    YEAR, DOY, HR, MIN, SEC
C
C
        YEAR = TIME.YEAR - LTIME.YEAR
        DOY = TIME.DOY - LTIME.DOY
        HR = TIME.HR - LTIME.HR
        MIN = TIME.MIN - LTIME.MIN
        SEC = TIME.SEC - LTIME.SEC
C
C Test if leap year.  NOTE: Since 2000 is also a multiple of 400, it *is* a
C leap year.  Test appropriately.
C
        IF ((MOD(LTIME.YEAR, 4).EQ.0 .AND. MOD(LTIME.YEAR,100).NE.0) .OR.
     &	    MOD(LTIME.YEAR,400).EQ.0 ) THEN
          SUB_TIME = YEAR*60*24*366 + DOY*60*24 + HR*60 + MIN + SEC/60.
        ELSE
          SUB_TIME = YEAR*60*24*365 + DOY*60*24 + HR*60 + MIN + SEC/60.
        END IF
C
        RETURN
        END 
C**************************************************************************
     	FUNCTION T6 (RTIO,R,T)
C*******************************************************************
C	a function that calculates the ionization temperature (T6) 
C	given a ratio of ions.  It interpolates (on a log-log scale)
C	between 6 different provided points.
C
C	written by C. Shafer, May 22, 1991
C*******************************************************************
C
C			DECLARATIONS
C
        IMPLICIT NONE
C
     	REAL*4
     .	 RTIO,
     .	 LRATIO,
     .	 T6,
     .	 LOGT6,
     .	 M					!SLOPE = M, 
C						LOGT6 = LOG BASE 10 OF T6
C
     	REAL*4
     .	 R(6),
     .	 T(6)     			!GIVEN POINTS (IN LOG)
C
        INTEGER
     .    I
C
C************************************************************************
C
C			STATEMENTS
C
     	LRATIO = LOG10(RTIO)
C
     	IF (LRATIO.LE.R(1)) THEN
          LOGT6 = T(1)		! LRATIO < R1
        ELSE IF (LRATIO.GT.R(6)) THEN
          LOGT6 = T(6)
        ELSE 
          DO I = 1, 5
            IF ((LRATIO.GT.R(I)).AND.(LRATIO.LE.R(I+1))) THEN
              M = (T(I+1) - T(I)) / (R(I+1) - R(I))
              LOGT6 = M*(LRATIO - R(I)) + T(I)
            END IF
          END DO
        END IF
C
     	T6 = 10**LOGT6
C
        RETURN
C
     	END
C**************************************************************************
	REAL FUNCTION DELTA(VALUE,FRAC)
	REAL VALUE,FRAC
	DELTA = VALUE*FRAC
	IF (VALUE.EQ.-100.) DELTA = -100.
	RETURN
	END
