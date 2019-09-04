CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      PROGRAM STICS_RATE_EPQ
C This program reads a WIND L1 data file and decompresses the rate data, printing 
C it in a table vs time and science record number. Only those items with E/Q > 26
C are printed, and rates are averaged over 10 science records. Taken from 
C STICS_RATE_TABLE
C 
C Created  16-JUNE-1994	JOHN PAQUETTE
C
C This version:   1.16   16-MAR-1995
C
C Revision history:
C
CDEC$ IDENT '1.16'
      INCLUDE	'INC_DIR:EDBDEF.INC/NOLIST'
      INCLUDE   'INC_DIR:L1FMT.INC/NOLIST'
      INCLUDE   '($RMSDEF)'
      INTEGER*4 STATUS,L1_LUN/19/,STICS_RATES(179),D_RATES(17), AVCNT/0/,III
      INTEGER*4 WIND_DECOMPRESS,VSTEP,MONTH,DAYOFMONTH
      REAL*4 D1/6.190722/,D2/1.1225857/      ! Default values used for E/Q calculation
      REAL*4 E_OVER_Q
      CHARACTER L1FILENAME*80, HMRNAME*16, SMRNAME*19, BRNAME*9
      CHARACTER FULL_L1_NAME*80, FIFO*3, TGS*3, RATEFLAG*1
      CHARACTER*17 MODE(10) /'Science mode     ','Engineering mode ',
     &                       'Maneuver mode    ','Contingency mode ',
     &                       'Science mode     ','Engineering mode ',
     &                       'Maneuver mode    ','Contingency mode ',
     &                       'Transitional mode','Unknown mode     '/,
     &             MODE_AT_START
      LOGICAL*1 BITRATE
      LOGICAL CODE_C,CODE_C2,STARTFOUND,ENDFOUND,USE_SC_CLOCK,COMPARE_TIME_GE,GOODSPIN
      INTEGER I,J,K,L,M,N,NN,CNTR,ITEMP,ITEMP2,NAMELEN,YR2DIGIT,INCREMENT,SUMBR(0:2),SUMHMR,SUMSMR
C
      RECORD /CORE/ SCI
      RECORD /HDBstr/ HDB
      RECORD /ST_RATE/ STICS_RATE
      RECORD /ST_PHA/ STICS_PHA
      RECORD /T/ START, END, TIME_TO_USE
C
C Get input L1 filename
C
      L1FILENAME = 'DATA_DIR:WI*L1*.DAT'
      CALL WIND_GET_FILENAME(L1FILENAME, STATUS)
      IF(STATUS.NE.1)STOP
C
C Ask user to choose time format
C
      TYPE 5
   5  FORMAT(' Enter 1 for Spacecraft Clock, or 0 for Wall Time: '$)
      ACCEPT*,USE_SC_CLOCK
C
C Prompt user for start and stop times
C
      TYPE 10
  10  FORMAT(' Enter Start time( 0/ means whole file) - Year,Month,Day,Hour,Min,Sec: '$) 
      ACCEPT*, START.YEAR,MONTH,DAYOFMONTH,START.HR,START.MIN,START.SEC
      IF(START.YEAR.NE.0)THEN
C
C Convert month and day to day of year.
C
        CALL CNVRT_T_YRMONDAY_TO_YEARDAY( START, MONTH, DAYOFMONTH)
        IF(START.YEAR.LT.100)START.YEAR=START.YEAR+1900        ! Allow for 2-digit years
        IF(START.YEAR.LT.1980)START.YEAR=START.YEAR+100        ! Allow for years beyond 1999
        TYPE 12
  12    FORMAT(' Enter End time - Year,Month,Day,Hour,Min,Sec: '$) 
        ACCEPT*, END.YEAR,MONTH,DAYOFMONTH,END.HR,END.MIN,END.SEC
C
C Convert month and day to day of year.
C
        CALL CNVRT_T_YRMONDAY_TO_YEARDAY( END, MONTH, DAYOFMONTH)
        IF(END.YEAR.LT.100)END.YEAR=END.YEAR+1900        ! Allow for 2-digit years
        IF(END.YEAR.LT.1980)END.YEAR=END.YEAR+100        ! Allow for years beyond 1999
      ELSE
        END.YEAR=3000
      ENDIF
C
C Open L1 file for input, and read first SR, Core, HDB, and STICS RATES
C
      CALL WIND_L1_OPEN( L1_LUN, L1FILENAME, L1_FILE_HEADER, STATUS)
      IF (STATUS .NE. 1) GOTO 9010
      INQUIRE(FILE=L1FILENAME,NAME=FULL_L1_NAME)
      NAMELEN = INDEX(FULL_L1_NAME,' ') - 1
      IF (NAMELEN .LT. 1) NAMELEN=LEN(FULL_L1_NAME)
  13  CALL WIND_L1_READ_SR( L1_LUN, CNTR, L1_SR_HEADER, STATUS )
      IF(STATUS.EQ.RMS$_EOF)GO TO 1000
      IF (STATUS .NE. 1) GOTO 9020
      CALL WIND_L1_GET_BASIC( L1_LUN, SCI, STATUS)
      IF (STATUS .NE. 1) GOTO 9030
      CALL WIND_L1_GET_HDB( L1_LUN, HDB, STATUS)
      IF (STATUS .NE. 1) GOTO 9050
      CALL WIND_L1_GET_STICS( L1_LUN, STICS_RATE, STICS_PHA, STATUS )
      IF (STATUS .NE. 1) GOTO 9080

C
C Find first good spin, get year and DOY from it.
C
      N=0
      GOODSPIN=.FALSE.
      DO 14 WHILE(.NOT.GOODSPIN)
        K=N/32+1
        NN=MOD(N,32)
        GOODSPIN=BTEST(L1_SR_HEADER.PRESENT(K),NN) 
        N=N+1
        IF(N.GT.59)GOTO 13 ! This SCI-REC all bad; get next
  14  CONTINUE
  17    FORMAT(T10,'Input Level 1 Filename: ',A)
  18    FORMAT(T10,'Clock selected:         ',A)
  19    FORMAT(T10,'FIFO mode is ',A3,T30,'TGS mode is ',A3,T50,' Telemetry mode is ',A17)
C
C If low bitrate, make sure that N points to an even spin (odd spins 
C have been emptied by concatenation).
C If high bitrate, decrement because N was incremented after the 1st 
C good spin was found.
C
      IF(.NOT.SCI.EDB(N).BITRATE)THEN
        N=N+MOD(N,2)
        IF(N.GE.60)GOTO 13
      ELSE
        N=N-1
      ENDIF
C
      OPEN(UNIT= 90, NAME='ER1.STICS', DEFAULTFILE='TABLE_DIR:', FORM='FORMATTED', 
     &     TYPE='NEW', RECL=168)
      WRITE(90,85)
  85  FORMAT(' STICS ENGINEERING RATES: FSR''s, RSR''s, MPF and MPR')
      WRITE(90,17) FULL_L1_NAME(1:NAMELEN)
C
      IF(USE_SC_CLOCK.EQ.1)THEN
        WRITE(90,18) 'SpaceCraft'
      ELSE
        WRITE(90,18) 'Wall'
      ENDIF                    
      WRITE(90,19)FIFO,TGS,MODE_AT_START
      WRITE(90,90) 
  90  FORMAT(' Sci Rec#      Time      BR VS    E/Q     ',
     &'  DCR 1   DCR 2   DCR 3 ',/,
     &'________________________________________________________________________')
C
C
C Set flags for start and stop
C
      STARTFOUND=.FALSE.
      ENDFOUND=.FALSE.
C
C Get the Basic Core data
C
 100  AVCNT = AVCNT + 1
      IF(AVCNT.GT.10)THEN
        AVCNT = 1
C
C Print out ER data. Thus we have 1 file containing 3 RATE values per voltage step.
C
        WRITE(90,650) L1_SR_HEADER.SR_COUNT,
     &                YR2DIGIT,TIME_TO_USE.DOY,
     &                TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
     &                RATEFLAG,
     &                VSTEP,
     &                E_OVER_Q,
     &                STICS_RATES(171), STICS_RATES(172), STICS_RATES(173)
 650    FORMAT(1X,I8,1X,I2,1X,I3,1X,I2,':',I2.2,':',I2.2,1X,A1,1X,I2,1X,F8.3,3(1X,I7))
        DO 855 III=171,173
          STICS_RATES(III)=0
 855    CONTINUE
      ENDIF
C
      CALL WIND_L1_GET_BASIC( L1_LUN, SCI, STATUS)
      IF (STATUS .NE. 1) GOTO 9030
C
C Get Housekeeping Data Block
C
      CALL WIND_L1_GET_HDB( L1_LUN, HDB, STATUS)
      IF (STATUS .NE. 1) GOTO 9050
C
C If HDB bad, skip entire science record
C 
      IF(HDB.Qual.NE.0)GOTO 990
C
C Get STICS rates and PHA
C
      CALL WIND_L1_GET_STICS( L1_LUN, STICS_RATE, STICS_PHA, STATUS )
      IF (STATUS .NE. 1) GOTO 9080
C
C Get appropriate increment for loop below. At HBR, there is data in every spin; at LBR
C there is data only in even-numbered spins.
C
      BITRATE = SCI.EDB(N).BITRATE
      IF(BITRATE)THEN
        INCREMENT=1
      ELSE
        INCREMENT=2
      ENDIF
C
C Decompress STICS rates for one science record and output
C
      DO 900 J=0,59,INCREMENT
        IF(USE_SC_CLOCK)THEN
          TIME_TO_USE = SCI.EDB(J).SC_TIME
        ELSE
          TIME_TO_USE = SCI.EDB(J).W_TIME
        ENDIF
C
C Compare time read to start time. If the time read is greater than or equal to the user-
C specified start time, then begin processing data.
C
        IF(.NOT.STARTFOUND)THEN
          STARTFOUND=COMPARE_TIME_GE(TIME_TO_USE,START)
        ENDIF
        IF(STARTFOUND)THEN
C
C Compare time read to end time. If time read is greater than the user-specified end time, 
C we're done. Go to the part of the code that closes files and STOP's program.
C
          ENDFOUND=COMPARE_TIME_GE(TIME_TO_USE,END)
          IF(ENDFOUND)GO TO 1000
C
C Check quality flags. If ANY quality flags are bad, skip this spin
C
          IF(BITRATE)THEN
            IF(SCI.EDB(J).Qual.NE.0)GOTO 900
          ELSE
            IF((SCI.EDB(J).Qual.NE.0).OR.(SCI.EDB(J+1).Qual.NE.0))GOTO 900
          ENDIF
          IF(SCI.EDB(J).ST_ER_Qual.NE.0)GOTO 900
C
C Get year in 2-digit form for output
C
          IF(TIME_TO_USE.YEAR.GE.2000)THEN
            YR2DIGIT=TIME_TO_USE.YEAR-2000
          ELSEIF(TIME_TO_USE.YEAR.GE.1900)THEN
            YR2DIGIT=TIME_TO_USE.YEAR-1900
          ELSE
            YR2DIGIT=TIME_TO_USE.YEAR
          ENDIF

C
C Get voltage step and determine if the deflection systems are on.
C
          ITEMP = STICS_RATE.EDB(J).RATE(179)
          VSTEP = IBITS(ITEMP,0,5)
          ITEMP=HDB.DAT(24)
          ITEMP2=HDB.DAT(25)
          IF(VSTEP.LE.26)GOTO 900
C
C The logic that follows assumes that both + and - high voltages are on or both are off.
C If both are on, calculate e/q. If both are off, set e/q to 0.
C
          IF(IAND(IBITS(ITEMP,7,1), IBITS(ITEMP2,0,1)).EQ.1)THEN
            E_OVER_Q = D1 * (D2**VSTEP)
          ELSE
            E_OVER_Q = 0
          ENDIF
C
C Get bit rate
C
          IF(SCI.EDB(J).BITRATE)THEN
            RATEFLAG='H'                ! High bit rate
          ELSEIF(TIME_TO_USE.YEAR.NE.0)THEN
            RATEFLAG='L'                ! Low bit rate
          ELSE
            RATEFLAG='?'                ! Bit rate indeterminate, due to bad quality data
          ENDIF
C
C FSR's, RSR's, and DCR's 
C
            DO 600 I=164,172
              STICS_RATES(I+1)= STICS_RATES(I+1) + WIND_DECOMPRESS(STICS_RATE.EDB(J).RATE(I),CODE_C2)
 600        CONTINUE
        ENDIF
 900  CONTINUE
C
C Read next Science Record
C
 990  CALL WIND_L1_READ_SR( L1_LUN, CNTR, L1_SR_HEADER, STATUS )
      IF(STATUS.EQ.RMS$_EOF)GO TO 1000
      IF (STATUS .NE. 1) GOTO 9020
C
      GO TO 100
C
C Close files and STOP
C
1000  CALL WIND_L1_CLOSE( L1_LUN, STATUS )      ! Close input file
      IF (STATUS .NE. 1) GOTO 9090
      CLOSE(UNIT=90)                          ! Close ER file
2000  CONTINUE
      STOP
C
C Come here for errors
C
9010  TYPE *,'ERROR OPENING L1 FILE; STATUS=',STATUS
      STOP
9020  TYPE *,'ERROR READING SCIENCE RECORD ',CNTR,'; STATUS=',STATUS
      STOP
9030  TYPE *,'ERROR GETTING BASIC CORE; STATUS=',STATUS
      STOP
9050  TYPE *,'ERROR GETTING HDB; STATUS=',STATUS
      STOP
9080  TYPE *,'ERROR GETTING STICS DATA; STATUS=',STATUS
      STOP
9090  TYPE *,'ERROR CLOSING L1 FILE; STATUS=',STATUS
      STOP
      END
