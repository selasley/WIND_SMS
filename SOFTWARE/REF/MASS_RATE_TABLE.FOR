CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      PROGRAM MASS_RATE_TABLE
C This program reads a WIND L1 data file and decompresses the rate data, printing 
C it in a table vs time and science record number
C 
C Created 	16-NOV-1994	JOHN PAQUETTE
C
C This version:   1.5   16-DEC-1994
C
C Revision history:
C       16-NOV-1994     J.PAQUETTE      MASS instead of SWICS
C       22-NOV-1994     L. Bleau        added heading showing filename, which clock
C	29-NOV-1994	L. Bleau	add file selection via menu
C	29-NOV-1994	L. Bleau	change output directory to TABLE_DIR:
C	30-NOV-1994	L. Bleau	add local time variable TIME_TO_USE to
C					allow use of either time
C        1-DEC-1994     J.PAQUETTE      changed e/q numbers, minor output format changes
C        9-DEC-1994     J.PAQUETTE      space inserted before 'MASS', moved initialization
C                                       of STARTFOUND and ENDFOUND above label 100, and
C                                       corrected start-finding logic
C	16-DEC-1994	J.PAQUETTE	allow user to input month and day instead of DOY
C
CDEC$ IDENT  '1.5'
      INCLUDE	'INC_DIR:EDBDEF.INC/NOLIST'
      INCLUDE   'INC_DIR:L1FMT.INC/NOLIST'
      INCLUDE   '($RMSDEF)'
      INTEGER*4 STATUS,L1_LUN/19/,MASS_RATES(116),WIND_DECOMPRESS,VSTEP,MONTH,DAYOFMONTH
      REAL*4 E_OVER_Q
      CHARACTER L1FILENAME*80
      CHARACTER FULL_L1_NAME*80
      INTEGER UNIT_TABLE(0:2,3)/52,53,54,53,52,54,54,52,53/
      LOGICAL CODE_C,CODE_C2,STARTFOUND,ENDFOUND,USE_SC_CLOCK,COMPARE_TIME_GE,GOODSPIN
      INTEGER I,J,K,L,N,NN,CNTR,ITEMP,NAMELEN
C
      RECORD /CORE/ SCI
      RECORD /HDBstr/ HDB
      RECORD /MA_RATE/ MASS_RATE
      RECORD /MA_PHA/ MASS_PHA
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
C Open L1 file for input, and read first SR, Core, and STICS records.
C
      CALL WIND_L1_OPEN( L1_LUN, L1FILENAME, L1_FILE_HEADER, STATUS)
      IF (STATUS .NE. 1) GOTO 9010
      INQUIRE(FILE=L1FILENAME,NAME=FULL_L1_NAME)
      NAMELEN = INDEX(FULL_L1_NAME,' ') - 1
      IF (NAMELEN .LT. 1) NAMELEN=LEN(FULL_L1_NAME)
  13  CALL WIND_L1_READ_SR( L1_LUN, CNTR, L1_SR_HEADER, STATUS )
      IF(STATUS.EQ.RMS$_EOF)GO TO 900
      IF (STATUS .NE. 1) GOTO 9020
      CALL WIND_L1_GET_BASIC( L1_LUN, SCI, STATUS)
      IF (STATUS .NE. 1) GOTO 9030
      CALL WIND_L1_GET_MASS( L1_LUN, MASS_RATE, MASS_PHA, STATUS )
      IF (STATUS .NE. 1) GOTO 9080
C
C Find first good spin, get year, and DOY from it.
C
      N=0
      GOODSPIN=.FALSE.
      DO 14 WHILE(.NOT.GOODSPIN)
        K=N/32+1
        NN=MOD(N,31)
        GOODSPIN=BTEST(L1_SR_HEADER.PRESENT(K),NN)
        N=N+1
        IF(N.GT.59)GOTO 13 ! This SCI-REC all bad; get next
  14  CONTINUE
c      N=N-1
C
C Open other files for output
C
      OPEN(UNIT= 50, NAME='UER.MASS', DEFAULTFILE='TABLE_DIR:', FORM='FORMATTED', 
     &     TYPE='NEW', RECL=168)
      WRITE(50,16) FULL_L1_NAME(1:NAMELEN)
  16  FORMAT(' MASS',/,T10,'Input Level 1 Filename: ',A)
      IF(USE_SC_CLOCK.EQ.1)THEN
        WRITE(50,17) 'SpaceCraft'
        WRITE(50,20) SCI.EDB(N).SC_TIME.YEAR,SCI.EDB(N).SC_TIME.DOY
      ELSE
        WRITE(50,17) 'Wall'
        WRITE(50,20) SCI.EDB(N).W_TIME.YEAR,SCI.EDB(N).W_TIME.DOY
      ENDIF
  17  FORMAT(T10,'Clock selected:         ',A/)
  20  FORMAT(' YEAR: ',I4,' DAY: ',I3,/,' Sci Rec#   Time   VS    E/Q     ',
     &'  FSR 1   FSR 2   FSR B   MDCR',/,
     &'_______________________________________________________________')
C
      OPEN(UNIT= 60, NAME='SER.MASS', DEFAULTFILE='TABLE_DIR:', FORM='FORMATTED', 
     &     TYPE='NEW', RECL=168)
      WRITE(60,16) FULL_L1_NAME(1:NAMELEN)
      IF(USE_SC_CLOCK.EQ.1)THEN
        WRITE(60,17) 'SpaceCraft'
        WRITE(60,40) SCI.EDB(N).SC_TIME.YEAR,SCI.EDB(N).SC_TIME.DOY
      ELSE
        WRITE(60,17) 'Wall'
        WRITE(60,40) SCI.EDB(N).W_TIME.YEAR,SCI.EDB(N).W_TIME.DOY
      ENDIF
  40  FORMAT(' YEAR: ',I4,' DAY: ',I3,/,' Sci Rec#   Time   VS     E/Q    ',
     &' UFSR_S  UFSR_NS  RSR_S  RSR_NS   DCR_S  DCR_NS   MFSR',/,
     &'______________________________________________________',
     &'_________________________________')
C
      OPEN(UNIT= 70, NAME='BR.MASS', DEFAULTFILE='TABLE_DIR:', FORM='FORMATTED', 
     &     TYPE='NEW', RECL=168)
      WRITE(70,16) FULL_L1_NAME(1:NAMELEN)
      IF(USE_SC_CLOCK.EQ.1)THEN
        WRITE(70,17) 'SpaceCraft'
        WRITE(70,60) SCI.EDB(N).SC_TIME.YEAR,SCI.EDB(N).SC_TIME.DOY
      ELSE
        WRITE(70,17) 'Wall'
        WRITE(70,60) SCI.EDB(N).W_TIME.YEAR,SCI.EDB(N).W_TIME.DOY
      ENDIF
  60  FORMAT(' YEAR: ',I4,' DAY: ',I3,/,' Sci Rec#   Time   VS     E/Q   ',
     &'   BR0_S  BR0_NS   BR1_S  BR1_NS',/,
     &'_________________________________________________________________')
C
      OPEN(UNIT= 80, NAME='MR.MASS', DEFAULTFILE='TABLE_DIR:', FORM='FORMATTED', 
     &     TYPE='NEW', RECL=168)
      WRITE(80,16) FULL_L1_NAME(1:NAMELEN)
      IF(USE_SC_CLOCK.EQ.1)THEN
        WRITE(80,17) 'SpaceCraft'
        WRITE(80,70) SCI.EDB(N).SC_TIME.YEAR,SCI.EDB(N).SC_TIME.DOY
      ELSE
        WRITE(80,17) 'Wall'
        WRITE(80,70) SCI.EDB(N).W_TIME.YEAR,SCI.EDB(N).W_TIME.DOY
      ENDIF
  70  FORMAT(' YEAR: ',I4,' DAY: ',I3,/,' Sci Rec#   Time   VS     E/Q   ',
     &'   MR 0    MR 1    MR 2    MR 3    MR 4    MR 5    MR 6    MR 7    MR 8    MR 9',/,
     &'_______________________________________________________________________________',
     &'________________________________')
C
C Set flags for start and stop
C
      STARTFOUND=.FALSE.
      ENDFOUND=.FALSE.
C
C Get the Basic Core data
C
 100  CALL WIND_L1_GET_BASIC( L1_LUN, SCI, STATUS)
      IF (STATUS .NE. 1) GOTO 9030
C
C Get Housekeeping Data Block
C
      CALL WIND_L1_GET_HDB( L1_LUN, HDB, STATUS)
      IF (STATUS .NE. 1) GOTO 9050
C
C Get MASS rates and PHA
C
      CALL WIND_L1_GET_MASS( L1_LUN, MASS_RATE, MASS_PHA, STATUS )
      IF (STATUS .NE. 1) GOTO 9080
C
C Decompress MASS rates for one science record and output
C
      DO 800 J=0,59
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
          IF(ENDFOUND)GO TO 900
C
C Get voltage step and e/q
C
          ITEMP = MASS_RATE.EDB(J).RATE(25)
          VSTEP = IBITS(ITEMP,0,6)
          E_OVER_Q = 0.5156 * 1.05135**VSTEP
C
C Compression codes for all MASS rates are in the HDB in byte 189, bit 4
C
          ITEMP = HDB.BDH.DAT(189)
          CODE_C = BTEST(ITEMP,4)
C
C Decompress MASS rates
C
          DO 200 I=0,24
            MASS_RATES(I+1)=WIND_DECOMPRESS(MASS_RATE.EDB(J).RATE(I),CODE_C)
 200      CONTINUE
C
C Print out UER data. We have 1 file containing 4 RATE values per voltage step.
C
          WRITE(50,210) L1_SR_HEADER.SR_COUNT,
     &                  TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
     &                  VSTEP,
     &                  E_OVER_Q,
     &                  (MASS_RATES(L),L=1,4)
 210      FORMAT(1X,I8,1X,I2,':',I2.2,':',I2.2,1X,I2,1X,F8.3,4(1X,I7))
C
C Print out SER data. We have 1 file containing 7 RATE values per voltage step.
C
          WRITE(60,250) L1_SR_HEADER.SR_COUNT,
     &                  TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
     &                  VSTEP,
     &                  E_OVER_Q,
     &                  (MASS_RATES(L+4),L=1,7)
 250      FORMAT(1X,I8,1X,I2,':',I2.2,':',I2.2,1X,I2,1X,F8.3,7(1X,I7))
C
C Print out BR data. We have 1 file containing 4 RATE values per voltage step.
C
          WRITE(70,210) L1_SR_HEADER.SR_COUNT,
     &                  TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
     &                  VSTEP,
     &                  E_OVER_Q,
     &                  (MASS_RATES(L+11),L=1,4)
C
C Print out MR data. We have 1 file, containing 10 RATE values per voltage step.
C
          WRITE(80,350) L1_SR_HEADER.SR_COUNT,
     &                  TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
     &                  VSTEP,
     &                  E_OVER_Q,
     &                  (MASS_RATES(L+15),L=1,10)
 350      FORMAT(1X,I8,1X,I2,':',I2.2,':',I2.2,1X,I2,1X,F8.3,10(1X,I7))
C
        ENDIF
 800  CONTINUE
C
C Read next Science Record
C
      CALL WIND_L1_READ_SR( L1_LUN, CNTR, L1_SR_HEADER, STATUS )
      IF(STATUS.EQ.RMS$_EOF)GO TO 900
      IF (STATUS .NE. 1) GOTO 9020
C
      GO TO 100
C
C Close files and STOP
C
 900  CALL WIND_L1_CLOSE( L1_LUN, STATUS )      ! Close input file
      IF (STATUS .NE. 1) GOTO 9090
      CLOSE(UNIT=50)                            ! Close UER file
      CLOSE(UNIT=60)                            ! Close SER file
      CLOSE(UNIT=70)                            ! Close BR file
      CLOSE(UNIT=80)                            ! Close MR file
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
9080  TYPE *,'ERROR GETTING MASS DATA; STATUS=',STATUS
      STOP
9090  TYPE *,'ERROR CLOSING L1 FILE; STATUS=',STATUS
      STOP
      END
