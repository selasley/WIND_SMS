CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      PROGRAM STICS_RATE_TABLE
C This program reads a WIND L1 data file and decompresses the rate data, printing 
C it in a table vs time and science record number
C 
C Created 	3-NOV-1994	JOHN PAQUETTE
C
C This version:   1.15   16-MAR-1995
C
C Revision history:
C       11-NOV-1994     J.PAQUETTE      use start and stop times
C       16-NOV-1994     J.PAQUETTE      changed output file extent to STICS
C       18-NOV-1994     J.PAQUETTE      user inputs month+day, instead of DOY
C       22-NOV-1994     L. Bleau        added heading showing filename, which clock
C	29-NOV-1994	L. Bleau	add file selection via menu
C	29-NOV-1994	L. Bleau	change output directory to TABLE_DIR:
C	30-NOV-1994	L. Bleau	add local time variable TIME_TO_USE to
C					allow use of either time
C        1-DEC-1994     J.PAQUETTE      various output format changes as requested
C        6-DEC-1994     J.PAQUETTE      telemetry mode in header
C        9-DEC-1994     J.PAQUETTE      moved initialization of STARTFOUND and ENDFOUND 
C                                       above label 100, and corrected start-finding logic;
C                                       use new time conversion routine to get DOY
C        5-JAN-1995     J.PAQUETTE      also print out diagnostic rates
C       10-JAN-1995     J.PAQUETTE      corrected error; VSTEP is not neccesarily 0 when
C                                       voltage off, according to ABG
C       23-JAN-1995     J.PAQUETTE      first goodspin now required to be even at low BR
C       31-JAN-1995     J.PAQUETTE      corrected error in DDPS voltage checking and in
C                                       compression code for engineering rates
C        8-FEB-1995     J.PAQUETTE      tighten logic on pointer
C       16-FEB-1995     J.PAQUETTE      added checks to quality flags
C       20-FEB-1995     J.PAQUETTE      added check to prevent N exceeding 59
C       21-FEB-1995     J.PAQUETTE      if any quality flag is bad, skip to next spin;
C                                       added SUMHMR,SUMSMR, and SUMBR to OMR files
C	16-MAR-1995	J.PAQUETTE      changed TGS mode check to every spin,N=>J,
C					also if HDB is bad skip entire Science Record
C
C
CDEC$ IDENT '1.14'
      INCLUDE	'INC_DIR:EDBDEF.INC/NOLIST'
      INCLUDE   'INC_DIR:L1FMT.INC/NOLIST'
      INCLUDE   '($RMSDEF)'
      INTEGER*4 STATUS,L1_LUN/19/,STICS_RATES(179),D_RATES(17)
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

C     naming the output file
      LOGICAL BRACK                     !A variable which is .TRUE. if there is
C                                       a set of square brackets in the input file name.
C                                       If there is, they, and everything between them,
C                                       will be stripped out so that the output file is
C                                       created in the user's account and not on the
C                                       account from which we read the data.
      INTEGER*4 NINDEX,                 !used in rearranging the input file name to
C                                       create the output file name.
     .          JSAVE                   !used in rearranging the input file name to
C                                       create the output file name.
      CHARACTER*90 LISFIL1,LISFIL2      !holds the name of the input file
      CHARACTER OUTFILENAME1*90,OUTFILENAME2*90
      CHARACTER*40 USER_ER1_NAME	!user name for ER1 output file

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
C      TYPE 5
C   5  FORMAT(' Enter 1 for Spacecraft Clock, or 0 for Wall Time: '$)
C      ACCEPT*,USE_SC_CLOCK
       USE_SC_CLOCK = 0
C
C Prompt user to name ER1 output file
C
      WRITE(6,*)'Enter output ER1 filename or <RETURN> for default name'
      READ(5,'(A)') USER_ER1_NAME
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



C   **********************************************************
C   *                                                        *
C   *                   CREATE OUTPUT FILE NAME              *
C   *                                                        *
C   **********************************************************
C
C   Strip the extension off of the input file name so
C   that we can give a new extension to the output file
C
      NINDEX  = 80
      DO WHILE (L1FILENAME(NINDEX:NINDEX) .NE. '_')
         NINDEX = NINDEX - 1
      END DO
C
C   Find out if the input file name contains some directory
C   designation.  This, if it exists, it will also be stripped off.
C
      I = 1
      DO WHILE (.NOT. BRACK .AND. I .LT. NINDEX)
         IF (L1FILENAME(I:I) .EQ. '[') BRACK = .TRUE.
         I = I + 1
      END DO
C
C   Now, finally, load the input file name into the output
C   file name and add proper extension.
C
      JSAVE = 1
      DO 3 I = 1, NINDEX
         IF (BRACK) THEN
            IF (L1FILENAME(I:I) .EQ. ']') BRACK = .FALSE.
         ELSE
            LISFIL1(JSAVE:JSAVE) = L1FILENAME(I:I)
            JSAVE = JSAVE + 1
         ENDIF
   3  CONTINUE
      LISFIL1(1:9) = 'STICS_ER1'
      LISFIL1(JSAVE+1:JSAVE+4) = '.TBL'        ! ALL DATA
      LISFIL2 = LISFIL1
      LISFIL2(1:9) = 'STICSOMR1'
     
      IF (USER_ER1_NAME(1:12) .NE. '            ')THEN
        OUTFILENAME1 = USER_ER1_NAME
      ELSE
        OUTFILENAME1 = LISFIL1			!default name
      ENDIF
      OUTFILENAME2 = LISFIL2
C**********************************************************************
C
C Open other files for output
C
C      DO 60 J=0,5
C        WRITE(HMRNAME,15) J
C  15    FORMAT('HMR_ANODE',I1,'.STICS')
C        OPEN(UNIT= 50+J, NAME=HMRNAME, DEFAULTFILE='TABLE_DIR:', FORM='FORMATTED', 
C     &       TYPE='NEW', RECL=168)
C        WRITE(50+J,16)J
C  16    FORMAT(' STICS HIGH RESOLUTION MATRIX RATES ANODE',I1)
C        WRITE(50+J,17) FULL_L1_NAME(1:NAMELEN)
  17    FORMAT(T10,'Input Level 1 Filename: ',A)
C            
C        IF(USE_SC_CLOCK.EQ.1)THEN
C          WRITE(50+J,18) 'SpaceCraft'
C        ELSE
C          WRITE(50+J,18) 'Wall'
C        ENDIF
C
C Check FIFO mode, telemetry mode, and TGS mode; place info in header.
C
        ITEMP = HDB.BDH.DAT(221)
        IF(IBITS(ITEMP,7,1).EQ.1)THEN
          FIFO='ON '
        ELSE
          FIFO='OFF'
        ENDIF
C
        ITEMP = SCI.EDB(N).TLM_Mode
        IF(ITEMP.EQ.128)ITEMP=9
        IF(ITEMP.EQ.256)ITEMP=10
        MODE_AT_START = MODE(ITEMP)
C
        ITEMP = STICS_RATE.EDB(N).RATE(179)
        IF(IBITS(ITEMP,7,1).EQ.1)THEN
          TGS='ON '
        ELSE
          TGS='OFF'
        ENDIF
C
C        WRITE(50+J,19)FIFO,TGS,MODE_AT_START
C        WRITE(50+J,20)
  18    FORMAT(T10,'Clock selected:         ',A)
  19    FORMAT(T10,'FIFO mode is ',A3,T30,'TGS mode is ',A3,T50,' Telemetry mode is ',A17)
C  20    FORMAT(' Sci Rec#      Time      BR VS    E/Q     ',
C     &  '   S0      S1      S2      S3      S4      S5      S6      S7  ',
C     &  '   S8      S9      S10     S11     S12     S13     S14     S15',/,
C     &  '________________________________________________________',
C     &  '________________________________________________________',
C     &  '_______________________________________________________')
C
C        IF(MOD(J,2).NE.1)THEN
C          WRITE(SMRNAME,25) J,J+1
C  25      FORMAT('SMR_ANODES',I1,'-',I1,'.STICS')
C          OPEN(UNIT= 60+J, NAME=SMRNAME, DEFAULTFILE='TABLE_DIR:', FORM='FORMATTED', 
C     &         TYPE='NEW', RECL=168)
C          WRITE(60+J,26)J,J+1
C  26      FORMAT(' STICS SECTORED MATRIX RATES ANODE',I1,'-',I1)
C          WRITE(60+J,17) FULL_L1_NAME(1:NAMELEN)
C          IF(USE_SC_CLOCK.EQ.1)THEN
C            WRITE(60+J,18) 'SpaceCraft'
C          ELSE
C            WRITE(60+J,18) 'Wall'
C          ENDIF                    
C          WRITE(60+J,19)FIFO,TGS,MODE_AT_START
C          WRITE(60+J,30) 
C  30      FORMAT(' Sci Rec#      Time      BR VS    E/Q  ',
C     &    '   S0+S1   S2+S3   S4+S5   S6+S7',
C     &    '   S8+S9 S10+S11 S12+S13 S14+S15',/,
C     &    '____________________________________________________',
C     &    '___________________________________________________')
C          
C          WRITE(BRNAME,45) J/2
C  45      FORMAT('BR',I1,'.STICS')
C          OPEN(UNIT= 70+J, NAME=BRNAME, DEFAULTFILE='TABLE_DIR:', FORM='FORMATTED', 
C     &         TYPE='NEW', RECL=168)
C          WRITE(70+J,46)J/2
C  46      FORMAT(' STICS BASIC RATES ',I1)
C          WRITE(70+J,17) FULL_L1_NAME(1:NAMELEN)
C          IF(USE_SC_CLOCK.EQ.1)THEN
C            WRITE(70+J,18) 'SpaceCraft'
C          ELSE
C            WRITE(70+J,18) 'Wall'
C          ENDIF                    
C          WRITE(70+J,19)FIFO,TGS,MODE_AT_START
C         WRITE(70+J,50) 
C  50      FORMAT(' Sci Rec#       Time     BR VS     E/Q   ',
C     &    '   S0+S1   S2+S3   S4+S5   S6+S7',
C     &    '   S8+S9 S10+S11 S12+S13 S14+S15',/,
C     &    '_____________________________________________________',
C     &    '____________________________________________________')
C        ENDIF
C  60  CONTINUE
C
      OPEN(UNIT= 80, NAME= OUTFILENAME2, DEFAULTFILE='TABLE_DIR:', FORM='FORMATTED', 
     &     TYPE='NEW', RECL=168)
C      OPEN(UNIT= 81, NAME='OMR2.STICS', DEFAULTFILE='TABLE_DIR:', FORM='FORMATTED', 
C     &     TYPE='NEW', RECL=168)
      WRITE(80,65)
  65  FORMAT(' STICS OMNIDIRECTIONAL MATRIX RATES 1')
      WRITE(80,17) FULL_L1_NAME(1:NAMELEN)
C      WRITE(81,66)
C  66  FORMAT(' STICS OMNIDIRECTIONAL MATRIX RATES 2')
C      WRITE(81,17) FULL_L1_NAME(1:NAMELEN)
C
      IF(USE_SC_CLOCK.EQ.1)THEN
        WRITE(80,18) 'SpaceCraft'
      ELSE
        WRITE(80,18) 'Wall'
      ENDIF                    
      WRITE(80,19)FIFO,TGS,MODE_AT_START
      WRITE(80,70) 
  70  FORMAT(' Sci Rec#      Time      BR VS    E/Q     ',
     &' OMR 0   OMR 1   OMR 2   OMR 3   OMR 4  ',
     &' OMR 5   OMR 6   OMR 7   OMR 8   OMR 9  HMR Sum  SMR Sum',/,
     &'_____________________________________________________________________',
     &'_____________________________________________________________________')
C
C        IF(USE_SC_CLOCK.EQ.1)THEN
C          WRITE(81,18) 'SpaceCraft'
C        ELSE
C          WRITE(81,18) 'Wall'
C        ENDIF                    
C        WRITE(81,19)FIFO,TGS,MODE_AT_START
C        WRITE(81,80) 
C  80  FORMAT(' Sci Rec#      Time      BR VS    E/Q     ',
C     &' OMR 10  OMR 11  OMR 12  OMR 13  OMR 14 ',
C     &' OMR 15  OMR 16  OMR 17  OMR 18  OMR 19 BR0 Sum  BR1 Sum  BR2 Sum',/,
C     &'_________________________________________________________________________',
C     &'_________________________________________________________________________')
C
      OPEN(UNIT= 90, NAME=OUTFILENAME1, DEFAULTFILE='TABLE_DIR:', FORM='FORMATTED', 
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
     &'  FSR 1   FSR 2   FSR 3   FSR 4   FSR 5   FSR 6   UFSR    URSR    MFSR  ',
     &'  MRSR     MPF     MPR    RSR 1   RSR 2   RSR 3',/,
     &'________________________________________________________________________',
     &'_________________________________________________',
     &'________________________________________')
C
C      OPEN(UNIT= 91, NAME='ER2.STICS', DEFAULTFILE='TABLE_DIR:', FORM='FORMATTED', 
C     &     TYPE='NEW', RECL=168)
C      WRITE(91,92)
C  92  FORMAT(' STICS ENGINEERING RATES: DCR''s, TCR''s, and SSD''s')
C      WRITE(91,17) FULL_L1_NAME(1:NAMELEN)
CC
C      IF(USE_SC_CLOCK.EQ.1)THEN
C        WRITE(91,18) 'SpaceCraft'
C      ELSE
C        WRITE(91,18) 'Wall'
C      ENDIF                    
C      WRITE(91,19)FIFO,TGS,MODE_AT_START
C      WRITE(91,95) 
C  95  FORMAT(' Sci Rec#      Time      BR VS    E/Q     ',
C     &'  DCR 1   DCR 2   DCR 3   TCR 1   TCR 2   TCR 3   SSD 1   SSD 2   SSD 3',/,
C     &'_______________________________________________________________________',
C     &'__________________________________________')
C
C      OPEN(UNIT= 100, NAME='DR.STICS', DEFAULTFILE='TABLE_DIR:', FORM='FORMATTED', 
C     &     TYPE='NEW', RECL=168)
C      WRITE(100,96)
C  96  FORMAT(' STICS DIAGNOSTIC RATES (1 science record late)')
C      WRITE(100,17) FULL_L1_NAME(1:NAMELEN)
C
C      IF(USE_SC_CLOCK.EQ.1)THEN
C        WRITE(100,18) 'SpaceCraft'
C      ELSE
C        WRITE(100,18) 'Wall'
C      ENDIF                    
C      WRITE(100,19)FIFO,TGS,MODE_AT_START
C     WRITE(100,97) 
C      WRITE(100,98) 
C  97  FORMAT(T27,'     WRONG                                                              ',
C     &'     MASS    MASS   ENERGY   TIME    TIME     NQ      NQ    BKGND ')
C  98  FORMAT(' Sci Rec#      Time      BR',
C    &'    HEAD      D1      D2      D3      D4      D5      D6      D7      D8  ',
C     &' OVERFL  UNDERFL OVERFL  UNDERFL OVERFL  UNDERFL OVERFL  BASIC ',/,
C     &'__________________________________________________________________________________',
C     &'__________________________________________________________________________________')
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
C If HDB flag is bad, skip entire science record
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
          IF(SCI.EDB(J).ST_HMR_Qual.NE.0)GOTO 900
          IF(SCI.EDB(J).ST_SMR_Qual.NE.0)GOTO 900
          IF(SCI.EDB(J).ST_BR_Qual.NE.0)GOTO 900
          IF(SCI.EDB(J).ST_OMR_Qual.NE.0)GOTO 900
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
          ITEMP=HDB.BDH.DAT(24)
          ITEMP2=HDB.BDH.DAT(25)
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
C Compression codes for STICS High Resolution Matrix rates are in the HDB in byte 189, bit 1
C
          ITEMP = HDB.BDH.DAT(189)
          CODE_C = BTEST(ITEMP,1)
C
C Decompress STICS rates and get sum of HMR's:
C
          SUMHMR = 0
          DO 200 I=0,95
            STICS_RATES(I+1)=WIND_DECOMPRESS(STICS_RATE.EDB(J).RATE(I),CODE_C)
            SUMHMR = SUMHMR + STICS_RATES(I+1)
 200      CONTINUE
C
C Print out HMR data by anode. Thus we have 6 files, each containing 16 RATE values per 
C voltage step (one rate value per sector).
C
C          DO 270 K=0,5
C            WRITE(50+K,250) L1_SR_HEADER.SR_COUNT,
C     &                      YR2DIGIT,TIME_TO_USE.DOY,
C     &                      TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
C     &                      RATEFLAG,
C     &                      VSTEP,
C     &                      E_OVER_Q,
C     &                      (STICS_RATES(L+K*16),L=1,16)
C 250        FORMAT(1X,I8,1X,I2,1X,I3,1X,I2,':',I2.2,':',I2.2,1X,A1,1X,I2,1X,F8.3,16(1X,I7))
C 270      CONTINUE
C
C Compression codes for STICS Sectored Matrix rates are also in the HDB in byte 189, bit 1.
C Therefore, the value of CODE_C is unchanged. Decompress STICS rates, and get sum of SMR's:
C
          SUMSMR = 0
          DO 300 I=96,119
            STICS_RATES(I+1)=WIND_DECOMPRESS(STICS_RATE.EDB(J).RATE(I),CODE_C)
            SUMSMR = SUMSMR + STICS_RATES(I+1)
 300      CONTINUE
C
C Print out SMR data by MCP Thus we have 3 files, each containing 8 RATE values per 
C voltage step (one rate value per 2 sectors).
C
C          DO 370 K=0,2
C            WRITE(60+2*K,350) L1_SR_HEADER.SR_COUNT,
C     &                        YR2DIGIT,TIME_TO_USE.DOY,
C     &                        TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
C     &                        RATEFLAG,
C     &                        VSTEP,
C     &                        E_OVER_Q,
C     &                        (STICS_RATES(L+K*8+96),L=1,8)
C 350        FORMAT(1X,I8,1X,I2,1X,I3,1X,I2,':',I2.2,':',I2.2,1X,A1,1X,I2,1X,F8.3,8(1X,I7))
C 370      CONTINUE
C
C Compression codes for STICS Basic rates are also in the HDB in byte 189, bit 1.
C Therefore, the value of CODE_C is unchanged. Decompress STICS rates:
C
C          DO 400 I=120,143
C            STICS_RATES(I+1)=WIND_DECOMPRESS(STICS_RATE.EDB(J).RATE(I),CODE_C)
C 400      CONTINUE
C
C Get sum of BR0, BR1, and BR2 for output
C
C          DO 430 L=0,2
C            SUMBR(L) = 0
C            DO 420 M=1,8
C              SUMBR(L) = SUMBR(L) + STICS_RATES(120+L*8+M)
C 420        CONTINUE  
C 430      CONTINUE  
C
C Print out BR data. We have 3 files, each containing 8 RATE values per voltage step.
C
C          DO 470 K=0,2
C            WRITE(70+2*K,450) L1_SR_HEADER.SR_COUNT,
C     &                        YR2DIGIT,TIME_TO_USE.DOY,
C     &                        TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
C     &                        RATEFLAG,
C     &                        VSTEP,
C     &                        E_OVER_Q,
C     &                        (STICS_RATES(L+K*8+120),L=1,8)
C 450        FORMAT(1X,I8,1X,I2,1X,I3,1X,I2,':',I2.2,':',I2.2,1X,A1,1X,I2,1X,F8.3,8(1X,I7))
C 470      CONTINUE
C
C If we are in normal mode, we have 20 Omnidirectional Matrix rates and 15 engineering
C rates. In TGS mode (also known as diagnostic mode) we have 11 OMR's and 24 ER's.
C Check TGS mode flag and act appropraiately.
C
          ITEMP = STICS_RATE.EDB(J).RATE(179)
          IF(IBITS(ITEMP,7,1).NE.1)THEN                 ! Normal mode
C
C Compression codes for STICS Omnidirectional Matrix rates are also in the HDB in byte 189, 
C bit 1. Therefore, the value of CODE_C is unchanged. Decompress STICS rates:
C
            DO 500 I=144,163
              STICS_RATES(I+1)=WIND_DECOMPRESS(STICS_RATE.EDB(J).RATE(I),CODE_C)
 500        CONTINUE
C
C Print out OMR data by bin #. Thus we have 1 file, containing 10 RATE values per 
C voltage step (one rate value per bin #) and the sum of the HMR's and SMR's ...
C
            WRITE(80,550) L1_SR_HEADER.SR_COUNT,
     &                    YR2DIGIT,TIME_TO_USE.DOY,
     &                    TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
     &                    RATEFLAG,
     &                    VSTEP,
     &                    E_OVER_Q,
     &                    (STICS_RATES(L+144),L=1,10),
     &                    SUMHMR,SUMSMR
 550        FORMAT(1X,I8,1X,I2,1X,I3,1X,I2,':',I2.2,':',I2.2,1X,A1,1X,I2,1X,F8.3,10(1X,I7),2(1X,I8))
C
C ... and 1 file containing 10 RATE values per voltage step (one rate value per bin #) and the 
C sum of the BR's.
C
C            WRITE(81,560) L1_SR_HEADER.SR_COUNT,
C     &                      YR2DIGIT,TIME_TO_USE.DOY,
C     &                      TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
C     &                      RATEFLAG,
C    &                      VSTEP,
C     &                      E_OVER_Q,
C     &                      (STICS_RATES(L+154),L=1,10),
C     &                      SUMBR
C 560        FORMAT(1X,I8,1X,I2,1X,I3,1X,I2,':',I2.2,':',I2.2,1X,A1,1X,I2,1X,F8.3,10(1X,I7),3(1X,I8))
C
C Compression codes for STICS Direct Sensor rates are in the HDB in byte 189, bit 2. 
C except for TCR, UFSR, and URSR. The latter 2 come out only in TGS mode.
C
            ITEMP = HDB.BDH.DAT(189)
            CODE_C2 = BTEST(ITEMP,2)
C
C FSR's, RSR's, and DCR's 
C
            DO 600 I=164,172
              STICS_RATES(I+1)=WIND_DECOMPRESS(STICS_RATE.EDB(J).RATE(I),CODE_C2)
 600        CONTINUE
C
C The TCR compression code is in bit 1 
C
            DO 610 I=173,175
              STICS_RATES(I+1)=WIND_DECOMPRESS(STICS_RATE.EDB(J).RATE(I),CODE_C)
 610        CONTINUE
C
C The remaining ER (the SSD's) use the compression code from bit 2
C
            DO 620 I=176,178
              STICS_RATES(I+1)=WIND_DECOMPRESS(STICS_RATE.EDB(J).RATE(I),CODE_C2)
 620        CONTINUE
C
C Print out ER data. Thus we have 1 file containing 6 RATE values per voltage step...
C
            WRITE(90,650) L1_SR_HEADER.SR_COUNT,
     &                    YR2DIGIT,TIME_TO_USE.DOY,
     &                    TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
     &                    RATEFLAG,
     &                    VSTEP,
     &                    E_OVER_Q,
     &                    (STICS_RATES(L+164),L=1,6)
 650        FORMAT(1X,I8,1X,I2,1X,I3,1X,I2,':',I2.2,':',I2.2,1X,A1,1X,I2,1X,F8.3,3(1X,I7),
     &             72X,3(1X,I7))
C
C ...and 1 file containing 9 RATE values per voltage step.
C
C            WRITE(91,655) L1_SR_HEADER.SR_COUNT,
C     &                    YR2DIGIT,TIME_TO_USE.DOY,
C     &                    TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
C     &                    RATEFLAG,
C     &                    VSTEP,
C     &                    E_OVER_Q,
C     &                    (STICS_RATES(L+170),L=1,9)
C 655        FORMAT(1X,I8,1X,I2,1X,I3,1X,I2,':',I2.2,':',I2.2,1X,A1,1X,I2,1X,F8.3,9(1X,I7))
C
          ELSE                       ! TGS mode
C
C Compression codes for STICS Omnidirectional Matrix rates are also in the HDB in byte 189, 
C bit 1. Therefore, the value of CODE_C is unchanged. Decompress STICS rates:
C
            DO 700 I=144,154
              STICS_RATES(I+1)=WIND_DECOMPRESS(STICS_RATE.EDB(J).RATE(I),CODE_C)
 700        CONTINUE
C
C Print out OMR data by bin #. The first file  contains 10 RATE values per voltage step,
C and  the sum of the HMR's and SMR's. The second file will contain only 1 OMR value, 
C and the sum of the various BR's.
C
           WRITE(80,550) L1_SR_HEADER.SR_COUNT,
     &                    YR2DIGIT,TIME_TO_USE.DOY,
     &                    TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
     &                    RATEFLAG,
     &                    VSTEP,
     &                    E_OVER_Q,
     &                    (STICS_RATES(L+144),L=1,10),
     &                    SUMHMR,SUMSMR
C
C            WRITE(81,750) L1_SR_HEADER.SR_COUNT,
C     &                    YR2DIGIT,TIME_TO_USE.DOY,
C     &                    TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
C     &                    RATEFLAG,
C     &                    VSTEP,
C     &                    E_OVER_Q,
C     &                    STICS_RATES(155),
C     &                    SUMBR
C 750        FORMAT(1X,I8,1X,I2,1X,I3,1X,I2,':',I2.2,':',I2.2,1X,A1,1X,I2,1X,F8.3,(1X,I7),72X,3(1X,I8))
C
C FSR's
C
            DO 800 I=155,157
              STICS_RATES(I+1)=WIND_DECOMPRESS(STICS_RATE.EDB(J).RATE(I),CODE_C2)
 800        CONTINUE
C
C Additional FSR's present when in TGS mode are handled below. 
C
            DO 805 I=158,160
              STICS_RATES(I+1)=WIND_DECOMPRESS(STICS_RATE.EDB(J).RATE(I),CODE_C2)
 805        CONTINUE
C
C Then we need to handle the UFSR and URSR which have their compression code in bit 1
C
            DO 810 I=161,162
              STICS_RATES(I+1)=WIND_DECOMPRESS(STICS_RATE.EDB(J).RATE(I),CODE_C)
 810        CONTINUE
C
C The next rates use the compression code from bit 2 again.
C
            DO 820 I=163,172
              STICS_RATES(I+1)=WIND_DECOMPRESS(STICS_RATE.EDB(J).RATE(I),CODE_C2)
 820        CONTINUE
C
C The TCR compression code is in bit 1 
C
            DO 830 I=173,175
              STICS_RATES(I+1)=WIND_DECOMPRESS(STICS_RATE.EDB(J).RATE(I),CODE_C)
 830        CONTINUE
C
C The remaining ER use the compression code from bit 2
C
            DO 840 I=176,178
              STICS_RATES(I+1)=WIND_DECOMPRESS(STICS_RATE.EDB(J).RATE(I),CODE_C2)
 840        CONTINUE
C
C Print out ER data. Thus we have 1 file containing 15 RATE values per voltage step...
C
            WRITE(90,850) L1_SR_HEADER.SR_COUNT,
     &                    YR2DIGIT,TIME_TO_USE.DOY,
     &                    TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
     &                    RATEFLAG,
     &                    VSTEP,
     &                    E_OVER_Q,
     &                    (STICS_RATES(L+155),L=1,15)
 850        FORMAT(1X,I8,1X,I2,1X,I3,1X,I2,':',I2.2,':',I2.2,1X,A1,1X,I2,1X,F8.3,15(1X,I7))
C
C ...and 1 file containing 9 RATE values per voltage step.
C
C            WRITE(91,855) L1_SR_HEADER.SR_COUNT,
C     &                    YR2DIGIT,TIME_TO_USE.DOY,
C     &                    TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
C     &                    RATEFLAG,
C     &                    VSTEP,
C     &                    E_OVER_Q,
C     &                    (STICS_RATES(L+170),L=1,9)
C 855        FORMAT(1X,I8,1X,I2,1X,I3,1X,I2,':',I2.2,':',I2.2,1X,A1,1X,I2,1X,F8.3,9(1X,I7))
          ENDIF

        ENDIF
 900  CONTINUE
C
C STICS Diagnostic rates come out once per science record, in the HDB. Their compression 
C codes are in the HDB in byte 189, bit 1. Therefore, the value of CODE_C is unchanged. 
C Decompress STICS rates:
C
C      DO 950 I=1,17
C        D_RATES(I)=WIND_DECOMPRESS(HDB.BDH.DAT(I+236),CODE_C)
C 950  CONTINUE
C
C Print out Diagnostic data. Thus we have 1 file, containing 17 RATE values per science
C record.
C
C      IF(HDB.BDH.Qual.EQ.0)THEN
C        WRITE(100,970) L1_SR_HEADER.SR_COUNT,
C     &                 YR2DIGIT,TIME_TO_USE.DOY,
C     &                 TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
C     &                 RATEFLAG,
C     &                 (D_RATES(L),L=1,17)
C 970    FORMAT(1X,I8,1X,I2,1X,I3,1X,I2,':',I2.2,':',I2.2,1X,A1,17(1X,I7))
C      ENDIF
C      
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
C      DO 2000 I=0,5
C        CLOSE(UNIT=50+I)                        ! Close HMR files
C        IF(MOD(I,2).NE.1)THEN           
C          CLOSE(UNIT=60+I)                      ! Close SMR files
C          CLOSE(UNIT=70+I)                      ! Close BR files
C        ENDIF
        CLOSE(UNIT=80)                          ! Close OMR file
C        CLOSE(UNIT=81)                          ! Close OMR file
        CLOSE(UNIT=90)                          ! Close ER file
C        CLOSE(UNIT=100)                         ! Close DR file
C2000  CONTINUE
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
