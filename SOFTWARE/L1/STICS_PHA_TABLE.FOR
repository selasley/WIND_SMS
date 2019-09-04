CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      PROGRAM STICS_PHA_TABLE
C This program reads a WIND L1 data file and dissects the PHA data, printing 
C it in a table vs time and science record number
C 
C Created 	17-NOV-1994	JOHN PAQUETTE
C
C This version:   1.13   16-MAR-1995
C
C Revision history:
C       22-NOV-1994     L. Bleau        added heading showing filename, which clock
C	29-NOV-1994	L. Bleau	add file selection via menu
C	29-NOV-1994	L. Bleau	change output directory to TABLE_DIR:
C	30-NOV-1994	L. Bleau	add local time variable TIME_TO_USE to
C					allow use of either time
C        5-DEC-1994     J.PAQUETTE      change format to output leading 0's for min,sec
C        9-DEC-1994     J.PAQUETTE      moved initialization of STARTFOUND and ENDFOUND 
C                                       above label 100, and corrected start-finding logic
C	16-DEC-1994	J.PAQUETTE	allow user to input month and day instead of DOY
C        4-JAN-1994     J.PAQUETTE      added title to output file, added trigger mode 
C                                       to header
C       17-JAN-1995     J.PAQUETTE      fixed error in range calculation (spotted by ABG)
C       23-JAN-1995     J.PAQUETTE      first goodspin now required to be even at low BR
C       31-JAN-1995     J.PAQUETTE      corrected error in DPPS voltage checking
C        7-FEB-1995     J.PAQUETTE      added check to see that pointer to 1st good spin
C                                       does not exceed 59
C        8-FEB-1995     J.PAQUETTE      tighten logic on pointer
C       21-FEB-1995     J.PAQUETTE      if BR, PHA, or overall quality flag is bad, skip to next spin
C       22-FEB-1995	L. Bleau	change ref to HDB, omit .BDH field
C       27-FEB-1995     J.PAQUETTE      changed DO 800 loop to increment by 2 at LBR; corrected
C                                       logic in selection of 1st good spin
C       16-MAR-1995     J.PAQUETTE      if HDB is bad, calculate E_OVER_Q as if DPPS on
C
CDEC$ IDENT '1.13'
      INCLUDE	'INC_DIR:EDBDEF.INC/NOLIST'
      INCLUDE   'INC_DIR:L1FMT.INC/NOLIST'
      INCLUDE   '($RMSDEF)'
      INTEGER*4 STATUS,L1_LUN/19/,VSTEP,MONTH,DAYOFMONTH,TRIGGER_MODE_CODE,INCREMENT
      REAL*4 D1/6.190722/,D2/1.1225857/      ! Default values used for E/Q calculation
      REAL*4 E_OVER_Q
      CHARACTER L1FILENAME*80
      CHARACTER FULL_L1_NAME*80, FIFO*3, TGS*3, RATEFLAG*1
      CHARACTER*17 MODE(10) /'Science mode     ','Engineering mode ',
     &                       'Maneuver mode    ','Contingency mode ',
     &                       'Science mode     ','Engineering mode ',
     &                       'Maneuver mode    ','Contingency mode ',
     &                       'Transitional mode','Unknown mode     '/,
     &             MODE_AT_START
      CHARACTER*20 TRIGGER_MODE(0:7) /'E                    ','Tn                  ',
     &                                '(Tn.E)+(Tn.En)       ','Tn.En               ',
     &                                'E+T                  ','(E.T)+(Tn.E)+(Tn.En)',
     &                                'C.(E+T)              ','(E.!T)+(Tn.En)      '/
      LOGICAL*1 BITRATE
      LOGICAL CODE_C,CODE_C2,STARTFOUND,ENDFOUND,USE_SC_CLOCK,COMPARE_TIME_GE,GOODSPIN
      INTEGER I,J,K,L,N,NN,CNTR,ITEMP,ITEMP2,NAMELEN
      INTEGER*4 STOP_ID,START_ID,ENERGY,SECTOR,SSD_ID,TIME_OF_FLIGHT,PSTART,RANGE
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
C Open L1 file for input, and read first SR, Core, HDB, and STICS_RATES
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
C
C Check FIFO mode, telemetry mode, TGS mode, and trigger mode; place info in header.
C
      ITEMP = HDB.DAT(221)
      IF(IBITS(ITEMP,7,1).EQ.1)THEN
        FIFO='ON '
      ELSE
        FIFO='OFF'
      ENDIF
C  
      ITEMP = SCI.EDB(N).TLM_Mode
      IF(ITEMP.EQ.0)ITEMP=10
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
      ITEMP = HDB.DAT(25)
      TRIGGER_MODE_CODE = IBITS(ITEMP,1,3)
C
C Open PHA file for output
C
      OPEN(UNIT= 50, NAME='PHA.STICS', DEFAULTFILE='TABLE_DIR:', FORM='FORMATTED', 
     &     TYPE='NEW', RECL=168)
      WRITE(50,15)
  15  FORMAT(' STICS PHA')
      WRITE(50,16) FULL_L1_NAME(1:NAMELEN)
  16  FORMAT(T10,'Input Level 1 Filename: ',A)
      IF(USE_SC_CLOCK.EQ.1)THEN
        WRITE(50,17) 'SpaceCraft'
        WRITE(50,18)TRIGGER_MODE(TRIGGER_MODE_CODE)
        WRITE(50,19)FIFO,TGS,MODE_AT_START
        WRITE(50,20) SCI.EDB(N).SC_TIME.YEAR,SCI.EDB(N).SC_TIME.DOY
      ELSE
        WRITE(50,17) 'Wall'
        WRITE(50,18)TRIGGER_MODE(TRIGGER_MODE_CODE)
        WRITE(50,19)FIFO,TGS,MODE_AT_START
        WRITE(50,20) SCI.EDB(N).W_TIME.YEAR,SCI.EDB(N).W_TIME.DOY
      ENDIF
  17  FORMAT(T10,'Clock selected:         ',A/)
  18  FORMAT(T10,'Trigger mode is ',A20)
  19  FORMAT(T10,'FIFO mode is ',A3,T30,'TGS mode is ',A3,T50,' Telemetry mode is ',A17)
  20  FORMAT(' YEAR: ',I4,' DAY: ',I3,/,' Sci Rec#   Time   VS     E/Q',
     &'    Stop ID  Start ID/Range  Energy(Ed)  Sector  SSD ID  Time of Flight(Td)',
     &'   START   RANGE',/,
     &'____________________________________________________________',
     &'____________________________________________________________')
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
C Output STICS PHA one science record.
C
      DO 800 J=0,58,INCREMENT
        IF((.NOT.SCI.EDB(J).BITRATE).AND.(MOD(J,2).NE.0))GOTO 800
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
C Check quality flags. If PHA, BR, or overall quality flags are bad, skip this spin
C
          IF(BITRATE)THEN
            IF(SCI.EDB(J).Qual.NE.0)GOTO 800
          ELSE
            IF((SCI.EDB(J).Qual.NE.0).OR.(SCI.EDB(J+1).Qual.NE.0))GOTO 800
          ENDIF
          IF(SCI.EDB(J).ST_BR_Qual.NE.0)GOTO 800
          IF(SCI.EDB(J).ST_P_Qual.NE.0)GOTO 800
C
C Get voltage step and determine if the deflection systems are on.
C
          ITEMP = STICS_RATE.EDB(J).RATE(179)
          VSTEP = IBITS(ITEMP,0,5)
          ITEMP=HDB.DAT(24)
          ITEMP2=HDB.DAT(25)
C
C The following line assumes that both deflection systems must be on to calculate e/q. 
C If the either DPPS high voltage is off, set e/q to zero.
C
          IF(HDB.Qual.NE.0)THEN
            E_OVER_Q = D1 * (D2**VSTEP)
          ELSE
            IF(IAND(IBITS(ITEMP,7,1), IBITS(ITEMP2,0,1)).EQ.1)THEN
              E_OVER_Q = D1 * (D2**VSTEP)
            ELSE
              E_OVER_Q = 0
            ENDIF
          ENDIF
C
C Get bit rate. If SCI.EDB(J).BITRATE is .TRUE. then high bit rate. If its .FALSE. (0)
C then it may really be Low Bit Rate, or it may simply have not been set. Check the
C YEAR field of the time; if its 0 then the bit rate is unknown. If the year is not 0
C then we have low bit rate data.
C
          IF(SCI.EDB(J).BITRATE)THEN
            RATEFLAG='H'                ! High bit rate
          ELSEIF(TIME_TO_USE.YEAR.NE.0)THEN
            RATEFLAG='L'                ! Low bit rate
          ELSE
            RATEFLAG='?'                ! Bit rate indeterminate, due to bad quality data
          ENDIF
C
C Do for all STICS PHA words
C
          DO 300 I=0, (SCI.EDB(J).STICS_Len-1) * 4, 4
C
C Get parts of PHA word
C
            CALL DISSECT_PHA(STICS_PHA.EDB(J).PHA(I),
     &                       STOP_ID,START_ID,ENERGY,SECTOR,SSD_ID,TIME_OF_FLIGHT)
C
C Get START and RANGE from START_ID
C
            PSTART = START_ID/3
            RANGE = START_ID - PSTART * 3
C
C Print out PHA data by word. There will thus be one row per PHA word, and no enties in the
C table for voltage steps that have no PHA words.
C
            WRITE(50,250) L1_SR_HEADER.SR_COUNT,
     &                      TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
     &                      VSTEP,
     &                      E_OVER_Q,
     &                      STOP_ID,
     &                      START_ID,
     &                      ENERGY,
     &                      SECTOR,
     &                      SSD_ID,
     &                      TIME_OF_FLIGHT,
     &                      PSTART,
     &                      RANGE
 250        FORMAT(1X,I8,1X,I2,':',I2.2,':',I2.2,1X,I2,1X,F8.3,5X,I2,11X,I2,11X,I3,8X,I2,6X,
     &             I2,10X,I4,13X,I1,7X,I1)
 300      CONTINUE
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
      CLOSE(UNIT=50)                            ! Close PHA file
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
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE DISSECT_PHA(PHA,STOP_ID,START_ID,ENERGY,SECTOR,SSD_ID,TIME_OF_FLIGHT)
C
C This subroutine does bit computations to get Stop ID, Start ID, Energy, Sector, SSD ID,
C and Time of flight for STICS PHA words on WIND
C
      INTEGER*4 STOP_ID,START_ID,ENERGY,SECTOR,SSD_ID,TIME_OF_FLIGHT
      INTEGER*4 ITEMP
      BYTE PHA(4)
C
C      WRITE(50,101) PHA
C101   FORMAT(4(1X,Z2.2))
C
      STOP_ID = 0
      ITEMP = PHA(1)
      CALL MVBITS( ITEMP, 6, 2, STOP_ID, 0 )
C
      START_ID = 0
      CALL MVBITS( ITEMP, 1, 5, START_ID, 0 )
C
      ENERGY = ISHFT( IAND( ITEMP, 1 ), 8 )
      ITEMP = PHA(2)
      ENERGY = ENERGY + IAND( ITEMP, 255 )
C
      SECTOR = 0
      ITEMP = PHA(3)
      CALL MVBITS( ITEMP, 4, 4, SECTOR, 0 )
C
      SSD_ID = 0
      CALL MVBITS( ITEMP, 2, 2, SSD_ID, 0 )
C
      TIME_OF_FLIGHT = ISHFT( IAND( ITEMP, 3 ), 8 )
      ITEMP = PHA(4)
      TIME_OF_FLIGHT = TIME_OF_FLIGHT + IAND( ITEMP, 255 )
C
      RETURN
      END
