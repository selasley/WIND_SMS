CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      PROGRAM MASS_PHA_TABLE
C This program reads a WIND L1 data file and dissects the PHA data, printing 
C it in a table vs time and science record number
C 
C Created 	17-NOV-1994	JOHN PAQUETTE
C
C This version:   1.5   16-DEC-1994
C
C Revision history:
C       22-NOV-1994     L. Bleau        added heading showing filename, which clock
C	29-NOV-1994	L. Bleau	add file selection via menu
C	29-NOV-1994	L. Bleau	change output directory to TABLE_DIR:
C	30-NOV-1994	L. Bleau	add local time variable TIME_TO_USE to
C					allow use of either time
C        1-DEC-1994     J.PAQUETTE      changed e/q numbers, made minor output format changes
C        9-DEC-1994     J.PAQUETTE      space inserted before 'MASS', moved initialization
C                                       of STARTFOUND and ENDFOUND above label 100, and
C                                       corrected start-finding logic
C	16-DEC-1994	J.PAQUETTE	allow user to input month and day instead of DOY
C
CDEC$ IDENT '1.5'
      INCLUDE	'INC_DIR:EDBDEF.INC/NOLIST'
      INCLUDE   'INC_DIR:L1FMT.INC/NOLIST'
      INCLUDE   '($RMSDEF)'
      INTEGER*4 STATUS,L1_LUN/19/,VSTEP,MONTH,DAYOFMONTH
      REAL*4 D1/6.190722/,D2/1.1225857/      ! Default values used for E/Q calculation
      REAL*4 E_OVER_Q
      CHARACTER DEFAULT_L1FILENAME*6/'L1FNEW'/
      CHARACTER L1FILENAME*80
      CHARACTER FULL_L1_NAME*80
      LOGICAL CODE_C,CODE_C2,STARTFOUND,ENDFOUND,USE_SC_CLOCK,COMPARE_TIME_GE,GOODSPIN
      INTEGER I,J,K,L,N,NN,CNTR,ITEMP,NAMELEN,VSTEPMIN,VSTEPMAX
      INTEGER SECTOR,RANGE,ANODE,TIME_OF_FLIGHT
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
C Prompt user for minimum and maximum VSTEP
C
      TYPE 7
   7  FORMAT(' Enter minimum and maximum VSTEP: '$)
      ACCEPT*, VSTEPMIN,VSTEPMAX
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
C Open L1 file for input, and read first SR and Core
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
C
C Find first good spin, get year and DOY from it.
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
C Open PHA file for output
C
      OPEN(UNIT= 50, NAME='PHA.MASS', DEFAULTFILE='TABLE_DIR:', FORM='FORMATTED', 
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
  20  FORMAT(' YEAR: ',I4,' DAY: ',I3,/,' Sci Rec#   Time   VS     E/Q',
     &'   Sector  Range  Anode  Time of Flight(Td)',/,
     &'________________________________________________________________________')
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
C Output MASS PHA one science record.
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
C Get voltage step and e/q. If voltage step is outside the user-specified range, skip to 
C the next spin.
C
          ITEMP = MASS_RATE.EDB(J).RATE(25)
          VSTEP = IBITS(ITEMP,0,6)
          IF((VSTEP.LT.VSTEPMIN).OR.(VSTEP.GT.VSTEPMAX))GOTO 800
          E_OVER_Q = 0.5156 * 1.05135**VSTEP
C
C Do for all MASS PHA words
C
          DO 300 I=0, (SCI.EDB(J).MASS_Len-1) * 2, 2
C
C Get parts of PHA word
C
            CALL DISSECT_PHA(MASS_PHA.EDB(J).PHA(I),SECTOR,RANGE,ANODE,TIME_OF_FLIGHT)
C
C Print out PHA data by word. There will thus be one row per PHA word, and no enties in the
C table for voltage steps that have no PHA words.
C
            WRITE(50,250) L1_SR_HEADER.SR_COUNT,
     &                      TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
     &                      VSTEP,
     &                      E_OVER_Q,
     &                      SECTOR,
     &                      RANGE,
     &                      ANODE,
     &                      TIME_OF_FLIGHT
 250        FORMAT(1X,I8,1X,I2,':',I2.2,':',I2.2,1X,I2,1X,F8.3,5X,I1,9X,I1,7X,I1,10X,I4)
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
9080  TYPE *,'ERROR GETTING MASS DATA; STATUS=',STATUS
      STOP
9090  TYPE *,'ERROR CLOSING L1 FILE; STATUS=',STATUS
      STOP
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE DISSECT_PHA(PHA,SECTOR,RANGE,ANODE,TIME_OF_FLIGHT)
      INTEGER*4 SECTOR,RANGE,ANODE,TIME_OF_FLIGHT
      INTEGER*4 ITEMP
      BYTE PHA(2)
C
      SECTOR = 0
      ITEMP = PHA(1)
      CALL MVBITS( ITEMP, 7, 1, SECTOR, 0 )
C
      RANGE = 0
      CALL MVBITS( ITEMP, 6, 1, RANGE, 0 )
C
      ANODE = 0
      CALL MVBITS( ITEMP, 4, 2, ANODE, 0 )
C
      TIME_OF_FLIGHT = ISHFT( IAND( ITEMP, 15 ), 8 )
      ITEMP = PHA(2)
      TIME_OF_FLIGHT = TIME_OF_FLIGHT + IAND( ITEMP, 255 )
C
      RETURN
      END
