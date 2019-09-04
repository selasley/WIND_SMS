CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      PROGRAM SWICS_RATE_TABLE
C This program reads a WIND L1 data file and decompresses the rate data, printing 
C it in a table vs time and science record number
C 
C Created 	14-NOV-1994	JOHN PAQUETTE
C
C This version: 18-JAN-1995
C
C Revision history:
C       15-NOV-1994     J.PAQUETTE      SWICS instead of STICS
C       22-NOV-1994     L. Bleau        added heading showing filename, which clock
C	29-NOV-1994	L. Bleau	add file selection via menu
C	29-NOV-1994	L. Bleau	change output directory to TABLE_DIR:
C	30-NOV-1994	L. Bleau	add local time variable TIME_TO_USE to
C					allow use of either time
C        5-DEC-1994     J.PAQUETTE      changed format so leading 0's in min, sec printed
C        9-DEC-1994     J.PAQUETTE      moved initialization of STARTFOUND and ENDFOUND 
C                                       above label 100, and corrected start-finding logic;
C	16-DEC-1994	J.PAQUETTE	allow user to input month and day instead of DOY
C       18-JAN-1995     J.PAQUETTE      created variable HKBYTE_INDEX that points to byte
C                                       at end of SWICS rates holding Vstep and active PHA 
C
CDEC$ IDENT  '1.5'
      INCLUDE	'INC_DIR:EDBDEF.INC/NOLIST'
      INCLUDE   'INC_DIR:L1FMT.INC/NOLIST'
      INCLUDE   '($RMSDEF)'
      INTEGER*4 STATUS,L1_LUN/19/,SWICS_RATES(116),WIND_DECOMPRESS,VSTEP,MONTH,DAYOFMONTH
      REAL*4 E_OVER_Q
      CHARACTER L1FILENAME*80,BRNAME*9,BR2NAME*10
      CHARACTER FULL_L1_NAME*80
      INTEGER UNIT_TABLE(0:2,3)/52,53,54,53,52,54,54,52,53/
      LOGICAL CODE_C,CODE_C2,STARTFOUND,ENDFOUND,USE_SC_CLOCK,COMPARE_TIME_GE,GOODSPIN
      INTEGER I,J,K,L,N,NN,CNTR,ITEMP,ACTIVE_PHA,BRMAX,MRMAX,MEMAX,NAMELEN,HKBYTE_INDEX
C
      RECORD /CORE/ SCI
      RECORD /HDBstr/ HDB
      RECORD /SW_RATE/ SWICS_RATE
      RECORD /SW_PHA/ SWICS_PHA
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
      CALL WIND_L1_GET_SWICS( L1_LUN, SWICS_RATE, SWICS_PHA, STATUS )
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
      DO 30 J=0,1
        WRITE(BRNAME,15) J
  15    FORMAT('BR',I1,'.SWICS')
        OPEN(UNIT= 50+J, NAME=BRNAME, DEFAULTFILE='TABLE_DIR:', FORM='FORMATTED', 
     &       TYPE='NEW', RECL=168)
        WRITE(50+J,16) FULL_L1_NAME(1:NAMELEN)
  16    FORMAT(T10,'Input Level 1 Filename: ',A)
        IF(USE_SC_CLOCK.EQ.1)THEN
          WRITE(50+J,17) 'SpaceCraft'
          WRITE(50+J,20) SCI.EDB(N).SC_TIME.YEAR,SCI.EDB(N).SC_TIME.DOY
        ELSE
          WRITE(50+J,17) 'Wall'
          WRITE(50+J,20) SCI.EDB(N).W_TIME.YEAR,SCI.EDB(N).W_TIME.DOY
        ENDIF
  17    FORMAT(T10,'Clock selected:         ',A/)
  20    FORMAT(' YEAR: ',I4,' DAY: ',I3,/,' Sci Rec#   Time   VS    E/Q     ',
     &  '   S0      S1      S2      S3      S4      S5      S6      S7  ',
     &  '   S8      S9      S10     S11     S12     S13     S14     S15',/,
     &  '_____________________________________________________',
     &  '_____________________________________________________',
     &  '____________________________________________________')
  30  CONTINUE
C
      DO 50 J=1,3
        BR2NAME='BR2'//CHAR(64+J)//'.SWICS'     ! J=1-->A, J=2-->B, J=3-->C
        OPEN(UNIT= 51+J, NAME=BR2NAME, DEFAULTFILE='TABLE_DIR:', FORM='FORMATTED', 
     &       TYPE='NEW', RECL=168)
        WRITE(51+J,16) FULL_L1_NAME(1:NAMELEN)
        IF(USE_SC_CLOCK.EQ.1)THEN
          WRITE(51+J,17) 'SpaceCraft'
          WRITE(51+J,40) SCI.EDB(N).SC_TIME.YEAR,SCI.EDB(N).SC_TIME.DOY
        ELSE
          WRITE(51+J,17) 'Wall'
          WRITE(51+J,40) SCI.EDB(N).W_TIME.YEAR,SCI.EDB(N).W_TIME.DOY
        ENDIF
  40    FORMAT(' YEAR: ',I4,' DAY: ',I3,/,' Sci Rec#   Time   VS     E/Q    ',
     &  '   S0      S1      S2      S3      S4      S5      S6      S7  ',
     &  '   S8      S9      S10     S11     S12     S13     S14     S15',/,
     &  '_____________________________________________________',
     &  '_____________________________________________________',
     &  '____________________________________________________')
  50  CONTINUE
C
      OPEN(UNIT= 70, NAME='MR1.SWICS', DEFAULTFILE='TABLE_DIR:', FORM='FORMATTED', 
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
     &'    MR 0    MR 1    MR 2    MR 3    MR 4    MR 5',
     &'    MR 6    MR 7    MR 8    MR 9    MR 10',/,
     &'____________________________________________________________',
     &'____________________________________________________________')
C
      OPEN(UNIT= 71, NAME='MR2.SWICS', DEFAULTFILE='TABLE_DIR:', FORM='FORMATTED', 
     &     TYPE='NEW', RECL=168)
      WRITE(71,16) FULL_L1_NAME(1:NAMELEN)
      IF(USE_SC_CLOCK.EQ.1)THEN
        WRITE(71,17) 'SpaceCraft'
        WRITE(71,70) SCI.EDB(N).SC_TIME.YEAR,SCI.EDB(N).SC_TIME.DOY
      ELSE
        WRITE(71,17) 'Wall'
        WRITE(71,70) SCI.EDB(N).W_TIME.YEAR,SCI.EDB(N).W_TIME.DOY
      ENDIF
  70  FORMAT(' YEAR: ',I4,' DAY: ',I3,/,' Sci Rec#   Time   VS     E/Q   ',
     &'   MR 11   MR 12   MR 13   MR 14   MR 15   MR 16   MR 17   MR 18   MR 19   MR 20',/,
     &'________________________________________________________',
     &'________________________________________________________')
C
      OPEN(UNIT= 80, NAME='ME.SWICS', DEFAULTFILE='TABLE_DIR:', FORM='FORMATTED', 
     &     TYPE='NEW', RECL=168)
      WRITE(80,16) FULL_L1_NAME(1:NAMELEN)
C
      IF(USE_SC_CLOCK.EQ.1)THEN
        WRITE(80,17) 'SpaceCraft'
        WRITE(80,80) SCI.EDB(N).SC_TIME.YEAR,SCI.EDB(N).SC_TIME.DOY
      ELSE
        WRITE(80,17) 'Wall'
        WRITE(80,80) SCI.EDB(N).W_TIME.YEAR,SCI.EDB(N).W_TIME.DOY
      ENDIF
  80  FORMAT(' YEAR: ',I4,' DAY: ',I3,/,' Sci Rec#   Time   VS     E/Q    ',
     &'  ME 0    ME 1    ME 2    ME 3    ME 4    ME 5    ME 6    ME 7  ',/,
     &'________________________________________________',
     &'_______________________________________________')
C
      OPEN(UNIT= 90, NAME='ER.SWICS', DEFAULTFILE='TABLE_DIR:', FORM='FORMATTED', 
     &     TYPE='NEW', RECL=168)
      WRITE(90,16) FULL_L1_NAME(1:NAMELEN)
C
      IF(USE_SC_CLOCK.EQ.1)THEN
        WRITE(90,17) 'SpaceCraft'
        WRITE(90,90) SCI.EDB(N).SC_TIME.YEAR,SCI.EDB(N).SC_TIME.DOY
      ELSE
        WRITE(90,17) 'Wall'
        WRITE(90,90) SCI.EDB(N).W_TIME.YEAR,SCI.EDB(N).W_TIME.DOY
      ENDIF
  90  FORMAT(' YEAR: ',I4,' DAY: ',I3,/,' Sci Rec#   Time   VS     E/Q     ',
     &'  FSR     DCR     TCR    SSD 1   SSD 2   SSD 3',/,
     &'________________________________________',
     &'________________________________________')
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
C Get SWICS rates and PHA
C
      CALL WIND_L1_GET_SWICS( L1_LUN, SWICS_RATE, SWICS_PHA, STATUS )
      IF (STATUS .NE. 1) GOTO 9080
C
C Decompress SWICS rates for one science record and output
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
C Get bitrate, set loop limits, find out where housekeeping byte is
C
           IF(SCI.EDB(J).BITRATE)THEN   ! High bit rate
             BRMAX=80
             MRMAX=101
             MEMAX=109
             HKBYTE_INDEX = 115
           ELSE                         ! Low bit rate
             BRMAX=26
             MRMAX=47
             MEMAX=55
             HKBYTE_INDEX = 61
           ENDIF          
C
C Get voltage step and e/q
C
          ITEMP = SWICS_RATE.EDB(J).RATE(HKBYTE_INDEX)
          VSTEP = IBITS(ITEMP,0,6)
          E_OVER_Q = 0.5 * 60.0**(VSTEP/59.0)
C
C Compression codes for all SWICS rates are in the HDB in byte 189, bit 2
C
          ITEMP = HDB.BDH.DAT(189)
          CODE_C = BTEST(ITEMP,2)
C
C Decompress SWICS rates
C
          DO 200 I=0,114
            SWICS_RATES(I+1)=WIND_DECOMPRESS(SWICS_RATE.EDB(J).RATE(I),CODE_C)
 200      CONTINUE
C
C Print out BR data. At high bit rate  we have 5 files, each containing 16 RATE values per 
C voltage step (one rate value per sector).
C
          IF(SCI.EDB(J).BITRATE)THEN
            DO 220 K=0,4
              WRITE(50+K,210) L1_SR_HEADER.SR_COUNT,
     &                        TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
     &                        VSTEP,
     &                        E_OVER_Q,
     &                        (SWICS_RATES(L+K*16),L=1,16)
 210          FORMAT(1X,I8,1X,I2,':',I2.2,':',I2.2,1X,I2,1X,F8.3,16(1X,I7))
 220        CONTINUE
C
C At low bit rate we have the same files, but the first 3 contain only 8 values per voltage
C step (1 per 2 sectors) and the the last two contain only 1 value per voltage step (1 per
C 16 sectors).
C
          ELSE
            DO 240 K=0,1
              WRITE(50+K,230) L1_SR_HEADER.SR_COUNT,
     &                        TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
     &                        VSTEP,
     &                        E_OVER_Q,
     &                        (SWICS_RATES(L+K*8),L=1,8)
 230          FORMAT(1X,I8,1X,I2,':',I2.2,':',I2.2,1X,I2,1X,F8.3,8(1X,I7,8X))
 240        CONTINUE
C
            ITEMP = SWICS_RATE.EDB(J).RATE(HKBYTE_INDEX)
            ACTIVE_PHA=IBITS(ITEMP,6,2)
            WRITE(UNIT_TABLE(ACTIVE_PHA,1),230) L1_SR_HEADER.SR_COUNT,
     &                      TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
     &                      VSTEP,
     &                      E_OVER_Q,
     &                      (SWICS_RATES(L+16),L=1,8)
C
            DO 300 K=2,3
              WRITE(UNIT_TABLE(ACTIVE_PHA,K),250) L1_SR_HEADER.SR_COUNT,
     &                        TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
     &                        VSTEP,
     &                        E_OVER_Q,
     &                        SWICS_RATES(22+K)
 250          FORMAT(1X,I8,1X,I2,':',I2.2,':',I2.2,1X,I2,1X,F8.3,1X,I7)
 300        CONTINUE
          ENDIF
C
C Print out MR data. We have 2 files, one containing 11 RATE values per voltage step, the
C other 10 values per voltage step (one rate value per 16 sectors). They are broken up 
C this way because 21 values will not fit across the printed page.
C
          WRITE(70,350) L1_SR_HEADER.SR_COUNT,
     &                  TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
     &                  VSTEP,
     &                  E_OVER_Q,
     &                  (SWICS_RATES(L+BRMAX),L=1,11)
 350      FORMAT(1X,I8,1X,I2,':',I2.2,':',I2.2,1X,I2,1X,F8.3,11(1X,I7))
C
          WRITE(71,360) L1_SR_HEADER.SR_COUNT,
     &                  TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
     &                  VSTEP,
     &                  E_OVER_Q,
     &                  (SWICS_RATES(L+BRMAX),L=12,21)
 360      FORMAT(1X,I8,1X,I2,':',I2.2,':',I2.2,1X,I2,1X,F8.3,10(1X,I7))
C
C Print out ME data. We have 1 file, each containing 8 Matrix Element values per voltage step.
C
          WRITE(80,450) L1_SR_HEADER.SR_COUNT,
     &                      TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
     &                      VSTEP,
     &                      E_OVER_Q,
     &                      (SWICS_RATES(L+MRMAX),L=1,8)
 450      FORMAT(1X,I8,1X,I2,':',I2.2,':',I2.2,1X,I2,1X,F8.3,8(1X,I7))
 470      CONTINUE
C
C Print out ER data. Thus we have 1 file containing 6 RATE values per voltage step.
C
          WRITE(90,650) L1_SR_HEADER.SR_COUNT,
     &                  TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
     &                  VSTEP,
     &                  E_OVER_Q,
     &                  (SWICS_RATES(L+MEMAX),L=1,6)
 650      FORMAT(1X,I8,1X,I2,':',I2.2,':',I2.2,1X,I2,1X,F8.3,6(1X,I7))
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
      DO 1000 I=0,4
        CLOSE(UNIT=50+I)                        ! Close BR files
1000  CONTINUE
      CLOSE(UNIT=70)                            ! Close MR files
      CLOSE(UNIT=71)                            ! Close MR file
      CLOSE(UNIT=80)                            ! Close ME file
      CLOSE(UNIT=90)                            ! Close ER file
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
9080  TYPE *,'ERROR GETTING SWICS DATA; STATUS=',STATUS
      STOP
9090  TYPE *,'ERROR CLOSING L1 FILE; STATUS=',STATUS
      STOP
      END
