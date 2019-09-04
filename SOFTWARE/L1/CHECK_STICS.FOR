CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      PROGRAM CHECK_STICS
C This program reads a WIND L1 data file and decompresses the rate data, then
C checks for internal consistency in the STICS data. The program CHECK_MASS was
C used as a model.
C 
C Created 	31-JAN-1995	JOHN PAQUETTE
C
C This version:   1.1   22-FEB-1995
C
C Revision history:
C       14-FEB-1995     J.PAQUETTE      added check on BR,PHA,and ER quality flags
C       22-FEB-1995	L. Bleau	change ref to HDB, omit .BDH field
C
CDEC$ IDENT  '1.1'
      INCLUDE	'INC_DIR:EDBDEF.INC/NOLIST'
      INCLUDE   'INC_DIR:L1FMT.INC/NOLIST'
      INCLUDE   '($RMSDEF)'
      BYTE NEXTCODE
      INTEGER*4 STATUS,L1_LUN/19/,STICS_RATES(116),WIND_DECOMPRESS,VSTEP,MONTH,DAYOFMONTH
      REAL*4 D1/6.190722/,D2/1.1225857/      ! Default values used for E/Q calculation
      REAL*4 E_OVER_Q
      REAL*8 OLDTIME,TIMEDIFF
      CHARACTER L1FILENAME*80
      CHARACTER FULL_L1_NAME*80
      LOGICAL CODE_C,CODE_C2,STARTFOUND,ENDFOUND,USE_SC_CLOCK,COMPARE_TIME_GE,GOODSPIN
      INTEGER I, J, K, L, N, NN, CNTR, ITEMP, ITEMP2, NAMELEN
      INTEGER*4 STOP_ID,START_ID,ENERGY,SECTOR,SSD_ID,TIME_OF_FLIGHT,PSTART,RANGE
      INTEGER BR(0:2,0:7), SUM_BR(0:2), SUM_ALL_BR, RANGE_SUM(0:2), STOP_SUM(0:3), DCR1, DCR2, DCR3, DCR
      INTEGER BR_MAX(0:2), SUM_BR_MAX(0:2), DCR1_MAX, DCR2_MAX, DCR3_MAX, DCR_MAX
C
      RECORD /CORE/ SCI
      RECORD /HDBstr/ HDB
      RECORD /ST_RATE/ STICS_RATE
      RECORD /ST_PHA/ STICS_PHA
      RECORD /T/ OLDTIME_T
C
C Get input L1 filename
C
      L1FILENAME = 'DATA_DIR:WI*L1*.DAT'
      CALL WIND_GET_FILENAME(L1FILENAME, STATUS)
      IF(STATUS.NE.1)STOP
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
      CALL WIND_L1_GET_STICS( L1_LUN, STICS_RATE, STICS_PHA, STATUS )
      IF (STATUS .NE. 1) GOTO 9080
C
C Open other files for output
C
      OPEN(UNIT= 50, NAME='ERRORS.STICS', DEFAULTFILE='DATA_DIR:', FORM='FORMATTED', 
     &     TYPE='NEW', RECL=168)
      WRITE(50,16) FULL_L1_NAME(1:NAMELEN)
  16  FORMAT(' STICS',/,T10,'Input Level 1 Filename: ',A)
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
C Decompress STICS rates for one science record and compare
C
      DO 800 J=0,59
C
C Get voltage step and e/q
C
        ITEMP = STICS_RATE.EDB(J).RATE(179)
        VSTEP = IBITS(ITEMP,0,5)
        ITEMP=HDB.DAT(24)
        ITEMP2=HDB.DAT(25)
C
C The logic that follows assumes that both + and - high voltages are on or both are off
C
        IF(IAND(IBITS(ITEMP,7,1), IBITS(ITEMP2,0,1)).EQ.1)THEN
          E_OVER_Q = D1 * (D2**VSTEP)
        ELSE
          E_OVER_Q = 0
        ENDIF
C
C Get time difference
C
        IF((OLDTIME.NE.0.0).AND.(SCI.EDB(J).EPOCH.NE.0.0))THEN
          TIMEDIFF=SCI.EDB(J).EPOCH-OLDTIME
          IF(TIMEDIFF.LE.0.0)THEN
            WRITE(50,130)
            WRITE(50,140)OLDTIME_T.YEAR,OLDTIME_T.DOY,OLDTIME_T.HR,
     &                   OLDTIME_T.MIN,OLDTIME_T.SEC
            WRITE(50,150)SCI.EDB(J).W_TIME.YEAR,SCI.EDB(J).W_TIME.DOY,SCI.EDB(J).W_TIME.HR,
     &                   SCI.EDB(J).W_TIME.MIN,SCI.EDB(J).W_TIME.SEC
 130        FORMAT('  TIME PROBLEM')
 140        FORMAT(' OLD TIME: ',I4,1X,I3,1X,I2,':',I2.2,':',I2.2)         
 150        FORMAT(' NEW TIME: ',I4,1X,I3,1X,I2,':',I2.2,':',I2.2)         
          ENDIF
        ENDIF
C
C Compression codes for STICS Basic Rates are in the HDB in byte 189, bit 1
C
        ITEMP = HDB.DAT(189)
        CODE_C = BTEST(ITEMP,1)
C
C Decompress STICS rates and sum them
C
        SUM_ALL_BR = 0
        DO 250 I=0,2
          SUM_BR(I) = 0
          DO 200 K=0,7     
            BR(I,K) = WIND_DECOMPRESS(STICS_RATE.EDB(J).RATE(K+120+I*8),CODE_C)
            SUM_BR(I) = SUM_BR(I) + BR(I,K)
 200      CONTINUE
          SUM_ALL_BR = SUM_ALL_BR + SUM_BR(I)
          RANGE_SUM(I) = 0
 250    CONTINUE
        DO 255 I=0,3
          STOP_SUM(I) = 0
 255    CONTINUE
C
C Compression codes for STICS DCR's are in the HDB in byte 189, bit 2. 
C
        ITEMP = HDB.DAT(189)
        CODE_C2 = BTEST(ITEMP,2)
C
C Find, then sum, all DCR's to compare to the sum of all BR's
C
        DCR1 = WIND_DECOMPRESS(STICS_RATE.EDB(J).RATE(170),CODE_C2) 
        DCR2 = WIND_DECOMPRESS(STICS_RATE.EDB(J).RATE(171),CODE_C2) 
        DCR3 = WIND_DECOMPRESS(STICS_RATE.EDB(J).RATE(172),CODE_C2) 
        DCR = DCR1 + DCR2 + DCR3
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
C Count number of PHA events in each range.
C
          RANGE_SUM(RANGE) = RANGE_SUM(RANGE) + 1
          STOP_SUM(STOP_ID) = STOP_SUM(STOP_ID) + 1
 300    CONTINUE
C
C Is the # PHA events of a given range  > the corresponding BR? If so, we may have an error.
C
        DO 700 I=0,2
          IF(RANGE_SUM(I).GT.SUM_BR(I))THEN
C
C If the sum of the BR's is too low, it may be because  one or more of the values have been 
C rounded down by the compression algorithm.  The highest possible  result for the original 
C (pre-compression) value is 1 less than the decompressed result from the next higher code. 
C Check to see if the sum of these maximum values is also too low. If it is, we have a true 
C error.
C
            DO 400 K=0,7     
              NEXTCODE = STICS_RATE.EDB(J).RATE(K+112+I*8) + 1
              BR_MAX(I)  = WIND_DECOMPRESS(NEXTCODE,CODE_C)
              BR_MAX(I)  = BR_MAX(I) - 1
              SUM_BR_MAX(I) = SUM_BR_MAX(I) + BR_MAX(I) 
 400        CONTINUE
C
C Don't print an error mesage if we have bad quality flags
C
            IF((RANGE_SUM(I).GT.SUM_BR_MAX(I)).AND.
     &         (SCI.EDB(J).ST_BR_Qual.EQ.0).AND.(SCI.EDB(J).ST_P_Qual.EQ.0))THEN
              WRITE(50,600)SCI.EDB(J).W_TIME.YEAR,SCI.EDB(J).W_TIME.DOY,SCI.EDB(J).W_TIME.HR,
     &                     SCI.EDB(J).W_TIME.MIN,SCI.EDB(J).W_TIME.SEC
 600          FORMAT(/,' TIME: ',I4,1X,I3,1X,I2,':',I2.2,':',I2.2)         
              WRITE(50,650) I, RANGE_SUM(I), (BR(I,K), K=0,7), SUM_BR(I)
 650          FORMAT(' # OF RANGE ',I1,' PHA EVENTS= ',I4,' BASIC RATES:',8I6,' SUM=',I7)
            ENDIF
          ENDIF
 700    CONTINUE
C
C Is the sum of all the basic rates > DCR? If so, we may have an error.
C
        IF(SUM_ALL_BR.GT.DCR)THEN
C
C If DCR is too low, it may be because the value has been rounded down by the compression
C algorithm. Check to see if the next higher code is also too low. If it is, we have a true
C error.
C
          NEXTCODE = STICS_RATE.EDB(J).RATE(170) + 1
          DCR1_MAX = WIND_DECOMPRESS(NEXTCODE,CODE_C) - 1
          NEXTCODE = STICS_RATE.EDB(J).RATE(171) + 1
          DCR2_MAX = WIND_DECOMPRESS(NEXTCODE,CODE_C) - 1
          NEXTCODE = STICS_RATE.EDB(J).RATE(172) + 1
          DCR3_MAX = WIND_DECOMPRESS(NEXTCODE,CODE_C) - 1
          DCR_MAX  = DCR1_MAX + DCR2_MAX + DCR3_MAX
C
C Don't print an error mesage if we have bad quality flags
C
          IF((SUM_ALL_BR.GT.DCR_MAX).AND.
     &       (SCI.EDB(J).ST_BR_Qual.EQ.0).AND.(SCI.EDB(J).ST_ER_Qual.EQ.0))THEN
            WRITE(50,600)SCI.EDB(J).W_TIME.YEAR,SCI.EDB(J).W_TIME.DOY,SCI.EDB(J).W_TIME.HR,
     &                   SCI.EDB(J).W_TIME.MIN,SCI.EDB(J).W_TIME.SEC
            WRITE(50,750)SUM_ALL_BR,DCR1,DCR2,DCR3,DCR
 750        FORMAT(' SUM OF ALL BR=',I6,'  DCR1= ',I6,'  DCR2= ',I6,'  DCR3= ',I6,'  DCR= ',I6)
            WRITE(50,760)STOP_SUM
 760        FORMAT(' SUM OF STOP ID''s FOR 0 ',I3,' FOR 1 ',I3,' FOR 2 ',I3,' FOR 3 ',I3)
          ENDIF
        ENDIF
        IF(SCI.EDB(J).EPOCH.NE.0.0)THEN
          OLDTIME=SCI.EDB(J).EPOCH
          OLDTIME_T=SCI.EDB(J).W_TIME
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
