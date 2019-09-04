CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      PROGRAM CHECK_MASS
C This program reads a WIND L1 data file and decompresses the rate data, then
C checks for internal consistency in the MASS data. The programs MASS_RATE_TABLE
C and MASS_PHA_TABLE were used as models.
C 
C Created 	6-JAN-1994	JOHN PAQUETTE
C
C This version:    9-JAN-1995
C
C Revision history:
C        9-JAN-1995     J.PAQUETTE      altered code to check if discrepancies
C                                       could be attributable to compression.
CDEC$ IDENT  '1.0'
      INCLUDE	'INC_DIR:EDBDEF.INC/NOLIST'
      INCLUDE   'INC_DIR:L1FMT.INC/NOLIST'
      INCLUDE   '($RMSDEF)'
      BYTE NEXTCODE
      INTEGER*4 STATUS,L1_LUN/19/,MASS_RATES(116),WIND_DECOMPRESS,VSTEP,MONTH,DAYOFMONTH
      REAL*4 E_OVER_Q
      REAL*8 OLDTIME,TIMEDIFF
      CHARACTER L1FILENAME*80
      CHARACTER FULL_L1_NAME*80
      INTEGER UNIT_TABLE(0:2,3)/52,53,54,53,52,54,54,52,53/
      LOGICAL CODE_C,CODE_C2,STARTFOUND,ENDFOUND,USE_SC_CLOCK,COMPARE_TIME_GE,GOODSPIN
      INTEGER I,J,K,L,N,NN,CNTR,ITEMP,NAMELEN
      INTEGER SECTOR,RANGE,ANODE,TIME_OF_FLIGHT,RANGE1
      INTEGER BR0_S,BR0_NS,BR1_S,BR1_NS,DCR_S,DCR_NS,BR0_S_MAX,BR0_NS_MAX,BR1_S_MAX,
     &        BR1_NS_MAX,DCR_S_MAX,DCR_NS_MAX
C
      RECORD /CORE/ SCI
      RECORD /HDBstr/ HDB
      RECORD /MA_RATE/ MASS_RATE
      RECORD /MA_PHA/ MASS_PHA
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
      CALL WIND_L1_GET_MASS( L1_LUN, MASS_RATE, MASS_PHA, STATUS )
      IF (STATUS .NE. 1) GOTO 9080
C
C Open other files for output
C
      OPEN(UNIT= 50, NAME='ERRORS.MASS', DEFAULTFILE='DATA_DIR:', FORM='FORMATTED', 
     &     TYPE='NEW', RECL=168)
      WRITE(50,16) FULL_L1_NAME(1:NAMELEN)
  16  FORMAT(' MASS',/,T10,'Input Level 1 Filename: ',A)
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
C Decompress MASS rates for one science record and compare
C
      DO 800 J=0,59
C
C Get voltage step and e/q
C
        ITEMP = MASS_RATE.EDB(J).RATE(25)
        VSTEP = IBITS(ITEMP,0,6)
        E_OVER_Q = 0.5156 * 1.05135**VSTEP
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
C Compression codes for all MASS rates are in the HDB in byte 189, bit 4
C
        ITEMP = HDB.BDH.DAT(189)
        CODE_C = BTEST(ITEMP,4)
C
C Decompress MASS rates
C
        DCR_S  = WIND_DECOMPRESS(MASS_RATE.EDB(J).RATE(8),CODE_C)
        DCR_NS = WIND_DECOMPRESS(MASS_RATE.EDB(J).RATE(9),CODE_C)
        BR0_S  = WIND_DECOMPRESS(MASS_RATE.EDB(J).RATE(11),CODE_C)
        BR0_NS = WIND_DECOMPRESS(MASS_RATE.EDB(J).RATE(12),CODE_C)
        BR1_S  = WIND_DECOMPRESS(MASS_RATE.EDB(J).RATE(13),CODE_C)
        BR1_NS = WIND_DECOMPRESS(MASS_RATE.EDB(J).RATE(14),CODE_C)
C
C Do for all MASS PHA words
C
        RANGE1=0
        DO 300 I=0, (SCI.EDB(J).MASS_Len-1) * 2, 2
C
C Get parts of PHA word and count number of range 1 PHA events.
C
          CALL DISSECT_PHA(MASS_PHA.EDB(J).PHA(I),SECTOR,RANGE,ANODE,TIME_OF_FLIGHT)
          IF(RANGE.EQ.1)RANGE1=RANGE1+1
 300    CONTINUE
C
C Is the # of range 1 PHA events > BR1? If so, we may have an error.
C
        IF(RANGE1.GT.BR1_S+BR1_NS)THEN
C
C If the sum of BR_S and BR_NS is too low, it may be because one or both of the values
C have been rounded down by the compression algorithm. The highest possible result for
C the original (pre-compression) value is 1 less than the decompressed result from the
C next higher code. Check to see if the sum of these maximum values is also too low. 
C If it is, we have a true error.
C
          NEXTCODE=MASS_RATE.EDB(J).RATE(13)+1
          BR1_S_MAX  = WIND_DECOMPRESS(NEXTCODE,CODE_C)
          BR1_S_MAX  = BR1_S_MAX - 1
          NEXTCODE=MASS_RATE.EDB(J).RATE(14)+1
          BR1_NS_MAX  = WIND_DECOMPRESS(NEXTCODE,CODE_C)
          BR1_NS_MAX  = BR1_NS_MAX - 1
          IF(RANGE1.GT.BR1_S_MAX+BR1_NS_MAX)THEN
            WRITE(50,200)SCI.EDB(J).W_TIME.YEAR,SCI.EDB(J).W_TIME.DOY,SCI.EDB(J).W_TIME.HR,
     &                   SCI.EDB(J).W_TIME.MIN,SCI.EDB(J).W_TIME.SEC
 200        FORMAT(/,' TIME: ',I4,1X,I3,1X,I2,':',I2.2,':',I2.2)         
            WRITE(50,250)RANGE1,BR1_S,BR1_NS
 250        FORMAT(' # OF RANGE 1 PHA EVENTS= ',I4,' BR1_S= ',I9,' BR1_NS= ',I9)
          ENDIF
        ENDIF
C
C For sun sectors, is the sum of BR0+BR1 > DCR? If so, we may have an error.
C
        IF((BR0_S+BR1_S).GT.DCR_S)THEN
C
C If DCR_S is too low, it may be because the value has been rounded down by the compression
C algorithm. Check to see if the next higher code is also too low. If it is, we have a true
C error.
C
          NEXTCODE=MASS_RATE.EDB(J).RATE(8)+1
          DCR_S_MAX  = WIND_DECOMPRESS(NEXTCODE,CODE_C)
          DCR_S_MAX  = DCR_S_MAX - 1
          IF((BR0_S+BR1_S).GT.DCR_S_MAX)THEN
            WRITE(50,200)SCI.EDB(J).W_TIME.YEAR,SCI.EDB(J).W_TIME.DOY,SCI.EDB(J).W_TIME.HR,
     &                   SCI.EDB(J).W_TIME.MIN,SCI.EDB(J).W_TIME.SEC
            WRITE(50,350)BR0_S,BR1_S,DCR_S
            WRITE(50,360)BR0_NS,BR1_NS,DCR_NS
 350        FORMAT(' BR0_S',I9,' BR1_S= ',I9,' DCR_S= ',I9)
          ENDIF
        ENDIF
C
C For non-sun sectors, is the sum of BR0+BR1 > DCR? If so, we may have an error.
C
        IF((BR0_NS+BR1_NS).GT.DCR_NS)THEN
C
C If DCR_NS is too low, it may be because the value has been rounded down by the compression
C algorithm. Check  to see if the next higher code is also too low. If it is, we have a true
C error.
C
          NEXTCODE=MASS_RATE.EDB(J).RATE(9)+1
          DCR_NS_MAX  = WIND_DECOMPRESS(NEXTCODE,CODE_C)
          DCR_NS_MAX  = DCR_NS_MAX - 1
          IF((BR0_NS+BR1_NS).GT.DCR_NS_MAX)THEN
            WRITE(50,200)SCI.EDB(J).W_TIME.YEAR,SCI.EDB(J).W_TIME.DOY,SCI.EDB(J).W_TIME.HR,
     &                   SCI.EDB(J).W_TIME.MIN,SCI.EDB(J).W_TIME.SEC
            WRITE(50,350)BR0_S,BR1_S,DCR_S
            WRITE(50,360)BR0_NS,BR1_NS,DCR_NS
 360        FORMAT(' BR0_NS',I9,' BR1_NS= ',I9,' DCR_NS= ',I9)
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
