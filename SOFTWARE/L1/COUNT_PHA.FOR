	PROGRAM COUNT_PHA
C
C This program reads the L1 data files and compares the number of PHA words
C for each sensor in each spin with the stated number of PHA words.
C
C Specifically, it examines the PHA words to detect if any of them are all
C zero.  For SWICS and STICS PHA words are 4 bytes long; for MASS 2 bytes.
C
C Created:  27-JAN-1995  Larry Bleau
C
C This version: 1-FEB-1995
C
C Revisions:
C
C  1-FEB-1995   L. Bleau   add code to tally, print total # of PHAs; check for
C                          end of file and exit gracefully
C
      IMPLICIT NONE

      INCLUDE 'INC_DIR:EDBDEF.INC/NOLIST'
      INCLUDE 'INC_DIR:L1FMT.INC/NOLIST'
      INCLUDE '($RMSDEF)/NOLIST'

      INTEGER*4	STATUS,SPIN,KK,MM,RECCNTR,IOUT/17/,HKNUM,L1_LUN/8/
      INTEGER START,LAST,COUNT,SENSOR,I,JJ,DETAIL_LEVEL
      INTEGER WHERE(100),NZERO,TOTAL_PHA(3),NONZERO_PHA(3),ZERO_PHA(3)
      CHARACTER*80 L1FILENAME,LISTFILENAME
      CHARACTER*30 FORMAT_TIME
      RECORD /CORE/ SCI
      RECORD /SW_RATE/ SWICS_RATE
      RECORD /SW_PHA/ SWICS_PHA
      RECORD /MA_RATE/ MASS_RATE
      RECORD /MA_PHA/ MASS_PHA
      RECORD /ST_RATE/ STICS_RATE
      RECORD /ST_PHA/ STICS_PHA
      INTEGER FORTERR,RMSSTC,RMSSTV,LUN,VAXERR
C
C Get input file
C
      L1FILENAME = 'DATA_DIR:WI*L1*.DAT'
      WRITE(6,1010) 'L1-FILE:'
1010  FORMAT(1X,A)
      CALL WIND_GET_FILENAME(L1FILENAME, STATUS)
      IF(STATUS.NE.1)STOP
C
C Construct output filename
C
      LISTFILENAME = L1FILENAME
      KK = INDEX(LISTFILENAME, ']')
      MM = INDEX(LISTFILENAME(KK+1:), '.')
      IF (MM .NE. 0) THEN
        LISTFILENAME(KK+MM:) = '.COUNT'
      ELSE
        MM = INDEX(LISTFILENAME(KK+1:), ' ')
        LISTFILENAME(KK+MM:) = '.COUNT'
      ENDIF
      WRITE(6,1010) 'LISTING FILE:'
      CALL WIND_GET_FILENAME(LISTFILENAME, STATUS)
      KK = INDEX(LISTFILENAME, ';')
      IF (KK .NE. 0) LISTFILENAME(KK:) = ' '
C
C Get run parameters
C
      WRITE(6,1001) 
1001  FORMAT(' Number of first science record to examine (1=first): ',$)
      READ(5,*,END=9000) START
      IF (START .LT. 1) STOP
      WRITE(6,1002) 
1002  FORMAT(' Number of science records to examine (0=entire file): ',$)
      READ(5,*,END=9000) COUNT
      IF (COUNT .EQ. 0) COUNT = 9999
      IF (COUNT .LT. 1) STOP
      LAST = START + COUNT - 1
      WRITE(6,1003)
1003  FORMAT(' What sensor to examine (1=SWICS, 2=MASS, 3=STICS, 4=all): ',$)
      READ(5,*,END=9000) SENSOR
      WRITE(6,1004)
1004  FORMAT(' Detail (1) or summary only (2): ',$)
      READ(5,*,END=9000) DETAIL_LEVEL
      IF (DETAIL_LEVEL .LT. 1 .OR. DETAIL_LEVEL .GT. 2) STOP
C
C Open files
C
      CALL WIND_L1_OPEN( L1_LUN, L1FILENAME, L1_FILE_HEADER, STATUS)
      IF (STATUS .NE. 1) GOTO 9001
      OPEN(UNIT=IOUT, FILE=LISTFILENAME, STATUS='NEW', ERR=9010, IOSTAT=STATUS)
C
C Initialize variables
C
      DO I=1,3
        TOTAL_PHA(I) = 0
        NONZERO_PHA(I) = 0
        ZERO_PHA(I) = 0
      END DO
C
C Dump file header
C
C      CALL DUMP_FILE_HEADER(IOUT,L1_FILE_HEADER)
C
C Skip START-1 Science Records
C The next call to WIND_L1_READ_SR will get the START'th Sci Rec
C
      CALL WIND_L1_SKIP_SR( L1_LUN, START-1, STATUS )
      IF (STATUS .NE. 1) GOTO 9002
C
C Read next Science Record
C
100   CONTINUE
      CALL WIND_L1_READ_SR( L1_LUN, RECCNTR, L1_SR_HEADER, STATUS )
      IF (STATUS .EQ. RMS$_EOF) GOTO 8000
      IF (STATUS .NE. 1) GOTO 9002
C
C      WRITE(IOUT,1004) RECCNTR,L1_SR_HEADER.SR_COUNT
C1004  FORMAT(/' RECORD ',I4,'   ORIGINAL SCI REC# ',I8)
C
C Get the Basic Core data
C
      CALL WIND_L1_GET_BASIC( L1_LUN, SCI, STATUS)
      IF (STATUS .NE. 1) GOTO 9003
C
C Get SWICS rates and PHA, but ignore rates
C
      IF (SENSOR .EQ. 1 .OR. SENSOR .EQ. 4) THEN
        CALL WIND_L1_GET_SWICS( L1_LUN, SWICS_RATE, SWICS_PHA, STATUS )
        IF (STATUS .NE. 1) GOTO 9006
C
C Now scan through PHA words for each spin, looking for ones that are all zero
C
        DO SPIN=0,59
          JJ = SCI.EDB( SPIN ).SWICS_Len * 4
          IF (JJ .GT. 0 .AND. SCI.EDB( SPIN ).Qual .EQ. 0) THEN
            CALL SCAN_ZERO_PHA( SWICS_PHA.EDB(SPIN).PHA, JJ, 4, NZERO, WHERE )
            TOTAL_PHA(1) = TOTAL_PHA(1) + SCI.EDB( SPIN ).SWICS_Len
            NONZERO_PHA(1) = NONZERO_PHA(1) + (SCI.EDB(SPIN).SWICS_Len - NZERO)
            ZERO_PHA(1) = ZERO_PHA(1) + NZERO
            IF (NZERO .NE. 0 .AND. DETAIL_LEVEL .EQ. 1) THEN
              WRITE(IOUT,1013) 'SWICS',RECCNTR,L1_SR_HEADER.SR_COUNT,SPIN,
     &              SCI.EDB( SPIN ).SWICS_Len,NZERO
              WRITE(IOUT,1011) (WHERE(I)+1,4*WHERE(I),I=1,NZERO)
            ENDIF
          ENDIF
        END DO
      ENDIF
C
1011  FORMAT(4(1X,I6,4X,I5,3X))
1013  FORMAT(/' ZERO ',A5,' PHA FOUND!   RECORD ',I4,
     &    '   ORIGINAL SCI REC# ',I8,'   SPIN ',I2/
     &        ' # OF PHA WORDS REPORTED: ',5X,I4,'    # OF ZERO PHA WORDS: ',I4/
     &    4(' PHA word  offset  '))
C
C Get MASS rates and PHA, but ignore rates
C
      IF (SENSOR .EQ. 2 .OR. SENSOR .EQ. 4) THEN
        CALL WIND_L1_GET_MASS( L1_LUN, MASS_RATE, MASS_PHA, STATUS )
        IF (STATUS .NE. 1) GOTO 9007
C
C Now scan through PHA words for each spin, looking for ones that are all zero
C
        DO SPIN=0,59
          JJ = SCI.EDB( SPIN ).MASS_Len * 2
          IF (JJ .GT. 0 .AND. SCI.EDB( SPIN ).Qual .EQ. 0) THEN
            CALL SCAN_ZERO_PHA( MASS_PHA.EDB(SPIN).PHA, JJ, 2, NZERO, WHERE )
            TOTAL_PHA(2) = TOTAL_PHA(2) + SCI.EDB( SPIN ).MASS_Len
            NONZERO_PHA(2) = NONZERO_PHA(2) + (SCI.EDB(SPIN).MASS_Len - NZERO)
            ZERO_PHA(2) = ZERO_PHA(2) + NZERO
            IF (NZERO .NE. 0 .AND. DETAIL_LEVEL .EQ. 1) THEN
              WRITE(IOUT,1013) 'MASS',RECCNTR,L1_SR_HEADER.SR_COUNT,SPIN,
     &              SCI.EDB( SPIN ).MASS_Len,NZERO
              WRITE(IOUT,1011) (WHERE(I)+1,2*WHERE(I),I=1,NZERO)
            ENDIF
          ENDIF
        END DO
      ENDIF
C
C Get STICS rates and PHA, but ignore rates
C
      IF (SENSOR .EQ. 3 .OR. SENSOR .EQ. 4) THEN
        CALL WIND_L1_GET_STICS( L1_LUN, STICS_RATE, STICS_PHA, STATUS )
        IF (STATUS .NE. 1) GOTO 9008
C
C Now scan through PHA words for each spin, looking for ones that are all zero
C
        DO SPIN=0,59
          JJ = SCI.EDB( SPIN ).STICS_Len * 4
          IF (JJ .GT. 0 .AND. SCI.EDB( SPIN ).Qual .EQ. 0) THEN
            CALL SCAN_ZERO_PHA( STICS_PHA.EDB(SPIN).PHA, JJ, 4, NZERO, WHERE )
            TOTAL_PHA(3) = TOTAL_PHA(3) + SCI.EDB( SPIN ).STICS_Len
            NONZERO_PHA(3) = NONZERO_PHA(3) + (SCI.EDB(SPIN).STICS_Len - NZERO)
            ZERO_PHA(3) = ZERO_PHA(3) + NZERO
            IF (NZERO .NE. 0 .AND. DETAIL_LEVEL .EQ. 1) THEN
              WRITE(IOUT,1013) 'STICS',RECCNTR,L1_SR_HEADER.SR_COUNT,SPIN,
     &              SCI.EDB( SPIN ).STICS_Len,NZERO
              WRITE(IOUT,1011) (WHERE(I)+1,4*WHERE(I),I=1,NZERO)
            ENDIF
          ENDIF
        END DO
      ENDIF
C
      IF (RECCNTR .LT. LAST) GOTO 100
C
C Done, close files and exit
C
8000  CALL WIND_L1_CLOSE( L1_LUN, STATUS )
      IF (STATUS .NE. 1) GOTO 9009
      WRITE(6,8001) NONZERO_PHA,ZERO_PHA,TOTAL_PHA
      WRITE(IOUT,8001) NONZERO_PHA,ZERO_PHA,TOTAL_PHA
8001  FORMAT(/'                                SUMMARY'/
     &       /' Sensor:',T35,'       SWICS       MASS        STICS'/
     &        ' Number of non-zero PHA words: ',T35,3I12/
     &        ' Number of zero PHA words: ',T35,3I12/
     &        ' Total number of PHA words: ',T35,3I12)
      CLOSE(IOUT)
      STOP
C
C Come here for errors
C
9000  STOP
9001  TYPE *,'ERROR OPENING L1 FILE; STATUS=',STATUS
      GOTO 9900
9002  CALL ERRSNS(FORTERR,RMSSTC,RMSSTV,LUN,VAXERR)
      IF (RMSSTC .EQ. RMS$_EOF) GOTO 8000
      TYPE *,'ERROR READING SCIENCE RECORD ',RECCNTR,'; STATUS=',STATUS
      GOTO 9900
9003  TYPE *,'ERROR GETTING BASIC CORE; STATUS=',STATUS
      GOTO 9900
9006  TYPE *,'ERROR GETTING SWICS DATA; STATUS=',STATUS
      GOTO 9900
9007  TYPE *,'ERROR GETTING MASS DATA; STATUS=',STATUS
      GOTO 9900
9008  TYPE *,'ERROR GETTING STICS DATA; STATUS=',STATUS
      GOTO 9900
9009  TYPE *,'ERROR CLOSING L1 FILE; STATUS=',STATUS
      GOTO 9900
9010  TYPE *,'ERROR OPENING OUTPUT FILE; STATUS=',STATUS
      GOTO 9900
9900  CALL LIB$STOP(%VAL(STATUS))
      END
      SUBROUTINE SCAN_ZERO_PHA( PHA, NBYTES, PHASIZE, ZERO_COUNT, WHERE )
C
C This subroutine scans a set of PHA words, counting the number of zero PHA
C words which are non-zero.  This count (the number of zero PHA words) is
C returned to the caller.  Since one of the arguments is the size (in bytes) of
C a PHA word this routine can be used to examine PHA words from any of the
C sensors. 
C
C Parameters:
C
C   PHA:
C          A BYTE array containing the PHA words for a given sensor.
C
C   NBYTES:
C          An INTEGER giving the length (in bytes) of the array PHA.  This is
C          not the same as the number of PHA words, since a PHA word occupies
C          either 2 or 4 bytes, depending on the sensor.
C
C   PHASIZE:
C          An INTEGER specifying the number of bytes contained in a single PHA
C          word.
C
C   ZERO_COUNT:
C          An INTEGER which will contain, upon return, the number of zero
C          PHA words.
C
C   WHERE:
C          An INTEGER array of length 100.  Each element of WHERE gives the
C          PHA word number (1 origin) of a PHA word which is zero.  The number
C          of valid entries in WHERE is given by the argument ZERO_COUNT.
C
C
C Created:  30-JAN-1995   Larry Bleau
C
C This version:  1-FEB-1995
C
C Revisions:
C  1-FEB-1995     L. Bleau   added comments, changed NZERO to ZERO_COUNT
C
      IMPLICIT NONE
      INTEGER NBYTES,ZERO_COUNT,PHASIZE
      INTEGER WHERE(100)
      BYTE PHA(NBYTES)
C
      INTEGER NWORDS,I,J
      LOGICAL GOOD
C
      ZERO_COUNT = 0
      NWORDS = NBYTES / PHASIZE
      IF (NWORDS .EQ. 0) RETURN
C
      DO J=0,NWORDS-1
        GOOD = .FALSE.
C
C If any of the bytes which comprise the PHA word is non-zero count this PHA
C word as good
C
        DO I=1,PHASIZE
          IF (PHA(J*PHASIZE+I) .NE. 0) GOOD = .TRUE.
        ENDDO
C Found one not good, same its index
        IF (.NOT. GOOD) THEN
          ZERO_COUNT = ZERO_COUNT + 1
          WHERE(ZERO_COUNT) = J
        ENDIF
      ENDDO
      RETURN
      END
