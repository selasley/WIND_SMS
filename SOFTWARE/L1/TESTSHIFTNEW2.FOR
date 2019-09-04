	PROGRAM PRINTL1
C
C This program reads the L1 data files on disk and prints out the times, rates
C and PHA words.
C
C	This version:	31-OCT-1994
C
C     Creation:
C	H.BROER	8-SEP-1994
C
C     Revisions:
C
C     9/29/94 	Reduced maximum index used to write out PHA byte arrays by 1. If its the right
C		number for a (1 origin) substring spec its 1 too big for a (0 origin) byte index
C     9/30/94 	The index for accessing the byte arrays was reduced by 4 more, because of the
C		form of the structure
C    10/06/94   Altered (from TESTSHIFT, its predecessor) to work on new L1 format
C    10/19/94   Now reads headers
C    10/20/94   Zero out SWICS  rate array between reads; use L1FNEW (not L1F) as input file
C    10/20/94   Changed to use WIND_L1_* calls to read L1 file and access parts
C               of L1 record
C    10/31/94   Changed comments, format, style, some variable names; removed
C               include file EDB.INC, use EDBDEF.INC instead, declare own
C               RECORD variables 
C
      IMPLICIT NONE

      INCLUDE	'INC_DIR:EDBDEF.INC/NOLIST'
      INCLUDE   'INC_DIR:L1FMT.INC/NOLIST'

      INTEGER*4	II,JJ,STATUS,DUMMY,SPIN,KK,CNTR,IOUT/17/,HKNUM,L1_LUN/8/
      INTEGER START/1/,LAST/20/
      CHARACTER L1FILENAME*6/'L1FNEW'/
      RECORD /CORE/ SCI
      RECORD /HKeep/ HKeep
      RECORD /HDBstr/ HDB
      RECORD /SW_RATE/ SWICS_RATE
      RECORD /SW_PHA/ SWICS_PHA
      RECORD /MA_RATE/ MASS_RATE
      RECORD /MA_PHA/ MASS_PHA
      RECORD /ST_RATE/ STICS_RATE
      RECORD /ST_PHA/ STICS_PHA
C
      CALL WIND_L1_OPEN( L1_LUN, L1FILENAME, L1_FILE_HEADER, STATUS)
      IF (STATUS .NE. 1) GOTO 9001
C
C Read next Science Record
C
 100  CONTINUE
      CALL WIND_L1_READ_SR( L1_LUN, CNTR, L1_SR_HEADER, STATUS )
      IF (STATUS .NE. 1) GOTO 9002
C Skip up to START'th Science Record
      IF (CNTR .LT. START) GOTO 100
C
C Get the Basic Core data
C
      CALL WIND_L1_GET_BASIC( L1_LUN, SCI, STATUS)
      IF (STATUS .NE. 1) GOTO 9003
C
C Also get the Housekeeping Records (HKs) and the Housekeeping Data Block (HDB)
C HKNUM will have number of HKs in data structure HKeep
C
      CALL WIND_L1_GET_HK( L1_LUN, HKeep, HKNUM, STATUS)
      IF (STATUS .NE. 1) GOTO 9004
      CALL WIND_L1_GET_HDB( L1_LUN, HDB, STATUS)
      IF (STATUS .NE. 1) GOTO 9005
C
      WRITE(IOUT,300) CNTR
 300  FORMAT(' RECORD ',I4)
C
C Get SWICS rates and PHA
C
      CALL WIND_L1_GET_SWICS( L1_LUN, SWICS_RATE, SWICS_PHA, STATUS )
      IF (STATUS .NE. 1) GOTO 9006
C
C Now write them out formatted by voltage step for all 60 voltage steps
C
      DO SPIN=0,59
        WRITE(IOUT,1002) SPIN
        WRITE(IOUT,1001)  (SWICS_RATE.EDB(SPIN).RATE(KK),KK=0,115)
1001    FORMAT(1X,20Z3.2)
1002    FORMAT(1X,/' DUMP OF SWICS RATES IN SPIN ',I2)
      END DO
C
      DO SPIN=0,59
        JJ = SCI.EDB( SPIN ).SWICS_Len * 4 + 4
        WRITE(IOUT,1003) SPIN
        WRITE(IOUT,1001)  (SWICS_PHA.EDB(SPIN).PHA(KK),KK=0,JJ-5)
1003    FORMAT(1X,/' DUMP OF SWICS PHA IN SPIN ',I2)
      END DO
C
C Get MASS rates and PHA
C
      CALL WIND_L1_GET_MASS( L1_LUN, MASS_RATE, MASS_PHA, STATUS )
      IF (STATUS .NE. 1) GOTO 9007
C
C Now write them out formatted by voltage step for all 60 voltage steps
C
      DO SPIN=0,59
        WRITE(IOUT,1004) SPIN
        WRITE(IOUT,1001)  (MASS_RATE.EDB(SPIN).RATE(KK),KK=0,26)
1004    FORMAT(1X,/' DUMP OF MASS RATES IN SPIN ',I2)
      END DO
C
      DO SPIN=0,59
        JJ = SCI.EDB( SPIN ).MASS_Len * 2 + 4
        WRITE(IOUT,1005) SPIN
        WRITE(IOUT,1001)  (MASS_PHA.EDB(SPIN).PHA(KK),KK=0,JJ-5)
1005    FORMAT(1X,/' DUMP OF MASS PHA IN SPIN ',I2)
      END DO
C
C Get STICS rates and PHA
C
      CALL WIND_L1_GET_STICS( L1_LUN, STICS_RATE, STICS_PHA, STATUS )
      IF (STATUS .NE. 1) GOTO 9008
C
C Now write them out formatted by voltage step for all 60 voltage steps
C
      DO SPIN=0,59
        WRITE(IOUT,1006) SPIN
        WRITE(IOUT,1001)  (STICS_RATE.EDB(SPIN).RATE(KK),KK=0,179)
1006    FORMAT(1X,/'  DUMP OF STICS RATES IN SPIN ',I2)
      END DO
C
      DO SPIN=0,59
        JJ = SCI.EDB( SPIN ).STICS_Len * 4 + 4
        WRITE(IOUT,1007) SPIN
        WRITE(IOUT,1001)  (STICS_PHA.EDB(SPIN).PHA(KK),KK=0,JJ-5)
1007    FORMAT(1X,/' DUMP OF STICS PHA IN SPIN ',I2)
      END DO
C
      IF (CNTR .LT. LAST) GOTO 100
      CALL WIND_L1_CLOSE( L1_LUN, STATUS )
      IF (STATUS .NE. 1) GOTO 9009
      STOP
C
C Come here for errors
C
9001  TYPE *,'ERROR OPENING L1 FILE; STATUS=',STATUS
      STOP
9002  TYPE *,'ERROR READING SCIENCE RECORD ',CNTR,'; STATUS=',STATUS
      STOP
9003  TYPE *,'ERROR GETTING BASIC CORE; STATUS=',STATUS
      STOP
9004  TYPE *,'ERROR GETTING HK; STATUS=',STATUS
      STOP
9005  TYPE *,'ERROR GETTING HDB; STATUS=',STATUS
      STOP
9006  TYPE *,'ERROR GETTING SWICS DATA; STATUS=',STATUS
      STOP
9007  TYPE *,'ERROR GETTING MASS DATA; STATUS=',STATUS
      STOP
9008  TYPE *,'ERROR GETTING STICS DATA; STATUS=',STATUS
      STOP
9009  TYPE *,'ERROR CLOSING L1 FILE; STATUS=',STATUS
      STOP
      END
