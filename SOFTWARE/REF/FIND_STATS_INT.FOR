	SUBROUTINE FIND_STATS_INT(INPUT,STATS)

C+
C 
C FUNCTIONAL DESCRIPTION:	
C 
C    This subroutine finds the minimum, maxixmum, and running sum of the
C    housekeeping quantity passed in the parameter INPUT.  These are stored in
C    the structure STATS, along with the time of the maxiumum and minimum.
C    Also, if limit checking is enabled for this quantity, the engineering
C    value, passed in parameter ENG_VALUE, is compared against the upper and
C    lower limits.  If the quantity is out of range the condition is reported.
C    Finally the engineering value is stored in the structure; this saves the
C    last good value of the quantity.
C
C FORMAL PARAMETERS:
C  
C     INPUT:
C	  An INTEGER*4 variable giving the raw value of the quantity in
C	  question. 
C      
C     ENG_VALUE:
C	  A REAL*4 variable giving the engineering value of the quantity in
C	  question.  This is a 5th order polynomial function of the raw value.
C
C     STATS:
C	  A record of structure type HOUSEKEEPING_STATS describing the quantity
C	  in question.
C  
C COMMON BLOCKS:
C  
C     INFO	makes information about the current HK, HDB, or SR available
C  
C DESIGN:
C  
C     The engineering value should be computed before calling FIND_STATS.  If it
C     is not the coefficients of the polynomial will also have to be passed in
C     as arguemnts.
C  
C 
C This version:   1.2   17-NOV-1995
C 
C Created: 
C 
C	 [dd-mmm-yyyy]    John Paquette
C 
C Revised:
C 
C	 Date     | Name  | Description
C ----------------+-------+-----------------------------------------------------
C 13-NOV-1995 1.1 |  LRB  | enhanced to check limits, too; change calling
C			    sequence to pass in engineering value; use
C			    engineering value to compare to limits; added
C			    documentation; save Epoch time and wall time of min
C			    and max values
C 17-NOV-1995 1.2 |  LRB  | added routine FIND_STATS_INT to do same but for
C			    integer argument and to take zeros into account
C-
CDEC$	IDENT  '1.2'
	IMPLICIT NONE
	INCLUDE 'INC_DIR:EDBDEF.INC/NOLIST'
	INCLUDE 'INC_DIR:LIMITDEF.INC/NOLIST'
C
C Parameter declarations
C
	INTEGER*4 INPUT
	REAL*4 ENG_VALUE
	RECORD /HOUSEKEEPING_STATS/ STATS
C
	LOGICAL IGNORE_ZEROS
C
C COMMON blocks
C
	INTEGER*4 NUM_SR
	REAL*8 EPOCH
	RECORD /T/ W_TIME
	COMMON /INFO/ EPOCH,W_TIME,NUM_SR
C
C Keep a count of the number of times we are called.  This is the number of
C values in the sum, which we'll need to compute the average.
C
	STATS.COUNT = STATS.COUNT + 1
C
C Compare to previously established min and max, and add to accumulator.  Also
C compare to upper and lower limits if reporting is enabled for this parameter.
C
	IF (INPUT .LT. STATS.MIN_VALUE) THEN
	  STATS.MIN_VALUE = INPUT
	  STATS.MIN_TIME = EPOCH
	  STATS.MIN_W_TIME = W_TIME
	ENDIF
	IF (INPUT .GT. STATS.MAX_VALUE) THEN
	  STATS.MAX_VALUE = INPUT
	  STATS.MAX_TIME = EPOCH
	  STATS.MAX_W_TIME = W_TIME
	ENDIF
	STATS.AVERAGE = STATS.AVERAGE + INPUT
C
C Save engineering value as the last good value
C
	STATS.LAST_VALUE = INPUT
C
C If reporting is enabled check against upper and lower limits.  Take proper
C action is value exceeds either of these.
C
	IF (STATS.REPORT) THEN
	  IF (INPUT .GT. STATS.HI_LIMIT) THEN
	    STATS.HI_COUNT = STATS.HI_COUNT + 1
	    CALL LIMIT_REPORT(STATS,FLOAT(INPUT))
	  ENDIF
	  IF (INPUT .LT. STATS.LO_LIMIT) THEN
	    STATS.LO_COUNT = STATS.LO_COUNT + 1
	    CALL LIMIT_REPORT(STATS,FLOAT(INPUT))
	  ENDIF
	ENDIF
C
	RETURN
	END
