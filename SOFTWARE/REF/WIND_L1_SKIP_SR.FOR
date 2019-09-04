	SUBROUTINE WIND_L1_SKIP_SR( unit, count, status )
C+
C 
C FUNCTIONAL DESCRIPTION:
C 
C    This program reads and discards the next `count' Science Records from the
C    specified L1 data file.  A call to WIND_L1_READ_SR must be made for the
C    next Science Record to be available for accessing.  No data is transferred
C    to the caller.
C 
C FORMAL PARAMETERS:
C  
C     unit:
C	  An INTEGER*4 variable indicating the Level 1 file to be read.
C  
C     count:
C	  An INTEGER*4 variable indicating the number of Science Records to be
C         skipped.
C      
C     status:
C	  An INTEGER*4 variable which will receive the read completion status.
C	  A 1 indicates success.
C  
C COMMON BLOCKS:
C  
C     L1INFO	  contains arrays of unit numbers, each one's current science
C		  record, and other control information
C  
C DESIGN:
C  
C     The routine WIND_L1_READ_SR is called the proper number of times.
C 
C AUTHOR(S): 
C 
C	 L. Bleau
C 
C Created: 
C 
C	 31-OCT-1994    L. Bleau
C 
C Revised:
C 
C        Date     | Name  | Description
C ----------------+-------+-----------------------------------------------------
C-
CDEC$	IDENT   '1.0'

	IMPLICIT NONE

	INCLUDE 'INC_DIR:L1FMT.INC/NOLIST'

	INTEGER*4 unit, count, status

	INTEGER*4 i, scirec_number
	RECORD /ST_L1_SR_HEADER/ header
C
	status = 1
	IF (count .lt. 1) RETURN
	DO i=1,count
	  CALL WIND_L1_READ_SR(unit,scirec_number,header,status)
	  IF (status .ne. 1) RETURN
	ENDDO
	RETURN
	END
