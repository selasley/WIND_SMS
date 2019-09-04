	SUBROUTINE LIMIT_REPORT(STATS,VALUE)

C+
C 
C FUNCTIONAL DESCRIPTION:	
C 
C    This subroutine performs various initilization operations for the
C    WIND_HEALTH program.  Amoung them are:
C
C    	  Prompt user to input L1 filename
C	  Open the L1 file to unit L1_LUN, saving its header in L1_FILE_HEADER
C	  Construct the output filename, saving the name and its length in
C	    OUTPUTFILENAME and NCH 
C 
C FORMAL PARAMETERS:
C  
C     STATS
C         A structure of type HOUSEKEEPING_STATS containing information about
C         the quantity which is out-of-limits.
C
C     VALUE
C         A REAL*4 variable containig the value of the quantity which is
C         out-of-limits.
C
C  
C COMMON BLOCKS:
C  
C     REPORT	  Information controlling the limit reporting feature of the
C     		  program.
C     INFO	  Information about the L1 file and the current science record.
C  
C DESIGN:
C  
C     If this is the first time we are called write a header message to each
C	output file.
C     Write a 1-line message to WH_LUN, along with value, limit, science
C	record, and time.  Check which limit was exceeded and write proper one.
C     If this is the first time this parameter has exceeded either of its
C	limits write the parameter name to WH_LUN2.
C     Keep a count of the number of times we are called (= # of times any
C	parameter exceeds either of its limits).
C
C 
C This version:   1.2   20-NOV-1995
C 
C Created: 
C 
C	 13-NOV-1995    Larry Bleau
C 
C Revised:
C 
C	 Date     | Name  | Description
C ----------------+-------+-----------------------------------------------------
C 14-NOV-1995	  |  LRB  | changed from stub to fully functional subroutine;
C		  |       | changed format/content of mail msg header, detail
C		  |       | file
C 20-NOV-1995     |  LRB  | changed format of mail message
C 
C-
CDEC$	IDENT  '1.2'
	IMPLICIT NONE
	INCLUDE 'INC_DIR:EDBDEF.INC/NOLIST'
	INCLUDE 'INC_DIR:LIMITDEF.INC/NOLIST'
C
C Parameter declarations
C
	RECORD /HOUSEKEEPING_STATS/ STATS
	REAL*4 VALUE
C
	INTEGER NCH,NCH2
	CHARACTER*100 FNAME
C
C COMMON blocks
C
	INTEGER NUM_SR
	REAL*8 THIS_TIME
	RECORD /T/ W_THIS_TIME
	COMMON /INFO/ THIS_TIME,W_THIS_TIME,NUM_SR
C
	INTEGER WH_LUN,WH_LUN2,NEXCEED,NCHL1,NCHMAIL
	CHARACTER*80 L1NAME,MAIL_LIST
	COMMON /REPORT/ WH_LUN,WH_LUN2,L1NAME,NCHL1,NEXCEED,MAIL_LIST,NCHMAIL
C
	IF (NEXCEED .EQ. 0) THEN
	  INQUIRE(UNIT=WH_LUN,NAME=FNAME)
	  NCH = INDEX(FNAME, ']')
	  NCH2 = INDEX(FNAME, ' ')
	  IF (NCH2 .EQ. 0) NCH2 = LEN(FNAME)
	  WRITE(WH_LUN,101) L1NAME(1:NCHL1)
101	  FORMAT('                           WIND SMS detail report'/
     &		 ' '/
     &		 'Level 1 file: ',A//
     &		 ' '/
     &		 'Below is a list of parameters which exceeded their limits.'//
     &		 '                                                                   Sci'/
     &		 'Parameter name                               Value     Limit Units Rec   Time'/
     &		 '---------------------------------------- --------- --------- ----  --- --------')
	  WRITE(WH_LUN2,102) L1NAME(1:NCHL1),FNAME(NCH+1:NCH2),FNAME(1:NCH)
102	  FORMAT('WIND SMS brief report'/
     &		 'Level 1 file: ',A/
     &		 ' '/
     &		 'More detailed information can be found in the file '//
     &		 T10,A//
     &		 'located in   ',A/
     &		 ' '/
     &		 'The WIND SMS parameters listed below have exceeded their limits:'/
     &		 ' ')
	END IF
	NEXCEED = NEXCEED + 1
C
C Write out a 1-line message identifying the parameter, the value and the limit
C it exceeded, and when it happened (both science record number and time of
C day). 
C
C Message format:
C
C 12345678901234567890123456789012345678901234567890123456789012345678901234567890
C Parameter name                               Value     Limit Units Rec   Time
C IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII VVVVVV.VV VVVVVV.VV UUUU  NNN HH:MM:SS
C
	IF (VALUE .GT. STATS.HI_LIMIT) THEN
	  WRITE(WH_LUN,103) STATS.IDENT,VALUE,STATS.HI_LIMIT,STATS.UNITS,
     &			  NUM_SR,W_THIS_TIME.HR,W_THIS_TIME.MIN,W_THIS_TIME.SEC
	ELSE
	  WRITE(WH_LUN,103) STATS.IDENT,VALUE,STATS.LO_LIMIT,STATS.UNITS,
     &			  NUM_SR,W_THIS_TIME.HR,W_THIS_TIME.MIN,W_THIS_TIME.SEC
	END IF
103	FORMAT(A40, T42, F9.2, 1X, F9.2, 1X, A4, I5, 1X, I2.2,':',I2.2,':',I2.2)
C
C If this is the first time this parameter has exceeded either of its limits
C output just the parameter name to another file, opened to unit WH_LUN2.  This
C shorter file can be mailed to someone and easily read.
C One of HI_COUNT or LO_COUNT has already been incremented before we are
C called, so testing if their sum equals 1 satisfies the above condition.
C
	IF (STATS.HI_COUNT+STATS.LO_COUNT .EQ. 1) WRITE(WH_LUN2,104) STATS.IDENT
104	FORMAT(3X,A)
	RETURN
	END
