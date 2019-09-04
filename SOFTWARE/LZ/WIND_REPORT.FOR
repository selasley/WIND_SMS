      SUBROUTINE WIND_REPORT( TEXT, NVALUES, IARRAY )
C
C   This subroutine is a tool to track possible problems in the processing of
C WIND Level Zero data files.  It is called by various routines when those
C routines encounter a condition which should be reported.  This can range from
C a minor annoyance to a major error.  WIND_REPORT writes the information on a
C single line to the WIND report file.  
C
C  Parameters:
C
C    TEXT:
C
C	A CHARACTER variable or constant whose contents give a fuller
C	explanation of the condition being reported.  This can be formatted
C	by the calling routine using an internal WRITE statement, or it can
C	be a FAO (Formatted Ascii Output) string containing embedded format
C	specifiers.  The length of this argument may vary from one call to the
C	next.
C
C    NVALUES:
C
C	An INTEGER*4 value indicating the number of integer values required
C	by the format string TEXT.  The position of each of these values 
C	is denoted by the three characters "!UL" within the format string.
C	If no values need be included in the message NVALUES should be 0.
C	                                                                       
C    IARRAY:
C
C	An INTEGER*4 array which contains the integer values to be formatted
C	and included in the final message.  This array is NVALUES in length.
C	This argument is ignored if NVALUES is zero.
C
C
C WIND_REPORT writes the information on a single line to the WIND report file. 
C The first time WIND_REPORT is called it opens the report file.  If the report
C file does not exist it is created; if it already exists lines are appended to
C it. 
C
C The report file name is derived from the L1 filename and is located in the
C same directory as the L1 file. 
C
C The following information is written into the report file each time
C WIND_REPORT is called: the current science record counter, the current spin
C (Mspin in WIND_CD_READ), and a string formatted by the LIB$FAO system
C service.  In addition, the first time WIND_REPORT is called a header is
C written to the file giving the input Level Zero filename, the date and time,
C and other useful information.
C The format of the report file entry is given below.
C
C Sci Rec 
C Counter  Spin Text of msg
C nnnnnnnn  ss  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
C 1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
C          1         2         3         4         5         6         7         8         9        10
C 
C   This version:   1.3   12-APR-1995
C
C   Created by:
C
C       Larry Bleau    9-FEB-1995
C 
C   Revisions:
C 1.1	10-FEB-1995	L. Bleau	completely rewrote
C 1.2	23-FEB-1995	L. Bleau	do nothing if PRINTLZ_MODE set
C 1.3	12-APR-1995	L. Bleau	added include L1INFO.INC
C
CDEC$	IDENT  '1.3'

	IMPLICIT NONE
	INCLUDE 'INC_DIR:EDBDEF.INC/NOLIST'
	INCLUDE 'INC_DIR:EDBVARS.INC/NOLIST'
	INCLUDE 'INC_DIR:WIND_LUN.INC/NOLIST'
	INCLUDE 'INC_DIR:DIAG.INC/NOLIST'
	INCLUDE 'INC_DIR:L1INFO.INC/NOLIST'
	INCLUDE '($FORIOSDEF)/NOLIST'

	CHARACTER*(*) TEXT
	INTEGER*4 NVALUES, IARRAY(10)
C
	INTEGER ISTAT,IPOS,NCH,DEVDIR_LEN,NCH0,NCH1,I
	INTEGER*2 OUTLEN
	INTEGER SYS$FAOL
	CHARACTER DATETIME*23,DEVDIR*60,REPNAME*90,LINE*200
	LOGICAL FILE_OPEN

	CHARACTER*80 FILENAME, FIL1NAME
	COMMON /FILE/  FILENAME, FIL1NAME
C
C If PRINTLZ mode don't do anything
C
	IF (PRINTLZ_MODE) RETURN
C
C Check if file is already open
C
	INQUIRE( LOGF_Lun, OPENED=FILE_OPEN )
	IF (.NOT. FILE_OPEN) THEN
C
C Create report file each time to keep info from diffrent runs separate
C Construct report filename from the L1 filename and make it reside in the same
C directory as the L1 file.  Change the file extension to .REPORT for most
C cases.  For NRT data, where the extension is not .DAT and serves to
C distinguish different data sets, make the extension part of the filename (see
C case 2 below) and add .REPORT as the new extension.
C
C 1)  WI_L1_SMS_yyyymmdd_Vvv.DAT   WI_L1_SMS_yyyymmdd_Vvv.REPORT
C 2)  WI_L1_SMS_yyyymmdd_Vvv.kkkk  WI_L1_SMS_yyyymmdd_Vvv_kkkk.REPORT
C 3)  yymmdd_Vvv.DAT               yymmdd_Vvv.REPORT
C
	  REPNAME = FIL1NAME
	  IPOS = INDEX( REPNAME, ';' )		! strip off version #
	  IF (IPOS .NE. 0) REPNAME(IPOS:) = ' '
	  NCH = INDEX( REPNAME, ' ') - 1	! get # of chars
	  IF (IPOS .EQ. -1) NCH = LEN(REPNAME)
	  IPOS = INDEX( REPNAME, ']' )		! extract device, directory
	  IF (IPOS .NE. 0) THEN
	    DEVDIR = REPNAME(1:IPOS)		! save device/directory
	    DEVDIR_LEN = IPOS
	    REPNAME = REPNAME(IPOS+1:)
	    NCH = NCH - IPOS
	  ELSE
	    DEVDIR = '[]'
	    DEVDIR_LEN = 2
	  ENDIF
	  IPOS = INDEX( REPNAME, '.' )		! locate file type
	  IF (IPOS .EQ. 0) THEN			! none, add one
	    REPNAME(NCH+1:NCH+7) = '.REPORT'
	    IPOS = NCH + 1
	    NCH = NCH + 7
	  ELSE					! if .DAT just replace
	    IF (REPNAME(IPOS:IPOS+3) .EQ. '.DAT' .AND. IPOS+3 .EQ. NCH) THEN
	      REPNAME(IPOS:IPOS+6) = '.REPORT'
	      NCH = IPOS + 6
	    ELSE				! not .DAT, keep file type
	      REPNAME(IPOS:IPOS) = '_'		! combine it with filename
	      REPNAME(NCH+1:NCH+7) = '.REPORT'	! and use standard file type
	      IPOS = NCH + 1
	      NCH = NCH + 7
	    ENDIF
	  ENDIF
c	type *,'report name = "',repname(1:nch),'"'
c	type *,'device/direcory = "',devdir(1:devdir_len),'"'
C
C Now create and open file
C
	  OPEN( UNIT          = LOGF_Lun,
     &        FILE            = REPNAME(1:NCH),
     &        DEFAULTFILE     = DEVDIR(1:DEVDIR_LEN),
     &        STATUS          = 'NEW',
     &        ACCESS          = 'SEQUENTIAL',
     &        RECORDSIZE      = 200,
     &        CARRIAGECONTROL = 'LIST',
     &        IOSTAT	      = ISTAT,
     &        ERR             = 900 )
C
C Get information to write header to report file
C This includes:
C   name of LZ file
C   name of L1 file
C   current date and time
C
	  CALL LIB$DATE_TIME(DATETIME)
C
C Get lengths of filenames
C
	  NCH0 = INDEX( FILENAME, ' ') - 1
	  IF (NCH0 .EQ. -1) NCH0 = LEN(FILENAME)
	  NCH1 = INDEX( FIL1NAME, ' ') - 1
	  IF (NCH1 .EQ. -1) NCH1 = LEN(FIL1NAME)
C
C Write header info into report file
C
	  WRITE(LOGF_Lun, 101) REPNAME(1:IPOS-1),FILENAME(1:NCH0),
     &			       FIL1NAME(1:NCH1),DATETIME(1:20)
101	FORMAT( T<MAX(40-(IPOS+9)/2,1)>,'Data Set: ',A//
     &		'Level Zero file:  ',A/
     &		'Level 1 file:     ',A/
     &		'Date/Time of run: ',A/
     &		/
     &		' Sci Rec  Spin'/
     &		' Counter  Num  Message'/
     &		'========  ==   =========================================================')
	ENDIF
C
C Common Code
C Using parameters caller passed in IARRAY, format the output string using
C the system service $FAO
C
	IF (NVALUES .NE. 0) THEN
	  ISTAT = SYS$FAOL( TEXT, OUTLEN, LINE, IARRAY)
	  IF (ISTAT .NE. 1) THEN
	    WRITE(LINE,102) ISTAT
102	    FORMAT('ERROR CALLING FAO ROUTINE, ISTAT=',I8)
	    OUTLEN = 41
	  ENDIF
	ELSE
	  LINE = TEXT
	  OUTLEN = LEN(TEXT)
	ENDIF
C
C Write info into report file
C
	IF (Curr_Spin .NE. -1) THEN
	  WRITE(LOGF_Lun,103) Curr_Sci_Rec_Number,Curr_Spin,LINE(1:OUTLEN)
103	  FORMAT(I8,2X,I2,3X,A)
	ELSE
	  WRITE(LOGF_Lun,104) Curr_Sci_Rec_Number,LINE(1:OUTLEN)
104	  FORMAT(I8,2X,'  ',3X,A)
	END IF
	RETURN
C
900	TYPE 105,ISTAT
105	FORMAT(/' *** Cannot open REPORT file, Fortran error code ',I5/)
	RETURN
	END
