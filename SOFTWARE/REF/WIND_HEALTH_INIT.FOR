	SUBROUTINE WIND_HEALTH_INIT(L1_LUN, L1FILENAME, OUTPUTFILENAME, NCH)

C+
C
C FUNCTIONAL DESCRIPTION:
C
C    This subroutine performs various initilization operations for the
C    WIND_HEALTH program.  Among them are:
C
C    	  Prompt user to input L1 filename
C	  Open the L1 file to unit L1_LUN, saving its header in L1_FILE_HEADER
C	  Construct the output filename, saving the name and its length in
C	    OUTPUTFILENAME and NCH
C	  Construct the summary filename and open it for output
C
C FORMAL PARAMETERS:
C
C     L1_LUN
C         An INTEGER containing the Fortran unit number on which the L1 file is
C         to be opened.
C
C     L1FILENAME
C         A CHARACTER variable which will receive the name of the input L1 file
C         selected by the user.
C
C     OUTPUTFILENAME
C         A CHARACTER variable which will receive the constructed name of the
C         output file.  The output file is not, however, opened.
C
C     NCH
C         An INTEGER variable which will receive the length of the name stored
C         in OUTPUTFILENAME.
C
C
C COMMON BLOCKS:
C
C     REPORT	  Information controlling the limit reporting feature of the
C     		  program.
C
C DESIGN:
C
C     Initialize some variables
C     Get input L1 filename
C     Construct output filename
C     Open file named after L1 file but with file type .SUMM on unit
C	WH_LUN; this will be used to hold out-of-limit messages generated while
C	processing the L1 file.
C     Open temporary file named WIND_HEALTH_BRIEF.TMP on unit WH_LUN2; this
C	will be used to hold a summary of out-of-limit messages generated while
C	processing the L1 file.  It will be mailed to a user then deleted at
C	the end of the program.
C     Keep mailing list in MAIL_LIST, length in NCHMAIL.  Get mailing list
C	at runtime by translating logical name WIND_HEALTH_MAIL; default to
C	current user if not defined.
C
C This version:   1.2   21-NOV-1995
C
C Created:
C
C	 14-NOV-1995    Larry Bleau
C
C Revised:
C
C	 Date     | Name  | Description
C ----------------+-------+-----------------------------------------------------
C 20-NOV-1995     |  LRB  | removed explicit 0 from call to LIB$GETJPI
C 21-NOV-1995     |  LRB  | added comments; added TYPE statement displaying
C			    filename
C [change_entry]
C
C-
CDEC$	IDENT  '1.2'
	IMPLICIT NONE
	INCLUDE	'INC_DIR:EDBDEF.INC/NOLIST'
	INCLUDE 'INC_DIR:L1FMT.INC/NOLIST'
	INCLUDE '($JPIDEF)/NOLIST'
C
C Parameter declarations
C
	CHARACTER*(*) L1FILENAME,OUTPUTFILENAME
	INTEGER L1_LUN,NCH,NCH2
C
        INTEGER STATUS,FORTERR,RMSSTC,RMSSTV,LUN,VAXERR
	INTEGER DEVDIR_LEN, IPOS
	CHARACTER*80 DEVDIR,BRIEF
	CHARACTER*30 TEMPNAME/'SYS$LOGIN:WIND_HEALTH_MAIL.TMP'/
	CHARACTER USERNAME*12
	INTEGER*2 UNCH
C
C COMMON blocks
C
	INTEGER WH_LUN,WH_LUN2,NEXCEED,NCHL1,NCHMAIL,GET_LOGICAL,LIB$GETJPI
	CHARACTER*80 L1NAME,MAIL_LIST
	COMMON /REPORT/ WH_LUN,WH_LUN2,L1NAME,NCHL1,NEXCEED,MAIL_LIST,NCHMAIL
C
C Initialize some variables
C
	WH_LUN = 15
	WH_LUN2 = 16
	NEXCEED = 0
	NCHMAIL = GET_LOGICAL( 'WIND_HEALTH_MAIL', MAIL_LIST)
	IF (NCHMAIL .EQ. 0) THEN
	  STATUS = LIB$GETJPI( JPI$_USERNAME, ,,, USERNAME, UNCH)
	  MAIL_LIST = USERNAME
	  NCHMAIL = INDEX(MAIL_LIST, ' ') - 1
	ENDIF
C
C Get input L1 filename
C
	L1FILENAME = 'DATA_DIR:WI*L1*.DAT'
	CALL WIND_GET_FILENAME(L1FILENAME, STATUS)
	IF (STATUS .NE .1) STOP
C
C Open L1 file for input
C
	CALL WIND_L1_OPEN( L1_LUN, L1FILENAME, L1_FILE_HEADER, STATUS)
	IF (STATUS .NE. 1) GOTO 9010
	L1NAME = L1FILENAME
	NCHL1 = INDEX(L1NAME, ' ') - 1
	IF (NCHL1 .EQ. -1) NCHL1 = LEN(L1NAME)
C
C Construct output filenames
C
C There are three output files: a detailed report of the instrument's health
C (called the output file) which has a file extension of .HEALTH; a summary of
C parameters whose value exceeds their limits (called a summary file) which has
C a file extension of .SUMM; and file listing only the names of the parameters
C whose value exceeds their limits (call the mail file or message file) which
C has a file extension of .TMP and is located in the user's login directory.
C
C Only the first two files are patterned after the input L1 file; the message
C file has a fixed name.  Both the detail file and summary file filenames are
C constructed here.  The summary and message files are both opened here.  The
C detail file is not opened here; it is opened by the main routine.
C
C Create output file each time to keep info from diffrent runs separate
C Construct output filename from the L1 filename and make it reside in the
C current directory.  Change the file extension to .HEALTH for most
C cases.  For NRT data, where the extension is not .DAT but serves to
C distinguish different data sets, make the extension part of the filename (see
C case 2 below) and add .HEALTH as the new extension.
C
C Do the same operations to construct the summary filename, but give it a file
C type of.SUMM .
C
C     L1 filename                  detail report filename
C 1)  WI_L1_SMS_yyyymmdd_Vvv.DAT   WI_L1_SMS_yyyymmdd_Vvv.HEALTH
C 2)  WI_L1_SMS_yyyymmdd_Vvv.kkkk  WI_L1_SMS_yyyymmdd_Vvv_kkkk.HEALTH
C 3)  yymmdd_Vvv.DAT               yymmdd_Vvv.HEALTH
C
	OUTPUTFILENAME = L1FILENAME
	IPOS = INDEX( OUTPUTFILENAME, ';' )		! strip off version #
	IF (IPOS .NE. 0) OUTPUTFILENAME(IPOS:) = ' '
	NCH = INDEX( OUTPUTFILENAME, ' ') - 1		! get # of chars
	IF (IPOS .EQ. -1) NCH = LEN(OUTPUTFILENAME)
	IPOS = INDEX( OUTPUTFILENAME, ']' )		! extract device, directory
	IF (IPOS .NE. 0) THEN
	  DEVDIR = OUTPUTFILENAME(1:IPOS)		! save device/directory
	  DEVDIR_LEN = IPOS
	  OUTPUTFILENAME = OUTPUTFILENAME(IPOS+1:)
	  NCH = NCH - IPOS
	ELSE
	  DEVDIR = '[]'
	  DEVDIR_LEN = 2
	ENDIF
	BRIEF = OUTPUTFILENAME
	IPOS = INDEX( OUTPUTFILENAME, '.' )		! locate file type
	IF (IPOS .EQ. 0) THEN				! none, add one
	  OUTPUTFILENAME(NCH+1:NCH+7) = '.HEALTH'
	  BRIEF(NCH+1:NCH+5) = '.SUMM'
	  IPOS = NCH + 1
	  NCH2 = NCH + 5
	  NCH = NCH + 7
	ELSE						! if .DAT just replace
	  IF (OUTPUTFILENAME(IPOS:IPOS+3) .EQ. '.DAT' .AND. IPOS+3 .EQ. NCH) THEN
	    OUTPUTFILENAME(IPOS:IPOS+6) = '.HEALTH'
	    BRIEF(IPOS:IPOS+4) = '.SUMM'
	    NCH = IPOS + 6
	    NCH2 = IPOS + 4
	  ELSE						! not .DAT, keep file type
	    OUTPUTFILENAME(IPOS:IPOS) = '_'		! combine it with filename
	    OUTPUTFILENAME(NCH+1:NCH+7) = '.HEALTH'	! and use standard file type
	    BRIEF(IPOS:IPOS) = '_'			! same with filename
	    BRIEF(NCH+1:NCH+13) = '.SUMM'		! stored in BRIEF
	    NCH2 = NCH + 5
	    IPOS = NCH + 1
	    NCH = NCH + 7
	  ENDIF
	ENDIF
D	type *,'detail report name  = "',outputfilename(1:nch),'"'
D	type *,'summary report name = "',brief(1:nch2),'"'
D	type *,'device/direcory = "',devdir(1:devdir_len),'"'
C
C Open file named after L1 file but with file type .SUMM on unit WH_LUN
C This will be used to hold out-of-limit messages generated while processing
C the L1 file.
C
	TYPE *,'WIND SMS instrument health summary limit report will be written to the file'
	TYPE *,BRIEF(1:NCH2)
	OPEN(WH_LUN, FILE=BRIEF(1:NCH2), STATUS='NEW', ERR=9020,
     &	     CARRIAGECONTROL='LIST')
C
C Open temporary file named WIND_HEALTH_BRIEF.TMP on unit WH_LUN2
C This will be used to hold a summary out-of-limit messages generated while
C processing the L1 file.  Since it will be mailed to a user and can be then
C deleted, its name need not be patterned after the L1 file's name.
C
	OPEN(WH_LUN2, FILE=TEMPNAME, STATUS='NEW', ERR=9020,
     &	     CARRIAGECONTROL='LIST')
	RETURN
C
C Come here for errors
C
9010	TYPE *,'ERROR OPENING L1 FILE; STATUS=',STATUS
	CALL EXIT(STATUS)
C
9020	TYPE *,'ERROR OPENING TEMPORARY FILE'
        CALL ERRSNS(FORTERR,RMSSTC,RMSSTV,LUN,VAXERR)
	CALL EXIT(RMSSTC)
	END
