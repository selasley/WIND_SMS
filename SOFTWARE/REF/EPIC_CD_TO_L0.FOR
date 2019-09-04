      PROGRAM EPIC_CD_to_L0
!+
!     EPIC_CD_to_L0
!       This program transfers the data (24-hr span) from a CD-ROM to
!       a Level 0 disk file and appends several major frames of data from the
!       CD for the next sequencial day,
!       OR
!       pulls data from EDB0.dat and simulates header data.
!
!     REFERENCE:
!
!     USAGE and PARAMETERS:
!       RUN EPIC_CD_to_L0
!
!     CALLS/FUNCTIONS:
!         CALL EPIC_OPEN_L0(FILENAME, TYPE, STATUS)
!         CALL EPIC_OPEN_CD(FILENAME, TYPE, STATUS)
!         CALL EPIC_READ_CD_LABL_RECORD(STATUS)
!       STATUS	  INTEGER*4	Output variable containing the completion
!				status of the subroutine's execution.
!         CALL EPIC_READ_CD_DATA_RECORD(STATUS)
!   !!!   CALL EPIC_WRITE_L0_FILE_HEADER(STATUS)
!         CALL EPIC_WRITE_L0_LABL_RECORD(STATUS)
!         CALL EPIC_WRITE_L0_DATA_RECORD(STATUS)
!         CALL EPIC_CLOSE_CD(STATUS)
!         CALL EPIC_CLOSE_L0(STATUS)
!
!     CREATION:
!        9 Mar 1992, S. R. Nylund
!     REVISIONS:
!-
      IMPLICIT NONE

      DATA	Ver_EPIC_SFDU_to_L0 / 1.00, 0/ ! Software version number
!*!	NOTE: Ver_EPIC_SFDU_to_L0 is used later in code
!*!      DATA	Ver_EPIC_CD_to_L0 / 1.00, 0/ ! Software version number

      INCLUDE	'INC_DIR:SW_CODES.INC'
      INCLUDE	'INC_DIR:MAINSEC_L0.INC'
      INCLUDE	'INC_DIR:MAINSEC_L1.INC'
      INCLUDE	'($SSDEF)'

      CHARACTER*10800	C_Day2(7)	! <-- MUST be set to I$Append
      CHARACTER*1	C_TAPE_NO
      CHARACTER*80	FILENAME
      CHARACTER*3	TYPE
      CHARACTER*23	C_TIME

      INTEGER*4		Day2_Beg_MajFr_Cnt,
     +			Day2_Beg_SC_Clock(2),
     +			Day2_Beg_Year,
     +			Day2_Beg_DOY,
     +			Day2_Beg_MSec

      INTEGER*4	SYS$ASCTIM,
     +		DOY,
     +		I$Append	/ 7 /,
     +		Prev_MajFr_Cnt,
     +		MajFr_Total	/ 0 /,
     +		MajFr_Jumps	/ 0 /,
     +		Num_CD,
     +		I,
     +		J,
     +		TAPE_No,
     +		Num_RecToRead,
     +		Num_RecRead	/ 0 /



      LOGICAL*1	First_MajFr	/.TRUE./

!     ------------------------------------------------------------------------


!**********************
!  OPEN LEVEL-0 FILE  *
!**********************
   19 TYPE 900, 'Enter output filename (e.g. EPI_L0_92240_V01.DAT):  '
      ACCEPT 910, FILENAME
      TYPE = 'NEW'
      CALL EPIC_OPEN_L0(FILENAME, TYPE, STATUS)
      IF (STATUS .NE. SS$_NORMAL) THEN
         GOTO 19
      ENDIF


   20 TYPE 900, 'Enter number of data records to read (-1 for all):  '
      ACCEPT 902, Num_RecToRead
      IF (( Num_RecToRead .EQ.  0) .OR. ( Num_RecToRead .LT.  -1)) THEN
         TYPE *,'*** INVALID VALUE, please reenter ***'
         GOTO 20
      ENDIF
      IF ( Num_RecToRead .EQ. -1) Num_RecToRead = 1000000  ! Set very high

!*!   TYPE 900, 'Enter tape drive unit # (0,1,2) OR -1 for sim data: '
      TAPE_NO = -1
  900 FORMAT('$',A)
!*!   ACCEPT 901, TAPE_NO
  901 FORMAT(I5)
      IF (TAPE_NO .GE. 0) THEN
         IF (TAPE_NO .EQ. 0) C_TAPE_NO = '0'
         IF (TAPE_NO .EQ. 1) C_TAPE_NO = '1'
         IF (TAPE_NO .EQ. 2) C_TAPE_NO = '2'
         FILENAME = 'MUB'//C_TAPE_NO//':'
         TYPE = 'OLD'
      ELSE
         FILENAME(1:23)='DKA400:[DATA.GE.EPI.LZ]'
         TYPE 900, 'Enter input CD filename (e.g. 93013101.DAT):       '
         ACCEPT 910, FILENAME(24:80)
910      FORMAT(A)
!*!      IF (FILENAME .EQ. ' ') FILENAME = 'TSTDATA_DIR:L0.DAT'
         TYPE 911, FILENAME
911      FORMAT(' Using ',A,' as input')
         TYPE = 'OLD'
         Num_CD = 1
         GOTO 500
      ENDIF

      TYPE 900, 'Enter 2 if using 2 input CD files or 1 if only 1: '
      ACCEPT 902, Num_CD
  902 FORMAT(I5)
      IF (Num_CD .EQ. 1) GOTO 500


!********************************************
!  GET DATA FROM NEXT (2ND) SEQUENTIAL DAY  *
!********************************************
      CALL EPIC_OPEN_CD(FILENAME, TYPE, STATUS)

!!!   Code removed to perhaps be replaces by read of detached SFDU Header file
!!!   CALL EPIC_READ_SFDU_FILE_HEADER(STATUS)
!!!   ! Save in special Location

      CALL EPIC_READ_CD_LABL_RECORD(STATUS)
      IF (STATUS .NE. SS$_NORMAL) TYPE *,'CD LABEL READ STATUS IS ',STATUS

      ! Confirm that this is the right tape
      IF (LR_SCID .NE. 24) THEN
         TYPE *,'*** ERROR: Wrong Spacecraft ID = ',LR_SCID,' ***'
         GOTO 1000
      ENDIF
      IF (LR_Instr_Num .NE. 6) THEN
         TYPE *,'*** ERROR: Wrong Spacecraft Number = ',LR_Instr_Num,' ***'
         GOTO 1000
      ENDIF

      ! Save portions of Label Record
      Day2_Beg_MajFr_Cnt =   LR_Beg_MajFr_Cnt
      Day2_Beg_SC_Clock(1) = LR_Beg_SC_Clock(1)
      Day2_Beg_SC_Clock(2) = LR_Beg_SC_Clock(2)
      Day2_Beg_Year =        LR_Beg_Year
      Day2_Beg_DOY =         LR_Beg_DOY
      Day2_Beg_MSec =        LR_Beg_MSec

      DO J = 1, I$Append
         CALL EPIC_READ_CD_DATA_RECORD(STATUS)
         IF (STATUS .EQ. EPIC$_READ_ERROR) CONTINUE
         ! Save the Data Records
         C_Day2(J) = C_DR_Buffer
      ENDDO

!*!      CALL EPIC_CLOSE_CD(STATUS)



!**************************************
!  TRANSFER 1ST DAY                   *
!**************************************

  500 CONTINUE
      IF (TAPE_NO .GE. 0) THEN
!*!         CALL EPIC_OPEN_CD(FILENAME, TYPE, STATUS)
      ELSE
         CALL EPIC_OPEN_CD(FILENAME, TYPE, STATUS)
      ENDIF

!!!   Code removed to perhaps be replaces by read of detached SFDU Header file
!!!   CALL EPIC_READ_SFDU_FILE_HEADER(STATUS)
!!!   ! Modify for appended 2nd day
!!!   CALL EPIC_WRITE_L0_FILE_HEADER(STATUS)

      IF (TAPE_NO .GE. 0) THEN
!*!         CALL EPIC_READ_CD_LABL_RECORD(STATUS)
      ELSE
         CALL EPIC_READ_CD_LABL_RECORD(STATUS)
      ENDIF

      ! Confirm that this is the right tape
      IF (LR_SCID .NE. 24) THEN
         TYPE *,'*** ERROR: Wrong Spacecraft ID = ',LR_SCID,' ***'
         GOTO 1000
      ENDIF
      IF (LR_Instr_Num .NE. 6) THEN
         TYPE *,'*** ERROR: Wrong Spacecraft Number = ',LR_Instr_Num,' ***'
         GOTO 1000
      ENDIF

      IF (Num_CD .NE. 1) THEN
         ! Modify for appended 2nd day (information from prior read of 2nd day)
         LR_Max_Phys_Rec_per_File = LR_Max_Phys_Rec_per_File + I$Append	
         LR_End_MajFr_Cnt         = DR_Beg_MajFr_Cnt
         LR_End_SC_Clock(1)       = DR_Beg_SC_Clock(1)
         LR_End_SC_Clock(2)       = DR_Beg_SC_Clock(2)
         LR_End_Year              = DR_Beg_Year
         LR_End_DOY               = DR_Beg_DOY
         LR_End_MSec              = DR_Beg_MSec
         LR_Num_MajFr_File        = LR_Num_MajFr_File + I$Append
      ENDIF




  600 CONTINUE

      ENCODE (8, 905, LR_L0_Prog_V_Num) Ver_EPIC_SFDU_to_L0(1)
  905 FORMAT(F8.2)

!     Store current time as production run time.
      I = SYS$ASCTIM(,C_TIME,,)			! Get current clock time
      LR_L0_Run_DateTime(1:4)   = C_TIME(8:11)	! Save Year
      CALL JULIAN_DOY (C_TIME, DOY)	! Convert Day to Day Of Year (DOY)
      ENCODE (3, 906, LR_L0_Run_DateTime(5:7)) DOY ! Save Day Of Year (DOY)
  906 FORMAT(I3)
      IF (DOY .LT. 100) LR_L0_Run_DateTime(5:5) = '0'
      IF (DOY .LT.  10) LR_L0_Run_DateTime(6:6) = '0'
      LR_L0_Run_DateTime(8:9)   = C_TIME(13:14)	! Save Hour
      LR_L0_Run_DateTime(10:11) = C_TIME(16:17)	! Save Minute
      LR_L0_Run_DateTime(12:13) = C_TIME(19:20)	! Save Second
      LR_L0_Run_DateTime(14:15) = C_TIME(22:23)	! Save tenths, hundredths sec
      LR_L0_Run_DateTime(16:16) = '0'		! 0 for thousandths of sec

      TYPE *,'>>> PROCESSING CD DATA TO LEVEL-0 FORMAT'
      TYPE *,'    from ', LR_Beg_Year, LR_Beg_DOY, LR_Beg_MSec
      TYPE *,'    to   ', LR_End_Year, LR_End_DOY, LR_End_MSec
      TYPE *,'         ( Major Frames: ', LR_Beg_MajFr_Cnt,
     +				   ' - ', LR_End_MajFr_Cnt,')'
      TYPE *,'    on   ', C_Time,'(',LR_L0_Run_DateTime,')'
      TYPE *,'    via  EPIC_CD_to_L0 version ', LR_L0_Prog_V_Num

      CALL EPIC_WRITE_L0_LABL_RECORD(STATUS)

      DO WHILE ((.NOT. SFDU_EOF) .AND. (Num_RecToRead .GT. Num_RecRead))
         IF (TAPE_NO .GE. 0) THEN
!*!            CALL EPIC_READ_CD_DATA_RECORD(STATUS)
         ELSE
            CALL EPIC_READ_CD_DATA_RECORD(STATUS)
         ENDIF
         Num_RecRead = Num_RecRead + 1 
D        TYPE *,'Num_RecRead=',Num_RecRead 
         IF (STATUS .EQ. EPIC$_READ_ERROR) CONTINUE
         IF (SFDU_EOF) GOTO 800

         IF (.NOT. First_MajFr) THEN
           IF (DR_Beg_MajFr_Cnt .NE. Prev_MajFr_Cnt + 1) THEN
!          IF (DR_Beg_MajFr_Cnt .NE. Prev_MajFr_Cnt + 2) THEN
             IF ((DR_Beg_MajFr_Cnt .EQ. 0) .AND. (Prev_MajFr_Cnt .EQ. 7)) THEN
                Continue
             ELSE
                MajFr_Jumps = MajFr_Jumps + 1 
                TYPE *,'*** Major Frame Jump', Prev_MajFr_Cnt, DR_Beg_MajFr_Cnt
             ENDIF
           ENDIF
         ENDIF
         First_MajFr = .FALSE.
         Prev_MajFr_Cnt = DR_Beg_MajFr_Cnt

         CALL EPIC_WRITE_L0_DATA_RECORD(STATUS)
         MajFr_Total = MajFr_Total + 2 
      ENDDO

  800 CONTINUE
         IF (TAPE_NO .GE. 0) THEN
!*!            CALL EPIC_CLOSE_CD(STATUS)
         ELSE
!*!            CALL EPIC_CLOSE_CD(STATUS)
         ENDIF

      IF (Num_CD .NE. 1) THEN
         DO J = 1, I$Append
            ! Recall append info from special location(STATUS)
            C_DR_Buffer = C_Day2(J)
            CALL EPIC_WRITE_L0_DATA_RECORD(STATUS)
         ENDDO
      ENDIF



1000  CALL EPIC_CLOSE_L0(STATUS)

      TYPE *, ' '
      TYPE *, 'Total Number of Major Frame Jumps', MajFr_Jumps 

      STOP
      END

      SUBROUTINE EPIC_OPEN_CD(FILENAME, TYPE, STATUS)
!+
!     EPIC_OPEN_CD
!       This subroutine...
!
!     REFERENCE:
!
!     USAGE and PARAMETERS:
!       CALL EPIC_OPEN_CD(FILENAME, TYPE, STATUS)
!       FILENAME  CHAR*80	Input string containing the name
!				of the file to be opened.
!       TYPE	  CHAR*3	Input string containing 'NEW' or 'OLD'
!				to indicate what type file to open.
!       STATUS	  INTEGER*4	Output variable containing the completion
!				status of the subroutine's execution.
!
!     CALLS/FUNCTIONS:
!
!     CREATION:
!        6 Feb 1992, S. R. Nylund
!     REVISIONS:
!-
      IMPLICIT NONE

!*!      DATA	Ver_EPIC_OPEN_CD / 1.00, 0/	! Software version number

      INCLUDE	'INC_DIR:SW_CODES.INC'
      INCLUDE	'INC_DIR:MAINSEC_L0.INC'
      INCLUDE	'($SSDEF)'

      CHARACTER*80	FILENAME
      CHARACTER*35	FILENAME2
      CHARACTER*3	TYPE


      INTEGER*4	STATUS

!     ------------------------------------------------------------------------

      STATUS = SS$_NORMAL	! Start with successful status




      ! Open old ONLY
      IF ((TYPE .EQ. 'OLD') .OR. (TYPE  .EQ. 'old')) THEN

	FILENAME2 = FILENAME
!*!	FILENAME2 = 'DKA400:[DATA.GE.EPI.LZ]93080301.DAT'
           CALL C_OPEN(FILENAME2)
!*!         OPEN (UNIT       = SFDU_Unit,
!*!     +         NAME       = FILENAME,
!*!     +         FORM       = 'UNFORMATTED',
!*!     +         TYPE       = 'OLD',
!*!     +         ACCESS     = 'SEQUENTIAL',
!*!     +         BLOCKSIZE  = 10800,
!*!!!!     +         RECORDTYPE = 'VARIABLE',
!*!     +         READONLY)
      ELSE
         TYPE *,'*** ERROR: TRYING TO OPEN CD AS NEW ***'
      ENDIF




1000  RETURN
      END

      SUBROUTINE EPIC_READ_CD_DATA_RECORD(STATUS)
!+
!     EPIC_READ_CD_DATA_RECORD
!       This subroutine ...
!
!     REFERENCE:
!
!     USAGE and PARAMETERS:
!       CALL EPIC_READ_CD_DATA_RECORD(STATUS)
!       STATUS	  INTEGER*4	Output variable containing the completion
!				status of the subroutine's execution.
!
!     CALLS/FUNCTIONS:
!
!     CREATION:
!        9 Mar 1992, S. R. Nylund
!     REVISIONS:
!-
      IMPLICIT NONE

!*!      DATA	Ver_EPIC_READ_CD_DATA_RECORD / 1.00, 0/ ! Software version number

      INCLUDE	'INC_DIR:SW_CODES.INC'
      INCLUDE	'INC_DIR:MAINSEC_L0.INC'
      INCLUDE	'($SSDEF)'
      INCLUDE	'($FORIOSDEF)'


      INTEGER*4	IOS				! I/O Status Value



      LOGICAL*1	X	/.TRUE./,
     +		LAST_READ_EOF  /.FALSE./



      INTEGER*4	STATUS,
     +		SIZE,
     +		S,
     +		J

!     ----------------------------------------------------------------------

      STATUS = SS$_NORMAL	! Start by assuming a successful status

      IF (LAST_READ_EOF) THEN	! Previous read hit EOF, so return
         SIZE = 0
         LAST_READ_EOF = .FALSE.
         GOTO 2000
      ENDIF

!*!      IF ( X ) THEN
!*!         READ (SFDU_Unit, IOSTAT=IOS, ERR=2100, END=2000) DR_Buffer
!*!      ELSE
!*!         READ (SFDU_Unit, IOSTAT=IOS, ERR=2100, END=2000) DR_Buffer_Contingency
!*!      ENDIF
      SIZE = 10800
      CALL C_READ(DR_Buffer, SIZE, STATUS)
      IF (SIZE .LT. 10800) THEN
         TYPE *,'*** C_READ:  CD Read of only ',SIZE,' BYTES ***'
         DO J = S+1, 10800
            DR_Buffer(J) = 0
         ENDDO
         IF (STATUS .EQ. -1) THEN
            IF (SIZE .EQ. 0) GOTO 2000
            LAST_READ_EOF = .TRUE.
         ENDIF
      ENDIF
!*!      IF ((STATUS .EQ. -1) .OR. (SIZE .EQ. 0)) GOTO 2000

1000  RETURN

2000  SFDU_EOF = .TRUE.
      STATUS = EPIC$_EOF_PR_LZ
	TYPE *,'**** END OF CD FILE ****'
      GOTO 1000

2100  IF (IOS .EQ. FOR$IOS_INPRECTOO) THEN	! INPUT RECORD TOO LONG
      	TYPE *,' *** ERROR: SFDU DATA RECORD TOO LONG ***'
      ELSEIF(IOS .EQ. FOR$IOS_ERRDURREA) THEN	! Error during read 
      	TYPE *,' *** ERROR: ERROR DURING READ OF SFDU DATA ***'
      ELSE
      	TYPE *,' *** ERROR: SFDU DATA READ ERROR, IOSTAT = ', IOS,' ***'
      ENDIF
      STATUS = EPIC$_READ_ERROR
      GOTO 1000


      END

      SUBROUTINE EPIC_READ_CD_LABL_RECORD(STATUS)
!+
!     EPIC_READ_CD_LABL_RECORD
!       This subroutine ...
!
!     REFERENCE:
!
!     USAGE and PARAMETERS:
!       CALL EPIC_READ_CD_LABL_RECORD(STATUS)
!       STATUS	  INTEGER*4	Output variable containing the completion
!				status of the subroutine's execution.
!
!     CALLS/FUNCTIONS:
!
!     CREATION:
!        9 Mar 1992, S. R. Nylund
!     REVISIONS:
!-
      IMPLICIT NONE

!*!      DATA	Ver_EPIC_READ_CD_LABL_RECORD / 1.00, 0/ ! Software version number

      INCLUDE	'INC_DIR:SW_CODES.INC'
      INCLUDE	'INC_DIR:MAINSEC_L0.INC'
      INCLUDE	'($SSDEF)'
      INCLUDE	'($FORIOSDEF)'


      INTEGER*4	IOS				! I/O Status Value

      INTEGER*4	STATUS,
     +		SIZE

!     ----------------------------------------------------------------------

      STATUS = SS$_NORMAL	! Start by assuming a successful status



!*!   READ (SFDU_Unit, IOSTAT=IOS, ERR=2100, END=2000) LR_Buffer
      SIZE =  10800
      CALL C_READ(LR_Buffer, SIZE, STATUS)
      IF (SIZE .LT. 10800) TYPE *,'*** C_READ:  CD Read of only ',SIZE,' BYTES ***'
      IF (STATUS .EQ. -1) GOTO 2000


1000  RETURN

2000  SFDU_EOF = .TRUE.
      STATUS = EPIC$_EOF_PR_LZ
      GOTO 1000

2100  IF (IOS .EQ. FOR$IOS_INPRECTOO) THEN	! INPUT RECORD TOO LONG
      	TYPE *,' *** ERROR: SFDU LABEL RECORD TOO LONG ***'
      ELSEIF(IOS .EQ. FOR$IOS_ERRDURREA) THEN	! Error during read 
      	TYPE *,' *** ERROR: ERROR DURING READ OF SFDU LABEL ***'
      ELSE
      	TYPE *,' *** ERROR: SFDU LABEL READ ERROR, IOSTAT = ', IOS,' ***'
      ENDIF
      STATUS = EPIC$_READ_ERROR
      GOTO 1000

      END
!  FOR$IOS_REWERR    = '00000014'X !   REWIND error 
!  FOR$IOS_INPRECTOO = '00000016'X !   input record too long 
!  FOR$IOS_BACERR    = '00000017'X !   BACKSPACE error 
!  FOR$IOS_RECNUMOUT = '00000019'X !   record number outside range 
!  FOR$IOS_OPEDEFREQ = '0000001A'X !   OPEN or DEFINE FILE required
!  FOR$IOS_TOOMANREC = '0000001B'X !   too many records in I/O statement
!  FOR$IOS_CLOERR    = '0000001C'X !   CLOSE error 
!  FOR$IOS_FILNOTFOU = '0000001D'X !   file not found 
!  FOR$IOS_OPEFAI    = '0000001E'X !   open failure 
!  FOR$IOS_INVLOGUNI = '00000020'X !   invalid logical unit number 
!  FOR$IOS_SEGRECFOR = '00000023'X !   segmented record format error 
!  FOR$IOS_ATTACCNON = '00000024'X !   attempt to access non-existent record
!  FOR$IOS_INCRECLEN = '00000025'X !   inconsistent record length 
!  FOR$IOS_ERRDURWRI = '00000026'X !   error during write 
!  FOR$IOS_ERRDURREA = '00000027'X !   error during read 
!  FOR$IOS_NO_SUCDEV = '0000002A'X !   no such device 
!  FOR$IOS_FILNAMSPE = '0000002B'X !   file name specification error 
!  FOR$IOS_INCRECTYP = '0000002C'X !   inconsistent record type
!  FOR$IOS_INCOPECLO = '0000002E'X !   inconsistent OPEN/CLOSE ! s
!  FOR$IOS_WRIREAFIL = '0000002F'X !   write to READONLY file 
!  FOR$IOS_OUTSTAOVE = '00000042'X !   output statement overflows record 
!  FOR$IOS_INPSTAREQ = '00000043'X !   input statement requires too much data 
