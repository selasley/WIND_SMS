DR_Beg_MajFr_Cnt
      SUBROUTINE EPIC_READ_L0_DATA_RECORD(L0_FLAG, STATUS)
!+
!     EPIC_READ_L0_DATA_RECORD
!       This subroutine ...
!
!     REFERENCE:
!
!     USAGE and PARAMETERS:
!       CALL EPIC_READ_L0_DATA_RECORD(L0_FLAG, STATUS)
!       L0_FLAG	  INTEGER*4	Input variable containing a flag telling
!				whether to change questionable data to FILL
!				 L0_FLAG = 1: change questionable data to FILL
!				 L0_FLAG = 0: leave questionable data as is
!       STATUS	  INTEGER*4	Output variable containing the completion
!				status of the subroutine's execution.
!
!     CALLS/FUNCTIONS:
!
!     CREATION:
!        9 Mar 1992, S. R. Nylund
!     REVISIONS:
!       24 Jun 1993, S. R. Nylund
!         Added code to convert bad quality data to fill data.
!-
      IMPLICIT NONE

      DATA	Ver_EPIC_READ_L0_DATA_RECORD / 1.00, 0/ ! Software version number

      INCLUDE	'INC_DIR:SW_CODES.INC'
      INCLUDE	'INC_DIR:MAINSEC_L0.INC'
      INCLUDE	'($SSDEF)'
      INCLUDE	'($FORIOSDEF)'




      INTEGER*4	IOS,
     +		J,
     +		K,
     +		L0_FLAG


	LOGICAL*1 X	/.TRUE./


      INTEGER*4	STATUS

!     ----------------------------------------------------------------------

      STATUS = SS$_NORMAL	! Start with successful status


      IF ( X ) THEN
         READ (L0_Unit'L0_FILE_REC_Pntr, IOSTAT=IOS, ERR=2000)
     +	       DR_Buffer
         DR_Beg_MajFr_Cnt = 2 * DR_Beg_MajFr_Cnt  ! Correct for NASA's problem

         ! Change bad quality data to fill data?
         IF (L0_FLAG .EQ. 1) THEN
            DO J = 1, 512
               IF ((DR_MinFr_Qual(J) .NE. 0) .AND.	! Good data
     +		   (DR_MinFr_Qual(J) .NE. 4)) THEN	! FILL data
                  DR_MinFr_Qual(J) = 4
                  DO K = 1, 20
                     DR_MinFr(K,J) = 0
                  ENDDO
               ENDIF
            ENDDO
         ENDIF
      ELSE
         READ (L0_Unit'L0_FILE_REC_Pntr, IOSTAT=IOS, ERR=2000)
     +	       DR_Buffer_Contingency
      ENDIF

      ! Convert from year,doy,msec,micsec to EPOCH format
      CALL ICSS_CNVRT_TO_EPOCH(DR_Beg_Year, DR_Beg_DOY, DR_Beg_MSec, 0,
     +			       DR_Beg_EPOCH, STATUS)

1000  RETURN

2000  IF (IOS .EQ. FOR$IOS_INPRECTOO) THEN
        ! input record too long 
      	TYPE *,' *** ERROR: L0 DATA RECORD TOO LONG ***'
      ELSEIF (IOS .EQ. FOR$IOS_ERRDURREA ) THEN
        ! error during read 
      	TYPE *,' *** ERROR: ERROR DURING READ OF L0 DATA ***'
      ELSEIF (IOS .EQ. FOR$IOS_ATTACCNON ) THEN
        ! attempt to access non-existent record
      	L0_EOF = .TRUE.
      	TYPE *,' --- NOTICE: EOF ENCOUNTERED DURING READ OF L0 DATA ---'
      ELSEIF (IOS .EQ. FOR$IOS_RECNUMOUT ) THEN
        ! record number outside range 
      	TYPE *,' *** ERROR:  RECORD NUMBER OUTSIDE RANGE DURING READ OF L0 DATA ***'
      ELSE
      	TYPE *,' *** ERROR: L0 DATA READ ERROR, IOSTAT = ', IOS,' ***'
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
