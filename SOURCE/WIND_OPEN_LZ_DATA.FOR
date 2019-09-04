      SUBROUTINE WIND_OPEN_LZ_DATA( FILENAME, TYPE, STATUS )
!+
!       This version	5-APR-1994
!
!     REFERENCE:
!
!     USAGE and PARAMETERS:
!       CALL WIND_OPEN_LZ_DATA( FILENAME, TYPE, STATUS )
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
!	23-FEB-1994	H.BROER	adopted from EPIC to WIND
!	 5-APR-1994	H.BROER	name changed
!-
      IMPLICIT NONE

!      DATA	Ver_EPIC_OPEN_L0 / 1.00, 0/	! Software version number

      INCLUDE	'INC_DIR:SW_CODES.INC'
      INCLUDE	'INC_DIR:L0_Labl_Record.INC'
      INCLUDE	'INC_DIR:L0_DATA_Record.INC'
      INCLUDE	'INC_DIR:WIND_Lun.INC'		! WIND SYSTEM Logical Unit #s
      INCLUDE	'($SSDEF)'

      INTEGER*4		Record_Size,
     +			STATUS

      CHARACTER*80	FILENAME
      CHARACTER*3	TYPE

!     ------------------------------------------------------------------------

      STATUS = SS$_NORMAL			! Start with successful status

      INQUIRE ( FILE = FILENAME, RECL = Record_Size )
      WRITE( KB, 100 ) Record_Size
  100 FORMAT( '  Record_Size =', I7 )
      IF( Record_Size .EQ. 0 )   THEN
          Record_Size	= 12800			! Max.length of WIND data record
      ENDIF					! in bytes

!   Open old or new file

      IF ((TYPE .EQ. 'OLD') .OR. (TYPE  .EQ. 'old'))    THEN

          OPEN(	UNIT	= SFDU_Lun,
     +		NAME	= FILENAME,
     +		FORM	= 'FORMATTED',
     +		TYPE	= 'OLD',
     +		RECL	= Record_Size,		! 2700*4 = 10800 bytes
     +		SHARED,
     +		READONLY,
     +		ERR	= 2000,			! Return if error
     +		IOSTAT	= STATUS)

          IF (STATUS .EQ. 0)   THEN
              READ( SFDU_Lun, 400 )   LR_Buffer
              Record_Size	= LR_Phys_Rec_Size

              CLOSE( SFDU_Lun )

              OPEN ( UNIT	= SFDU_Lun,
     +			NAME	= FILENAME,
     +			FORM	= 'FORMATTED',
     +			TYPE	= 'OLD',
     +			RECL	= Record_Size,
     +			SHARED,
     +			READONLY,
     +			ERR	= 2000,			! Return if error
     +			IOSTAT	= STATUS)

              IF (STATUS .EQ. 0)	STATUS = SS$_NORMAL
          ELSE
              WRITE( KB, 200 )
          ENDIF
      ENDIF

      RETURN

2000  TYPE 2001, STATUS
      RETURN

  200 FORMAT(' ***ERROR: Trying to open DATA not as OLD ***' )
  400 FORMAT( 12800A1 )
 2001 FORMAT(' ***ERROR in WIND_OPEN_LZ_DATA, STATUS =', I6, ' ***')
      END
