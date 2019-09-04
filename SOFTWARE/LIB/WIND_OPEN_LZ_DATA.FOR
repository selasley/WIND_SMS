      SUBROUTINE WIND_OPEN_LZ_DATA( FILENAME, TYPE, STATUS )
C+
C     This version:   1.2   17-FEB-1995
C
C     USAGE and PARAMETERS:
C       CALL WIND_OPEN_LZ_DATA( FILENAME, TYPE, STATUS )
C
C       FILENAME  CHAR*(*)	Input string containing the name
C				of the file to be opened.
C       TYPE	  CHAR*3	Input string containing 'NEW' or 'OLD'
C				to indicate what type file to open.
C       STATUS	  INTEGER*4	Output variable containing the completion
C				status of the subroutine's execution.
C
C     CALLS/FUNCTIONS:
C
C     CREATION:
C        6 Feb 1992, S. R. Nylund
C
C     REVISIONS:
C	23-FEB-1994	H.BROER         adopted from EPIC to WIND
C	 5-APR-1994	H.BROER         name changed
C        1-DEC-1994     L. Bleau        changed WRITE unit from KB to TTOUT
C 1.2   17-FEB-1995	L. Bleau	changed SFDU_Lun to L0_Lun, since it
C					refers to the L0 (or LZ) file and not
C					to the SFDU file; changed declaration
C					of FILENAME to CHAR*(*); changed
C					comment delimiter from ! to C
C-
CDEC$ IDENT '1.2'
      IMPLICIT NONE

      INCLUDE	'INC_DIR:SW_CODES.INC'
      INCLUDE	'INC_DIR:L0_Labl_Record.INC'
      INCLUDE	'INC_DIR:L0_DATA_Record.INC'
      INCLUDE	'INC_DIR:WIND_Lun.INC'		! WIND SYSTEM Logical Unit #s
      INCLUDE	'($SSDEF)'

      INTEGER*4		Record_Size,
     +			STATUS

      CHARACTER*(*)	FILENAME
      CHARACTER*3	TYPE

      STATUS = SS$_NORMAL			! Start with successful status

      INQUIRE ( FILE = FILENAME, RECL = Record_Size )
      WRITE( TTOUT, 100 ) Record_Size
  100 FORMAT( '  Record_Size =', I7 )
      IF( Record_Size .EQ. 0 )   THEN
          Record_Size	= 12800			! Max.length of WIND data record
      ENDIF					! in bytes
C
C   Open old or new file
C
      IF ((TYPE .EQ. 'OLD') .OR. (TYPE  .EQ. 'old'))    THEN

          OPEN(	UNIT	= L0_Lun,
     +		NAME	= FILENAME,
     +		FORM	= 'FORMATTED',
     +		TYPE	= 'OLD',
     +		RECL	= Record_Size,		! 2700*4 = 10800 bytes
     +		SHARED,
     +		READONLY,
     +		ERR	= 2000,			! Return if error
     +		IOSTAT	= STATUS)

          IF (STATUS .EQ. 0)   THEN
              READ( L0_Lun, 400 )   LR_Buffer
              Record_Size	= LR_Phys_Rec_Size

              CLOSE( L0_Lun )

              OPEN ( UNIT	= L0_Lun,
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
              WRITE( TTOUT, 200 )
          ENDIF
      ENDIF

      RETURN

2000  TYPE 2001, STATUS
      RETURN

  200 FORMAT(' ***ERROR: Trying to open DATA not as OLD ***' )
  400 FORMAT( 12800A1 )
 2001 FORMAT(' ***ERROR in WIND_OPEN_LZ_DATA, STATUS =', I6, ' ***')
      END
