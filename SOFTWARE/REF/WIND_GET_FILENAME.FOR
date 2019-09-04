      SUBROUTINE WIND_GET_FILENAME ( FILENAM, STATUS )
!+
!	SUBROUTINE WIND_GET_FILENAME
!	asks for a FILENAME and checks if this file exists.
!	STATUS	= 0	File does not exist
!		= 1	File exists
!
!	This version:   1.6    3-JAN-1995
!
!     REFERENCE:
!
!     USAGE and PARAMETERS:
!       CALL WIND_GET_FILENAME( FILENAM, STATUS )
!
!     CALLS/FUNCTIONS:
!       CALL TICHAR
!       CALL TILOG
!       CALL SELECT_FILE
!       = CHLAST
!       = INDEX
!       INQUIRE
!
!     CREATION:
!	18-FEB-1994	H. BROER	University of Maryland, Space Physics Group
!
!     REVISIONS:
!  1.1	22-JUN-1994	H. BROER	generalize for all files
!  1.2   4-NOV-1994     L. Bleau        changed KB to TTOUT in WRITE statements
!  1.3   9-NOV-1994     L. Bleau        allow wildcards in filename
!  1.4  10-NOV-1994     L. Bleau        make DATA_DIR:WI*LZ*.DAT the default name
!  1.5  29-NOV-1994	L. Bleau	change FILENAM to CHARACTER*(*)
!  1.6   3-JAN-1995     L. Bleau        changed internal logical vars to
!                                       LOGICAL, was LOGICAL*1, which caused
!                                       error when passed to TILOG
!-
CDEC$	IDENT	'1.6'

      IMPLICIT NONE

      INCLUDE	'INC_DIR:WIND_Lun.INC/NOLIST'
      INCLUDE	'($SSDEF)/NOLIST'

      INTEGER*4		CHLAST,		! internal value
     :			KK,		! internal value
     :			LL,		! internal value
     :			MM,		! internal value
     :			STATUS,		! Reporting execution status
     :			CONFIRM/0/	! set zero to indicate to SELECT_FILE
					! not to confirm file selection

      INTEGER*2 MESLEN			! message length

      LOGICAL		FILEEXIST,	! internal logical flag
     :			OK		! internal logical flag

      CHARACTER*(*)	FILENAM 	! resulting filename

      CHARACTER*80	FILENA,		! internal filename
     :			INFILE,		! internal filename
     :			PATTERN,	! internal filename
     :			MESSAGE		! internal message

      KK	= CHLAST( FILENAM )

      IF( KK .LT. 2 )   THEN
          INFILE	= 'DATA_DIR:WI*LZ*.DAT'
      ELSE
          INFILE	= FILENAM
      ENDIF

 1000 CONTINUE

      FILENAM	= INFILE

      WRITE( TTOUT, 100 )	'FILENAME = '
  100 FORMAT( 1H , A, $ )

      CALL TICHAR( FILENAM, *1000 )
C
C If user entered nothing, use copy contents of INFILE to FILENAM and use it
C If user entered a filename without a device or directory name, copy the
C device and directory name from INFILE, prefix it to user's response, and
C store back in FILENAM 
C
      IF( FILENAM .EQ. ' ') THEN
          FILENAM = INFILE
      ELSE

          KK	= MAX( INDEX( FILENAM, ':' ), INDEX( FILENAM, ']' ))
          LL	= MAX( INDEX( INFILE, ':' ), INDEX( INFILE, ']' ))

          IF( KK .EQ. 0 .AND. LL .NE. 0) THEN
              KK	= CHLAST( FILENAM )
              FILENA(1:LL)	= INFILE(1:LL)
              FILENA(LL+1:)	= FILENAM(1:KK)
              FILENAM	= FILENA
          ENDIF
      ENDIF

1100  CONTINUE
C
C Execute this code only if user specified a wildcard character in FILENAM
C If user made no selection (STATUS = 0) or if there is some error
C (STATUS <> 1) go back and prompt again
C
      IF( INDEX( FILENAM, '*' ) .NE. 0 .OR. INDEX( FILENAM, '%' ) .NE. 0) THEN
          PATTERN = FILENAM
          CALL SELECT_FILE( PATTERN, ' ', CONFIRM, FILENAM, KK, STATUS )
          IF (STATUS .EQ. 0) GOTO 1000
          IF (STATUS .NE. 1) THEN
              MESLEN = 0
              CALL LIB$SYS_GETMSG (STATUS, MESLEN, MESSAGE, , )
              IF( MESLEN .NE. 0) WRITE( TTOUT, *) MESSAGE(1:MESLEN)
              GOTO 1000
          ENDIF
      ENDIF

C Remove ; and version number from filename before opening
 
      MM	= INDEX( FILENAM, ';' )
      IF (MM .NE. 0) FILENAM(MM:)=' '

      INQUIRE ( FILE= FILENAM, EXIST= FILEEXIST )

      IF( FILEEXIST )   THEN

          INQUIRE ( FILE= FILENAM, NAME= FILENA )

          WRITE( TTOUT, 120 )   'EXPLICIT FILENAME = ', FILENA
  120 FORMAT( 1H , A, A )
          OK		= .TRUE.
          WRITE( TTOUT, 100 )   'IS THIS OKAY ?'
          CALL TILOG( OK, *1000 )

          IF( OK )   THEN
              FILENAM	= FILENA
              STATUS	= SS$_NORMAL
          ELSE
              GOTO 1000
          ENDIF

      ELSE

          WRITE( TTOUT, 120 )   'FILE DOES NOT EXIST: ', FILENAM

          OK		= .TRUE.
          WRITE( TTOUT, 100 )   'FILENAME OK ?'
          CALL TILOG( OK, *1000 )

          IF( .NOT.OK )    THEN
              OK		= .TRUE.
              WRITE( TTOUT, 100 )   'TRY AGAIN ?'
              CALL TILOG( OK, *1000 )
              IF( OK )    THEN
                  GOTO 1000
              ELSE
                  STATUS	= '18928'X
              ENDIF
          ELSE
              STATUS	= 0
          ENDIF

      ENDIF

      RETURN

      END
