      SUBROUTINE WIND_GET_FILENAME ( FILENAM, STATUS )
!+
!	SUBROUTINE WIND_GET_FILENAME
!	asks for a FILENAME and checks if this file exists.
!	STATUS	= 0	File does not exist
!		= 1	File exists
!
!	This version:	22-JUN-1994
!
!     REFERENCE:
!
!     USAGE and PARAMETERS:
!       CALL WIND_GET_FILENAME( FILENAM, STATUS )
!
!     CALLS/FUNCTIONS:
!       CALL TICHAR
!       CALL TILOG
!       = CHLAST
!       = INDEX
!       INQUIRE
!
!     CREATION:
!	18-FEB-1994	H. BROER	University of Maryland, Space Physics Group
!
!     REVISIONS:
!	22-JUN-1994	H. BROER	generalize for all files
!
!-
CDEC$	IDENT	'1.01'

      IMPLICIT NONE

      INCLUDE	'INC_DIR:WIND_Lun.INC/NOLIST'
      INCLUDE	'($SSDEF)/NOLIST'

      INTEGER*4		CHLAST,		! internal value
     :			KK,		! internal value
     :			LL,		! internal value
     :			MM,		! internal value
     :			STATUS		! Reporting execution status

      LOGICAL*1		FILEEXIST,	! internal logical flag
     :			OK		! internal logical flag

      CHARACTER*80	FILENAM,	! resulting filename
     :			FILENA,		! internal filename
     :			INFILE		! internal filename

      KK	= CHLAST( FILENAM )

      IF( KK .LT. 5 )   THEN
          INFILE	= 'DATA_DIR:93043003.DAT'
      ELSE
          INFILE	= FILENAM
      ENDIF

 1000 CONTINUE

      FILENAM	= INFILE

      WRITE( KB, 100 )	'FILENAME = '
  100 FORMAT( 1H , A, $ )

      CALL TICHAR( FILENAM, *1000 )

      KK	= INDEX( FILENAM(1:), ':' )

      LL	= INDEX( INFILE(1:), ':' )

      IF( KK .EQ. 0 .AND. LL .NE. 0)   THEN
          KK		= CHLAST( FILENAM )
          FILENA(1:LL)	= INFILE(1:LL)
          FILENA(LL+1:)	= FILENAM(1:KK)
          FILENAM	= FILENA
      ENDIF

C Remove ; and version number from filename before opening
 
      MM	= INDEX( FILENAM(1:), ';' )
      FILENAM(MM:)=' '

      INQUIRE ( FILE= FILENAM, EXIST= FILEEXIST )

      IF( FILEEXIST )   THEN

          INQUIRE ( FILE= FILENAM, NAME= FILENA )

          WRITE( KB, 120 )   'EXPLICIT FILENAME = ', FILENA
  120 FORMAT( 1H , A, A )
          OK		= .TRUE.
          WRITE( KB, 100 )   'IS THIS OKAY ?'
          CALL TILOG( OK, *1000 )

          IF( OK )   THEN
              FILENAM	= FILENA
              STATUS	= SS$_NORMAL
          ELSE
              GOTO 1000
          ENDIF

      ELSE

          WRITE( KB, 120 )   'NOT AN ESTABLISHED FILE : ', FILENAM

          OK		= .TRUE.
          WRITE( KB, 100 )   'FILENAME OK ?'
          CALL TILOG( OK, *1000 )

          IF( .NOT.OK )    THEN
              OK		= .TRUE.
              WRITE( KB, 100 )   'TRY AGAIN ?'
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
