      SUBROUTINE  TIM_STAMP
 
      INCLUDE	'INC_DIR:WIND_LUN.INC/NOLIST'
      INCLUDE	'INC_DIR:DIAG.INC/NOLIST'
 
      LOGICAL*4	A
 
      WRITE( TTOUT, 100) '+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+'
      WRITE( TTOUT, 100) '|                                 |'
      IF (.NOT. PRINTLZ_MODE) THEN
      WRITE( TTOUT, 100) '+          WIND_CD_READ           +'
      ELSE
      WRITE( TTOUT, 100) '+             PRINTLZ             +'
      ENDIF
      WRITE( TTOUT, 100) '|                                 |'
      WRITE( TTOUT, 100) '+          Version ' // PROG_VERSION // '          +'
      WRITE( TTOUT, 100) '|                                 |'
      WRITE( TTOUT, 100) '+             created             +'
      WRITE( TTOUT, 100) '|                                 |'
      WRITE( TTOUT, 100) '+     ' // LINK_DATE_TIME // '     +'
      WRITE( TTOUT, 100) '|                                 |'
      WRITE( TTOUT, 100) '+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+'
      WRITE( TTOUT, 120) 'IS THIS THE VERSION YOU WANT ?'
      A	= .TRUE.
      CALL TILOG( A, *999 )
      IF( .NOT. A )   GO TO 999
 100  FORMAT( 1H , T22, A35 )
 120  FORMAT( 1H , A, $ )
      RETURN
 
 999  STOP 999
      END
