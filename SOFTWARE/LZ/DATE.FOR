ç       SUBROUTINE  TIM_STAMP
ç 
ç       COMMON /TIME_STAMP/  TIME_STAMP
ç 
ç       INCLUDE	'INC_DIR:WIND_LUN.INC/NOLIST'
ç 
ç       LOGICAL*4	A
ç 
ç       CHARACTER*23	TIME_STAMP
ç	DATA TIME_STAMP/' 8-MAR-1995 13:59:03.70'/
ç 
ç      WRITE( TTOUT, 100) '+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+'
ç      WRITE( TTOUT, 100) '|                                 |'
ç      WRITE( TTOUT, 100) '+          WIND_CD_READ           +'
ç      WRITE( TTOUT, 100) '|                                 |'
ç      WRITE( TTOUT, 100) '+             created             +'
ç      WRITE( TTOUT, 100) '|                                 |'
ç      WRITE( TTOUT, 100) '+      8-MAR-1995 13:59:03.70     +'
ç      WRITE( TTOUT, 100) '|                                 |'
ç      WRITE( TTOUT, 100) '+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+'
ç      WRITE( TTOUT, 120) 'IS THIS THE VERSION YOU WANT ?'
ç      A	= .TRUE.
ç      CALL TILOG( A, *999 )
ç      IF( .NOT.A )   GO TO 999
ç 100  FORMAT( 1H , T22, A35 )
ç 120  FORMAT( 1H , A, $ )
ç      RETURN
ç 
ç 999  STOP 999
ç      END
