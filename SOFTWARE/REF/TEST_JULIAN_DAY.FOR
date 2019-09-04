	Program TEST_JULIAN_DAY
!	This program calls conversion routines which produce the Julian day
!	from year, month, day, and hour, minute, second, millisecond, and
!	microsecond or reverse the operation.
!
!	Version: 28-Mar-1994
!
!	CALLS/USAGE:
!	JD	= CNVRT_YRMONDAY_TO_JULIAN( YEAR, MONTH, DAY )
!	FJUL	= CNVRT_TO_FLOAT_JULIAN( JD, HH, MI, SS, MS, US )
!	CALL JULIAN_TO_GREGORIAN( T, IYY, IMO, IDY, HH, MI, SS, MS, US )
!
      IMPLICIT	NONE
!
      REAL*8		CNVRT_TO_FLOAT_JULIAN,
     :			FJUL,
     :			T
      INTEGER*4		CNVRT_YRMONDAY_TO_JULIAN,
     :			YEAR,
     :			MONTH,
     :			DAY,
     :			IDY,
     :			IHH,
     :			IMO,
     :			IYY,
     :			JD,
     :			JD2,
     :			YY,
     :			MO,
     :			DD,
     :			HH,
     :			MI,
     :			SS,
     :			MS,
     :			US
  105 FORMAT( ' JULIAN DATE = ', F25.10, / )
  110 FORMAT( D25.3 )
  210 FORMAT(	' JULIAN DATE = ', F25.10,
     :	/,	'        YEAR = ', I10,
     :	/,	'       MONTH = ', I5,
     :	/,	'         DAY = ', I5,
     :	/,	'        HOUR = ', I5,
     :	/,	'      MINUTE = ', I5,
     :	/,	'      SECOND = ', I5,
     :	/,	' MILLISECOND = ', I5,
     :	/,	' MICROSECOND = ', I5, / )
  300 FORMAT( 1H , 'Please type in ', A, ', DEFAULT:[', I10, ' ] ', $ ) 
  310 FORMAT( 1H , 'Please type in ', A, ', DEFAULT:[', I3, '] "-1 = 0" ', $ ) 
  330 FORMAT( I10 )

      TYPE   *, ' PLEASE TYPE IN:  YEAR, MONTH, DAY'
      ACCEPT *, YEAR, MONTH, DAY
      TYPE   *, ' PLEASE TYPE IN:  HOURS, MINUTES, SECONDS'
      ACCEPT *, HH, MI, SS
      TYPE   *, ' PLEASE TYPE IN:  millisecs, microsecs'
      ACCEPT *, MS, US
   10 JD	= CNVRT_YRMONDAY_TO_JULIAN( YEAR, MONTH, DAY )
      FJUL	= CNVRT_TO_FLOAT_JULIAN( JD, HH, MI, SS, MS, US )
      TYPE 105, FJUL
!
!	Input JULIAN date
!
      TYPE   *, ' PLEASE TYPE IN:  JULIAN day'
      ACCEPT 110, T
      IF( T .EQ. 0D0 )	T	= FJUL
      CALL JULIAN_TO_GREGORIAN( T, IYY, IMO, IDY, HH, MI, SS, MS, US )
      TYPE 210, T, IYY, IMO, IDY, HH, MI, SS, MS, US

!	Start input Gregorian parameters
!
      TYPE 300, 'YEAR', IYY		! Input Year
      ACCEPT 330, YEAR
      IF( YEAR  .EQ. 0 )	THEN
          YEAR	= IYY
          MONTH	= IMO
          DAY	= IDY
          GOTO 10
      END IF	! ( YEAR  .EQ. 0 )
      TYPE 300, 'MONTH', IMO		! Input Months
      ACCEPT 330, MONTH
      IF( MONTH .EQ. 0 )	THEN
          MONTH	= IMO
          DAY	= IDY
          GOTO 10
      END IF	! ( MONTH .EQ. 0 )
      TYPE 300, 'DAY  ', IDY		! Input Days
      ACCEPT 330, DAY
      IF( DAY .EQ. 0 )	THEN
          DAY	= IDY
          GOTO 10
      END IF	! ( DAY .EQ. 0 )
      TYPE 310, 'HOUR', HH		! Input Hours
      ACCEPT 330, IHH
      IF( IHH .EQ. 0 )	GO TO 10
      IF( IHH .EQ. -1 )	THEN
          HH	= 0
      ELSE
          HH	= IHH
      END IF
      TYPE 310, 'MINUTES', MI		! Input Minutes
      ACCEPT 330, IHH
      IF( IHH .EQ. 0 )	GO TO 10
      IF( IHH .EQ. -1 )	THEN
          MI	= 0
      ELSE
          MI	= IHH
      END IF
      TYPE 310, 'SECONDS', SS		! Input Seconds
      ACCEPT 330, IHH
      IF( IHH .EQ. 0 )	GO TO 10
      IF( IHH .EQ. -1 )	THEN
          SS	= 0
      ELSE
          SS	= IHH
      END IF
      TYPE 310, 'MILLIS', MS		! Input Milliseconds
      ACCEPT 330, IHH
      IF( IHH .EQ. 0 )	GO TO 10
      IF( IHH .EQ. -1 )	THEN
          MS	= 0
      ELSE
          MS	= IHH
      END IF
      TYPE 310, 'MICROS', US		! Input Microseconds
      ACCEPT 330, IHH
      IF( IHH .EQ. 0 )	GO TO 10
      IF( IHH .EQ. -1 )	THEN
          US	= 0
      ELSE
          US	= IHH
      END IF
      GO TO 10
      END
