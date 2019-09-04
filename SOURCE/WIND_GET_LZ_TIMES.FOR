      SUBROUTINE WIND_GET_LZ_TIMES (epoch_start_tm, epoch_stop_tm,
     :				req_start_time, req_end_time, status )
!+
!     This version:	28-APR-1994
!
!     EPIC_GET_LZ_TIMES
!       This subroutine prompts the user for a time interval (start & stop
!       times) within an LZ file.
!
!     REFERENCE:
!
!     USAGE and PARAMETERS:
!       call WIND_GET_LZ_TIMES (epoch_start_tm, epoch_stop_tm,
!    :				req_start_time, req_end_time, status )
!
!       epoch_start_tm	R*8	Input variable containing time of first record
!
!       epoch_stop_tm	R*8	Input variable containing time of last record
!
!       req_start_time	R*8	Output variable containing requested start time
!
!       end_end_time	R*8	Output variable containing requested end time
!
!       status		I*4	Output variable for the return status.
!
!
!     CALLS/FUNCTIONS:
!	CALL WIND_EPOCH_TO_GREG
!	CALL MODEPS	! deleted
!	CALL TIINT4
!	CALL TILOG
!	CALL WIND_GREG_TO_EPOCH
!
!     CREATION:
!        10-MAR-1994	H.BROER
!
!     REVISIONS:
!	  6-APR-1994	H.BROER	time conversion routines changed to WIND modules
!	 15-APR-1994	H.BROER	replace MODEPS
!	 28-APR-1994	H.BROER update correcting logic
!-
CDEC$ IDENT	'1.02'

      implicit none

      INCLUDE	'($SSDEF)/NOLIST'

      REAL*8
     :		epoch_start_tm,
     :		epoch_stop_tm,
     :		req_start_time,
     :		req_end_time

      INTEGER*4	YEAR,
     +		sYEAR,
     +		eYEAR,
     +		sLY,
     +		eLY,
     +		DOY,
     +          sDOY,
     +          eDOY,
     +          HOUR,
     +          MINUTE,
     +          SECOND,
     +          MILLIS,
     +		sMILLS,
     +		eMILLS,
     +          MICROS,
     +          status,
     +		KB/5/,
     +		ftime(4)

      equivalence (ftime(1), YEAR),
     +		  (ftime(2), DOY),
     +		  (ftime(3), MILLIS),
     +		  (ftime(4), MICROS)

      logical*1 OKAY

!-----------------------------------------------------------------------------
!     Format statements
!-----------------------------------------------------------------------------
   10 FORMAT ( 1H , A, $ )

   20 FORMAT	( 1H , A, /,
     :		' YEAR', T9, ':', I4, /,
     :		' DAY ', T9, ':', I4, /,
     :		' HOUR', T9, ':', I4, /,
     :		' MINUTE', T9, ':', I4, /,
     :		' SECOND', T9, ':', I4, /,
     :		' MILLIS', T9, ':', I4, /,
     :		' MICROS', T9, ':', I4 )

!-----------------------------------------------------------------------------
      status	= ss$_normal			! GOOD START
!-----------------------------------------------------------------------------
!     Get STARTING time request
!-----------------------------------------------------------------------------
  100 CALL WIND_EPOCH_TO_GREG( YEAR, DOY, MILLIS, MICROS, epoch_start_tm, status )
      sYEAR	= YEAR
  110 CONTINUE
      WRITE( KB, 10 )	' STARTING YEAR IS (NEG = ALL DATA ON FILE) '
      CALL TIINT4( sYEAR, *990 )

      IF( sYEAR .LT. 0 )	THEN
          req_start_time	= epoch_start_tm
          req_end_time		= epoch_stop_tm
          GOTO 990
      ENDIF
      sLY	= 0
      IF( MOD( sYEAR, 4 ) .EQ. 0 )   THEN
          IF( MOD( sYEAR, 100 ) .NE. 0 )   THEN
              sLY	= 1
          ELSE
              IF( MOD( sYEAR, 400 ) .EQ. 0 )	sLY = 1
          END IF
      END IF
      sDOY	= DOY
      SECOND	= MILLIS / 1000
      sMILLS	= MOD( MILLIS, 1000 )
      MINUTE	= SECOND / 60
      SECOND	= MOD( SECOND, 60 )
      HOUR	= MINUTE / 60
      MINUTE	= MOD( MINUTE, 60 )

  120 WRITE( KB, 10 )	' STARTING DAY IS '
      CALL TIINT4( sDOY, *110 )
      IF( sDOY .LE. 0 )			GOTO 120
      IF( (sDOY - sLY) .GT. 365 )	GOTO 120

  130 WRITE( KB, 10 )	' STARTING HOUR IS '
      CALL TIINT4( HOUR, *120 )
      IF( HOUR .LT. 0  .OR.  HOUR .GT. 23 )	GOTO 130

  140 WRITE( KB, 10 )	' STARTING MINUTE IS '
      CALL TIINT4( MINUTE, *130 )
      IF( MINUTE .LT. 0  .OR.  MINUTE .GT. 59 )	GOTO 140

  150 WRITE( KB, 10 )	' STARTING SECOND IS '
      CALL TIINT4( SECOND, *140 )
      IF( SECOND .LT. 0  .OR.  SECOND .GT. 59 )	GOTO 150

  160 WRITE( KB, 10 )	' STARTING MILLISECOND IS '
      CALL TIINT4( sMILLS, *150 )
      IF( sMILLS .LT. 0 .OR. sMILLS .GT. 999 )	GOTO 160

  170 WRITE( KB, 10 )	' STARTING MICROSECOND IS '
      CALL TIINT4( MICROS, *160 )
      IF( MICROS .LT. 0 .OR. MICROS .GT. 999 )	GOTO 170

  180 WRITE( KB, 20 )	' STARTING TIME CHOOSEN :',
     :			sYEAR, sDOY, HOUR, MINUTE, SECOND, sMILLS, MICROS

      OKAY	= .TRUE.
      WRITE( KB, 10 )	' OKAY'
      CALL TILOG( OKAY, *100 )
      IF( .not.OKAY )		GOTO 110
      MILLIS	= ( ( HOUR * 60 + MINUTE ) * 60 + SECOND ) * 1000 + sMILLS
      CALL WIND_GREG_TO_EPOCH( sYEAR, sDOY, MILLIS, MICROS, req_start_time, status )

!-----------------------------------------------------------------------------
!     Get stop time request
!-----------------------------------------------------------------------------
  200 CALL WIND_EPOCH_TO_GREG( YEAR, DOY, MILLIS, MICROS, epoch_stop_tm, status )
      eYEAR	= YEAR
  210 WRITE( KB, 10 )	' STOP YEAR IS  (NEG INPUT = ALL DATA TO END-OF-FILE)'
      CALL TIINT4( eYEAR, *180 )
      IF( eYEAR .LT. 0 )	THEN
          req_end_time		= epoch_stop_tm
          GOTO 990
      ENDIF

      eLY	= 0
      IF( MOD( eYEAR, 4 ) .EQ. 0 )   THEN
          IF( MOD( eYEAR, 100 ) .NE. 0 )   THEN
              eLY	= 1
          ELSE
              IF( MOD( eYEAR, 400 ) .EQ. 0 )	eLY = 1
          END IF
      END IF
      eDOY	= DOY
      SECOND	= MILLIS / 1000
      eMILLS	= MOD( MILLIS, 1000 )
      MINUTE	= SECOND / 60
      SECOND	= MOD( SECOND, 60 )
      HOUR	= MINUTE / 60
      MINUTE	= MOD( MINUTE, 60 )

  220 WRITE( KB, 10 )	' STOP DAY IS '
      CALL TIINT4( eDOY, *210 )
      IF( eDOY .LE. 0 )			GOTO 220
      IF( (eDOY - eLY) .GT. 365 )	GOTO 220

  230 WRITE( KB, 10 )	' STOP HOUR IS '
      CALL TIINT4( HOUR, *220 )
      IF( HOUR .LT. 0  .OR.  HOUR .GT. 23 )	GOTO 230

  240 WRITE( KB, 10 )	' STOP MINUTE IS '
      CALL TIINT4( MINUTE, *230 )
      IF( MINUTE .LT. 0  .OR.  MINUTE .GT. 59 )	GOTO 240

  250 WRITE( KB, 10 )	' STOP SECOND IS '
      CALL TIINT4( SECOND, *240 )
      IF( SECOND .LT. 0  .OR.  SECOND .GT. 59 )	GOTO 250

  260 WRITE( KB, 10 )	' STOP MILLISECOND IS '
      CALL TIINT4( eMILLS, *250 )
      IF( eMILLS .LT. 0 .OR. eMILLS .GT. 999 )	GOTO 260

  270 WRITE( KB, 10 )	' STOP MICROSECOND IS '
      CALL TIINT4( MICROS, *260 )
      IF( MICROS .LT. 0 .OR. MICROS .GT. 999 )	GOTO 270

      WRITE( KB, 20 )	' STOP TIME CHOOSEN :',
     :			eYEAR, eDOY, HOUR, MINUTE, SECOND, eMILLS, MICROS

      OKAY	= .TRUE.
      WRITE( KB, 10 )	' OKAY'
      CALL TILOG( OKAY, *200 )
      IF( .not.OKAY )		GOTO 210

      MILLIS	= ( ( HOUR * 60 + MINUTE ) * 60 + SECOND ) * 1000 + eMILLS
      CALL WIND_GREG_TO_EPOCH( eYEAR, eDOY, MILLIS, MICROS, req_end_time, status )

  990 CONTINUE
      RETURN
      END
