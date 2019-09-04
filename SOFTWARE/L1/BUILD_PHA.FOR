      SUBROUTINE BUILD_PHA( FIRST )                          ! 17-AUG-93
C
C  BUILDS THE ARRAY OF PHA WORDS AS REPORTED BY TELEMETRY
C
      IMPLICIT NONE
C
      INCLUDE 'INC_DIR:STICS.INC'
C
      COMMON /ORGPHA/   ORG_PHA,SINDEX, MAXINDEX,
     :                  OLD_PHA,OLDDEX, OINDEX, ERRCNT,
     :                  CORRCNT,CORR0CNT,       CORR00CNT,
     :                  OLDCORR,OLDCORR0,       OLDCOR00
C
      REAL*4    ENG,    rtemp,  TOF
C
C                      index, rng, sect, nedb          rng, sect, nedb
      INTEGER*4 ORG_PHA(0:15, 0:2, 0:15, 0:31), SINDEX(0:2, 0:15, 0:31),
     :          OLD_PHA(0:15, 0:2, 0:15, 0:31), OLDDEX(0:2, 0:15, 0:31),
     :          MAXINDEX,       ERRCNT,  EVNT,
     :          CORRCNT,        CORR0CNT,       CORR00CNT,
     :          INDEX,  int_pha,IRANG,  IRNG,   NEDB,   NOMDEX,
     :          OINDEX, OLDCORR,OLDCORR0,       OLDCOR00,       RNG,    SECT
C
      LOGICAL*1         FIRST
C
      CHARACTER*22      EDBTIME
C
      EXTERNAL          TCONA
C
      NOMDEX = OINDEX
      IF( FIRST )   NOMDEX = MAXINDEX

      DO  NEDB = 0, 31
          EVNT   = -1
          DO  INDEX = 0, NOMDEX
              DO  RNG = 0, 2
                  DO  SECT = 0, 15
                      IF( FIRST )   THEN
                         IF(ORG_PHA(INDEX,RNG,SECT,NEDB).NE.0 .AND. EVNT.LT.46)   THEN
                              EVNT  = EVNT + 1
                              PHA( EVNT, NEDB ) = ORG_PHA( INDEX, RNG, SECT, NEDB )
                          END IF ! IF( ORG_PHA( INDEX,
                      ELSE
                         IF(OLD_PHA(INDEX,RNG,SECT,NEDB).NE.0 .AND. EVNT.LT.46)   THEN
                              EVNT  = EVNT + 1
                              PHA( EVNT, NEDB ) = OLD_PHA( INDEX, RNG, SECT, NEDB )
                          END IF ! IF( OLD_PHA( INDEX,
                      END IF ! FIRST
                  END DO ! SECT
              END DO ! RNG
          END DO ! INDEX
          S_PHA_NM1( NEDB ) = EVNT
          IF ( EVNT .LT. 46 ) THEN
              DO INDEX = EVNT + 1, 46
                  PHA( INDEX, NEDB ) = 0
              END DO ! INDEX
          END IF ! EVNT < 46 
      END DO ! NEDB

!-----------------------------------------------------------------------------
!     Unpack PHA
!-----------------------------------------------------------------------------
      DO  NEDB = 0, 31
          IF( S_PHA_NM1( NEDB ) .GE. 0 )   THEN
              DO  INDEX = 0, S_PHA_NM1( NEDB )
                  int_pha = PHA( INDEX, NEDB ) 
                  call mvbits (int_pha, 16, 9, S_PHA( 1, INDEX, NEDB), 0)	! eng
                  call mvbits (int_pha, 0, 10, S_PHA( 2, INDEX, NEDB), 0)	! tof
                  call mvbits (int_pha, 10, 2, S_PHA( 3, INDEX, NEDB), 0)	! ssd
                  call mvbits (int_pha, 12, 4, S_PHA( 4, INDEX, NEDB), 0)	! sector
                  call mvbits (int_pha, 25, 5, S_PHA( 6, INDEX, NEDB), 0)	! start and range
                  S_PHA( 5, INDEX, NEDB) = mod(S_PHA( 6, INDEX, NEDB), 3)	! range
                  S_PHA( 6, INDEX, NEDB) = S_PHA( 6, INDEX, NEDB) / 3		! start
                  call mvbits (int_pha, 30, 2, S_PHA( 7, INDEX, NEDB), 0)	! stop

                  TOF	= S_PHA( 2, INDEX, NEDB ) * TADC + TOC
                  ENG	= S_PHA( 1, INDEX, NEDB )
                  if ( ENG .ge. 256. )	then
                      if ( ENG .lt. 384. )	then
                          ENG	= 2. * ENG -  256.
                      else
                          ENG	= 4. * ENG - 1024.
                      end if
                  end if
                  ENG	= ENG * EADC + EOC

                  if ( TOF .gt. 0. )	then
                      rtemp	= TOF**2 * C1
                      MpQ( INDEX, NEDB )	= ( EpQ( NEDB ) - C2 ) * rtemp
                      if ( MpQ( INDEX, NEDB ) .ge. BRKPNT )	then
                          MpQ( INDEX, NEDB )	= ( EpQ( NEDB ) - C3 ) * rtemp
                      end if
                      if ( MpQ( INDEX, NEDB ) .gt. 99. )	MpQ( INDEX, NEDB ) = 99.

                      if ( ENG .gt. 0. )	then
                          if ( S_PHA( 3, INDEX, NEDB ) .ne. 0 )   then
                              MASS( INDEX, NEDB ) = exp(	A1
     +                                            + A2 * log( ENG )
     +                                            + A3 * log( TOF )
     +                                            + A4 * log( ENG ) * log( TOF )
     +                                            + A5 * log( ENG ) ** 2
     +                                            + A6 * log( TOF ) ** 3  )
                              if ( MASS( INDEX, NEDB ) .gt. 99. )	MASS( INDEX, NEDB ) = 99.
                          else
                              MASS( INDEX, NEDB ) = 0.5
                          endif
                      end if
                  end if
              END DO ! INDEX = 0, S_PHA_NM1
          END IF ! ( S_PHA_NM1( NEDB ) .GE. 0 )
      END DO ! NEDB = 0, 31

      RETURN

      END
