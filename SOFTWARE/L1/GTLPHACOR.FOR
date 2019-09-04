      SUBROUTINE GTLPHACOR( FIRST )                          ! 25-AUG-93
C***********************************************************************
C  This routine will correct as far as possible the misplaced
C  or mislabeled PHA words. The cause for misplacement is the
C  DPU which occasionally receives the PHA interrupt after a
C  SECTOR interrupt which has a higher priority.  The criteria
C  to start correction is the number of PHAs in one sector is 
C  higher than the comparable basic rate.
C
C  This version:   23-AUG-93
C
C  28-JUL-93   H.Broer   programming basic comparision
C   9-AUG-93   H.Broer   added limitation of correction attempts
C  20-AUG-93   H.Broer   accelerated program by using character strings
C                        for moving big arrays
C  23-AUG-93   H.Broer   include GMQETSV.INC
C***********************************************************************
      IMPLICIT NONE
C
      INCLUDE   'INC_DIR:GMQETSV.INC'                 ! 211 LINES  23-AUG-93
      INCLUDE   'INC_DIR:STICS.INC'                   ! 190 LINES  12-JUL-93
                            ! INCLudes: 
                            !          'MAINSEC_L1.INC' 317 LINES  19-FEB-93
                            !                'HSKP.INC' 271 LINES  27-JAN-93
                            !           'G_S_CONST.INC'  38 LINES  19-APR-93
C                           !               TOTAL      1027 LINES
C
      COMMON /ORGPHA/   ORG_PHA,SINDEX, MAXINDEX,
     :                  OLD_PHA,OLDDEX, OINDEX, ERRCNT,
     :                  CORRCNT,CORR0CNT,       CORR00CNT,
     :                  OLDCORR,OLDCORR0,       OLDCOR00,       ILONG
C
      REAL*4    PHA0(0:7,3),    PHA1(0:7,3),    PHA2(0:7,3)  ! PHA*(SECTOR,TELESCOPE)
C
C                      index, rng, sect, nedb          rng, sect, nedb
      INTEGER*4 ORG_PHA(0:15, 0:2, 0:15, 0:31), SINDEX(0:2, 0:15, 0:31),
     :          OLD_PHA(0:15, 0:2, 0:15, 0:31), OLDDEX(0:2, 0:15, 0:31),
     :          MAXINDEX,       ACOUNT, COUNT,  DET,    ECH,    ERRCNT, EVNT,
     :          CORRCNT,        CORR0CNT,       CORR00CNT,
     :          II,     INDEX,  IRNGE,  ILONG(12288),   JJ,     JJJ,    JSEC,
     :          KK,     KKEND,  KTEL,   LL,     MEDB,   MM,     NEDB,   
     :          OINDEX, OLDCORR,        OLDCORR0,       OLDCOR00,
     :          RNG,    SECT,   SECT_WRD,       STRT,   STP,
     :          PHA_WORD,       TCH,    TDI4/0/
C
      LOGICAL*1         BUILT,  DONE,   FIRST,  FIRSTT/.TRUE./,
     :                  LPH0,   LPH1,   LPH2,   TDONE
C
      CHARACTER*22      EDBTIME
      CHARACTER*49152   CLONG,          CORG_PHA(2),    COLD_PHA(2)
      CHARACTER*6144    CSINDEX,        COLDDEX,        CSHORT
C
      EXTERNAL          TCONA
C
      EQUIVALENCE       (ORG_PHA, CORG_PHA),   (OLD_PHA, COLD_PHA),
     :                  (SINDEX,  CSINDEX),    (OLDDEX,  COLDDEX),
     :                  (TDI4,    TDONE),      (ILONG,   CLONG,     CSHORT)
C
      DATA              ILONG/12288*0/

C
C  INITIATION
C
      BUILT = .FALSE.
10000 CONTINUE
C
C  SET UP ORG_PHA array
C
      OINDEX   = MAXINDEX
      MAXINDEX = 0
      IF( FIRSTT )    THEN
          FIRSTT    = .FALSE.
          LPH0      = .FALSE.
          LPH1      = .FALSE.
          LPH2      = .FALSE.
          ERRCNT    = 0
          CORRCNT   = 0
          CORR0CNT  = 0
          CORR00CNT = 0
      END IF ! FIRSTT

      IF( FIRST )  THEN
          COLDDEX(1:6144) = CSHORT(1:6144)
      ELSE
          COLDDEX(1:6144) = CSINDEX(1:6144)
      END IF ! ( FIRST )

      CSINDEX(1:6144) = CSHORT(1:6144)

      IF( FIRST )  THEN
          COLD_PHA(1) = CLONG(1:49152)
          COLD_PHA(2) = CLONG(1:49152)
      ELSE
          COLD_PHA(1) = CORG_PHA(1)(1:49152)
          COLD_PHA(2) = CORG_PHA(2)(1:49152)
      END IF ! ( FIRST )

      CORG_PHA(1) = CLONG(1:49152)
      CORG_PHA(2) = CLONG(1:49152)

      CALL BUILD_ORG( BUILT )

C
C  START WITH SPIN = 0
C
      NEDB  = 0
10080 CONTINUE
      COUNT = 0
10100 CONTINUE
      TDONE = .FALSE.
10105 CONTINUE
      COUNT = COUNT + 1
      CALL TCONA( rec_pre.EDB_tm( NEDB ), EDBTIME )
      DO  II = 1, 3         ! TELESCOPES
          DO  JJ = 0, 7     ! SECTORS or QUASISECTORS
              PHA0( JJ, II ) = 0.        ! RANGE=0
              PHA1( JJ, II ) = 0.        ! RANGE=1
              PHA2( JJ, II ) = 0.        ! RANGE=2
          END DO ! JJ
      END DO ! II
C
      IF( S_PHA_NM1( NEDB ) .GE. 0 )   THEN
          DO  EVNT = 0, S_PHA_NM1( NEDB )
              call mvbits (PHA( EVNT, NEDB), 10, 2, DET, 0)	! ssd
              call mvbits (PHA( EVNT, NEDB), 12, 4, SECT, 0)	! sector
              call mvbits (PHA( EVNT, NEDB), 25, 5, STRT, 0)	! start and range
              RNG  = mod(STRT, 3)                               ! range
              STRT = STRT / 3                                   ! start
              call mvbits (PHA( EVNT, NEDB), 30, 2, STP, 0)	! stop
C
C  GET THE TELESCOPE NUMBER
C
              IF( STRT .LE. 6 )   THEN      ! THIS SETS KTEL = 1, 2,OR 3  (OR 0)
                  KTEL = ( STRT + 1 ) / 2
              ELSE 
                  KTEL = STRT - 6
              END IF ! ( STRT .LE. 6 )
C
C  COUNT PHA WORDS PER RANGE AND PSEUDO-SECTOR
C
              IF((KTEL.GE.1.AND.KTEL.LE.3) .AND. STP.EQ.KTEL)   THEN
                  GO TO ( 10110, 10120, 10130 ),  RNG + 1             ! RANGE
10110             PHA0( SECT / 2, KTEL ) = PHA0( SECT / 2, KTEL ) + 1
                  GO TO 10200
10120             PHA1( SECT / 2, KTEL ) = PHA1( SECT / 2, KTEL ) + 1
                  GO TO 10200
10130             PHA2( SECT / 2, KTEL ) = PHA2( SECT / 2, KTEL ) + 1
10200             CONTINUE
              END IF ! ((KTEL.GE.1.AND.KTEL.LE.3) .AND.
          END DO ! EVNT = 0, S_PHA_NM1( NEDB )



1010  FORMAT(1H , A, I4, ' SR=', I10, ' S_PHA_NM1(', I2, ')=', I3,
     :    ' CNT=', I2, ' TIME=', A, /,
     :             ' N:', A, 8F3.0, A, 8F5.0, /,
     :             ' E:', A, 8F3.0, A, 8F5.0, /,
     :             ' S:', A, 8F3.0, A, 8F5.0 )

C
C  ALL SET.   NOW CHECK THAT PHA* IS NOT GRAETER THAN BR*
C
          DO  II = 1, 3         ! TELESCOPES
              DO  JJ = 0, 7     ! SECTORS or QUASISECTORS
                  DO  IRNGE = 0, 2
                      GO TO (10400, 10500, 10600), IRNGE + 1          ! RANGE
10400 CONTINUE
1040  FORMAT( 1H  )
                      IF((PHA0(JJ, II) .GT. BR0(JJ, II, NEDB)) .OR. LPH0)   THEN
                          IF( .NOT.LPH0 )   WRITE( JPRNT, 1040 )
                          ACOUNT = 0
                          DO JJJ = 0, 7
                              ACOUNT=ACOUNT+PHA0(JJJ,1)+PHA0(JJJ,2)+PHA0(JJJ,3)
                          END DO
                          WRITE ( JPRNT, 1010 ) 'PHA0* PASS=', COUNT,
     :                          SCRcnt, NEDB, S_PHA_NM1(NEDB), ACOUNT, EDBTIME,
     :   ' PH0: ',(PHA0(JJJ,1), JJJ=0,7), ' BR0: ',( BR0(JJJ,1,NEDB),JJJ=0,7) ,
     :   ' PH0: ',(PHA0(JJJ,2), JJJ=0,7), ' BR0: ',( BR0(JJJ,2,NEDB),JJJ=0,7) ,
     :   ' PH0: ',(PHA0(JJJ,3), JJJ=0,7), ' BR0: ',( BR0(JJJ,3,NEDB),JJJ=0,7)
                          IF(PHA0(JJ, II) .GT. BR0(JJ, II, NEDB))   THEN
                              LPH0   = .TRUE.
                              GO TO 11000
                          ELSE
                              IF( TDI4 .EQ. 2 )   THEN
                                  NEDB  = NEDB + 1
                                  TDI4  = 0
                                  GO TO 10710
                              END IF ! TDI4 = 2
                              LPH0   = .FALSE.
                          END IF ! PHA0(JJ, II) .GT. BR0(JJ, II, NEDB)
                      END IF ! (PHA0(JJ, II) .GT. BR0(JJ, II, NEDB)) .OR. LPH0
                      GO TO 10700
10500 CONTINUE
                      IF((PHA1(JJ, II) .GT. BR1(JJ, II, NEDB)) .OR. LPH1)   THEN
                          IF( .NOT.LPH1 )   WRITE( JPRNT, 1040 )
                          ACOUNT = 0
                          DO JJJ = 0, 7
                              ACOUNT=ACOUNT+PHA1(JJJ,1)+PHA1(JJJ,2)+PHA1(JJJ,3)
                          END DO
                          WRITE ( JPRNT, 1010 ) 'PHA1* PASS=', COUNT,
     :                          SCRcnt, NEDB, S_PHA_NM1(NEDB), ACOUNT, EDBTIME,
     :   ' PH1: ',(PHA1(JJJ,1), JJJ=0,7), ' BR1: ',( BR1(JJJ,1,NEDB),JJJ=0,7) ,
     :   ' PH1: ',(PHA1(JJJ,2), JJJ=0,7), ' BR1: ',( BR1(JJJ,2,NEDB),JJJ=0,7) ,
     :   ' PH1: ',(PHA1(JJJ,3), JJJ=0,7), ' BR1: ',( BR1(JJJ,3,NEDB),JJJ=0,7)
                          IF(PHA1(JJ, II) .GT. BR1(JJ, II, NEDB))   THEN
                              LPH1   = .TRUE.
                              GO TO 11000
                          ELSE
                              IF( TDI4 .EQ. 2 )   THEN
                                  NEDB  = NEDB + 1
                                  TDI4  = 0
                                  GO TO 10710
                              END IF ! TDI4 = 2
                              LPH1   = .FALSE.
                          END IF ! PHA1(JJ, II) .GT. BR1(JJ, II, NEDB)
                      END IF ! (PHA1(JJ, II) .GT. BR1(JJ, II, NEDB)) .OR. LPH1
                      GO TO 10700

10600 CONTINUE
                      IF((PHA2(JJ, II) .GT. BR2(JJ, II, NEDB)) .OR. LPH2)   THEN
                          IF( .NOT.LPH2 )   WRITE( JPRNT, 1040 )
                          ACOUNT = 0
                          DO JJJ = 0, 7
                              ACOUNT=ACOUNT+PHA2(JJJ,1)+PHA2(JJJ,2)+PHA2(JJJ,3)
                          END DO
                          WRITE ( JPRNT, 1010 ) 'PHA2* PASS=', COUNT,
     :                          SCRcnt, NEDB, S_PHA_NM1(NEDB), ACOUNT, EDBTIME,
     :   ' PH2: ',(PHA2(JJJ,1), JJJ=0,7), ' BR2: ',( BR2(JJJ,1,NEDB),JJJ=0,7) ,
     :   ' PH2: ',(PHA2(JJJ,2), JJJ=0,7), ' BR2: ',( BR2(JJJ,2,NEDB),JJJ=0,7) ,
     :   ' PH2: ',(PHA2(JJJ,3), JJJ=0,7), ' BR2: ',( BR2(JJJ,3,NEDB),JJJ=0,7)
                          IF(PHA2(JJ, II) .GT. BR2(JJ, II, NEDB))   THEN
                              LPH2   = .TRUE.
                              GO TO 11000
                          ELSE
                              IF( TDI4 .EQ. 2 )   THEN
                                  NEDB  = NEDB + 1
                                  TDI4  = 0
                                  GO TO 10710
                              END IF ! TDI4 = 2
                              LPH2   = .FALSE.
                          END IF ! PHA2(JJ, II) .GT. BR2(JJ, II, NEDB)
                      END IF ! (PHA2(JJ, II) .GT. BR2(JJ, II, NEDB)) .OR. LPH2
10700 CONTINUE
                      IF( TDONE )   THEN
!                          WRITE( JPRNT, 1070 )  TDI4
!1070  FORMAT( ' TDI4=', Z8 )
                          TDI4  = TDI4 / 16
                          GO TO ( 10710, 10720, 10730 ), TDI4
10710 CONTINUE
                          CALL BUILD_PHA( .TRUE. )
                          GO TO 10100
10720 CONTINUE
                          CALL BUILD_PHA( .TRUE. )
                          NEDB  = NEDB - 1
                          GO TO 10105
10730 CONTINUE
                          CALL BUILD_PHA( .TRUE. )
                          GO TO 10100
                      END IF ! ( TDONE )
                  END DO ! IRNGE = 0,2 ! RANGES
              END DO ! JJ = 0, 7       ! SECTORS or QUASISECTORS
          END DO ! II = 1, 3           ! TELESCOPES
      END IF ! ( S_PHA_NM1( NEDB ) .GE. 0 )
10800 CONTINUE
      COUNT = 0
      NEDB  = NEDB + 1
      IF( MOD( NEDB, 32 ) .NE. 0 )     GO TO 10080
      IF( TDONE .OR. .NOT.FIRST )   CALL BUILD_PHA( FIRST )
      RETURN

11000 DONE  = .FALSE.                ! START CORRECTION OF PHA WORD AND POSITION
      DO  EVNT = 0, S_PHA_NM1( NEDB )
        IF( .NOT.DONE )   THEN
        call mvbits (PHA( EVNT, NEDB), 10, 2, DET, 0)  ! ssd
        call mvbits (PHA( EVNT, NEDB), 12, 4, SECT, 0) ! sector
        call mvbits (PHA( EVNT, NEDB), 25, 5, STRT, 0) ! start and range
        RNG  = mod(STRT, 3)                            ! range
        STRT = STRT / 3                                ! start
        call mvbits (PHA( EVNT, NEDB), 30, 2, STP, 0)  ! stop
!        WRITE( JPRNT, 1110)  EVNT, NEDB, PHA(EVNT,NEDB), SECT, RNG, STRT, STP
!1110  FORMAT(1H ,'PHA('I2,',',I2,')=',Z8,', SEC=',I2,', RNG=',I1,', STRT=',
!     :  I2, ', STP=', I1 )
        IF( IRNGE .EQ. RNG )   THEN
C***********************************************************************
          IF( ( SECT .EQ. (2*JJ) ) .AND. ( STP  .EQ. II) )   THEN
            PHA_WORD = PHA( EVNT, NEDB ) .AND. 'FFFF0FFF'X
            SECT_WRD = (SECT - 1) .AND. '0000000F'X
            PHA_WORD = PHA_WORD .OR. (SECT_WRD * '00001000'X)
            IF( ORG_PHA(0, IRNGE, SECT, NEDB) .EQ. PHA( EVNT, NEDB ) )   THEN
              DONE = .TRUE.
              IF( SECT .NE. 0 )   THEN
                CORRCNT   = CORRCNT + 1
                ORG_PHA(SINDEX(IRNGE,SECT_WRD,NEDB), IRNGE, SECT_WRD, NEDB) = PHA_WORD
                MAXINDEX  = MAX( MAXINDEX, SINDEX(IRNGE, SECT_WRD, NEDB) )
                SINDEX(IRNGE,SECT_WRD,NEDB) = SINDEX(IRNGE,SECT_WRD,NEDB) + 1
                TDONE     = '11'X
!                WRITE( JPRNT, 1120 ) PHA_WORD,SECT_WRD,SINDEX(IRNGE,SECT_WRD,NEDB),MAXINDEX
!1120  FORMAT( 1H , 'PHA_WORD=', Z8, ',  SECTOR=', I2,', INDEX=',I2,' MAX=',I2 )
              ELSE
                IF( NEDB .NE. 0 )   THEN
                  CORR0CNT  = CORR0CNT + 1
!                 TYPE *, ' **** NOT EDB ZERO SECTOR ZERO PHA word ****',CORR0CNT
!                    TYPE *, ' INDEX=', SINDEX(IRNGE,SECT_WRD,NEDB-1)
                ORG_PHA(SINDEX(IRNGE,SECT_WRD,NEDB-1),IRNGE,SECT_WRD,NEDB-1) = PHA_WORD
                MAXINDEX = MAX( MAXINDEX, SINDEX(IRNGE, SECT_WRD, NEDB-1) )
                SINDEX(IRNGE,SECT_WRD,NEDB-1) = SINDEX(IRNGE,SECT_WRD,NEDB-1) + 1
                TDONE     = '21'X
!                WRITE( JPRNT, 1120 ) PHA_WORD,SECT_WRD,SINDEX(IRNGE,SECT_WRD,NEDB-1),MAXINDEX
                ELSE
                  CORR00CNT = CORR00CNT + 1
!                  TYPE *,' ***** EDB ZERO SECTOR ZERO PHA word *****', CORR00CNT
                OLD_PHA( OLDDEX( IRNGE, SECT_WRD, 31), IRNGE, SECT_WRD, 31) = PHA_WORD
                OINDEX = MAX( OINDEX, OLDDEX(IRNGE, SECT_WRD, 31) )
                OLDDEX( IRNGE, SECT_WRD, 31) = OLDDEX( IRNGE, SECT_WRD, 31) + 1
                TDONE     = '41'X
!                WRITE( JPRNT, 1120 ) PHA_WORD,SECT_WRD,OLDDEX(IRNGE,SECT_WRD,31),OINDEX
                END IF ! ( NEDB .NE. 0 )
              END IF ! (SECT .NE. 0 )
              KKEND = SINDEX(IRNGE, SECT, NEDB) - 1
              SINDEX(IRNGE, SECT, NEDB) = KKEND
              DO KK = 0, KKEND
                ORG_PHA(KK, IRNGE, SECT, NEDB) = ORG_PHA(KK+1, IRNGE, SECT, NEDB)
              END DO ! KK
            END IF ! ( ORG_PHA(0, IRNGE, SECT, NEDB) .EQ. PHA( EVNT, NEDB ) )
          END IF ! ( ( SECT .EQ. (2*JJ) ) .AND. ( STP  .EQ. II) )
        END IF ! ( IRNGE .EQ. RNG )
        END IF ! .NOT.DONE
      END DO ! DO  EVNT = 0, S_PHA_NM1( NEDB )
      IF( .NOT.DONE )   THEN
        COUNT = COUNT + 1
        IF( COUNT .GT. 3 )   THEN
          ERRCNT  = ERRCNT + 1
          WRITE( JPRNT, 1100 )   EDBTIME, ERRCNT
1100  FORMAT( 1H ,'**** CAN NOT CORRECT PHA pos. ****,   ',A,'   ERRCNT=',I10 )
          GO TO 10800        ! NEXT EDB
        END IF ! ( COUNT .GT. 3 )
      END IF ! ( .NOT.DONE )  
      WRITE( JPRNT, 1130 )  'PAST', CORRCNT, CORR0CNT, CORR00CNT, ERRCNT
1130  FORMAT(1H ,A,' CORR.COUNTS: NOT SECT0=', I6,',  NOT EDB0=',I5,
     :             ',  EDB0-SECT0=', I4,',  ERRORS=', I4)
      GO TO 10700            ! CONTINUE
      END 
