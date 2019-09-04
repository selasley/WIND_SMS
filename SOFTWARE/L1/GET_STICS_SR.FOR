      SUBROUTINE GET_STICS_SR( NEDB, FIRST )
C
C	This version:	25-AUG-93
C
C      READ a GEOTAIL EPI_L1_*.DAT data FILE, analyze the pulse height, and
C      correct PHA word and position if the number of PHA words in one sector
C      is graeter than the appropiate Base Rate number.
C
C      PHA words will be stored in a sequence as produced by the DPU.
C
C      Each PHA word will be also partioned to show the following values:
C
C	DVSTEP,	ECH, TCH, DET, SECT, RNG, STRT, STP, RMASS, RMPQ
C  FORMAT ( I3,  I5,  I5,  I3,   I3,  I2,   I3,  I3, F7.2,  F7.3 )
!
! WHERE:  item    [range] description
!       DVSTEP     [0,31] is the level of the DPPS
!          ECH   [0,1023] is the energy channel
!          TCH   [0,1023] is the time   channel
!          DET      [0,3] is the SSD identifier
!         SECT     [0,15] is the equatorial azimuthal sector, sector 0 has
!                         the deadtime and is not necessarily the sun sector
!          RNG      [0,2] is the basic rate identifier
!         STRT      [0,9] is the start MCP/anode identifier
!          STP      [0,3] is the stop MCP identifier
!        RMASS [0.5,.99.] is the mass calculated via the DPU algorithm
!         RMPQ [0.5,.99.] is the mass calculated via the DPU algorithm
!
!   AND   KTEL      [1,3] is the telescope, i.e., 
!
!                                   (single firings)      (double firings)   
!           TELESCOPE: KTEL  =      1        2       3 |  1      2      3
!                                   -        -       - |  -      -      -
!                      NONE  |  NORTH    EQUAT.  SOUTH |  N      E      S
!                         -  |    - -      - -     - - |  -      -      -
! STRT/startMCP [0,9]:    0  |    1 2      3 4     5 6 |  7=1&2  8=3&4  9=5&6
! STOP/rear MCP [0,3]:    0  |      1        2       3 | 
!    DET == SSD [0,3]:    0  |      1        2       3 | 
C	
C
C      13-AUG-93     H. BROER         First approach to PHA POSITION CORRECTION
C      23-AUG-93     H. BROER         Put COMMON /MATPRT/,/SUD/ into GMQETSV.INC
C
        IMPLICIT NONE
C
        INCLUDE 'INC_DIR:GMQETSV.INC'                   ! 211 LINES  23-AUG-93
        INCLUDE 'INC_DIR:STICS.INC'                     ! 190 LINES  12-JUL-93
                              ! INCLudes: 
                              !          'MAINSEC_L1.INC' 317 LINES  19-FEB-93
                              !                'HSKP.INC' 271 LINES  27-JAN-93
                              !           'G_S_CONST.INC'  38 LINES  19-APR-93
        INCLUDE '($SSDEF)'    !                           867 LINES = 1894 TOTAL

        INTEGER*4       FnameLen, JJ,       KK,
     :                  NEDB,     OLDSCR,   PHASELMOD
C
        BYTE            MEMREADOUT(0:31)
C
        LOGICAL*1       FIRST,    FIRSTR,   MEMDMP,   CORSET/.FALSE./
C
        CHARACTER       Fname*80
C
        EQUIVALENCE     ( MEMREADOUT(0),    THSKP32(0,12) )
C
10550   CONTINUE
        MEMDMP = .FALSE.
        IF( FIRST )   THEN
            JJ = 1
            IF( .NOT.CORSET )   THEN
                DO WHILE ( JJ .NE. 0 )
                    KK       = JJ
                    JJ       = INDEX( INFILE(KK:), ':')
                    IF( JJ .NE. 0 )    JJ = JJ + 1
                END DO ! WHILE ( JJ .NE. 0 )
                JJ       = INDEX( INFILE(KK:), '.') - 1
                IF( JJ .LT. 0 )    JJ = LEN( INFILE )
                Fname    = INFILE(KK:JJ)
                FnameLen = LEN( Fname )
                JJ       = INDEX( Fname, '_L' )
                IF( JJ .GT. 0  .AND.  JJ .LT. (FnameLen - 2) )   THEN
                    JJ = JJ + 2
                    KK = 0
                    DO WHILE ( Fname(JJ:JJ) .GE. '0'  .AND.  Fname(JJ:JJ) .LE. '9' )
                        KK = KK * 10 + (ICHAR(Fname(JJ:JJ)) - 48)
                        JJ = JJ + 1
                    END DO ! WHILE
                    JJ = file.hdr_pre.file_ver
                    KK = KK * 100 + JJ
                    TYPE*, ' CODE-ID IS', KK
                    IF( KK .LE. 103 )    THEN
                        CORPHA = .TRUE.
                        TYPE svFRMD,' DO YOU WANT PHA SECTOR CORRECTION ?'
                        CALL TI LOG ( CORPHA, *10550 )
                    END IF ! ( KK .LE. 103 )
                END IF ! ( JJ .GT. 0 ....
                CORSET   = .TRUE.
            END IF ! .NOT.SETCOR
            FIRST  = .FALSE.
            call EPIC_READ_L1 ( req_time, status )   ! read a Science Record
            FIRSTR = .TRUE.
            DO JJ = 0, 31
                IF( MEMREADOUT( JJ ) .EQ. 1 )   MEMDMP = .TRUE.
            END DO ! JJ
            IF( MEMDMP                                   !   EXCLUDE MEMORY DUMP
     :          .OR.(VLMD.EQ.'N'.AND.SVALMOD.EQ.6)       !   EXCLUDE CALIBRATE
     :          .OR.(VLMD.EQ.'C'.AND.SVALMOD.NE.6)) THEN
                NNSR   = NNSR   + 1
                GOTO 10550
            END IF ! MEMDMP

            IF( CORPHA )   THEN
                OLDSCR = SCRcnt
                call EPIC_MAKE_STICS ( status )           ! Buildup Eng.Rates in STICS_EDB, for 1 SCI REC
                call EPIC_MAKE_S_PHA ( PHASELMOD, status )! Buildup PHA words in STICS_EDB, for 1 SCI REC
                WRITE( JPRNT, 1500 ) SCRcnt, '1 ', (S_PHA_NM1( JJ ), JJ = 0, 31)
                call GTLPHACOR ( FIRSTR )
                WRITE( JPRNT, 1500 ) SCRcnt, '2 ', (S_PHA_NM1( JJ ), JJ = 0, 31)
            ELSE !   ( CORPHA )
                OLDSCR = SCRcnt - 1
                DATA_REC_pntr = DATA_REC_pntr - 1
            END IF ! ( CORPHA )
        END IF ! ( FIRST )

1500  FORMAT( 1H0, 'SR# =', I10, 2X, A3, 'S_PHA_NM1=', T32, 16I3, / T32, 16I3 )

        IF( CORPHA )   THEN
            MEMDMP = .FALSE.
            IF( file.hdr_rec(DATA_REC_pntr).tot_recs .ne. 0 )   THEN
                call EPIC_READ_L1 ( req_time, status )   ! read Science Record 2
                DO JJ = 0, 31
                    IF( MEMREADOUT( JJ ) .EQ. 1 )   MEMDMP = .TRUE.
                END DO ! JJ
                IF( MEMDMP                                   !   EXCLUDE MEMORY DUMP
     :              .OR.(VLMD.EQ.'N'.AND.SVALMOD.EQ.6)       !   EXCLUDE CALIBRATE
     :              .OR.(VLMD.EQ.'C'.AND.SVALMOD.NE.6)) THEN
                    FIRST  = .TRUE.
                    GOTO 10570
                END IF
            ELSE
                GO TO 10580
            END IF ! ( file.hdr_rec(DATA_REC_pntr).tot_recs .ne. 0 )

            IF( (OLDSCR+1) .NE. SCRcnt )   THEN
                TYPE *,' *** ERROR 2 ***  OLDSCR=',OLDSCR,'  SCR=',SCRcnt
                NNSR  = NNSR + SCRcnt - OLDSCR
                FIRST = .TRUE.
                GOTO 10570
            END IF
            FIRSTR = .FALSE.
            call EPIC_MAKE_STICS ( status )           ! Buildup Eng.Rates in STICS_EDB, for 1 SCI REC
            call EPIC_MAKE_S_PHA ( PHASELMOD, status )! Buildup PHA words in STICS_EDB, for 1 SCI REC
            WRITE( JPRNT, 1500 ) SCRcnt, '3 ', (S_PHA_NM1( JJ ), JJ = 0, 31)
            call GTLPHACOR ( FIRSTR )

10570   CONTINUE
            DATA_REC_pntr = DATA_REC_pntr - 2
            call EPIC_READ_L1 ( req_time, status )    ! read Science Record 1
            call EPIC_MAKE_STICS ( status )           ! Buildup Eng.Rates in STICS_EDB, for 1 SCI REC
            IF( OLDSCR .NE. SCRcnt )   THEN
                TYPE *,' *** ERROR 3 ***  OLDSCR=',OLDSCR,'  SCR=',SCRcnt
                FIRST = .TRUE.
                GOTO 10580
            END IF

10580   CONTINUE
            IF( .NOT.FIRST )     DATA_REC_pntr = DATA_REC_pntr + 1
            TYPE *, ' OLDSCR=', OLDSCR, '  SCR=', SCRcnt
            WRITE( JPRNT, 1500 ) SCRcnt, '5 ', (S_PHA_NM1( JJ ), JJ = 0, 31)
            OLDSCR  = OLDSCR + 1
        ELSE
            MEMDMP = .FALSE.
            IF( file.hdr_rec(DATA_REC_pntr).tot_recs .ne. 0 )   THEN
                call EPIC_READ_L1 ( req_time, status )   ! read Science Record 2
                DO JJ = 0, 31
                    IF( MEMREADOUT( JJ ) .EQ. 1 )   MEMDMP = .TRUE.
                END DO ! JJ
                IF( MEMDMP                                   !   EXCLUDE MEMORY DUMP
     :              .OR.(VLMD.EQ.'N'.AND.SVALMOD.EQ.6)       !   EXCLUDE CALIBRATE
     :              .OR.(VLMD.EQ.'C'.AND.SVALMOD.NE.6)) THEN
                    NNSR   = NNSR + 1
                    OLDSCR = OLDSCR + 1
                    FIRST  = .TRUE.
                    GOTO 10550
                END IF
            END IF ! ( file.hdr_rec(DATA_REC_pntr).tot_recs .ne. 0 )
            call EPIC_MAKE_STICS ( status )           ! Buildup Eng.Rates in STICS_EDB, for 1 SCI REC
            call EPIC_MAKE_S_PHA ( PHASELMOD, status )! Buildup PHA words in STICS_EDB, for 1 SCI REC
        END IF ! CORPHA
        SELMODE = PHASELMOD
        RETURN

        END
