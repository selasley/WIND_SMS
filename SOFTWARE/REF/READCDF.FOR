      PROGRAM READCDF                            ! received  1-sep-93
C                                                ! VERSION  30-Nov-93
C  READ COMMON DATA RECORD ( CDR and CDF )
C
C  WRITTEN BY GARY BURGESS  1/8/92  GSFC  CODE 694
C  MODIFIED BY GARY BURGESS  10/15/92
C  ENHANCED H.BROER  9-SEP-93 using HEADER information for record length
C           H.BROER 15-SEP-93 selective data production
C           H.BROER 27-SEP-93 changed MINOR = 6 & 7 to LET & HET per
C                             mail from Gary Burgess, Goddard
C           H.BROER 22-OCT-93 changed OPEN statement to accommodate files
C                             produced from CD-ROM
C***********************************************************************
C
C  This file containes :                                PAGE
C             READCDF              MAIN PROGRAM            1
C             BIT8                 SUBROUTINE             51
C             BYTE2                SUBROUTINE             53
C             CDR_DATA             SUBROUTINE             25
C             CHCCSP               SUBROUTINE             70
C             CHLAST               FUNCTION               72
C             CHLCUC               SUBROUTINE             74
C             CHPRFX               FUNCTION               76
C             CONV                 SUBROUTINE             49
C             CONVERT_FILE         SUBROUTINE             17
C             I2SEDR               FUNCTION               33
C             I4SEDR               FUNCTION               35
C             JUL2DT               SUBROUTINE             44
C             R4SEDR               FUNCTION               37
C    ?        R8SEDR               FUNCTION               40
C             R8_UNSIGN_I4         FUNCTION               42
C             RTIME                FUNCTION               47
C             SFDU_HEADER          SUBROUTINE             19
C             TI                   SUBROUTINE             58
C             TIXENT               FUNCTION               65
C             VXDCL                SUBROUTINE             77
C             XYZ_TO_RTHETAPHI     SUBROUTINE             56
C             YRTO50A              SUBROUTINE             55
C
C  Input routines are contained in XTI_LIB.OLB

C****************************COMMANDPROCEDURE FOR COMPILATION*************
C$! File name  :  FORL.COM
C$! Purpose    :  Compile source code for TEST
C$!
C$! Version :	 29-JUL-93
C$!
C$ ON ERROR THEN GOTO EXIT
C$!
C$ SET DEF SRC_DIR
C$!
C$! Compile everything
C$ WRITE SYS$OUTPUT "Compiling"
C$ CALL FOR_COMPILE 'P1'
C$!
C$EXIT:
C$ SET DEF COM_DIR
C$ EXIT
C$
C$!========================================================================
C$!			FOR_COMPILE Subroutine
C$!
C$FOR_COMPILE: SUBROUTINE
C$!
C$ ON WARNING THEN EXIT
C$ IF P1.EQS."" THEN INQUIRE P1 FILE
C$ SET DEF OBJ_DIR
C$ WRITE SYS$OUTPUT "Compiling ",''P1'
C$ FOR SRC_DIR:'P1'.FOR -
C			/Warning=(GEN, DECLAR) -
C                        /LIST -
C			/NoOpt -
C			/I4 -
C			/NoD_Lines -
C			/CHECK=ALL/SHOW=ALL -
C			/Extend
C$ EXIT
C$ ENDSUBROUTINE		!End of FOR_COMPILE
C$!========================================================================

      IMPLICIT NONE

      COMMON/FILE/       FILENAME
      COMMON/CDR_TIME/   DYRDAY, YRDAY, IYR, IDAY, IHR, IMIN, ISEC, IMILLI, SEC
      COMMON/INSTRUMENT/ LFLAGS
C
      REAL*8       DSTPYRDAY,           ! STOP TIME     !
     :             DSTRYRDAY,           ! START TIME    ! IN SECOF50 NOTATION
     :             DYRDAY,              ! CURRENT TIME  !
     :             R8_UNSIGN_I4,        ! FUNCTION
     :             RTIME,               ! FUNCTION
     :             YRDAY,               ! CURRENT TIME
     :             T                    ! TEMPORARY TIME

      REAL*4       SEC                  ! INTERMEDIATE SECONDS

      INTEGER*4    CHLAST,              ! FUNCTION
     :             I2SEDR,              ! FUNCTION
     :             IBLOCK/0/,           ! BLOCK NUMBER
     :             ICDR(25),            ! RECORD BUFFER EQUIV. TO CCDR + CHCDR
     :             IDATA(4250),         ! ADJ.INPUT DATA EQUIV. TO IBYTE + CBYTE
     :             IDAY,                ! CURRENT DAY
     :             IFLAG,               ! DATA QUALITY FLAG, 0=GOOD
     :             IHR,                 ! CURRENT HOUR
     :             II,                  ! LOOP COUNTER
     :             IMILLI,              ! CURRENT MILLISECONDS
     :             IMIN,                ! CURRENT MINUTES
     :             IMON,                ! TEMP. MONTH
     :             INUM,                ! TEMP. INPUT
!     :             IPRT/7/,             ! DEBUGGING INFO TO FILE
     :             IREC,                ! last character position of read string
     :             ISEC,                ! CURRENT SECONDS
     :             ISTAT,               ! STATUS FOR OPENING DATAFILE
     :             ISTPDAY/1/,          ! STOP TIME DAY
     :             ISTPHR/23/,          ! STOP TIME HOUR
     :             ISTPMILLI/999/,      ! STOP TIME MILLISECONDS
     :             ISTPMIN/59/,         ! STOP TIME MINUTES
     :             ISTPSEC/59/,         ! STOP TIME SECONDS
     :             ISTPYR,              ! STOP TIME YEAR
     :             ISTRDAY,             ! START TIME DAY
     :             ISTRHR,              ! START TIME HOUR
     :             ISTRMIN,             ! START TIME MINUTES
     :             ISTRYR,              ! START TIME YEAR
     :             IYR,                 ! CURRENT YEAR
     :             JJ,                  ! LOOP COUNTER
     :             KB/5/,               ! KEYBOARD
     :             KK,                  ! LOOP COUNTER
     :             LAST,                ! LAST CHARACTER POINTER
     :             MINOR,               ! CLASS ID
     :             NCDR,                ! NUMBER OF BYTES IN BLOCK
     :             NCF,                 ! NUMBER OF CHARACTERS IN FILENAME
     :             NCHAR                ! NUMBER OF CHARACTERS IN KEYBOARD INPUT

      INTEGER*2    JDATA(50)            ! ADJ.INPUT DATA EQUIV. TO IBYTE + CBYTE

      BYTE         AAA(17000),          ! ORG.INPUT DATA EQIV. TO CAAA
     :             CCDR(100),           ! RECORD BUFFER EQUIV. TO ICDR + CHCDR
     :             IBYTE(17000)         ! ADJ.INPUT DATA EQUIV. TO IDATA + CBYTE

      LOGICAL*4    OKFLAG/.FALSE./

      LOGICAL*1    LFLAGS(0:11),
     :             ALLFLAG/.FALSE./,
     :             BAMFLAG/.FALSE./,   BAMFIRST/.TRUE./,
     :             GLGFLAG/.FALSE./,   GLGFIRST/.TRUE./,
     :             HEDFLAG/.FALSE./,   HEDFIRST/.TRUE./,
     :             HETFLAG/.FALSE./,   HETFIRST/.TRUE./,
     :             HUSFLAG/.FALSE./,   HUSFIRST/.TRUE./,
     :             KEPFLAG/.FALSE./,   KEPFIRST/.TRUE./,
     :             KETFLAG/.FALSE./,   KETFIRST/.TRUE./,
     :             LANFLAG/.FALSE./,   LANFIRST/.TRUE./,
     :             LETFLAG/.FALSE./,   LETFIRST/.TRUE./,
     :             STOFLAG/.FALSE./,   STOFIRST/.TRUE./
C
      CHARACTER*100 CHCDR
      CHARACTER*60 FILENAME, INFILE, INCHAR
      CHARACTER*20 F60,F61,F62,F63,F64,F65,F66,F67,F68,F69,F70,F75,F50
      CHARACTER*34 CLASS(0:9)
      CHARACTER    CAAA*17000, CBYTE*17000

      EQUIVALENCE  (ICDR,  CCDR,  CHCDR),
     :             (IDATA, IBYTE, CBYTE, JDATA),
     :             (AAA,       CAAA),
     :             (LFLAGS(0), BAMFLAG),
     :             (LFLAGS(1), GLGFLAG),
     :             (LFLAGS(2), HEDFLAG),
     :             (LFLAGS(3), HUSFLAG),
     :             (LFLAGS(4), KEPFLAG),
     :             (LFLAGS(5), LANFLAG),
     :             (LFLAGS(6), LETFLAG),
     :             (LFLAGS(7), HETFLAG),
     :             (LFLAGS(8), KETFLAG),
     :             (LFLAGS(9), STOFLAG),
     :             (LFLAGS(10),ALLFLAG)

      DATA CLASS  /'Solar Wind Plasma (BAM - SWOOPS)',
     1             'Solar Wind Ion Comp (GLG - SWICS)',
     2             'Magnetic Field (HED - VHM/FGM)',
     3             'Solar X-Ray/Gamma Ray (HUS)',
     4             'Energetic Particles (KEP - EPAC)',
     5             'Low Energy Ions (LAN - HISCALE)',
     6             'Cosmic Ray - 1 LET (SIM - COSPIN)',
     7             'Cosmic Ray - 2 HET (SIM - COSPIN)',
     8             'Cosmic Ray - 3 KET (SIM - COSPIN)',
     9             'Plasma Wave -'/

      FILENAME = 'DISK1:[ULYSSES.SCRATCH]CDF93152_B.CDF'                      ! GET INPUT FILENAME
1000  WRITE( KB, 100 )   'CDF FILENAME = '
  100 FORMAT(1H , A, $)
      INFILE   = FILENAME
      CALL TICHAR( INFILE, *9997 )
      FILENAME = INFILE
      NCF      = CHLAST( INFILE )
C
      OPEN( UNIT=12, NAME=INFILE, STATUS='OLD', ERR=1099,                     ! OPEN INPUT FILE
     :      READONLY, IOSTAT=ISTAT, BLOCKSIZE=48000, RECORDSIZE=12800 )
      GO TO 1100

1099  IF( ISTAT .EQ. 29 )  THEN
          WRITE( KB, 120 ) INFILE(1:NCF), ISTAT              ! ERROR OPENING INPUT FILE
  120 FORMAT( 1X, 'CANNOT FIND INFILE: ', A, ' STATUS IS', I4 )
      ELSE
          WRITE( KB, 140 ) INFILE(1:NCF), ISTAT
  140 FORMAT( 1X, 'CANNOT OPEN INFILE: ', A, ' STATUS IS ', I2 )
      END IF ! ISTAT = 29
      STOP
C****
1100  INQUIRE( 12, NAME=FILENAME )
      NCF    = CHLAST( FILENAME )
      WRITE( KB, 160 )   FILENAME(1:NCF), ISTAT
  160 FORMAT( 1H , 'OPENED FILE: ', A, ',  STATUS IS', I3 )
C
C   START SELECTION OF EXPERIMENT
C******************************************************************************
1120  DO JJ = 0, 11                                                           ! EXPERIMENT SELECTION
          LFLAGS(JJ) = .FALSE.
      END DO
      IREC  = 0                            ! INITIALIZE LAST CHARACTER POSITION
      WRITE( KB, 180 )   ( II, CLASS(II), II=0,9 )
  180 FORMAT( 1H , 'WHICH EXPERIMENT DATASET WOULD YOU LIKE TO CREATE ?',
     :       /, 2X, '-1   ALL EXPERIMENTS, except HED', /, ( 2X, I2, 3X, A) )
      WRITE( KB, 100 )   ' ENTER SINGLE VALUE OR MULTIPLE CHOICES AS 1,4,6: '
      CALL TICHAR( INCHAR, *1120 )
      NCHAR = CHLAST( INCHAR )
      IF( NCHAR .EQ. 0 )  GO TO 1120
      IF( INCHAR(NCHAR:NCHAR) .EQ. ',' )  GO TO 1140
      NCHAR = NCHAR + 1
      INCHAR(NCHAR:NCHAR) = ','
1140  JJ = 1
      DO WHILE ( JJ .LT. NCHAR )
          KK = INDEX( INCHAR(JJ:), ',' )
          IF(KK .EQ. 0)   GO TO 1200
          DECODE ( KK, '(I<KK>)', INCHAR(JJ:) ) INUM
          IF( INUM .EQ. -1 )   THEN
              ALLFLAG = .TRUE.
          ELSEIF( INUM .GE. 0  .AND.  INUM .LE. 9 )   THEN
              LFLAGS(INUM) = .TRUE.
          ELSE
              GO TO 1120
          END IF ! INUM
          JJ = JJ + KK
      END DO ! WHILE ( JJ .LT. NCHAR )

      IF( ALLFLAG )   THEN
          BAMFLAG = .TRUE.
          GLGFLAG = .TRUE.
          HETFLAG = .TRUE.
          HUSFLAG = .TRUE.
          KEPFLAG = .TRUE.
          KETFLAG = .TRUE.
          LANFLAG = .TRUE.
          LETFLAG = .TRUE.
          STOFLAG = .TRUE.
      END IF ! ALLFLAG

      WRITE( KB, 200 )
  200 FORMAT( 1H0, ' YOU HAVE CHOSEN :' )
      IF( ALLFLAG )   THEN
          WRITE( KB, 220 )  '  ALL EXPERIMENTS, except HED'
  220 FORMAT( 1H , A )
      ELSE
          DO JJ = 0, 9
              IF( LFLAGS(JJ) )   WRITE( KB, 240 ) JJ, CLASS(JJ)
  240 FORMAT( 1H , I3, 3X, A )
          END DO ! JJ
      END IF ! ALLFLAG

      OKFLAG = .TRUE.
      WRITE( KB, 100 )   'IS THIS OKAY ?'
      CALL TI LOG( OKFLAG, *1120 )
      IF( .NOT.OKFLAG )   GO TO 1120

      IF( HEDFLAG .AND. ( ALLFLAG.or.BAMFLAG.or.GLGFLAG.or.HUSFLAG.or.
     : KEPFLAG.or.LANFLAG.or.HETFLAG.or.LETFLAG.or.KETFLAG.or.STOFLAG) ) THEN
          WRITE( KB, 220 )  '**** YOU CANNOT COMBINE HED WITH ANY OTHER DATASET'
          GO TO 1120
      END IF ! HEDFLAG .AND.

      READ( 12, 400, END=9999, ERR=9998)   NCDR, (AAA(II), II=1,NCDR)    !  READ BLOCK OF DATA
      CBYTE( 1:NCDR ) = CAAA( 1:NCDR )
      IREC  = NCDR                            ! ADJUST LAST CHARACTER POSITION

      IF( HEDFLAG )   THEN
          KK   = INDEX( CAAA(1:), '.' )
          IF( KK .LE. 0  .OR.  KK .GT. 18 )   THEN
              WRITE( KB, 220 )   ' This file does not conform to the expected'
              WRITE( KB, 220 )   ' format.  Please start again.'
              CLOSE( 12 )
              GO TO 1000
          END IF ! KK <= 0
          MINOR = 2
          LAST  = 47

          CHCDR( 1:LAST ) = CBYTE( 1:LAST )      ! GET HEADER RECORD
          IF( IREC .GT. LAST )
     :      CBYTE( 1: ) = CBYTE( 1+LAST:IREC )   ! DISCARD RECORD FROM ADJ.INBUF
          IREC  = IREC - LAST                    ! ADJUST LAST CHARACTER POSITION

          DECODE( 39, 260, CHCDR(1:39) )   DSTRYRDAY, DSTPYRDAY, IMON
  260 FORMAT( F16.3, F17.3, I6 )
          WRITE( KB, 260 )  DSTRYRDAY, DSTPYRDAY, IMON

          T     = DSTPYRDAY / (36525D0 * 86400D0) + 0.5D0
          CALL JUL2DT( T, ISTPYR, IMON, IDAY, ISTPHR, ISTPMIN, ISTPSEC, ISTPMILLI )
          CALL CONV( ISTPYR, IMON, IDAY, ISTPDAY )
      ELSE
          DSTRYRDAY = R8_UNSIGN_I4( JDATA(6) )
          IMON      = I2SEDR( JDATA(8) )
          INUM      = DSTRYRDAY
          DSTRYRDAY = RTIME( INUM, IMON )
      END IF ! HEDFLAG

      T     = DSTRYRDAY / (36525D0 * 86400D0) + 0.5D0
      CALL JUL2DT( T, ISTRYR, IMON, IDAY, ISTRHR, ISTRMIN, ISEC, IMILLI )
      CALL CONV( ISTRYR, IMON, IDAY, ISTRDAY )

      IF( .NOT.HEDFLAG )   THEN
          ISTPYR    = ISTRYR
          ISTPDAY   = ISTRDAY
          ISTPHR    = ISTRHR + 1
          ISTPMIN   = ISTRMIN
      END IF ! .NOT.HEDFLAG

1200  CONTINUE                                                                ! SELECTION OF TIME PERIOD    START
      WRITE( KB, 100 )
     :    'SELECT START YEAR, A NEGATIVE NUMBER MEANS ALL DATA IN FILE! ?'
      INUM = ISTRYR
      CALL TI INT4( INUM, *1120 )
      ISTRYR = INUM
      IF( INUM .GT. 0 )   THEN
1220      WRITE( KB, 100 )   'SELECT START DAY ?'
          INUM    = ISTRDAY
          CALL TI INT4( INUM, *1200 )
          IF( INUM .LT. 1  .OR.  INUM .GE. 366 )   GO TO 1220
          ISTRDAY = INUM
1240      WRITE( KB, 100 )   'SELECT START HOUR OF DAY( 0 - 23 ) ?'
          INUM    = ISTRHR
          CALL TI INT4( INUM, *1220 )
          IF( INUM .LT. 0  .OR. INUM .GT. 23 )   GO TO 1240
          ISTRHR  = INUM
1260      WRITE( KB, 100 )   'SELECT STARTING MINUTE( 0 - 59 ) ?'
          INUM    = ISTRMIN
          CALL TI INT4( INUM, *1240 )
          IF( INUM .LT. 0  .OR. INUM .GT. 59 )   GO TO 1260
          ISTRMIN = INUM

          ISEC      = 0
          CALL YRTO50A( IMON, ISTRYR, ISTRDAY, ISTRHR, ISTRMIN, ISEC )
          DSTRYRDAY = IMON
      END IF ! INUM > 0
      T         = ISTRYR * 1000.D0
     :          + ISTRDAY
     :          + ISTRHR / 24.0D0
     :          + ISTRMIN / 1440.0D0

      WRITE( KB, 200 )
      WRITE( KB, 220 )   ' STARTING TIME ='
      WRITE( KB, 280 )   T, ISTRYR, ISTRDAY, ISTRHR, ISTRMIN
  280 FORMAT( T10, 'YEARDAY=', F14.5, '  YEAR=', I4, ' DAY=', I3,
     :             ' HRMIN=', 2I2.2 )

      OKFLAG = .TRUE.
      WRITE( KB, 100 )   'IS THIS OKAY ?'
      CALL TI LOG( OKFLAG, *1260 )
      IF( .NOT.OKFLAG )   GO TO 1200

      IF( ISTRYR .GE. 0 )   THEN
1300      CONTINUE                                                            ! SELECTION OF TIME PERIOD    STOP
          WRITE( KB, 100 )
     :    'SELECT STOP YEAR, A NEGATIVE NUMBER MEANS ALL DATA TO END OF FILE! ?'
          INUM    = ISTPYR
          CALL TI INT4( INUM, *1260 )
          ISTPYR  = INUM
          IF( INUM .GE. 0 )   THEN
1320          WRITE( KB, 100 )   'SELECT STOP DAY ?'
              INUM    = ISTPDAY
              CALL TI INT4( INUM, *1300 )
              IF( INUM .LT. 1  .OR.  INUM .GT. 366 )   GO TO 1320
              ISTPDAY = INUM
1340          WRITE( KB, 100 )   'SELECT STOP HOUR OF DAY( 0 - 23 ) ?'
              INUM    = ISTPHR
              CALL TI INT4( INUM, *1320 )
              IF( INUM .LT. 0  .OR. INUM .GT. 23 )   GO TO 1340
              ISTPHR  = INUM
1360          WRITE( KB, 100 )   'SELECT STOP MINUTE( 0 - 59 ) ?'
              INUM    = ISTPMIN
              CALL TI INT4( INUM, *1340 )
              IF( INUM .LT. 0  .OR. INUM .GT. 59 )   GO TO 1360
              ISTPMIN = INUM

              ISEC      = 1
              CALL YRTO50A( IMON, ISTPYR, ISTPDAY, ISTPHR, ISTPMIN, ISEC )
              DSTPYRDAY = IMON
          END IF ! INUM >= 0
          T         = ISTPYR * 1000.D0
     :              + ISTPDAY
     :              + ISTPHR / 24.0D0
     :              + ISTPMIN / 1440.0D0

          WRITE( KB, 200 )
          WRITE( KB, 220 )   ' STOP TIME ='
          WRITE( KB, 280 )   T, ISTPYR, ISTPDAY, ISTPHR, ISTPMIN

          OKFLAG = .TRUE.
          WRITE( KB, 100 )   'IS THIS OKAY ?'
          CALL TI LOG( OKFLAG, *1360 )
          IF( .NOT.OKFLAG )   GO TO 1300
      END IF ! ISTRYR >= 0

      IF( HEDFLAG )   GO TO 1440
      GO TO 1420

1400  CONTINUE
      READ(12, 400, END=9999, ERR=9998)   NCDR, (AAA(II),II=1,NCDR)           ! READ BLOCK DATA
  400 FORMAT( Q, 17000A1 )
C***
C*** DO NOT SKIP THE FIRST 8 BYTES OF EACH BLOCK
C***     4/19/93
      CBYTE(1+IREC:) = CAAA(1:NCDR)
      IREC  = IREC + NCDR                       ! ADJUST LAST CHARACTER POSITION
C****
1420  CONTINUE
C                                                                             !
1440  CONTINUE                                                                ! GET ONE RECORD
      IF( .NOT.HEDFLAG )   THEN
          IF( IREC .LT. 6 )   GO TO 1400
          LAST = IBYTE( 5 ) * 256 + IBYTE( 6 )
      END IF ! .NOT.HEDFLAG

      IF( IREC .LT. LAST )   GO TO 1400        ! CHECK FOR ONE RECORD IN ADJ.B.

      CHCDR( 1:LAST ) = CBYTE( 1:LAST )        ! GET NEXT RECORD
      IF( IREC-LAST .GT. 0 )
     :    CBYTE( 1: ) = CBYTE( 1+LAST:IREC )   ! DISCARD RECORD FROM ADJ.INBUF
      IREC  = IREC - LAST                      ! ADJUST LAST CHARACTER POSITION

C      WRITE(*,440) (ICDR(II),II=1,11)
C  440 FORMAT(1X,6Z10/1X,6Z10/)
      IF( .NOT.HEDFLAG )   THEN
          CALL SFDU_HEADER( ICDR, MINOR, IFLAG )
      ELSE
          IF( INDEX( CHCDR(1:16), '.' ) .LE. 0 )    GO TO 9999
          DECODE( 16, 260, CHCDR(1:16) )   DYRDAY
          T     = DYRDAY / (36525D0 * 86400D0) + 0.5D0
          CALL JUL2DT( T, IYR, IMON, IDAY, IHR, IMIN, ISEC, IMILLI )
          CALL CONV( IYR, IMON, IDAY, IDAY )
          SEC   = IMILLI / 1000.D0 + ISEC
          YRDAY = IYR*1.D3 + IDAY + IHR/24.D0 + IMIN/144.D1 + SEC/864.D2
      END IF

      IF( IFLAG .NE. 0 )   GO TO 1440

C***********************************************************************
      IF( ISTRYR .LT. 0  .OR.  ( DSTRYRDAY .LE. DYRDAY  .AND.
     :   (DYRDAY .LT. DSTPYRDAY .OR. ISTPYR .LT. 0) ) )   THEN
      IF( IBLOCK .EQ. 0 )   THEN
!          WRITE(*,460) MINOR,CLASS(MINOR),YRDAY,IYR,IDAY,IHR,IMIN
  460 FORMAT(1H , 'MINOR=', I2, 3X, A34, F15.5, I6, ':', I3.3, ':', 2I2.2)
C         CALL CONVERT_FILE(50,YRDAY,F50)
C         OPEN(UNIT=50,NAME=F50,TYPE='NEW')
C         WRITE(50,480) FILENAME
  480 FORMAT(1X,'CDF FILENAME = ',A40/)
C
          CALL CONVERT_FILE(75,YRDAY,F75)
          OPEN(UNIT=75,NAME=F75,TYPE='NEW')
          WRITE(75,480) FILENAME
      ENDIF

      IF( LFLAGS( MINOR ) )
     :    WRITE(75, 460) MINOR,CLASS(MINOR),YRDAY,IYR,IDAY,IHR,IMIN
      IBLOCK = IBLOCK + 1

      IF(MINOR .EQ. 0  .AND.  BAMFLAG)   THEN
          IF( BAMFIRST )   THEN
              BAMFIRST = .FALSE.

              CALL CONVERT_FILE(60,YRDAY,F60)
              OPEN(UNIT=60,NAME=F60,TYPE='NEW')
              WRITE(60,500) CLASS(0)
  500 FORMAT( T36, A40, /, T2, 'YY DOY HH MM  SS    DT       YRDAY',
     :        T44, 'V(x)', 6X, 'V(y)', 6X, 'V(z)', 6X, 'Vmag', 8X, 'N(sw)', 9X, 'Tkin', /,
     :        T44, 'km/s', 6X, 'km/s', 6X, 'km/s', 6X, 'km/s', 9X,  'cm-3', 8X, 'deg K')
          END IF ! BAMFIRST
          CALL CDR_BAM(ICDR(12))
      END IF ! MINOR == 0

      IF(MINOR .EQ. 1  .AND.  GLGFLAG)   THEN
          IF( GLGFIRST )   THEN
              GLGFIRST = .FALSE.

              CALL CONVERT_FILE(61,YRDAY,F61)
              OPEN(UNIT=61,NAME=F61,TYPE='NEW')
              WRITE(61,510) CLASS(1)
  510 FORMAT( T22, A40, /, T2, 'YY DOY HH MM  SS    DT       YRDAY',
     :        T47,'He DEN', 6X, 'He VEL', 8X, 'Z>2 FLUX',/,
     :        T48,  'cm-3', 8X,   'km/s', 9X, 'cm-2 s-1' )
          END IF ! GLGFIRST
          IF( LAST .EQ. 56 )   CALL CDR_GLG( ICDR(12) )
          IF( LAST .EQ. 64 )   CALL CDR_GLG( ICDR(14) )
      END IF ! MINOR == 1

      IF(MINOR .EQ. 2  .OR.  HEDFLAG)   THEN
          IF( HEDFIRST )   THEN
              HEDFIRST = .FALSE.

              CALL CONVERT_FILE(62,YRDAY,F62)
              OPEN(UNIT=62,NAME=F62,TYPE='NEW')
              WRITE(62,520) CLASS(2)
  520 FORMAT( T40, A40, /, T2, 'YY DOY HH MM  SS    DT       YRDAY',
     :        T46, 'Bx', 8X, 'By', 8X, 'Bz', 8X, 'B', 9X, 'theta',7X, 'phi', /,
     :        T46, 'nT', 8X, 'nT', 8X, 'nT', 8X, 'nT',9X,  'deg', 8X, 'deg' )
          END IF ! HEDFIRST
          CALL CDR_HED(CHCDR(17:47))
      END IF ! MINOR == 2

      IF(MINOR .EQ. 3  .AND.  HUSFLAG)   THEN
          IF( HUSFIRST )   THEN
              HUSFIRST = .FALSE.

              CALL CONVERT_FILE(63,YRDAY,F63)
              OPEN(UNIT=63,NAME=F63,TYPE='NEW')
              WRITE(63,530) CLASS(3)
  530 FORMAT( 26X, A40, /, T2, 'YY DOY HH MM  SS    DT       YRDAY',
     *        T42, 'F0', 8X, 'F1', 8X, 'F2', 8X, 'F3', 8X, 'F4', 8X, 'F5' )
          END IF ! HUSFIRST
          CALL CDR_HUS(ICDR(12))
      END IF ! MINOR == 3

      IF(MINOR .EQ. 4  .AND.  KEPFLAG)   THEN
          IF( KEPFIRST )   THEN
              KEPFIRST = .FALSE.

              CALL CONVERT_FILE(64,YRDAY,F64)
              OPEN(UNIT=64,NAME=F64,TYPE='NEW')
              WRITE(64,540) CLASS(4)
  540 FORMAT( T14, A40, /, T2, 'YY DOY HH MM  SS    DT       YRDAY',
     *        T48, 'OP FLUX', /, T44, '0.5-1.6 MeV', /, T44, 'cm-2s-1sr-1' )
          END IF ! KEPFIRST
          CALL CDR_KEP(ICDR(12))
      END IF ! MINOR == 4

      IF(MINOR .EQ. 5  .AND.  LANFLAG)   THEN
          IF( LANFIRST )   THEN
              LANFIRST = .FALSE.

              CALL CONVERT_FILE(65,YRDAY,F65)
              OPEN(UNIT=65,NAME=F65,TYPE='NEW')
              WRITE(65,550) CLASS(5)
  550 FORMAT( 45X, A40, /, T2, 'YY DOY HH MM  SS    DT       YRDAY',
     :       T48, 'P2''Ion',      9X,       'P5''Ion', 4X, 'E2 Electron',
     :       4X,   'E4 Electron',    7X, 'W3 Heavy', 7X, 'W5 Heavy', /,
     :       T44, '77-127 keV', 4X, '366-601 keV', 5X, '53-103 keV',
     :       4X, '175-315 keV',   ' .39-1.28 MeV/n',  ' .46-1.71 MeV/n', /,
     :       T41, 'cm-2/s/sr/MeV', 2X, 'cm-2/s/sr/MeV', 2x, 'cm-2/s/sr/MeV',
     :       2X,  'cm-2/s/sr/MeV', 2x, 'cm-2/s/sr/MeV', 2x, 'cm-2/s/sr/MeV' )
          END IF ! LANFIRST
          CALL CDR_LAN(ICDR(12))
      END IF ! MINOR == 5

      IF(MINOR .EQ. 6  .AND.  LETFLAG)   THEN
          IF( LETFIRST )   THEN
              LETFIRST = .FALSE.

              CALL CONVERT_FILE(66,YRDAY,F66)
              OPEN(UNIT=66,NAME=F66,TYPE='NEW')
              WRITE(66,560) CLASS(6)
  560 FORMAT( 18X, A40, /, T2, 'YY DOY HH MM  SS    DT       YRDAY',
     *        T45,  'L3+L12 Pr', 5X,  'L24+L25 He', 8X, 'L28 CNO', /,
     *        T43,'1.8-8 MeV/n', 3X,'4.0-19 MeV/n', 2X, '2.6-7.1 MeV/n', /,
     *        T51, 's-1', 12X, 's-1', 12X, 's-1' )
          END IF ! LETFIRST
          CALL CDR_SIM1(ICDR(12))
      END IF ! MINOR == 6

      IF(MINOR .EQ. 7  .AND.  HETFLAG)   THEN
          IF( HETFIRST )   THEN
              HETFIRST = .FALSE.

              CALL CONVERT_FILE(67,YRDAY,F67)
              OPEN(UNIT=67,NAME=F67,TYPE='NEW')
              WRITE(67,570) CLASS(7)
  570 FORMAT( 18X, A40, /, T2, 'YY DOY HH MM  SS    DT       YRDAY',
     *        T44, 'H2 Protons', 5X, 'H5 Protons', 3X, 'H8 Electrons', /,
     *        T45,  '14-19 MeV', 6X,  '68-92 MeV', 8X,      '3-5 MeV', /,
     *        T51, 's-1', 12X, 's-1', 12X, 's-1' )
          END IF ! HETFIRST
          CALL CDR_SIM2(ICDR(12))
      END IF ! MINOR == 7

      IF(MINOR .EQ. 8  .AND.  KETFLAG)   THEN
          IF( KETFIRST )   THEN
              KETFIRST = .FALSE.

              CALL CONVERT_FILE(68,YRDAY,F68)
              OPEN(UNIT=68,NAME=F68,TYPE='NEW')
              WRITE(68,580) CLASS(8)
  580 FORMAT( 10X, A40, /, T2, 'YY DOY HH MM  SS    DT       YRDAY',
     *        T42, 'E4 Electrons', 2X, 'E12 Electrons', /,
     *        T45,    '2.5-7 MeV', 6X,     '7-170 MeV', /,
     *        T51, 's-1', 12X, 's-1' )
          END IF ! KETFIRST
          CALL CDR_SIM3(ICDR(12))
      END IF ! MINOR == 8

      IF(MINOR .EQ. 9  .AND.  STOFLAG)   THEN
          IF( STOFIRST )   THEN
              STOFIRST = .FALSE.

              CALL CONVERT_FILE(69,YRDAY,F69)
              OPEN(UNIT=69,NAME=F69,TYPE='NEW')
              WRITE(69,590) CLASS(9)
  590 FORMAT( 27X, A14, '1   ELF (STO - URAP)', /,
     :        T2,'YY DOY HH MM  SS    DT       YRDAY',
     *        T45, 'AVE POWER', 5X, 'PEAK POWER',
     *         6X, 'AVE POWER', 5X, 'PEAK POWER',/,
     *        T44, '(10-100Hz)', 5X,  '(10-100Hz)',
     *        4X, '(100-500Hz)', 4X, '(100-500Hz)', /,
     *        T48, 'V^2m-2', 3( 9X, 'V^2m-2' ) )

              CALL CONVERT_FILE(70,YRDAY,F70)
              OPEN(UNIT=70,NAME=F70,TYPE='NEW')
              WRITE(70,600) CLASS(9)
  600 FORMAT( 42X, A14, '2   VLF (STO - URAP)', /,
     :        T2, 'YY DOY HH MM  SS    DT       YRDAY',
     *        T45, 'AVE POWER', 5X, 'PEAK POWER',
     *        6X,  'AVE POWER', 5X, 'PEAK POWER',
     *        6X,  'AVE POWER', 5X, 'PEAK POWER', /,
     *        T43, '(0.5-2 kHz)', 4X,  '(0.5-2 kHz)',
     *        6X,    '(2-8 kHz)', 6X,   '(2-8 kHz)',
     *        5X,   '(8-32 kHz)', 5X,  '(8-32 kHz)', /,
     *        T48, 'V^2m-2', 5( 9X, 'V^2m-2' ) )
          END IF ! STOFIRST
          CALL CDR_STO(ICDR(12))
      END IF ! MINOR == 9
      ELSE
          IF( ISTPYR .GT. 0  .AND.  DYRDAY .GE. DSTPYRDAY )   GO TO 9997
      END IF
      GO TO 1440

C****
9999  CONTINUE
      WRITE( KB, 999 )
  999 FORMAT(1X,'END OF FILE')
      GO TO 9990

9998  CONTINUE
      WRITE( KB, 998 )
  998 FORMAT(1X,'ERROR READING DATA BLOCK')
      GO TO 9990

9997  CONTINUE
      WRITE( KB, 997 )
  997 FORMAT( 1H , 'END OF TIME PERIOD REACHED' )

9990  CONTINUE
      WRITE( KB, 460 )   MINOR,CLASS(MINOR),YRDAY,IYR,IDAY,IHR,IMIN
      IF( BAMFLAG )   CLOSE( 60 )
      IF( GLGFLAG )   CLOSE( 61 )
      IF( HEDFLAG )   CLOSE( 62 )
      IF( HUSFLAG )   CLOSE( 63 )
      IF( KEPFLAG )   CLOSE( 64 )
      IF( LANFLAG )   CLOSE( 65 )
      IF( LETFLAG )   CLOSE( 66 )
      IF( HETFLAG )   CLOSE( 67 )
      IF( KETFLAG )   CLOSE( 68 )
      IF( STOFLAG )   THEN
          CLOSE( 69 )
          CLOSE( 70 )
      END IF
      CLOSE( 75 )
      CLOSE( 50 )
      END

      SUBROUTINE CONVERT_FILE(IUNIT,TIMEPC,F16)
C
      IMPLICIT NONE

      real*8       timepc

      INTEGER*4    II,
     :             IUNIT,
     :             JJ,
     :             KK,
     :             MM,
     :             NASCII,
     :             NTIME
      CHARACTER*20 F16
      CHARACTER*7  CDATE
c
C      timepc = 1990342.1234
      ntime = timepc
      DO II = 1,7
          KK = NTIME/10
          MM = NTIME - KK*10
          NASCII = MM+48
          JJ=7-(II-1)
          CDATE(JJ:JJ) = CHAR(NASCII)
          NTIME = KK
      ENDDO
      F16(1:7) = '       '
      IF(IUNIT.EQ.60) F16(1:7) = 'CDFBAM_'
      IF(IUNIT.EQ.61) F16(1:7) = 'CDFGLG_'
      IF(IUNIT.EQ.62) F16(1:7) = 'CDFHED_'
      IF(IUNIT.EQ.63) F16(1:7) = 'CDFHUS_'
      IF(IUNIT.EQ.64) F16(1:7) = 'CDFKEP_'
      IF(IUNIT.EQ.65) F16(1:7) = 'CDFLAN_'
      IF(IUNIT.EQ.66) F16(1:7) = 'CDFLET_'
      IF(IUNIT.EQ.67) F16(1:7) = 'CDFHET_'
      IF(IUNIT.EQ.68) F16(1:7) = 'CDFKET_'
      IF(IUNIT.EQ.69) F16(1:7) = 'CDFELF_'
      IF(IUNIT.EQ.70) F16(1:7) = 'CDFVLF_'
      F16(8:14) = CDATE
      F16(15:20) = '.DAT  '
C
      IF(IUNIT.EQ.75) THEN
          F16(1:9) = 'CDFTIMES_'
          F16(10:16) = CDATE
          F16(17:20) = '.DAT'
      ENDIF
      IF(IUNIT.EQ.50) THEN
          F16(1:9) = 'CDFSHEAD_'
          F16(10:16) = CDATE
          F16(17:20) = '.DAT'
      ENDIF
C      write(*,*) CDATE,F16
      RETURN
      END

      SUBROUTINE SFDU_HEADER(ICDR,MINOR,IFLAG)
C
c     The Routine has been written by Gary Burgess and will be used
c     to print the Ulysses CDR tape. 
C     Routines RTIME and JUL2DT have been supplied by R.V. Steiger
c     of the Univ of Bern. These are used in the computation of time.
c     Modifications to this program also made by F.W. Ottens.
C
      IMPLICIT NONE

      COMMON/CDR_TIME/ UTC_COARSE_TIME, YRDAY,
     :                 ISC_EVENT_YR,  ISC_EVENT_DAY, ISC_EVENT_HR,
     :                 ISC_EVENT_MIN, ISC_EVENT_SEC, ISC_EVENT_MILLI, SEC
      COMMON/FILE/     FILENAME
      COMMON/IDATA_FLAG/ WORD11_FLAG
      COMMON/INSTRUMENT/ LFLAGS
C
      REAL*8       R8_UNSIGN_I4,      ! FUNCTION
     :             RSCET0,
     :             RTIME,             ! FUNCTION
     :             UTC_COARSE_TIME,
     :             UTC_FINE_TIME,
     :             YRDAY
      REAL*4       DSCET_C_TIME,
     :             SEC
      INTEGER*4    IBIT_SUM,
     :             IBLOCK/0/,
     :             ICLOCK,
     :             IDATA_UNIT_CLASS,
     :             IDATA_UNIT_SPECI,
     :             IFLAG,
     :             II,
     :             IMAJOR_CLASS,
     :             IREC/0/,
     :             ISCET_FINE_TIME,
     :             ISC_EVENT_DAY,
     :             ISC_EVENT_DAY_OF_MONTH,
     :             ISC_EVENT_HR,
     :             ISC_EVENT_MILLI,
     :             ISC_EVENT_MIN,
     :             ISC_EVENT_MO,
     :             ISC_EVENT_SEC,
     :             ISC_EVENT_YR,
     :             ISTART_OF_MESS,
     :             IUNIT/50/,
     :             IUTC,
     :             IUTC_COARSE_TIME,
     :             IUTC_FINE_TIME,
     :             JJ,
     :             LENGTH_RECORD,
     :             MINOR,
     :             I2SEDR,            ! FUNCTION
     :             ICDR(25),
     :             ICDR_DATA(25)

      INTEGER*2    JSWAP,
     :             JCLOCK(2),
     :             JSEDR(50)

      CHARACTER*40 FILENAME
      CHARACTER*20 F50

      BYTE         BSWAP(2),
     :             IBITS(8),
     :             IDATA(100),
     :             WORD11_FLAG(0:15),
     :             WORD6_FLAG(8)

      LOGICAL*1    LFLAGS(0:11),
     :             BAMFLAG/.FALSE./,
     :             GLGFLAG/.FALSE./,
     :             HEDFLAG/.FALSE./,
     :             HETFLAG/.FALSE./,
     :             HUSFLAG/.FALSE./,
     :             KEPFLAG/.FALSE./,
     :             KETFLAG/.FALSE./,
     :             LANFLAG/.FALSE./,
     :             LETFLAG/.FALSE./,
     :             STOFLAG/.FALSE./

      EQUIVALENCE  (ICDR_DATA, IDATA,  JSEDR),
     :             (BSWAP,     JSWAP),
     :             (JCLOCK,    ICLOCK),
     :             (LFLAGS(0), BAMFLAG),
     :             (LFLAGS(1), GLGFLAG),
     :             (LFLAGS(2), HEDFLAG),
     :             (LFLAGS(3), HUSFLAG),
     :             (LFLAGS(4), KEPFLAG),
     :             (LFLAGS(5), LANFLAG),
     :             (LFLAGS(6), LETFLAG),
     :             (LFLAGS(7), HETFLAG),
     :             (LFLAGS(8), KETFLAG),
     :             (LFLAGS(9), STOFLAG)

C
C     PROCESS AND PRINT THE INFORMATION FOUND IN THE HEADERS
C
      DO II = 1,25
          ICDR_DATA(II) = ICDR(II)
      ENDDO
C
      IBLOCK = IBLOCK + 1
C
      IFLAG = 0
      CALL BIT8(IDATA(24),IBITS)
      IBIT_SUM = IBITS(5)+IBITS(6)+IBITS(7)+IBITS(8)
      DO II=1,8
          WORD6_FLAG(II) = IBITS(II)
      ENDDO
      IF(IBIT_SUM.GT.0) THEN
          IFLAG = 1
          RETURN
      ENDIF
C
      CALL BIT8(IDATA(41),IBITS)
      DO II=1,8
          WORD11_FLAG(II-1) = IBITS(II)
      ENDDO
      CALL BIT8(IDATA(42),IBITS)
      DO II=1,8
          WORD11_FLAG(II+7) = IBITS(II)
      ENDDO
C
      IDATA_UNIT_SPECI = IDATA(1)
      ISTART_OF_MESS   = IDATA(2)
      IDATA_UNIT_CLASS = I2SEDR(JSEDR(2))
      LENGTH_RECORD    = I2SEDR(JSEDR(3))
      IMAJOR_CLASS     = I2SEDR(JSEDR(5))
      MINOR            = IAND(IMAJOR_CLASS,255)
      UTC_COARSE_TIME  = R8_UNSIGN_I4(JSEDR(6))
      IUTC_FINE_TIME   = I2SEDR(JSEDR(8))
      UTC_FINE_TIME    = IUTC_FINE_TIME / 65536.0D0
      IUTC_COARSE_TIME = UTC_COARSE_TIME

C     OBTAIN THE SC_EVENT_TIME USING RTIME AND JUL2DT
C
      RSCET0=RTIME(IUTC_COARSE_TIME,IUTC_FINE_TIME)
      CALL JUL2DT( RSCET0 / 3.15576E9+5E-1,    ISC_EVENT_YR,
     :  ISC_EVENT_MO,  ISC_EVENT_DAY_OF_MONTH, ISC_EVENT_HR,
     :  ISC_EVENT_MIN, ISC_EVENT_SEC,          ISC_EVENT_MILLI)
C
C     CALL CONV TO CONVERT FROM MONTH, DAY OF MONTH TO DAY OF YEAR
C
      CALL CONV( ISC_EVENT_YR,           ISC_EVENT_MO,
     :           ISC_EVENT_DAY_OF_MONTH, ISC_EVENT_DAY )
      ISCET_FINE_TIME  = I2SEDR(JSEDR(19))
      DSCET_C_TIME     = R8_UNSIGN_I4(JSEDR(17))
C
      SEC   = ISC_EVENT_SEC+ISC_EVENT_MILLI/1000.
      YRDAY = ISC_EVENT_YR * 1000.0D0
     :      + ISC_EVENT_DAY
     :      + ISC_EVENT_HR / 24.0D0
     :      + ISC_EVENT_MIN / 1440.0D0
     :      + SEC/86400.0D0
 
      IF(IREC.EQ.0) THEN
          CALL CONVERT_FILE(50,YRDAY,F50)
          OPEN(UNIT=50,NAME=F50,TYPE='NEW')
          WRITE(IUNIT,70) FILENAME
  70  FORMAT(1X,'CDR FILENAME = ',A40)
      ENDIF
      IREC = IREC + 1
      IF (  (MINOR .EQ. 1  .AND.  GLGFLAG)
     : .OR. (MINOR .EQ. 9  .AND.  STOFLAG) )   THEN
          WRITE(IUNIT,500)  IBLOCK,         IDATA_UNIT_SPECI,
     *                      ISTART_OF_MESS, IDATA_UNIT_CLASS,
     *                      LENGTH_RECORD,
     *                      IDATA(7),      IDATA(8),    IDATA(24),
     *                      IMAJOR_CLASS,
     *                      ICDR(6),  ICDR(7),  ICDR(8),  ICDR(11)
  500 FORMAT(1X,'# RECORD = ',I5/
     *       1X,'DATA UNIT SPECIFICATION  = ',Z2,
     *       5X,'START OF MESSAGE POINTER = ',Z2/
     *       1X,'DATA UNIT CLASSIFICATION = ',Z4/
     *       1X,'MESSAGE DATA UNIT TOTAL LENGTH = ',Z4/
     *       1X,'ORIGINATOR ID. = ',Z2,
     *       5X,'LAST MODIFIER ID. = ',Z2,'   WORD 6(24-31) = ',Z2/
     *       1X,'MAJOR/MINOR DATA CLASS = ',Z4 /
     *       1X,'WORD 6,7,8 = ',3Z10,'  WORD 11 = ',Z8)
          WRITE(IUNIT,550) (WORD6_FLAG(JJ),JJ=1,8)
  550 FORMAT(1X,'WORD 6(24-31) BIT = ',8I2)
          WRITE(IUNIT,551) (WORD11_FLAG(II),II=0,7)
  551 FORMAT(1X,'WORD 11(0- 7) BIT = ',8I2)
          WRITE(IUNIT,552) (WORD11_FLAG(II),II=8,15)
  552 FORMAT(1X,'WORD 11(8_15) BIT = ',8I2)
          WRITE(IUNIT,501) UTC_COARSE_TIME,IUTC_FINE_TIME
  501 FORMAT(1X,'SCET (COARSE,FINE TIME) SECONDS SINCE'
     *       ' JAN. 1, 1950 = ',F12.1,I10)

          WRITE(IUNIT,508)  ISC_EVENT_YR,  ISC_EVENT_DAY, ISC_EVENT_HR,
     *                      ISC_EVENT_MIN, ISC_EVENT_SEC, ISC_EVENT_MILLI
  508 FORMAT(' HEADER SCET CONVERTED TO '/,
     *   ' YEAR,DAY,HOUR,MINUTES,SECONDS,MILLISECS = ',6I6)
          WRITE(IUNIT,503)  DSCET_C_TIME,  ISCET_FINE_TIME
  503 FORMAT(1X,'SPACECRAFT CLOCK COUNT - WHOLE & FRACTIONAL =', F12.0, I10)
          IUTC = UTC_COARSE_TIME
          IF(IUTC.GE.1360999000.AND.IUTC.LE.1360999500) THEN
              WRITE(40,140) IBLOCK
  140 FORMAT(1X,'IBLOCK = ',I6)
              WRITE(40,141) (ICDR(JJ),JJ=1,25)
  141 FORMAT(1X,10Z8/1X,10Z8/1X,5Z8)
          ENDIF
      ENDIF
C
      RETURN
      END

      SUBROUTINE CDR_DATA
C
      IMPLICIT NONE

      COMMON/IDATA_FLAG/ WORD11_FLAG

      COMMON/CDR_TIME/ DYRDAY, YRDAY, IYR, IDAY, IHR, IMIN, ISEC, IMILLI, SEC
C
      REAL*8      DYRDAY,             ! Current time in SECOF50 notation
     :            YRDAY,              ! Current time in YEARDAY.fraction
     :            Bx, By, Bz,         ! Cartesian components
     :            R,                  ! Radius
     :	          THETA,              ! latitude angle in degrees (+90 to -90)
     :	          PHI                 ! longitude angle in degrees (180 to -180)

      REAL*4      AREA(14),
     :            R4SEDR,             ! FUNCTION
     :            SEC,                ! Real sec including millisec
     :            DT( 0:9 )           ! Delta times for each instrument

      INTEGER*4   I4SEDR,             ! FUNCTION
     :            IAREA(14),
     :            IBAD,
     :            IDATA(*),
     :            IDAY,               ! CURRENT DAY OF YEAR
     :            IDTA,
     :            IDTA1,
     :            IDTA2,
     :            IFES,
     :            IHR,                ! CURRENT HOURS
     :            II,
     :            IKET,
     :            IMILLI,             ! CURRENT MILLISECONDS
     :            IMIN,               ! CURRENT MINUTES
     :            IQF,
     :            ISC,
     :            ISEC,               ! CURRENT SECONDS
     :            ISF,
     :            IYR                 ! CURRENT YEAR

      INTEGER*2   I2DTA(2),
     :            I2DTA2(4)

      BYTE        WORD11_FLAG(0:15)

      CHARACTER*30 CHCDR

      DATA  DT/ 1280., 780., 256., 0., 1024., 512., 1024., 1024., 1024., 256./
            !    BAM,  GLG,  HED,  ?,   KEP,  LAN,   LET,   HET,   KET,  STO

      EQUIVALENCE (IDTA,I2DTA),(IDTA1,I2DTA2(1)),(IDTA2,I2DTA2(3))

      ENTRY CDR_BAM(IDATA)                 ! BAM

      DO II = 1,5
          IDTA = IDATA(II)
          AREA(II) = R4SEDR(I2DTA)
      ENDDO
      AREA(6) = SQRT( AREA(1)**2 + AREA(2)**2 + AREA(3)**2 )

C
C  CHECK BAD DATA FLAG
C
      IF( (WORD11_FLAG(0).EQ.0) .OR. (WORD11_FLAG(7).EQ.1) )   THEN
          DO II=1,6
              AREA(II) = -1.e10
          ENDDO
      ENDIF
C
      WRITE( 60, 160)   MOD(IYR,100), IDAY, IHR, IMIN, SEC, DT(0), YRDAY,
     :           ( AREA(II), II = 1, 3 ), AREA(6), (AREA(II), II = 4, 5 )
  160 FORMAT( I3.2, I4.3, 2I3.2, F5.1, F6.0, F14.5, T39, 4F10.3, 2(1PG15.5) )
      RETURN



      ENTRY CDR_GLG(IDATA)                 ! GLG

      DO II = 1,3
          IDTA = IDATA(II)
          AREA(II) = R4SEDR(I2DTA)
      ENDDO
C
C  CHECK BAD DATA FLAG
C
      IF(WORD11_FLAG(0).EQ.0) THEN
          DO II=1,3
              AREA(II) = -1.e10
          ENDDO
      ENDIF
C
      IF(WORD11_FLAG(7).EQ.1) THEN
          IF(WORD11_FLAG(13).EQ.1) AREA(1) = -1.e10
          IF(WORD11_FLAG(14).EQ.1) AREA(2) = -1.e10
          IF(WORD11_FLAG(15).EQ.1) AREA(3) = -1.e10
      ENDIF
C
      WRITE( 61, 161)   MOD(IYR,100), IDAY, IHR, IMIN, SEC, DT(1), YRDAY,
     :           ( AREA(II), II = 1, 3 )
  161 FORMAT( I3.2, I4.3, 2I3.2, F5.1, F6.0, F14.5,
     :       T39, 1PE17.6, 0PF10.3, 1PE17.6)
!      WRITE(61,161) YRDAY,(AREA(II),II=1,3)
!  161 FORMAT(1X,F20.11,C      WRITE(61,361)IYR,IDAY,IHR,IMIN,ISEC,IMILLI,(AREA(II),II=1,3)
C  361 FORMAT(1X,6I6,1X,E15.6,F10.3,E15.6)
      RETURN

      ENTRY CDR_HED(CHCDR)                 ! HED

      DECODE( 30, 362, CHCDR(1:30) )    Bx, By, Bz

      CALL xyz_to_rthetaphi ( Bx, By, Bz, R, THETA, PHI)

      WRITE( 62, 162)   MOD(IYR,100), IDAY, IHR, IMIN, SEC, DT(2), YRDAY,
     :           Bx, By, Bz, R, THETA, PHI
  162 FORMAT( I3.2, I4.3, 2I3.2, F5.1, F6.0, F14.5,
     :       T39, 3F10.4, F10.4, F13.4, F11.4 )

!      WRITE( 62, 162 )   YRDAY, Bx, By, Bz, R, THETA, PHI

  362 FORMAT( 3F10.4, F10.4, 2F10.4, E17.7 )
!  162 FORMAT( F16.5, 3F10.4, F10.4, 2F10.4, E17.7 )

      RETURN


      ENTRY CDR_HUS(IDATA)                 ! HUS

      DO II = 1,6
          IDTA      = IDATA(II)
          IAREA(II) = I4SEDR(I2DTA)
      ENDDO
C
      IF(WORD11_FLAG(0).EQ.0) THEN
          DO II=1,6
              IAREA(II) = -9999
          ENDDO
      ENDIF
C
      WRITE(63,163) YRDAY,(IAREA(II),II=1,6)
  163 FORMAT(1X,F15.5,6I10)
      RETURN


      ENTRY CDR_KEP(IDATA)                 ! KEP

      IDTA    = IDATA(1)
      AREA(1) = R4SEDR(I2DTA)
C
C  CHECK BAD DATA FLAG
C
      IBAD = 0
      IF( WORD11_FLAG(0) .EQ. 0 )        IBAD = 1
C
      DO II=3,7
          IF( WORD11_FLAG(II) .EQ. 1 )   IBAD = 1
      ENDDO

      IF( IBAD .EQ. 1 )       AREA(1) = -1.e10
C
      WRITE( 64, 164)   MOD(IYR,100), IDAY, IHR, IMIN, SEC, DT(4), YRDAY,
     :            AREA(1)
  164 FORMAT( I3.2, I4.3, 2I3.2, F5.1, F6.0, F14.5, T39, 1PG20.6 )
!      WRITE(64,164) YRDAY,AREA(1)
!  164 FORMAT(1X,F15.5,G20.6)
      RETURN

      ENTRY CDR_LAN(IDATA)                 ! LAN

      DO II = 1,6
          IDTA     = IDATA(II)
          AREA(II) = R4SEDR(I2DTA)
      ENDDO
C
C  CHECK BAD DATA FLAG
C
      IF(WORD11_FLAG(0).EQ.0) THEN
          DO II=1,6
              AREA(II) = -1.e10
          ENDDO
      ENDIF
C
      IF(WORD11_FLAG(7).EQ.1) THEN
          IF(WORD11_FLAG(10).EQ.1) AREA(1) = -1.e10
          IF(WORD11_FLAG(11).EQ.1) AREA(2) = -1.e10
          IF(WORD11_FLAG(12).EQ.1) AREA(3) = -1.e10
          IF(WORD11_FLAG(13).EQ.1) AREA(4) = -1.e10
          IF(WORD11_FLAG(14).EQ.1) AREA(5) = -1.e10
          IF(WORD11_FLAG(15).EQ.1) AREA(6) = -1.e10
      ENDIF
C
      WRITE( 65, 165)   MOD(IYR,100), IDAY, IHR, IMIN, SEC, DT(5), YRDAY,
     :           ( AREA(II), II = 1, 6 )
  165 FORMAT( I3.2, I4.3, 2I3.2, F5.1, F6.0, F14.5, T39, 6(1PE15.5) )
!      WRITE(65,165) YRDAY,(AREA(II),II=1,6)
!  165 FORMAT(1X,F15.5,6E15.5)
      RETURN



      ENTRY CDR_SIM2(IDATA)                ! HET 

      DO II = 1,3
          IDTA     = IDATA(II)
          AREA(II) = R4SEDR(I2DTA)
      ENDDO
C
C  CHECK BAD DATA FLAG
C
      IF( (WORD11_FLAG(0).EQ.0) .OR. (WORD11_FLAG(7).EQ.1) )   THEN
          DO II=1,3
              AREA(II) = -1.e10
          ENDDO
      ENDIF
C
      WRITE( 67, 167)   MOD(IYR,100), IDAY, IHR, IMIN, SEC, DT(7), YRDAY,
     :           ( AREA(II), II = 1, 3 )
  167 FORMAT( I3.2, I4.3, 2I3.2, F5.1, F6.0, F14.5,
     :           T39, 3(1PG15.5) )
!      WRITE(67,167) YRDAY,(AREA(II),II=1,3)
!  167 FORMAT(1X,F15.5,3G15.5)
      RETURN

      ENTRY CDR_SIM1(IDATA)                ! LET

      DO II = 1,3
          IDTA     = IDATA(II)
          AREA(II) = R4SEDR(I2DTA)
      ENDDO
C
C  CHECK BAD DATA FLAG
C
      IF(WORD11_FLAG(0).EQ.0) THEN
          DO II=1,3
              AREA(II) = -1.e10
          ENDDO
      ELSE
          IF(WORD11_FLAG(7).EQ.1) THEN
              IF(WORD11_FLAG(13).EQ.1)   AREA(1) = -1.e10
              IF(WORD11_FLAG(14).EQ.1)   AREA(2) = -1.e10
              IF(WORD11_FLAG(15).EQ.1)   AREA(3) = -1.e10
          ENDIF
      ENDIF
C
      WRITE( 66, 166)   MOD(IYR,100), IDAY, IHR, IMIN, SEC, DT(6), YRDAY,
     :           ( AREA(II), II = 1, 3 )
  166 FORMAT( I3.2, I4.3, 2I3.2, F5.1, F6.0, F14.5,
     :           T39, 3(1PG15.5) )
!      WRITE(66,166) YRDAY,(AREA(II),II=1,3)
!  166 FORMAT(1X,F15.5,3G15.5)
      RETURN

      ENTRY CDR_SIM3(IDATA)                !  KET

      DO II = 1,2
         IDTA      = IDATA(II)
         AREA(II)  = R4SEDR(I2DTA)
      ENDDO
      DO II = 3,5
         IDTA      = IDATA(II)
         IAREA(II) = I4SEDR(I2DTA)
      ENDDO
C
C  CHECK BAD DATA FLAG
C
      IF( (WORD11_FLAG(0).EQ.0) .OR.
     :  ( (WORD11_FLAG(7).EQ.1) .AND. (WORD11_FLAG(13).EQ.1) ) )   THEN
          DO II=1,2
              AREA(II) = -1.e10
          ENDDO
      ENDIF
C
      CALL MVBITS(IAREA(4),16,16,ISC,0)
      CALL MVBITS(IAREA(5),20,12,IKET,0)
      CALL MVBITS(IAREA(5),8,8,IQF,0)
      CALL MVBITS(IAREA(5),0,8,ISF,0)
C      WRITE(68,168) YRDAY,(AREA(II),II=1,2),IAREA(3),ISC,IKET,IQF,ISF
C  168 FORMAT(1X,F15.5,2G15.5,2I10,I8,2I6)
      WRITE( 68, 168)   MOD(IYR,100), IDAY, IHR, IMIN, SEC, DT(8), YRDAY,
     :           ( AREA(II), II = 1, 2 )
  168 FORMAT( I3.2, I4.3, 2I3.2, F5.1, F6.0, F14.5,
     :           T39, 2(1PG15.5) )
!      WRITE(68,168) YRDAY,(AREA(II),II=1,2)
!  168 FORMAT(1X,F15.5,2G15.5)
      RETURN

      ENTRY CDR_STO(IDATA)                 ! STO

      DO II = 1,11
          IDTA      = IDATA(II)
          AREA(II)  = R4SEDR(I2DTA)
      ENDDO
      DO II = 12,14
          IDTA      = IDATA(II)
          IAREA(II) = I4SEDR(I2DTA)
      ENDDO
C
C  CHECK BAD DATA FLAG
C
      IF(WORD11_FLAG(0).EQ.0) THEN
          DO II=2,11
              AREA(II)  = -1.e10
          ENDDO
          DO II=12,14
              IAREA(II) = -9999
          ENDDO
      ENDIF
C
      IBAD = 0
      IF(WORD11_FLAG(1).EQ.1) IBAD = 1
      IF(WORD11_FLAG(2).EQ.2) IBAD = 1
      IF(IBAD.EQ.1) THEN
          DO II=12,14
              IAREA(II) = -9999
          ENDDO
      ENDIF
      DO II=4,7
          IF(WORD11_FLAG(II).EQ.1) AREA(II-2) = -1.e10
      ENDDO
      IF(WORD11_FLAG(8).EQ.1) THEN
          DO II =2,5
              AREA(II)  = -1.e10
          ENDDO
      ENDIF
      IF(WORD11_FLAG(9).EQ.1) THEN
          DO II=6,11
              AREA(II)  = -1.e10
          ENDDO
      ENDIF
C
      CALL MVBITS(IAREA(14),24,8,IFES,0)
C      WRITE(69,169) YRDAY,(AREA(II),II=1,11),(IAREA(II),II=12,13),IFES
C  169 FORMAT(1X,F15.5,11F8.3,3I8)
      WRITE( 69, 169)   MOD(IYR,100), IDAY, IHR, IMIN, SEC, DT(9), YRDAY,
     :           ( AREA(II), II = 2, 5 )
  169 FORMAT( I3.2, I4.3, 2I3.2, F5.1, F6.0, F14.5,
     :           T39, 4(1PG15.5) )
!      WRITE(69,169) YRDAY,(AREA(II),II=2,5)
!  169 FORMAT(1X,F15.5,4G15.5)
      WRITE( 70, 170)   MOD(IYR,100), IDAY, IHR, IMIN, SEC, DT(9), YRDAY,
     :           ( AREA(II), II = 6, 11 )
  170 FORMAT( I3.2, I4.3, 2I3.2, F5.1, F6.0, F14.5,
     :           T39, 6(1PG15.5) )
!      WRITE(70,170) YRDAY,(AREA(II),II=6,11)
!  170 FORMAT(1X,F15.5,6G15.5)

      RETURN
      END

      FUNCTION I2SEDR(IDATA)
C
      IMPLICIT NONE

      INTEGER*4   I2SEDR,
     :            KSWAP

      INTEGER*2   IDATA,
     :            ISWAP(2),
     :            JSWAP

      BYTE        BSWAP(2),
     :            ISAVE

      EQUIVALENCE (JSWAP,BSWAP),
     :            (ISWAP,KSWAP)
C
      JSWAP    = IDATA
      ISAVE    = BSWAP(1)
      BSWAP(1) = BSWAP(2)
      BSWAP(2) = ISAVE
      ISWAP(2) = 0
      ISWAP(1) = JSWAP
      I2SEDR   = KSWAP
      RETURN
      END

      FUNCTION I4SEDR(IDATA)
C
      IMPLICIT NONE

      INTEGER*4   I4SEDR,
     :            ICLOCK

      INTEGER*2   IDATA(2),
     :            JCLOCK(2),
     :            JSWAP

      BYTE        BSWAP(2),
     :            ISAVE

      EQUIVALENCE (JSWAP,BSWAP),
     :            (ICLOCK,JCLOCK)
C
      JSWAP     = IDATA(1)
      ISAVE     = BSWAP(1)
      BSWAP(1)  = BSWAP(2)
      BSWAP(2)  = ISAVE
      JCLOCK(2) = JSWAP

      JSWAP     = IDATA(2)
      ISAVE     = BSWAP(1)
      BSWAP(1)  = BSWAP(2)
      BSWAP(2)  = ISAVE
      JCLOCK(1) = JSWAP

      I4SEDR    = ICLOCK
C
      RETURN
      END

      FUNCTION R4SEDR(IDATA)
C
      IMPLICIT NONE

      REAL*16     RR,
     :            RTEMP

      REAL*4      R4SEDR,
     :            RDATA,
     :            SIGN

      INTEGER*4   CHAR,
     :            II,
     :            IREAL,
     :            JCHAR,
     :            JFRAC,
     :            KREAL,
     :            KFRAC(6)

      INTEGER*2   IDATA(2),
     :            JREAL(2),
     :            JSWAP

      BYTE        BCHAR(4),
     :            BREAL(4),
     :            BSWAP(2),
     :            ISAVE

      EQUIVALENCE (JSWAP,BSWAP),
     :            (RDATA,JREAL,IREAL),
     :            (CHAR,BCHAR),
     :            (KREAL,BREAL)

C
      JSWAP = IDATA(1)
      ISAVE = BSWAP(1)
      BSWAP(1) = BSWAP(2)
      BSWAP(2) = ISAVE
      JREAL(2) = JSWAP

      JSWAP = IDATA(2)
      ISAVE = BSWAP(1)
      BSWAP(1) = BSWAP(2)
      BSWAP(2) = ISAVE
      JREAL(1) = JSWAP

      CHAR = IREAL
      JCHAR = BCHAR(4)
      JFRAC = IREAL
      JCHAR = IAND(JCHAR,'7F'X)
      JFRAC = IAND(JFRAC,'FFFFFF'X)
      SIGN = 1.0
      IF(IREAL.LT.0) SIGN = -1.0
C
      KFRAC(1) = ISHFT(IAND(JFRAC,'F00000'X),-20)
      KFRAC(2) = ISHFT(IAND(JFRAC,'F0000'X),-16)
      KFRAC(3) = ISHFT(IAND(JFRAC,'F000'X),-12)
      KFRAC(4) = ISHFT(IAND(JFRAC,'F00'X),-8)
      KFRAC(5) = ISHFT(IAND(JFRAC,'F0'X),-4)
      KFRAC(6) = IAND(JFRAC,'F'X)
      RR = 0.Q0

      DO II = 1,6
          RTEMP = 16.Q0 ** ((JCHAR-64) - II)
!100   FORMAT( 1H , E14.7, I3, I4, I2, E15.7 )
!          WRITE( *, 100 )  RR, KFRAC(II), JCHAR, II, RTEMP
          RR    = RR + KFRAC(II) * RTEMP
      ENDDO

      IF( RR .LT. 0.29Q-38 )   THEN
          RR    = 0.Q0
      ELSE
          IF( RR .GT. 1.6Q38 )  RR = 1.6Q38
      END IF ! RR <      

      R4SEDR = SIGN * RR
C
      RETURN
      END

!      FUNCTION R8SEDR(IDATA)
!C
!      IMPLICIT NONE
!
!      REAL*16     RR,
!     :            RTEMP
!      REAL*8      R8SEDR
!      REAL*4      SIGN
!      INTEGER*4   CHAR,
!     :            II,
!     :            IREAL,
!     :            IREAL2,
!     :            JCHAR,
!     :            JFRAC,
!     :            KFRAC(14),
!     :            KREAL
!      INTEGER*2   IDATA(4),
!     :            JREAL(2),
!     :            JREAL2(2),
!     :            JSWAP
!      BYTE        BCHAR(4),
!     :            BREAL(4),
!     :            BSWAP(2),
!     :            ISAVE
!
!      EQUIVALENCE (CHAR,BCHAR),
!     :            (JREAL,IREAL),
!     :            (JREAL2,IREAL2),
!     :            (JSWAP,BSWAP),
!     :            (KREAL,BREAL)
!C
!      JSWAP = IDATA(1)
!      ISAVE = BSWAP(1)
!      BSWAP(1) = BSWAP(2)
!      BSWAP(2) = ISAVE
!      JREAL(2) = JSWAP
!
!      JSWAP = IDATA(2)
!      ISAVE = BSWAP(1)
!      BSWAP(1) = BSWAP(2)
!      BSWAP(2) = ISAVE
!      JREAL(1) = JSWAP
!C
!      JSWAP = IDATA(3)
!      ISAVE = BSWAP(1)
!      BSWAP(1) = BSWAP(2)
!      BSWAP(2) = ISAVE
!      JREAL2(2) = JSWAP
!
!      JSWAP = IDATA(4)
!      ISAVE = BSWAP(1)
!      BSWAP(1) = BSWAP(2)
!      BSWAP(2) = ISAVE
!      JREAL2(1) = JSWAP
!C
!      CHAR = IREAL
!      JCHAR = BCHAR(4)
!      JFRAC = IREAL
!      JCHAR = IAND(JCHAR,'7F'X) - 64
!      JFRAC = IAND(JFRAC,'FFFFFF'X)
!      SIGN = 1.0
!      IF(IREAL.LT.0) SIGN = -1.0
!C
!      KFRAC(1) = ISHFT(IAND(JFRAC,'F00000'X),-20)
!      KFRAC(2) = ISHFT(IAND(JFRAC,'F0000'X),-16)
!      KFRAC(3) = ISHFT(IAND(JFRAC,'F000'X),-12)
!      KFRAC(4) = ISHFT(IAND(JFRAC,'F00'X),-8)
!      KFRAC(5) = ISHFT(IAND(JFRAC,'F0'X),-4)
!      KFRAC(6) = IAND(JFRAC,'F'X)
!C
!      JFRAC = IREAL2
!      KFRAC(7)  = IAND(ISHFT(JFRAC,-28),'F'X)
!      KFRAC(8)  = ISHFT(IAND(JFRAC,'F000000'X),-24)
!      KFRAC(9)  = ISHFT(IAND(JFRAC,'F00000'X),-20)
!      KFRAC(10) = ISHFT(IAND(JFRAC,'F0000'X),-16)
!      KFRAC(11) = ISHFT(IAND(JFRAC,'F000'X),-12)
!      KFRAC(12) = ISHFT(IAND(JFRAC,'F00'X),-8)
!      KFRAC(13) = ISHFT(IAND(JFRAC,'F0'X),-4)
!      KFRAC(14) = IAND(JFRAC,'F'X)
!C
!      RR = 0.Q0
!      DO II = 1, 14
!          RTEMP = 16.Q0 ** (JCHAR - II)
!          RR = RR + KFRAC(II) * RTEMP
!      ENDDO
!      IF( RR .LT. 0.29Q-38 )   THEN
!          RR = 0.Q0
!      ELSE
!          IF( RR .GT. 1.6Q38 )  RR = 1.6Q38
!      END IF ! RR <      
!      R8SEDR = SIGN * RR
!C
!      RETURN
!
!      END

      FUNCTION R8_UNSIGN_I4(IDATA)
C
      IMPLICIT NONE

      REAL*8      R8_UNSIGN_I4,
     :            DD

      INTEGER*4   I4,
     :            ICLOCK

      INTEGER*2   IDATA(2),
     :            JCLOCK(2),
     :            JSWAP

      BYTE        BB(4),
     :            BSWAP(2),
     :            ISAVE

      EQUIVALENCE (JSWAP,BSWAP),
     :            (ICLOCK,JCLOCK,BB)
C
      JSWAP = IDATA(1)
      ISAVE = BSWAP(1)
      BSWAP(1) = BSWAP(2)
      BSWAP(2) = ISAVE
      JCLOCK(2) = JSWAP

      JSWAP = IDATA(2)
      ISAVE = BSWAP(1)
      BSWAP(1) = BSWAP(2)
      BSWAP(2) = ISAVE
      JCLOCK(1) = JSWAP

      I4 = ICLOCK
C
      JSWAP = 0
      BSWAP(1) = BB(4)
      DD = JSWAP*16777216.D0
      I4 = IAND( I4, 'FFFFFF'X )       ! I4 = IAND(I4,16777215)
      DD = DD + I4
      R8_UNSIGN_I4 = DD

      RETURN

      END

C************************************************************************

      SUBROUTINE JUL2DT( T, YY, MO, DD, HH, MI, SS, MS )

C************************************************************************

C***  subroutine to calculate the date and time from julian centuries
C***  since January 0.5, 1900. 
C***  For Ulysses, remember that T = SCET / (36525*86400) + 0.5
C***  written by r.v.steiger (using "astronomical formulae" by j.meeus)
C***  march 5, 1990
C***********************************************************************
C     PER MESSAGE FROM RUEDI OF 24 JUL 1991: ALL REAL CONSTANTS SHOULD
C     BE MADE DOUBLE PRECISION
C***********************************************************************

C***  input: time in julian centuries since january 0.5, 1900;
C***  julian day number
C***  fractional and integer part of jd+0.5
C***  output: year, month, day, hour, minute, second, milliseconds

C************************************************************************

C***  BEGINNING OF THE CODE:
C**   DECLARATION PART:

      IMPLICIT NONE

C***  input: time in julian centuries since january 0.5, 1900;
C***  julian day number
      REAL*8 T, JD
C***  fractional and integer part of jd+0.5
      INTEGER*4    Z
      REAL*8 F

C***  output: year, month, day, hour, minute, second, milliseconds
      INTEGER*4 YY, MO, DD, HH, MI, SS, MS

C     auxiliary variables
      INTEGER*4 ALPHA, A, B, C, D, E

C********  ALGORITHM ************************************

C     calculate julian day number
      JD = T * 36525D0 + 2415020D0
      Z = INT( JD + 5D-1 )
      F = JD + 5D-1 - Z 

C     correct for gregorian calendar after oct. 4, 1582
      IF ( Z .GE. 2299161 ) THEN
         ALPHA = INT( ( Z - 1867216.25D0 ) / 36524.25D0 )
         A = Z + 1 + ALPHA - INT( ALPHA / 4D0 )
      ELSE
         A = Z
      END IF
    
C     calculate auxiliary results
      B = A + 1524
      C = INT( ( B - 122.1D0 ) / 365.25D0 )
      D = INT( 365.25D0 * C )
      E = INT( ( B - D ) / 30.6001D0 )

C     find day, month, and year
      DD = B - D - INT( 30.6001D0 * E )
      IF ( E .LT. 13.5D0 ) THEN
         MO = E - 1
      ELSE
         MO = E - 13
      END IF
      IF ( MO .GT. 2.5D0 ) THEN
         YY = C - 4716
      ELSE
         YY = C - 4715
      END IF

C     find hour, minute, second, and millisecond
      HH = INT( 24 * F )
      MI = INT( 1440 * F ) - 60 * HH
      SS = INT( 86400 * F ) - 60 * MI - 3600 * HH
      MS = INT( 86400000 * F ) - 1000 * SS - 60000 * MI - 3600000 * HH

C***  END OF SUBROUTINE JUL2DT
      RETURN
      END

      DOUBLE PRECISION FUNCTION RTIME( ITIME, FTIME )                           
                                                                                
C     function to convert the 6-byte time format to floating point              
C     written by r.v.steiger                                                    
C     november 9, 1989                                                          
                                                                                
C------------------------- DECLARATION PART ----------------------------        
                                                                                
      IMPLICIT NONE

C     integer and fractional part of time, dummy counter                        
      INTEGER*4 ITIME, I, F, FTIME
C     ******************************
c     commented out by fwo 2/6/91
c      INTEGER*2 FTIME                                                           
c                                                                                
C     dummy to accumulate fractional part                                       
      REAL*8 R                                                                  
                                                                                
C--------------------------- STATEMENT PART ----------------------------        
                                                                                
      R = 0                                                                     
      F = FTIME                                                                 
      DO I = 0, 15                                                              
         IF ( BTEST( F, 15-I ) )                                                
     1      R = R + 2D0**(-I-1)                                                 
      END DO                                                                    
      RTIME = ITIME + R                                                         
                                                                                
      RETURN                                                                    
      END                                                                       

      subroutine conv(iyr,imo,iday,iday_yr)
c     subroutine to convert from year, month, day to year day of year.
c
C     17-SEP-93    H.BROER   enhanced to conform with the Gregorian calender
c
      IMPLICIT NONE

      INTEGER*4    IDAY,
     :             IDAY_YR,
     :             IMO,
     :             IMOL(12),
     :             IMONL(12),
     :             IYR

      LOGICAL*1    IYRL
C
      data imol/0,31,60,91,121,152,182,213,244,274,305,335/
      data imonl/0,31,59,90,120,151,181,212,243,273,304,334/
C
      IF( MOD( iyr, 4 ) .NE. 0 )   THEN
          IYRL = .FALSE.
      ELSE
          IF( MOD( iyr, 100 ) .NE. 0 )   THEN
              IYRL = .TRUE.
          ELSE
              IF( MOD( iyr, 400 ) .NE. 0 )   THEN
                  IYRL = .FALSE.
              ELSE
                  IYRL = .TRUE.
              END IF
          END IF
      END IF
      if( iyrl ) then
          iday_yr=imol(imo)+iday
      else
          iday_yr=imonl(imo)+iday
      end if
c      write(6,*)iyr,imo,iday,iday_yr

      return

      end

      SUBROUTINE BIT8(AREA,B)
C
C     THIS ROUTINE TAKES A BYTE PASSED TO IT AND CONVERTS IT INTO
C     EIGHT BYTES WHERE THE LEAST SIGNIFICANT BIT OF EACH BYTE IS ONE
C     OF THE BITS OF THE BYTE PASSED. THE REMAINING 7 BITS ARE ZERO.
C
C     FIRST ARGUMENT : AREA - INPUT ONLY ONE BYTE
C     SECOND ARGUMENT : B   - OUTPUT TO BE CONVERTED TO 8 BITS
C                             REQUIRE : CHARACTER*1 B(8)
C
C
C    SUBROUTINES CALLED: BYTE2
C    THIS ROUTINE CALLED BY: DPUDTA,PTMF,HOUSE,PME
C
C                       WRITTEN BY G. BURGESS AT GSFC  11/01/82
C                       MODIFIED BY G. BURGESS AT GSFC  11/01/82
C         FOR IMB PC  - MODIFIED BY G. BURGESS AT GSFC   5/11/88
C
      IMPLICIT NONE
C
      INTEGER*4    IA,
     :             IB,
     :             JJ
C
      INTEGER*2    INT
C
      BYTE         AREA,
     :             B(8),
     :             LL(2)

      EQUIVALENCE(INT,LL)
C
      CALL BYTE2(AREA,IA,IB)
C
      DO 10 JJ=1,4
C         B(5-JJ) = MOD(IA,2)
          INT     = MOD(IA,2)
          B(5-JJ) = LL(1)
          IA      = IA/2
   10 CONTINUE
C
      DO 20 JJ=1,4
C         B(9-JJ) = MOD(IB,2)
          INT     = MOD(IB,2)
          B(9-JJ) = LL(1)
          IB      = IB/2
   20 CONTINUE
C
      RETURN
      END

      SUBROUTINE BYTE2(LL,NA,NB)
C
C    CALL BYTES TO TAKE A BYTE PASSED TO IT AND TO BREAK IT UP INTO
C    TWO PARTS CONSISTING OF THE FIRST FOUR BITS AND THE LAST FOUR
C    FOUR BITS.
C
C    INPUT FIRST ARGUMENT:
C               LL    CHARACTER*1     A BYTE OF DATA
C    OUTPUT SECOND ARGUMENT AND THIRD ARGUMENT:
C               NA    I*4             FIRST FOUR BITS OF BYTE DATA
C               NB    I*4             LAST FOUR BITS OF BYTE DATA
C
C     SUBROUTINES CALLED: NONE
C     THIS ROUTINE CALLED BY: ISWIC,HKDPU,SWIC,PTMF,HOUSE,PME,DPUDTA
C
C                       WRITTEN BY G. BURGESS AT GSFC  3/4/82
C                       MODIFIED BY G. BURGESS AT GSFC  3/4/82
C          FOR IMB PC - MODIFIED BY G. BURGESS AT GSFC  5/11/88
C
      IMPLICIT NONE
C
      INTEGER*4    IA,
     :             IB,
     :             NA,
     :             NB

      BYTE         A(4),
     :             B(4),
     :             LL

      EQUIVALENCE  (IA,A(1)),
     :             (IB,B(1))
C
      IA   = 0
      IB   = 0
      A(1) = LL
      IA   = IA/16
C
      B(1) = LL
      IB   = IB - IA*16
C
      NA   = IA
      NB   = IB
      RETURN
      END

      SUBROUTINE YRTO50A(SECOF50,YR,DOY,HR,MN,SC)

*	a subroutine that is essentially the subroutine yrto50 	
*	written by christon.  The only change is noted below, and
*	was done by Chris Shafer on March 20, 1991

      IMPLICIT NONE

      INTEGER*4 SECOF50,YR,DOY,HR,MN,SC
      INTEGER*4 SEC_YR, NYR, LEAP
      DATA SEC_YR    /  31536000/ 	!  365*86400
        NYR = YR - 1950 		! changed from YR - 50

*			this change was done to make the yr variable
*			compatible with other subroutines written, i.e.
*			to make it a 4 digit number rather than 2 digits.

      LEAP    = (NYR+1)/4
      SECOF50 = NYR * SEC_YR 
     :        + (DOY-1 + LEAP) * 86400
     :        + HR * 3600 + MN * 60 + SC
      RETURN
      END

      Subroutine xyz_to_rthetaphi (x,y,z,r,theta,phi)
c
c   author:  d. hamilton (9/16/92)
c   version: 12/16/92

c  change history:
c	12/16/92  put in check for zero x**2 + y**2

c   purpose: to convert Cartesian components of a vector to spherical
c	     components

c   input:  x,y,z - Cartesian components (real*8)

c   output: r     - radius (real*8)
c	    theta - latitude angle in degrees (-90 to +90)(real*8)
c	    phi   - longitude angle in degrees (0 to 360) (real*8)

c ******************************************************************************
c
      IMPLICIT NONE
c
      REAL*8   x,y,z,         ! Cartesian components
     :         r,             ! radius
     :         theta,         ! latitude angle in degrees (-90 to +90)
     :         phi,           ! longitude angle in degrees (0 to 360)
     :         rootx2y2

	r = dsqrt(x*x + y*y + z*z)

	rootx2y2 = dsqrt(x*x + y*y)

	if (rootx2y2 .eq. 0.d0) then

		if ( z .ge. 0.d0 )    theta =  90.0d0
		if ( z .lt. 0.d0 )    theta = -90.0d0
		phi   = 0.0d0
		return

	else

		theta = datan2d(z,rootx2y2)

	end if

	if ( x .eq. 0.d0 ) then
		if ( y .ge. 0.d0 )	phi =  90.d0
		if ( y .lt. 0.d0 )	phi = -90.d0
	else
		if (y .gt. 0) then
	
			phi   =   dacosd(x/rootx2y2)
		else
			phi   = - dacosd(x/rootx2y2)

		end if
	end if

	return
	end

      SUBROUTINE TI
c||=====================================| TI.inc
c|| terminal input common

	Character*(*)	tiqPID
	Parameter	(tiqPID	='TI/1.2')

c| functions
c  ---------
	Integer*4	TIX ENT		! classify user's entry fcn
	Logical		TI USER		! user fcn
	Integer		ch last		! last nonspace fcn
	Integer		ch prfx		! test prefix fcn

c| TIX ENT outcomes
c  --- --- --------
	Integer		ticENR,	ticEBL,	ticERP,	ticETI, jent

	Parameter	(ticENR	=0)	! entry not recognized
	Parameter	(ticEBL	=1)	! entry is blank line
	Parameter	(ticERP	=2)	! entry is return to previous question
	Parameter	(ticETI	=3)	! entry is TI command

	Character*80	ticENT		! line entered by user

	Common		/ticomc/	ticENT

c| helpfile
c  --------
	Logical		ticHFO		! helpfile currently open
	Logical		ticHFT		! if helpfile has been tested

	Common		/ticomn/	ticHFO,	ticHFT

c| learn mode
c  ----- ----
	Integer		ticLMU		! file unit
	Integer		ticLMS		! state
					!  0 => no file open
					!  1 => writing
					!  2 => reading
	Logical		ticLME		! if TTY input to be echoed
	Logical		ticHPL		! if printer is HP LaserJet

	Common		/ticomn/	ticLMU,	ticLMS,	ticLME,	ticHPL

	Character*80	ticLMF		! filespec
	Character*20	ticPID		! calling pgm's id string
	Character*4	ticHU1		! HP begin underline seqence
	Character*4	ticHU2		! HP end   "         "

	common		/ticomc/	ticLMF, ticPID,	ticHU1,	ticHU2

C **** Variable for TI CHAR ****
	Character*(*)	CHAR            ! TI CHAR entry

C **** Variables for TI INT4 ****
	Integer*4       INT, itemp, JJ  ! TI INT4 entry
	Character*5     ifrmt		! to encode E format

C **** Variable for TI LOG ****
	Logical		LOG             ! TI LOG

C **** Variable for TIX LRN ****
	Character*1	cmd
	Equivalence	(cmd, ticENT (2:2))

	Character*3	status
	Integer		state






	ENTRY TI CHAR (CHAR, *)
c||-------------------------------------| TI CHAR
c|| accept character
  100	Type 101, CHAR (1: ch last (CHAR))
  101	Format ('+/',a,'/ ',$)

	jent	=TIX ENT ()		! accept and classify line
	If (jent .eq. ticEBL) Then	! entered blank line
					! leave argument unchanged
	Else If (jent .eq. ticERP) Then	! entered "return previous" command
		Return 1
	Else If (jent .eq. ticETI) Then	! entered other TI command
		go to 100		! reprompt
	Else If (jent .eq. ticENR) Then	! not recognized by TIX ENT
		If (ch last (ticENT) .gt. Len (CHAR)) Then
			Type 120, Len (CHAR)
  120			Format (/,' >>>truncating entry to ',i2.2,' bytes')
		Endif
		CHAR	=ticENT		! must have been new string
	Endif
        RETURN

	Entry TI INT4 (INT, *)
c||-------------------------------------| TI INT4
c|| accept integer * 4

  200	Write (ticENT, '(i6)') INT

	Do 201 JJ = 1, 5
		If (ticENT (JJ:JJ) .ne. ' ') Then
			go to 202
		Endif
  201	Continue

  202	Type 203, ticENT (JJ: 6)
  203	Format ('+/',a,'/ ',$)

	jent	=TIX ENT ()		! accept and classify line
	If (jent .eq. ticEBL) Then	! entered blank line
					! leave argument unchanged
	Else If (jent .eq. ticERP) Then	! entered "return previous" command
		Return 1
	Else If (jent .eq. ticETI) Then	! entered other TI command
		go to 200		! reprompt
	Else If (jent .eq. ticENR) Then	! not recognized by TIX ENT
		Write (ifrmt, 217) ch last (ticENT)
  217		Format ('(i',i2.2,')')
		Read (ticENT, ifrmt, Err =220) itemp
c     	If (	(itempC.GT. 32767) .OR. 
c    1		(itempC.LT. -32766)) GOTO 220
		INT	=itemp
		Return

c|	invalid integer
c	------- -------
  220		Type '(/,1x,a,//)', '>>>expect integer*4 or "/"'
		go to 200
	Endif
        RETURN

	Entry TI LOG (LOG, *)
c||-------------------------------------| TI LOG
c|| accept logical

  300	If (LOG) Then
		ticENT	='Yes'
	Else
		ticENT	='No'
	Endif

  302	Type 303, ticENT (1: ch last (ticENT))
  303	Format ('+/',a,'/ ',$)

	jent	=TIX ENT ()		! accept and classify line
	If (jent .eq. ticEBL) Then	! entered blank line
					! leave argument unchanged
	Else If (jent .eq. ticERP) Then	! entered "return previous" command
		Return 1
	Else If (jent .eq. ticETI) Then	! entered other TI command
		go to 300		! reprompt
	Else If (jent .eq. ticENR) Then	! not recognized by TIX ENT
		Call ch lcuc (ticENT)
		If (ch prfx (ticENT, 'Y') .ne. .0) Then
			LOG	=.true.
			Return
		Else If (ch prfx (ticENT, 'N') .ne. .0) Then
			LOG	=.false.
			Return
		Else
			Type '(/,1x,a,//)',
     x			'>>>first character must be "y" or "n"'
			go to 300
		Endif
	Endif

        RETURN

	Entry TIX EIL
c||-------------------------------------| TIX EIL
c|| echo input line

	If (ticLME) Then	! echo reassigned sys$input
		If (ticHPL) Then! HP LaserJet
			Type '(a,a,a,a)', '+', ticHU1,
     x				ticENT (1: ch last (ticENT)),
     x				ticHU2
		Else
			Type '(a,a)',	'+',
     x				ticENT (1: ch last (tic ENT))
		Endif
	Endif
	RETURN


	Entry TIX LRN
c||-------------------------------------| TIX LRN
c|| handle learn files

	If (ticLMU .le. 0) Then
		Type '(/,1x,a,//)',	'>>> learn files illegal'
		Return
	Endif

	If (cmd .eq. 'L') Then

		If (ticLMS .eq. 0) Then
			state	=1
			status	='NEW'
			go to 1000
		Else
			Type 701, ticLMF (1: ch last (ticLMF))
  701			Format (/, '   >Close "learn" file ',a,//)
			Close (Unit =ticLMU)
			ticLMS	=0
		Endif

	Else If (cmd .eq. '@') Then

		If (ticLMS .eq. 0) Then
			state	=2
			status	='OLD'
			go to 1000

		Else
			Type '(/,1x,a,//)',
     x			'>>> @ illegal while learning'

		Endif
	Endif
	Return

c| open a learn file
c  ---- - ----- ----
 1000	If (ticENT (3: ) .ne. ' ') Then
		ticLMF	=ticENT (3: )
	Endif
	If (ticLMF .eq. ' ') Then
		Type '(/,1x,a,$)',	'?filespec: '
 1001		Accept '(a)', ticLMF
		If (ticLMF .eq. ' ') Then
			Type '(//)'
			Return		! changed his mind
		Endif
	Endif

 1002	If (status .eq. 'OLD') Then

		Open (
     x			Unit	=ticLMU,
     x			File	=ticLMF,
     x			Status	=status,
     x			Err	=1003,
     x			READONLY)
	Else
		Open (
     x			Unit	=ticLMU,
     x			File	=ticLMF,
     x			Status	=status,
     x			Err	=1003,
     x
     x			CARRIAGECONTROL	='LIST')
	Endif
	ticLMS	=state
	If (ticLMS .eq. 1) Then
		Write (ticLMU, 1005) tiqPID, ticPID (1: ch last (ticPID))
 1005		Format ('! ',a,' "learn" file for program ', a)
	Endif
	Return

 1003		Type 1004, ticLMF (1: ch last (ticLMF))
 1004		Format (/,' Unable to open "',a,'" ',$)

		go to 1001		! try opening
        END

	Function TIX ENT ()
c||=====================================| TI.inc
c|| terminal input common
	Character*(*)	tiqPID
	Parameter	(tiqPID	='TI/1.2')

c| functions
c  ---------
	Integer		TIX ENT		! classify user's entry fcn
	Logical		TI USER		! user fcn
	Integer		ch last		! last nonspace fcn
	Integer		ch prfx		! test prefix fcn

c| TIX ENT outcomes
c  --- --- --------
	Integer		ticENR,	ticEBL,	ticERP,	ticETI, jent
	Parameter	(ticENR	=0)	! entry not recognized
	Parameter	(ticEBL	=1)	! entry is blank line
	Parameter	(ticERP	=2)	! entry is return to previous question
	Parameter	(ticETI	=3)	! entry is TI command

	Character*80	ticENT		! line entered by user

	Common		/ticomc/	ticENT

c| helpfile
c  --------
	Logical		ticHFO		! helpfile currently open
	Logical		ticHFT		! if helpfile has been tested

	Common		/ticomn/	ticHFO,	ticHFT

c| learn mode
c  ----- ----
	Integer		ticLMU		! file unit
	Integer		ticLMS		! state	  0 => no file open
					!         1 => writing
					!         2 => reading
	Logical		ticLME		! if TTY input to be echoed
	Logical		ticHPL		! if printer is HP LaserJet

	Common		/ticomn/	ticLMU,	ticLMS,	ticLME,	ticHPL

	Character*80	ticLMF		! filespec
	Character*20	ticPID		! calling pgm's id string
	Character*4	ticHU1		! HP begin underline seqence
	Character*4	ticHU2		! HP end   "         "

	common		/ticomc/	ticLMF, ticPID,	ticHU1,	ticHU2
C **** Variables for TIXENT ****
	Integer*4	jslant
	Character*2	cmd
	Equivalence	(cmd, ticENT (2: 3))
	Character*80	temp

c||-------------------------------------| TIX ENT
c| fetch input line
c  ----- ----- ----
  400	If (ticLMS .eq. 2) Then		! reading learn file
		Read (ticLMU, '(a)', End =401) ticENT
		Type '(a,a)',	'+', ticENT (1: ch last (tic ENT))
		go to 402

  401			Close (Unit =ticLMU)
			ticLMS	=0	! learn mode inactive
			go to 400

  402		Continue

	Else
		Accept '(a)', ticENT	! get string entry
		Call TIX EIL
	Endif

	Call ch ccsp (ticENT)		! ctl chars to spaces

c| blank line
c  ----- ----
	If (ticENT .eq. ' ') Then
		TIX ENT	=ticEBL

c| comment
c  -------
	Else If (ch prfx (ticENT, '!') .ne. 0) Then
		TIX ENT	=ticETI		! ask another
			Type *, ' '

c| display command menu
c  ------- ------- ----
	Else If (ch prfx (ticENT, '/') .ne. 0) Then
		jslant	=ch prfx (ticENT, '/')
		temp	=ticENT (jslant: )
		ticENT	=temp
		Call ch lcuc (cmd)
		If (ticENT (2: ) .eq. ' ') Then
			Type 1, tiqPID
    1			Format (
     x
     x			/,t3, a, ' commands are:',
     x			/,t5, '/P',
     x			/,t10,'returns to previous question',
     x			/,t5, '/V COMMAND',
     x			/,t10,'calls VMS to execute COMMAND and waits',
     x			/,t10,'for completion of COMMAND',
     x			/,t5, '/VP COMMAND',
     x			/,t10,'like /V COMMAND, but program execution',
     x			/,t10,'continues in parallel with COMMAND',
     x			/,t5, '/Q',
     x			/,t10,'quits (stops program)')

c|	learn options
c	----- -------
			If (ticLMU .ne. 0) Then
					! learn mode ok
				If (ticLMS .eq. 0) Then
					Type 1001
 1001					Format (t5,'/L',
     x					/,t10,'start learning')
				Else If (ticLMS .eq. 1) Then
					Type 1002
 1002					Format (t5,'/L',
     x					/,t10,'stop learning')
				Endif
				If (ticLMS .eq. 0) Then
					Type 1003
 1003					Format (t5,'/@',
     x					/,t10,'fetch commands from file')
				Endif
			Endif
			Type '(/)'
			TIX ENT	=ticETI
c| entered command
c  ------- -------
c|	return predecessor
c	------ -----------
		Else If (cmd .eq. 'P ') Then
			TIX ENT	=ticERP

c|	execute VMS command and wait
c	------- --- ------- --- ----
		Else If (cmd .eq. 'V ') Then
			Type *, ' '
			Call VX DCL (ticENT (4: ), .true.)
			Type '(/)'
			TIX ENT	=ticETI

c|	execute VMS command and continue
c	------- --- ------- --- --------
		Else If (cmd .eq. 'VP') Then
			Type *, ' '
			Call VX DCL (ticENT (4: ), .false.)
			Type '(/)'
			TIX ENT	=ticETI

c|	learn file
c	----- ----
		Else If (
     x			(ticENT (2: 2) .eq. 'L') .or.
     x			(ticENT (2: 2) .eq. '@')  )   Then

			Call TIX LRN	! handle learn file
			TIX ENT	= ticETI
			If (ticLMS .eq. 1)   Return	! don't record /L

c|	echo
c	----
		Else If (ticENT (2: 2) .eq. 'E') Then
			ticLME	=.not. ticLME
			TIX ENT	=ticETI
			ticHPL	=ticENT (3: 4) .eq. 'HP'
					! if HP LaserJet
			ticHU1		=' &dD'
			ticHU1 (1:1)	=Char ('1B'x)
			ticHU2		=' &d@'
			ticHU2 (1:1)	=Char ('1B'x)
			Call TIX EIL	! echo it
			Type *, ' '

c| exit
c  ----
		Else If (ticENT (2: 2) .eq. 'Q') Then
			Call Exit

c| unrecognizable
c  --------------
		Else
			Type '(/,1x,a,//)',
     x			'>>>don''t recognize "/" command'
			TIX ENT	= ticETI
		Endif
	Else
		TIX ENT	=ticENR		! entry not recognized
	Endif

c| copy to learn file
c  ---- -- ----- ----
	If (ticLMS .eq. 1) Then		! writing learn file
		Write (ticLMU, '(a)') ticENT (1: ch last (ticENT))
	Endif

	return

	End

c||=====================================| CH.for
c|| character manipulation
c||-------------------------------------| CH CCSP
c|| convert control characters to spaces
c
	Subroutine CH CCSP (string)
c
	INTEGER*4	JJ

	Character*(*)	string
c
	Do 111 JJ = 1, Len (string)
		If (Ichar (string (JJ:JJ)) .lt. 32) Then
			string (JJ:JJ) = ' '
		Endif
  111	Continue

	return

	End

c||-------------------------------------| CH LAST
c|| find last nonblank in string
c
	Integer Function CH LAST (string)
c
	INTEGER*4	JJ

	Character*(*)	string
c
	Do 111 JJ = Len (string), 2, -1
		If (string (JJ: JJ) .ne. ' ') Then
			go to 1
		Endif
  111	Continue

    1	CH LAST = JJ

	return

	End

c||-------------------------------------| CH LCUC
c|| convert lower to upper case
c
	Subroutine CH LCUC (string)
c
	INTEGER*4	idiff,	jj

	Character*(*)	string
	Character*1	c
c
	idiff	=Ichar ('A') -Ichar ('a')
	Do 111 jj = 1, Len (string)
		c =string (jj:jj)
		If ((c .ge. 'a') .and. (c .le. 'z')) Then
			string (jj:jj) =Char (Ichar (c) +idiff)
		Endif
  111	Continue
	return
	End

c||-------------------------------------| CH PRFX
c|| find prefix string
c
	Integer Function CH PRFX (STR1, STR2)
c
	Character* (*)	STR1, STR2
c
	CH PRFX =Index (STR1, STR2)
	If (CH PRFX .gt. 1) Then	! string is interior
		If (STR1 (1: CH PRFX -1) .ne. ' ') Then
			CH PRFX =0	! wasn't a prefix
		Endif
	Endif
	return
	End

c||-------------------------------------| VX DCL
c|| spawn DCL

	Subroutine VX DCL (CMD, WAIT)

	Character*(*)	CMD		! DCL command to be executed

	Logical		WAIT		! if want to wait

	External	Cli$m_nowait	! no wait bit

	If (WAIT) Then
		Call Lib$spawn (CMD,,,,)
	Else
		Call Lib$spawn (CMD,,, %Loc (Cli$m_nowait))
	Endif
	return
	End
