      PROGRAM PRINTLZ
C+
C       Taken from WIND_CD_READ, this program transfers the data from a Level 
C       0 file on a CD-ROM and constructs a Level 1 disk file.  The user is 
C       prompted for the start and stop times to use (typically a 24 hr period), 
C       as well as several other operating parameters.
C
C     This version:   1.2   17-FEB-1995
C
C     REFERENCE:
C
C     USAGE and PARAMETERS:
C       RUN WIND_PRINTLZ
C
C     CALLS/FUNCTIONS:
C       CALL EPOCH_TO_ITIME( RTIME, ITIME, ISTAT )
C       CALL EXIT( ISTAT )                              ! SYSTEM
C       CALL MVBITS( TEMP2, 2, 6, MSpin, 0 )            ! SYSTEM
C       CALL TILOG( lSCTime, *1000 )                    !! XTI_LIB.OLB
C       CALL WIND_CD_INIT()
C       CALL WIND_EPOCH_TO_GREG(        SC_Beg_Year,                                            !! WIND_LIB.OLB
C     :                                 SC_Beg_DOY,
C     :                                 SC_Beg_Msec,
C     :                                 SC_Beg_MicSec,
C     +                                 STRTime,        ! UTC: EPOCH as Real*8
C     +                                 ISTAT )
C       CALL WIND_GET_FILENAME( FILENAME, ISTAT )                                               !! WIND_LIB.OLB
C       CALL WIND_GET_LZ_TIMES( DSTRYRDAY, DSTPYRDAY, STRTime, STPTime, ISTAT )                 !! WIND_LIB.OLB
C       CALL WIND_GREG_TO_EPOCH(        LR_Beg_Year,    ! ATC: Year                             !! WIND_LIB.OLB
C     +                                 LR_Beg_DOY,     ! ATC: Day Of Year
C     +                                 LR_Beg_MSec,    ! ATC: Millisecond of DAY
C     +                                 LR_Beg_MicSec,  ! ATC: Microsecond of MSec
C     +                                 LR_Beg_EPOCH,   ! ATC: EPOCH AS REAL*8
C     +                                 ISTAT )
C       CALL WIND_OPEN_LZ_DATA( FILENAME, 'OLD', ISTAT )                                !! WIND_LIB.OLB
C       CALL WIND_PB5_TO_EPOCH(         LR_Beg_SC_Clock,! UTC: PB5 (2 INTEGER*4)        !! WIND_LIB.OLB
C     +                                 SC_Beg_EPOCH,   ! UTC: EPOCH as Real*8
C     +                                 ISTAT )
C       CALL WIND_PRINT_HK( DR_Beg_MajFr_Cnt, HKset, SYNC_WORD )
C       CALL WIND_RESET_HK(EDBSet-1)
C
C       = BTEST( TEMP2, 1 )                                     ! SYSTEM
C       = CHLAST( FILENAME )                                    ! UMDCOMMON.OLB
C       = IAND( TEMP2, 'FF'X )                                  ! SYSTEM
C       = IOR( ISHFT( HDBTime, 8 ), (TEMP4 .AND. 'FF'X) )       ! SYSTEM
C       = JIOR( ISHFT( (TEMP4 .AND. 'FF'X), 8 ), SRCount )      ! SYSTEM
C
C     CREATION:
C       24-JAN-1995     J.PAQUETTE
C
C     REVISIONS:
C	27-JAN-1995	L. Bleau	added code to write multiple dump files,
C                                       changed FIL1NAME to DUMPNAME
C 1.2   17-FEB-1995	L. Bleau	changed SFDU_Lun to L0_Lun, since it
C					refers to the L0 (or LZ) file and not
C					to the SFDU file 
C-

CDEC$ IDENT     '1.3'

      IMPLICIT NONE

      DATA      Ver_EPIC_SFDU_to_L0 / 1.00, 0/ ! Software version number

      INCLUDE   'INC_DIR:EDB.INC/NOLIST'
      INCLUDE   'INC_DIR:L0_DATA_Record.INC/NOLIST'
      INCLUDE   'INC_DIR:L0_Labl_Record.INC/NOLIST'
      INCLUDE   'INC_DIR:SW_CODES.INC/NOLIST'
      INCLUDE   'INC_DIR:WIND_Lun.INC/NOLIST'
      INCLUDE   '($SSDEF)/NOLIST'

      COMMON /FILE/  FILENAME, DUMPNAME

      COMMON /DATES/ SC_Beg_Year, SC_Beg_DOY, SC_Beg_Msec, SC_Beg_MicSec,
     &               SC_End_Year, SC_End_DOY, SC_End_Msec, SC_End_MicSec
C
      REAL*8            DR_Beg_SC_EPOCH,!
     :                  DSTPYRDAY,      ! STOP TIME
     :                  DSTRYRDAY,      ! START TIME
     :                  DYRDAY,         ! CURRENT TIME
     :                  SC_Beg_EPOCH,   ! UTC: S/C Clock
     :                  SC_End_EPOCH,   ! UTC: S/C Clock
     :                  STRTime,        ! Requested start time
     :                  STPTime,        ! Requested stop time
     +                  RECFLAG/0D0/,   ! TEMPORARY
     +                  DELTATime/184D0/,! HighBit Rate
     :                  DELTAT(0:59)/   2097.6D0,
     +                                  1674.4D0,
     +                                  1214.4D0,
     +                                  57*828.D0/,     ! Empiric set EDB output delay
     :                  T,              ! TEMPORARY TIME
     :                  T1              ! TEMPORARY TIME

      INTEGER*4         Bytes_SFrame,   ! Bytes per Subframe
     +                  CHLAST,         ! FUNCTION
     +                  EDBpos,         ! internal
     +                  EDB_Start,      ! internal
     +                  HDBSet/1/,      ! internal
     +                  HDB_End,        ! End position of HDB
     +                  HDB_Pos,        ! Current position of HDB
     +                  HDB_Start,      ! Starting position of HDB
     :                  HDBTime,        ! Time contained in HDB-Header
     :                  ID,             ! DAY
     :                  II,             ! LOOP COUNTER
     :                  IJ,             !  "      "
     :                  IM,             ! MONTH
     :                  IY,             ! YEAR
     :                  IPRNT/8/,       ! DEBUGGING INFO TO FILE
     :                  IPRT/7/,        ! DEBUGGING INFO TO FILE
     :                  IREC,           ! last character position of read string
     :                  ISTAT,          ! STATUS FOR OPENING DATAFILE
     :                  JJ,             ! LOOP COUNTER
     :                  JIOR,           ! FUNCTION
     :                  KK,             ! LOOP COUNTER
     +                  LOAD_EDB,       ! Assigned GOTO Label
     +                  MAXSFrame/250/, ! maximum number of Subframes per Majorframe
     :                  MAXSpCnt/255/,  ! maximum number of cont. SpinCount
     :                  MM,             ! POSITION OF ';'
     :                  NCF,            ! NUMBER OF CHARACTERS IN FILENAME
     :                  O_SpinCnt,      ! Last Spin counter
     :                  POINTER,        ! INTERNAL 
     :                  SC_Beg_Year,    !
     :                  SC_Beg_DOY,     !
     :                  SC_Beg_Msec,    !
     :                  SC_Beg_MicSec,  !
     :                  SC_End_Year,    !
     :                  SC_End_DOY,     !
     :                  SC_End_Msec,    !
     :                  SC_End_MicSec,  !
     +                  SRCount,        ! Science Record Count, read from MajF
     +                  TEMP4,
     +                  IJK             ! IMPLICIT NONE SUCKS

      INTEGER*4
     :                  MSpin,          ! Measured Spin
     :                  O_MSpin,        ! Last Measured Spin
     +                  TEMP2

      INTEGER*4         SR_Qual/8/      ! anticipated HDB Quality

      LOGICAL*4         HDBFirst/.TRUE./,
     +                  BTEST,          ! FUNCTION
     +                  BUFFER_READ/.FALSE./,   ! MAJOR FRAME READ FLAG
     +                  CmdErrFlag,     ! Command Error Flag
     +                  EDB_done,       ! internal
     +                  FirstEDB/.TRUE./,!First time flag
     +                  lSCTime,        ! S/C clock is PREFERRED TIME
     :                  OBitRate/.TRUE./,       ! Old BitRate
     :                  OK,             ! internal value
     :                  VALData         ! Valid Data available (within time)

      LOGICAL SYNC_WORD
C
      CHARACTER*80      FILENAME, DUMPNAME, INFILE, INCHAR
      CHARACTER*34      CLASS(0:9)
      CHARACTER*8       PROGRAM_START_TIME, PROGRAM_END_TIME
      CHARACTER*23      START_DATE_TIME
      CHARACTER*30      ATIME, WIND_FORMAT_TIME

      INTEGER           DUMP_SEQUENCE/1/,
     &                  DUMP_FILE_L1_RECS/120/

      INTEGER SPIN, DIAG
      LOGICAL INTER
      COMMON /DIAG/ DIAG,INTER
      DATA DIAG/6/

      INTEGER*4 FORTERR,RMSSTC,RMSSTV,LUN,VAXERR

      CALL TIM_STAMP
1000  CONTINUE
      CALL WIND_CD_INIT

  100 FORMAT(1H , A, $)

      FILENAME = 'WIND_LZ_DATA:*LZ*.DAT'
      WRITE( TTOUT, 100 )   'LZ-FILENAME = '
      CALL WIND_GET_FILENAME( FILENAME, ISTAT )                 ! FILE NAME
      IF( ISTAT .NE. SS$_NORMAL )   STOP 0001
C
 1020 CONTINUE
      CALL WIND_OPEN_LZ_DATA( FILENAME, 'OLD', ISTAT )          ! OPEN FILE
C
      INQUIRE( L0_Lun, NAME=FILENAME )
      NCF    = CHLAST( FILENAME )
      IF(DIAG .LE. 5) WRITE( TTOUT, 110 )   FILENAME(1:NCF), ISTAT
  110 FORMAT( 1H , 'OPENED FILE: ', A, ',  STATUS IS', I3 )
      IF( ISTAT .NE. SS$_NORMAL ) STOP 0002
C
      IREC = LR_Phys_Rec_Size
C
C  LABEL_RECORD                                              ! READ LABEL RECORD
C
C      READ( L0_Lun, 400, END=9999, ERR=9998)  IREC, ( LR_BUFFER(II), II=1,IREC )
      READ( L0_Lun, 401, END=9999, ERR=9998)  IREC, C_LR_BUFFER(1:IREC)
  400 FORMAT( Q, 12800A1 )
  401 FORMAT( Q, A )

      CALL FIXUP( LR_Beg_SC_Clock )
      CALL FIXUP( LR_End_SC_Clock )

      IF(DIAG .LE. 4) WRITE( TTOUT, 111 )  LR_Phys_Rec_Size
  111 FORMAT( 1H0, ' RECORD SIZE FOR THIS FILE =', I6, // )

 1040 CONTINUE
      MM = INDEX( FILENAME(1:NCF), ']' )
      KK = INDEX( FILENAME(MM+1:NCF), '.' )
      DUMPNAME = FILENAME
      IF (KK .NE. 0) THEN
        DUMPNAME(MM+KK:MM+KK+3) = '.DMP'
      ELSE
        DUMPNAME(NCF+1:NCF+4) = '.DMP'
      ENDIF

      KK = INDEX( DUMPNAME(1:NCF), ';' )
      IF( KK .NE. 0 )    DUMPNAME(KK:)  = ' '
      KK = INDEX( DUMPNAME(1:NCF), ' ' )
      WRITE( TTOUT, 101 )   'LZ-DUMP-FILENAME = ',DUMPNAME(1:KK)
101   FORMAT(1X,A,A)
      CALL WIND_GET_FILENAME( DUMPNAME, ISTAT )                 ! FILE NAME

      IF( ISTAT .NE. SS$_NORMAL .AND.
     :    ISTAT .NE. 0 ) CALL EXIT( ISTAT )

C Remove explicit version number from filename (starting at ';') before opening
C If we don't Fortran OPEN gives error 43 when file already exists
 
      MM = INDEX( DUMPNAME, ';' )
      IF (MM .NE. 0) DUMPNAME(MM:)=' '
      OPEN(UNIT=28,NAME=DUMPNAME,STATUS='NEW')

 1060 CONTINUE
C
 1120 WRITE( TTOUT, 100 ) 'Do you prefer S/C clock ?'
      lSCTime = .FALSE.
      CALL TILOG( lSCTime, *1000 )

      CALL WIND_PB5_TO_EPOCH(           LR_Beg_SC_Clock,! UTC: PB5 (2 INTEGER*4)
     +                                  SC_Beg_EPOCH,   ! UTC: EPOCH as Real*8
     +                                  ISTAT )
      IF( ISTAT .NE. SS$_NORMAL ) STOP 0003

      CALL WIND_PB5_TO_EPOCH(           LR_End_SC_Clock,! UTC: PB5 (2 INTEGER*4)
     +                                  SC_End_EPOCH,   ! UTC: EPOCH as Real*8
     +                                  ISTAT )
      IF( ISTAT .NE. SS$_NORMAL ) STOP 0004

      CALL WIND_GREG_TO_EPOCH(          LR_Beg_Year,    ! ATC: Year
     +                                  LR_Beg_DOY,     ! ATC: Day Of Year
     +                                  LR_Beg_MSec,    ! ATC: Millisecond of DAY
     +                                  LR_Beg_MicSec,  ! ATC: Microsecond of MSec
     +                                  LR_Beg_EPOCH,   ! ATC: EPOCH AS REAL*8
     +                                  ISTAT )
      IF( ISTAT .NE. SS$_NORMAL ) STOP 0005

      CALL WIND_GREG_TO_EPOCH(          LR_End_Year,
     +                                  LR_End_DOY,
     +                                  LR_End_MSec,
     +                                  LR_End_MicSec,
     +                                  LR_End_EPOCH,
     +                                  ISTAT )
      IF( ISTAT .NE. SS$_NORMAL ) STOP 0006

 1140 CONTINUE
      IF( lSCTime ) THEN
          DSTRYRDAY = SC_Beg_EPOCH
          DSTPYRDAY = SC_End_EPOCH
      ELSE
          DSTRYRDAY = LR_Beg_EPOCH
          DSTPYRDAY = LR_End_EPOCH
      ENDIF
C
C  GET START & STOP TIMES
C
      CALL WIND_GET_LZ_TIMES( DSTRYRDAY, DSTPYRDAY, STRTime, STPTime, ISTAT )

      CALL WIND_EPOCH_TO_GREG(          SC_Beg_Year,
     :                                  SC_Beg_DOY,
     :                                  SC_Beg_Msec,
     :                                  SC_Beg_MicSec,
     +                                  STRTime,        ! UTC: EPOCH as Real*8
     +                                  ISTAT )
      IF( ISTAT .NE. SS$_NORMAL ) STOP 0007

      CALL WIND_EPOCH_TO_GREG(          SC_End_Year,
     :                                  SC_End_DOY,
     :                                  SC_End_Msec,
     :                                  SC_End_MicSec,
     +                                  STPTime,        ! UTC: EPOCH as Real*8
     +                                  ISTAT )
      IF( ISTAT .NE. SS$_NORMAL ) STOP 0008

      IF(DIAG .LE. 5) WRITE( TTOUT, 114 )          SC_Beg_Year,
     :                          SC_Beg_DOY,
     :                          SC_Beg_Msec,
     :                          SC_Beg_MicSec,
     +                          STRTime,
     :                          SC_End_Year,
     :                          SC_End_DOY,
     :                          SC_End_Msec,
     :                          SC_End_MicSec,
     +                          STPTime

  114 FORMAT(   ' Requested times', /,
     :          ' START :',     T12, ' Year =', T24, I4, /,
     :                          T12, ' DOY =',  T24, I4, /,
     :                          T12, ' Msec =', T20, I8, /,
     :                          T12, ' uSec =', T24, I4, /,
     :                          T12, ' STRTime=',  T22, F22.3, //,
     :          ' End :',       T12, ' Year =', T24, I4, /,
     :                          T12, ' DOY =',  T24, I4, /,
     :                          T12, ' Msec =', T20, I8, /,
     :                          T12, ' uSec =', T24, I4, /,
     :                          T12, ' STPTime=',  T22, F22.3 )

      IF(DIAG .LE. 4) WRITE( TTOUT, 116 )  LR_Phys_Rec_Num, LR_Phys_Rec_in_File
  116 FORMAT( ' Starting Physical Record Number   =', I5, /,
     :        ' Number of Physical Records in File=', I5 ) 
      CALL LIB$DATE_TIME(START_DATE_TIME)
      CALL TIME(PROGRAM_START_TIME)

      IF(DIAG .LE. 4) WRITE( IPRT, 117 ) LR_SCID,       ! Spacecraft ID
     +          LR_Instr_Num,           ! Instrument Number
     +          LR_Instr_Name,          ! Instrument Name
     +          LR_Phys_Rec_Num,        ! Physical Record Number
     +          LR_Phys_Rec_per_MajFr,  ! Physical Records Per Major Frame
     +          LR_Phys_Rec_in_File,    ! Number of Physical Records in File
     +          LR_Beg_MajFr_Cnt,       ! Major Frame Count - First Major Frame
     +          LR_End_MajFr_Cnt,       ! Major Frame Count _ Last Major Frame
     +          LR_Beg_SC_Clock,        ! S/C Clock-Beginning of First MajFrame
     +          LR_End_SC_Clock,        ! S/C Clock-Beginning of Last MajFrame
     +          LR_Beg_Year,            ! ATC: year-Beginning of First MajFrame
     +          LR_Beg_DOY,             ! ATC: day -Beginning of First MajFrame
     +          LR_Beg_MSec,            ! ATC: msec-Beginning of First MajFrame
     +          LR_Beg_MicSec,          ! ATC: usec-Beginning of First MajFrame
     +          LR_End_Year,            ! ATC: year-Beginning of Last MajFrame
     +          LR_End_DOY,             ! ATC: day -Beginning of Last MajFrame
     +          LR_End_MSec,            ! ATC: msec-Beginning of Last MajFrame
     +          LR_End_MicSec,          ! ATC: usec-Beginning of Last MajFrame
     +          LR_Num_MajFr_Exp,       ! Number of Major Frames Expected
     +          LR_Num_MajFr_File,      ! Number of Major Frames in File
     +          LR_Num_MajFr_Gaps,      ! Major Frame Level Gaps in Coverage
     +          LR_Data_Cov_Typ,        ! Data Coverage Type
     +          LR_Decom_Rerun_Num,     ! Decommutation Rerun Number
     +          LR_Decom_Prog_V_Num,    ! Decommutation Program Version Number
     +          LR_Decom_Char_DB_V_Num, ! Decom.Characteristic Database Vers.Num
     +          LR_Decom_Run_DateTime,  ! Decommutation Run Date/Time
     +          LR_Instr_File_Name,     ! Instrument File Name
     +          LR_Phys_Rec_Size,       ! Physical Record Length
     +          LR_Merge_Rerun_Num,     ! Merge Rerun Number
     +          LR_Merge_Prog_V_Num,    ! Merge Program Version Number
     +          LR_Merge_Run_DateTime,  ! Merge Run Date/Time
     +          LR_Num_Edit_Files,      ! Number of Edit Files
     +          LR_Beg_EPOCH,
     +          LR_End_EPOCH
  117 FORMAT( 1H1,' LR_SCID             =', I6,
     +  /,      ' LR_Instr_Num          =', I6,
     +  /,      ' LR_Instr_Name         =  ', A4,
     +  /,      ' LR_Phys_Rec_Num       =', I6,
     +  /,      ' LR_Phys_Rec_per_MajFr =', I6,
     +  /,      ' LR_Phys_Rec_in_File   =', I6,
     +  /,      ' LR_Beg_MajFr_Cnt      =', I6,
     +  /,      ' LR_End_MajFr_Cnt      =', I6,
     +  /,      ' LR_Beg_SC_Clock       =', 2Z8.8,      ! S/C Clock-Beginning of First MajFrame
     +  /,      ' LR_End_SC_Clock       =', 2Z8.8,      ! S/C Clock-Beginning of Last MajFrame
     +  /,      ' LR_Beg_Year           =', I6,         ! ATC: year-Beginning of First MajFrame
     +  /,      ' LR_Beg_DOY            =', I6,         ! ATC: day -Beginning of First MajFrame
     +  /,      ' LR_Beg_MSec           =', I9,         ! ATC: msec-Beginning of First MajFrame
     +  /,      ' LR_Beg_MicSec         =', I6,         ! ATC: usec-Beginning of First MajFrame
     +  /,      ' LR_End_Year           =', I6,         ! ATC: year-Beginning of Last MajFrame
     +  /,      ' LR_End_DOY            =', I6,         ! ATC: day -Beginning of Last MajFrame
     +  /,      ' LR_End_MSec           =', I9,         ! ATC: msec-Beginning of Last MajFrame
     +  /,      ' LR_End_MicSec         =', I6,         ! ATC: usec-Beginning of Last MajFrame
     +  /,      ' LR_Num_MajFr_Exp      =', I6          ! Number of Major Frames Expected
     +  /,      ' LR_Num_MajFr_File     =', I6,         ! Number of Major Frames in File
     +  /,      ' LR_Num_MajFr_Gaps     =', I6,         ! Major Frame Level Gaps in Coverage
     +  /,      ' LR_Data_Cov_Typ       =  ', A4,       ! Data Coverage Type
     +  /,      ' LR_Decom_Rerun_Num    =', I6,         ! Decommutation Rerun Number
     +  /,      ' LR_Decom_Prog_V_Num   =', A8,         ! Decommutation Program Version Number
     +  /,      ' LR_Decom_Char_DB_V_Num=', A8,         ! Decom.Characteristic Database Vers.Num
     +  /,      ' LR_Decom_Run_DateTime =', A16,        ! Decommutation Run Date/Time
     +  /,      ' LR_Instr_File_Name    =', A44,        ! Instrument File Name
     +  /,      ' LR_Phys_Rec_Size      =', I6,         ! Physical Record Length
     +  /,      ' LR_Merge_Rerun_Num    =', I6,         ! Merge Rerun Number
     +  /,      ' LR_Merge_Prog_V_Num   =', A8,         ! Merge Program Version Number
     +  /,      ' LR_Merge_Run_DateTime =', A16,        ! Merge Run Date/Time
     +  /,      ' LR_Num_Edit_Files     =', I6,         ! Number of Edit Files
     +  /,      ' LR_Beg_EPOCH          =', F20.5,      ! ATC: EPOCH, First Major Frame
     +  /,      ' LR_End_EPOCH          =', F20.5 )     ! ATC: EPOCH, Last Major Frame

      C_DR_Buffer = C_NULL_Buffer                               ! Initialize DATA buffer
C
C  DATA_RECORD  1
C
C  READ DATA RECORD                                             ! READ DATA RECORD
C      READ( L0_Lun, 400, END=9999, ERR=9998) IREC,( DR2_BUFFER(II), II=1,IREC )
      READ( L0_Lun, 401, END=9999, ERR=9998) IREC,C_DR2_BUFFER(1:IREC)

      CALL FIXUP( DR_Beg_SC_Clock )

      IF(DIAG .LE. 3) WRITE( TTOUT, 118 )   'First READ', DR2_Phys_Rec_Num, DR2_Beg_MajFr_Cnt
  118 FORMAT( 1H0, A, ', DR2_Phys_Rec_Num =', I5, ', DR2_Beg_MajFr_Cnt = ', Z8 )

      BUFFER_READ = .TRUE.
      IF( lSCTime ) THEN
          CALL WIND_PB5_TO_EPOCH(       DR2_Beg_SC_Clock,! UTC: PB5 (2 INTEGER*4)! Calc S/C-EPOCH
     +                                  DR2_SC_EPOCH,    ! UTC: EPOCH as Real*8
     +                                  ISTAT )
          IF( ISTAT .NE. SS$_NORMAL ) STOP 0010
          T = DR2_SC_EPOCH
      ELSE
          CALL WIND_GREG_TO_EPOCH(      DR2_Beg_Year,   ! ATC: Year     ! Calc WALL-EPOCH
     +                                  DR2_Beg_DOY,    ! ATC: Day Of Year
     +                                  DR2_Beg_MSec,   ! ATC: Millisecond of DAY
     +                                  DR2_Beg_MicSec, ! ATC: Microsecond of MSec
     +                                  DR2_Beg_EPOCH,  ! ATC: EPOCH AS REAL*8
     +                                  ISTAT )
          IF( ISTAT .NE. SS$_NORMAL ) STOP 0009
          T = DR2_Beg_EPOCH
      END IF
      IF( STRTime .LE. T ) VALData = .TRUE.

 1200 CONTINUE
      IF( BUFFER_READ ) THEN
          C_DR_Buffer = C_DR2_Buffer
c          IF( .NOT.FirstEDB ) OBitRate = BitRate
          OBitRate = BitRate

          IF(DIAG .LE. 2) WRITE( IPRNT, 120 )   ' SHIFTED', DR_Phys_Rec_Num, DR_Beg_MajFr_Cnt
  120 FORMAT( 1H0, A, ', DR_Phys_Rec_Num =', I5, ', DR_Beg_MajFr_Cnt = ', Z8 )

      END IF ! ( BUFFER_READ )

C      READ( L0_Lun, 400, END=9999, ERR=9998) IREC,( DR2_BUFFER(II), II=1,IREC )
      READ( L0_Lun, 401, END=9999, ERR=9998) IREC,C_DR2_BUFFER(1:IREC)

      CALL FIXUP( DR_Beg_SC_Clock )

      BUFFER_READ = .TRUE.
      IF( VALData ) GOTO 1300

      IF( lSCTime ) THEN
          CALL WIND_PB5_TO_EPOCH(       DR2_Beg_SC_Clock,! UTC: PB5 (2 INTEGER*4)
     +                                  T,               ! UTC: EPOCH as Real*8
     +                                  ISTAT )
          IF( ISTAT .NE. SS$_NORMAL ) STOP 0011
      ELSE
          CALL WIND_GREG_TO_EPOCH(      DR2_Beg_Year,   ! ATC: Year
     +                                  DR2_Beg_DOY,    ! ATC: Day Of Year
     +                                  DR2_Beg_MSec,   ! ATC: Millisecond of DAY
     +                                  DR2_Beg_MicSec, ! ATC: Microsecond of MSec
     +                                  T,              ! ATC: EPOCH AS REAL*8
     +                                  ISTAT )
          IF( ISTAT .NE. SS$_NORMAL ) STOP 0012
      END IF

      IF( STRTime .GT. T ) GOTO 1200

      VALData = .TRUE.

      IF( STRTime .EQ. T ) GOTO 1200
      
      IF(DIAG .LE. 3) WRITE( TTOUT, 118 )   ' 2nd READ', DR2_Phys_Rec_Num, DR2_Beg_MajFr_Cnt

 1300 CONTINUE
      CALL WIND_PB5_TO_EPOCH(           DR_Beg_SC_Clock,! UTC: PB5 (2 INTEGER*4)
     +                                  DR_SC_EPOCH,    ! UTC: EPOCH as Real*8
     +                                  ISTAT )
      IF( ISTAT .NE. SS$_NORMAL ) STOP 0013

      CALL WIND_EPOCH_TO_GREG(          SC_Beg_Year,
     :                                  SC_Beg_DOY,
     :                                  SC_Beg_Msec,
     :                                  SC_Beg_MicSec,
     +                                  DR_SC_EPOCH,    ! UTC: EPOCH as Real*8
     +                                  ISTAT )
      IF( ISTAT .NE. SS$_NORMAL ) STOP 0014

      CALL WIND_GREG_TO_EPOCH(          DR_Beg_Year,    ! ATC: Year
     +                                  DR_Beg_DOY,     ! ATC: Day Of Year
     +                                  DR_Beg_MSec,    ! ATC: Millisecond of DAY
     +                                  DR_Beg_MicSec,  ! ATC: Microsecond of MSec
     +                                  DR_Beg_EPOCH,   ! ATC: EPOCH AS REAL*8
     +                                  ISTAT )
      IF( ISTAT .NE. SS$_NORMAL ) STOP 0015

      IF( lSCTime ) THEN
          DYRDAY = DR_SC_EPOCH
      ELSE
          DYRDAY = DR_Beg_EPOCH
      END IF
      Pref_EPOCH = DYRDAY

      CALL WIND_PRINT_HK( DR_Beg_MajFr_Cnt, HKset(EDBset), SYNC_WORD )

      IF(DIAG .LE. 2) WRITE( IPRNT, 130 )       DR_Phys_Rec_Num,DR_Beg_MajFr_Cnt,
     +                          DR_Beg_Year,    SC_Beg_Year,    ! Year
     +                          DR_Beg_DOY,     SC_Beg_DOY,     ! Day Of Year
     +                          DR_Beg_MSec,    SC_Beg_Msec,    ! Millisecond of DAY
     +                          DR_Beg_MicSec,  SC_Beg_MicSec,  ! Microsecond of MSec
     +                          DR_Beg_EPOCH,   DR_SC_EPOCH     ! EPOCH AS REAL*8

  130 FORMAT(   T10, 'Phys.Rec.#', I4,  T50, ' WALL-TIME',      T72, ' S/C-TIME', /,
     :          T10, 'Major Fr.=',Z4.4, T50, ' =========',      T72, ' ========', /,
     :          T30, 'YEAR :',          T54, I4,        T75, I4, /,
     :          T30, ' DOY :',          T54, I4,        T75, I4, /,
     :          T30, 'msec :',          T50, I8,        T71, I8, /,
     :          T30, 'usec :',          T54, I4,        T75, I4, /,
     :          T30, 'EPOCH :',         T38, F20.3,     T59, F20.3, /)

  131 FORMAT(   T10, 'SCI.Rec.#', I5,   T50, ' WALL-TIME',      T72, ' S/C-TIME', /,
     :          T10, 'MSpin #.=', I5,   T50, ' =========',      T72, ' ========', /,
     :          T30, 'YEAR :',          T53, I5,        T74, I5, /,
     :          T30, ' DOY :',          T53, I5,        T74, I5, /,
     :          T30, 'HOURS:',          T53, I5,        T74, I5, /,
     :          T30, 'MINUTES:',        T53, I5,        T74, I5, /,
     :          T30, 'SECONDS:',        T53, I5,        T74, I5, /,
     :          T30, 'CENTISECONDS:',   T53, I5,        T74, I5, /)

      IF(DIAG .LE. 2) WRITE( IPRT, 132 )
     +          DR_Instr_Num,           ! Instrument Number
     +          DR_Phys_Rec_Num,        ! Physical Record Number within File
     +          DR_Beg_MajFr_Cnt,       ! Major Frame Count-Beginning of Maj.Fra
     +          DR_Beg_SC_Clock,        ! Spacecraft Clock -Beginning of Maj.Fra
     +          DR_Beg_Year,            ! ATC: year-Beginning of Major Frame
     +          DR_Beg_DOY,             ! ATC: doy -Beginning of Major Frame
     +          DR_Beg_MSec,            ! ATC: msec-Beginning of Major Frame
     +          DR_Beg_MicSec,          ! ATC: usec-Beginning of Major Frame
     +          DR_Num_MinFr_Fill,      ! Number of Minor Frames with Fill
     +          DR_Num_MinFr_Sync_Err,  ! Number of Minor Frames with Sync_Err
     +          DR_TLM_Mode,            ! Telemetry Mode Indicator
                                                ! 1 = science mode
                                                ! 2 = engineering mode
                                                ! 3 = maneuver mode
                                                ! 4 = contingency mode
     +          DR_MinFr_Qual,          ! Minor Frame Quality
                                                ! bit 0 = frame sync error flag
                                                ! bit 1 = frame counter error fllag
                                                ! bit 2 = fill frame flag
                                                ! bit 3-31 spares
     +          DR_Fill,                ! adjustment to Longword
     +          DR_Beg_EPOCH            ! EPOCH time-Beginning of Major Frame ?

  132 FORMAT( 1H1,
     +  /,      ' DR_Instr_Num          =', I6,         ! Instrument Number
     +  /,      ' DR_Phys_Rec_Num       =', I6,         ! Physical Record Number within File
     +  /,      ' DR_Beg_MajFr_Cnt      =', I6,         ! Major Frame Count-Beginning of Maj.Fra
     +  /,      ' DR_Beg_SC_Clock       =', 2Z8.8       ! Spacecraft Clock -Beginning of Maj.Fra
     +  /,      ' DR_Beg_Year           =', I6,         ! ATC: year-Beginning of Major Frame
     +  /,      ' DR_Beg_DOY            =', I6,         ! ATC: doy -Beginning of Major Frame
     +  /,      ' DR_Beg_MSec           =', I9,         ! ATC: msec-Beginning of Major Frame
     +  /,      ' DR_Beg_MicSec         =', I6,         ! ATC: usec-Beginning of Major Frame
     +  /,      ' DR_Num_MinFr_Fill     =', I6,         ! Number of Minor Frames with Fill
     +  /,      ' DR_Num_MinFr_Sync_Err =', I6,         ! Number of Minor Frames with Sync_Err
     +  /,      ' DR_TLM_Mode           =', I6,         ! Telemetry Mode Indicator
                        ! WIND          ! 1 = science mode      - High Bitrate
                                        ! 2 = engineering mode  - High Bitrate
                                        ! 3 = maneuver mode     - High Bitrate
                                        ! 4 = contingency mode  - High Bitrate
                                        ! 5 = science mode      - Normal Bitrate
                                        ! 6 = engineer mode     - Normal Bitrate
                                        ! 7 = maneuver mode     - Normal Bitrate
                                        ! 8 = contingency mode  - Normal Bitrate
                                        ! 128 = Transitional mode
                                        ! 256 = Unknown mode
     +  /,      ' DR_MinFr_Qual =', 25( /, 10Z3.2 ),    ! Minor Frame Quality
                                                ! bit 0 = frame sync error flag
                                                ! bit 1 = frame counter error fllag
                                                ! bit 2 = fill frame flag
                                                ! bit 3-31 spares
     +  /,      ' DR_Fill       =', 2Z3.2,              ! adjustment to Longword
     +  /,      ' DR_Beg_EPOCH  =' F22.5 )              ! EPOCH time-Beginning of Major Frame ?

      IF(DIAG .LE. 1) WRITE( IPRT, 134 )  ((II, (DR_MinFr_SMS( JJ, II ), JJ = 1,42) , II = 0,249))
  134 FORMAT( 250( /, I5, 5X, 2Z3.2, 2( /, 10Z3.2, 3X, 10Z3.2 ) ) )

C
C       Find start of EDB or HDB. Byte 0 = 14h, byte 1 = 6Fh.
C
C       Find Start of first Data Block  -  EDB or HDB
C
      JJ = 0
C
 1400 IF(DIAG .LE. 3) WRITE( IPRNT, 140 ) ' OLD ', MSpin, JJ
  140 FORMAT( A, 'MSpin =', I3.2, ' and MinFr =', I4.3 )
      IF( JJ .GE. MAXSFrame )   GOTO 1200
C
C Step through subframes until we find one whose first three bytes contain the
C sync flag (byte 0 = 14h, byte 1 = 6Fh, byte 2 is nonzero).
C
C 1st subscript to DP_MinFr_SMS is a byte offset, but 3 greater than in
C documentation
C 2nd subscript to DP_MinFr_SMS (variable JJ) is a minor frame (subframe) index
C
      SYNC_WORD=.FALSE.
      DO WHILE ( .NOT.( ( DR_MinFr_SMS( 3, JJ ) .EQ. '14'X ) .AND.
     +                  ( DR_MinFr_SMS( 4, JJ ) .EQ. '6F'X ) .AND.
     +                  ( DR_MinFr_SMS( 5, JJ ) .NE. '00'X ) ) )
        JJ = JJ + 1
        IF( JJ .GE. MAXSFrame )   THEN
            GOTO 1200
        ENDIF         
      END DO
C
      SYNC_WORD=.TRUE.
C
      IF(DIAG .LE. 3) WRITE( IPRNT, 140 ) ' OLD ', MSpin, JJ
      EDB_done = .FALSE.

!      Collect characteristic properties from CORE data

      TEMP2 = DR_MinFr_SMS( 5, JJ )                             ! BYTE 2

      IF(DIAG .LE. 1) WRITE( TTOUT, 150 )  ( DR_MinFr_SMS( IJ, JJ ), IJ = 3, 13 )
  150 FORMAT( 1H , 11Z5.3 )

C Get bit rate, HDB flag, and number of subframes per block
C Set Bytes_SFrame based on BitRate

      BitRate = BTEST( TEMP2, 7 )                       ! BitRate
      IF( BTEST( TEMP2, 6 ) ) THEN                      ! Byte/SF
          Bytes_SFrame = 40
      ELSE
          Bytes_SFrame = 33
      ENDIF
      HDBFlag = BTEST( TEMP2, 5 )                       ! Block Mode
      SFperBlock = IAND( TEMP2, '1F'X )                 ! Subframes per Block

C Go to different sections of cade based on what type of block this is (EDB or
C HDB) as they are quite different in format and content

      IF( .NOT. HDBFlag )  THEN                         ! this is EDB
          O_MSpin   = MSpin
          O_SpinCnt = SpinCount

          EDB_Sum = EDB_Sum + 1

!      Interpret CORE data
          MSpin         = 0
          TEMP2         = DR_MinFr_SMS( 6, JJ )                 ! BYTE 3
          CALL  MVBITS( TEMP2, 2, 6, MSpin, 0 )         ! Measured Spin
          RAMCheck      = BTEST( TEMP2, 1 )             ! RAMCheck MODE
          MASSPower     = BTEST( TEMP2, 0 )             ! MASSPower ON
C
C The code once apparently assumed that the first value of MSpin in the first EDB would 
C never be 0. The code that follows allows for that possibility.
C
          IF( FirstEDB ) THEN
            O_MSpin = MSpin - 1
            IF(MSpin .EQ. 0)O_MSpin = 59
          ENDIF

!          IF( ( MSpin .LE. O_MSpin ) .AND.
!     +         ( O_MSpin .NE. 59 ) ) GO TO 2500        ! OR CALL SUB TO SAVE SCI-REC

          SCI( EDBset ).EDB( MSpin ).SYNC(1)    = DR_MinFr_SMS( 3, JJ )
          SCI( EDBset ).EDB( MSpin ).SYNC(2)    = DR_MinFr_SMS( 4, JJ )
          SCI( EDBset ).EDB( MSpin ).Qual       = DR_MinFr_Qual( JJ )
          SCI( EDBset ).EDB( MSpin ).BitRate    = BitRate
          SCI( EDBset ).EDB( MSpin ).HDB_FLAG   = HDBFlag
          SCI( EDBset ).EDB( MSpin ).MeaSpin    = MSpin
          SCI( EDBset ).EDB( MSpin ).BYTE_SF    = Bytes_SFrame
          SCI( EDBset ).EDB( MSpin ).SFperBlock = SFperBlock
          SCI( EDBset ).EDB( MSpin ).RAMCheck   = RAMCheck
          SCI( EDBset ).EDB( MSpin ).MASSPower  = MASSPower
C
C Added this to CORE structure
C
          SCI( EDBset ).EDB( MSpin ).TLM_Mode   = DR_TLM_Mode

          IF(DIAG .LE. 3) WRITE( IPRNT, 140 ) ' NEW ', MSpin, JJ
          TEMP2         = DR_MinFr_SMS(  7, JJ )                ! BYTE 4  - Cont. Spin Counter
          SpinCount     = IAND( TEMP2, 'FF'X )
          SCI( EDBset ).EDB( MSpin ).SpinCnt = SpinCount

          IF( FirstEDB ) O_SpinCnt = SpinCount - 1
          IF( MSpin .NE. MOD( O_MSpin+1, 60 ) ) THEN
              IF(DIAG .LE. 4) THEN
                  WRITE( IPRNT, * )   ' MISSING SPIN!  SCI_rec=',Sci_rec_cntr,', MSpin=',MSpin,', JJ=',JJ
                  WRITE( IPRNT, 900 )   '   MSpin =', SCI( EDBset ).EDB( MSpin ).MeaSpin
                  WRITE( IPRNT, 900 )   ' O_MSpin =', O_MSpin
                  WRITE( IPRNT, 900 )   'SpinCount=', SCI( EDBset ).EDB( MSpin ).SpinCnt
                  WRITE( IPRNT, 900 )   'O_SpinCnt=', O_SpinCnt
              ENDIF
              IF( (MSpin .EQ. 59) .AND.
     :          ((( O_SpinCnt+1 ) .AND. MAXSpCnt) .EQ. SpinCount) ) THEN
                  SR_Qual = 8                                   ! Skipped Spins
                  DO II = O_MSpin+1, 58
                      SCI( EDBset ).EDB( II ).Qual = 8
                  END DO
                  EDB_Skip = EDB_Skip + 58 - O_MSpin
                  O_MSpin = 58
              END IF
          END IF
C
C The last good spin was O_MSpin, and the current (good) spin is Mspin.  Mark
C all spins between the two as missing (qual flag 8).
C There are two spin counters: MSpin and SpinCount.  MSpin is a Science Record
C spin counter and goes from 0 to 59.  SpinCount is a revolution (spin) counter
C which continues to increase and, being a byte quantity, resets every 256
C spins.  Its value is in the range 0 to 255.
C Two loops are used because, if exactly 60 spins are missing, Mspin will equal
C O_MSpin+1, giving the mistaken impression no spins are missing.  Likewise, if
C exactly 256 spins are missing, SpinCount will be equal to O_SpinCnt+1.
C
C This code currently has a bug.  If Mspin .GT. O_MSpin+1 it works okay, but if
C MSpin .LT. O_MSpin+1 it marks ALL spins (from O_MSpin+1 through 59 and from
C 0 through MSpin-1) of the CURRENT EDB as missing.
C
          DO WHILE ( MSpin .NE. MOD( O_MSpin+1, 60 ) )
              O_MSpin = MOD( O_MSpin+1, 60 )
              IF( (( O_SpinCnt+1 ) .AND. MAXSpCnt) .NE. SpinCount ) O_SpinCnt = ( O_SpinCnt+1 ) .AND. MAXSpCnt
              SCI( EDBset ).EDB( O_MSpin ).Qual = 8
              EDB_Lost = EDB_Lost + 1
              IF(DIAG .LE. 3) WRITE( IPRNT, 900 )  ' EDB_Lost =', EDB_Lost
          END DO ! ( MSpin .NE. MOD( O_MSpin+1, 60 ) )

          DO WHILE ( SpinCount .NE. ( O_SpinCnt+1 .AND. MAXSpCnt ) )
              IF( MSpin .NE. MOD( O_MSpin+1, 60 ) ) O_MSpin = MOD( O_MSpin+1, 60 )
              O_SpinCnt = ( O_SpinCnt+1 ) .AND. MAXSpCnt
              SCI( EDBset ).EDB( O_MSpin ).Qual = 8
              EDB_Lost = EDB_Lost + 1
          END DO ! ( SpinCount .NE. ( O_MSpinCnt+1 .AND. MAXSpCnt ) )


          SCI( EDBset ).EDB( MSpin ).SWICS_Len = 0
          SCI( EDBset ).EDB( MSpin ).SWICSPower = .FALSE.
          TEMP2         = DR_MinFr_SMS(  8, JJ )                ! BYTE 5
          TEMP2         = TEMP2 .AND. 'FF'X
          IF( TEMP2 .NE. 255 ) THEN
              SCI( EDBset ).EDB( MSpin ).SWICS_Len  = TEMP2
C
C Check to see if # of PHA words makes sense.
C
              IF(SCI( EDBset ).EDB( MSpin ).SWICS_Len.GT.168)THEN
                TYPE 155, SCI( EDBset ).EDB( MSpin ).SWICS_Len
 155            FORMAT(' *** Too many SWICS PHA words, value is ',I4,' maximum possible value is 168'/)
                SCI( EDBset ).EDB( MSpin ).SWICS_Len = 168
              ENDIF
              SCI( EDBset ).EDB( MSpin ).SWICSPower = .TRUE.
          ENDIF


          SCI( EDBset ).EDB( MSpin ).STICS_Len  = 0
          SCI( EDBset ).EDB( MSpin ).STICSPower = .FALSE.
          TEMP2         = DR_MinFr_SMS(  9, JJ )                ! BYTE 6
          TEMP2         = TEMP2 .AND. 'FF'X
          IF( TEMP2 .NE. 255 ) THEN
              SCI( EDBset ).EDB( MSpin ).STICS_Len  = TEMP2
C
C Check to see if # of PHA words makes sense.
C
              IF(SCI( EDBset ).EDB( MSpin ).STICS_Len.GT.152)THEN
                TYPE 156, SCI( EDBset ).EDB( MSpin ).STICS_Len
 156            FORMAT(' *** Too many STICS PHA words, value is ',I4,' maximum possible value is 152'/)
                SCI( EDBset ).EDB( MSpin ).STICS_Len = 152
              ENDIF
              SCI( EDBset ).EDB( MSpin ).STICSPower = .TRUE.
          ENDIF


          TEMP2 = DR_MinFr_SMS( 10, JJ )                        ! BYTE 7
          SCI( EDBset ).EDB( MSpin ).ValCmdFl   = BTEST( TEMP2, 7 )
          SCI( EDBset ).EDB( MSpin ).InvCmdFl   = BTEST( TEMP2, 6 )
          SCI( EDBset ).EDB( MSpin ).CmdErrFl   = BTEST( TEMP2, 5 )
          SCI( EDBset ).EDB( MSpin ).MASS_CP    = BTEST( TEMP2, 4 )
          SCI( EDBset ).EDB( MSpin ).SWICS_HP   = BTEST( TEMP2, 3 )
          SCI( EDBset ).EDB( MSpin ).SWICS_CP   = BTEST( TEMP2, 2 )
          SCI( EDBset ).EDB( MSpin ).STICS_HP   = BTEST( TEMP2, 1 )
          SCI( EDBset ).EDB( MSpin ).STICS_CP   = BTEST( TEMP2, 0 )

          TEMP2         = DR_MinFr_SMS( 11, JJ )                ! BYTE 8
          TEMP2         = TEMP2 .AND. 'FF'X
          SCI( EDBset ).EDB( MSpin ).MASS_Len = ISHFT( TEMP2, 1 )

          TEMP2         = DR_MinFr_SMS( 12, JJ )                ! BYTE 9
          CALL MVBITS( TEMP2, 7, 1, SCI( EDBset ).EDB( MSpin ).MASS_Len, 0 )    ! Number of PHA-words
C
C Check to see if # of PHA words makes sense.
C
          IF(SCI( EDBset ).EDB( MSpin ).MASS_Len.GT.381)THEN
            TYPE 157, SCI( EDBset ).EDB( MSpin ).MASS_Len
 157        FORMAT(' *** Too many MASS PHA words, value is ',I4,' maximum possible value is 381'/)
            SCI( EDBset ).EDB( MSpin ).MASS_Len = 381
          ENDIF

          SCI( EDBset ).EDB( MSpin ).MDisChar   = BTEST( TEMP2, 6 )
          SCI( EDBset ).EDB( MSpin ).DeadTime   = BTEST( TEMP2, 5 )
          SCI( EDBset ).EDB( MSpin ).WDisChar   = BTEST( TEMP2, 4 )
          SCI( EDBset ).EDB( MSpin ).SpareTime  = TEMP2 .AND. '0F'X


          TEMP2         = DR_MinFr_SMS( 13, JJ )                ! BYTE 10

          SCI( EDBset ).EDB( MSpin ).MHStep     = BTEST( TEMP2, 7 )
          SCI( EDBset ).EDB( MSpin ).MSPStep    = BTEST( TEMP2, 6 )
          SCI( EDBset ).EDB( MSpin ).MSTStep    = BTEST( TEMP2, 5 )
          SCI( EDBset ).EDB( MSpin ).WPStep     = BTEST( TEMP2, 4 )
          SCI( EDBset ).EDB( MSpin ).WSPStep    = BTEST( TEMP2, 3 )
          SCI( EDBset ).EDB( MSpin ).WSTStep    = BTEST( TEMP2, 2 )
          SCI( EDBset ).EDB( MSpin ).SSPStep    = BTEST( TEMP2, 1 )
          SCI( EDBset ).EDB( MSpin ).SSTStep    = BTEST( TEMP2, 0 )

          EDB_Start = JJ

!      Display for testing some of CORE data contents
          IF(DIAG .LE. 3) WRITE( TTOUT, 160 )  MSpin, EDB_Start
  160 FORMAT( ' Start of EDB', /,
     +        ' MeaSpin = ', I8.2, ' starts at SubFr.', I3 )
          IF( BitRate ) THEN
              IF(DIAG .LE. 2) WRITE( IPRNT, 165 )  ' TLM is in HIGH BitRate'
              SWICS_RATE_Size   = SWICS_H_RATE_Size     ! MAX. Index
              STICS_RATE_Size   = STICS_H_RATE_Size     ! MAX. Index
              DELTATime         = 184.D0
          ELSE
              IF(DIAG .LE. 2) WRITE( IPRNT, 165 )  ' TLM is in NORMAL BitRate'
              SWICS_RATE_Size = SWICS_N_RATE_Size       ! MAX. Index
              STICS_RATE_Size = STICS_N_RATE_Size       ! MAX. Index
              IF( .not.OBitRate ) DELTATime = 368.D0
          ENDIF
  165 FORMAT( 1H , A )
          SCI( EDBset ).EDB( MSpin ).SWICS_RATE_Size = SWICS_RATE_Size
          SCI( EDBset ).EDB( MSpin ).STICS_RATE_Size = STICS_RATE_Size

          IF(DIAG .LE. 2) WRITE( IPRNT, 166 ) DR_TLM_MODE
  166 FORMAT( ' DR_TLM_MODE =', I2 )
          IF(DIAG .LE. 3) THEN
              WRITE( TTOUT, 170 )   SCI( EDBset ).EDB( MSpin ).BYTE_SF
  170 FORMAT( ' There are ', I2, ' Bytes per Subframe' )
              WRITE( TTOUT, 175 )   SCI( EDBset ).EDB( MSpin ).SFperBlock
  175 FORMAT( ' There are ', I2, ' Subframes per Block' )
              WRITE( TTOUT, 180 )   SCI( EDBset ).EDB( MSpin ).SpinCnt
  180 FORMAT( ' Current Spin Count is ', I3 )
          ENDIF

!      Maintain Quality Flags for EDBs
          IF( FirstEDB )   THEN
              IF( MSpin .NE. 0 )   THEN
                  DO II = 0, MSpin
                      SCI( EDBset ).EDB( II ).Qual              = 4     ! FILL indicator
                      SCI( EDBset ).EDB( II ).CORE_Qual         = 8
                      SCI( EDBset ).EDB( II ).SW_BR_Qual        = 8
                      SCI( EDBset ).EDB( II ).SW_MR_Qual        = 8
                      SCI( EDBset ).EDB( II ).SW_ME1_Qual       = 8
                      SCI( EDBset ).EDB( II ).SW_ME2_Qual       = 8
                      SCI( EDBset ).EDB( II ).SW_ER_Qual        = 8
                      SCI( EDBset ).EDB( II ).SW_P_Qual         = 8
                      SCI( EDBset ).EDB( II ).ST_HMR_Qual       = 8
                      SCI( EDBset ).EDB( II ).ST_SMR_Qual       = 8
                      SCI( EDBset ).EDB( II ).ST_BR_Qual        = 8
                      SCI( EDBset ).EDB( II ).ST_OMR_Qual       = 8
                      SCI( EDBset ).EDB( II ).ST_ER_Qual        = 8
                      SCI( EDBset ).EDB( II ).ST_P_Qual         = 8
                      SCI( EDBset ).EDB( II ).MA_UER_Qual       = 8
                      SCI( EDBset ).EDB( II ).MA_SER_Qual       = 8
                      SCI( EDBset ).EDB( II ).MA_BR_Qual        = 8
                      SCI( EDBset ).EDB( II ).MA_MR_Qual        = 8
                      SCI( EDBset ).EDB( II ).MA_P_Qual         = 8
                  ENDDO
              ENDIF
              FirstEDB = .FALSE.
              IF(DIAG .LE. 3) WRITE( TTOUT, 165 )   ' First EDBs indicated as FILL '
          ENDIF

!  MOVE RawData / SubFrame to EDB
          IF( MSpin .EQ. 0 ) RECFLAG = RECFLAG + 1D0
          T1 = (JJ * 1D0) * DELTATime - DELTAT(MSpin)

          SCI( EDBset ).EDB( MSpin ).EPOCH = DR_Beg_EPOCH + T1
          CALL EPOCH_TO_ITIME( SCI(EDBset).EDB(MSpin).EPOCH, 
     :                         SCI(EDBset).EDB(MSpin).W_Time, ISTAT )
          IF( (SCI( EDBset ).EDB( MSpin ).EPOCH - SCI( EDBset ).EDB( O_MSpin ).EPOCH) .GT. 4.D3 ) THEN
              IF(DIAG .LE. 2) THEN
                  WRITE( TTOUT, 191 ) MSpin, SCI( EDBset ).EDB( MSpin ).EPOCH
                  WRITE( TTOUT, 191 ) O_MSpin, SCI( EDBset ).EDB( O_MSpin ).EPOCH
                  WRITE( TTOUT, 190 ) JJ, DELTATime
  190 FORMAT( ' MinF', I3.3, '=', F22.3, ' ms', / )
              END IF
          END IF

          SCI( EDBset ).EDB( MSpin ).SC_EPOCH = DR_SC_EPOCH + T1
          CALL EPOCH_TO_ITIME( SCI(EDBset).EDB(MSpin).SC_EPOCH, 
     :                         SCI(EDBset).EDB(MSpin).SC_TIME, ISTAT )

          IF( MSpin .EQ. 0 ) THEN
              IF(DIAG .LE. 3) WRITE( IPRNT, 191 ) MSpin, SCI( EDBset ).EDB( MSpin ).EPOCH
  191 FORMAT( ' EDB', I2.2, ' =', F22.3, ' ms' )
              IF(DIAG .LE. 2) WRITE( IPRNT, 131 ) SRCount, MSpin,
     :          SCI(EDBset).EDB(MSpin).W_Time.YEAR,
     :          SCI(EDBset).EDB(MSpin).SC_Time.YEAR,
     :          SCI(EDBset).EDB(MSpin).W_Time.DOY,
     :          SCI(EDBset).EDB(MSpin).SC_Time.DOY,
     :          SCI(EDBset).EDB(MSpin).W_Time.HR,
     :          SCI(EDBset).EDB(MSpin).SC_Time.HR,
     :          SCI(EDBset).EDB(MSpin).W_Time.MIN,
     :          SCI(EDBset).EDB(MSpin).SC_Time.MIN,
     :          SCI(EDBset).EDB(MSpin).W_Time.SEC,
     :          SCI(EDBset).EDB(MSpin).SC_Time.SEC,
     :          SCI(EDBset).EDB(MSpin).W_Time.CSEC,
     :          SCI(EDBset).EDB(MSpin).SC_Time.CSEC
          END IF ! ( MSpin .EQ. 0 )

          IF( lSCTime ) THEN
              DYRDAY = DR_SC_EPOCH + T1
          ELSE
              DYRDAY = DR_Beg_EPOCH + T1
          END IF

          SCI( EDBset ).EDB( MSpin ).Qual = 0           ! Clear EDB Quality Flag
          EDBpos = 0
          ASSIGN 2020 TO LOAD_EDB

 2000     DO WHILE ( .NOT.EDB_done )
              IF( JJ .GE. MAXSFrame ) THEN
                  JJ = 0
                  IF( .NOT.BUFFER_READ ) THEN
C                      READ( L0_Lun, 400, END=9999, ERR=9998) IREC,( DR2_BUFFER(II), II=1,IREC )
                      READ( L0_Lun, 401, END=9999, ERR=9998) IREC,C_DR2_BUFFER(1:IREC)

                      CALL FIXUP( DR_Beg_SC_Clock )

                      BUFFER_READ = .TRUE.
                  ENDIF

                  C_DR_Buffer   = C_DR2_Buffer
                  OBitRate      = BitRate
                  BUFFER_READ   = .FALSE.
                  IF(DIAG .LE. 3) WRITE( IPRNT, 120 )   ' SHIFTED2', DR_Phys_Rec_Num, DR_Beg_MajFr_Cnt

                  CALL WIND_PB5_TO_EPOCH(       DR_Beg_SC_Clock,! UTC: PB5 (2 INTEGER*4)
     +                                          DR_SC_EPOCH,    ! UTC: EPOCH as Real*8
     +                                          ISTAT )
                  IF( ISTAT .NE. SS$_NORMAL ) CALL EXIT( ISTAT )

                  CALL WIND_EPOCH_TO_GREG(      SC_Beg_Year,
     :                                          SC_Beg_DOY,
     :                                          SC_Beg_Msec,
     :                                          SC_Beg_MicSec,
     +                                          DR_SC_EPOCH,    ! UTC: EPOCH as Real*8
     +                                          ISTAT )
                  IF( ISTAT .NE. SS$_NORMAL ) CALL EXIT( ISTAT )

                  CALL WIND_GREG_TO_EPOCH( DR_Beg_Year, ! ATC: Year
     +                                  DR_Beg_DOY,     ! ATC: Day Of Year
     +                                  DR_Beg_MSec,    ! ATC: Millisecond of DAY
     +                                  DR_Beg_MicSec,  ! ATC: Microsecond of MSec
     +                                  DR_Beg_EPOCH,   ! ATC: EPOCH AS REAL*8
     +                                  ISTAT )
                  IF( ISTAT .NE. SS$_NORMAL ) CALL EXIT( ISTAT )

                  IF( lSCTime ) THEN
                      DYRDAY = DR_SC_EPOCH
                  ELSE
                      DYRDAY = DR_Beg_EPOCH
                  END IF
                  Pref_EPOCH = DYRDAY

                  CALL WIND_PRINT_HK( DR_Beg_MajFr_Cnt, HKset(EDBset), SYNC_WORD )

                  IF(DIAG .LE. 2) WRITE( IPRNT, 130 ) DR_Phys_Rec_Num, DR_Beg_MajFr_Cnt,
     +                          DR_Beg_Year,    SC_Beg_Year,    ! Year
     +                          DR_Beg_DOY,     SC_Beg_DOY,     ! Day Of Year
     +                          DR_Beg_MSec,    SC_Beg_Msec,    ! Millisecond of DAY
     +                          DR_Beg_MicSec,  SC_Beg_MicSec,  ! Microsecond of MSec
     +                          DR_Beg_EPOCH,   DR_SC_EPOCH     ! EPOCH AS REAL*8

                  IF(DIAG .LE. 2) WRITE( IPRT, 132 )
     +          DR_Instr_Num,           ! Instrument Number
     +          DR_Phys_Rec_Num,        ! Physical Record Number within File
     +          DR_Beg_MajFr_Cnt,       ! Major Frame Count-Beginning of Maj.Fra
     +          DR_Beg_SC_Clock,        ! Spacecraft Clock -Beginning of Maj.Fra
     +          DR_Beg_Year,            ! ATC: year-Beginning of Major Frame
     +          DR_Beg_DOY,             ! ATC: doy -Beginning of Major Frame
     +          DR_Beg_MSec,            ! ATC: msec-Beginning of Major Frame
     +          DR_Beg_MicSec,          ! ATC: usec-Beginning of Major Frame
     +          DR_Num_MinFr_Fill,      ! Number of Minor Frames with Fill
     +          DR_Num_MinFr_Sync_Err,  ! Number of Minor Frames with Sync_Err
     +          DR_TLM_Mode,            ! Telemetry Mode Indicator
                                                ! 1 = science mode
                                                ! 2 = engineering mode
                                                ! 3 = maneuver mode
                                                ! 4 = contingency mode
     +          DR_MinFr_Qual,          ! Minor Frame Quality
                                                ! bit 0 = frame sync error flag
                                                ! bit 1 = frame counter error fllag
                                                ! bit 2 = fill frame flag
                                                ! bit 3-31 spares
     +          DR_Fill,                ! adjustment to Longword
     +          DR_Beg_EPOCH            ! EPOCH time-Beginning of Major Frame ?

                  IF(DIAG .LE. 1) WRITE( IPRT, 134 )            ! One Major Frame
     :                 ((II, (DR_MinFr_SMS( IJ, II ), IJ = 1,42) , II = 0,249))

                  C_DR2_Buffer = C_NULL_Buffer
C                  READ( L0_Lun, 400, END=9999, ERR=9998) IREC,( DR2_BUFFER(II), II=1,IREC )
                  READ( L0_Lun, 401, END=9999, ERR=9998) IREC,C_DR2_BUFFER(1:IREC)

                  CALL FIXUP( DR_Beg_SC_Clock )

                  IF(DIAG .LE. 2) WRITE( TTOUT, 118 )   ' 3rd READ', DR2_Phys_Rec_Num, DR2_Beg_MajFr_Cnt

                  BUFFER_READ = .TRUE.
                  IF(DIAG .LE. 3) WRITE( TTOUT, 165 )   ' Next Major Frame read'

              ENDIF ! ( JJ .GE. MAXSFrame )

              IF( DR_MinFr_Qual( JJ ) .NE. 0 )   THEN
                  SCI( EDBset ).EDB( MSpin ).Qual = SCI( EDBset ).EDB( MSpin ).Qual
     +                                  .OR. 2 .OR. DR_MinFr_Qual( JJ )
              ENDIF

              GOTO LOAD_EDB

 2020 CONTINUE
              IF( DR_MinFr_Qual( JJ ) .NE. 0 )   THEN
                  SCI( EDBset ).EDB( MSpin ).CORE_Qual = 4
                  DO II = 0, EDB_Core_Size
                      CORE_data( II, MSpin, EDBset ) = 0
                      EDBpos = EDBpos + 1
                  ENDDO
              ELSE
                  SCI( EDBset ).EDB( MSpin ).CORE_Qual = 0
                  DO II = 0, EDB_Core_Size
                      CORE_data( II, MSpin, EDBset ) = DR_MinFr_SMS( 3 + II, JJ )
                      EDBpos = EDBpos + 1
                  ENDDO
                  IF(DIAG .LE. 5) WRITE( TTOUT, 165 )   ' CORE data transferred '
                  ATIME=WIND_FORMAT_TIME( SCI(EDBset).EDB(MSpin).W_Time)
                  WRITE(28,2025) MSPIN, ATIME, (CORE_data( II, MSpin, EDBset ) ,II=0,EDB_Core_Size )
 2025             FORMAT(' EDB Header for spin',I3,A30,/,1X,<EDB_Core_Size + 1>Z3.2)
                  IF(DIAG .LE. 3) WRITE( IPRNT, 206 )  'CORE', II, MSpin, EDBset, EDBpos, JJ
  206 FORMAT( 1H , A, ' POINTER:', I4, '  MSpin:', I3, '  EDBset:', I3,
     :          '  EDBpos:', I4, '  JJ:', I3 )

                  SCI( EDBset ).EDB( MSpin ).SW_BR_Qual         = 8
                  SCI( EDBset ).EDB( MSpin ).SW_MR_Qual         = 8
                  SCI( EDBset ).EDB( MSpin ).SW_ME1_Qual        = 8
                  SCI( EDBset ).EDB( MSpin ).SW_ME2_Qual        = 8
                  SCI( EDBset ).EDB( MSpin ).SW_ER_Qual         = 8
                  SCI( EDBset ).EDB( MSpin ).SW_P_Qual          = 8
!   Check IF SWICS is ON
                  SWICSPower = SCI( EDBset ).EDB( MSpin ).SWICSPower
                  IF( SWICSPOWER ) THEN                         ! Move SWICS-RATES
                      SCI( EDBset ).EDB( MSpin ).SW_BR_Qual     = DR_MinFr_Qual( JJ )
                      SCI( EDBset ).EDB( MSpin ).SW_MR_Qual     = DR_MinFr_Qual( JJ )
                      SCI( EDBset ).EDB( MSpin ).SW_ME1_Qual    = DR_MinFr_Qual( JJ )
                      SCI( EDBset ).EDB( MSpin ).SW_ME2_Qual    = DR_MinFr_Qual( JJ )
                      SCI( EDBset ).EDB( MSpin ).SW_ER_Qual     = DR_MinFr_Qual( JJ )
                      POINTER = 0
                      ASSIGN 2040 TO LOAD_EDB
 2040 CONTINUE
                      SWICS_RATE( EDBset ).EDB( MSpin ).RATE( POINTER ) = DR_MinFr_SMS( EDBpos + 3, JJ )

                      IF( EDBpos .EQ. 0 ) THEN
                            SCI( EDBset ).EDB( MSpin ).SW_MR_Qual 
     :                    = SCI( EDBset ).EDB( MSpin ).SW_MR_Qual  .OR. DR_MinFr_Qual( JJ )
                            SCI( EDBset ).EDB( MSpin ).SW_ME1_Qual
     :                    = SCI( EDBset ).EDB( MSpin ).SW_ME1_Qual .OR. DR_MinFr_Qual( JJ )
                            SCI( EDBset ).EDB( MSpin ).SW_ME2_Qual
     :                    = SCI( EDBset ).EDB( MSpin ).SW_ME2_Qual .OR. DR_MinFr_Qual( JJ )
                            SCI( EDBset ).EDB( MSpin ).SW_ER_Qual 
     :                    = SCI( EDBset ).EDB( MSpin ).SW_ER_Qual  .OR. DR_MinFr_Qual( JJ )
                      END IF

                      EDBpos = EDBpos + 1
                      POINTER = POINTER + 1

                      IF( EDBpos .GE. Bytes_SFrame )   THEN
                          EDBpos = 0
                          JJ = JJ + 1
                          IF( POINTER .LE. SWICS_RATE_Size )   GOTO 2000
                      ELSEIF( POINTER .LE. SWICS_RATE_Size )   THEN
                          GOTO LOAD_EDB
                      ENDIF

                      IF(DIAG .LE. 5) WRITE( TTOUT, 165 )   ' SWICS RATES transferred '
                      WRITE(28,2045) MSPIN,SWICS_RATE( EDBset ).EDB( MSpin ).RATE
 2045                 FORMAT(' SWICS Rates for spin',I3,/,(1X,20Z3.2))
                  ENDIF


                  IF( SWICSPower .AND. SCI( EDBset ).EDB( MSpin ).SWICS_Len .GT. 0 ) THEN       ! Move SWICS-PHA
                      SCI( EDBset ).EDB( MSpin ).SW_P_Qual = DR_MinFr_Qual( JJ )
                      POINTER = 0
                      ASSIGN 2050 TO LOAD_EDB
 2050 CONTINUE
                      SWICS_PHA( EDBset ).EDB( MSpin ).PHA( POINTER ) = DR_MinFr_SMS( EDBpos + 3, JJ )

                      IF( EDBpos .EQ. 0 ) THEN
                            SCI( EDBset ).EDB( MSpin ).SW_P_Qual
     :                    = SCI( EDBset ).EDB( MSpin ).SW_P_Qual .OR. DR_MinFr_Qual( JJ )
                      END IF

                      EDBpos = EDBpos + 1
                      POINTER = POINTER + 1

                      IF( EDBpos .GE. Bytes_SFrame )   THEN
                          EDBpos = 0
                          JJ = JJ + 1
                          IF( POINTER .LT. SCI( EDBset ).EDB( MSpin ).SWICS_Len*4 )   GOTO 2000
                      ELSEIF( POINTER .LT. SCI( EDBset ).EDB( MSpin ).SWICS_Len*4 )   THEN
                          GOTO LOAD_EDB
                      ENDIF

                      IF(DIAG .LE. 5) WRITE( TTOUT, 165 )   ' SWICS PHA transferred '
                      WRITE(28,2055) MSPIN, SCI( EDBset ).EDB( MSpin ).SWICS_Len, 
     &                               (SWICS_PHA( EDBset ).EDB( MSpin ).PHA( ijk),ijk=0,4*SCI( EDBset ).EDB( MSpin ).SWICS_Len - 1)
 2055                 FORMAT(' SWICS PHA for spin',I3,'          # words =',I5,/,(1X,20Z3.2))
                  ENDIF

                  SCI( EDBset ).EDB( MSpin ).ST_HMR_Qual        = 8
                  SCI( EDBset ).EDB( MSpin ).ST_SMR_Qual        = 8
                  SCI( EDBset ).EDB( MSpin ).ST_BR_Qual         = 8
                  SCI( EDBset ).EDB( MSpin ).ST_OMR_Qual        = 8
                  SCI( EDBset ).EDB( MSpin ).ST_ER_Qual         = 8
                  SCI( EDBset ).EDB( MSpin ).ST_P_Qual          = 8
!   Check IF STICS is ON
                  STICSPower = SCI( EDBset ).EDB( MSpin ).STICSPower
                  IF( STICSPower ) THEN                         ! Move STICS-RATE
                      SCI( EDBset ).EDB( MSpin ).ST_HMR_Qual    = DR_MinFr_Qual( JJ )
                      SCI( EDBset ).EDB( MSpin ).ST_SMR_Qual    = DR_MinFr_Qual( JJ )
                      SCI( EDBset ).EDB( MSpin ).ST_BR_Qual     = DR_MinFr_Qual( JJ )
                      SCI( EDBset ).EDB( MSpin ).ST_OMR_Qual    = DR_MinFr_Qual( JJ )
                      SCI( EDBset ).EDB( MSpin ).ST_ER_Qual     = DR_MinFr_Qual( JJ )
                      POINTER = 0
                      ASSIGN 2060 TO LOAD_EDB
 2060 CONTINUE
                      STICS_RATE( EDBset ).EDB( MSpin ).RATE( POINTER ) = DR_MinFr_SMS( EDBpos + 3, JJ )

                      IF( EDBpos .EQ. 0 ) THEN
                            SCI( EDBset ).EDB( MSpin ).ST_HMR_Qual
     :                    = SCI( EDBset ).EDB( MSpin ).ST_HMR_Qual .OR. DR_MinFr_Qual( JJ )
                            SCI( EDBset ).EDB( MSpin ).ST_SMR_Qual
     :                    = SCI( EDBset ).EDB( MSpin ).ST_SMR_Qual .OR. DR_MinFr_Qual( JJ )
                            SCI( EDBset ).EDB( MSpin ).ST_BR_Qual 
     :                    = SCI( EDBset ).EDB( MSpin ).ST_BR_Qual  .OR. DR_MinFr_Qual( JJ )
                            SCI( EDBset ).EDB( MSpin ).ST_OMR_Qual
     :                    = SCI( EDBset ).EDB( MSpin ).ST_OMR_Qual .OR. DR_MinFr_Qual( JJ )
                            SCI( EDBset ).EDB( MSpin ).ST_ER_Qual 
     :                    = SCI( EDBset ).EDB( MSpin ).ST_ER_Qual  .OR. DR_MinFr_Qual( JJ )
                      END IF

                      EDBpos = EDBpos + 1
                      POINTER = POINTER + 1

                      IF( EDBpos .GE. Bytes_SFrame )   THEN
                          EDBpos = 0
                          JJ = JJ + 1
                          IF( POINTER .LE. STICS_RATE_Size )   GOTO 2000
                      ELSEIF( POINTER .LE. STICS_RATE_Size )   THEN
                          GOTO LOAD_EDB
                      ENDIF

                      IF(DIAG .LE. 5) WRITE( TTOUT, 165 )   ' STICS RATES transferred '
                      WRITE(28,2065) MSPIN,STICS_RATE( EDBset ).EDB( MSpin ).RATE
 2065                 FORMAT(' STICS Rates for spin',I3,/,(1X,20Z3.2))
                  ENDIF


                  IF( STICSPower .AND. SCI( EDBset ).EDB( MSpin ).STICS_Len .GT. 0 ) THEN       ! Move STICS-PHA
                      SCI( EDBset ).EDB( MSpin ).ST_P_Qual = DR_MinFr_Qual( JJ )
                      POINTER = 0
                      ASSIGN 2070 TO LOAD_EDB
 2070 CONTINUE
                      STICS_PHA( EDBset ).EDB( MSpin ).PHA( POINTER ) = DR_MinFr_SMS( EDBpos + 3, JJ )

                      IF( EDBpos .EQ. 0 ) THEN
                            SCI( EDBset ).EDB( MSpin ).ST_P_Qual
     :                    = SCI( EDBset ).EDB( MSpin ).ST_P_Qual .OR. DR_MinFr_Qual( JJ )
                      END IF

                      EDBpos = EDBpos + 1
                      POINTER = POINTER + 1

                      IF( EDBpos .GE. Bytes_SFrame )   THEN
                          EDBpos = 0
                          JJ = JJ + 1
                          IF( POINTER .LT. SCI( EDBset ).EDB( MSpin ).STICS_Len*4 )   GOTO 2000
                      ELSEIF( POINTER .LT. SCI( EDBset ).EDB( MSpin ).STICS_Len*4 )   THEN
                          GOTO LOAD_EDB
                      ENDIF

                      IF(DIAG .LE. 5) WRITE( TTOUT, 165 )   ' STICS PHA transferred '
                      WRITE(28,2075) MSPIN,SCI( EDBset ).EDB( MSpin ).STICS_Len, 
     &                               (STICS_PHA( EDBset ).EDB( MSpin ).PHA( ijk),ijk=0,4*SCI( EDBset ).EDB( MSpin ).STICS_Len - 1)
 2075                 FORMAT(' STICS PHA for spin',I3,'         # words =',I5,/,(1X,20Z3.2))
                  ENDIF

                  SCI( EDBset ).EDB( MSpin ).MA_UER_Qual        = 8
                  SCI( EDBset ).EDB( MSpin ).MA_SER_Qual        = 8
                  SCI( EDBset ).EDB( MSpin ).MA_BR_Qual         = 8
                  SCI( EDBset ).EDB( MSpin ).MA_MR_Qual         = 8
                  SCI( EDBset ).EDB( MSpin ).MA_P_Qual          = 8
!   Check IF MASS is ON
                  IF( MASSPower ) THEN                          ! Move MASS-RATECore
                      SCI( EDBset ).EDB( MSpin ).MA_UER_Qual    = DR_MinFr_Qual( JJ )
                      SCI( EDBset ).EDB( MSpin ).MA_SER_Qual    = DR_MinFr_Qual( JJ )
                      SCI( EDBset ).EDB( MSpin ).MA_BR_Qual     = DR_MinFr_Qual( JJ )
                      SCI( EDBset ).EDB( MSpin ).MA_MR_Qual     = DR_MinFr_Qual( JJ )
                      POINTER = 0
                      ASSIGN 2080 TO LOAD_EDB
 2080 CONTINUE
                      MASS_RATE( EDBset ).EDB( MSpin ).RATE( POINTER ) = DR_MinFr_SMS( EDBpos + 3, JJ )

                      IF( EDBpos .EQ. 0 ) THEN
                          SCI( EDBset ).EDB( MSpin ).MA_UER_Qual
     :                  = SCI( EDBset ).EDB( MSpin ).MA_UER_Qual .OR. DR_MinFr_Qual( JJ )
                          SCI( EDBset ).EDB( MSpin ).MA_SER_Qual
     :                  = SCI( EDBset ).EDB( MSpin ).MA_SER_Qual .OR. DR_MinFr_Qual( JJ )
                          SCI( EDBset ).EDB( MSpin ).MA_BR_Qual
     :                  = SCI( EDBset ).EDB( MSpin ).MA_BR_Qual .OR. DR_MinFr_Qual( JJ )
                          SCI( EDBset ).EDB( MSpin ).MA_MR_Qual
     :                  = SCI( EDBset ).EDB( MSpin ).MA_MR_Qual .OR. DR_MinFr_Qual( JJ )
                      END IF

                      EDBpos = EDBpos + 1
                      POINTER = POINTER + 1

                      IF( EDBpos .GE. Bytes_SFrame )   THEN
                          EDBpos = 0
                          JJ = JJ + 1
                          IF( POINTER .LE. MASS_RATE_Size )   GOTO 2000
                      ELSEIF( POINTER .LE. MASS_RATE_Size )   THEN
                          GOTO LOAD_EDB
                      ENDIF

                      IF(DIAG .LE. 5) WRITE( TTOUT, 165 )   ' MASS RATES transferred '
                      WRITE(28,2085) MSPIN,MASS_RATE( EDBset ).EDB( MSpin ).RATE
 2085                 FORMAT(' MASS Rates for spin',I3,/,(1X,20Z3.2))
                  ENDIF


                  IF( MASSPower .AND. SCI( EDBset ).EDB( MSpin ).MASS_Len .GT. 0 ) THEN         ! Move MASS-PHA
                      SCI( EDBset ).EDB( MSpin ).MA_P_Qual = DR_MinFr_Qual( JJ )
                      POINTER = 0
                      ASSIGN 2090 TO LOAD_EDB
 2090 CONTINUE
                      MASS_PHA( EDBset ).EDB( MSpin ).PHA( POINTER ) = DR_MinFr_SMS( EDBpos + 3, JJ )

                      IF( EDBpos .EQ. 0 ) THEN
                            SCI( EDBset ).EDB( MSpin ).MA_P_Qual
     :                    = SCI( EDBset ).EDB( MSpin ).MA_P_Qual .OR. DR_MinFr_Qual( JJ )
                      END IF

                      EDBpos = EDBpos + 1
                      POINTER = POINTER + 1

                      IF( EDBpos .GE. Bytes_SFrame )   THEN
                          EDBpos = 0
                          JJ = JJ + 1
                          IF( POINTER .LT. SCI( EDBset ).EDB( MSpin ).MASS_Len*2 )   GOTO 2000
                      ELSEIF( POINTER .LT. SCI( EDBset ).EDB( MSpin ).MASS_Len*2 )   THEN
                          GOTO LOAD_EDB
                      ENDIF

                      IF(DIAG .LE. 5) WRITE( TTOUT, 165 )   ' MASS PHA transferred '
                      WRITE(28,2095) MSPIN,SCI( EDBset ).EDB( MSpin ).MASS_Len, 
     &                               (MASS_PHA( EDBset ).EDB( MSpin ).PHA( ijk),ijk=0,2*SCI( EDBset ).EDB( MSpin ).MASS_Len - 1)
 2095                 FORMAT(' MASS PHA for spin',I3,'         # words =',I5,/,(1X,20Z3.2))
                  ENDIF
              ENDIF
              EDB_done = .TRUE.
              IF(DIAG .LE. 5) WRITE( TTOUT, 165 )   ' EDB done '
          ENDDO ! WHILE ( .NOT.EDB_done )

 2500 CONTINUE
          IF( ( MSpin .EQ. 59 ) .OR. 
     +          ( ( MSpin .LE. O_MSpin ) .AND. ( O_MSpin .NE. 59 ) ) )  THEN
!  if   1 SCI-Record is filled          prepare the next
!
!  if   2 SCI-Records are filled        prepare the next
!
!  if   3 SCI-Records are filled        do all data manipulations
!                                       save 1 SCI-Record to L1-file
!                                       shift record 2 to record 1
!                                       shift record 3 to record 2
!
              IF(DIAG .LE. 2) WRITE( IPRNT, 209 ) ( II, SCI( EDBset ).EDB( II ).EPOCH,
     +                                  SCI( EDBset ).EDB( II ).Qual,
     +                                  DELTAT( II ),
     +                                  SCI( EDBset ).EDB( II ).CORE_Qual,
     +                                  SCI( EDBset ).EDB( II ).SW_BR_Qual,
     +                                  SCI( EDBset ).EDB( II ).SW_P_Qual,
     +                                  SCI( EDBset ).EDB( II ).ST_BR_Qual,
     +                                  SCI( EDBset ).EDB( II ).ST_P_Qual,
     +                                  SCI( EDBset ).EDB( II ).MA_BR_Qual,
     +                                  SCI( EDBset ).EDB( II ).MA_P_Qual,
     +                                  II = 0, 59 )
  209 FORMAT( 1H1, T23, 'EPOCH', T38, 'QUAL', T44, 'DELTA', T65, 'QUAL-FLAGS'
     +      60( /, T5, 'EDB', I2.2, ' =', F19.3, ' ms', T39, Z2.2, 'h,',
     +             F8.1, ' ms', T60, 7Z3.2) )
C
C At this point science record EDBset has the completed copy of the most recent
C science record.  
C We must still do things with it before writing it out.  They are:
C  1) concatenate the STICS data
C  2) shift data between spins/science records
C Also, since three science records are held in memory, the write is only done 
C after the third call.
C Finally, after the L1 record is written, the other two science records' data
C must be moved down within the SCI array and the vacated data structure zero
C filled.
C
	      CALL WIND_MOVE_DOWN_L1
C
C As a convenience to users, close the output dump file and open another dump
C file with a different name, sequencing the names (01, 02, etc.)  This makes
C them easier to edit should an entire LZ file be dumped.
C Do this every DUMP_FILE_L1_RECS L1 records.
C
            IF ( MOD (SCI_REC_CNTR, DUMP_FILE_L1_RECS) .EQ. 0 ) THEN
              DUMP_SEQUENCE = DUMP_SEQUENCE + 1
              INQUIRE(UNIT=28,NAME=DUMPNAME)
              CLOSE(28)
              KK = INDEX(DUMPNAME, ';')        ! will always have ;
C
C If we have a 2-digit ending on the file extension, store updated sequence
C number (effectively incrementing it) to form sequenced filename.
C If not extend the filename by adding the sequence number on the end.
C
              IF (INDEX('0123456789', DUMPNAME(KK-1:KK-1)) .EQ. 0) THEN
                DUMPNAME(KK:) = '1;'
                KK = KK + 1
              ENDIF
              WRITE( DUMPNAME(KK-1:KK-1), '(I1)' ) MOD(DUMP_SEQUENCE,10)
              WRITE(TTOUT,1001) DUMP_SEQUENCE,DUMPNAME(1:KK)
1001          FORMAT(' Closing previous dump file, opening dump file ',I2/
     &               ' New dump filename = ',A/)
              OPEN(UNIT=28,FILE=DUMPNAME(1:KK),STATUS='NEW')
            ENDIF
C
C Zero out EPOCH times in all spins of SCI(EDBset)
C
              DO II = 0, 59
                  SCI( EDBset ).EDB( II ).EPOCH = 0D0
                  SCI( EDBset ).EDB( II ).SC_EPOCH = 0D0
              END DO
          ENDIF

          IF(DIAG .LE. 4) THEN
            WRITE( TTOUT, 900 ) 'EDB_Start =', EDB_Start
            WRITE( TTOUT, 900 ) 'SFperBlock=', SFperBlock
            WRITE( TTOUT, 900 ) 'MAXSFrame =', MAXSFrame
            WRITE( TTOUT, 900 ) '       JJ =', JJ
          ENDIF

          IF(((EDB_Start + SFperBlock).GE.MAXSFrame) .AND. (JJ.GT.EDB_Start))   THEN
              IF( .NOT.BUFFER_READ )   THEN
                  IF(DIAG .LE. 3) WRITE( TTOUT, 165 )  ' BUFFER_READ is FALSE'
                  IF( DR_Phys_Rec_Num .GT. LR_Phys_Rec_in_File )   GOTO 9999
                  C_DR2_Buffer = C_NULL_Buffer
C                  READ( L0_Lun, 400, END=9999, ERR=9998) IREC,( DR2_BUFFER(II), II=1,IREC )
                  READ( L0_Lun, 401, END=9999, ERR=9998) IREC,C_DR2_BUFFER(1:IREC)

                  CALL FIXUP( DR_Beg_SC_Clock )

                  BUFFER_READ = .TRUE.
                  IF(DIAG .LE. 3) WRITE( TTOUT, 165 )  ' BUFFER_READ is TRUE'
                  GOTO 1200
              ENDIF
          ELSE
              EDB_done = .FALSE.
C
C Update JJ to point to next minor frame
C Don't worry about it being larger than MAXSFrame, as we'll go to statement
C label 1400 and it'll check that for us
C
              JJ = EDB_Start + SFperBlock
              IF(JJ.NE.MAXSFrame)THEN
                JJ = MOD(JJ, MAXSFrame)
              ENDIF
          ENDIF

      ELSE ! IF( .NOT.HDBFlag )                                 ! *** HDB ***
C
C This is ELSE part of very large IF
C Come here if the block we found is a HDB, not an EDB
C
          HDBFirst = .FALSE.
C
C Guard against the possibility of consecutive HDB's (instead of alternating HDB's and 
C EDB's, the expected situation. If we have consecutive HDB's without an intervening 
C EDB, then HDBset could become greater than 3, and 3 is the maximum valid value.
C
          IF( HDBset .GT. 3 )THEN
            WRITE(TTOUT,*) '*** HDBset was incremented to greater than 4 !!'
            WRITE(TTOUT,*) '    setting it to 4 and continuing...'
            WRITE(TTOUT,*) ' '
            HDBset = 3
          ENDIF
C
C Assemble science record counter from HDB
C 
          SRCount       = DR_MinFr_SMS( 8, JJ )
          SRCount       = SRCount .AND. 'FF'X
          TEMP4         = DR_MinFr_SMS( 7, JJ )
          SRCount       = JIOR( ISHFT( (TEMP4 .AND. 'FF'X), 8 ), SRCount )
          TEMP4         = DR_MinFr_SMS( 6, JJ )
          SRCount       = JIOR( ISHFT( (TEMP4 .AND. 'FF'X),16 ), SRCount )
C
C Pick up spacecraft clock time, convert from PB5 to EPOCH format, and store in
C the HDB structure
C Also convert it to ITIME format, and store it, too, in structure 
C
          TEMP4         = DR_MinFr_SMS( 9, JJ )
          HDBTime       = TEMP4 .AND. 'FF'X
          TEMP4         = DR_MinFr_SMS(10, JJ )
          HDBTime       = IOR( ISHFT( HDBTime, 8 ), (TEMP4 .AND. 'FF'X) )
          TEMP4         = DR_MinFr_SMS(11, JJ )
          HDBTime       = IOR( ISHFT( HDBTime, 8 ), (TEMP4 .AND. 'FF'X) )
          TEMP4         = DR_MinFr_SMS(12, JJ )
          HDBTime       = IOR( ISHFT( HDBTime, 8 ), (TEMP4 .AND. 'FF'X) )

          HDBTime2( 1, HDBset ) = HDBTime
          CALL WIND_PB5_TO_EPOCH(       HDBTime2( 1, HDBset ),
     :                                  HDB( HDBset ).BDH.SC_EPOCH,
     :                                  ISTAT )

          CALL EPOCH_TO_ITIME( HDB(HDBset).BDH.SC_EPOCH, 
     :                          HDB(HDBset).BDH.SC_Time, ISTAT )
C
C Since wall clock time, is not given as part of the HDB, we get it from the
C EDB of spin zero.  Since the 0'th EDB for this Science Record may not yet
C have been read in and processed, defer fetching the time until the Level 1
C Science Record is complete, then get it.
C

          IF(DIAG .LE. 3) WRITE( IPRNT, 320 )  JJ, SRCount, HDBTime
  320 FORMAT( ' Start of HDB at SubFr.', I3,
     +        ',  SRCount =', I8, ',  HDBTime =', Z8.8, / )

          HDB_Start     = JJ
          HDB_End       = HDB_Start + SFperBlock - 1
          HDB_Pos       = 0
          HDB( HDBset ).BDH.Qual = SR_Qual
          IF(DIAG .LE. 3) WRITE( TTOUT, 340 )   HDB_Start, HDB_End
  340 FORMAT( 1H0, ' HDB   Start =', I4.3, '  End =', I4.3 )

          DO II = HDB_Start, HDB_End
              KK = MOD( II, MAXSFrame )
              IF( (KK .NE. HDB_Start) .AND. (KK .EQ. 0) )   THEN
                  C_DR_Buffer = C_DR2_Buffer
                  OBitRate    = BitRate
                  IF(DIAG .LE. 2) WRITE( IPRNT, 120 )   'SHIFTED3', DR_Phys_Rec_Num, DR_Beg_MajFr_Cnt

                  CALL WIND_PB5_TO_EPOCH(       DR_Beg_SC_Clock,! UTC: PB5 (2 INTEGER*4)
     +                                          DR_SC_EPOCH,    ! UTC: EPOCH as Real*8
     +                                          ISTAT )
                  IF( ISTAT .NE. SS$_NORMAL ) CALL EXIT( ISTAT )

                  CALL WIND_EPOCH_TO_GREG(      SC_Beg_Year,
     :                                          SC_Beg_DOY,
     :                                          SC_Beg_Msec,
     :                                          SC_Beg_MicSec,
     +                                          DR_SC_EPOCH,    ! UTC: EPOCH as Real*8
     +                                          ISTAT )
                  IF( ISTAT .NE. SS$_NORMAL ) CALL EXIT( ISTAT )

                  CALL WIND_GREG_TO_EPOCH( DR_Beg_Year,         ! ATC: Year
     +                                          DR_Beg_DOY,     ! ATC: Day Of Year
     +                                          DR_Beg_MSec,    ! ATC: Millisecond of DAY
     +                                          DR_Beg_MicSec,  ! ATC: Microsecond of MSec
     +                                          DR_Beg_EPOCH,   ! ATC: EPOCH AS REAL*8
     +                                          ISTAT )
                  IF( ISTAT .NE. SS$_NORMAL ) CALL EXIT( ISTAT )

                  IF( lSCTime ) THEN
                      DYRDAY = DR_SC_EPOCH
                  ELSE
                      DYRDAY = DR_Beg_EPOCH
                  END IF
                  Pref_EPOCH = DYRDAY

                  CALL WIND_PRINT_HK( DR_Beg_MajFr_Cnt, HKset(EDBset), SYNC_WORD )

                  IF(DIAG .LE. 3) WRITE( IPRNT, 130 ) DR_Phys_Rec_Num, DR_Beg_MajFr_Cnt,
     +                          DR_Beg_Year,    SC_Beg_Year,    ! Year
     +                          DR_Beg_DOY,     SC_Beg_DOY,     ! Day Of Year
     +                          DR_Beg_MSec,    SC_Beg_Msec,    ! Millisecond of DAY
     +                          DR_Beg_MicSec,  SC_Beg_MicSec,  ! Microsecond of MSec
     +                          DR_Beg_EPOCH,   DR_SC_EPOCH     ! EPOCH AS REAL*8

                  IF(DIAG .LE. 2) WRITE( IPRT, 132 )
     +          DR_Instr_Num,           ! Instrument Number
     +          DR_Phys_Rec_Num,        ! Physical Record Number within File
     +          DR_Beg_MajFr_Cnt,       ! Major Frame Count-Beginning of Maj.Fra
     +          DR_Beg_SC_Clock,        ! Spacecraft Clock -Beginning of Maj.Fra
     +          DR_Beg_Year,            ! ATC: year-Beginning of Major Frame
     +          DR_Beg_DOY,             ! ATC: doy -Beginning of Major Frame
     +          DR_Beg_MSec,            ! ATC: msec-Beginning of Major Frame
     +          DR_Beg_MicSec,          ! ATC: usec-Beginning of Major Frame
     +          DR_Num_MinFr_Fill,      ! Number of Minor Frames with Fill
     +          DR_Num_MinFr_Sync_Err,  ! Number of Minor Frames with Sync_Err
     +          DR_TLM_Mode,            ! Telemetry Mode Indicator
                                                ! 4 = contingency mode
     +          DR_MinFr_Qual,          ! Minor Frame Quality
                                                ! bit 0 = frame sync error flag
                                                ! bit 1 = frame counter error fllag
                                                ! bit 2 = fill frame flag
                                                ! bit 3-31 spares
     +          DR_Fill,                ! adjustment to Longword
     +          DR_Beg_EPOCH            ! EPOCH time-Beginning of Major Frame ?

                  BUFFER_READ = .FALSE.
              ENDIF
                  
              HDB( HDBset ).BDH.Qual = HDB( HDBset ).BDH.Qual .OR. DR_MinFr_Qual( KK )

              DO IJ = 3, Bytes_SFrame + 2
                  HDB( HDBset ).BDH.DAT( HDB_Pos ) = DR_MinFr_SMS( IJ, KK )
                  HDB_Pos = HDB_Pos + 1
              ENDDO
          ENDDO !       HDB_Start, HDB_End

          IF(DIAG .LE. 1) WRITE( IPRNT, 510 )   HDB( HDBset ).BDH.SC_EPOCH, HDB( HDBset ).BDH.Qual,
     +                          ( HDBTime2( II, HDBset ), II = 1, 2 ),
     +                          ( HDB( HDBset ).BDH.DAT( II ), II = 0, HDB_Pos-1 )
  510 FORMAT( 1H1, T10, 'HDB_EPOCH =', F16.0, T40, 'QUAL =', Z3.2,
     +             T51, 'HDBTime2 =',  2z9.8,
     +      /, ( 2( T10, 10Z3.2, T45, 10Z3.2, / ) ) )
C
C Update JJ to point to next minor frame
C Don't worry about it being larger than MAXSFrame, as we'll go to statement
C label 1400 and it'll check that for us
C
          JJ = HDB_Start + SFperBlock
          IF(JJ.NE.MAXSFrame)THEN
            JJ = MOD(JJ, MAXSFrame)
          ENDIF
          SR_Qual = 0
C
C We just finished filling the HDB structure (actually an array 3 long indexed
C by HDBset).  Increment HDBset so our next time through this code we'll store
C into the next HDB position.  Since HDBs are written out with Science Records,
C whose writes are deferred to allow shifting of times (see earlier comments),
C we must keep three of them and allow WIND_STORE_L1 to write the HDB out with
C its associated EDB.
        WRITE(28,9995) SRCount, HDB( HDBset ).BDH.DAT
        WRITE(28,9996)
 9995   FORMAT(' **************************************************************************',/,' HDB for SR',I7,/,(1X,20Z3.2))
 9996   FORMAT(' **************************************************************************')
C
C While HDBset is incremented in this section of code (the ELSE part of the
C large IF), it is decremented in the THEN part where EDBs are complete and the
C write is done.  Thus, we allow HDBset to be incremented beyond the array size
C of 3, since it will be decremented to 3 before reaching an array reference.
C
          HDBset = HDBset + 1

      ENDIF ! IF( .NOT.HDBFlag )
C
C Check if we are done reading L0 data by testing against user-supplied stop
C time.  Branch back to label 1400 if there is more to do.
C
      IF( DYRDAY .GT. STPTime ) GOTO 9999
      GOTO 1400
  900 FORMAT( 1X, A, I6 )
C
C All done, wind down processing.
C
9997  CONTINUE
9998  CONTINUE
9999  CONTINUE
99090 continue
      CLOSE ( L0_Lun )
      CLOSE ( 28 )
      END

      SUBROUTINE FIXUP( SC_Clock )
C
C This routine is called after a READ to possibly reverse the two 32-bit parts
C of the SC clock.  On the older CD (1993) these values come in correctly,
C while on the newer files (both production and NRT) they are in the reverse
C order.  This may change again when the CDs of Level 0 data start being
C produced.
C
C For now FIXUP is called unconditionally.  This will have to be
C reevaluated later, when the CDs of actual data start coming out.
C
C ALGORITHM:
C
C The first time we are called obtain the current date (we're interested in the
C year) and extract the year from the SC clock.  Compare these two values.
C If the year obtained from the SC clock (SC_Year) is later than the
C current year, and obvious impossibility, set the logical variable SWAP_PARTS
C to .TRUE.; otherwise set it to .FALSE. .
C
C For all calls test SWAP_PARTS.  If it is set exchange the two 32-bit parts of
C SC_Clock.
C
C While this will not catch all errors, it will catch the more obvious one
C and allow WIND_PRINTLZ to operate
C Further refinements will have to wait for a later date.
C
      IMPLICIT NONE
      INTEGER*4 SC_Clock(2), ITEMP
      INTEGER MONTH, DAY, THIS_YEAR
      LOGICAL FIRST_TIME, SWAP_PARTS
      SAVE FIRST_TIME, SWAP_PARTS
      DATA FIRST_TIME, SWAP_PARTS /.TRUE., .FALSE./
C
      REAL*8 SC_EPOCH
      INTEGER*4  SC_Year, SC_DOY, SC_Msec, SC_MicSec, ISTAT
C
C The first time we are called:
C   Obtain the current date
C   Decode the SC clock time into normal calendar date/time
C   Compare the years from the two sources and set SWAP_PARTS accordingly
C
      IF (FIRST_TIME) THEN
C
C Get the date.  Remember to adjust year to proper century, as it only give a
C 2-digit value (0..99).
C
        CALL IDATE(MONTH,DAY,THIS_YEAR)
        IF (THIS_YEAR .LT. 90) THIS_YEAR = THIS_YEAR + 2000
        IF (THIS_YEAR .LT. 100) THIS_YEAR = THIS_YEAR + 1900
C
C First convert SC clock into EPOCH time
C
        CALL WIND_PB5_TO_EPOCH( SC_Clock,         ! UTC: PB5 (2 INTEGER*4)
     +                          SC_EPOCH,         ! UTC: EPOCH as Real*8
     +                          ISTAT )
C
C Now convert the EPOCH time into normal time: calendar year, day of year, etc.
C
        CALL WIND_EPOCH_TO_GREG(  SC_Year,
     +                            SC_DOY,
     +                            SC_Msec,
     +                            SC_MicSec,
     +                            SC_EPOCH,       ! UTC: EPOCH as Real*8
     +                            ISTAT )
C
C Now for the big test.  If the year just obtained (SC_Year) is later than the
C current year, set SWAP_PARTS to .TRUE., else set it to .FALSE.
C Reset the first-time-called flag.
C
C While this will not catch all errors, it will catch the more obvious ones.
C Further refinements will have to wait for a later date.
C
        SWAP_PARTS = SC_Year .GT. THIS_YEAR
        FIRST_TIME = .FALSE.
        TYPE *,'THIS_YEAR, SC_Year =',THIS_YEAR,SC_Year
        IF (SWAP_PARTS) TYPE *,'SC clock parts will be swapped'
        IF (.NOT. SWAP_PARTS) TYPE *,'SC clock parts will NOT be swapped'
      END IF
C
C At this point SWAP_PARTS has been properly set
C Come here for all calls, test SWAP_PARTS, and exchange the two parts of
C SC_Clock if it is set
C
      IF (SWAP_PARTS) THEN
        ITEMP = SC_Clock(1)
        SC_Clock(1) = SC_Clock(2)
        SC_Clock(2) = ITEMP
      END IF
C
      RETURN
      END

