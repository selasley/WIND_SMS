      PROGRAM WIND_CD_READ
C+
C     WIND_CD_READ
C       This program transfers the data from a Level 0 file on a CD-ROM and
C       constructs a Level 1 disk file.  The user is prompted for the start
C       and stop times to use (typically a 24 hr period), as well as several
C       other operating parameters.
C
C       (As a future upgrade it appends several major frames of data from the
C       CD for the next sequencial day****FUTURE****).
C
C     This version:   5.2   25-MAY-1995
C
C     REFERENCE:
C
C     USAGE and PARAMETERS:
C       RUN WIND_READ_CD
C
C     CALLS/FUNCTIONS:
C       CALL EPOCH_TO_ITIME( RTIME, ITIME, ISTAT )
C       CALL EXIT( ISTAT )                              ! SYSTEM
C       CALL MVBITS( TEMP2, 2, 6, MSpin, 0 )            ! SYSTEM
C       CALL RECORD_PC_DATA( DR_Beg_EPOCH, ISTAT )
C       CALL TILOG( lSCTime, *1120 )                    !! XTI_LIB.OLB
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
C       CALL WIND_OPEN_L1( FIL1NAME, 'OLD', ISTAT )
C       CALL WIND_OPEN_LZ_DATA( FILENAME, 'OLD', ISTAT )                                !! WIND_LIB.OLB
C       CALL WIND_PB5_TO_EPOCH(         LR_Beg_SC_Clock,! UTC: PB5 (2 INTEGER*4)        !! WIND_LIB.OLB
C     +                                 SC_Beg_EPOCH,   ! UTC: EPOCH as Real*8
C     +                                 ISTAT )
C       CALL WIND_SAVE_HK( DR_Beg_MajFr_Cnt, HKset, SYNC_WORD )
C       CALL WIND_CONCATENATE_STICS(EDBSet)
C       CALL WIND_SHIFT_DATA
C       CALL WIND_STORE_L1( STATUS )
C       CALL WIND_RESET_HK(EDBSet-1)
C       CALL WIND_BUILD_FILE_HEADER
C       CALL WIND_BUILD_SR_HEADER
C
C       = BTEST( TEMP2, 1 )                                     ! SYSTEM
C       = CHLAST( FILENAME )                                    ! UMDCOMMON.OLB
C       = IAND( TEMP2, 'FF'X )                                  ! SYSTEM
C       = IOR( ISHFT( HDBTime, 8 ), (TEMP4 .AND. 'FF'X) )       ! SYSTEM
C       = JIOR( ISHFT( (TEMP4 .AND. 'FF'X), 8 ), SRNumber )      ! SYSTEM
C
C     CREATION:
C       16-Feb-94       H.BROER         UMD
C     REVISIONS:
C       12-APR-1994     H.BROER         changed EDB
C       28-APR-1994     H.BROER         corrected EDB Time
C       24-MAY-1994     H.BROER         included Quality Flags
C	25-MAY-1994	H.BROER         enhancement of HK storage, changed
C					L0_DATA_Record
C       10-JUN-1994     H.BROER         combined EDB.inc + HK.inc
C       17-JUN-1994     H.BROER         added INTEGER times
C       21-JUN-1994     H.BROER         added WIND_OPEN_L1 & WIND_STORE_L1
C       29-JUN-1994     H.BROER         changed calls to EPOCH_TO_ITIME
C       19-JUL-1994     J.PAQUETTE      for output L1 file which exists, remove
C                                       explicit version number before OPEN
C       20-JUL-1994     J.PAQUETTE      added call to WIND_CONCATENATE_STICS
C       29-JUL-1994     J.PAQUETTE      changed call to WIND_CONCATENATE_STICS
C        4-AUG-1994     J.PAQUETTE      3 science records in memory instead of 2
C       24-AUG-1994     J.PAQUETTE      added call to WIND_SHIFT_DATA
C        9-SEP-1994     J.PAQUETTE      changed location of SHIFT and STORE;
C                                       altered HK structure; changed
C                                       references to HKset and Hkeep to array
C                                       references using EDBset as subscript
C       20-SEP-1994     J.PAQUETTE,     The change in the number of Science Recs
C                       L.BLEAU         neccesitated a change in the HK structures.
C                                       The calls to SHIFT, STORE, and CONCATENATE
C                                       have been moved. The STORE routine has
C                                       been altered, as has SAVE_HK; and a new
C					routine, RESET_HK has been created.
C       21-SEP-1994     L. Bleau        remove initialization of HDBset; it is
C                                       set to 1 in WIND_CD_INIT
C       21-SEP-1994     J.PAQUETTE,     add logic to increment HDBset after
C                       L. Bleau        each HDB is complete, and to move HDBs
C                                       as EDBs are moved
C       21-SEP-1994     J.PAQUETTE      changed dimensions and indexing of
C                                       HDBTIME and HDBTIME2
C       22-SEP-1994     J.PAQUETTE      removed EQUIVALENCE of HDBTime and
C                                       HDBTime2; HDBTime(1,HDBset) now equated
C                                       to HDBTime by assignment
C       22-SEP-1994     L. Bleau        add call to EPOCH_TO_ITIME and store
C                                       HDB's SC_EPOCH into HDB structure
C       23-SEP-1994     J.PAQUETTE,     zero out the third science record
C                       L.BLEAU         between writes
C       26-SEP-1994     J.PAQUETTE      added a host of new quality flags (one
C					for each type of rates); neccesary
C					because of shifting
C       30-SEP-1994     L.Bleau         fix clearing of science record between
C					writes so it doesn't overwrite headers
C       30-SEP-1994     J.PAQUETTE      change WIND_CONCATENATE_STICS call and
C					move it
C                                       outside of IF(EDBSET.EQ.3) block
C        4-OCT-1994     J.PAQUETTE      Note that information about some spins
C                                       may be absent from some Sci Rec's,
C                                       there is no guarantee that spins w/o
C                                       PHA data will have a zero value for the
C                                       # of PHA words.
C        4-OCT-1994     L.Bleau         made diagnostic variable CNTR permanent
C                                       and renamed it L1_SCI_REC_CNTR
C       18-OCT-1994     J.PAQUETTE      added calls to WIND_BUILD_FILE_HEADER
C                                       and to WIND_BUILD_SR_HEADER
C       25-OCT-1994     J.PAQUETTE      corrected error in setting of SW_BR_Qual
C       27-OCT-1994     L. Bleau        put DIAG into common block DIAG, made
C                                       remaining WRITEs conditional
C       27-OCT-1994     L. Bleau        changed name L1_sci_rec_cntr to sci_rec_cntr
C       27-OCT-1994     L. Bleau        added printing of program start, stop times
C       27-OCT-1994     L. Bleau        added L1_recs_written
C        1-NOV-1994     L. Bleau        added comments, documented bug
C        4-NOV-1994     L. Bleau        added code to copy spin 0 wall clock
C                                       time to HDB, added comments; changed KB
C                                       to TTOUT in WRITE statements
C        7-NOV-1994     J.PAQUETTE      added variable SYNC_WORD to allow for cases
C                                       when DPU is off, but still have HK data
C        7-NOV-1994     L. Bleau        Add routine FIXUP to swap the two 32-bit
C                                       parts of SC clock when reading NRT
C					files; call it after each READ if not
C					PRODuction
C       17-NOV-1994     J.PAQUETTE      added tests on HDBset to allow for non-
C					alternating order of HDB's and EDB's in
C					NRT files
C       18-NOV-1994     L. BLeau        remove MOD operation when updating JJ
C                                       to eliminate infinite loop problem
C        1-DEC-1994     J.PAQUETTE      added code to allow for 1st MSPIN=0 case
C        5-DEC-1994     J.PAQUETTE      added telemetry mode to output
C  3.4   6-DEC-1994     L.Bleau         test if interactive job to determine
C                                       which write format to use; changed TYPE
C                                       statements to WRITE
C        8-DEC-1994     J.PAQUETTE      reinserted MOD operation in computation
C                                       of new JJ value; see earlier revision
C                                       of 18-NOV-1994; still needs work
C  3.5   9-DEC-1994     L. Bleau        removed code to prompt user if L1 file
C                                       already exists, add informational WRITE
C  3.6  12-DEC-1994     L. Bleau        reinstated MOD function where JJ is
C                                       computed; see earlier revision
C  3.7  15-DEC-1994     J.PAQUETTE      added workaround on computation of JJ
C       15-DEC-1994     L. Bleau        removed definitions of EDB_Sum,
C                                       EDB_Lost, and EDB_Skip, put them in
C					EDBVARS.INC to be written to L1 file
C					header; add code to close, open, read,
C					rewrite L1 file header
C  3.8  16-DEC-1994     L. Bleau        make calls to FIXUP to (possibly)
C                                       reverse parts of SC clock times uncondi-
C                                       tional, add smarts to FIXUP to detect
C                                       obvious cases where swap is needed;
C                                       review when new CDs come out;
C       16-DEC-1994     L. Bleau        eliminate variable CTYPE
C  3.9  19-DEC-1994     L. Bleau        changed READs to use CHAR variables
C 3.10  20-DEC-1994     L. Bleau        change all local LOGICAL*1 variables to
C                                       LOGICAL*4, all local INTEGER*2 variables
C                                       to INTEGER*4, changed IIAND call to IAND
C 3.11   6-JAN-1995     L. Bleau        write summary line to a log file
C 3.12  11-JAN-1995     J.PAQUETTE      check to see if # of PHA words is > than
C					maximum possible
C 3.13  13-JAN-1995     L. Bleau        remove code that modifies+outputs L1
C                                       record and put into subroutine
C					WIND_OUTPUT_L1, add HDBtime2 to common
C					block HDB
C 3.14  23-JAN-1995     J.PAQUETTE      altered order of diagnostic, correction
C                                       if too many PHA words; OBitRate is now
C					initialized to BitRate, even for 1st EDB
C 4.0    2-FEB-1995     J.PAQUETTE      changed logic that marks missing spins
C                                       as such (formerly erroneosly marked
C                                       good spins as missing)
C 4.1    6-FEB-1995     J.PAQUETTE      allow for possibility that MSpin exceeds
C                       L.BLEAU         59; if it does, skip that spin
C 4.2    8-FEB-1995	L. Bleau	make use of NRT_MODE flag (in common
C					block DIAG) to not call RECORD_PC_DATA
C					if set, also change default input file
C					pattern
C	 9-FEB-1995	L. Bleau	changed '!' to 'C' in program comments
C	 9-FEB-1995	L. Bleau	change include stmt to use EDBDEF.INC
C					and EDBVARS.INC instead of EDB.INC only
C	 9-FEB-1995	L. Bleau	change WRITE_TO_LOG to WRITE_SUMMARY;
C					change several EXIT to LIB$STOP calls
C 4.4   13-FEB-1995	L. Bleau	added code to report interesting/error
C					conditions using WIND_REPORT; moved
C					resetting of EDBfirst to after EDB_done
C					loop (label 2500); add variable
C					O_Good_Bit_Rate
C 4.5	14-FEB-1995	L. Bleau	changed call to FIXUP to have it swap
C					correct copy of SC clock, which is read
C					into DR2_BUFFER, *NOT* DR_BUFFER
C 4.6	14-FEB-1995	L. Bleau	added code to report missing or
C					multiple HDBs
C 4.7   15-FEB-1995     J.PAQUETTE      HDBset variable removed from common; 
C                       L. Bleau        HDB structure now indexed by EDBset
C					guaranteeing that HDB is associated
C					with the proper EDB; add code to take
C					care of back-to-back HDBs w/missing EDBs
C 4.8	16-FEB-1995	L. Bleau	when spin is missing set *all* qual
C			J.PAQUETTE	flags to indicate missing; add routine
C					MARK_MISSING to help us
C 4.9	17-FEB-1995	L. Bleau	removed filename prompting and manipu-
C					lation code, put into new subroutine
C					WIND_CD_OPEN_FILES
C       17-FEB-1995	L. Bleau	changed SFDU_Lun to L0_Lun, since it
C					refers to the L0 (or LZ) file and not
C					to the SFDU file 
C 4.10  23-FEB-1995     J.PAQUETTE      mark missing HDB's as such in long data
C                       L. Bleau        gaps; throw out a spin if # PHA words
C					is too large
C 4.11	23-FEB-1995	L. Bleau	add code to let run in PRINTLZ mode to
C					dump the LZ file
C 4.12  24-FEB-1995     J.PAQUETTE      make sure to save old spin value if # 
C                                       PHA words too large
C 4.13	27-FEB-1995	L. Bleau	if HDB first in LZ file got subscript
C					error; fix by outputting sci rec of 60
C					missing spins and an HDB
C	27-FEB-1995	L. Bleau	changed HDBFirst to FirstHDB
C 4.14	27-FEB-1995	L. Bleau	fixed bug where TEMP2 was overwritten
C			J.PAQUETTE	before being tested for MASSPower
C        1-MAR-1995     J.PAQUETTE      PHA quality flags not set bad if no PHA words
C 4.15	 2-MAR-1995	L. Bleau	validate EDB/HDB header field SFperBlock,
C					throw out if illegal value
C 4.16	 3-MAR-1995	L. Bleau	incremented JJ when core qual is bad to
C					force scan for new EDB and read next LZ
C					record 
C 4.18	 8-MAR-1995	L. Bleau	switched calls to WIND_CD_INIT,
C					TIM_STAMP; now using GET_IMAGE_INFO to
C					get link date/time for TIM_STAMP use
C 4.19	14-MAR-1995	L. Bleau	limit output file to 600 L1 records in
C					case there's an infinite loop (change
C					made in WIND_OUTPUT_L1)
C 4.20	15-MAR-1995	L. Bleau	modified WIND_BUILD_L1_HEADER to have
C					it get LZ file creation date, put into
C					file header 
C 4.21	15-MAR-1995	L. Bleau	save start, stop times into other
C					variables to they don't get overwritten,
C					change contents of common block DATES,
C					also changed WIND_BUILD_L1_HEADER
C 4.22	15-MAR-1995	L. Bleau	use logical name WIND_COMMENT as
C					contents of file hdr comment field
C					(change made to WIND_CD_INIT)
C 4.23	 7-APR-1995	L. Bleau	changed WIND_BUILD_L1_HEADER to store
C					orbit, attitude info in sci rec header;
C					changed WIND_CD_OPEN_FILES to correct
C					filename construction error
C 5.0	12-APR-1995	L.Bleau		update to allow orbit, attitude data to
C			J.PAQUETTE	be written to L1 file; include
C					L1INFO.INC
C 5.1	26-APR-1995	L. Bleau	add option to stop program if orbit and
C					attitude files not present; use search
C					pattern for orbit, attitude files to get
C					lowest version of each file; see
C					WIND_CD_OPEN_FILES.FOR for details
C 5.2	25-MAY-1995	L. Bleau	corrected FIXUP routine to swap parts
C					of SC clock if high bit is SC(1) is set
C-

CDEC$ IDENT     '5.2-2'

      IMPLICIT NONE

      INCLUDE   'INC_DIR:EDBDEF.INC/NOLIST'
      INCLUDE   'INC_DIR:EDBVARS.INC/NOLIST'
      INCLUDE   'INC_DIR:L0_DATA_Record.INC/NOLIST'
      INCLUDE   'INC_DIR:L0_Labl_Record.INC/NOLIST'
      INCLUDE   'INC_DIR:SW_CODES.INC/NOLIST'
      INCLUDE   'INC_DIR:WIND_Lun.INC/NOLIST'
      INCLUDE   'INC_DIR:DIAG.INC/NOLIST'
      INCLUDE	'INC_DIR:L1FMT.INC/NOLIST'
      INCLUDE	'INC_DIR:L1INFO.INC/NOLIST'
      INCLUDE	'INC_DIR:L1_HDR_INFO.INC/NOLIST'
      INCLUDE   '($SSDEF)/NOLIST'

      COMMON /FILE/  FILENAME, FIL1NAME

      COMMON /DATES/ Clock_Beg_Year, Clock_Beg_DOY, Clock_Beg_Msec, Clock_Beg_MicSec,
     &               Clock_End_Year, Clock_End_DOY, Clock_End_Msec, Clock_End_MicSec
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
     +                  Clock_Beg_Year,	! save start time here
     +                  Clock_Beg_DOY,	!
     +                  Clock_Beg_Msec,	!
     +                  Clock_Beg_MicSec, !
     +                  Clock_End_Year,	! save stop time here
     +                  Clock_End_DOY,	!
     +                  Clock_End_Msec,	!
     +                  Clock_End_MicSec, !
     +                  EDBpos,         ! internal
     +                  EDB_Start,      ! internal
     +                  HDBset,         ! index into HDB
     +                  HDB_End,        ! End position of HDB
     +                  HDB_Pos,        ! Current position of HDB
     +                  HDB_Start,      ! Starting position of HDB
     :                  HDBTime,        ! Time contained in HDB-Header
     :                  IARRAY(5),      ! temp array for calls to WIND_REPORT
     :                  II,             ! LOOP COUNTER
     :                  IJ,             !  "      "
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
     :                  MSpin,          ! Measured Spin
     :                  NCF,            ! NUMBER OF CHARACTERS IN FILENAME
     :                  O_MSpin,        ! Last Measured Spin
     :                  O_SpinCnt,      ! Last Spin counter
     :                  POINTER,        ! INTERNAL
     &                  Prev_O_MSpin,   ! temp: previous value of O_MSpin
     &                  Prev_Sci_rec_cntr, ! temp: prev value of Sci_Rec_Cntr
     :                  SC_Beg_Year,    !
     :                  SC_Beg_DOY,     !
     :                  SC_Beg_Msec,    !
     :                  SC_Beg_MicSec,  !
     :                  SC_End_Year,    !
     :                  SC_End_DOY,     !
     :                  SC_End_Msec,    !
     :                  SC_End_MicSec,  !
     +                  SRNumber,       ! Science Record Count, read from MajF
     +                  TEMP,
     +                  TEMP2,
     +                  TEMP4

      LOGICAL*4         FirstHDB/.TRUE./,
     +                  Last_Was_HDB,   ! set if HDB processed, clear for EDB
     +                  BTEST,          ! FUNCTION
     +                  BUFFER_READ/.FALSE./,   ! MAJOR FRAME READ FLAG
     +                  CmdErrFlag,     ! Command Error Flag
     +                  EDB_done,       ! internal
     +                  FirstEDB/.TRUE./,!First time flag
     +                  lSCTime,        ! S/C clock is PREFERRED TIME
     &                  MISSING_SPIN/.FALSE./,	! Set if data gap found
     :                  OBitRate/.TRUE./,       ! Old BitRate
     :                  O_Good_Bit_Rate, ! BitRate in previous good spin
     :                  OK,             ! internal value
     &                  SYNC_WORD,	! Set when sync word is found
     :                  VALData         ! Valid Data available (within time)

C
      CHARACTER*80      FILENAME, FIL1NAME, INFILE, INCHAR
      CHARACTER*34      CLASS(0:9)
      CHARACTER*8       PROGRAM_START_TIME, PROGRAM_END_TIME
      CHARACTER*23      START_DATE_TIME

      INTEGER           DUMP_SEQUENCE/1/,
     &                  DUMP_FILE_L1_RECS/120/,
     &                  IJK

      CHARACTER*80	DUMPNAME
      CHARACTER*30	ATIME, WIND_FORMAT_TIME

      INTEGER SPIN
      DATA DIAG/6/

      INTEGER*4 FORTERR,RMSSTC,RMSSTV,LUN,VAXERR

      CALL WIND_CD_INIT

      CALL TIM_STAMP

      CALL WIND_CD_OPEN_FILES(FILENAME,FIL1NAME,NCF)

  100 FORMAT(1H , A, $)
C
      IREC = LR_Phys_Rec_Size
C
C  LABEL_RECORD                                              ! READ LABEL RECORD
C
      READ( L0_Lun, 401, END=9999, ERR=9998)  IREC, C_LR_BUFFER(1:IREC)
  400 FORMAT( Q, 12800A1 )
  401 FORMAT( Q, A )

      CALL FIXUP( LR_Beg_SC_Clock )
      CALL FIXUP( LR_End_SC_Clock )

      IF(DIAG .LE. 4) WRITE( TTOUT, 111 )  LR_Phys_Rec_Size
  111 FORMAT( 1H0, ' RECORD SIZE FOR THIS FILE =', I6, // )

 1120 WRITE( TTOUT, 100 ) 'Do you prefer S/C clock ?'
      lSCTime = .FALSE.
      CALL TILOG( lSCTime, *1120 )

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
C
C Since the SC_Beg_xxxx variables get overwritten with each read operation
C save their values under different variable names so they can be written to
C the L1 file header correctly.  Do same with SC_End_xxxx values.
C
      Clock_Beg_Year = SC_Beg_Year
      Clock_Beg_DOY = SC_Beg_DOY
      Clock_Beg_Msec = SC_Beg_Msec
      Clock_Beg_MicSec = SC_Beg_MicSec

      Clock_End_Year = SC_End_Year
      Clock_End_DOY = SC_End_DOY
      Clock_End_Msec = SC_End_Msec
      Clock_End_MicSec = SC_End_MicSec

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
C
C Get current date/time as well as initializing timer
C
      CALL LIB$DATE_TIME(START_DATE_TIME)
      CALL TIME(PROGRAM_START_TIME)
C
C Set New_Sci_Rec and Last_Was_HDB to .TRUE. for first pass through data
C
      New_Sci_Rec = .TRUE. 
      Last_Was_HDB = .TRUE. 

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
C
      READ( L0_Lun, 401, END=9999, ERR=9998) IREC,C_DR2_BUFFER(1:IREC)

      CALL FIXUP( DR2_Beg_SC_Clock )

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
          OBitRate = BitRate

          IF(DIAG .LE. 2) WRITE( IPRNT, 120 )   ' SHIFTED', DR_Phys_Rec_Num, DR_Beg_MajFr_Cnt
  120 FORMAT( 1H0, A, ', DR_Phys_Rec_Num =', I5, ', DR_Beg_MajFr_Cnt = ', Z8 )

      END IF ! ( BUFFER_READ )

      READ( L0_Lun, 401, END=9999, ERR=9998) IREC,C_DR2_BUFFER(1:IREC)

      CALL FIXUP( DR2_Beg_SC_Clock )

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

      CALL WIND_SAVE_HK( DR_Beg_MajFr_Cnt, HKset(EDBset), SYNC_WORD )

      ISTAT = 100
      IF (.NOT. NRT_MODE) CALL RECORD_PC_DATA( DR_Beg_EPOCH, ISTAT )                ! Record Hk data

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
C Step through subframes until we find one whose first two bytes contain the
C sync flag (byte 0 = 14h, byte 1 = 6Fh) Also require byte 2 is nonzero.
C
C 1st subscript to DP_MinFr_SMS is a byte offset, but 3 greater than in
C documentation
C 2nd subscript to DP_MinFr_SMS (variable JJ) is a minor frame (subframe) index
C
      Curr_Spin = -1
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

C      Collect characteristic properties from CORE data

C Do sanity check on byte 2; it should be a zero since we got out of previous
C WHILE loop

      TEMP2 = DR_MinFr_SMS( 5, JJ )                             ! BYTE 2
      IF( TEMP2 .EQ. 0 ) THEN
          JJ = JJ + 1
          GOTO 1400
      ENDIF
      IF(DIAG .LE. 1) WRITE( TTOUT, 150 )  ( DR_MinFr_SMS( IJ, JJ ), IJ = 3, 13 )
  150 FORMAT( 1H , 11Z5.3 )
C
C Get bit rate, HDB flag, and number of subframes per block
C Set Bytes_SFrame based on BitRate
C Don't report a bitrate change yet as we don't yet know if this is a good spin
C
      BitRate = BTEST( TEMP2, 7 )                       ! BitRate
      IF( BTEST( TEMP2, 6 ) ) THEN                      ! Byte/SF
          Bytes_SFrame = 40
      ELSE
          Bytes_SFrame = 33
      ENDIF
      HDBFlag = BTEST( TEMP2, 5 )                       ! Block Mode
      SFperBlock = IAND( TEMP2, '1F'X )                 ! Subframes per Block
C
C Go to different sections of cade based on what type of block this is (EDB or
C HDB) as they are quite different in format and content
C
      IF( .NOT. HDBFlag )  THEN                         ! this is EDB
C
C Interpret CORE data
C
C If any part has an invalid value we cannot trust this block at all; pass the
C data on to the L1 file anyway, though.
C
C If any header part which is used to control WIND_CD_READ's operations has an
C invalid value we not only cannot trust this block, but we dare not process
C the rest normally as this could result in improper WIND_CD_READ operation.
C In this case cause the block to be discarded.
C
C Validate number of subframes per block, which for an EDB should be in the
C range 6 to 10 for low bit rate or 13 to 20 for high bit rate.
C
	  IF (.NOT. BitRate .AND. (SFperBlock .LT.  6 .OR. SFperBlock .GT. 10) .OR.
     *         BitRate      .AND. (SFperBlock .LT. 13 .OR. SFperBlock .GT. 20)) THEN
	    CALL WIND_REPORT( 'illegal value of !SL for SFperBlock, ignoring this EDB', 1, SFperBlock )
            EDB_Skip = EDB_Skip + 1
            EDB_Lost = EDB_Lost - 1
            JJ = JJ + 1
            GO TO 1400
	  ENDIF
C
C Extract and validate measured spin
C Save previous value of MSpin in TEMP4
C
          TEMP4 = MSpin
C
          MSpin         = 0
          TEMP2         = DR_MinFr_SMS( 6, JJ )         ! BYTE 3
          CALL  MVBITS( TEMP2, 2, 6, MSpin, 0 )         ! Measured Spin
C
C If MSpin is too large, skip this EDB and get another.  Increment JJ (minor
C frame counter) and go to label 1400 to do this.  Count this EDB as skipped.
C We'll depend on later code to mark the intervening spins as bad
C
          IF (MSpin .GT. 59) THEN
	    CALL WIND_REPORT( 'illegal value of !SL for Mspin, ignoring', 1, Mspin )
            EDB_Skip = EDB_Skip + 1
            EDB_Lost = EDB_Lost - 1
            JJ = JJ + 1
            MSpin = TEMP4
            GO TO 1400
          ENDIF
C
C Get RAMCheck and MASSPower modes
C TEMP2 still has byte 3
C
          RAMCheck      = BTEST( TEMP2, 1 )             ! RAMCheck MODE
          MASSPower     = BTEST( TEMP2, 0 )             ! MASSPower ON
C
C If the number of SWICS PHA words is too large (> 168) assume this EDB is bad
C and get another.  Increment JJ (minor frame counter) and go to label 1400
C to do this.  Count this EDB as skipped.
C
          TEMP2         = DR_MinFr_SMS(  8, JJ )
          TEMP2         = TEMP2 .AND. 'FF'X
          IF( (TEMP2 .NE. 255).AND.(TEMP2.GT.168) ) THEN
            CALL WIND_REPORT(
     &        'too many SWICS PHA words, value is !UL, maximum is 168',
     &        1, TEMP2 )
            EDB_Skip = EDB_Skip + 1
            EDB_Lost = EDB_Lost - 1
            JJ = JJ + 1
            MSpin = TEMP4
            GO TO 1400
          ENDIF
C
C If the number of STICS PHA words is too large (> 152) assume this EDB is bad
C and get another.  Increment JJ (minor frame counter) and go to label 1400
C to do this.  Count this EDB as skipped.
C
          TEMP2         = DR_MinFr_SMS(  9, JJ )
          TEMP2         = TEMP2 .AND. 'FF'X
          IF( (TEMP2 .NE. 255).AND.(TEMP2.GT.152) ) THEN
            CALL WIND_REPORT(
     &        'too many STICS PHA words, value is !UL, maximum is 152',
     &        1, TEMP2 )
            EDB_Skip = EDB_Skip + 1
            EDB_Lost = EDB_Lost - 1
            JJ = JJ + 1
            MSpin = TEMP4
            GO TO 1400
          ENDIF
C
C If the number of MASS PHA words is too large (> 381) assume this EDB is bad
C and get another.  Increment JJ (minor frame coutner) and go to label 1400
C to do this.  Count this EDB as skipped.
C
          TEMP2         = DR_MinFr_SMS( 11, JJ )
          TEMP2         = TEMP2 .AND. 'FF'X
          TEMP          = ISHFT( TEMP2, 1 )

          TEMP2         = DR_MinFr_SMS( 12, JJ )
          CALL MVBITS( TEMP2, 7, 1, TEMP, 0 )    ! Number of PHA-words
          IF( TEMP.GT.381 ) THEN
            CALL WIND_REPORT(
     &        'too many MASS PHA words, value is !UL, maximum is 381',
     &        1, TEMP )
            EDB_Skip = EDB_Skip + 1
            EDB_Lost = EDB_Lost - 1
            JJ = JJ + 1
            MSpin = TEMP4
            GO TO 1400
          ENDIF
C
C Since MSpin was not too large, save the previous value as O_MSpin
C
          O_MSpin   = TEMP4
          O_SpinCnt = SpinCount
          EDB_Sum   = EDB_Sum + 1
	  Curr_Spin = MSpin
C
          IF(DIAG .LE. 3) WRITE( IPRNT, 140 ) ' NEW ', MSpin, JJ
          TEMP2         = DR_MinFr_SMS(  7, JJ )                ! BYTE 4  - Cont. Spin Counter
          SpinCount     = IAND( TEMP2, 'FF'X )
C
C The code once apparently assumed that the first value of MSpin in the first
C EDB would never be 0. The code that follows allows for that possibility.
C
          IF( FirstEDB ) THEN
            O_MSpin = MSpin - 1
            IF(MSpin .EQ. 0)O_MSpin = 59
            O_SpinCnt = SpinCount - 1
            IF(SpinCount .EQ. 0) O_SpinCnt = 255
          ENDIF
C
C If MSpin and O_MSpin differ then some spins are missing.  Report this.
C
          IF( MSpin .NE. MOD( O_MSpin+1, 60 ) ) THEN
              IF(DIAG .LE. 4) THEN
                  WRITE( IPRNT, * )   ' MISSING SPIN!  SCI_rec=',Sci_rec_cntr,', MSpin=',MSpin,', JJ=',JJ
                  WRITE( IPRNT, 900 )   '   MSpin =', SCI( EDBset ).EDB( MSpin ).MeaSpin
                  WRITE( IPRNT, 900 )   ' O_MSpin =', O_MSpin
                  WRITE( IPRNT, 900 )   'SpinCount=', SCI( EDBset ).EDB( MSpin ).SpinCnt
                  WRITE( IPRNT, 900 )   'O_SpinCnt=', O_SpinCnt
              ENDIF
C
C Found missing spins, possible data gap
C Since we don't know the new Science Record counter value we can't output the
C message just yet
C Save information about last good Science Record Counter and Measured Spin to
C be output later
C
              MISSING_SPIN = .TRUE.
              Prev_Sci_rec_cntr = Sci_rec_cntr
              Prev_O_MSpin = O_MSpin
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
C This code formerly had a bug.  If Mspin .GT. O_MSpin+1 it worked okay, but if
C MSpin .LT. O_MSpin+1 it marked ALL spins (from O_MSpin+1 through 59 and from
C 0 through MSpin-1) of the CURRENT EDB as missing.
C
C To correct this, test to see if O_MSpin is 59.  If it is, we have just
C finished marking the last spin of a science record as bad.  Therefore, we
C need to output this science record, and move the data down, so that we don't
C erroneosly mark the good spins at the beginning of that sci-rec as bad.
C Instead, we will mark the spins of the NEXT sci-rec as bad. 
C
C If we have a data gap that spans science records, mark the HDB's for those
C records as missing, since they are not there. If we have a data gap, then
C an HDB, then another data gap, do not flag that HDB since it was actually 
C present.
C
C Do the same thing if O_SpinCnt is 0. The increment to O_MSpin has been made
C unconditional, because the former IF condition could never be fufilled.
C
          DO WHILE ( MSpin .NE. MOD( O_MSpin+1, 60 ) )
              O_MSpin = MOD( O_MSpin+1, 60 )
              IF( (( O_SpinCnt+1 ) .AND. MAXSpCnt) .NE. SpinCount ) O_SpinCnt = ( O_SpinCnt+1 ) .AND. MAXSpCnt
              Curr_Spin = O_MSpin
              Curr_Sci_Rec_Number = Sci_Rec_Cntr
              CALL MARK_MISSING( SCI( EDBset ).EDB( O_MSpin ) )
              IF (O_MSpin.EQ.59) THEN
    	        IF (.NOT. Last_Was_HDB) HDB(EDBSet).Qual = 8
                CALL WIND_OUTPUT_L1
                Last_Was_HDB = .FALSE.
              ENDIF
              EDB_Lost = EDB_Lost + 1
              IF(DIAG .LE. 3) WRITE( IPRNT, 900 )  ' EDB_Lost =', EDB_Lost
          END DO ! ( MSpin .NE. MOD( O_MSpin+1, 60 ) )

          DO WHILE ( SpinCount .NE. ( O_SpinCnt+1 .AND. MAXSpCnt ) )
              O_MSpin = MOD( O_MSpin+1, 60 )
              O_SpinCnt = ( O_SpinCnt+1 ) .AND. MAXSpCnt
              Curr_Spin = O_MSpin
              Curr_Sci_Rec_Number = Sci_Rec_Cntr
              CALL MARK_MISSING( SCI( EDBset ).EDB( O_MSpin ) )
              IF (O_MSpin.EQ.59) THEN
    	        IF (.NOT. Last_Was_HDB) HDB(EDBSet).Qual = 8
                CALL WIND_OUTPUT_L1
                Last_Was_HDB = .FALSE.
              ENDIF
              EDB_Lost = EDB_Lost + 1
              IF(DIAG .LE. 3) WRITE( IPRNT, 900 )  ' EDB_Lost =', EDB_Lost
          END DO ! ( SpinCount .NE. ( O_MSpinCnt+1 .AND. MAXSpCnt ) )
C
C Update current spin variable to present value of MSpin (measured spin) to
C indicate we are processing this spin.  Also update the current sciecne record
C counter variable in case it was modified.
C
          Curr_Spin = Mspin
          Curr_Sci_Rec_Number = Sci_Rec_Cntr
C
C If we had any missing spins, which may have extended over a Science Record
C boundry, report the condition here.  We previously noted some spins were
C missing (flag MISSING_SPIN was set), and saved information at that point.
C After calling WIND_REPORT restore values of Curr_Spin and Curr_Sci_Rec_Number
C
          IF (MISSING_SPIN) THEN
            Curr_Sci_Rec_Number = Prev_Sci_rec_cntr
            Curr_Spin = Prev_O_MSpin
            IARRAY(1) = Sci_Rec_Cntr
            IARRAY(2) = MSpin
            CALL WIND_REPORT(
     &        'data gap found, next good data at sci rec !UL, spin !UL',
     &        2, IARRAY )
            MISSING_SPIN = .FALSE.
            Curr_Sci_Rec_Number = Sci_Rec_Cntr
	    Curr_Spin = Mspin
          END IF
C
C Check for missing HDBs
C
C Since the HDB follow the last EDB for a given science record we need to test
C whether or not an HDB was found *after* a science record is complete.  Since
C WIND_OUTPUT_L1 is called for each completed sci rec, it also sets the logical
C variable New_Sci_Rec to .TRUE. for us.  (Note: Finishing a sci rec and
C starting the next sci rec are logically equivalent for these purposes.)
C
C Test the flag New_Sci_Rec.  It can be set in one of two ways:
C   1) we finished processing spin 59 of a sci rec and called WIND_OUTPUT_L1
C   2) there was a data gap which bridged the boundary between sci recs,
C      causing a new sci rec to be started
C
C In either case we want to be sure to detect a missing HDB.  In the first
C case if an HDB is missing the variable Last_Was_HDB would still be .FALSE.,
C since it is set to .TRUE. only after an HDB processed.  In the second case
C WIND_OUTPUT_L1 will have been called to complete the previous (truncated)
C science record, thus setting New_Sci_Rec, and Last_Was_HDB would still be
C .FALSE. since there was no intervening HDB.  We are guaranteed this is so
C since HDBs come between science records: after spin 59 of the science record
C with which it is associated and before spin 0 of the succeeding science
C record. 
C
C If we determine an HDB is missing, report this condition.  Also set the
C quality flag in the HDB to 8 (missing), since the default value for this
C field is 0, which indicates no errors.  Even though we're about to fill in
C science record EDBset, set this flag in HDB(EDBset-1), since the HDB is
C missing for the previous science record, which has already been moved down
C one position by the call to WIND_OUTPUT_L1.
C
          IF (New_Sci_Rec) THEN
    	    IF (.NOT. Last_Was_HDB) THEN
              Curr_Sci_Rec_Number = Prev_Sci_rec_cntr
              Curr_Spin = -1
              CALL WIND_REPORT( 'missing HDB', 0, IARRAY )
              Curr_Sci_Rec_Number = Sci_Rec_Cntr
              Curr_Spin = Mspin
              HDB( EDBset - 1 ).BDH.Qual = 8
            ENDIF
          ELSE	! .NOT. New_Sci_Rec  (should never happen)
            IF (Last_Was_HDB) THEN
              CALL WIND_REPORT('got HDB in middle of series of EDBs', 0, IARRAY )
            ENDIF
          ENDIF                                   
C
C Now that we're done testing these flags reset (clear) them
C
          Last_Was_HDB = .FALSE.
          New_Sci_Rec = .FALSE.
C
C Start storing information into the current science record and EDB.
C This code was moved here, after the above two loops, in case a science record
C was written out and the others moved down.  If done before the above loops
C an earlier spin would be overwritten.
C
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
          SCI( EDBset ).EDB( MSpin ).TLM_Mode   = DR_TLM_Mode
          SCI( EDBset ).EDB( MSpin ).SpinCnt    = SpinCount

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
                CALL WIND_REPORT(
     &            'too many SWICS PHA words, value is !UL, maximum is 168',
     &            1, SCI( EDBset ).EDB( MSpin ).SWICS_Len )
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
                CALL WIND_REPORT(
     &            'too many STICS PHA words, value is !UL, maximum is 152',
     &            1, SCI( EDBset ).EDB( MSpin ).STICS_Len )
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
                CALL WIND_REPORT(
     &            'too many MASS PHA words, value is !UL, maximum is 381',
     &            1, SCI( EDBset ).EDB( MSpin ).MASS_Len )
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

C      Display for testing some of CORE data contents
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

C      Maintain Quality Flags for EDBs
          IF( FirstEDB )   THEN
              IF( MSpin .NE. 0 )   THEN
                  DO II = 0, MSpin
                      CALL MARK_MISSING( SCI(EDBset).EDB(II) )
                      SCI( EDBset ).EDB( II ).Qual = 4     ! FILL indicator
                  ENDDO
              ENDIF
              IF(DIAG .LE. 3) WRITE( TTOUT, 165 )   ' First EDBs indicated as FILL '
          ENDIF

C  MOVE RawData / SubFrame to EDB
          IF( MSpin .EQ. 0 ) RECFLAG = RECFLAG + 1D0
          T1 = (JJ * 1D0) * DELTATime - DELTAT(MSpin)

          SCI( EDBset ).EDB( MSpin ).EPOCH = DR_Beg_EPOCH + T1
          CALL EPOCH_TO_ITIME( SCI(EDBset).EDB(MSpin).EPOCH,
     :                         SCI(EDBset).EDB(MSpin).W_Time, ISTAT )
          WRITE(181,*)SCI(EDBset).EDB(MSpin).W_TIME.HR, SCI(EDBset).EDB(MSpin).W_TIME.MIN, SCI(EDBset).EDB(MSpin).W_TIME.SEC
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
              IF(DIAG .LE. 2) WRITE( IPRNT, 131 ) SRNumber, MSpin,
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
                      READ( L0_Lun, 401, END=9999, ERR=9998) IREC,C_DR2_BUFFER(1:IREC)

                      CALL FIXUP( DR2_Beg_SC_Clock )

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

                  CALL WIND_SAVE_HK( DR_Beg_MajFr_Cnt, HKset(EDBset), SYNC_WORD )

                  ISTAT = 100
                  IF (.NOT. NRT_MODE) CALL RECORD_PC_DATA( DR_Beg_EPOCH, ISTAT )            ! Record Hk data

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
                  READ( L0_Lun, 401, END=9999, ERR=9998) IREC,C_DR2_BUFFER(1:IREC)

                  CALL FIXUP( DR2_Beg_SC_Clock )

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
                  JJ = JJ + 1			! Added to force scan for next EDB
                  EDB_Skip = EDB_Skip + 1
                  CALL WIND_REPORT( 'core qual bad, skipped', 0, JJ )
              ELSE
                  SCI( EDBset ).EDB( MSpin ).CORE_Qual = 0
                  DO II = 0, EDB_Core_Size
                      CORE_data( II, MSpin, EDBset ) = DR_MinFr_SMS( 3 + II, JJ )
                      EDBpos = EDBpos + 1
                  ENDDO
                  IF(DIAG .LE. 5) WRITE( TTOUT, 165 )   ' CORE data transferred '
                  IF (PRINTLZ_MODE) THEN
                    ATIME=WIND_FORMAT_TIME( SCI(EDBset).EDB(MSpin).W_Time)
                    WRITE(28,2025) MSPIN, ATIME, (CORE_data( II, MSpin, EDBset ) ,II=0,EDB_Core_Size )
 2025               FORMAT(' EDB Header for spin',I3,A30,/,1X,<EDB_Core_Size + 1>Z3.2)
                  ENDIF
                  IF(DIAG .LE. 3) WRITE( IPRNT, 206 )  'CORE', II, MSpin, EDBset, EDBpos, JJ
  206 FORMAT( 1H , A, ' POINTER:', I4, '  MSpin:', I3, '  EDBset:', I3,
     :          '  EDBpos:', I4, '  JJ:', I3 )

                  SCI( EDBset ).EDB( MSpin ).SW_BR_Qual         = 8
                  SCI( EDBset ).EDB( MSpin ).SW_MR_Qual         = 8
                  SCI( EDBset ).EDB( MSpin ).SW_ME1_Qual        = 8
                  SCI( EDBset ).EDB( MSpin ).SW_ME2_Qual        = 8
                  SCI( EDBset ).EDB( MSpin ).SW_ER_Qual         = 8
C   Check IF SWICS is ON
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
                      IF (PRINTLZ_MODE) THEN
                        WRITE(28,2045) MSPIN,SWICS_RATE( EDBset ).EDB( MSpin ).RATE
 2045                   FORMAT(' SWICS Rates for spin',I3,/,(1X,20Z3.2))
                      ENDIF
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
                      IF (PRINTLZ_MODE) THEN
                        WRITE(28,2055) MSPIN, SCI( EDBset ).EDB( MSpin ).SWICS_Len, 
     &                                 (SWICS_PHA( EDBset ).EDB( MSpin ).PHA( ijk),ijk=0,4*SCI( EDBset ).EDB( MSpin ).SWICS_Len - 1)
 2055                   FORMAT(' SWICS PHA for spin',I3,'          # words =',I5,/,(1X,20Z3.2))
                      ENDIF
                  ENDIF

                  SCI( EDBset ).EDB( MSpin ).ST_HMR_Qual        = 8
                  SCI( EDBset ).EDB( MSpin ).ST_SMR_Qual        = 8
                  SCI( EDBset ).EDB( MSpin ).ST_BR_Qual         = 8
                  SCI( EDBset ).EDB( MSpin ).ST_OMR_Qual        = 8
                  SCI( EDBset ).EDB( MSpin ).ST_ER_Qual         = 8
C   Check IF STICS is ON
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
                      IF (PRINTLZ_MODE) THEN
                        WRITE(28,2065) MSPIN,STICS_RATE( EDBset ).EDB( MSpin ).RATE
 2065                   FORMAT(' STICS Rates for spin',I3,/,(1X,20Z3.2))
                      ENDIF
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
                      IF (PRINTLZ_MODE) THEN
                        WRITE(28,2075) MSPIN,SCI( EDBset ).EDB( MSpin ).STICS_Len, 
     &                                 (STICS_PHA( EDBset ).EDB( MSpin ).PHA( ijk),ijk=0,4*SCI( EDBset ).EDB( MSpin ).STICS_Len - 1)
 2075                   FORMAT(' STICS PHA for spin',I3,'         # words =',I5,/,(1X,20Z3.2))
                      ENDIF
                  ENDIF

                  SCI( EDBset ).EDB( MSpin ).MA_UER_Qual        = 8
                  SCI( EDBset ).EDB( MSpin ).MA_SER_Qual        = 8
                  SCI( EDBset ).EDB( MSpin ).MA_BR_Qual         = 8
                  SCI( EDBset ).EDB( MSpin ).MA_MR_Qual         = 8
C   Check IF MASS is ON
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
                      IF (PRINTLZ_MODE) THEN
                        WRITE(28,2085) MSPIN,MASS_RATE( EDBset ).EDB( MSpin ).RATE
 2085                   FORMAT(' MASS Rates for spin',I3,/,(1X,20Z3.2))
                      ENDIF
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
                      IF (PRINTLZ_MODE) THEN
                        WRITE(28,2095) MSPIN,SCI( EDBset ).EDB( MSpin ).MASS_Len, 
     &                                 (MASS_PHA( EDBset ).EDB( MSpin ).PHA( ijk),ijk=0,2*SCI( EDBset ).EDB( MSpin ).MASS_Len - 1)
 2095                   FORMAT(' MASS PHA for spin',I3,'         # words =',I5,/,(1X,20Z3.2))
                      ENDIF
                  ENDIF
              ENDIF
              EDB_done = .TRUE.
              ISTAT = MSpin
              IF (.NOT. NRT_MODE) CALL RECORD_PC_DATA( SCI( EDBset ).EDB( MSpin ).EPOCH, ISTAT )    ! Record EDB data
              IF(DIAG .LE. 5) WRITE( TTOUT, 165 )   ' EDB done '
          ENDDO ! WHILE ( .NOT.EDB_done )

C
C Come here when we are done processing an EDB
C
 2500 CONTINUE
C
C Report a change in bit rate as an interesting condition, but only if the
C current spin is good (quality flag field is zero).  Compare its bit rate with
C the bit rate of the previous good spin, which was saved in O_Good_Bit_Rate .
C If this is the first then EDB O_Good_Bit_Rate is meaningless, so don't do
C anything.
C
          IF (SCI(EDBset).EDB(MSpin).BitRate .NE. O_Good_Bit_Rate .AND.
     &        SCI(EDBset).EDB(MSpin).Qual .EQ. 0                  .AND.
     &        .NOT. FirstEDB ) THEN
            IF (SCI(EDBset).EDB(MSpin).BitRate) THEN
              CALL WIND_REPORT('bit rate changed, new bit rate is HI', 0, IARRAY )
            ELSE
              CALL WIND_REPORT('bit rate changed, new bit rate is LOW', 0, IARRAY )
            ENDIF
          ENDIF
C
C Update previous (old) good bit rate variable only if this spin has no quality
C flags set (it is good).
C
          IF (SCI(EDBset).EDB(MSpin).Qual .EQ. 0) THEN
            O_Good_Bit_Rate = SCI(EDBset).EDB(MSpin).BitRate
          ENDIF
C
C Done with EDB, clear first EDB flag
C
          FirstEDB = .FALSE.
C
C Check measured spin value (MSpin).  If it is 59 we just finished the last
C spin of a Science Record.  Also, if MSpin is less than the previous MSpin
C value (saved in O_MSpin) and the O_MSpin value is not 59, we also completed a
C Science Record.
C
          IF( ( MSpin .EQ. 59 ) .OR.
     +          ( ( MSpin .LE. O_MSpin ) .AND. ( O_MSpin .NE. 59 ) ) )  THEN
C
C Come here if we decided a Science Record has been filled.  That is, all 60
C of its EDBs/spins - those that exist - have been processed and stored.
C
C  if   1 SCI-Record is filled          prepare the next
C
C  if   2 SCI-Records are filled        prepare the next
C
C  if   3 SCI-Records are filled        do all data manipulations
C                                       save 1 SCI-Record to L1-file
C                                       shift record 2 to record 1
C                                       shift record 3 to record 2
C
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
C science record except for the HDB.
C We must still do things with it before writing it out.  They are:
C  1) concatenate the STICS data
C  2) shift data between spins/science records
C  3) store the HDB into memory to complete the science record
C If in PRINTLZ mode #3 need not be done.
C
C Since three science records are held in memory, the write is only done
C on the third call to WIND_OUTPUT_L1.  By this time the HDB for the first
C science record, if found, has been processed and filled in.
C
C Finally, after the L1 record is written, the other two science records' data
C must be moved down within the SCI (and other) arrays and the vacated data
C structures zero filled.
C
	      CALL WIND_OUTPUT_L1
C
C Do the following only if we are in dump mode (PRINTLZ_MODE is .TRUE.)
C
C As a convenience to users, close the output dump file and open another dump
C file with a different name, sequencing the names (01, 02, etc.)  This makes
C them easier to edit should an entire LZ file be dumped.
C Do this every DUMP_FILE_L1_RECS L1 records.
C
              IF ( PRINTLZ_MODE  .AND.
     &             MOD (SCI_REC_CNTR, DUMP_FILE_L1_RECS) .EQ. 0 ) THEN
                DUMP_SEQUENCE = DUMP_SEQUENCE + 1
                INQUIRE(UNIT=28,NAME=DUMPNAME)
                CLOSE(28)
                KK = INDEX(DUMPNAME, ';')        ! will always have ;
C
C If the file extension ends with a digit, store updated sequence
C number (effectively incrementing it) to form sequenced filename.
C If not extend the filename by adding the sequence number on the end.
C
                IF (INDEX('0123456789', DUMPNAME(KK-1:KK-1)) .EQ. 0) THEN
                  DUMPNAME(KK:) = '1;'
                  KK = KK + 1
                ENDIF
                WRITE( DUMPNAME(KK-1:KK-1), '(I1)' ) MOD(DUMP_SEQUENCE,10)
                WRITE(TTOUT,1001) DUMP_SEQUENCE,DUMPNAME(1:KK)
1001            FORMAT(' Closing previous dump file, opening dump file ',I2/
     &                 ' New dump filename = ',A/)
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
                  READ( L0_Lun, 401, END=9999, ERR=9998) IREC,C_DR2_BUFFER(1:IREC)

                  CALL FIXUP( DR2_Beg_SC_Clock )

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
C If this is the first HDB in the LZ file we want to set Last_Was_HDB to
C .FALSE. so we don't falsely report there being consecutive HDBs without
C intervening EDBs.
C
C Also, if this is the first HDB in the LZ file *and* there are no preceeding
C EDBs (ie, the first sync word in the LZ file is for an HDB), mark all spins
C of SCI(1) as missing, call WIND_OUTPUT_L1 to handle bookkeeping (including
C incrementing EDBset to 2), and process the HDB normally.  The HDB will be put
C into HDB(1), where it belongs, and the subscript error (of using HDBset as
C EDBset-1 when EDBset is 1) is fixed.
C
          IF (FirstHDB) THEN
C
            IF (FirstEDB) THEN
              Curr_Sci_Rec_Number = Sci_Rec_Cntr
              Curr_Spin = -1
              CALL WIND_REPORT('initial HDB with no EDBs, created science record with only an HDB',
     &          0, HDBset )
              DO II = 0, 59
                CALL MARK_MISSING( SCI(1).EDB(II) )
              ENDDO
              CALL WIND_OUTPUT_L1
            ENDIF
C
            Last_Was_HDB = .FALSE.
            FirstHDB = .FALSE.
          ENDIF
C
C Array index HDBset says which science record to associate with this EDB. 
C Except in unusual circumstances this will always be EDBset - 1.
C
          HDBset = EDBset - 1
C
C Test the flag Last_Was_HDB.  If it is .TRUE. the last data block processed
C was an HDB.  Since this data block is also an HDB this means there were no
C intervening EDBs *at all*.  That is, exactly 60 EDBs are missing. 
C Do two things: 
C  1) Report this condition
C  2) Store the current HDB into position EDBset, *not* EDBset-1.
C
C The second action is required to avoid storing the current HDB, which has
C *no* associated EDBs, with the previous science record, which does have EDBs.
C It should instead be stored in the EDBset position.  When the next EDB is
C processed all 60 intervening EDBs (spins) in EDBset will be marked as missing
C and WIND_OUTPUT_L1 will be called, a side effect of which will be to move the
C science records (and their HDB) down one position, to EDBset-1.  By storing
C the current HDB into position EDBset we assure it remains associated with the
C proper science record.
C
C Do this by setting HDBset to EDBset.
C
          IF (Last_Was_HDB) THEN
            CALL WIND_REPORT('consecutive HDBs with no intervening EDBs',
     &        0, EDBset - 1 )
            HDBset = EDBset
          ENDIF
C
C Now set flag Last_Was_HDB to .TRUE. to indicate to the rest of the program
C the last data block processed was an HDB.
C
          Last_Was_HDB = .TRUE.
C
C Validate number of subframes per block, which for an HDB should always be 7
C
	  IF (SFperBlock .NE. 7) THEN
	    CALL WIND_REPORT( 'illegal value of !SL for SFperBlock, ignoring this HDB', 1, SFperBlock )
            HDB(HDBset).Qual = DR_MinFr_Qual( JJ ) .OR. 32	! BIT 5 = CORRUPTED EDB OR HDB HEADER
            JJ = JJ + 1
            GO TO 1400
	  ENDIF
C
C Assemble science record number from HDB
C
          SRNumber      = DR_MinFr_SMS( 8, JJ )
          SRNumber      = SRNumber .AND. 'FF'X
          TEMP4         = DR_MinFr_SMS( 7, JJ )
          SRNumber      = JIOR( ISHFT( (TEMP4 .AND. 'FF'X), 8 ), SRNumber )
          TEMP4         = DR_MinFr_SMS( 6, JJ )
          SRNumber      = JIOR( ISHFT( (TEMP4 .AND. 'FF'X),16 ), SRNumber )
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

          IF(DIAG .LE. 3) WRITE( IPRNT, 320 )  JJ, SRNumber, HDBTime
  320 FORMAT( ' Start of HDB at SubFr.', I3,
     +        ',  SRNumber =', I8, ',  HDBTime =', Z8.8, / )

          HDB_Start     = JJ
          HDB_End       = HDB_Start + SFperBlock - 1
          HDB_Pos       = 0
          HDB( HDBset ).BDH.Qual = 0
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

                  CALL WIND_SAVE_HK( DR_Beg_MajFr_Cnt, HKset(EDBset), SYNC_WORD )

                  ISTAT = 100
                  IF (.NOT. NRT_MODE) CALL RECORD_PC_DATA( DR_Beg_EPOCH, ISTAT )            ! Record Hk data

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
          ISTAT = 200
          IF (.NOT. NRT_MODE) CALL RECORD_PC_DATA( HDB( HDBset ).BDH.SC_EPOCH, ISTAT )              ! Record HDB data
C
C Update JJ to point to next minor frame
C Don't worry about it being larger than MAXSFrame, as we'll go to statement
C label 1400 and it'll check that for us
C
          JJ = HDB_Start + SFperBlock
          IF(JJ.NE.MAXSFrame)THEN
            JJ = MOD(JJ, MAXSFrame)
          ENDIF
C
C We just finished filling the HDB structure (actually an array 3 long indexed
C by EDBset-1).  The HDB structure is now indexed by EDBset to guarantee that
C each HDB is output along with its appropriate EDB. Since HDBs are written out 
C with Science Records, whose writes are deferred to allow shifting of times 
C (see earlier comments), we must keep three of them and allow WIND_STORE_L1 to 
C write the HDB out with its associated EDB.
C
          IF (PRINTLZ_MODE) THEN
            WRITE(28,9995) SRNumber, HDB( HDBset ).BDH.DAT
            WRITE(28,9996)
 9995       FORMAT(' **************************************************************************'/
     &             ' HDB for SR',I7,/,(1X,20Z3.2))
 9996       FORMAT(' **************************************************************************')
          ENDIF
      ENDIF ! IF( .NOT.HDBFlag )

C
C We have finished processing a data block, either an EDB or an HDB.
C
C Check if we are done reading L0 data by testing against user-supplied stop
C time.  Branch back to label 1400 if there is more to do.
C
      IF( DYRDAY .GT. STPTime ) GOTO 9999
      GOTO 1400
C
C All done, wind down processing.
C
9997  CONTINUE
9998  CONTINUE
9999  CONTINUE
      ISTAT = 900
      IF (.NOT. NRT_MODE) CALL RECORD_PC_DATA( HDB( EDBset - 1 ).BDH.SC_EPOCH, ISTAT )          ! Close Recording File
99090 CONTINUE
C
      CLOSE ( L0_Lun )
C
      IF (PRINTLZ_MODE) THEN
        CLOSE ( 28 )
      ELSE
        CLOSE ( L1_lun )
      ENDIF
C
C Update L1 file header, reopen L1 file, and write header back to L1 file
C Use REWRITE statement since a normal WRITE truncates the file
C
      IF (.NOT. PRINTLZ_MODE) THEN
        CALL WIND_BUILD_FILE_HEADER
        OPEN( L1_lun, FILE=FIL1NAME(1:NCF), STATUS='OLD', RECL=32768,
     1                IOSTAT=ISTAT, ERR=99001 )
        CALL WIND_WRITE_L1_HEADER( L1_lun, 3 )
        CLOSE( L1_lun )
      ENDIF
      GOTO 99010
C
99001 CONTINUE
      CALL ERRSNS(FORTERR,RMSSTC,RMSSTV,LUN,VAXERR)
      WRITE( TTOUT, 99002 ) ISTAT,FIL1NAME(1:NCF)
99002 FORMAT(' *** ERROR REOPENING L1 FILE, ISTAT = ',I3/' *** FILENAME = ',A)
      CALL LIB$STOP(%VAL(RMSSTV))
C
C Compute and print program statistics
C
C Skip this if in PRINTLZ mode
C
99010 CONTINUE
      IF (.NOT. PRINTLZ_MODE) THEN
        CALL TIME(PROGRAM_END_TIME)
        WRITE( TTOUT, 900 )   'EDB Count = ', EDB_Sum
        WRITE( TTOUT, 900 )   'EDBs lost = ', EDB_Lost
        WRITE( TTOUT, 900 )   'EDB Skips = ', EDB_Skip
        IF(DIAG .LE. 5) THEN
          WRITE( IPRNT, 900 )   'EDB Count = ', EDB_Sum
          WRITE( IPRNT, 900 )   'EDBs lost = ', EDB_Lost
          WRITE( IPRNT, 900 )   'EDB Skips = ', EDB_Skip
  900     FORMAT( 1X, A, I6 )
        ENDIF
C
        WRITE( TTOUT, 1003) PROGRAM_START_TIME,PROGRAM_END_TIME
1003    FORMAT(/' Program started at  ',A/
     &          ' Program finished at ',A)
C
C Write statistics to log file
C
        CALL WRITE_SUMMARY( FILENAME,START_DATE_TIME(1:11),
     &                     PROGRAM_START_TIME,PROGRAM_END_TIME,
     &                     L1_recs_written,EDB_Sum,EDB_Lost,EDB_Skip )
      ENDIF
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
C to .TRUE..  Also, if the high bit of SCClock(1) is set - which it never
C should be - set SWAP_PARTS to .TRUE..  If neither of these conditions holds
C set SWAP_PARTS to .FALSE..
C
C For all calls test SWAP_PARTS.  If it is set exchange the two 32-bit parts of
C SC_Clock.
C
C While this will not catch all errors, it will catch the more obvious one
C and allow WIND_CD_READ to operate
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
C current year, set SWAP_PARTS to .TRUE..  Also, if the high bit of SCClock(1)
C is set - which it never should be - set SWAP_PARTS to .TRUE..  If neither of
C these conditions holds set SWAP_PARTS to .FALSE..
C Reset the first-time-called flag.
C
C While this will not catch all errors, it will catch the more obvious ones.
C Further refinements will have to wait for a later date.
C
        SWAP_PARTS = SC_Year .GT. THIS_YEAR .OR. BTEST(SC_Clock(1),31)
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

      SUBROUTINE WRITE_SUMMARY( FILENAME,START_DATE,START_TIME,END_TIME,
     &                   	L1_RECORDS,EDB_Sum,EDB_Lost,EDB_Skip )
C
C   This subroutine opens the WIND summary log file, writes a single line of
C information into it describing this run, and closes it.  If the file does not
C exist it is created.  If the file is locked by another user (eg, another copy
C of WIND_CD_READ is running), wait for 1 second for the user to close the file
C then try again.
C
C The following information is written into the log file: filename, current date,
C the start and finish times, the number of L1 records written, the number of
C EDBs written, the number of EDBs lost, and the number of EDBs skipped.  The
C format of the log file entry is given below.
C
C                              Date         Start     Finish      L1     EDB     EDBs  EDBs
C Filename                     Processed    Time      Time      Records  Count   Lost  Skipped
C WI_LZ_SMS_yyyymmdd_Vxx.DAT   nn-mmm-yyyy  mm:hh:ss  mm:hh:ss
C 1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
C          1         2         3         4         5         6         7         8         9        10
C
      IMPLICIT NONE
      INCLUDE '($FORIOSDEF)/NOLIST'
      CHARACTER*(*) FILENAME,START_DATE,START_TIME,END_TIME
      INTEGER L1_RECORDS,EDB_Sum,EDB_Lost,EDB_Skip
C
      INTEGER NTRY,LOG_UNIT,ISTAT,IPOS
      PARAMETER (LOG_UNIT = 18)
      CHARACTER MODE*3,ACC_MODE*10,SHORT_NAME*80
C
C Try to open log file for appending
C If file is already in use, wait 1 sec and try again
C
      NTRY = 1
      MODE = 'OLD'
      ACC_MODE = 'APPEND'
C
100   IF (NTRY .GT. 2) GOTO 910
      OPEN( UNIT            = LOG_UNIT,
     &      FILE            = 'WIND_LOG',
     &      DEFAULTFILE     = 'DATA_DIR:.TXT',
     &      STATUS          = MODE,
     &      ACCESS          = ACC_MODE,
     &      RECORDSIZE      = 132,
     &      CARRIAGECONTROL = 'LIST',
     &      IOSTAT          = ISTAT,
     &      ERR = 900 )
C
C Construct shorter filename (name portion only, no directory)
C
      SHORT_NAME = FILENAME
      IPOS = INDEX( SHORT_NAME, ']' )
      IF (IPOS .NE. 0) SHORT_NAME = SHORT_NAME(IPOS+1:)
      IPOS = INDEX( SHORT_NAME, '.' )
      IF (IPOS .NE. 0) SHORT_NAME = SHORT_NAME(:IPOS-1)
C
C Write info into log file
C
      IPOS = INDEX( SHORT_NAME, ' ' )
      WRITE(LOG_UNIT,101) SHORT_NAME(1:IPOS),START_DATE,START_TIME,END_TIME,
     &                    L1_RECORDS,EDB_Sum,EDB_Lost,EDB_Skip
101   FORMAT(A,T30,A11,2X,A8,2X,A8,2X,I5,2X,I5,2X,I5,2X,I5)
      RETURN
C
C Come here if OPEN error and test status.  If file did not exist change OPEN
C parameters and try again.
C
900   IF (ISTAT .EQ. FOR$IOS_FILNOTFOU) THEN
        MODE = 'NEW'
        ACC_MODE = 'SEQUENTIAL'
        GOTO 100
      ENDIF
C Assume file was opened by another job.  Wait one second and try once more.
      IF (NTRY .GT. 2) GOTO 910
      NTRY = NTRY + 1
      CALL LIB$WAIT(1.0)
      GOTO 100
C
910   TYPE *,'*** Cannot open to LOG file, Fortran error code',ISTAT
      RETURN
      END

      SUBROUTINE MARK_MISSING( ONE_EDB )
C
C Set each of the quality flags in this EDB (aka spin) to indicate the spin is
C missing.  Note the argument ONE_EDB is of data type STRUCTURE /E/.
C
      INCLUDE 'INC_DIR:EDBDEF.INC/NOLIST'
      RECORD /E/ ONE_EDB
C
      ONE_EDB.Qual = 8
      ONE_EDB.CORE_Qual = 8
      ONE_EDB.SW_BR_Qual = 8
      ONE_EDB.SW_MR_Qual = 8
      ONE_EDB.SW_ME1_Qual = 8
      ONE_EDB.SW_ME2_Qual = 8
      ONE_EDB.SW_ER_Qual = 8
      ONE_EDB.SW_P_Qual = 8
      ONE_EDB.ST_HMR_Qual = 8
      ONE_EDB.ST_SMR_Qual = 8
      ONE_EDB.ST_BR_Qual = 8
      ONE_EDB.ST_OMR_Qual = 8
      ONE_EDB.ST_ER_Qual = 8
      ONE_EDB.ST_P_Qual = 8
      ONE_EDB.MA_UER_Qual = 8
      ONE_EDB.MA_SER_Qual = 8
      ONE_EDB.MA_BR_Qual = 8
      ONE_EDB.MA_MR_Qual = 8
      ONE_EDB.MA_P_Qual = 8
      RETURN
      END
