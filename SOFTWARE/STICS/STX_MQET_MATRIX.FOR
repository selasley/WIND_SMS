CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      PROGRAM STX_MQET_MATRIX

C This program reads a WIND L1 data file and dissects the PHA data, printing 
C it in matrix form e,t or  calculated m & m/q
C 
C STICS_MQET_MATRIX created by ABG using
C                   routines from stics_mmq_table
C                             and gmqet
C
C STX_MQET_MATRIX  Created 26-MAR-1997  by K.CHOTOO  using STICS_MQET_MATRIX as template.
C
C Revision history:
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE

      INCLUDE	'INC_DIR:EDBDEF.INC/NOLIST'
      INCLUDE   'INC_DIR:L1FMT.INC/NOLIST'
      INCLUDE   '($RMSDEF)'

      INTEGER*4 STATUS
      INTEGER*4 L1_LUN/19/, OPENMODE
      INTEGER*4 ST_MONTH,ST_DAYOFMONTH,END_MONTH,END_DAYOFMONTH
      CHARACTER L1FILENAME*80
      CHARACTER FULL_L1_NAME*80
      CHARACTER FIFO*3, TGS*3, RATEFLAG*1
      CHARACTER*17 MODE(10) /'Science mode     ','Engineering mode ',
     &                       'Maneuver mode    ','Contingency mode ',
     &                       'Science mode     ','Engineering mode ',
     &                       'Maneuver mode    ','Contingency mode ',
     &                       'Transitional mode','Unknown mode     '/,
     &             MODE_AT_START

      INTEGER*4 TRIGGER_MODE_CODE		!ADC trigger logic
      CHARACTER*20 TRIGGER_MODE(0:7) /'E                    ','Tn                  ',
     &                                '(Tn.E)+(Tn.En)       ','Tn.En               ',
     &                                'E+T                  ','(E.T)+(Tn.E)+(Tn.En)',
     &                                'C.(E+T)              ','(E.!T)+(Tn.En)      '/
      LOGICAL CODE_C,CODE_C2,STARTFOUND,ENDFOUND,USE_SC_CLOCK,
     .        COMPARE_TIME_GE,GOODSPIN
      INTEGER I,J,K,L,N,NN,CNTR,ITEMP,ITEMP2,NAMELEN
      LOGICAL*1 BITRATE
      INTEGER*4 INCREMENT

C    pha word - dissected:
      INTEGER*4 START_ID_RANGE,
     .          STOP_ID,
     .          ENERGY,
     .          SECTOR,
     .          SSD_ID,
     .          TIME_OF_FLIGHT,
     .          PSTART,
     .          RANGE

C    choose data filtering options
      INTEGER*4  DATA_FILTER,		!data filter options 
     .           DATA_FILTER_COINC,	!data filtering by coincidence 
C					 conditions
     .           DATA_FILTER_DVSTEP,	!data filter by voltage step
     .           DATA_DVSTEP_MIN,	!dv step min-max values for data filter
     .           DATA_DVSTEP_MAX,
     .           DATA_FILTER_RANGE,       !range selection
     .           DATA_FILTER_RANGECHECK,  !check calculated vs. dpu range
     .           DIRECTION_FILTER,        !select data by telescope info
     .           DIRECTION_FILTER_TEL1,   ! telescope 1 id
     .           DIRECTION_FILTER_TEL2,   ! telescope 2 id
     .		 DIRECTION_FILTER_TEL3,   ! telescope 3 id
     .		 DIRECTION_SECTOR_MIN,	  !sector filter: sum sectors .ge. min and .le. max
     .		 DIRECTION_SECTOR_MAX,
     .           TELE_FLAG,		  !Telescope check (0=good, 1=bad)
     .		 TELE_ID,		  !Telescope id
     .           DATA_FILTER_MSPEED,      !speed filter
     .           DATA_MSPEED_MIN,         !	select data only if MASS solar
     .           DATA_MSPEED_MAX          !	speed is .GE. MIN and .LE. MAX

      CHARACTER*3 RONOFF, TONOFF            !ON, OFF status
      CHARACTER*10 COINC(0:2) / '       ALL',
     .                          '  M/Q-only',
     .                          '   M/Q & M'/
      CHARACTER*5 RANGESEL(0:6) /
     .                           '2 1 0' ,
     .                           '    0' ,
     .                           '  1  ' ,
     .                           '2    ' ,
     .                           '2   0' ,
     .                           '2 1  ' ,
     .                           '  1 0' /
      LOGICAL SEL_RANGE(0:2)
      CHARACTER*1 YORN			!yes or no answer

C    DPPS information
      INTEGER*4 DPPSPLUS,		!STICS +DPPS on=1, off=0
     .          DPPSMINUS,		!STICS -DPPS on=1, off=0
     .		DVSTEP			!DPPS voltage step number (0-31)

C    dpu algorithm values for e/q, mass and m/q:
      REAL*4 DPU_E_OVER_Q,
     .       DPU_MASS,
     .       DPU_MASS_OVER_Q

      INTEGER*4 DPU_NM,DPU_NQ,		!dpu calculations for m, m/q bins
     .          DPU_RANGE		!dpu calculations for range

      REAL*4    ELOW,EHIGH,TLOW,THIGH,
     .          AM(6),BM(6),
     .          C1_EQ,C2A_EQ,C2B_EQ,D1_EQ,D2_EQ,
     .          EMQ(3), BK_MQ, RKM, RKQ
      INTEGER*4 BK_C2
                             
 
C    choose output format for matrix
      INTEGER*4  MATRIX_FORMAT		!option = 1 for STANDARD NM VS NQ format
C      					!option = 2 for E VS T  format
      INTEGER*4 TABLE_FORMAT            !option for pha table listing
c
      REAL*4   AVE_SPEED,		!Average solar wind speed
     .         BULK_SPEED,		!Solar wind speed
     .         BULK_UNCER,		!Uncertainty in solar wind speed from width
     .	       HIGH_SPEED,		!Highest valid speed encountered
     .	       LOW_SPEED,		!Lowest valid speed encountered
     .         SPEED_MEAN,		!Average speed over interval
     .         SPEED_HITS,		!Number of valid speed values in interval
     .	       SPEED_SIGMA,		!Standard deviation of speed
     .	       SUM_SW_SPEED,
     .	       SUM_SQ_SPEED,		!Sum of speed squared
     .         SW_SPEED                 !chosen solar wind speed to check against                      

      INTEGER*4 BAD_SPEED/0/,		!Number of invalid speed values
     .		ERR_SPD_FLAG,		!Error speed flag
     .		FIRSTSPEED/0/		!Used to get speed at start time
      CHARACTER*80 ERR_MESSAGE		!Message for invalid speed
                           

C    arrays for nm,nq
      INTEGER*4 NQNM (0:1000,0:500)         !nq nm BIN array 
      INTEGER*4 DNQNM(0:1000,0:500)         !               DPU VALUES
      CHARACTER*5 CNQNM (0:1000,0:500),     !character format
     .            CDNQNM(0:1000,0:500)

      REAL*4 NM_M(0:500),NQ_MPQ(0:1000)    !mass and m/q values of bins
C					    (lower bounds)

C    binning parameters
      INTEGER*4 NQ_MAX,NM_MAX,
     .          BNQ_MAX,BNM_MAX,
     .          USER_NQ_MAX,USER_NM_MAX
      REAL*4    MQMAX,MQMIN,
     .          BMQMAX,BMQMIN,
     .          USER_MQMAX,USER_MQMIN,
     .          MMAX, MMIN,
     .          BMMAX,BMMIN,
     .          USER_MMAX,USER_MMIN
      INTEGER*4 NQ,      NM,
     .          USER_NQ, USER_NM

C     naming the output file
      LOGICAL BRACK   			!A variable which is .TRUE. if there is 
C                     			a set of square brackets in the input file name.
C                     			If there is, they, and everything between them,  
C                     			will be stripped out so that the output file is  
C                     			created in the user's account and not on the     
C                     			account from which we read the data.             
      INTEGER*4 NINDEX, 		!used in rearranging the input file name to     
C                      			create the output file name.                    
     .          JSAVE   		!used in rearranging the input file name to 
C                        		create the output file name.
      CHARACTER*90 LISFIL1,LISFIL2	!holds the name of the input file           
      CHARACTER OUTFILENAME1*90, OUTFILENAME2*90
      CHARACTER*40 USERNAME1,USERNAME2	!user defined output file name
      INTEGER*4 IFILE		        !number of L1 files used (use with INPUTFILES)
      LOGICAL FIRSTFILE			!set to true if this is the first input file 
      CHARACTER RUN_TIME*8, RUN_DATE*9		!run time and date

C S/C position
      REAL*4 X1_GSE,Y1_GSE,Z1_GSE,     	!Start position
     .       X2_GSE,Y2_GSE,Z2_GSE	!Stop position
      INTEGER*4 START_POS/1/		!Change to -1 after getting start position

      RECORD /CORE/ SCI
      RECORD /HDBstr/ HDB
      RECORD /ST_RATE/ STICS_RATE
      RECORD /ST_PHA/ STICS_PHA
      RECORD /T/ START, END, TIME_TO_USE

      RECORD /MA_RATE/ MASS_RATE   ! receives data on rates from MASS sensor
      RECORD /MA_PHA/ MASS_PHA     ! receives data on tof's from the mass sensor


C   **********************************************************
C   *							     *
C   *		INITIALIZATION				     *
C   *							     *
C   **********************************************************

       DO I = 0,1000			!zero out nm nq array
          DO J = 0,500
            NQNM(I,J)   = 0
            DNQNM(I,J)  = 0
            CNQNM(I,J)  = '     '
            CDNQNM(I,J) = '     '
          END DO
       ENDDO

       LOW_SPEED  = 1000.0
       HIGH_SPEED = 200.0

       CALL TIME(RUN_TIME)			!Get run time
       CALL DATE(RUN_DATE)			!Get run date


C   **********************************************************
C   *							     *
C   *		USER PROMPTS	: input file		     *
C   *				  table format		     *
C   *				  data filtering	     *
C   *				  times			     *
C   *							     *
C   **********************************************************

C
C Get input L1 filename
C
      L1FILENAME = 'DATA_DIR:WI*L1*.DAT'
      CALL WIND_GET_FILENAME(L1FILENAME, STATUS)
      IF(STATUS.NE.1) THEN
	WRITE (*,2) STATUS
    2   FORMAT(' FILE STATUS = ',I3)
        STOP
      ENDIF

C
C Set time format to "wall" time (=0)
C
      USE_SC_CLOCK = 0

      FIRSTFILE = .TRUE.
C
C Prompt user for matrix format
C 
      MATRIX_FORMAT = 1
      TABLE_FORMAT  = 0
      TYPE 5
    5 FORMAT(' Is table listing of PHA events desired [Y/N]: ',$)
      ACCEPT 7, YORN
      IF (YORN.EQ.'Y' .OR. YORN.EQ.'y') THEN
         TABLE_FORMAT = 1
         WRITE(6,*)'Enter output PHA filename or <RETURN> for default name'
         READ(5,'(A)')USERNAME2
      ENDIF
C
C Prompt user for filtering
C 
C     Initialize default conditions
      DATA_FILTER = 0		!no data filtering
      DATA_FILTER_COINC = 0	!no filtering by m or m/q conditions
      DATA_FILTER_DVSTEP  = -1	!no filtering by e/q
      DATA_DVSTEP_MIN = 0	!min dv step = 0
      DATA_DVSTEP_MAX = 31	!max dv step = 31
      DATA_FILTER_RANGE = 0	!no filtering by range
      DATA_FILTER_RANGECHECK = 0 !no filtering by range check
      SEL_RANGE(0) = .TRUE.	!accept range 0 events
      SEL_RANGE(1) = .TRUE.	!accept range 1 events
      SEL_RANGE(2) = .TRUE.	!accept range 2 events
      DATA_FILTER_MSPEED = -1        	!no check on MASS solar wind speed
      DATA_MSPEED_MAX    = 1000     		!max sw speed selection is 1000 km/sec
      DATA_MSPEED_MIN    = 260      		!min sw speed selection is 260 km/sec

      DIRECTION_FILTER = 0         !no filtering by telescope or sector info
      DIRECTION_FILTER_TEL1  =  1  !   telescope 1
      DIRECTION_FILTER_TEL2  =  2  !   telescope 2
      DIRECTION_FILTER_TEL3  =  3  !   telescope 3
      DIRECTION_SECTOR_MIN   =  0  !Sector range to sum over
      DIRECTION_SECTOR_MAX   = 15

      TYPE 6
    6 FORMAT(' Is data filtering by DATA TYPE desired? '
     .      /' (i.e.:  M,M/Q conditions,  E/Q,  PHA Range, SW Speed,) '
     .      /' [Y/N] '$)
      ACCEPT 7, YORN
    7 FORMAT (A)
      IF (YORN.EQ.'Y' .OR. YORN.EQ.'y') THEN
        DATA_FILTER = 1
	CALL USER_REQUESTS_DATA_TYPE(
     .           DATA_FILTER_COINC,
     .           DATA_FILTER_DVSTEP,
     .		 DATA_DVSTEP_MIN,
     .		 DATA_DVSTEP_MAX,
     .           DATA_FILTER_RANGE,
     .           SEL_RANGE, 
     .           DATA_FILTER_RANGECHECK,
     .           DATA_FILTER_MSPEED,
     .           DATA_MSPEED_MIN,
     .           DATA_MSPEED_MAX)
      ENDIF


      TYPE 8
    8 FORMAT(/,' Is data filtering by DIRECTION desired? '
     .      /' (i.e.:  Telescope or Sector) '
     .      /' [Y/N] '$)
      ACCEPT 7, YORN
      IF (YORN.EQ.'Y' .OR. YORN.EQ.'y') THEN
        DIRECTION_FILTER = 1
	CALL DIRECTION_REQUESTS (
     .           DIRECTION_FILTER_TEL1,
     .           DIRECTION_FILTER_TEL2,
     .		 DIRECTION_FILTER_TEL3,
     .		 DIRECTION_SECTOR_MIN,
     .		 DIRECTION_SECTOR_MAX)


      ENDIF

C      TYPE 9
C    9 FORMAT(/,' Input number of mass bins desired, '
C     .         ' and min and max mass values, '/,
C     .         ' [Nm_max, M_min, M_max] '/
C     .         ' (for default dpu = -1,-1,-1) '/)
C      ACCEPT *, USER_NM_MAX, USER_MMIN, USER_MMAX
      USER_NM_MAX = -1
      USER_MMIN = -1
      USER_MMAX = -1
      NM_MAX = USER_NM_MAX
      MMIN   = USER_MMIN
      MMAX   = USER_MMAX
      BNM_MAX = USER_NM_MAX
      BMMIN   = USER_MMIN
      BMMAX   = USER_MMAX

C      TYPE 10
C   10 FORMAT(/,' Input number of mass/charge bins desired, '
C     .         ' and min and max m/q values, '/,
C     .         ' [Nq_max, MQ_min, MQ_max] '/
C     .         ' (for default dpu = -1,-1,-1) '/)
C      ACCEPT *, USER_NQ_MAX, USER_MQMIN, USER_MQMAX
      USER_NQ_MAX = -1
      USER_MQMIN = -1
      USER_MQMAX = -1
      NQ_MAX = USER_NQ_MAX
      MQMIN  = USER_MQMIN
      MQMAX  = USER_MQMAX
      BNQ_MAX = USER_NQ_MAX
      BMQMIN  = USER_MQMIN
      BMQMAX  = USER_MQMAX


      CALL NMNQ_BINS(BNQ_MAX,BMQMAX,BMQMIN,NQ_MPQ,
     .               BNM_MAX,BMMAX, BMMIN, NM_M,
     .               ELOW,EHIGH,TLOW,THIGH,
     .               AM,BM,C1_EQ,C2A_EQ,C2B_EQ,D1_EQ,D2_EQ,
     .               EMQ, BK_C2, BK_MQ, RKM, RKQ)

C
C Prompt user for output filename
C
      WRITE(6,*)'Enter name for output MATRIX file or <RETURN> for default name'
      READ(5,'(A)') USERNAME1
                                   
C
C Prompt user for start and stop times
C
      TYPE 11
  11  FORMAT(/' Enter Start time( -1/ = whole file) - ',/,
     .       ' Year,Month,Day,Hour,Min,Sec: '$) 
      ACCEPT*, START.YEAR,ST_MONTH,ST_DAYOFMONTH,START.HR,START.MIN,START.SEC
      IF(START.YEAR.GE.0)THEN
C
C Convert month and day to day of year.
C
        CALL CNVRT_T_YRMONDAY_TO_YEARDAY( START, ST_MONTH, ST_DAYOFMONTH)
        IF(START.YEAR.LT.100)START.YEAR=START.YEAR+1900        ! Allow for 2-digit years
        IF(START.YEAR.LT.1980)START.YEAR=START.YEAR+100        ! Allow for years beyond 1999
        TYPE 12
  12    FORMAT(' Enter End time - ',/,
     .         ' Year,Month,Day,Hour,Min,Sec: '$) 
        ACCEPT*, END.YEAR,END_MONTH,END_DAYOFMONTH,END.HR,END.MIN,END.SEC
C
C Convert month and day to day of year.
C
        CALL CNVRT_T_YRMONDAY_TO_YEARDAY( END, END_MONTH, END_DAYOFMONTH)
        IF(END.YEAR.LT.100)END.YEAR=END.YEAR+1900        ! Allow for 2-digit years
        IF(END.YEAR.LT.1980)END.YEAR=END.YEAR+100        ! Allow for years beyond 1999
      ELSE
        END.YEAR=3000
      ENDIF

C   **********************************************************
C   *							     *
C   *			CREATE OUTPUT FILE NAME		     *
C   *							     *
C   **********************************************************
C
C   Strip the extension off of the input file name so
C   that we can give a new extension to the output file
C
      NINDEX  = 80
      DO WHILE (L1FILENAME(NINDEX:NINDEX) .NE. '_')
         NINDEX = NINDEX - 1
      END DO
C
C   Find out if the input file name contains some directory
C   designation.  This, if it exists, it will also be stripped off.
C
      I = 1
      DO WHILE (.NOT. BRACK .AND. I .LT. NINDEX)
         IF (L1FILENAME(I:I) .EQ. '[') BRACK = .TRUE.
         I = I + 1
      END DO
C
C   Now, finally, load the input file name into the output
C   file name and add proper extension.
C
      JSAVE = 1
      DO 3 I = 1, NINDEX
         IF (BRACK) THEN
            IF (L1FILENAME(I:I) .EQ. ']') BRACK = .FALSE.
         ELSE
            LISFIL1(JSAVE:JSAVE) = L1FILENAME(I:I)
            JSAVE = JSAVE + 1
         ENDIF
   3  CONTINUE
      LISFIL1(1:9) = 'STICS_PHA'
      LISFIL2 = LISFIL1
      IF  (MATRIX_FORMAT .EQ. 1) THEN
	      LISFIL1(JSAVE+1:JSAVE+6) = 'MQ.MTX'        ! mass vs mq
              LISFIL2(JSAVE+1:JSAVE+6) = 'MQ.TBL'
      ELSE
	      LISFIL1(JSAVE+1:JSAVE+6) = 'ET.MTX'        ! energy vs time
              LISFIL2(JSAVE+1:JSAVE+6) = 'ET.TBL'
      ENDIF

      IF(USERNAME1(1:12) .NE. '            ') THEN
        OUTFILENAME1 = USERNAME1
      ELSE
        OUTFILENAME1 = LISFIL1 				! default matrix filename
      ENDIF      
      IF(USERNAME2(1:12) .NE. '            ') THEN
         OUTFILENAME2 = USERNAME2
      ELSE
         OUTFILENAME2 = LISFIL2                         ! default PHA filename
      ENDIF      

      
      
C   **********************************************************
C   *							     *
C   *			OPEN L1 INPUT FILE 		     *
C   *			READ DATA			     *
C   *							     *
C   **********************************************************

C
C Open L1 file for input, and read first SR, Core, HDB, and STICS_RATES
C
 1000 CONTINUE
      IF (FIRSTFILE .EQ. .FALSE.) THEN
        IFILE = IFILE + 1
	OPENMODE = 1
	CALL WIND_L1_OPEN_NEXT(L1_LUN,OPENMODE,L1FILENAME,L1_FILE_HEADER,STATUS)
      ELSE
        IFILE = 1
        FIRSTFILE = .FALSE.
        CALL WIND_L1_OPEN( L1_LUN, L1FILENAME, L1_FILE_HEADER, STATUS)
      ENDIF
      IF (STATUS .NE. 1) GOTO 9010

      INQUIRE(FILE=L1FILENAME,NAME=FULL_L1_NAME)
      WRITE(6,1001) FULL_L1_NAME
 1001 FORMAT(' OPEN FILE ',A)

      NAMELEN = INDEX(FULL_L1_NAME,' ') - 1
      IF (NAMELEN .LT. 1) NAMELEN=LEN(FULL_L1_NAME)

  13  CALL WIND_L1_READ_SR( L1_LUN, CNTR, L1_SR_HEADER, STATUS )
      IF(STATUS.EQ.RMS$_EOF)GO TO 900
      IF (STATUS .NE. 1) GOTO 9020
      CALL WIND_L1_GET_BASIC( L1_LUN, SCI, STATUS)
      IF (STATUS .NE. 1) GOTO 9030
      CALL WIND_L1_GET_HDB( L1_LUN, HDB, STATUS)
      IF (STATUS .NE. 1) GOTO 9050
      CALL WIND_L1_GET_STICS( L1_LUN, STICS_RATE, STICS_PHA, STATUS )
      IF (STATUS .NE. 1) GOTO 9080
C
C Find first good spin, get year and DOY from it.
C
      N=0
      GOODSPIN=.FALSE.
      DO 14 WHILE(.NOT.GOODSPIN)
        K=N/32+1
        NN=MOD(N,32)
        GOODSPIN=BTEST(L1_SR_HEADER.PRESENT(K),NN)
        N=N+1
        IF(N.GT.59)GOTO 13 ! This SCI-REC all bad; get next
  14  CONTINUE
C
C If low bitrate, make sure that N points to an even spin (odd spins 
C have been emptied by concatenation).
C If high bitrate, decrement because N was incremented after the 1st 
C good spin was found.
C
      IF(.NOT.SCI.EDB(N).BITRATE)THEN
        N=N+MOD(N,2)
        IF(N.GE.60)GOTO 13   ! This SCI-REC all bad; get next 
      ELSE
        N=N-1
      ENDIF
C
C Check FIFO mode, telemetry mode, TGS mode, and trigger mode; place info in header.
C
      ITEMP = HDB.BDH.DAT(221)
      IF(IBITS(ITEMP,7,1).EQ.1)THEN
        FIFO='ON '
      ELSE
        FIFO='OFF'
      ENDIF
C  
      ITEMP = SCI.EDB(N).TLM_Mode
      IF(ITEMP.EQ.0)ITEMP=10
      IF(ITEMP.EQ.128)ITEMP=9
      IF(ITEMP.EQ.256)ITEMP=10
      MODE_AT_START = MODE(ITEMP)
C
      ITEMP = STICS_RATE.EDB(N).RATE(179)
      IF(IBITS(ITEMP,7,1).EQ.1)THEN
        TGS='ON '
      ELSE
        TGS='OFF'
      ENDIF
C
      ITEMP = HDB.BDH.DAT(25)
      TRIGGER_MODE_CODE = IBITS(ITEMP,1,3)

C   **********************************************************
C   *							     *
C   *			OPEN OUTPUT FILES		     *
C   *			WRITE HEADER			     *
C   *							     *
C   **********************************************************
C
C Open PHA file for table listing output
C
      IF(IFILE .EQ. 1) THEN
        IF (TABLE_FORMAT.GT.0) THEN
         OPEN(UNIT= 50, NAME= OUTFILENAME2, FORM='FORMATTED', 
     &     TYPE='NEW', RECL=168)
        ENDIF
C
C Open PHA file for matrix listing output
C
        OPEN(UNIT= 55, NAME= OUTFILENAME1, FORM='FORMATTED', 
     &     TYPE='NEW', RECL=2600)

        OPEN(UNIT=57, NAME= 'DPU_NMNQ.DAT',
     &     DEFAULTFILE='TABLE_DIR:', FORM='FORMATTED', 
     &     TYPE='NEW', RECL=2600)


	RONOFF = 'OFF'
        IF(DATA_FILTER_RANGECHECK.EQ.1) RONOFF = ' ON'
        TONOFF = ' ON'

        IF (TABLE_FORMAT.GT.0) THEN
           WRITE(50,15) 
  15       FORMAT('  STICS TIME, PHA and DPU M & M/Q TABLE LISTING')
           WRITE(50,16) FULL_L1_NAME(1:NAMELEN)

           WRITE(50,18) 'Wall',TRIGGER_MODE(TRIGGER_MODE_CODE),FIFO,TGS,MODE_AT_START 
           WRITE(50,19)DATA_DVSTEP_MIN, DATA_DVSTEP_MAX,COINC(DATA_FILTER_COINC),
     .              RANGESEL(DATA_FILTER_RANGE),
     .              RONOFF,DATA_MSPEED_MIN,DATA_MSPEED_MAX,
     .		    DIRECTION_FILTER_TEL1,DIRECTION_FILTER_TEL2,DIRECTION_FILTER_TEL3,
     .		    DIRECTION_SECTOR_MIN,DIRECTION_SECTOR_MAX,TONOFF,
     .              START.YEAR,ST_MONTH, ST_DAYOFMONTH,START.HR,START.MIN,START.SEC,
     .              END.YEAR,END_MONTH,END_DAYOFMONTH,END.HR,END.MIN,END.SEC
                
        ENDIF

        WRITE(55,155)
  155   FORMAT('  STICS  NM vs NQ MATRIX ')
	WRITE(55,'(A)') '  Run time  '//RUN_TIME//'   '//RUN_DATE
        WRITE(55,16) FULL_L1_NAME(1:NAMELEN)
  16    FORMAT('  ',T10,'Input Level 1 Filename: ',A)

        WRITE(55,18) 'Wall',TRIGGER_MODE(TRIGGER_MODE_CODE),FIFO,TGS,MODE_AT_START 
  18    FORMAT(T3,'Clock selected: ',A,5X,'Trigger mode is ',A22,/,
     .         T3,'FIFO mode is ',A3,9X,'TGS  mode is ',A3,/,
     .         T3,'Telemetry mode is ',A17,/)



  19     FORMAT(T3,'FILTERING CHOICES:', /,
     .              T11,'Voltage step selection =  ', I2, ' to ' ,I2,/,
     .              T11,'Event type selection   =',A10,/,
     .              T11,'Range selection        =     ',A5,/,
     .              T11,'Range checking is ',A3,/,
     .		    T11,'Velocity range         = ',I4,' to ',I6,//,   
     .		    T11,'Selected Telescopes =  ',I2,',',I2,',',I2, '  (-1=not selected)', /,
     .		    T11,'Sectors     =  ',I2,' to ',I2,/,
     .              T11,'Telescope checking is ',A3,//,
     .              T3,'Start Time:  ',I4,I2.2,I2.2,2X,I2.2,2(':',I2.2),5X,
     .              'End Time:  ',I4,I2.2,I2.2,2X,I2.2,2(':',I2.2),/,
     .         /,'       SR BR  Year  Day    Time    VS     E/Q',
     &  '    StopID   StartID/Range  Energy(Ed)  Sector  SSD-ID',
     &  '    ToF(Td)  START   RANGE      M/Q     MASS   NM   NQ  NRG',/)


          WRITE(55,195)DATA_DVSTEP_MIN, DATA_DVSTEP_MAX,COINC(DATA_FILTER_COINC),
     .              RANGESEL(DATA_FILTER_RANGE),
     .              RONOFF,DATA_MSPEED_MIN,DATA_MSPEED_MAX,
     .		    DIRECTION_FILTER_TEL1,DIRECTION_FILTER_TEL2,DIRECTION_FILTER_TEL3,
     .		    DIRECTION_SECTOR_MIN,DIRECTION_SECTOR_MAX,TONOFF,
     .              START.YEAR,ST_MONTH, ST_DAYOFMONTH,START.HR,START.MIN,START.SEC,
     .              END.YEAR,END_MONTH,END_DAYOFMONTH,END.HR,END.MIN,END.SEC


  195     FORMAT(T3,'FILTERING CHOICES:', /,
     .              T11,'Voltage step selection =  ', I2, ' to ' ,I2,/,
     .              T11,'Event type selection   =',A10,/,
     .              T11,'Range selection        =     ',A5,/,
     .              T11,'Range checking is ',A3,/,
     .		    T11,'Velocity range         = ',I4,' to ',I6,//,   
     .		    T11,'Selected Telescopes =  ',I2,',',I2,',',I2, '  (-1=not selected)', /,
     .		    T11,'Sectors     =  ',I2,' to ',I2,/,
     .              T11,'Telescope checking is ',A3,//,
     .              T3,'Start Time:  ',I4,I2.2,I2.2,2X,I2.2,2(':',I2.2),5X,
     .              'End Time:  ',I4,I2.2,I2.2,2X,I2.2,2(':',I2.2),/)

 	 WRITE(55,480)D1_EQ,D2_EQ,C2A_EQ,BK_MQ,C2B_EQ,BK_MQ,C1_EQ,
     .                (I,EMQ(I), I=1,3)
480      FORMAT(/,'  ALGORITHMS: '//,
     $ '  DPU  E/Q = ',F7.4,' * (',F8.6,')**DVSTEP',//,
     $ '  DPU  M/Q = C1 * [ (E/Q) - ',F5.2,
     $               ' ]  *  (TOF)**2 , for m/q < 'F5.2/,
     $ '  DPU  M/Q = C1 * [ (E/Q) - ',F5.2,
     $               ' ]  *  (TOF)**2 , for m/q > 'F5.2/
     $ '     where   C1 = ',1PE11.4//,
     $       3(10X,'  E(',I1,') = 'G14.7/))
         WRITE (55, 9296)
 9296    FORMAT ('  (Default DPU M/Q coefficients used)')
    
C
       WRITE(55,481)
481    FORMAT(/'  DPU Log(M) = A1 + A2*Log(E)'
     $          ' + A3*Log(T) + A4*Log(E)*Log(T) + '
     $          'A5*[Log(E)]**2 + A6*[Log(T)]**3 '/25X,
     $          ', where T=measured TOF, E=measured Energy'//)
C
       WRITE(55,482)  (I,AM(I), I,BM(I), I = 1, 6)
482    FORMAT(5X,'A',I1,' =',G14.7,5X,'B',I1,' =',G14.7/)
       WRITE (55, 9294)
 9294  FORMAT ('  (Default DPU mass coefficients used)')

       WRITE(55,483) ELOW, EHIGH, TLOW, THIGH
483    FORMAT (/'  Emin = 'F5.2' keV    Emax = 'F8.2' keV'/,
     .         '  Tmin = 'F5.2' nsec   Tmax = 'F8.2' nsec')

       WRITE(55,484) RKQ, RKM
484    FORMAT(/'  MQ=MQmin * kQ**(Nq-1) :  kQ = ' F7.5,/
     .        '  M = Mmin * kM**(Nm-1) :  kM = ' F7.5)

      ENDIF   !ifile eq 1


C     ***************************************************
C     *		Get the Basic Core data			*
C     ***************************************************
C
 100  CALL WIND_L1_GET_BASIC( L1_LUN, SCI, STATUS)
      IF (STATUS .NE. 1) GOTO 9030
C
C Get Housekeeping Data Block
C
      CALL WIND_L1_GET_HDB( L1_LUN, HDB, STATUS)
      IF (STATUS .NE. 1) GOTO 9050
C
C Get STICS rates and PHA
C
      CALL WIND_L1_GET_STICS( L1_LUN, STICS_RATE, STICS_PHA, STATUS )
      IF (STATUS .NE. 1) GOTO 9080
c
c Get bitrate of 1st good spin
c
      BITRATE = SCI.EDB(N).BITRATE
C
c Get appropriate for loop below. At HBR, there is data in every spin;
c at LBR there is data only in even-numbered spins.
c
      IF(BITRATE)THEN
        INCREMENT = 1
      ELSE
        INCREMENT = 2
      ENDIF
C
C Set flags for start and stop
C
      STARTFOUND=.FALSE.
      ENDFOUND=.FALSE.

C
C Output STICS PHA one science record.
C
      DO 800 J=0,59,INCREMENT		!0-59 spins
        IF(USE_SC_CLOCK)THEN
          TIME_TO_USE = SCI.EDB(J).SC_TIME
        ELSE
          TIME_TO_USE = SCI.EDB(J).W_TIME
        ENDIF
C
C Compare time read to start time. If the time read is greater than or equal to the user-
C specified start time, then begin processing data.
C
        IF(.NOT.STARTFOUND)THEN
          STARTFOUND=COMPARE_TIME_GE(TIME_TO_USE,START)
        ENDIF
        IF(STARTFOUND)THEN
C
C Compare time read to end time. If time read is greater than the user-specified end time, 
C we're done. Go to the part of the code that closes files and STOP's program.
C
          ENDFOUND=COMPARE_TIME_GE(TIME_TO_USE,END)
          IF(ENDFOUND)GO TO 901
C
C Check quality flags. If PHA, BR, or overall quality flags are bad, skip this spin
C
          IF(BITRATE)THEN
            IF(SCI.EDB(J).Qual.NE.0)GOTO 800
          ELSE
            IF((SCI.EDB(J).Qual.NE.0).OR.(SCI.EDB(J+1).Qual.NE.0))GOTO 800
          ENDIF
          IF(SCI.EDB(J).ST_BR_Qual.NE.0)GOTO 800
          IF(SCI.EDB(J).ST_P_Qual.NE.0)GOTO 800

C
c Get S/C position at beginning of time period
c
	  IF(START_POS .EQ. 1) THEN
            X1_GSE = L1_SR_HEADER.GSE_POS.X/6380.0 
            Y1_GSE = L1_SR_HEADER.GSE_POS.Y/6380.0 
            Z1_GSE = L1_SR_HEADER.GSE_POS.Z/6380.0 
            WRITE(6,*)X1_GSE,Y1_GSE,Z1_GSE
            START_POS = -1
	  ENDIF
C
C Get MASS Speed at start time and at the beginning of every Sci Record.
C 
	  IF((J .EQ. 0) .OR. (FIRSTSPEED .EQ. 0)) THEN
            CALL WIND_L1_GET_MASS(L1_LUN,MASS_RATE,MASS_PHA,STATUS)   ! mass data
            IF (STATUS .NE. 1) GOTO 9085

      	    CALL SMS_MASS_SPEED(HDB, MASS_RATE,  	!input
     .         BULK_SPEED, BULK_UNCER, ERR_SPD_FLAG , ERR_MESSAGE)	!output

            IF((ERR_SPD_FLAG .EQ. 0) .AND. (BULK_UNCER .GT. 0) .AND. (BULK_UNCER .LT. 150.0)) THEN
	      SW_SPEED = BULK_SPEED
            ELSE
	      BAD_SPEED = BAD_SPEED + 1
c	      WRITE(56,151)TIME_TO_USE.DOY,TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
c     .                     BULK_SPEED,BULK_UNCER,ERR_MESSAGE
c 151	      FORMAT(1X,I4.3,3(':',I2.2),3X,'Invalid speed encountered:  ',
c     .               F10.1,' +/- ',F10.1,' km/s',/,16X,A) 
	      SW_SPEED = 0
C	      GOTO 800
            ENDIF
	      
	  ENDIF

  	  FIRSTSPEED = -1		!Change after giving 1st speed


C
C Get dpps enable information, voltage step, and default value of e/q. 
C
          ITEMP =HDB.BDH.DAT(24)
          ITEMP2=HDB.BDH.DAT(25)
	  DPPSPLUS  = IBITS(ITEMP,7,1)
          DPPSMINUS = IBITS(ITEMP2,0,1) 

          ITEMP = STICS_RATE.EDB(J).RATE(179)
          DVSTEP = IBITS(ITEMP,0,5)
	
          IF ( (DVSTEP.GE.DATA_DVSTEP_MIN) .AND.
     .         (DVSTEP.LE.DATA_DVSTEP_MAX) .AND.
		
     .         ( (DATA_FILTER_MSPEED .LT. 0) .OR. 
     .           ((SW_SPEED.GE.DATA_MSPEED_MIN) .AND.
     .            (SW_SPEED.LE.DATA_MSPEED_MAX)) )   ) THEN

C
C Get info for determining speed statistics
C
	     IF ((SW_SPEED.GE.DATA_MSPEED_MIN).AND.(SW_SPEED.LE.DATA_MSPEED_MAX)) THEN
	       IF(SW_SPEED .LT. LOW_SPEED) LOW_SPEED = SW_SPEED
	       IF(SW_SPEED .GT. HIGH_SPEED) HIGH_SPEED = SW_SPEED
               SPEED_HITS = SPEED_HITS +1.0
               SUM_SW_SPEED = SUM_SW_SPEED + SW_SPEED
               SUM_SQ_SPEED = SUM_SQ_SPEED + SW_SPEED**2
             ENDIF
C
C If HDB flag is bad, calculate e/q as if both DPPS plates are ON
C
	     IF(HDB.Qual .NE. 0) THEN
                DPU_E_OVER_Q = 6.190722 * (1.1225857**DVSTEP)
	     ELSE
  	        CALL DPU_EQ(DPPSPLUS,DPPSMINUS,DVSTEP,DPU_E_OVER_Q)
             ENDIF
C
C Get bit rate
C
            IF(SCI.EDB(J).BITRATE)THEN
               RATEFLAG='H'                ! High bit rate
            ELSEIF(TIME_TO_USE.YEAR.NE.0)THEN
               RATEFLAG='L'                ! Low bit rate
            ELSE
               RATEFLAG='?'                ! Bit rate indeterminate, due to bad quality data
            ENDIF


C
C Do for all STICS PHA words
C
            DO 300 I=0, (SCI.EDB(J).STICS_Len-1) * 4, 4
C
C Get parts of PHA word
C
              CALL DISSECT_STICS_PHA(STICS_PHA.EDB(J).PHA(I),
     .                      STOP_ID,START_ID_RANGE,ENERGY,SECTOR,
     .                      SSD_ID,TIME_OF_FLIGHT,PSTART,RANGE)

              CALL TELESCOPE_ID (TIME_OF_FLIGHT,ENERGY,
     .                PSTART,STOP_ID,SSD_ID, TELE_ID, TELE_FLAG)
C
C Calculate Mass and M/Q using DPU algorithms
C
              CALL MMPQ_DPU_WSTICS(DPPSPLUS,DPPSMINUS,DVSTEP,
     .                           ENERGY,TIME_OF_FLIGHT,
     .                           DPU_MASS_OVER_Q,DPU_MASS,
     .                           DPU_NM,DPU_NQ,DPU_RANGE)
	      IF (FIFO .EQ. 'ON ')DPU_RANGE = 2
		
              CALL NMNQ_DATA_BINNED(DPPSPLUS,DPPSMINUS,DVSTEP,
     .                           ENERGY,TIME_OF_FLIGHT,
     .                           NQ_MAX,MQMAX,MQMIN,USER_NQ,
     .                           NM_MAX,MMAX, MMIN, USER_NM)

              	                  
C
C Print out PHA data by word. There will thus be one row per PHA word, and no enties in the
C table for voltage steps that have no PHA words.
C

	      IF ( (DATA_FILTER_RANGE.EQ.0)  .OR.
     .          (RANGE .EQ. 0 .AND. SEL_RANGE(0))  .OR.
     .          (RANGE .EQ. 1 .AND. SEL_RANGE(1))  .OR.
     .          (RANGE .EQ. 2 .AND. SEL_RANGE(2)) ) THEN

	      IF ( (DATA_FILTER_COINC.EQ.0)  .OR. 
     .          (DATA_FILTER_COINC.EQ.1 .AND. DPU_MASS.EQ.0.)  .OR.
     .          (DATA_FILTER_COINC.EQ.2 .AND. DPU_MASS.GT.0.) ) THEN

              IF ( (DIRECTION_FILTER .EQ. 0)             .OR.
     .             (DIRECTION_FILTER_TEL1 .EQ. TELE_ID)  .OR.
     .             (DIRECTION_FILTER_TEL2 .EQ. TELE_ID)  .OR.
     .             (DIRECTION_FILTER_TEL3 .EQ. TELE_ID) ) THEN

	      IF ( 
     .              (  DATA_FILTER_RANGECHECK.EQ.0 .OR.
     .                (DATA_FILTER_RANGECHECK.EQ.1 .AND.	! range OK
     .                       RANGE.EQ.DPU_RANGE)  )
     .                .AND.
     .              ( TELE_FLAG.EQ.0 ) ! telescope OK
     .                ) THEN	         
	              IF(SECTOR .GE. DIRECTION_SECTOR_MIN .AND. 
     .			 SECTOR .LE. DIRECTION_SECTOR_MAX)THEN	         

               IF (TABLE_FORMAT.GT.0) WRITE(50,250) L1_SR_HEADER.SR_COUNT,RATEFLAG,
     &			    TIME_TO_USE.YEAR,TIME_TO_USE.DOY,
     &                      TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
     &                      DVSTEP,
     &                      DPU_E_OVER_Q,
     &                      STOP_ID,
     &                      START_ID_RANGE,
     &                      ENERGY,
     &                      SECTOR,
     &                      SSD_ID,
     &                      TIME_OF_FLIGHT,
     &                      PSTART,
     &                      RANGE,
     &                      DPU_MASS_OVER_Q,
     &                      DPU_MASS,
     &                      DPU_NM,
     &                      DPU_NQ,
     &                      DPU_RANGE
 250           FORMAT(1X,I8,2X,A1,  !sr counter, bit rate
     .                2X,I4,2X,I3.3,2X,I2,':',I2.2,':',I2.2,  	! time
     &             2X,I2,2X,F7.2,5X,I2,11X,I2,11X,I3,8X,I2,6X,		!dvstep,e/q,stop,start,E,sec,
     &             I2,6X,I4,7X,I1,7X,I1,				!ssd,tof,pstart,range
     &             5X,F6.2,3X,F5.1,3X,I2,2X,I3,3X,I2)				!m/q,m

C
C Increment NQNM array
C
	       IF (USER_NQ.GE.0  .AND.  USER_NM.GE.0) THEN
		   NQNM(USER_NQ,USER_NM) = NQNM(USER_NQ,USER_NM) + 1
               ENDIF

	       IF (DPU_NQ.GE.0  .AND.  DPU_NM.GE.0) THEN
		   DNQNM(DPU_NQ,DPU_NM) = DNQNM(DPU_NQ,DPU_NM) + 1
               ENDIF
		
              ENDIF     !check sector
              ENDIF     !check range and/or telescope conditions
              ENDIF     !data filtering by direction 
              ENDIF	!data_filtering by coincidence conditions
              ENDIF     !data filtering by range

 300        CONTINUE

          ENDIF   ! data within user selected voltage step range

        ENDIF     ! data within user selected time

 800  CONTINUE
C
C Read next Science Record
C
      CALL WIND_L1_READ_SR( L1_LUN, CNTR, L1_SR_HEADER, STATUS )
      IF(STATUS.EQ.RMS$_EOF)GO TO 900
      IF (STATUS .NE. 1) GOTO 9020
C
      GO TO 100
C
C Close files and STOP
C
 900  IF (START.YEAR .GE. 0) GO TO 1000
 901  CALL WIND_L1_CLOSE( L1_LUN, STATUS )      ! Close input file
      IF (STATUS .NE. 1) GOTO 9090
c
c Get S/C position at end of time period
c
      X2_GSE = L1_SR_HEADER.GSE_POS.X/6380.0 
      Y2_GSE = L1_SR_HEADER.GSE_POS.Y/6380.0 
      Z2_GSE = L1_SR_HEADER.GSE_POS.Z/6380.0 
      WRITE(6,*)X2_GSE,Y2_GSE,Z2_GSE
C
C Calculate standard deviation of speed
C
      SPEED_MEAN  = SUM_SW_SPEED/SPEED_HITS
      SPEED_SIGMA = SQRT((SUM_SQ_SPEED - SPEED_HITS*SPEED_MEAN**2)/(SPEED_HITS-1.0))
      WRITE(55,940)SPEED_MEAN, SPEED_SIGMA, LOW_SPEED, HIGH_SPEED
 940  FORMAT(/,1X,' Average Solar Wind Speed with sigma (km/s) = ',F6.1,' +/- ',F6.1,
     .       /,1X,' Range of valid speeds encountered (km/s) = ',F6.1,' to ', F6.1)  
C
C Write beginning and end positions to output files
C
      WRITE(55,945)X1_GSE,Y1_GSE,Z1_GSE,X2_GSE,Y2_GSE,Z2_GSE  
 945  FORMAT(1X,' Start postion in GSE cartesian coordinates(Re): ',3F9.3,/
     .       1X,' Stop  postion in GSE cartesian coordinates(Re): ',3F9.3//)


      IF (TABLE_FORMAT.GT.0) CLOSE(UNIT=50)                            ! Close PHA TABLE file


C   **********************************************************
C   *							     *
C   *			WRITE TO                             *
C   *                   OUTPUT matrix FILE		     *
C   *							     *
C   **********************************************************
      IF (NQ_MAX .EQ. -1) NQ_MAX = 126
      IF (NM_MAX .EQ. -1) NM_MAX = 59

      DO I = 0,NQ_MAX+1
         DO J = 0,NM_MAX+4
           IF (NQNM(I,J) .LT. 100000) THEN
             ENCODE(5,255,CNQNM(I,J)) NQNM(I,J)
  255        FORMAT(I5)
             IF(NQNM(I,J) .EQ. 0) CNQNM(I,J) = '     '
           ELSE
             CNQNM(I,J) = '  1E6'
           ENDIF
         END DO
      END DO

      WRITE(55,265) (J, J=0,NM_MAX+4)
      WRITE(55,266) (NM_M(K), K=0,NM_MAX)
  265 FORMAT('1', /,'    NQ ::       NM = ',/
     .              '    ||       '500(1X,I5))
  266 FORMAT(1X ,   '             '500(F6.2)/)

      DO I = 0,NQ_MAX+1
         WRITE(55,260) I,NQ_MPQ(I),(CNQNM(I,J), J=0,NM_MAX+4)
  260    FORMAT (3X,I3,2X,F5.2,500(1X,A5))
      END DO

      DO I = 0,127
         DO J = 0,63
           IF (DNQNM(I,J) .LT. 100000) THEN
             ENCODE(5,255,CDNQNM(I,J)) DNQNM(I,J)
             IF(DNQNM(I,J) .EQ. 0) CDNQNM(I,J) = '     '
           ELSE
             CDNQNM(I,J) = '  1E6'
           ENDIF
         END DO
      END DO

      
C    TEMP NQ_MPQ(J)
      WRITE(57,265) (J, J=0,63)
      DO I = 0,127
         WRITE(57,260) I, NQ_MPQ(I), (CDNQNM(I,J), J=0,63)
      END DO

      CLOSE(UNIT=55)				! Close PHA MATRIX file
      CLOSE(UNIT=57)

      TYPE *,' OUTPUT FILE WRITTEN TO  CURRENT DIRECTORY'

      STOP



C
C Come here for errors
C
9010  TYPE *,'ERROR OPENING L1 FILE; STATUS=',STATUS
      STOP
9020  TYPE *,'ERROR READING SCIENCE RECORD ',CNTR,'; STATUS=',STATUS
      STOP
9030  TYPE *,'ERROR GETTING BASIC CORE; STATUS=',STATUS
      STOP
9050  TYPE *,'ERROR GETTING HDB; STATUS=',STATUS
      STOP
9080  TYPE *,'ERROR GETTING STICS DATA; STATUS=',STATUS
      STOP
9085  TYPE *,'ERROR GETTING MASS DATA; STATUS=',STATUS
      STOP

9090  TYPE *,'ERROR CLOSING L1 FILE; STATUS=',STATUS
      STOP
      END

C***************************************************************************
      SUBROUTINE USER_REQUESTS_DATA_TYPE(
     .           FILTER_COINC,
     .           FILTER_DVSTEP,
     .		 DVSTEP_MIN,
     .		 DVSTEP_MAX,
     .           FILTER_RANGE,
     .           SRANGE,
     .           RANGECHECK,
     .           FILTER_SPEED,
     .           SPEED_MIN,
     .           SPEED_MAX)

C     user inputs for data filtering by data type
C     13.Jan.95  ABG


      IMPLICIT NONE

C     data filtering options
      INTEGER*4  FILTER_COINC,	!data filtering by coincidence 
C					 conditions
     .           FILTER_DVSTEP,	!data filter by voltage step
     .           DVSTEP_MIN,	!dv step min-max values for data filter
     .           DVSTEP_MAX,
     .           FILTER_RANGE,   !range selection
     .           RANGECHECK,
     .           FILTER_SPEED,       !MASS derived solar wind speed selection
     .           SPEED_MIN, 
     .           SPEED_MAX

      LOGICAL SRANGE(0:2)	!accept / reject this range
      CHARACTER*5 RSELECTION(0:6) /		!range selection
     .                           '2 1 0' ,
     .                           '    0' ,
     .                           '  1  ' ,
     .                           '2    ' ,
     .                           '2   0' ,
     .                           '2 1  ' ,
     .                           '  1 0' /

      INTEGER*4  I,J,K
      CHARACTER*1 YORN			!yes or no answer



      TYPE 7
    7 FORMAT(' Enter desired data filtering ',
     .       'by coincidence conditions: ',/,
     .       '   0 = no filtering (no M or M/Q restrictions) ',/,
     .       '   1 = M/Q-only     (i.e., M/Q > 0, Mass = 0) ',/,
     .       '   2 = M & M/Q      (i.e., M/Q > 0, Mass > 0) ',/,
     .       '       ',$)
      ACCEPT *, FILTER_COINC

      TYPE 8
    8 FORMAT(' Enter desired data filtering by voltage step range: ',/,
     .       '   -1 = no voltage step filtering (all e/q values) ',/,
     .       '    0 = voltage step filtering desired',/,
     .       '        ',$)
      ACCEPT *, FILTER_DVSTEP
      IF (FILTER_DVSTEP .GT. -1) THEN
         TYPE 9
    9    FORMAT(' Enter minimum and maximum voltage steps (inclusive)',/
     .          '     ',$)	  
	 ACCEPT *, DVSTEP_MIN, DVSTEP_MAX
      ENDIF

      TYPE 10 , (RSELECTION(I),I=0,6)
   10 FORMAT(' Enter range selection: ',/,
     .       '    0 = ',A5,'  (i.e., all ranges) ',/,
     .       '    1 = ',A5,/,
     .       '    2 = ',A5,/,
     .       '    3 = ',A5,/,
     .       '    4 = ',A5,/,
     .       '    5 = ',A5,/,
     .       '    6 = ',A5 )
       ACCEPT *, FILTER_RANGE
       TYPE 11, RSELECTION(FILTER_RANGE)
   11  FORMAT (/' User selected the following ranges : 'A5/)
      SRANGE(0) = .FALSE.	!reject range 0 events
      SRANGE(1) = .FALSE.	!reject range 1 events
      SRANGE(2) = .FALSE.	!reject range 2 events

       IF (  (FILTER_RANGE.EQ.0) .OR. (FILTER_RANGE.EQ.1) .OR.
     .       (FILTER_RANGE.EQ.4) .OR. (FILTER_RANGE.EQ.6) )
     .		SRANGE(0) = .TRUE.
       IF (  (FILTER_RANGE.EQ.0) .OR. (FILTER_RANGE.EQ.2) .OR.
     .       (FILTER_RANGE.EQ.5) .OR. (FILTER_RANGE.EQ.6) )
     .		SRANGE(1) = .TRUE.
       IF (  (FILTER_RANGE.EQ.0) .OR. (FILTER_RANGE.EQ.3) .OR.
     .       (FILTER_RANGE.EQ.4) .OR. (FILTER_RANGE.EQ.5) )
     .		SRANGE(2) = .TRUE.

       TYPE 12
   12  FORMAT(' Do you want to limit the output to range checks? '
     .        '[y/n] ',$)
       ACCEPT 13, YORN
   13  FORMAT (A)
       IF (YORN.EQ.'Y' .OR. YORN.EQ.'y') RANGECHECK=1

       TYPE 15
   15  FORMAT(' Enter desired data filtering by solar wind speed range: ',/,
     .       '    -1 = no filtering by MASS speed ',/,
     .       '    0  = MASS speed filtering desired',/,
     .       '        ',$)
      ACCEPT *, FILTER_SPEED
      IF (FILTER_SPEED .GT. -1) THEN
         TYPE 16
   16    FORMAT(' Enter minimum and maximum solar wind speed (inclusive)',/
     .          '     ',$)	  
	 ACCEPT *, SPEED_MIN, SPEED_MAX
      ENDIF


       RETURN
       END

