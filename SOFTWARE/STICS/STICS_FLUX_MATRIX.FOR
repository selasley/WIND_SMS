
      PROGRAM STICS_FLUX_MATRIX

C This program reads a WIND L1 data file and dissects the PHA data, printing 
C it in matrix form of calculated m & m/q for PHA events that fall into
C the Nm vs Nq matrix, with user requested data selection criteria imposed.
C Data has basic rate and efficiency corrections applied.
C
C STICS_FLUX_MATRIX created by ABG using stics_mmq_matrix v1.10 16-aug-95 as
C		  a template
C
C Revision History:
C	 6-DEC-1995	K.CHOTOO	Added energy bandwidth to efficiency correction
C	17-APR-1996	K.CHOTOO	Replaced FSR/Io with DCR/RSR
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE

      INCLUDE	'INC_DIR:EDBDEF.INC/NOLIST'
      INCLUDE   'INC_DIR:L1FMT.INC/NOLIST'
      INCLUDE   '($RMSDEF)'

C  	************************************************
C	*		data file names                *
C	************************************************

      INTEGER*4 L1_LUN/19/,OUTUNIT,OPENMODE      !lu unit numbers for input,output,
C						   mode option for searching 
C						   for additional input files
      INTEGER*4 NAMELEN		        !number of characters in file name
      CHARACTER L1FILENAME*80		!input L1 data file name
      CHARACTER FULL_L1_NAME*80		!L1 data file with full directory information
      CHARACTER INPUTFILES(1:100)*80	!L1 file name array for multiple inputs
      INTEGER*4 IFILE		        !number of L1 files used (use with INPUTFILES)

C     naming the output file
      LOGICAL   BRACK   		!A variable which is .TRUE. if there is 
C                     			   a set of square brackets in the input file name.
      INTEGER*4 NINDEX, 		!used in rearranging the input file name to     
C                      			   create the output file name.                    
     .          JSAVE   		!used in rearranging the input file name to 
C                        		   create the output file name.
      CHARACTER*90 LISFIL1,LISFIL2,	!holds the name of the input file for use
     .             LISFIL3,LISFIL4		!  in creating the default output file names
      CHARACTER OUTFILENAME1*90, OUTFILENAME2*90,
     .          OUTDPUFILE*90, OUTFILENAME3*90
      CHARACTER*40 USERNAME1,USERNAME2	!user defined output file name

      LOGICAL FIRSTFILE			!set to true if this is the first input file 

C  	************************************************
C	*		file, record or data status     *
C	************************************************

      INTEGER*4 STATUS			!did L1 file open successfully
      LOGICAL   GOODSPIN		!data record has good data

C  	************************************************
C	*		type of output files           *
C	************************************************

      INTEGER*4  MATRIX_FORMAT		!option = 1 for STANDARD NM VS NQ format
      INTEGER*4 TABLE_FORMAT            !option for pha table listing
      INTEGER*4 STANDARD		!option =1 for standard responses - 
C					 save time by skipping inquiries

C  	************************************************
C	*		time search status     *
C	************************************************

      LOGICAL COMPARE_TIME_GE,			!check on whether time is inbounds
     .        STARTFOUND,ENDFOUND,		!start/end time found?
     .        USE_SC_CLOCK			!which time on L1 to use (wall or s/c)

      INTEGER*4 MONTH,DAYOFMONTH,		 !user requested start and stop times
     .          ENDMONTH,ENDDAYOFMONTH		 ! month and day of month

C  	************************************************
C	*    intrument and s/c housekeeping info       *
C	************************************************

      CHARACTER FIFO*3, TGS*3, RATEFLAG*1			!FIFO, TGS, BIT RATE characters
      CHARACTER*17 MODE(10) /'Science mode     ','Engineering mode ',
     &                       'Maneuver mode    ','Contingency mode ',
     &                       'Science mode     ','Engineering mode ',
     &                       'Maneuver mode    ','Contingency mode ',
     &                       'Transitional mode','Unknown mode     '/,
     &             MODE_AT_START			      	!value on first good record

      INTEGER*4 TRIGGER_MODE_CODE			      	!ADC trigger logic
      CHARACTER*22 TRIGGER_MODE(0:7) /
     &   'E                      ','Tn                    ',
     &   '(Tn.!E)+(Tn.En)        ','Tn.En                 ',
     &   'E+T                    ','(E.!T)+(Tn.!E)+(Tn.En)',
     &   'C.(E+T)                ','(E.!T)+(Tn.En)        '/

      LOGICAL CODE_C1,CODE_C2		!rate compression codes for rate groups 1 and 2

      LOGICAL*1 BITRATE			!telemetry bit rate 
      INTEGER*4 INCREMENT    		!this value based on bitrate

C    DPPS information
      INTEGER*4 DPPSPLUS,		!STICS +DPPS on=1, off=0
     .          DPPSMINUS,		!STICS -DPPS on=1, off=0
     .		DVSTEP			!DPPS voltage step number (0-31)


C  	************************************************
C	*    data selection options                    *
C	************************************************

      INTEGER*4  DATA_FILTER,		!data filter options 
     .           DATA_FILTER_COINC,	!data filtering by coincidence 
C					 conditions
     .           DATA_FILTER_DVSTEP,	!data filter by voltage step
     .           DATA_DVSTEP_MIN,	!dv step min-max values for data filter
     .           DATA_DVSTEP_MAX,
     .           DATA_FILTER_RANGE,       !range selection
     .           DATA_FILTER_RANGECHECK,  !check calculated vs. dpu range
     .           DIRECTION_FILTER,        !select data by telescope info
     .           DIRECTION_FILTER_START,  ! start anode id
     .           DIRECTION_FILTER_STOP,   ! stop mcp id
     .		 DIRECTION_FILTER_SSD,    ! solid state detector id
     .           DIRECTION_CHECK,         !telescope check
     .           TELE_FLAG,TELE_IDENT,    !    flag on telescope check, telescope id
     .           DATA_FILTER_MSPEED,      !speed filter
     .           DATA_MSPEED_MIN,         !	select data only if MASS solar
     .           DATA_MSPEED_MAX          !	speed is .GE. MIN and .LE. MAX

      CHARACTER*10 COINC(0:2) / '       ALL',           !trigger coincidence selection choices
     .                          '  M/Q-only',
     .                          '   M/Q & M'/

      CHARACTER*3 RONOFF, TONOFF            		!range selection ON, OFF status
      CHARACTER*5 RANGESEL(0:6) /			!range selection choices
     .                           '2 1 0' ,
     .                           '    0' ,
     .                           '  1  ' ,
     .                           '2    ' ,
     .                           '2   0' ,
     .                           '2 1  ' ,
     .                           '  1 0' /
      LOGICAL SEL_RANGE(0:2)


C  	************************************************
C	*    DPU DEFAULT VALUES and Calculations       *
C	************************************************

C    dpu default coefficients
      REAL*4
     .          DPU_ELOW, DPU_EHIGH,DPU_TLOW,DPU_THIGH,	 !dpu values for Emin,Emax,Tmin,Tmax
     .          DPU_AM(6),DPU_BM(6),			 !dpu values for mass (A) and 
C							      Nm (B) coefficients
     .          DPU_C1_EQ,DPU_C2A_EQ,			 !    E/Q (C and D) coefficients
     .          DPU_C2B_EQ,DPU_D1_EQ,DPU_D2_EQ,
     .          DPU_EMQ(3),				 !    Nq (E) coefficients
     .          DPU_BK_MQ				 !    M/Q value where C2A vs C2B is used
      INTEGER*4 DPU_BK_C2	      			 !    Nq value where C2A vs C2B is used

C    dpu algorithm values for e/q, mass and m/q:
      REAL*4    DPU_E_OVER_Q,		!calculated e/q,m, and m/q using dpu algorithm
     .          DPU_MASS,
     .          DPU_MASS_OVER_Q

      INTEGER*4 DPU_RANGE		!dpu algorithm calculation for range

C  	************************************************************
C	*    DPU DEFAULT MASS, M/Q Matrix binning parameters       *
C	************************************************************

      INTEGER*4 DPU_NQ_MAX,DPU_NM_MAX	!dpu max NQ,NM values (i.e, number of bins)
      REAL*4    DPU_MQMAX,DPU_MQMIN,	!    max M/Q, min M/Q 
     .          DPU_MMAX,DPU_MMIN	!    max mass, min mass
      REAL*4    DPU_KM,DPU_KQ		!    Nm, Nq resolution (KM, KQ)

      INTEGER*4 DPU_NM,DPU_NQ		!dpu algorithm calculation for Nm and Nm assignments

      REAL*4    DPU_NM_M(0:70),DPU_NQ_MPQ(0:130)       !mass and m/q values of NM,NQ bin boundaries
C						        (lower bounds)

      INTEGER*4 DPU_NQNM(0:130,0:70)         	       !DPU NM,NQ matrix 
C								- no br or eff corrections
      CHARACTER*5 DPU_CNQNM (0:130,0:70)		     !character format

C  	************************************************
C	*    USER DEFINED  Coefficients		       *
C	************************************************

      REAL*4    ELOW,EHIGH,TLOW,THIGH,               !energy and time range
     .          AM(6),BM(6), 			     !mass algorithm coefficients
     .          C1_EQ,C2A_EQ,C2B_EQ,D1_EQ,D2_EQ,     !e/q, m/q algorihm coefficients
     .          EMQ(3),BK_MQ, 
     .		RKM, RKQ			     !NM,NQ binning resolution
      INTEGER*4 BK_C2				     !NQ value for selecting C2

C  	************************************************************
C	*    USER DEFINED MASS, M/Q Matrix binning parameters      *
C	************************************************************

C    binning parameters
      INTEGER*4 NQ_MAX,NM_MAX,		!user defined max NQ,NM values (i.e, number of bins)
     .          USER_NQ_MAX,USER_NM_MAX !user request for Nq_max,Nm_max 
C					   (allows -1 to mean "use dpu default values")
      REAL*4    MQMAX,MQMIN,		!user defined M/Q max, min
     .          USER_MQMAX,USER_MQMIN,	!     request              (-1 = dpu values)
     .          MMAX, MMIN,		!user defined Mass max, min
     .          USER_MMAX,USER_MMIN	!     request              (-1 = dpu values)

      INTEGER*4 PHA_NQ, PHA_NM		!algorithm calculation for Nm and Nm assignments

      REAL*4    USER_NM_M(0:200)  ,USER_NQ_MPQ(0:300)   !mass and m/q values of bin boundaries
      REAL*4    USER_NM_MID(0:200),USER_NQ_MID(0:300)   !mass and m/q values of bin middle
      REAL*4    SPECIES_ENERGY, SPECIES_MASS,		!species energy by voltage step
     .          SPECIES_CHARGE		  		!species mass and charge of each bin

C	:::::  MATRICES  :::::

      REAL*4  OMNI_NQNM(0:300,0:200),   !nq nm matrix array by USER bin values, 
     .         SUN_NQNM(0:300,0:200)    !  normalized by basic rate weight factor each
C					   readout, omni and sun only (T2,8-11) direction 
      CHARACTER*5 OMNI_CNQNM(0:300,0:200),   !character format
     .             SUN_CNQNM(0:300,0:200)

      INTEGER*4 SUN1_NQNM_VS_R (0:31,0:300,0:200,0:2), !by range and voltage step - SUN 8,9
     .          SUN2_NQNM_VS_R (0:31,0:300,0:200,0:2)  ! SUN 10,11
      REAL*4    SUN_EFF_NQNM   (0:300,0:200)           ! SUN with br and eff corrections
      CHARACTER*5 SUN_EFF_CNQNM(0:300,0:200)	       ! character format

      REAL*4 OMNI_MAX,   SUN_MAX,   SUN_EFF_MAX, DPU_MAX,
     .       OMNI_SCALE, SUN_SCALE, SUN_EFF_SCALE, DPU_SCALE

      REAL*4  SUM_FE

C  	************************************************
C	*    STICS DATA			               *
C	************************************************

C    pha word - dissected:
      INTEGER*4 START_ID_RANGE,
     .          STOP_ID,
     .          ENERGY,
     .          SECTOR,
     .          SSD_ID,
     .          TIME_OF_FLIGHT,
     .          PSTART,
     .          RANGE
      INTEGER*4 ISECT           !PHA SECTOR grouped by 2's corresponding to
C				      Basic Rate sectors

C  STICS rates
      INTEGER*4 WIND_DECOMPRESS		!function to return decompressed rates
      INTEGER*4 STICS_RATES(179),	!all the stics rates
C	                                !Basic rate arrays - accumulated over entire data set
     .          BR_SECT(1:8,0:31,0:2),	!Basic rate sectors in a voltage step,range arrray
     .          BR_OMNI(0:31,0:2),      ! sum of above
     .          
C	                                !Basic rate arrays - current readout, only
     .          BASIC_RATE (1:8,0:2),
     .          BASIC_RATE_OMNI(0:2),	!basic rates by range summed over all sectors
C					 for a given voltage step
     .          BR_OMNI_SUMMED           !basic rate summed over all ranges, 
C				         and all sectors for given voltage step
    
C  counters for pha falling within a basic rate domain
C	                                
       INTEGER*4 PHA_SECT(1:8,0:31,0:2),!PHA COUNT ARRAYS by sector,volt step, and range
     .           PHA_OMNI(0:31,0:2),    !  - sum over all sectors

     .           BASIC_PHA_OMNI(0:2),	!PHA COUNTS by range 0:2 - omni
     .	         BASIC_PHA(1:8,0:2)	!		         - sector

C Basic rate weight factor
C             weight factors for a given voltage step
       REAL*4 BASIC_W(1:8,0:2),		!weight factor by range 0:2 for a 
C		             		!  given voltage step, with full
C			                !  directional information

C             weight factors determined by entire data set
     .        BR_SECT_WEIGHT(1:8,0:31,0:2), !weight factor sector, voltage step ,range arrray
     .        BR_OMNI_WEIGHT(0:31,0:2)      !

       INTEGER*4 ISUN                         !pha data sun sector (8-11) flag

       REAL*4  EFF /1.0/
       REAL*4  EFF_VS_MMQ		     !efficiency TCR/Io for each bin
       REAL*4  DCR_RSR,TCR_FSR,DCR_FSR
       INTEGER*4 EFF_HITS		     !efficiency was zero, had to make >0
       REAL*4 ENERGY_BANDWIDTH	     !energy bandwidth allowed 

C  	************************************************
C	*    MASS derived Solar Wind values           *
C	************************************************

       REAL*4  AVE_SPEED,		!Average solar wind speed
     .         H_SPEED,			!Proton speed
     .         H_UNCER,			!Uncertainty in proton speed from width
     .         HE2_SPEED,		!Alpha speed
     .	       HE2_UNCER,		!Uncertainty in alpha speed  from width
     .         SPEED_HITS,		!Number of valid speed values in interval
     .         SUM_HE2_SPEED,		!Sum of alpha speeds over interval
     .         SW_SPEED                 !chosen solar wind speed to check against                      

C  	************************************************
C	*	DUMMY and TEMPORARY VARIABLES          *
C	************************************************

      CHARACTER*1 YORN			!yes or no answer
      INTEGER*4   I,II,J,JJ,KK,K,L,N,NN,MM,CNTR,ITEMP,ITEMP2
      INTEGER*4   INQ,INM, ISTEP

C  	************************************************
C	*	Getting SMS L1 data:                   *
C	************************************************

      RECORD /CORE/ SCI
      RECORD /HDBstr/ HDB
      RECORD /ST_RATE/ STICS_RATE
      RECORD /ST_PHA/ STICS_PHA
      RECORD /T/ START, END, TIME_TO_USE
      RECORD /MA_RATE/ MASS_RATE   ! receives data on rates from MASS sensor
      RECORD /MA_PHA/ MASS_PHA     ! receives data on tof's from the mass sensor




C   **********************************************************
C   *							     *
C   *		USER PROMPTS	: input file		     *
C   *				  table format		     *
C   *				  data filtering	     *
C   *				  times			     *
C   *							     *
C   **********************************************************

C			FILE NAME PROMPTS
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
C Prompt user for output filename
C
      WRITE(6,*)'Enter name for output MATRIX file or <RETURN> for default name'
      READ(5,'(A)') USERNAME1

      STANDARD = 0		!user prompts required

      TYPE 4
    4 FORMAT(' Are standard user requests desired? [Y/N]: ',$)
      ACCEPT 7, YORN
      IF (YORN.EQ.'Y' .OR. YORN.EQ.'y') STANDARD=1

C			TABLE and MATRIX FORMAT PROMPTS

C
C Prompt user if table desired
C 
      MATRIX_FORMAT = 1     !default MvsM/Q, EvsT format not implemented
      TABLE_FORMAT  = 0
      IF (STANDARD .EQ. 0) THEN 
         TYPE 5
    5    FORMAT(' Is table listing of PHA events desired [Y/N]: ',$)
         ACCEPT 7, YORN
         IF (YORN.EQ.'Y' .OR. YORN.EQ.'y') THEN
            TABLE_FORMAT = 1
            WRITE(6,*)'Enter output PHA filename or <RETURN> for default name'
            READ(5,'(A)')USERNAME2
         ENDIF
      ENDIF

C
C Prompt user for matrix resolution :  DPU DEFAULT VALUES are NM_MAX =  59
C								NMIN = 0.5
C								NMAX =  95
C 

      USER_NM_MAX = -1	     !use DPU default
      USER_MMIN   = -1	     !use DPU default
      USER_MMAX   = -1       !use DPU default
      NM_MAX      = USER_NM_MAX
      MMIN        = USER_MMIN
      MMAX        = USER_MMAX

C
C Prompt user for matrix resolution :  DPU DEFAULT VALUES are NQ_MAX = 126
C								NMIN = 0.9
C								NMAX = 42

      USER_NQ_MAX  = -1   !use DPU default
      USER_MQMIN   = -1	  !use DPU default
      USER_MQMAX   = -1	  !use DPU default
      NQ_MAX       = USER_NQ_MAX
      MQMIN        = USER_MQMIN
      MQMAX        = USER_MQMAX
     
C   ***********************************************************************
C   *                    determine Nm, Nq coefficients                    *
C   ***********************************************************************

       CALL USER_NMNQ_COEFF(USER_NQ_MAX,USER_MQMAX,USER_MQMIN,  !user input
     .                 USER_NM_MAX,USER_MMAX,USER_MMIN,
     .                 ELOW,EHIGH,TLOW,THIGH,      		!return values
     .                 NQ_MAX,MQMAX,MQMIN,
     .                 NM_MAX,MMAX ,MMIN,
     .                 RKM,RKQ,
     .                 AM,BM,C1_EQ,C2A_EQ,C2B_EQ,D1_EQ,D2_EQ,EMQ,
     .                 BK_C2, BK_MQ)

      DO INM = 0,200
         USER_NM_M(INM) = 0.
         IF(INM.GT.0.  .AND.  INM.LE.NM_MAX) THEN
            USER_NM_M(INM) = MMIN * RKM**(FLOAT(INM)-1.)	!lower bounds
	    USER_NM_MID(INM) = 0.5* MMIN * (RKM**(FLOAT(INM)-1.) + RKM**FLOAT(INM)) !mid point
	 ELSE IF (INM.EQ.0) THEN
	    USER_NM_MID(INM) = 0.
	 END IF
      END DO

      DO INQ = 0, 300
         USER_NQ_MPQ(INQ) = 0.
         IF(INQ.GT.0  .AND.  INQ.LE.NQ_MAX+1) THEN
           USER_NQ_MPQ(INQ) = MQMIN *RKQ**(FLOAT(INQ)-1.)
 	   USER_NQ_MID(INQ) = 0.5 * MQMIN *( RKQ**(FLOAT(INQ)-1.) + RKQ**FLOAT(INQ))
         ELSE IF (INQ.EQ.0) THEN
	   USER_NQ_MID(INQ) = 0.5 * MQMIN
	 ENDIF
      END DO

    

C			DATA FILTER PROMPTS

C
C Prompt user for filtering
C 
C     Initialize default conditions
      DATA_FILTER = 0			!no data filtering
      DATA_FILTER_COINC = 0		!no filtering by m or m/q conditions
      DATA_FILTER_DVSTEP  = -1		!no filtering by e/q
      DATA_DVSTEP_MIN = 0			!min dv step = 0
      DATA_DVSTEP_MAX = 31			!max dv step = 31
      DATA_FILTER_RANGE = 0		!no filtering by range
      DATA_FILTER_RANGECHECK = 0 	!no filtering by range check
      SEL_RANGE(0) = .TRUE.			!accept range 0 events
      SEL_RANGE(1) = .TRUE.			!accept range 1 events
      SEL_RANGE(2) = .TRUE.			!accept range 2 events
      DATA_FILTER_MSPEED = -1        	!no check on MASS solar wind speed
      DATA_MSPEED_MAX    = 10000     		!max sw speed selection is 10000 km/sec
      DATA_MSPEED_MIN    = 0      		!min sw speed selection is 100 km/sec


      DIRECTION_FILTER = 1         !YES - filtering by telescope info - see direction_check
      DIRECTION_FILTER_START = -1  	!no selection by start id
      DIRECTION_FILTER_STOP  = -1  	!no selection by stop id
      DIRECTION_FILTER_SSD   = -1  	!no selection by ssd
      DIRECTION_CHECK =  1         	!YES - check on telescope id for consistency

      IF (STANDARD .EQ. 0) THEN
        TYPE 6
    6   FORMAT(' Is data filtering by DATA TYPE desired? '
     .      /' (i.e.:  M,M/Q conditions, E/Q (VS), PHA Range, '
     .      /' Solar Wind Speed)    [Y/N] '$)
        ACCEPT 7, YORN
    7   FORMAT (A)
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
      ENDIF
                                  
C			TIME PROMPTS
 
C
C Prompt user for start and stop times
C
      TYPE 11
  11  FORMAT(/' Enter Start time( -1/ = whole file) - ',/,
     .       ' Year,Month,Day,Hour,Min,Sec: '$) 
      ACCEPT*, START.YEAR,MONTH,DAYOFMONTH,START.HR,START.MIN,START.SEC
      IF(START.YEAR.GE.0)THEN
C
C Convert month and day to day of year.
C
        CALL CNVRT_T_YRMONDAY_TO_YEARDAY( START, MONTH, DAYOFMONTH)
        IF(START.YEAR.LT.100)START.YEAR=START.YEAR+1900        ! Allow for 2-digit years
        IF(START.YEAR.LT.1980)START.YEAR=START.YEAR+100        ! Allow for years beyond 1999
        TYPE 12
  12    FORMAT(' Enter End time - ',/,
     .         ' Year,Month,Day,Hour,Min,Sec: '$) 
        ACCEPT*, END.YEAR,ENDMONTH,ENDDAYOFMONTH,END.HR,END.MIN,END.SEC
C
C Convert month and day to day of year.
C
        CALL CNVRT_T_YRMONDAY_TO_YEARDAY( END, ENDMONTH, ENDDAYOFMONTH)
        IF(END.YEAR.LT.100)END.YEAR=END.YEAR+1900        ! Allow for 2-digit years
        IF(END.YEAR.LT.1980)END.YEAR=END.YEAR+100        ! Allow for years beyond 1999
      ELSE
        END.YEAR=3000
      ENDIF

C   **********************************************************
C   *							     *
C   *		INITIALIZATION				     *
C   *							     *
C   **********************************************************

C Set time format to "wall" time (=0).  For flight data, s/c clock
C and wall clock should be equivalent

       USE_SC_CLOCK = 0

       FIRSTFILE = .TRUE.
       EFF_HITS  = 0



       DO INQ = 0,130			!zero out nm nq array for dpu default
          DO INM = 0,70
            DPU_NQNM(INQ,INM)     = 0
	    DPU_CNQNM(INQ,INM) = '     '
          END DO
       END DO

       DO INQ = 0,300			!zero out nm nq array
          DO INM = 0,200
	    OMNI_NQNM(INQ,INM)    = 0.
	    SUN_NQNM (INQ,INM)    = 0.
	    SUN_EFF_NQNM(INQ,INM) = 0.
	    OMNI_CNQNM(INQ,INM)    = '     '
            SUN_CNQNM (INQ,INM)    = '     '
	    SUN_EFF_CNQNM(INQ,INM) = '     '
          END DO
       ENDDO

       DO ISTEP = 0,31					!voltage step
          DO J=0,2					!range
		DO INQ=0,300				!Nq
                   DO INM = 0,200			!Nm
		      SUN1_NQNM_VS_R(ISTEP,INQ,INM,J) = 0
		      SUN2_NQNM_VS_R(ISTEP,INQ,INM,J) = 0
                   END DO
		END DO

	        DO MM = 1,8     !sector
     	          BR_SECT(MM,ISTEP,J)   = 0  !basic rate sector, voltage step ,range arrray
     	          PHA_SECT(MM,ISTEP,J)   = 0 !total PHA sector, voltage step ,range arrray
                END DO

	    BR_OMNI(ISTEP,J) = 0
            PHA_OMNI(ISTEP,J) = 0
	  END DO
       END DO

C   get dpu default values for mass, mq, e/q, etc calculations -
C   used here for header information only

       CALL DPU_DEFAULT_COEFF
     .                (DPU_NQ_MAX,DPU_MQMAX,DPU_MQMIN,
     .                 DPU_NM_MAX,DPU_MMAX,DPU_MMIN,
     .                 DPU_ELOW, DPU_EHIGH,DPU_TLOW,DPU_THIGH,
     .                 DPU_AM,DPU_BM,DPU_C1_EQ,DPU_C2A_EQ,
     .                 DPU_C2B_EQ,DPU_D1_EQ,DPU_D2_EQ,
     .                 DPU_EMQ,DPU_BK_C2,DPU_BK_MQ,DPU_KM,
     .                 DPU_KQ, DPU_NM_M, DPU_NQ_MPQ)

C   **********************************************************
C   *							     *
C   *			CREATE OUTPUT FILE NAME		     *
C   *                        either DEFAULT or		     *
C   *                               USER DEFINED             *
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
C   file name and add proper extension.  Format of input
C   file is SMSDISK3:[WIND.L1DATA]WI_L1_SMS_YYYYMMDD_Vxx.DAT
C   Write over all but the _YYYYMMDD_ part.
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
      LISFIL1(1:9) = 'STICS_FLX_'
      LISFIL2 = LISFIL1
      LISFIL3 = LISFIL1
      LISFIL4 = LISFIL1
      LISFIL3(1:9) = 'STICS_DPU'
      LISFIL4(1:9) = 'STICS_FLX'
      IF  (MATRIX_FORMAT .EQ. 1) THEN
	      LISFIL1(JSAVE:JSAVE+6) = 'MMQ.MTX'        ! mass vs mq
              LISFIL2(JSAVE:JSAVE+6) = 'MMQ.TBL'
              LISFIL3(JSAVE:JSAVE+6) = 'MMQ.MTX'
              LISFIL4(JSAVE:JSAVE+6) = 'IRON.NQ'
      ELSE						
	      LISFIL1(JSAVE:JSAVE+6) = 'ENT.MTX'        ! energy and time
              LISFIL2(JSAVE:JSAVE+6) = 'ENT.TBL'        ! not yet implemented
              LISFIL3(JSAVE:JSAVE+6) = 'ENT.MTX'
      ENDIF

      IF(USERNAME1(1:12) .NE. '            ') THEN
        OUTFILENAME1 = USERNAME1
      ELSE
        OUTFILENAME1 = LISFIL1 		   ! default matrix filename
      ENDIF      
      IF(USERNAME2(1:12) .NE. '            ') THEN
         OUTFILENAME2 = USERNAME2
      ELSE
         OUTFILENAME2 = LISFIL2            ! default table filename
      ENDIF      
         OUTDPUFILE = LISFIL3
         OUTFILENAME3 = LISFIL4
      
      
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
      INPUTFILES(IFILE) = FULL_L1_NAME
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
	RONOFF = 'OFF'					!some info on user input
        IF(DATA_FILTER_RANGECHECK.EQ.1) RONOFF = ' ON'
        TONOFF = 'OFF'
        IF(DIRECTION_CHECK.EQ.1) TONOFF = ' ON'

C Open PHA file for table listing output
C
       IF (TABLE_FORMAT.GT.0) THEN

        IF(IFILE.EQ. 1) OPEN(UNIT= 50, NAME= OUTFILENAME2, 
     &     DEFAULTFILE='SMSDISK4:[WIND.L2DATA.STICS]', FORM='FORMATTED', 
     &     TYPE='NEW', RECL=168)

        WRITE(50,15) 
        WRITE(50,16) FULL_L1_NAME(1:NAMELEN)
        WRITE(50,17) OUTFILENAME2
        WRITE(50,18) 'Wall', DATA_DVSTEP_MIN, DATA_DVSTEP_MAX,
     .                  DIRECTION_FILTER_START
        WRITE(50,19)
     .              TRIGGER_MODE(TRIGGER_MODE_CODE),COINC(DATA_FILTER_COINC),
     .              DIRECTION_FILTER_STOP, 
     .              FIFO,RANGESEL(DATA_FILTER_RANGE),DIRECTION_FILTER_SSD,
     .              TGS,RONOFF,TONOFF,MODE_AT_START,DATA_MSPEED_MIN,
     .              DATA_MSPEED_MAX,
     .              START.YEAR,MONTH,DAYOFMONTH,START.HR,START.MIN,START.SEC,
     .              END.YEAR,ENDMONTH,ENDDAYOFMONTH,END.HR,END.MIN,END.SEC
        WRITE(50,195)
       ENDIF

C
C Open PHA file for matrix listing output
C
       IF(IFILE.EQ.1) THEN
	   OPEN(UNIT= 55, NAME= OUTFILENAME1, 
     &     DEFAULTFILE='SMSDISK4:[WIND.L2DATA.STICS]', FORM='FORMATTED', 
     &     TYPE='NEW', RECL=2600)

	   OPEN(UNIT= 56, NAME= OUTDPUFILE, 
     &     DEFAULTFILE='SMSDISK4:[WIND.L2DATA.STICS]', FORM='FORMATTED', 
     &     TYPE='NEW', RECL=2600)

           OPEN(UNIT= 57, NAME= OUTFILENAME3, 
     &     DEFAULTFILE='SMSDISK4:[WIND.L2DATA.STICS]', FORM='FORMATTED', 
     &     TYPE='NEW', RECL=2600)

         DO OUTUNIT = 55,57
          WRITE(OUTUNIT,155)
          WRITE(OUTUNIT,16) FULL_L1_NAME(1:NAMELEN)
         END DO

          WRITE(55,17) OUTFILENAME1
          WRITE(56,17) OUTDPUFILE
          WRITE(57,17) OUTFILENAME3

         DO OUTUNIT = 55,57
          WRITE(OUTUNIT,18) 'Wall', DATA_DVSTEP_MIN, DATA_DVSTEP_MAX,
     .                  DIRECTION_FILTER_START

          WRITE(OUTUNIT,19)TRIGGER_MODE(TRIGGER_MODE_CODE),COINC(DATA_FILTER_COINC),
     .              DIRECTION_FILTER_STOP, 
     .              FIFO,RANGESEL(DATA_FILTER_RANGE),DIRECTION_FILTER_SSD,
     .              TGS,RONOFF,TONOFF,MODE_AT_START,DATA_MSPEED_MIN, 
     .              DATA_MSPEED_MAX,
     .              START.YEAR,MONTH, DAYOFMONTH,START.HR,START.MIN,START.SEC,
     .              END.YEAR,ENDMONTH,ENDDAYOFMONTH,END.HR,END.MIN,END.SEC

 	   WRITE(OUTUNIT,480) DPU_D1_EQ,DPU_D2_EQ,DPU_C2A_EQ,
     .                 DPU_BK_MQ,DPU_C2B_EQ,DPU_BK_MQ,DPU_C1_EQ,
     .                (I,DPU_EMQ(I), I=1,3)
           WRITE(OUTUNIT,481) 
           WRITE(OUTUNIT,482)(I,DPU_AM(I), I,DPU_BM(I), I = 1, 6)
           WRITE(OUTUNIT,483) DPU_ELOW, DPU_EHIGH, DPU_KQ,
     .                      DPU_TLOW, DPU_THIGH, DPU_KM
         END DO
       ENDIF		!first input file

C    format statements for headers:

  15    FORMAT('1',' 1  STICS TIME, PHA and DPU M & M/Q TABLE LISTING')
  155   FORMAT(' 1  STICS  NM vs NQ MATRIX ')
  16    FORMAT(' 2',T10,'Input Level 1 Filename: ',A)
  17    FORMAT(' 3',T10,'       Output Filename: ',A)
  18    FORMAT(' 4'/,' 5',T5,'Clock selected: ',A,
     .           17X,'FILTERING CHOICES:   Voltage step selection =  ' I2 ' to ' I2 ' ',
     .           10X,'   Start anode =  ' I2)

  19     FORMAT(' 6',T5,'Trigger mode is ',A22,20X,'Event type selection   =' 
     .              A10,14X,'Stop MCP    =  ' I2,/
     .         ' 7',T5,'FIFO mode is ',A3,    42X,'Range selection        =     ' 
     .              A5,14X, 'SSD ID      =  ' I2/,
     .         ' 8',T5,'TGS  mode is ',A3,    42X,'Range checking is 'A3,
     .              27X,'Telescope checking is 'A3,/,
     .	       ' 9',T5,'Telemetry mode is ',A17, 23X,'Velocity range         = '
     .              I4,' to', I6, /,
     .         ' 10',T5,'Start Time ',I4,1X,I2"/"I2,1X,
     .           I2.2,2(':',I2.2),5X,'End Time ',I4,1X,I2"/"I2,1X,
     .           I2.2,2(':',I2.2))

  195    FORMAT(' 11',/' 12    SR BR  Year  Day    Time    VS     E/Q'
     .  '    StopID  StartID/R  E(Ed)  Sect  SSD',
     &  '  ToF(Td) START RANGE  SPEED   M/Q    MASS   NM   NQ  NRG  BR(R)/BR012',/)

 480      FORMAT(/,'  DPU DEFAULT ALGORITHMS: '//,
     $ '  DPU  E/Q = ',F7.4,' * (',F8.6,')**DVSTEP',//,
     $ '  DPU  M/Q = C1 * [ (E/Q) - ',F5.2,
     $               ' ]  *  (TOF)**2 , for m/q < 'F5.2/,
     $ '  DPU  M/Q = C1 * [ (E/Q) - ',F5.2,
     $               ' ]  *  (TOF)**2 , for m/q > 'F5.2/
     $ '     where   C1 = ',1PE11.4//,
     $       3(10X,'  E(',I1,') = 'G14.7/),/)
    
 481     FORMAT(/'  DPU Log(M) = A1 + A2*Log(E)'
     $          ' + A3*Log(T) + A4*Log(E)*Log(T) + '
     $          'A5*[Log(E)]**2 + A6*[Log(T)]**3 '/25X,
     $          ', where T=measured TOF, E=measured Energy'//)
 482     FORMAT(5X,'A',I1,' =',G14.7,5X,'B',I1,' =',G14.7/)

 483     FORMAT (/'  DPU Energy and Time windows: ',20X,
     .		 '  DPU Binning Resolution: '
     .          /'  Emin = 'F5.2' keV    Emax = 'F8.2' keV'
     .           29X,'  MQ = MQmin * kQ**(Nq-1) :  DPU_kQ = ' F7.5,/,
     .           '  Tmin = 'F5.2' nsec   Tmax = 'F8.2' nsec'
     .           29X,' M  =  Mmin * kM**(Nm-1) :  DPU_kM = ' F7.5,/)


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
C
C Get MASS rates and PHA
C
      CALL WIND_L1_GET_MASS(L1_LUN,MASS_RATE,MASS_PHA,STATUS)   ! mass data
      IF (STATUS .NE. 1) GOTO 9085
C
C Get solar wind speed from MASS
C
      SW_SPEED = -100.		!set solar wind speed to outside normal range values
      CALL MASS_SPEED(HDB,MASS_RATE,H_SPEED,H_UNCER,HE2_SPEED,HE2_UNCER)
      WRITE(6,*) H_SPEED, HE2_SPEED
      IF((HE2_SPEED.GE.140.0).AND.(HE2_SPEED.LE.1500))THEN   !to find average speed		
         SW_SPEED = HE2_SPEED              !use helium for iron speeds
         SPEED_HITS = SPEED_HITS +1.0
         SUM_HE2_SPEED = SUM_HE2_SPEED + HE2_SPEED
      ENDIF 



C Get bitrate of 1st good spin.  At HBR, there is data in every spin;
C At LBR there is data only in even-numbered spins.

      BITRATE = SCI.EDB(N).BITRATE
      IF(BITRATE)THEN
        INCREMENT = 1
      ELSE
        INCREMENT = 2
      ENDIF
C
C Set flags for start and stop
      STARTFOUND=.FALSE.
      ENDFOUND=.FALSE.

C
C Output STICS PHA one science record.

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
C Get dpps enable inforation, voltage step, and default value of e/q. 
C
          ITEMP =HDB.BDH.DAT(24)
          ITEMP2=HDB.BDH.DAT(25)
	  DPPSPLUS  = IBITS(ITEMP,7,1)
          DPPSMINUS = IBITS(ITEMP2,0,1) 

          ITEMP = STICS_RATE.EDB(J).RATE(179)
          DVSTEP = IBITS(ITEMP,0,5)
	
C    check voltage step filter and solar wind speed filter

          IF ( (DVSTEP.GE.DATA_DVSTEP_MIN) .AND.
     .         (DVSTEP.LE.DATA_DVSTEP_MAX) .AND.
		
     .         (DATA_FILTER_MSPEED.EQ.-1 
     .          .OR. SW_SPEED.GE.DATA_MSPEED_MIN) .AND.
     .         (DATA_FILTER_MSPEED.EQ.-1 
     .          .OR. SW_SPEED.LE.DATA_MSPEED_MAX) ) THEN
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

C	*************************************************
C       *						*
C	*	Determine BR weighting factor   	*
C	*						*
C	*************************************************

	    ITEMP = HDB.BDH.DAT(189)		      !rate compression codes
            CODE_C1 = BTEST(ITEMP,1)
            CODE_C2 = BTEST(ITEMP,2)

	    ITEMP = STICS_RATE.EDB(J).RATE(179)       !if TGS off, then there 
	    IF (IBITS (ITEMP,7,1) .EQ. 1) THEN	      !also Fe matrix rates available
		TGS = 'ON'
	    ELSE
		TGS = 'OFF'
	    ENDIF

C		***** get the PHA COUNT in each basic rate  ******

	    DO II=0,2		!range
	      BASIC_PHA_OMNI(II) = 0		!initialize for this voltage step
              DO MM = 1,8	!sector
	        BASIC_PHA(MM,II)   = 0
	      END DO
            END DO

C	  look through all the pha for this voltage step,
C         and determine the count in each basic rate region

          DO I = 0, (SCI.EDB(J).STICS_Len-1) * 4, 4
              CALL DISSECT_STICS_PHA(STICS_PHA.EDB(J).PHA(I),
     .                      STOP_ID,START_ID_RANGE,ENERGY,SECTOR,
     .                      SSD_ID,TIME_OF_FLIGHT,PSTART,RANGE)

C          get count on pha within each range, without regard for direction
             BASIC_PHA_OMNI(RANGE) = BASIC_PHA_OMNI(RANGE) + 1

C          get count on pha within each range, by sector
	     IF (SECTOR.GE.0 .AND. SECTOR.LE.1) ISECT = 1
	     IF (SECTOR.GE.2 .AND. SECTOR.LE.3) ISECT = 2
	     IF (SECTOR.GE.4 .AND. SECTOR.LE.5) ISECT = 3
	     IF (SECTOR.GE.6 .AND. SECTOR.LE.7) ISECT = 4
	     IF (SECTOR.GE.8 .AND. SECTOR.LE.9) ISECT = 5
	     IF (SECTOR.GE.10.AND. SECTOR.LE.11)ISECT = 6
	     IF (SECTOR.GE.12.AND. SECTOR.LE.13)ISECT = 7
	     IF (SECTOR.GE.14.AND. SECTOR.LE.15)ISECT = 8
	
             BASIC_PHA(ISECT,RANGE) = BASIC_PHA(ISECT,RANGE) + 1
          END DO


C		***** get the Basic Rates  ******

	    DO 220 II=120,143			!these are the basic rates
	     STICS_RATES(II+1) = 
     .            WIND_DECOMPRESS(STICS_RATE.EDB(J).RATE(II),CODE_C1)
 220        CONTINUE

C 	get out basic rates for ranges R0, R1, R2

	    BR_OMNI_SUMMED = 0		!initialize for this voltage step

	    DO 230 II=0,2	!range
             BASIC_RATE_OMNI(II)= 0

             DO MM = 1,8   	!ISECT
		BASIC_RATE(MM,II) = STICS_RATES(120+II*8+MM)

                IF (BASIC_PHA(MM,II).GT.0) THEN
	              BASIC_W(MM,II) = FLOAT(BASIC_RATE(MM,II))
     .				/  FLOAT(BASIC_PHA(MM,II))
		ELSE
		      BASIC_W(MM,II) = 1.0
		END IF

	        BASIC_RATE_OMNI(II) = BASIC_RATE_OMNI(II) 
     .                              + STICS_RATES(120+II*8+MM)

	        BR_SECT(MM,DVSTEP,II) = BR_SECT(MM,DVSTEP,II) 
     .				      + STICS_RATES(120+II*8+MM)
             END DO	!over sectors

	     BR_OMNI(DVSTEP,II) = BR_OMNI(DVSTEP,II) + BASIC_RATE_OMNI(II)

             BR_OMNI_SUMMED = BR_OMNI_SUMMED + BASIC_RATE_OMNI(II)

 230        CONTINUE	!end do over range


C
C Do loop for all STICS PHA words in this voltage step
C
         DO 300 I=0, (SCI.EDB(J).STICS_Len-1) * 4, 4
C
C Get parts of PHA word
C
          CALL DISSECT_STICS_PHA(STICS_PHA.EDB(J).PHA(I),
     .                      STOP_ID,START_ID_RANGE,ENERGY,SECTOR,
     .                      SSD_ID,TIME_OF_FLIGHT,PSTART,RANGE)

C         get corresponding basic rate sector
	     IF (SECTOR.GE.0 .AND. SECTOR.LE.1) ISECT = 1
	     IF (SECTOR.GE.2 .AND. SECTOR.LE.3) ISECT = 2
	     IF (SECTOR.GE.4 .AND. SECTOR.LE.5) ISECT = 3
	     IF (SECTOR.GE.6 .AND. SECTOR.LE.7) ISECT = 4
	     IF (SECTOR.GE.8 .AND. SECTOR.LE.9) ISECT = 5
	     IF (SECTOR.GE.10.AND. SECTOR.LE.11)ISECT = 6
	     IF (SECTOR.GE.12.AND. SECTOR.LE.13)ISECT = 7
	     IF (SECTOR.GE.14.AND. SECTOR.LE.15)ISECT = 8

	     PHA_SECT(ISECT,DVSTEP,RANGE)  = PHA_SECT(ISECT,DVSTEP,RANGE) + 1
	
	     PHA_OMNI(DVSTEP,RANGE) = PHA_OMNI(DVSTEP,RANGE) + 1

C check data quality on telescope consistency
          CALL TELESCOPE_ID (TIME_OF_FLIGHT,ENERGY,
     .       PSTART,STOP_ID,SSD_ID, TELE_IDENT, TELE_FLAG)
C
C Calculate Mass and M/Q using DPU algorithms
C
          CALL MMPQ_DPU_WSTICS(DPPSPLUS,DPPSMINUS,DVSTEP,
     .                           ENERGY,TIME_OF_FLIGHT,
     .                           DPU_MASS_OVER_Q,DPU_MASS,
     .                           DPU_NM,DPU_NQ,DPU_RANGE)
	  IF (FIFO .EQ. 'ON ') DPU_RANGE = 2
		
          CALL USER_NMNQ_PHA_BINNED(DPPSPLUS,DPPSMINUS,DVSTEP,  !pha event information
     .                ENERGY,TIME_OF_FLIGHT,
     .                NQ_MAX,MQMAX,MQMIN,    !information for binning
     .                NM_MAX,MMAX, MMIN,
     .                ELOW,EHIGH,TLOW,THIGH,      !return values
     .                RKM,RKQ,
     .                AM,BM,C1_EQ,C2A_EQ,C2B_EQ,D1_EQ,D2_EQ,EMQ,
     .                BK_C2, BK_MQ,
     .                PHA_NQ,PHA_NM)			!return information

              	                  
C
C Print out PHA data by word. There will thus be one row per PHA word, and no enties in the
C table for voltage steps that have no PHA words.
C

	  IF ( (DATA_FILTER_RANGE.EQ.0)  .OR.
     .         (RANGE .EQ. 0 .AND. SEL_RANGE(0))  .OR.
     .         (RANGE .EQ. 1 .AND. SEL_RANGE(1))  .OR.
     .         (RANGE .EQ. 2 .AND. SEL_RANGE(2)) ) THEN

	      IF ( (DATA_FILTER_COINC.EQ.0)  .OR. 
     .         (DATA_FILTER_COINC.EQ.1 .AND. DPU_MASS.EQ.0.)  .OR.
     .         (DATA_FILTER_COINC.EQ.2 .AND. DPU_MASS.GT.0.) ) THEN

              IF ( (DIRECTION_FILTER.EQ.0)               .OR.
     .            (( (DIRECTION_FILTER_SSD.LT.0) .OR.
     .               (DIRECTION_FILTER_SSD.EQ.SSD_ID)  ) .AND.
     .             ( (DIRECTION_FILTER_STOP.LT.0) .OR.
     .               (DIRECTION_FILTER_STOP.EQ.STOP_ID)) .AND.
     .             ( (DIRECTION_FILTER_START.LT.0) .OR.
     .               (DIRECTION_FILTER_START.EQ.PSTART)) ) ) THEN

	      IF ( 
     .              (  DATA_FILTER_RANGECHECK.EQ.0 .OR.
     .                (DATA_FILTER_RANGECHECK.EQ.1 .AND.	! range OK
     .                       RANGE.EQ.DPU_RANGE)  )
     .                .AND.
     .              (  DIRECTION_CHECK.EQ.0 .OR.
     .                (DIRECTION_CHECK.EQ.1 .AND. TELE_FLAG.EQ.0)  ) ! telescope OK
     .                ) THEN	         
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
     &                      SW_SPEED,
     &                      DPU_MASS_OVER_Q,
     &                      DPU_MASS,
     &                      DPU_NM,
     &                      DPU_NQ,
     &                      DPU_RANGE,
     &                      BASIC_RATE_OMNI(RANGE),
     &                      BR_OMNI_SUMMED
 250           FORMAT(1X,I8,2X,A1,  !sr counter, bit rate
     .                2X,I4,2X,I3.3,2X,I2,':',I2.2,':',I2.2,  	! time
     &             2X,I2,2X,F7.2,5X,I2,7X,I2,5X,I3,5X,I2,4X,	!dvstep,e/q,stop,start,E,sec,
     &             I2,2X,I4,7X,I1,5X,I1,			!ssd,tof,pstart,range
     &             3X,F6.1,F6.2,3X,F5.1,3X,I2,2X,I3,3X,I2,   !sw speed, 
C								m/q,m,derived nm,nq,range
     &             2X,I5,'/',I5)					!basic rate (range)

C
C Increment NQNM arrays
C
	       IF (PHA_NQ.GE.0  .AND.  PHA_NM.GE.0) THEN

	          CALL SPECIES_IDENTIFIER( USER_NM_MID(PHA_NM), 
     .					USER_NQ_MID(PHA_NQ) , 
     .				        SPECIES_MASS,SPECIES_CHARGE)
	          SPECIES_ENERGY = SPECIES_CHARGE * DPU_D1_EQ * (DPU_D2_EQ**DVSTEP)
		  ENERGY_BANDWIDTH = 0.019*SPECIES_ENERGY
	          CALL WSTICSEFF(SPECIES_ENERGY, SPECIES_MASS, 
     .                        DCR_RSR,TCR_FSR,DCR_FSR)
	          IF (PHA_NM .NE. 0) THEN
		       EFF_VS_MMQ = TCR_FSR * DCR_RSR * ENERGY_BANDWIDTH
	          ELSE
		       EFF_VS_MMQ = DCR_FSR * DCR_RSR * ENERGY_BANDWIDTH	
	          ENDIF

C                 check against zero efficiency
		  IF (EFF_VS_MMQ .EQ. 0.) THEN
	              EFF_VS_MMQ = DCR_FSR * DCR_RSR * ENERGY_BANDWIDTH
		      IF (EFF_VS_MMQ .EQ. 0.) EFF_VS_MMQ = 1.0E-6
	              EFF_HITS = EFF_HITS + 1
		  END IF

C				*******************
C				*   OMNI ARRAYS   *
C				*******************

C               omnidirectional, all ranges together, 
C		corrected for basic rate (sector,range),by each readout
     	        OMNI_NQNM(PHA_NQ,PHA_NM) = OMNI_NQNM(PHA_NQ,PHA_NM) + 
     .               1.*( BASIC_W(ISECT,RANGE)/EFF_VS_MMQ )

C				*******************
C				*   SOLAR ARRAYS  *
C				*******************

C		solar direction event matrix:
		IF (ISECT.EQ.5 .AND. TELE_IDENT.EQ.2) THEN       !solar wind direction

C		 corrected for basic rate (sector,range),by each readout
                 SUN_NQNM(PHA_NQ,PHA_NM) = SUN_NQNM(PHA_NQ,PHA_NM) + 
     .               1.*( BASIC_W(ISECT,RANGE)/EFF_VS_MMQ )

C		 accumulate for later correction
		 SUN1_NQNM_VS_R(DVSTEP,PHA_NQ,PHA_NM,RANGE) = 
     .		      SUN1_NQNM_VS_R(DVSTEP,PHA_NQ,PHA_NM,RANGE) + 1

		ELSE IF (ISECT.EQ.6 .AND. TELE_IDENT.EQ.2) THEN  !solar wind direction

C		 corrected for basic rate (sector,range),by each readout
                 SUN_NQNM(PHA_NQ,PHA_NM) = SUN_NQNM(PHA_NQ,PHA_NM) + 
     .               1.*( BASIC_W(ISECT,RANGE)/EFF_VS_MMQ )

C		 accumulate for later correction
		 SUN2_NQNM_VS_R(DVSTEP,PHA_NQ,PHA_NM,RANGE) = 
     .				SUN2_NQNM_VS_R(DVSTEP,PHA_NQ,PHA_NM,RANGE) + 1

                END IF
               END IF


C				**********************
C				*   DPU-RAW ARRAYS   *
C				**********************

C	       omnidirectional, all ranges, no corrections, dpu defaults used
	       IF (DPU_NQ.GE.0  .AND.  DPU_NM.GE.0) THEN
		   DPU_NQNM(DPU_NQ,DPU_NM) = DPU_NQNM(DPU_NQ,DPU_NM) + 1
               ENDIF
		

              ENDIF     !check range and/or telescope conditions
              ENDIF     !data filtering by direction 
              ENDIF	!data_filtering by coincidence conditions
              ENDIF     !data filtering by range

 300        CONTINUE		!end pha for this given voltage step

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
      IF (TABLE_FORMAT.GT.0) CLOSE(UNIT=50)                            ! Close PHA TABLE file

      DO JJ = 1,IFILE
        WRITE(55,16) INPUTFILES(JJ)
        WRITE(57,16) INPUTFILES(JJ)
      END DO              



       CALL WEIGHT_FACTORS_VS( PHA_SECT,PHA_OMNI,  		!input
     .      BR_SECT,BR_OMNI,
     .      BR_SECT_WEIGHT,BR_OMNI_WEIGHT)			!return

C      sun-direction binning
       DO ISTEP = 0,31		!voltage step
           DO INQ = 0,NQ_MAX+1	!Nq
             DO INM = 0,NM_MAX	!Nm
               DO MM = 0,2

		IF( SUN1_NQNM_VS_R(ISTEP,INQ,INM,MM).NE.0 .OR.
     .		    SUN2_NQNM_VS_R(ISTEP,INQ,INM,MM).NE.0) THEN
		
	           CALL SPECIES_IDENTIFIER( USER_NM_MID(INM), 
     .					    USER_NQ_MID(INQ) , 
     .				            SPECIES_MASS,SPECIES_CHARGE)
	           SPECIES_ENERGY = SPECIES_CHARGE * DPU_D1_EQ * (DPU_D2_EQ**ISTEP)
                   ENERGY_BANDWIDTH = 0.019 * SPECIES_ENERGY
	           CALL WSTICSEFF(SPECIES_ENERGY, SPECIES_MASS, 
     .                        DCR_RSR,TCR_FSR,DCR_FSR)
	           IF (INM .NE. 0) THEN
		       EFF_VS_MMQ = TCR_FSR * DCR_RSR * ENERGY_BANDWIDTH
	           ELSE
		       EFF_VS_MMQ = DCR_FSR * DCR_RSR * ENERGY_BANDWIDTH
	           ENDIF

C                 check against zero efficiency
		  IF (EFF_VS_MMQ .EQ. 0.) THEN
	              EFF_VS_MMQ = DCR_FSR * DCR_RSR * ENERGY_BANDWIDTH
		      IF (EFF_VS_MMQ .EQ. 0.) EFF_VS_MMQ = 1.0E-6
		  END IF

                   SUN_EFF_NQNM(INQ,INM) = SUN_EFF_NQNM(INQ,INM) + 
     .               SUN1_NQNM_VS_R(ISTEP,INQ,INM,MM) * BR_SECT_WEIGHT(5,ISTEP,MM) 
     .						   / EFF_VS_MMQ
     .	              +
     . 		     SUN2_NQNM_VS_R(ISTEP,INQ,INM,MM) * BR_SECT_WEIGHT(6,ISTEP,MM)
     .						  / EFF_VS_MMQ
		ENDIF

               END DO
             END DO
          END DO
       END DO


C   **********************************************************
C   *							     *
C   *			WRITE TO                             *
C   *                   OUTPUT matrix FILE		     *
C   *							     *
C   **********************************************************
      IF (NQ_MAX .EQ. -1) NQ_MAX = 126
      IF (NM_MAX .EQ. -1) NM_MAX = 59

      OMNI_MAX    = 0.
      SUN_MAX     = 0.
      SUN_EFF_MAX = 0.
      DO I = 1,NQ_MAX+1
         DO J = 1,NM_MAX+4
	 	IF (OMNI_NQNM(I,J) .GT. OMNI_MAX) OMNI_MAX = OMNI_NQNM(I,J)
	 	IF (SUN_NQNM(I,J)  .GT.  SUN_MAX)  SUN_MAX = SUN_NQNM(I,J)
	 	IF (SUN_EFF_NQNM(I,J) .GT. SUN_EFF_MAX) SUN_EFF_MAX = SUN_EFF_NQNM(I,J)
         END DO
      END DO

      OMNI_SCALE = OMNI_MAX / 9999.
           IF (OMNI_SCALE .LE. 1.) THEN
	     OMNI_SCALE = 1.0
           ELSE
	     OMNI_SCALE = IFIX(OMNI_SCALE) + 1.
	   END IF

      SUN _SCALE = SUN _MAX / 9999.
           IF (SUN_SCALE .LE. 1.) THEN
	     SUN_SCALE = 1.0
           ELSE
	     SUN_SCALE = IFIX(SUN_SCALE) + 1.
	   END IF

      SUN_EFF_SCALE = SUN_EFF_MAX / 9999.
           IF (SUN_EFF_SCALE .LE. 1.) THEN
	     SUN_EFF_SCALE = 1.0
           ELSE
	     SUN_EFF_SCALE = IFIX(SUN_EFF_SCALE) + 1.
	   END IF

      DO I = 0,NQ_MAX+1
         DO J = 0,NM_MAX+4

	     OMNI_NQNM(I,J) = OMNI_NQNM(I,J) / OMNI_SCALE
             IF(J.NE.0) ENCODE(5,356,OMNI_CNQNM(I,J)) OMNI_NQNM(I,J)
             IF(OMNI_NQNM(I,J) .EQ. 0) OMNI_CNQNM(I,J) = '     '

	     SUN_NQNM(I,J) = SUN_NQNM(I,J) / SUN_SCALE
             IF(J.NE.0) ENCODE(5,356,SUN_CNQNM(I,J)) SUN_NQNM(I,J)
             IF(SUN_NQNM(I,J) .EQ. 0) SUN_CNQNM(I,J) = '     '

	     SUN_EFF_NQNM(I,J) = SUN_EFF_NQNM(I,J) / SUN_EFF_SCALE
             IF(J.NE.0) ENCODE(5,356,SUN_EFF_CNQNM(I,J)) SUN_EFF_NQNM(I,J)
             IF(SUN_EFF_NQNM(I,J) .EQ. 0) SUN_EFF_CNQNM(I,J) = '     '

         END DO
      END DO

  355        FORMAT(I5)
  356        FORMAT(F5.0)

C print out matrices
  365 FORMAT(/,'  NQ ::       NM = ',/
     .              '  ||       '200(1X,I5))
  366 FORMAT(1X ,   '           '200(F6.2)/)
  367 FORMAT(/,'  NQ ::       NM = ',/
     .              '  ||       '16(1X,I5),5X,'Fe SUM Nm=49-56')
  368 FORMAT(1X ,   '           '16(F6.2),5X, '  38.45-79.28'/)
  370 FORMAT (1X,I3,1X,F5.2,F8.0,199(1X,A5))
  371 FORMAT (1X,I3,1X,F5.2,I8,199(1X,A5))
  372 FORMAT (1X,I3,1X,F5.2,I8,15(1X,A5),5X,F10.2)
  375 FORMAT (1H1,1X,' BASIC RATE + EFF CORRECTED AT EACH VOLTAGE STEP AND ALL SECTORS '/
     .            1X,' SCALE FACTOR = ' F5.2, ' EFFICIENCY HITS = 'I7)
  376 FORMAT (1H1,1X,' BASIC RATE + EFF CORRECTED AT EACH VOLTAGE STEP - SUN SECTORS '/
     .            1X,' SCALE FACTOR = ' F5.2, ' EFFICIENCY HITS = 'I7)
  373 FORMAT (1H1,1X,' BASIC RATE + EFF CORRECTED AT END OF ACCUMULATION - SUN SECTORS '/
     .            1X,' SCALE FACTOR = ' F5.2)
  374 FORMAT (1H1,1X,' RAW DATA - DPU ALGORITHMS AND DPU BINNING USED '/
     .            1X,' SCALE FACTOR  = ' F5.2)

C unit 55 is concentrated on heavy ion range + mass zero

      WRITE(55,375) OMNI_SCALE, EFF_HITS
      WRITE(55,365) (J, J=0,0),(II, II=33,NM_MAX)
      WRITE(55,366) USER_NM_M(0),(USER_NM_M(MM), MM=33,NM_MAX)
      DO I = 11,82
         WRITE(55,370) I,USER_NQ_MPQ(I),OMNI_NQNM(I,0),
     .                                (OMNI_CNQNM(I,K), K=33,NM_MAX)
      END DO

      WRITE(55,376) SUN_SCALE, EFF_HITS
      WRITE(55,365) (J, J=0,0),(II, II=33,NM_MAX)
      WRITE(55,366) USER_NM_M(0),(USER_NM_M(MM), MM=33,NM_MAX)
      DO I = 11,82
         WRITE(55,370) I,USER_NQ_MPQ(I),SUN_NQNM(I,0),
     .                                (SUN_CNQNM(I,K), K=33,NM_MAX)
      END DO

      WRITE(55,373) SUN_EFF_SCALE
      WRITE(55,365) (J, J=0,0),(II, II=33,NM_MAX)
      WRITE(55,366) USER_NM_M(0),(USER_NM_M(MM), MM=33,NM_MAX)
      DO I = 11,82
         WRITE(55,370) I,USER_NQ_MPQ(I),SUN_EFF_NQNM(I,0),
     .                                (SUN_EFF_CNQNM(I,K), K=33,NM_MAX)
      END DO




C DPU VALUES

      DPU_MAX = 0.
      DO I = 0,127
         DO J = 1,63	!exclude mass zero for finding max
	 	IF (DPU_NQNM(I,J) .GT. DPU_MAX) DPU_MAX = DPU_NQNM(I,J)
         END DO
      END DO

      DPU_SCALE = DPU_MAX / 9999.
      IF (DPU_SCALE .LE. 1.) THEN
	 DPU_SCALE = 1.0
      ELSE
	 DPU_SCALE = IFIX(DPU_SCALE) + 1.
      END IF

      DO I = 0,127
         DO J = 0,63

	     DPU_NQNM(I,J) = FLOAT(DPU_NQNM(I,J)) / DPU_SCALE
             IF(J.NE.0) ENCODE(5,355,DPU_CNQNM(I,J)) DPU_NQNM(I,J)
             IF(DPU_NQNM(I,J) .EQ. 0) DPU_CNQNM(I,J) = '     '

         END DO
      END DO

C unit 56 is the entire matrix - based on dpu values

      WRITE(56,374) DPU_SCALE
      WRITE(56,365) (J, J=0,63)
      WRITE(56,366) (DPU_NM_M(K), K=0,59)

      DO I = 0,127
         WRITE(56,371) I,DPU_NQ_MPQ(I),DPU_NQNM(I,0),
     .            (DPU_CNQNM(I,K), K=1,63)
      END DO


C unit 57 is concentrated on iron ion range + mass zero

      WRITE(57,376) SUN_SCALE, EFF_HITS
      WRITE(57,365) (J, J=0,0),(II, II=45,NM_MAX)
      WRITE(57,366) USER_NM_M(0),(USER_NM_M(MM), MM=45,NM_MAX)

      DO I = 30,82
         SUM_FE = 0.
	 DO KK = 49,56
	   SUM_FE = SUM_FE + SUN_NQNM(I,KK)*SUN_SCALE
         END DO
         WRITE(57,372) I,USER_NQ_MPQ(I),IFIX(SUN_NQNM(I,0)),
     .               (SUN_CNQNM(I,K), K=45,NM_MAX), SUM_FE
      END DO
      
      WRITE(57,373) SUN_EFF_SCALE
      WRITE(57,365) (J, J=0,0),(II, II=45,NM_MAX)
      WRITE(57,366) USER_NM_M(0),(USER_NM_M(MM), MM=45,NM_MAX)
      DO I = 30,82
         SUM_FE = 0.
	 DO KK = 49,56
	   SUM_FE = SUM_FE + SUN_EFF_NQNM(I,KK)*SUN_EFF_SCALE
         END DO
         WRITE(57,372) I,USER_NQ_MPQ(I),IFIX(SUN_EFF_NQNM(I,0)),
     .               (SUN_EFF_CNQNM(I,K), K=45,NM_MAX),SUM_FE
      END DO

      WRITE(57,374) DPU_SCALE
      WRITE(57,365) (J, J=0,0),(II, II=45,NM_MAX)
      WRITE(57,366) DPU_NM_M(0),(DPU_NM_M(MM), MM=45,NM_MAX)
      DO I = 30,82
         SUM_FE = 0.
	 DO KK = 49,56
	   SUM_FE = SUM_FE + FLOAT(DPU_NQNM(I,KK))*DPU_SCALE
         END DO
         WRITE(57,372) I,DPU_NQ_MPQ(I),DPU_NQNM(I,0),
     .            (DPU_CNQNM(I,K), K=45,59),SUM_FE
      END DO

C     ***********************************************************
C     *								*
C     *			CLOSE FILES				*
C     ***********************************************************

      CLOSE(UNIT=55)				! Close PHA MATRIX file
      CLOSE(UNIT=56)                            ! Close "DPU MATRIX" file
      CLOSE(UNIT=57)				! Close Iron file

      TYPE *,' FILE WRITTEN TO  "STICS_TABLE:"'

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


C	*************************************************
C	*		SUBROUTINES			*
C	*************************************************


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
C     17.Aug.95  add solar wind speed


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

      LOGICAL SRANGE(0:2)			!accept / reject this range
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


C***************************************************************************

      SUBROUTINE DPU_DEFAULT_COEFF(DNQ_MAX,DMQMAX,DMQMIN,   
     .		            DNM_MAX,DMMAX, DMMIN,
     .                      DEMIN,DEMAX,DTMIN,DTMAX,
     .                      DA,DB,DC1,DC2A,DC2B,DD1,DD2,DE,
     .                      DBREAK_C2, DBREAK_MQ,
     .                      DKM,DKQ,DNM_BINS,DNQ_BINS)

C***
C     This SUBROUTINE returns DPU default coefficients
C***

      IMPLICIT NONE

      REAL*4 DA(6),			!mass coefficients
     .       DB(6),      		!NM coefficients
     .       DC1,DC2A,DC2B		!mass/charge coefficients
C			 		C1= time of flight **2 of 1 keV proton
C			 		C2= carbon foil correction
      INTEGER*4 DBREAK_C2  		!NQ value of C2 break point (83)
      REAL*4    DBREAK_MQ  		!M/Q value of C2 break point (~11)

      REAL*4 DD1,DD2,	!E/Q coefficients, D1= analyzer constant
C			                   D2= step resolution
     .       DE(3)

      REAL*4 DEMIN, DEMAX,		   !min max energy and time
     .       DTMIN, DTMAX

      INTEGER*4  DNM_MAX, DNQ_MAX !DPU default max bin values

      REAL*4 DMMAX, DMMIN,    !DPU DEFAULT measurable mass range in amu 
     .       DMQMAX, DMQMIN   !DPU DEFAULT measurable m/q range in amu/e        

      REAL*4  DKM,DKQ           !NM, NQ resolution parameters
      REAL*4  DNM_BINS(0:70),  !Nm and Nq bin lower boundary values in mass 
     .        DNQ_BINS(0:130)   !	and m/q, based on DKM and DKQ and 
C				        M min,M/q min

      INTEGER*4 I,J,K
     
C   ****
C   *  Initialize constants with default dpu values (ref:  pages 345-349  
C   *  of WIND SMS COMMAND AND DATA FORMATS FOR THE SMS INSTRUMENT R.3.0) 
C   ****	

      DNM_MAX = 59
      DNQ_MAX = 126
      DMMAX  = 95.
      DMMIN  = 0.5
      DMQMAX = 42.
      DMQMIN = 0.9

      DA(1) = 2.69575
      DA(2) = -0.843766
      DA(3) = -2.38009
      DA(4) = 0.385641
      DA(5) = 0.0513127
      DA(6) = 0.0690096


      DC1 = 1.9159E-05
      DC2A= 1.5
      DC2B= 2.5
      DBREAK_C2 = 83
C          m/q = MQmin *   kQ                     **(nq-1)
      DBREAK_MQ = 0.9   * ( (42.0/0.9)**(1./126.) )**(83.-1.)

      DD1 = 6.190722
      DD2 = 1.1225857

      DEMIN = 21.0
      DEMAX = 2675.0
      DTMIN = 11.0
      DTMAX = 408.0

C  ***
C  *              calculate binning parameters                
C  *              M(lower bound of Nm) = Mmin * kM**(Nm-1)    
C  *              M/Q(lower bound of Nq) = MQmin * kQ**(Nq-1) 
C  ***

      DKM = ( DMMAX/DMMIN )**(1./(FLOAT(DNM_MAX)-1.))   !substract 1 from
C                                                    nm_max (= overflow m)
      DKQ = (DMQMAX/DMQMIN)**(1./FLOAT(DNQ_MAX))

      DO I = 0,70
         DNM_BINS(I) = 0.
         IF(I.GT.0.  .AND.  I.LE.DNM_MAX) 
     .        DNM_BINS(I) = DMMIN * DKM**(FLOAT(I)-1.)
      END DO

      DO I = 0, 130
         DNQ_BINS(I) = 0.
         IF(I.GT.0  .AND.  I.LE.DNQ_MAX+1) 
     .        DNQ_BINS(I) = DMQMIN *DKQ**(FLOAT(I)-1.)
      END DO

C     coefficients related to both D values and binning resolution
      	DE(1) = -351.6997334
      	DE(2) = 32.7867706
      	DE(3) = 65.5735412

C     coefficients related to both A values and binning resolution
        DB(1) = 38.4605
        DB(2) = -9.32689
        DB(3) = -26.3092
        DB(4) = 4.26283
        DB(5) = 0.567205
        DB(6) = 0.762824


      RETURN
      END

C**************************************************************************
      SUBROUTINE USER_NMNQ_COEFF(UNQ_MAX,UMQMAX,UMQMIN,    !user input
     .		            UNM_MAX,UMMAX, UMMIN,
     .                      EMIN,EMAX,TMIN,TMAX,      !return values
     .                      NQ_MAX,MQMAX,MQMIN,
     .                      NM_MAX,MMAX ,MMIN,
     .                      KM,KQ,
     .                      A,B,C1,C2A,C2B,D1,D2,E,
     .                      BREAK_C2, BREAK_MQ)
C*******************************************************************
C     This SUBROUTINE calculates coefficients for nm and nq        *
C     parameters         					   *
C     Original version 24 Jan 95 by ABG                            *
C     History:							   *
C                                                                  *
c*******************************************************************
      IMPLICIT NONE

      REAL*4 A(6),	!mass coefficients
     .       B(6),      !NM coefficients
     .       C1,C2A,C2B	!mass/charge coefficients
C			 C1= time of flight **2 of 1 keV proton
C			 C2= carbon foil correction
      INTEGER*4 BREAK_C2  !NQ value of C2 break point (83)
      REAL*4    BREAK_MQ  !M/Q value of C2 break point (~11)

      REAL*4 D1,D2,	!E/Q coefficients, D1= analyzer constant
C			                   D2= step resolution
     .       E(3)

      INTEGER*4 DEFAULT_NQ,DEFAULT_NM   !default dpu algorithm used

      REAL*4 EMIN, EMAX,		!min max energy and time
     .       TMIN, TMAX

      INTEGER*4  NM_MAX,  NQ_MAX, 	   !max bin values 
     .           UNM_MAX, UNQ_MAX, 	   !USER max bin values (-1 = use default)
     .           DNM_MAX/59/, DNQ_MAX/126/ !DPU default max bin values

      REAL*4 MMAX,  MMIN,    !measurable mass range in amu 
     .       MQMAX, MQMIN,   !measurable m/q range in amu/e
     .       UMMAX, UMMIN,   !USER measurable mass range in amu (-1 = use default)
     .       UMQMAX,UMQMIN,  !USER measurable m/q range in amu/e (-1 = use default)
     .       DMMAX  /95./, DMMIN  /0.5/,  !DPU DEFAULT measurable mass range in amu 
     .       DMQMAX /42./, DMQMIN /0.9/   !DPU DEFAULT measurable m/q range in amu/e        

      REAL*4  KM,KQ           !NM, NQ resolution parameters

      INTEGER*4 I,J,K
     
C   ***********************************************************************
C   *  Initialize constants with default dpu values (ref:  pages 345-349  *
C   *  of WIND SMS COMMAND AND DATA FORMATS FOR THE SMS INSTRUMENT R.3.0) *
C   ***********************************************************************	

      A(1) = 2.69575
      A(2) = -0.843766
      A(3) = -2.38009
      A(4) = 0.385641
      A(5) = 0.0513127
      A(6) = 0.0690096


      C1 = 1.9159E-05
      C2A= 1.5
      C2B= 2.5
      BREAK_C2 = 83
C          m/q = MQmin *   kQ                     **(nq-1)
      BREAK_MQ = 0.9   * ( (42.0/0.9)**(1./126.) )**(83.-1.)

      D1 = 6.190722
      D2 = 1.1225857

      EMIN = 21.0
      EMAX = 2675.0
      TMIN = 11.0
      TMAX = 408.0

C   ***********************************************************************
C   *  determine if default (i.e., dpu) binning will be used, in whole or *
C   *  in part, or not at all.  Otherwise user input for min, max, number *
C   *  of bins will be used.                                              *
C   ***********************************************************************

      DEFAULT_NQ = 0		!initialize flag for dpu default
      DEFAULT_NM = 0
      IF(UMMIN.LT.0.  .AND.  UMMAX.LT.0.   .AND.   UNM_MAX.LT.0)
     .   DEFAULT_NM = 1		!if true, dpu bins for nm to be used
      IF(UMQMAX.LT.0. .AND.  UMQMIN.LT.0.  .AND.   UNQ_MAX.LT.0) 
     .   DEFAULT_NQ = 1		!if true, dpu bins for nq to be used

      IF(UMMIN.LT.0.)  THEN
             MMIN = DMMIN
      ELSE
             MMIN = UMMIN
      ENDIF

      IF(UMMAX.LT.0.) THEN
              MMAX = DMMAX
      ELSE
              MMAX = UMMAX
      ENDIF

      IF(UMQMIN.LT.0.) THEN
              MQMIN = DMQMIN
      ELSE
              MQMIN = UMQMIN
      ENDIF

      IF(UMQMAX.LT.0.) THEN
              MQMAX = DMQMAX 
      ELSE
              MQMAX = UMQMAX
      ENDIF

      IF(UNM_MAX.LT.0) THEN
            NM_MAX = DNM_MAX
      ELSE
            NM_MAX = UNM_MAX +1   !(DPU formulas use overflow mass as nmmax)
      ENDIF

      IF(UNQ_MAX.LT.0) THEN
            NQ_MAX = DNQ_MAX
      ELSE
            NQ_MAX = UNQ_MAX
      ENDIF



C  ************************************************************
C  *              calculate binning parameters                *
C  *              M(lower bound of Nm) = Mmin * kM**(Nm-1)    *
C  *              M/Q(lower bound of Nq) = MQmin * kQ**(Nq-1) *
C  ************************************************************

      KM = ( MMAX/MMIN )**(1./(FLOAT(NM_MAX)-1.))   !substract 1 from
C                                                    nm_max (= overflow m)
      KQ = (MQMAX/MQMIN)**(1./FLOAT(NQ_MAX))


      IF (DEFAULT_NQ .EQ. 1) THEN
      	E(1) = -351.6997334
      	E(2) = 32.7867706
      	E(3) = 65.5735412
      ELSE
	E(1) = 1. + (  (ALOG(C1) - ALOG(MQMIN))/ALOG(KQ) )
        E(2) = 1. / ALOG(KQ)
        E(3) = 2. / ALOG(KQ)
        BREAK_C2 = IFIX ( ALOG(BREAK_MQ/MQMIN) / ALOG(KQ) )
      END IF

      IF (DEFAULT_NM .EQ. 1) THEN
        B(1) = 38.4605
        B(2) = -9.32689
        B(3) = -26.3092
        B(4) = 4.26283
        B(5) = 0.567205
        B(6) = 0.762824
      ELSE
        B(1) = 1. + (  (A(1)-ALOG(MMIN)) / ALOG(KM) )
        B(2) = A(2) / ALOG(KM)
        B(3) = A(3) / ALOG(KM)
        B(4) = A(4) / ALOG(KM)
        B(5) = A(5) / ALOG(KM)
        B(6) = A(6) / ALOG(KM)
      ENDIF

      RETURN
      END


C**************************************************************************
      
      SUBROUTINE USER_NMNQ_PHA_BINNED(DPPSP,DPPSM,DVSTEP,ECHN,TCHN,    !this pha event
     .                      NQ_MAX,MQMAX,MQMIN,    !information for binning
     .                      NM_MAX,MMAX, MMIN,
     .                      EMIN,EMAX,TMIN,TMAX,      
     .                      KM,KQ,
     .                      A,B,C1,C2A,C2B,D1,D2,E,
     .                      BREAK_C2, BREAK_MQ,
     .                      NQ,NM)			!return information

C*******************************************************************
C     This SUBROUTINE calculates ln bins for MASS and MASS/CHARGE  *
C     given the number of bins requested by the user, plus         * 
C     the status of the +/- DPPS, DV Step number, the compressed   *
C     energy channel number,and the time-of-flight channel number  *
C     for WIND-STICS. The algorithms for m, m/q are those used by  *
C     the DPU  as of July 17, 1992.                                *
C     Original version 24 Jan 95 by ABG from a routine by SChotoo. *
C     History:
C                
c*******************************************************************
      IMPLICIT NONE

      REAL*4 A(6),	!mass coefficients
     .       B(6),      !NM coefficients
     .       C1,C2A,C2B	!mass/charge coefficients
C			 C1= time of flight **2 of 1 keV proton
C			 C2= carbon foil correction
      INTEGER*4 BREAK_C2  !NQ value of C2 break point (83)
      REAL*4    BREAK_MQ  !M/Q value of C2 break point (~11)

      REAL*4 D1,D2,	!E/Q coefficients, D1= analyzer constant
C			                   D2= step resolution
     .       E(3)
      INTEGER*4 DPPSP,  !enable bit for dpps plus plate
     .          DPPSM,  !enable bit for dpps minus plate
     .          DVSTEP, !deflection voltage step number 
     .          ECHN    !compressed energy channel number
      REAL*4 ED,        !decompressed energy channel number            
     .       EMEAS,     !measured energy in keV
     .       EMIN,EMAX, !measurable energy range in keV 
     .       EPQ,       !E/Q
     .       M,         !mass in amu
     .       MPQ	!mass per charge in amu/e
      INTEGER*4  NM,NQ   		   !mass bin, m/q bin
      INTEGER*4  NM_MAX,  NQ_MAX 	   !max bin values 

      REAL*4 MMAX,  MMIN,    !measurable mass range in amu 
     .       MQMAX, MQMIN   !measurable m/q range in amu/e

      REAL*4  KM,KQ           !NM, NQ resolution parameters

   
      INTEGER*4 TCHN    !time channel number
      REAL*4 TEMP,         
     .       TMIN,TMAX, !measurable time-of-flight range in ns
     .       TOF,       !measured time-of-flight in ns
     .       XX,YY

      INTEGER*4 I,J,K
     
C  **************************************************************
C  *			PHA Channel number conversions	 	*
C  **************************************************************

C     Energy Channel Number (ECHN) must first be decompressed (ED) 
C     from 9-bit to 10-bit
      IF (ECHN .LT. 256) ED = ECHN
      IF ((ECHN .GE. 256).AND.(ECHN .LT. 384))ED = 2.0*ECHN - 256.0 + 0.5
      IF (ECHN .GE. 384) ED = 4.0*ECHN - 1024.0 + 1.5

C     Calculate measured energy in keV from decompressed channel number
      EMEAS = (ED + 6)/0.37654782

     
C     Calculate time of flight in ns from time channel number
      TOF = (TCHN - 44)/2.3725306895


C  ******************************************************************
C  *                   E/Q, M/Q, and M algorithms                   *
C  *                   Nm, Nq values                                *
C  ******************************************************************

C     Calculate the e/q value based on the default dpu algorithm and constants.
          IF(IAND(DPPSP, DPPSM).EQ.1)THEN               !both plates on
            EPQ = D1 * (D2**DVSTEP)
          ELSE IF (DPPSP.EQ.1 .OR. DPPSM.EQ.1) THEN     !one plate on  
            EPQ = 0.5 * D1 * (D2**DVSTEP)                
          ELSE                                          !neither plate on
            EPQ = 0
          ENDIF

C     Calculate mass per charge in amu/e
    
      MPQ = C1*(EPQ - C2A)*(TOF**2) 
      IF (MPQ .GE. BREAK_MQ) MPQ = C1*(EPQ - C2B)*(TOF**2)   

      IF (TOF .GT. 0) THEN
           NQ = E(1) + E(2)*ALOG(D1* (D2**DVSTEP) - C2A) +
     .          E(3)*ALOG(TOF)
           IF (NQ .GE. BREAK_C2)       
     .      NQ = E(1) + E(2)*ALOG(D1* (D2**DVSTEP) - C2B) +
     .     	 E(3)*ALOG(TOF)
           IF (NQ .GT. NQ_MAX) NQ = NQ_MAX+1	!M/Q overflow
           IF (NQ .LT. 1  ) NQ = 0	        !M/Q underflow
      ELSE
         NQ = 0
      ENDIF

C     Calculate mass in amu
      IF ((EMEAS .GT. EMIN) .AND. (EMEAS .LT. EMAX) .AND.                
     .    (TOF   .GT. TMIN) .AND. (TOF   .LT. TMAX) ) THEN

         XX = ALOG(EMEAS)
         YY = ALOG(TOF)
         TEMP = A(1) + A(2)*XX + A(3)*YY + A(4)*XX*YY
         TEMP = TEMP + A(5)*XX*XX + A(6)*YY*YY*YY
         M = EXP(TEMP)

         NM = B(1) + B(2)*XX + B(3)*YY + B(4)*XX*YY +
     .        B(5)*XX*XX + B(6)*YY*YY*YY

         IF (NM .GE. NM_MAX) THEN
              NM =  NM_MAX		!overflow mass class  (59)
	 ELSE IF (NM .LT. 1) THEN
              NM =  NM_MAX + 1		!underflow mass class (60)
	 ENDIF
      ELSE IF(EMEAS .LE. EMIN) THEN
	 M   = 0
         NM  = 0                       !mass zero event
      ELSE IF (EMEAS .GE. EMAX) THEN
	 M   = 0
         NM  = NM_MAX + 2	       !energy overflow (61)
      ELSE IF (TOF .LE. TMIN) THEN
         M   = 0 
	 NM  = NM_MAX + 3              !time underflow (62)
      ELSE IF (TOF .GE. TMAX) THEN
	 M   = 0
         NM  = NM_MAX + 4              !time overflow (63)
      END IF             


      RETURN
      END


C**************************************************************************
C
       SUBROUTINE TELESCOPE_ID (TIME_OF_FLIGHT,ENERGY,
     .                PSTART,STOP_ID,SSD_ID, TELE_ID, CHECK_TELE)
C
C      this subroutine determines which telescope (1, 2, 3) the pha word comes 
C		from based on PSTART, or if the information from all the detectors 
C		is inconsistent, the check_tele flag is set
C      HISTORY:  derived from TELESCOPE_CHECK, 18Aug95 - ABG

       INTEGER*4 ENERGY,		!energy pulse height
     .           TIME_OF_FLIGHT,	!time pulse height
     .           PSTART,		!FSR id
     .           STOP_ID,		!RSR id
     .           SSD_ID,		!Solid state detector id
     .           TELE_ID,		!based on PSTART if TOF>0, or SSD if only E>0
     .           CHECK_TELE		!flag = 0 if all detectors match, 1 if not


                 CHECK_TELE = 0		!initialize
		 TELE_ID    = 0         

C                ssd checks
	  	 IF (SSD_ID.EQ.0 .AND. ENERGY.NE.0) CHECK_TELE = 1
	         IF (SSD_ID.EQ.1 
     .               .AND.
     .                    (PSTART.NE.1 .AND. PSTART.NE.2 .AND.
     .                     PSTART.NE.7 .AND. PSTART.NE.0) ) CHECK_TELE = 1
	         IF (SSD_ID.EQ.2 
     .               .AND.
     .                (PSTART.NE.3 .AND. PSTART.NE.4 .AND.
     .                 PSTART.NE.8 .AND. PSTART.NE.0) ) CHECK_TELE = 1
	         IF (SSD_ID.EQ.3 
     .                 .AND.
     .                (PSTART.NE.5 .AND. PSTART.NE.6 .AND.
     .                 PSTART.NE.9 .AND. PSTART.NE.0) ) CHECK_TELE = 1
	         IF (SSD_ID.EQ.1 
     .               .AND.
     .                (STOP_ID.NE.1 .AND. STOP_ID.NE.0) ) CHECK_TELE = 1
	         IF (SSD_ID.EQ.2 
     .               .AND.
     .                (STOP_ID.NE.2 .AND. STOP_ID.NE.0) ) CHECK_TELE = 1
	         IF (SSD_ID.EQ.3 
     .               .AND.
     .                (STOP_ID.NE.3 .AND. STOP_ID.NE.0) ) CHECK_TELE = 1

C                stop checks
	  	 IF (STOP_ID.EQ.0 .AND. TIME_OF_FLIGHT.NE.0) CHECK_TELE = 1
	         IF (STOP_ID.EQ.1 
     .               .AND.
     .                    (PSTART.NE.1 .AND. PSTART.NE.2 .AND.
     .                     PSTART.NE.7 .AND. PSTART.NE.0) ) CHECK_TELE = 1
	         IF (STOP_ID.EQ.2 
     .               .AND.
     .                (PSTART.NE.3 .AND. PSTART.NE.4 .AND.
     .                 PSTART.NE.8 .AND. PSTART.NE.0) ) CHECK_TELE = 1
	         IF (STOP_ID.EQ.3 
     .                 .AND.
     .                (PSTART.NE.5 .AND. PSTART.NE.6 .AND.
     .                 PSTART.NE.9 .AND. PSTART.NE.0) ) CHECK_TELE = 1
	         IF (STOP_ID.EQ.1 
     .               .AND.
     .                (SSD_ID.NE.1 .AND. SSD_ID.NE.0) ) CHECK_TELE = 1
	         IF (STOP_ID.EQ.2 
     .               .AND.
     .                (SSD_ID.NE.2 .AND. SSD_ID.NE.0) ) CHECK_TELE = 1
	         IF (STOP_ID.EQ.3 
     .               .AND.
     .                (SSD_ID.NE.3 .AND. SSD_ID.NE.0) ) CHECK_TELE = 1


C                start checks
C			start id only if TOF>0
	  	 IF (PSTART.EQ.0 .AND. TIME_OF_FLIGHT.NE.0) CHECK_TELE = 1

C			start id consistent with stop id
  	         IF ( (PSTART.EQ.1 .OR. PSTART.EQ.2  .OR. PSTART.EQ.7)
     .               .AND.
     .                (STOP_ID.NE.1 .AND. STOP_ID.NE.0) ) CHECK_TELE = 1
	         IF ( (PSTART.EQ.3 .OR. PSTART.EQ.4  .OR. PSTART.EQ.8)
     .               .AND.
     .                (STOP_ID.NE.2 .AND. STOP_ID.NE.0) ) CHECK_TELE = 1
	         IF ( (PSTART.EQ.5 .OR. PSTART.EQ.6  .OR. PSTART.EQ.9)
     .               .AND.
     .                (STOP_ID.NE.3 .AND. STOP_ID.NE.0) ) CHECK_TELE = 1

C			start id consistent with solid state detector id
	         IF ( (PSTART.EQ.1 .OR. PSTART.EQ.2  .OR. PSTART.EQ.7)
     .               .AND.
     .                (SSD_ID.NE.1 .AND. SSD_ID.NE.0) ) CHECK_TELE = 1
	         IF ( (PSTART.EQ.3 .OR. PSTART.EQ.4  .OR. PSTART.EQ.8)
     .               .AND.
     .                (SSD_ID.NE.2 .AND. SSD_ID.NE.0) ) CHECK_TELE = 1
	         IF ( (PSTART.EQ.5 .OR. PSTART.EQ.6  .OR. PSTART.EQ.9)
     .               .AND.
     .                (SSD_ID.NE.3 .AND. SSD_ID.NE.0) ) CHECK_TELE = 1

	
C  	find telescope ID if check_tele ok:
        IF (CHECK_TELE.EQ.0) THEN
	  IF (PSTART.NE.0) THEN
  	    IF (PSTART.EQ.1 .OR. PSTART.EQ.2  .OR. PSTART.EQ.7) TELE_ID = 1
	    IF (PSTART.EQ.3 .OR. PSTART.EQ.4  .OR. PSTART.EQ.8) TELE_ID = 2
	    IF (PSTART.EQ.5 .OR. PSTART.EQ.6  .OR. PSTART.EQ.9) TELE_ID = 3
	  ELSE IF (SSD_ID.NE.0) THEN
	    IF (SSD_ID.EQ.1) TELE_ID = 1
	    IF (SSD_ID.EQ.2) TELE_ID = 2
	    IF (SSD_ID.EQ.3) TELE_ID = 3
	  ENDIF
        ENDIF

       RETURN
       END

C ***************************************************************************
 
       SUBROUTINE WEIGHT_FACTORS_VS(PHA_S,PHA_SUMMED,
     .                           BR_S,BR_SUMMED,
     .                           W_S,W_SUMMED)


C   **********************************************************
C   *   DETERMINE WEIGHTING FACTORS BASED ON IRECORDS 	     *
C   **********************************************************

      IMPLICIT NONE

      INTEGER*4 II,JJ,KK

      INTEGER*4 PHA_S (1:8,0:31,0:2),
     .          PHA_SUMMED(0:31,0:2),
     .          BR_S (1:8,0:31,0:2),
     .          BR_SUMMED(0:31,0:2)

      REAL*4    W_S (1:8,0:31,0:2),
     .          W_SUMMED(0:31,0:2)


      DO II = 0,31 	!dv step
         DO JJ = 0,2	!range

	   IF (PHA_SUMMED(II,JJ) .GT.0) THEN
             W_SUMMED(II,JJ) = FLOAT(BR_SUMMED(II,JJ)) / FLOAT(PHA_SUMMED(II,JJ))
           ELSE
             W_SUMMED(II,JJ) = 1.
           END IF


           DO KK=1,8  	!sector
	      IF (PHA_S(KK,II,JJ) .GT.0) THEN
                 W_S(KK,II,JJ) = FLOAT(BR_S(KK,II,JJ)) / FLOAT(PHA_S(KK,II,JJ))
              ELSE
                 W_S(KK,II,JJ) = 1.
              END IF
	   END DO	!sector


         END DO         !range
      END DO            !dv step

        RETURN
        END
