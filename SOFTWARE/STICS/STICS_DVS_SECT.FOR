CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      PROGRAM STICS_DVS_SECT
C
C This program reads a WIND L1 data file and dissects the PHA data, calculates 
C the m & m/q for each event, then bins them to the corresponding ion, with 
C user requested data selection criteria imposed.
C Data has basic rate, deadtime, and efficiency corrections applied.
C
C STICS_PHA_SPECTRA created by K.CHOTOO using STICS_PHA_SPECTRA as a template
C
C Revision History:
C	14-JAN-1997	K.CHOTOO	Changed Geometric Factor to those in Masters
C					Paper. Corrected time of spin to time of sector.
C					Now using MASS speed routine supplied by George 
C					Ho.
C	 2-SEP-1997	K.CHOTOO	Fixed problem with getting start GSE position.
C					Get solar wind velocity in cartesian coordinates
C					from SWE key parameter data. 
C					Use SWE for solar wind speed instead of MASS.
C	 				Use new fudge factor (0.72 +/- 0.18) for parallel 
C					beam geometric factor = 0.68 +/- 0.17

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
      CHARACTER*90 LISFIL1,LISFIL2	!holds the name of the input file for use
     					!  in creating the default output file names
      CHARACTER OUTFILENAME1*90, OUTFILENAME2*90
      CHARACTER*40 USERNAME1		!user defined output file name

      LOGICAL FIRSTFILE			!set to true if this is the first input file 

      CHARACTER*4 LABELS(1:30)		!Ion names
      DATA LABELS/ 'H',    'He2',  'He1',   'C6',
     +             'C5',   'C4',   '07',    'O6',
     +             'O1',   'Ne8',  'Mg10',  'Mg8',
     +		   'Si12', 'Si9',  'Si8',   'Fe16',
     +		   'Fe14', 'Fe12', 'Fe11',  'Fe10',
     +             'Fe9',  'Fe8',   8*' '/
      INTEGER*4 ION_CHOICE		!Selected ion number

      REAL*4 CHARGE_ION(1:30), MASS_ION(1:30) 
      DATA MASS_ION/1.000,3.971,3.971,11.916,11.916,11.916,15.872,15.872,15.872,20.019,24.112,24.112,
     .              27.863,27.863,27.863,55.404,55.404,55.404,55.404,55.404,55.404,55.404,
     .              0,0,0,0,0,0,0,0/

      DATA CHARGE_ION/1.0, 2.0, 1.0, 6.0, 5.0, 4.0, 7.0, 6.0, 1.0, 8.0, 10.0, 8.0, 12.0,
     .                9.0, 8.0, 16.0, 14.0, 12.0, 11.0, 10.0, 9.0, 8.0, 0,0,0,0,0,0,0,0/


C  	************************************************
C	*		file, record or data status     *
C	************************************************

      INTEGER*4 STATUS			!did L1 file open successfully
      LOGICAL   GOODSPIN		!data record has good data

C  	************************************************
C	*		time search status     *
C	************************************************

      LOGICAL COMPARE_TIME_GE,			!check on whether time is inbounds
     .        STARTFOUND,ENDFOUND,		!start/end time found?
     .        USE_SC_CLOCK			!which time on L1 to use (wall or s/c)

      INTEGER*4 MONTH,DAYOFMONTH,		 !user requested start and stop times
     .          ENDMONTH,ENDDAYOFMONTH,		 ! month and day of month
     .		MON,DAY				 !Month and day for getting SWE data

      CHARACTER RUN_TIME*8, RUN_DATE*9		!run time and date

c		********************************
C               *   Spacecraft position in Re  *
C		********************************
      REAL*4 X1_GSE,Y1_GSE,Z1_GSE,     		!Start position
     .       X2_GSE,Y2_GSE,Z2_GSE		!Stop position
      INTEGER*4 START_POS/1/			!Change to -1 after getting start position

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
     .		DVSTEP,			!DPPS voltage step number (0-31)
     .          BAD_HDB/0/		!Number of Bad HDBs

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
     .          DPU_MASS_OVER_Q,
     .		DPU_EPQ(0:31)
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

C  STICS rates
      INTEGER*4 WIND_DECOMPRESS			!function to return decompressed rates
      INTEGER*4 STICS_RATES(179)		!all the stics rates
      REAL*4    BASIC_RATE (0:31,1:8,0:2), 	!Basic rate arrays accumulated over entire data set
     .          BASIC_PHA(0:31,0:15,0:2), 	!Counters for pha falling within a basic rate domain
     .		BASIC_W(0:31,0:15,0:2),		!Basic rate weight factor weight factors
     .   	CORR_DATA(0:31,0:15,0:2,1:3),	!Data corrected by efficiency, BR norm, DVS hits
     .		DEADTIME_CORR(0:15),		!Sector accumulation deadtime
     .		DVS_HITS(0:31),			!Counts number of hits for each voltage step
     .   	RAW_COUNTS(0:31,0:15,0:2,1:3),	!Raw PHA data
     .          SECT_DATA_COR(0:31,0:15,1:3), 	!Corrected Omni-directional data
     .          SECT_DATA_RAW(0:31,0:15,1:3)	!Omni-directional raw data

       REAL*4  EFF_CORR(0:31,1:30)	     !Efficiency correction by voltage step and ion
       REAL*4  EFF /1.0/, UNUSED_PHA
       INTEGER*4 ION_INDEX			!Ion array index

       INTEGER*4 FLUX_TYPE			!1 for omni-directional and 2 for parallel beam
       REAL*4 BETA/3.0/,			!Azimuthal acceptance angle
     .	      DELTA_T,				!Delta time
     .	      ENERGY_BANDWIDTH,	     		!energy bandwidth allowed 
     .        GEO_FAC,				!Geometric Factor
     .        SPIN_TIME/3.0/			!Spacecraft spin time = 3.0 secs


C  	************************************************
C	*    MASS derived Solar Wind values           *
C	************************************************

       REAL*4  AVG_SPEED_X,		!Average solar wind speed in x-direction
     . 	       AVG_SPEED_Y,		!Average solar wind speed in y-direction
     . 	       AVG_SPEED_Z,		!Average solar wind speed in z-direction
     .         BULK_SPEED,		!Solar wind speed
     .         BULK_UNCER,		!Uncertainty in solar wind speed from width
     .	       GSE_POS_SWE(3),		!Spaceraft position in GSE coordinates.
     .	       GSE_VEL(3),		!H+ solar wind speed in cartesian coord.
     .	       GSE_VEL_P(3),		!"   "     "     "    " spherical   "
     .	       H_DENSITY,		!H+ density
     .	       HIGH_SPEED,		!Highest valid speed encountered
     .	       HITS_XYZ,		!Number of valid xyz speeds
     .	       LOW_SPEED,		!Lowest valid speed encountered
     .         SPEED_MEAN,		!Average speed over interval
     .         SPEED_HITS,		!Number of valid speed values in interval
     .	       SPEED_SIGMA,		!Standard deviation of speed
     .         SUM_HE2_SPEED,		!Sum of alpha speeds over interval
     .	       SUM_SPEED_X,
     .	       SUM_SPEED_Y,
     .	       SUM_SPEED_Z,
     .	       SUM_SW_SPEED,
     .	       SUM_SQ_SPEED,		!Sum of speed squared
     .         SW_SPEED,                !chosen solar wind speed to check against                      
     .	       SW_SPEED_X,SW_SPEED_Y,SW_SPEED_Z,
     .	       THERM_VEL,		!Thermal velocity
     .	       TIME_DIFF		!Time difference in seconds between SWE time 
C					 and time sent to SWE subroutine.

      INTEGER*4 BAD_BR/0/,		!Number of times Sum of BR < Sum PHA
     .		BAD_DVS/0/,		!Number of bad voltage steps
     .		BAD_SPEED/0/,		!Number of invalid speed values
     .		DPPS_STEP_MODE,		!DPPS stepping mode
     .		DVS_CHECK,		!-1 for invalid DVS
     .		ERR_SPD_FLAG,		!Error speed flag
     .		FIRSTSPEED/0/,		!Used to get speed at start time
     .		GOOD_BR/0/    		!Number of good basic rates
      CHARACTER*80 ERR_MESSAGE		!Message for invalid speed
	
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
      WRITE(6,*)'Enter name for output file or <RETURN> for default name'
      READ(5,'(A)') USERNAME1
C
C Prompt user for ion
C
      WRITE(6,33)
  33  FORMAT(//,5X,'Which ion do you want from the following list:',//
     +	      10X, ' [1] H      [2] He2     [3] He1    [4] C6',/
     +        10X, ' [5] C5     [6] C4      [7] 07     [8] O6',/
     +        10X, ' [9] O1    [10] Ne8    [11] Mg10  [12] Mg8',/
     +        10X, '[13] Si12  [14] Si9    [15] Si8   [16] Fe16',/
     +        10X, '[17] Fe14  [18] Fe12   [19] Fe11  [20] Fe10',/
     +        10X, '[21] Fe9   [22] Fe8'//,
     +	       5X, 'Enter selection number:  ',$)
      READ(5,*)ION_CHOICE
      DO WHILE ((ION_CHOICE .LT. 1) .OR. (ION_CHOICE .GT. 22))
	WRITE(6,*)'Invalid selection. Please select again'
	READ(5,*)ION_CHOICE
      END DO
      WRITE(6,*)
      WRITE(6,*)
      WRITE(6,*)'    Selected ion => ',LABELS(ION_CHOICE)
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
      DATA_FILTER_MSPEED = -1        	!no check on SWE solar wind speed
      DATA_MSPEED_MAX    = 1100     		!max sw speed selection is 1100 km/sec
      DATA_MSPEED_MIN    = 250      		!min sw speed selection is 250 km/sec


        TYPE 6
    6   FORMAT(//,' Is data filtering by DATA TYPE desired? '
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

C      TYPE 9
C   9  FORMAT(//,' Is the flux omni-directional(1) or a parallel beam(2):  ',$)
C      ACCEPT*, FLUX_TYPE
      FLUX_TYPE = 1
      IF(FLUX_TYPE .EQ. 2) THEN
	GEO_FAC = 0.68				!parallel beam Geo Fac for each telescope
	DELTA_T = SPIN_TIME * BETA/360.0 	!time oberserving sun
      ELSE
	GEO_FAC = 0.029				!isotropic Geo Fac for each telescope
	DELTA_T = SPIN_TIME/16.0		!time for each sector
      ENDIF

C
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

       HIGH_SPEED = 0
       LOW_SPEED = 2000
       SW_SPEED = 0
       SUM_SQ_SPEED = 0
       SUM_SW_SPEED = 0
       SUM_SPEED_X = 0
       SUM_SPEED_Y = 0
       SUM_SPEED_Z = 0
       SPEED_HITS = 0
       FIRSTFILE = .TRUE.

       DO I = 0,31
	 DO J = 0,15
	   DO K = 0,2
             BASIC_PHA(I,J,K) = 0
	     BASIC_W(I,J,K) = 0
      	   END DO
	 END DO
       END DO

       DO I = 0,31
	 DO J = 1,8
	   DO K = 0,2
             BASIC_RATE(I,J,K) = 0
      	   END DO
	 END DO
       END DO
 
       DO I = 0,31					!voltage step
          DVS_HITS(I) = 0
	  DPU_EPQ(I) = 6.190722 * (1.1225857**I)
          DO L=1,3					!Ion
            DO J= 0,15
		SECT_DATA_RAW(I,J,L) =0
		SECT_DATA_COR(I,J,L) =0
		DO K = 0,2				!range
                    RAW_COUNTS(I,J,K,L) = 0
		    CORR_DATA(I,J,K,L) = 0
                END DO
	    END DO
	  END DO
       END DO
C
C At the start of sectors 1-15 there is a 12 ms deadtime during which no data is accumulated.
C The deadtime for sector 0 is 100 ms. Time for each sector = Spin Period/16. For nominal
C spin period of 3 secs, each sector time = 0.1875 sec. Therefore, deadtime corrections are:
C      	Sector 0    = 0.1875/(0.1875 - 0.100) = 2.143
C       Sector 1-15 = 0.1875/(0.1875 - 0.012) = 1.068
C
       DEADTIME_CORR(0) = 2.143
       DO I = 1,15
         DEADTIME_CORR(I) = 1.068
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
      LISFIL1(1:9) = '  SEC_DVS'
      LISFIL2 = LISFIL1
      LISFIL1(JSAVE:JSAVE+7) = 'FLX.'//LABELS(ION_CHOICE)     
      LISFIL2(JSAVE:JSAVE+7) = 'ERR.DAT'

      OUTFILENAME2 = LISFIL2
      IF(USERNAME1(1:12) .NE. '            ') THEN
        OUTFILENAME1 = USERNAME1
      ELSE
        OUTFILENAME1 = LISFIL1 				! default matrix filename
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
        TONOFF = ' ON'
C
C Open  output files
C
       IF(IFILE.EQ.1) THEN
	   OPEN(UNIT= 55, NAME= OUTFILENAME1, FORM='FORMATTED', TYPE='NEW', RECL=400)

	   OPEN(UNIT= 56, NAME= OUTFILENAME2, FORM='FORMATTED', TYPE='NEW', RECL=400)

	   CALL TIME(RUN_TIME)			!Get run time
	   CALL DATE(RUN_DATE)			!Get run date

           WRITE(55,'(A)') '  STICS SECTORED FLUX FOR '//LABELS(ION_CHOICE)
 	   WRITE(55,'(A)') ' ============================='
	   WRITE(55,'(A,I2)') '  Ion Number = ',ION_CHOICE
	   WRITE(55,'(A)') '  Run time  '//RUN_TIME//'   '//RUN_DATE
	   WRITE(55,'(A)') '  Flux Type: omni-directional'
           WRITE(55,17) OUTFILENAME1
	   
	   WRITE(56,'(A,A27)')' Errors found while generating the file : ',OUTFILENAME1
	   WRITE(56,*)'  Run time  '//RUN_TIME//'   '//RUN_DATE  
	   WRITE(56,*)
	   WRITE(56,*)' DOY:HR:MN:SEC                  COMMENTS'

           WRITE(55,18) 'Wall',TRIGGER_MODE(TRIGGER_MODE_CODE),FIFO,TGS,MODE_AT_START 
                       
           WRITE(55,19)DATA_DVSTEP_MIN, DATA_DVSTEP_MAX,COINC(DATA_FILTER_COINC),
     .              RANGESEL(DATA_FILTER_RANGE),
     .              RONOFF,DATA_MSPEED_MIN,DATA_MSPEED_MAX,TONOFF,
     .              START.YEAR,MONTH, DAYOFMONTH,START.HR,START.MIN,START.SEC,
     .              END.YEAR,ENDMONTH,ENDDAYOFMONTH,END.HR,END.MIN,END.SEC

       ENDIF		!first input file

C    format statements for headers:

  16    FORMAT('  ',T10,'Input Level 1 Filename: ',A)
  17    FORMAT(T3 ,'Output Filename: ',A27)
  18    FORMAT(T3,'Clock selected: ',A,5X,'Trigger mode is ',A22,/,
     .         T3,'FIFO mode is ',A3,9X,'TGS  mode is ',A3,/,
     .         T3,'Telemetry mode is ',A17,/)

  19     FORMAT(T3,'FILTERING CHOICES:', /,
     .              T11,'Voltage step selection =  ', I2, ' to ' ,I2,/,
     .              T11,'Event type selection   =',A10,/,
     .              T11,'Range selection        =     ',A5,/,
     .              T11,'Range checking is ',A3,/,
     .		    T11,'Velocity range         = ',I4,' to ',I6,//,   
     .              T11,'Telescope checking is ',A3,//,
     .              T3,'Start Time:  ',I4,I2.2,I2.2,2X,I2.2,2(':',I2.2),5X,
     .              'End Time:  ',I4,I2.2,I2.2,2X,I2.2,2(':',I2.2))



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
C For Bad HDB skip entire Science Record
C
      IF(HDB.Qual .NE. 0) THEN
        BAD_HDB = BAD_HDB + 1
        WRITE(56,150) TIME_TO_USE.DOY, TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC
 150    FORMAT(1X,I4.3,3(':',I2.2),3X, 'Bad HDB found')
	GOTO 800
      ENDIF
C
C Output STICS PHA one science record.
C
      DO 800 J=0,59,INCREMENT		!0-59 spins
        TIME_TO_USE = SCI.EDB(J).W_TIME
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
	  IF (START_POS .EQ. 1) THEN
      	    X1_GSE = L1_SR_HEADER.GSE_POS.X/6380.0 
            Y1_GSE = L1_SR_HEADER.GSE_POS.Y/6380.0 
            Z1_GSE = L1_SR_HEADER.GSE_POS.Z/6380.0 
            WRITE(6,*)X1_GSE,Y1_GSE,Z1_GSE
	    START_POS = -1
          ENDIF

C
C Get MASS Speed and SWE Vxyz at start time and at the beginning of every Sci Record.
C 
	  IF((J .EQ. 0) .OR. (FIRSTSPEED .EQ. 0)) THEN
C            CALL WIND_L1_GET_MASS(L1_LUN,MASS_RATE,MASS_PHA,STATUS)   ! mass data
C            IF (STATUS .NE. 1) GOTO 9085
C
C      	    CALL SMS_MASS_SPEED(HDB, MASS_RATE,  	!input
C     .         BULK_SPEED, BULK_UNCER, ERR_SPD_FLAG , ERR_MESSAGE)	!output

C
C Convert day of year to month, day.
C
	    CALL CNVRT_YRDY_TO_YRMNDY(JZEXT(TIME_TO_USE.YEAR),JZEXT(TIME_TO_USE.DOY),MON,DAY)
C
C Get SWE data. JZEXT -- zero-extend makes the time variables i*4 for compatibility--- 
C
            CALL GET_SWE_KP(JZEXT(TIME_TO_USE.YEAR), MON, DAY, JZEXT(TIME_TO_USE.HR), 
     .			    JZEXT(TIME_TO_USE.MIN), JZEXT(TIME_TO_USE.SEC), 
     .			    GSE_POS_SWE, GSE_VEL, GSE_VEL_P,THERM_VEL, H_DENSITY, TIME_DIFF)

            IF((TIME_DIFF .LT. 140.0) .AND.	!allow 140 secs difference in SWE and input times
     .	       (GSE_VEL_P(1) .GE. 250) .AND. (GSE_VEL_P(1) .LE. 1100)) THEN
                SW_SPEED   = GSE_VEL_P(1)
                SW_SPEED_X = GSE_VEL(1)
                SW_SPEED_Y = GSE_VEL(2)
                SW_SPEED_Z = GSE_VEL(3)
            ELSE
	      BAD_SPEED = BAD_SPEED + 1
	      WRITE(56,151)TIME_TO_USE.DOY,TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
     .                     GSE_VEL_P(1)
 151	      FORMAT(1X,I4.3,3(':',I2.2),3X,'Invalid speed encountered => ',F12.1) 
	      SW_SPEED = 0
            ENDIF

	  ENDIF

C
C Get dpps enable information, voltage step, and default value of e/q. 
C
          ITEMP =HDB.BDH.DAT(24)
          ITEMP2=HDB.BDH.DAT(25)
	  DPPSPLUS  = IBITS(ITEMP,7,1)
          DPPSMINUS = IBITS(ITEMP2,0,1) 

          ITEMP = STICS_RATE.EDB(J).RATE(179)
          DVSTEP = IBITS(ITEMP,0,5)
C
C Check for valid voltage step
C
      	  ITEMP = HDB.BDH.DAT(25)
      	  DPPS_STEP_MODE = IBITS(ITEMP,6,2)			!Get stepping mode

	  CALL VOLTAGE_CHECK(DPPS_STEP_MODE, J, BITRATE, DVSTEP, DVS_CHECK)
	  IF (DVS_CHECK .EQ. -1) THEN
     	    BAD_DVS = BAD_DVS + 1
	    WRITE(56,153) TIME_TO_USE.DOY,TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
     .      		  J, DPPS_STEP_MODE, DVSTEP
 153	    FORMAT(1X,I4.3,3(':',I2.2),3X,'Spin # = ',I2,'  Step Mode = ',I2,'   DVS = ',I2)
	  ENDIF

C Check voltage step filter and solar wind speed filter

          IF ( (DVSTEP.GE.DATA_DVSTEP_MIN) .AND.
     .         (DVSTEP.LE.DATA_DVSTEP_MAX) .AND.
		
     .         ( (DATA_FILTER_MSPEED .LT. 0) .OR. 
     .           ((SW_SPEED.GE.DATA_MSPEED_MIN) .AND.
     .            (SW_SPEED.LE.DATA_MSPEED_MAX)) )   ) THEN

C
C Get info for determining speed statistics
C
	     IF((J .EQ. 0) .OR. (FIRSTSPEED .EQ. 0)) THEN
              IF ((SW_SPEED.GE.DATA_MSPEED_MIN).AND.(SW_SPEED.LE.DATA_MSPEED_MAX)) THEN
	       IF(SW_SPEED .LT. LOW_SPEED) LOW_SPEED = SW_SPEED
	       IF(SW_SPEED .GT. HIGH_SPEED) HIGH_SPEED = SW_SPEED
               SPEED_HITS = SPEED_HITS +1.0
               SUM_SW_SPEED = SUM_SW_SPEED + SW_SPEED
               SUM_SQ_SPEED = SUM_SQ_SPEED + SW_SPEED**2
	       SUM_SPEED_X = SUM_SPEED_X + SW_SPEED_X
	       SUM_SPEED_Y = SUM_SPEED_Y + SW_SPEED_Y
	       SUM_SPEED_Z = SUM_SPEED_Z + SW_SPEED_Z
	      ENDIF
	     ENDIF
C
C If HDB flag is bad, calculate e/q as if both DPPS plates are ON
C
	   IF(HDB.Qual .NE. 0) THEN
		DPPSMINUS = 1
		DPPSPLUS  = 1 
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
C Count number of spins for which each voltage step was used 
C
           IF(RATEFLAG .EQ. 'H')THEN
	       DVS_HITS(DVSTEP) = DVS_HITS(DVSTEP) + 1.0
	   ELSE
	       DVS_HITS(DVSTEP) = DVS_HITS(DVSTEP) + 2.0
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

C	   ***** get the PHA COUNT in each basic rate  ******
C Look through all the pha for this voltage step, and determine the count in 
C each basic rate region and sector

           DO I = 0, (SCI.EDB(J).STICS_Len-1) * 4, 4
              CALL DISSECT_STICS_PHA(STICS_PHA.EDB(J).PHA(I),
     .                      STOP_ID,START_ID_RANGE,ENERGY,SECTOR,
     .                      SSD_ID,TIME_OF_FLIGHT,PSTART,RANGE)

              BASIC_PHA(DVSTEP,SECTOR,RANGE) = BASIC_PHA(DVSTEP,SECTOR,RANGE) + 1.0
           END DO


C		***** get the Basic Rates  ******

	   DO 220 II=120,143			!these are the basic rates
	      STICS_RATES(II+1) = WIND_DECOMPRESS(STICS_RATE.EDB(J).RATE(II),CODE_C1)
 220       CONTINUE

C Get out basic rates for ranges R0, R1, R2. Each range has 8 Basic Rate bins, as cosecutive
C sectors are added together, e.g., bin1=sector0+1, bin2=sector2+3,...

	   DO 230 II=0,2	!range
             DO MM = 1,8   	!8 sectors
		BASIC_RATE(DVSTEP,MM,II) = BASIC_RATE(DVSTEP,MM,II) + STICS_RATES(120+II*8+MM)
             END DO	!over sectors
 230       CONTINUE	!end do over range

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

C check data quality on telescope consistency
              CALL TELESCOPE_ID (TIME_OF_FLIGHT,ENERGY,
     .            PSTART,STOP_ID,SSD_ID, TELE_IDENT, TELE_FLAG)
C
C Calculate Mass and M/Q using DPU algorithms
C
              CALL MMPQ_DPU_WSTICS(DPPSPLUS,DPPSMINUS,DVSTEP,
     .                           ENERGY,TIME_OF_FLIGHT,
     .                           DPU_MASS_OVER_Q,DPU_MASS,
     .                           DPU_NM,DPU_NQ,DPU_RANGE)
	      IF (FIFO .EQ. 'ON ') DPU_RANGE = 2
		
              CALL USER_NMNQ_PHA_BINNED(DPPSPLUS,DPPSMINUS,DVSTEP,  !pha event information
     .                    ENERGY,TIME_OF_FLIGHT,
     .                    NQ_MAX,MQMAX,MQMIN,    !information for binning
     .                    NM_MAX,MMAX, MMIN,
     .                    ELOW,EHIGH,TLOW,THIGH,      !return values
     .                    RKM,RKQ,
     .                    AM,BM,C1_EQ,C2A_EQ,C2B_EQ,D1_EQ,D2_EQ,EMQ,
     .                    BK_C2, BK_MQ,
     .                    PHA_NQ,PHA_NM)			!return information

              	                  
	      IF ( (DATA_FILTER_RANGE.EQ.0)  .OR.
     .             (RANGE .EQ. 0 .AND. SEL_RANGE(0))  .OR.
     .             (RANGE .EQ. 1 .AND. SEL_RANGE(1))  .OR.
     .             (RANGE .EQ. 2 .AND. SEL_RANGE(2)) ) THEN

	           IF ( (DATA_FILTER_COINC.EQ.0)  .OR. 
     .                (DATA_FILTER_COINC.EQ.1 .AND. DPU_MASS.EQ.0.)  .OR.
     .                (DATA_FILTER_COINC.EQ.2 .AND. DPU_MASS.GT.0.) ) THEN

	                 IF ( 
     .                      (  DATA_FILTER_RANGECHECK.EQ.0 .OR.
     .                      (DATA_FILTER_RANGECHECK.EQ.1 .AND.	! range OK
     .                            RANGE.EQ.DPU_RANGE)  )
     .                      .AND. (TELE_FLAG.EQ.0)  ) THEN ! telescope OK
C
C Increment NQNM arrays
C
	                      IF (PHA_NQ.GE.0  .AND.  PHA_NM.GE.0) THEN
	                         CALL ION_IDENTIFIER( USER_NM_MID(PHA_NM),USER_NQ_MID(PHA_NQ), 
     .				        SPECIES_MASS,SPECIES_CHARGE,ION_INDEX)
				 IF(ION_INDEX.EQ.ION_CHOICE)
     .		                   RAW_COUNTS(DVSTEP,SECTOR,RANGE,TELE_IDENT) = 
     .					RAW_COUNTS(DVSTEP,SECTOR,RANGE,TELE_IDENT) + 1.0
                              ENDIF
                         ENDIF     !check range and/or telescope conditions
                   ENDIF	!data_filtering by coincidence conditions
              ENDIF     !data filtering by range

 300       CONTINUE		!end pha for this given voltage step

          ENDIF   ! data within user selected voltage step range

          FIRSTSPEED = -1		!Change after giving 1st speed

        ENDIF     ! data within user selected time (startfound)

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

      DO JJ = 1,IFILE
        WRITE(55,16) INPUTFILES(JJ)
      END DO                                     
C
C Calculate standard deviation of speed
C
      IF (SPEED_HITS .GT. 1) THEN
        SPEED_MEAN  = SUM_SW_SPEED/SPEED_HITS
        SPEED_SIGMA = SQRT((SUM_SQ_SPEED - SPEED_HITS*SPEED_MEAN**2)/(SPEED_HITS-1.0))
        AVG_SPEED_X = SUM_SPEED_X/SPEED_HITS
        AVG_SPEED_Y = SUM_SPEED_Y/SPEED_HITS
        AVG_SPEED_Z = SUM_SPEED_Z/SPEED_HITS
      ELSE
	SPEED_MEAN = 0
	SPEED_SIGMA = 0
        AVG_SPEED_X = 0
        AVG_SPEED_Y = 0
        AVG_SPEED_Z = 0
      ENDIF
      WRITE(55,940)SPEED_MEAN, SPEED_SIGMA, LOW_SPEED, HIGH_SPEED
 940  FORMAT(/,1X,' Average Solar Wind Speed with sigma (km/s) = ',F6.1,' +/- ',F6.1,
     .       /,1X,' Range of valid speeds encountered (km/s) = ',F6.1,' to ', F6.1)  

      WRITE(55,941) AVG_SPEED_X, AVG_SPEED_Y, AVG_SPEED_Z
 941  FORMAT(1X,' Average GSE Vx, Vy, Vz (km/s): ', 3F8.1)

C
C Write beginning and end positions to output files
C
      WRITE(55,945)X1_GSE,Y1_GSE,Z1_GSE,X2_GSE,Y2_GSE,Z2_GSE  
 945  FORMAT(1X,' Start postion in GSE cartesian coordinates(Re): ',3F9.3,/
     .       1X,' Stop  postion in GSE cartesian coordinates(Re): ',3F9.3//)
                 
      WRITE(55,946)
 946  FORMAT(1X,' First three datasets are flux values in units of #/(cm2-sr-sec-keV/amu) or '/,
     .       1X,' #/(cm2-sec-keV/amu) for telescopes 1, 2 and 3, respectively. '/,
     .       1X,' The next three are the raw counts for telescopes 1, 2 and 3, respectively.'//)

      WRITE(55,950)(I,I=0,15)		!Write labels to header
 950  FORMAT(1X,T40,'SECTORS',/,1X,'   DVS     E/Nuc   ',16(5X,I2,5X))

C
C Calculate Basic Rate normalization factors. Divide Basic Rate bin by the sum of the 
C corresponding two Basic_PHA sectors
C
      DO I = DATA_DVSTEP_MIN,DATA_DVSTEP_MAX
	DO K = 0,2
	  DO J = 0,15,2
	    IF(BASIC_PHA(I,J,K).GE.1.0 .OR. BASIC_PHA(I,J+1,K).GE.1.0)THEN
	       BASIC_W(I,J,K) = BASIC_RATE(I,(J+2)/2,K)/(BASIC_PHA(I,J,K) + BASIC_PHA(I,J+1,K)) 
	       IF(BASIC_W(I,J,K) .LT. 1.0) THEN		!for bad Basic Rate 
C     .		 ( (BASIC_RATE(I,(J+2)/2,K) - (BASIC_PHA(I,J,K) + BASIC_PHA(I,J+1,K))) .GT. 50000.0)) THEN
		 BASIC_W(I,J,K) = 1.0
		 BAD_BR = BAD_BR + 1
		 WRITE(56,250)TIME_TO_USE.DOY,TIME_TO_USE.HR,TIME_TO_USE.MIN,TIME_TO_USE.SEC,
     .			      J,J+1,BASIC_RATE(I,(J+2)/2,K),(BASIC_PHA(I,J,K) + BASIC_PHA(I,J+1,K))
 250		 FORMAT(1X,I4.3,3(':',I2.2),3X,'Sectors',I2,'+',I2,'  Basic Rate = ',F7.0,3X,
     .                       'Basic PHA = ', F7.0)
	       ELSE
		 GOOD_BR = GOOD_BR + 1
	       ENDIF
	    ELSE
	       BASIC_W(I,J,K) = 1.0
	    ENDIF
            BASIC_W(I,J+1,K) = BASIC_W(I,J,K)
	  END DO
       	END DO
      END DO
C
C Write error information to screen and error file

C      WRITE(6,*)'BAD HDBs = ', BAD_HDB
C      WRITE(6,*)'BAD DVS  = ', BAD_DVS
C      WRITE(6,*)'BAD BRs = ', BAD_BR, '   GOOD BRs = ', GOOD_BR
C      WRITE(6,*)'BAD SPEEDS = ',BAD_SPEED
      WRITE(6,920) BAD_HDB, BAD_DVS, BAD_SPEED, SPEED_HITS, BAD_BR,GOOD_BR
      WRITE(56,920) BAD_HDB, BAD_DVS, BAD_SPEED, SPEED_HITS, BAD_BR,GOOD_BR
 920  FORMAT(//,1X,'Total Bad HDBs = ',I2,/,1X,'Total Invalid DVS = ',I3,/,
     .          1X,'Total Bad Speeds = ',I3,5X,'Good Speeds = ',F12.0,/,
     .		1X,'Total Bad Basic Rates = ',I4,
     .          5X,'Total Good Basic Rates = ', I10)


      CALL OMR_EFF(EFF_CORR)		!Get efficiency correction factors
C
C Do Basic Rate normalization and efficiency correction 
C
      DO I = DATA_DVSTEP_MIN, DATA_DVSTEP_MAX
        IF(DVS_HITS(I).EQ.0)DVS_HITS(I)=1.0
	DO J = 0,15
	  DO K = 0,2
	    DO L = 1,3
              CORR_DATA(I,J,K,L) = RAW_COUNTS(I,J,K,L) * BASIC_W(I,J,K) * DEADTIME_CORR(J)
     .				   /(EFF_CORR(I,ION_CHOICE) * DVS_HITS(I) * GEO_FAC * DELTA_T)
	    END DO
          END DO
        END DO
      END DO	

C			****************************
C			*       SECTORED ARRAYS    *
C			****************************
C Sum over Basic Rates
C
      DO I = DATA_DVSTEP_MIN, DATA_DVSTEP_MAX
	DO L = 1,3
	  DO J = 0, 15
	    DO K = 0,2
	       SECT_DATA_RAW(I,J,L) = SECT_DATA_RAW(I,J,L) + RAW_COUNTS(I,J,K,L)
	       SECT_DATA_COR(I,J,L) = SECT_DATA_COR(I,J,L) + CORR_DATA(I,J,K,L)
	    END DO
	  END DO
     	END DO
      END DO

C
C Write data to output file
C 
      DO K = 1, 3
        DO I=DATA_DVSTEP_MIN,DATA_DVSTEP_MAX
	  WRITE(55,955)I,(DPU_EPQ(I)*CHARGE_ION(ION_CHOICE)/MASS_ION(ION_CHOICE)),
     .                 (SECT_DATA_COR(I,J,K),J=0,15)
 955      FORMAT(1X,I6,2X,F8.2,2X,16(1X,1PE11.4))
        END DO
        WRITE(55,*)
	WRITE(55,*)
      END DO

      DO K = 1, 3
        DO I=DATA_DVSTEP_MIN,DATA_DVSTEP_MAX
	  WRITE(55,956)I,(DPU_EPQ(I)*CHARGE_ION(ION_CHOICE)/MASS_ION(ION_CHOICE)),
     .                 (SECT_DATA_RAW(I,J,K),J=0,15)
 956      FORMAT(1X,I6,2X,F8.2,2X,16(1X,F11.1))
        END DO
        WRITE(55,*)
	WRITE(55,*)
      END DO


C     ***********************************************************
C     *								*
C     *			CLOSE FILES				*
C     ***********************************************************

      CLOSE(UNIT=55)				! Close flux file
      CLOSE(UNIT=56)                            ! Close raw counts file

      TYPE *,' FILE WRITTEN TO  CURRENT DIRECTORY'

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
C9085  TYPE *,'ERROR GETTING MASS DATA; STATUS=',STATUS
C      STOP

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
     .       '    -1 = no filtering by SWE speed ',/,
     .       '    0  = SWE speed filtering desired',/,
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


C***************************************************************************************
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


c***************************************************************************************************
      SUBROUTINE OMR_EFF(EFF_DATA)
c***************************************************************************************************
C This routine calculates the efficiency for different ions at each voltage step. These efficiencies 
C are for triple coincidence only, i.e. both mass/charge and non-zero mass obtained. 
c
c Created 30-MAR-1995 	by	K. CHOTOO from the one in WSTICS_RATES as template
c
C Subroutines called:	WSTICSEFF
c Revision History:
C	24-OCT-1996	K.CHOTOO	!Mass of ion included in efficiency correction
C
C***************************************************************************************************
			IMPLICIT NONE
      REAL*4 DCR_FSR,			!DCR/FSR
     .       deltaE,			!Energy bandwidth (=0.019E)
     .       E,				!Energy
     .       EFF_DATA(0:31,1:30),      	!Matrix rate efficiency TCR/Io
     .       EPQ(0:31),            	!Energy/charge for each voltage step
     .       DCR_RSR,			!DCR/RSR
     .	     ION_CHARGE(1:30),		!Charge of ions
     .       ION_MASS(1:30),		!Mass of ions
     .       M,				!Mass
     .       MR_ENERGY(0:31,1:30),	!Total energy at each voltage step for rates
     .       TCR_FSR			!TCR/FSR             

      INTEGER I,J			!Counters

      DATA ION_MASS/1.000,3.971,3.971,11.916,11.916,11.916,15.872,15.872,15.872,20.019,24.112,24.112,
     .              27.863,27.863,27.863,55.404,55.404,55.404,55.404,55.404,55.404,55.404,
     .              0,0,0,0,0,0,0,0/

      DATA ION_CHARGE/1.0, 2.0, 1.0, 6.0, 5.0, 4.0, 7.0, 6.0, 1.0, 8.0, 10.0, 8.0, 12.0,
     .                9.0, 8.0, 16.0, 14.0, 12.0, 11.0, 10.0, 9.0, 8.0, 0,0,0,0,0,0,0,0/


C		************ STATEMENTS *************

      DO 10  I = 0,31
        EPQ(I) = 6.190722 * (1.1225857**I)
  10  CONTINUE

      DO 15 J=1,30			!Initialize efficiency array to 1.0
        DO 14 I=0,31
          EFF_DATA(I,J) = 1.0
  14    CONTINUE
  15  CONTINUE

      DO 20 I = 0,31
       DO 18 J = 1,22
         MR_ENERGY(I,J)  = EPQ(I) * ION_CHARGE(J)	
  18   CONTINUE
  20  CONTINUE 

      DO 40 J=1,22
        DO 30 I=0,31
          M = ION_MASS(J)
          E = MR_ENERGY(I,J)
          deltaE = 0.019*E				!Energy bandwidth
          CALL WSTICSEFF(E,M,DCR_RSR,TCR_FSR,DCR_FSR)
          IF(J .LE. 3) THEN
	    IF((DCR_FSR * DCR_RSR).GT. 0) THEN		!Use DCR/Io for H+, He+2, and He+1
              EFF_DATA(I,J) = DCR_FSR * DCR_RSR * deltaE / M
            ELSE
              EFF_DATA(I,J) = 1.0
            ENDIF
	  ELSE	    
	    IF((TCR_FSR * DCR_RSR).GT. 0) THEN		!Use TCR/Io for other ions
              EFF_DATA(I,J) = TCR_FSR * DCR_RSR * deltaE / M	
            ELSE
              EFF_DATA(I,J) = 1.0
	    ENDIF
	  ENDIF
  30    CONTINUE
  40  CONTINUE

      RETURN
      END
