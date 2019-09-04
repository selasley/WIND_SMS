CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      PROGRAM MASS_SWE_SPD
C
C Revision history:
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CDEC$ IDENT '1.14'
      INCLUDE	'INC_DIR:EDBDEF.INC/NOLIST'
      INCLUDE   'INC_DIR:L1FMT.INC/NOLIST'
      INCLUDE   '($RMSDEF)'
      INTEGER*4 STATUS,L1_LUN/19/,STICS_RATES(179),D_RATES(17)
      INTEGER*4 WIND_DECOMPRESS,VSTEP,MONTH,DAYOFMONTH
      REAL*4 D1/6.190722/,D2/1.1225857/      ! Default values used for E/Q calculation
      REAL*4 E_OVER_Q
      CHARACTER L1FILENAME*80, HMRNAME*16, SMRNAME*19, BRNAME*9
      CHARACTER FULL_L1_NAME*80, FIFO*3, TGS*3, RATEFLAG*1
      CHARACTER*17 MODE(10) /'Science mode     ','Engineering mode ',
     &                       'Maneuver mode    ','Contingency mode ',
     &                       'Science mode     ','Engineering mode ',
     &                       'Maneuver mode    ','Contingency mode ',
     &                       'Transitional mode','Unknown mode     '/,
     &             MODE_AT_START
      LOGICAL*1 BITRATE
      LOGICAL STARTFOUND,ENDFOUND,USE_SC_CLOCK,COMPARE_TIME_GE,GOODSPIN
      INTEGER I,J,K,L,M,N,NN,P,CNTR,ITEMP,ITEMP2,NAMELEN,YR2DIGIT,INCREMENT,SUMBR(0:2),SUMHMR,SUMSMR
      LOGICAL CODE_C,CODE_C2,COMP_CODE  !Compression codes 
C
C MASS parameters
C
      REAL*4  BULK_SPEED,		!Solar wind speed
     .	      BULK_UNCER,		!Uncertainty in solar wind speed
     .	      MASS_HITS,		!Number of valid MASS hits
     .	      MASS_SPEED_AVG,		!Average MASS speed  
     .	      MASS_SPEED_SUM,		!Sum of MASS speeds
     .	      MASS_THERM_AVG,		!Average MASS thermal velocity
     .	      MASS_THERM_SUM		!Sum of MASS thermal velocities
C
C SWE parameters
C
      REAL*4   GSE_POS_SWE(3),		!Spaceraft position in GSE coordinates.
     .	       GSE_VEL(3),		!H+ solar wind speed in cartesian coord.
     .	       GSE_VEL_P(3),		!"   "     "     "    " spherical   "
     .	       H_DENSITY,		!H+ density                   
     .	       SWE_HITS,		!Number of valid SWE hits
     .	       SWE_SPEED_AVG,		!Average SWE speed  
     .	       SWE_SPEED_SUM,		!Sum of SWE speeds
     .	       SWE_THERM_AVG,		!Average SWE thermal velocity
     .	       SWE_THERM_SUM,		!Sum of SWE thermal velocities
     .	       THERM_VEL,		!Thermal velocity
     .	       TIME_DIFF		!Time difference in seconds between SWE time 
C					 and time sent to SWE subroutine.

C S/C position
      REAL*4 X1_GSE,Y1_GSE,Z1_GSE,     	!Start position
     .       X2_GSE,Y2_GSE,Z2_GSE,	!Stop position
     .       Re/6371.0/			!1 Re = 6371 km            

      INTEGER*4 ERR_SPD_FLAG,		!Set to 1 for bad speed
     .		FIRSTSPEED/0/,		!Set to -1 after getting speed at start time
     .          IFILE,			!Number of L1 files used
     .          INTERVAL,               !Number of Science Records for interval
     .          OFILE,			!Output file unit number
     .          OPENMODE,		!Variable for opening next file
     .          Q,			!Increments for each new interval or data gap
     .          RATENUM,		!Number of rates selected
     .          SR_INTERVAL		!Boundary for Science Record interval

C     naming the output file
      LOGICAL BRACK                     !A variable which is .TRUE. if there is
C                                       a set of square brackets in the input file name.
C                                       If there is, they, and everything between them,
C                                       will be stripped out so that the output file is
C                                       created in the user's account and not on the
C                                       account from which we read the data.
      INTEGER*4 NINDEX,                 !used in rearranging the input file name to
C                                       create the output file name.
     .          JSAVE                   !used in rearranging the input file name to
C                                       create the output file name.
      CHARACTER*90 LISFIL               !holds the name of the input file
      CHARACTER INPUTFILES(1:100)*90, 	!L1 filename array for multiple inputs   
     .          OUTFILENAME*90
      CHARACTER USERNAME*40,		!User defined output filename
     .		ERR_MESSAGE*80		!Error message for bad speed

      CHARACTER RUN_TIME*8, RUN_DATE*9	!run time and date
 
      LOGICAL FIRSTFILE,		!Set to true if this is the 1st input file
     .	      FIRST_TIME		!Set to true for 1st time interval
C
      RECORD /CORE/ SCI
      RECORD /HDBstr/ HDB
      RECORD /ST_RATE/ STICS_RATE
      RECORD /ST_PHA/ STICS_PHA
      RECORD /T/ START, END, TIME_TO_USE, INT_START_TIME

      RECORD /MA_RATE/ MASS_RATE   ! receives data on rates from MASS sensor
      RECORD /MA_PHA/ MASS_PHA     ! receives data on tof's from the mass sensor

C
C Get input L1 filename
C
      L1FILENAME = 'DATA_DIR:WI*L1*.DAT'
      CALL WIND_GET_FILENAME(L1FILENAME, STATUS)
      IF(STATUS.NE.1)STOP

      FIRSTFILE = .TRUE.
      FIRST_TIME = .TRUE.
C
C Prompt user for start and stop times
C
      TYPE 10
  10  FORMAT(' Enter Start time( -1/ means whole file) - Year,Month,Day,Hour,Min,Sec: '$) 
      ACCEPT*, START.YEAR,MONTH,DAYOFMONTH,START.HR,START.MIN,START.SEC
      IF(START.YEAR.GE.0)THEN
C
C Convert month and day to day of year.
C
        CALL CNVRT_T_YRMONDAY_TO_YEARDAY( START, MONTH, DAYOFMONTH)
        IF(START.YEAR.LT.100)START.YEAR=START.YEAR+1900        ! Allow for 2-digit years
        IF(START.YEAR.LT.1980)START.YEAR=START.YEAR+100        ! Allow for years beyond 1999
        TYPE 12
  12    FORMAT(' Enter End time - Year,Month,Day,Hour,Min,Sec: '$) 
        ACCEPT*, END.YEAR,MONTH,DAYOFMONTH,END.HR,END.MIN,END.SEC
C
C Convert month and day to day of year.
C
        CALL CNVRT_T_YRMONDAY_TO_YEARDAY( END, MONTH, DAYOFMONTH)
        IF(END.YEAR.LT.100)END.YEAR=END.YEAR+1900        ! Allow for 2-digit years
        IF(END.YEAR.LT.1980)END.YEAR=END.YEAR+100        ! Allow for years beyond 1999
      ELSE
        END.YEAR=3000
      ENDIF
C
C Get Science Record Interval to average rates over
C
      WRITE(6,*) 'ENTER SCIENCE RECORD INTERVAL'
      READ(5,*) INTERVAL
C
C Prompt user for output filename
C
      WRITE(6,*)'Enter name for output file or <RETURN> for default name'
      READ(5,'(A)') USERNAME

C   **********************************************************
C   *                                                        *
C   *                   CREATE OUTPUT FILE NAME              *
C   *                                                        *
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
            LISFIL(JSAVE:JSAVE) = L1FILENAME(I:I)
            JSAVE = JSAVE + 1
         ENDIF
   3  CONTINUE
      LISFIL(1:9) = ' MASS_SWE'
      LISFIL(JSAVE+1:JSAVE+7) = 'SPD.TBL'        

      IF(USERNAME(1:12) .NE. '            ')THEN
        OUTFILENAME = USERNAME
      ELSE
        OUTFILENAME = LISFIL				! default name
      ENDIF

C**********************************************************************

C======================================================================
C		OPEN L1 INPUT FILE AND READ DATA
C======================================================================
C
C
C Open L1 file for input, and read first SR, Core, HDB, and STICS RATES
C
 1000 CONTINUE
      IF (FIRSTFILE .EQ. .FALSE.) THEN
        IFILE = IFILE + 1
	OPENMODE = 2
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
      IF(STATUS.EQ.RMS$_EOF)GO TO 1990
      IF (STATUS .NE. 1) GOTO 9020
      CALL WIND_L1_GET_BASIC( L1_LUN, SCI, STATUS)
      IF (STATUS .NE. 1) GOTO 9030
      CALL WIND_L1_GET_HDB( L1_LUN, HDB, STATUS)
      IF (STATUS .NE. 1) GOTO 9050

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
        IF(N.GE.60)GOTO 13
      ELSE
        N=N-1
      ENDIF
C
c Get S/C position at beginning of time period
c
      IF(IFILE .EQ. 1) THEN
         X1_GSE = L1_SR_HEADER.GSE_POS.X/Re
         Y1_GSE = L1_SR_HEADER.GSE_POS.Y/Re
         Z1_GSE = L1_SR_HEADER.GSE_POS.Z/Re 
         WRITE(6,*)X1_GSE,Y1_GSE,Z1_GSE
      ENDIF


C    ******************************************************************************
C    *										  *
C    *                OPEN OUTPUT FILE AND WRITE HEADER INFORMATION               *
C    *										  *
C    ******************************************************************************
      IF(IFILE .EQ. 1) THEN
        OPEN(UNIT= 20, NAME=OUTFILENAME, DEFAULTFILE='TABLE_DIR:', FORM='FORMATTED', 
     &     TYPE='NEW', RECL=200)

	CALL TIME(RUN_TIME)			!Get run time
	CALL DATE(RUN_DATE)			!Get run date

        WRITE(20,30)
  30    FORMAT(' MASS and SWE Solar Wind Speeds')
	WRITE(20,'(A)') ' Run time  '//RUN_TIME//'   '//RUN_DATE

        WRITE(20,35) FULL_L1_NAME(1:NAMELEN)
  35    FORMAT(T10,'Input Level 1 Filename: ',A)
	WRITE(20,*)
	WRITE(20,37)
  37	FORMAT(1X,'YEAR DOY:HR:MN:SC',2X,'MASS_SPEED MASS_TH_VEL MASS_HITS', 
     .            2X,'SWE_SPEED SWE_TH_VEL SWE_HITS')

      ENDIF
C
C Set flags for start and stop
C
      STARTFOUND=.FALSE.
      ENDFOUND=.FALSE.

 50   MASS_HITS = 0
      MASS_SPEED_SUM = 0
      MASS_THERM_SUM = 0
      SWE_HITS = 0
      SWE_SPEED_SUM = 0
      SWE_THERM_SUM = 0
C
C Set Sci Rec boundary
C
      SR_INTERVAL = L1_SR_HEADER.SR_COUNT + INTERVAL
C
C Get the Basic Core data
C
 100  CALL WIND_L1_GET_BASIC( L1_LUN, SCI, STATUS)
      IF (STATUS .NE. 1) GOTO 9030
C
C Get Housekeeping Data Block
C
      CALL WIND_L1_GET_HDB( L1_LUN, HDB, STATUS)
      IF (STATUS .NE. 1) GOTO 9050
C
C If HDB flag is bad, skip entire science record
C
      IF(HDB.Qual.NE.0)GOTO 905

C
C Decompress STICS rates for one science record and output
C
      TIME_TO_USE = SCI.EDB(0).W_TIME
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
          IF(ENDFOUND)GO TO 907
C
C Get start time for first interval
C
	  IF (FIRST_TIME) THEN
	    INT_START_TIME = TIME_TO_USE
	    FIRST_TIME = .FALSE.
      	    SR_INTERVAL = L1_SR_HEADER.SR_COUNT + INTERVAL
	  ENDIF
C
C Get MASS Speed at start time and at the beginning of every Sci Record.
C 
          CALL WIND_L1_GET_MASS(L1_LUN,MASS_RATE,MASS_PHA,STATUS)   ! mass data
          IF (STATUS .NE. 1) GOTO 9085

      	  CALL SMS_MASS_SPEED(HDB, MASS_RATE,  	!input
     .         BULK_SPEED, BULK_UNCER, ERR_SPD_FLAG , ERR_MESSAGE)	!output

          IF(ERR_SPD_FLAG .EQ. 0) THEN
	    IF((BULK_UNCER .GT. 0) .AND. (BULK_UNCER .LT. 150.0)) THEN
	        MASS_HITS = MASS_HITS + 1.0
	        MASS_SPEED_SUM  = MASS_SPEED_SUM + BULK_SPEED
		MASS_THERM_SUM  = MASS_THERM_SUM + BULK_UNCER
	    ENDIF
          ELSE
	      WRITE(6,*) ERR_MESSAGE
          ENDIF
C
C Convert day of year to month, day.
C
	  CALL CNVRT_YRDY_TO_YRMNDY(JZEXT(TIME_TO_USE.YEAR),JZEXT(TIME_TO_USE.DOY),MON,DAY)
C
C Get SWE data. JZEXT -- zero-extend makes the time variables i*4 for compatibility--- 
C
          CALL GET_SWE_KP(JZEXT(TIME_TO_USE.YEAR), MON, DAY, JZEXT(TIME_TO_USE.HR), 
     .			  JZEXT(TIME_TO_USE.MIN), JZEXT(TIME_TO_USE.SEC), 
     .			  GSE_POS_SWE, GSE_VEL, GSE_VEL_P,THERM_VEL, H_DENSITY, TIME_DIFF)
          IF((TIME_DIFF .LT. 45.0) .AND.		!allow 45 secs difference in SWE time
     .	       (GSE_VEL_P(1) .GE. 200) .AND. (GSE_VEL_P(1) .LE. 1000)) THEN
   
               SWE_HITS = SWE_HITS +1.0
               SWE_SPEED_SUM = SWE_SPEED_SUM + GSE_VEL_P(1)
	       SWE_THERM_SUM = SWE_THERM_SUM + THERM_VEL
	  ENDIF  

 
        ENDIF !{startfound}

C
C Read next Science Record
C
 905  CALL WIND_L1_READ_SR( L1_LUN, CNTR, L1_SR_HEADER, STATUS )


      IF (.NOT. STARTFOUND) GO TO 100

      IF((ABS(L1_SR_HEADER.SR_COUNT - SR_INTERVAL) .GT. 500) .AND. (STATUS .NE. RMS$_EOF))
     +     GOTO 905   !In case of bad Sci Rec Num

C
C Average rates over interval. Average rates if END_TIME reached or at EOF.
C
 907  IF((L1_SR_HEADER.SR_COUNT .GE. SR_INTERVAL) .OR.
     +   (ENDFOUND) .OR. (STATUS .EQ. RMS$_EOF))THEN

C
C Find average speed
C
         IF(MASS_HITS.GT.0) THEN
	   MASS_SPEED_AVG = MASS_SPEED_SUM/MASS_HITS
	   MASS_THERM_AVG = MASS_THERM_SUM/MASS_HITS
	 ELSE 
	   MASS_SPEED_AVG = 0
	   MASS_THERM_AVG = 0
	 ENDIF

         IF(SWE_HITS.GT.0) THEN
	   SWE_SPEED_AVG = SWE_SPEED_SUM/SWE_HITS
	   SWE_THERM_AVG = SWE_THERM_SUM/SWE_HITS
	 ELSE 
	   SWE_SPEED_AVG = 0
	   SWE_THERM_AVG = 0
	 ENDIF

C
C Write rates and interval end time to output file
C
         WRITE(20,940)INT_START_TIME.YEAR,INT_START_TIME.DOY,INT_START_TIME.HR,
     +                INT_START_TIME.MIN,INT_START_TIME.SEC,MASS_SPEED_AVG,
     +                MASS_THERM_AVG,MASS_HITS,SWE_SPEED_AVG,SWE_THERM_AVG,SWE_HITS
 940     FORMAT(1X,I4,1X,I3,3(':',I2.2),2(4X,F6.1),5X,F6.0,2X,2(4X,F6.1),5X,F4.0)

	 INT_START_TIME = TIME_TO_USE 

         IF(ENDFOUND) GO TO 2000
         IF(STATUS.EQ.RMS$_EOF)GO TO 1990
         GO TO 50
      ENDIF !{L1_SR_HEADER.SR_COUNT...}

      IF (STATUS .NE. 1) GOTO 9020
         
      GO TO 100
C
C Close files and STOP
C
1990  IF(START.YEAR .GE. 0)GO TO 1000
2000  CALL WIND_L1_CLOSE( L1_LUN, STATUS )      ! Close input file
      IF (STATUS .NE. 1) GOTO 9090
c
c Get S/C position at end of time period
c
      X2_GSE = L1_SR_HEADER.GSE_POS.X/Re
      Y2_GSE = L1_SR_HEADER.GSE_POS.Y/Re 
      Z2_GSE = L1_SR_HEADER.GSE_POS.Z/Re 
      WRITE(6,*)X2_GSE,Y2_GSE,Z2_GSE

      CLOSE(UNIT=20)                            ! Close output file
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
9085  TYPE *,'ERROR GETTING MASS DATA; STATUS=',STATUS
      STOP
9090  TYPE *,'ERROR CLOSING L1 FILE; STATUS=',STATUS
      STOP
      END


