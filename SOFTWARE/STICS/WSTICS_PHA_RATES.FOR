CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      PROGRAM WSTICS_PHA_RATES
C 
C
C  CHOOSE_STICS_RATES Created from above  13-MAR-1995      K. CHOTOO
C
C  This version:	1.05	22-AUG-1995
C Revision history:
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
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
      LOGICAL CODE_C,CODE_C2		!Compression codes 
      REAL*4  AVE_SPEED,		!Average solar wind speed
     .        AVERAGE_RATES(0:31,1:4),	!Array for averaging rates per voltage step
     .        BR0_NORM,			!Basic rate normalization factor
     .	      BR0_NORM_FLAG,		!Increments if PHA_BR0 > MR_BR0
     .        BR0_NORM_GOOD,		!Increments if PHA_BRO < MR_BR0
     .        DVS_HITS(0:31),		!Counts # of hits for each voltage step
     .        DVS_STEPS,		!Counts number of voltage steps used
     .        H_SPEED,			!Proton speed
     .        H_UNCER,			!Uncertainty in proton speed from width
     .        HE2_SPEED,		!Alpha speed
     .	      HE2_UNCER,		!Uncertainty in alpha speed  from width
     .        MR_EFF(0:31,1:178),	!Efficiencies for rates at each voltage step
     .        RATE_DATA(4),    		!Stores average rates per spin averaged over all voltage step
     .        SPEED_HITS,		!Number of valid speed values in interval
     .        SUM_HE2_SPEED,		!Sum of alpha speeds over interval
     .        SUM_RATES(0:31,1:4),	!Sums rates over Science Record interval
     .        STX_RATE(4)             	!Stores decompressed iron rates            

      INTEGER*4 DPPSPLUS,		!STICS +DPPS on=1, off=0
     .          DPPSMINUS,		!STICS -DPPS on=1, off=0
     .          EFF_REPLY,              !User response for raw counts or corrected counts
     .          IFILE,			!Number of L1 files used
     .          INTERVAL,               !Number of Science Records for interval
     .          MR_BR0,			!Basic rate 0 from STICS_RATE
     .          OPENMODE,		!Variable for opening next file
     .          PHA_BR0,		!Basic Rate 0 from PHA
     .          PHA_FE09,PHA_FE10,	!Fe9 and Fe10 rates created from PHA
     .          Q,			!Increments for each new interval or data gap
     .          SR_INTERVAL		!Boundary for Science Record interval


C    pha word - dissected:
      INTEGER*4 START_ID_RANGE,
     .          STOP_ID,
     .          ENERGY,
     .          SECTOR,
     .          SSD_ID,
     .          TIME_OF_FLIGHT,
     .          PSTART,
     .          RANGE

C    dpu algorithm values for e/q, mass and m/q:
      REAL*4 DPU_E_OVER_Q,
     .       DPU_MASS,
     .       DPU_MASS_OVER_Q

      INTEGER*4 DPU_NM,DPU_NQ,		!dpu calculations for m, m/q bins
     .          DPU_RANGE		!dpu calculations for range

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
 
      LOGICAL FIRSTFILE			!Set to true if this is the 1st input file
C
      RECORD /CORE/ SCI
      RECORD /HDBstr/ HDB
      RECORD /ST_RATE/ STICS_RATE
      RECORD /ST_PHA/ STICS_PHA
      RECORD /T/ START, END, TIME_TO_USE, TIME_LAST

      RECORD /MA_RATE/ MASS_RATE   ! receives data on rates from MASS sensor
      RECORD /MA_PHA/ MASS_PHA     ! receives data on tof's from the mass sensor

C
C Get input L1 filename
C
      L1FILENAME = 'DATA_DIR:WI*L1*.DAT'
      CALL WIND_GET_FILENAME(L1FILENAME, STATUS)
      IF(STATUS.NE.1)STOP

      FIRSTFILE = .TRUE.
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
C Find out whether user wants raw counts or corrected counts
C
      WRITE(6,*)' FOR RAW COUNTS, ENTER 1'
      WRITE(6,*)' FOR COUNTS CORRECTED WITH EFFICIENCIS, ENTER 2'
      READ(5,*)EFF_REPLY
C
C Get Science Record Interval to average rates over
C
      WRITE(6,*) 'ENTER SCIENCE RECORD INTERVAL'
      READ(5,*) INTERVAL
C
C Get matrix rate efficiencies
C
      CALL OMR_EFF(MR_EFF)		!get efficiencies         


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
      LISFIL(1:9) = 'WSTX_RATE'
      IF(EFF_REPLY .EQ. 2) THEN
         LISFIL(JSAVE+1:JSAVE+7) = 'PHA.EFF'        
      ELSE
         LISFIL(JSAVE+1:JSAVE+7) = 'PHA.RAW'
      ENDIF
      OUTFILENAME = LISFIL
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
        IF(N.GE.60)GOTO 13
      ELSE
        N=N-1
      ENDIF
C
C Check FIFO mode, telemetry mode, and TGS mode
C
      ITEMP = HDB.BDH.DAT(221)
      IF(IBITS(ITEMP,7,1).EQ.1)THEN
        FIFO='ON '
      ELSE
        FIFO='OFF'
      ENDIF
C
      ITEMP = SCI.EDB(N).TLM_Mode
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
C    ******************************************************************************
C    *										  *
C    *                OPEN OUTPUT FILE AND WRITE HEADER INFORMATION               *
C    *										  *
C    ******************************************************************************
      IF(IFILE .EQ. 1) THEN
        OPEN(UNIT= 20, NAME=OUTFILENAME, DEFAULTFILE='TABLE_DIR:', FORM='FORMATTED', 
     &     TYPE='NEW', RECL=168)
        WRITE(20,30)
  30    FORMAT(' STICS SELECTED RATES')
        WRITE(20,35) FULL_L1_NAME(1:NAMELEN)
  35    FORMAT(T10,'Input Level 1 Filename: ',A)
        WRITE(20,37)FIFO,TGS,MODE_AT_START
  37    FORMAT(T10,'FIFO mode is ',A3,T30,'TGS mode is ',A3,T50,' Telemetry mode is ',A17/)
      ENDIF
C
C Set flags for start and stop
C
      STARTFOUND=.FALSE.
      ENDFOUND=.FALSE.
C
C Initialize arrays to 0
C
  50  DO 60 I = 0, 31
         DVS_HITS(I) = 0
         DO 70 J = 1,4
            SUM_RATES(I,J) = 0
  70	 CONTINUE
  60  CONTINUE
      DVS_STEPS = 0
      SPEED_HITS = 0
      SUM_HE2_SPEED = 0
      PHA_BR0 = 0
      MR_BR0 = 0
      BR0_NORM = 0
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
      CALL MASS_SPEED(HDB,MASS_RATE,H_SPEED,H_UNCER,HE2_SPEED,HE2_UNCER)
      IF((HE2_SPEED.GE.200.0).AND.(HE2_SPEED.LE.1000))THEN   !to find average speed		
         SPEED_HITS = SPEED_HITS +1.0
         SUM_HE2_SPEED = SUM_HE2_SPEED + HE2_SPEED
      ENDIF 

C
C Get appropriate increment for loop below. At HBR, there is data in every spin; at LBR
C there is data only in even-numbered spins.
C
      BITRATE = SCI.EDB(N).BITRATE
      IF(BITRATE)THEN
        INCREMENT=1
      ELSE
        INCREMENT=2
      ENDIF
C
C Decompress STICS rates for one science record and output
C
      DO 900 J=0,59,INCREMENT
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
          IF(ENDFOUND)GO TO 907
C
C Check quality flags. If ANY quality flags are bad, skip this spin
C
          IF(BITRATE)THEN
            IF(SCI.EDB(J).Qual.NE.0)GOTO 900
          ELSE
            IF((SCI.EDB(J).Qual.NE.0).OR.(SCI.EDB(J+1).Qual.NE.0))GOTO 900
          ENDIF
          IF(SCI.EDB(J).ST_HMR_Qual.NE.0)GOTO 900
          IF(SCI.EDB(J).ST_SMR_Qual.NE.0)GOTO 900
          IF(SCI.EDB(J).ST_BR_Qual.NE.0)GOTO 900
          IF(SCI.EDB(J).ST_OMR_Qual.NE.0)GOTO 900
          IF(SCI.EDB(J).ST_ER_Qual.NE.0)GOTO 900
C
C Get dpps enable information, voltage step
C
          ITEMP =HDB.BDH.DAT(24)
          ITEMP2=HDB.BDH.DAT(25)
	  DPPSPLUS  = IBITS(ITEMP,7,1)
          DPPSMINUS = IBITS(ITEMP2,0,1) 
          ITEMP = STICS_RATE.EDB(J).RATE(179)
          VSTEP = IBITS(ITEMP,0,5)
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
C Compression codes for STICS Matrix rates are in the HDB in byte 189, bit 1, and bit 2
C for the Engineering rates (except the UFSR, URSR, and TCRs which is in bit 1) 
C
          ITEMP = HDB.BDH.DAT(189)
          CODE_C = BTEST(ITEMP,1)
C
C Get TGS mode
C
          ITEMP = STICS_RATE.EDB(N).RATE(179)
          IF(IBITS(ITEMP,7,1).EQ.1)THEN
            TGS='ON '
          ELSE
            TGS='OFF'
          ENDIF
C
C     ********** Get Fe9 and Fe10 matrix rate ************
C
          IF(TGS .EQ. 'OFF') THEN  
            STX_RATE(1) = WIND_DECOMPRESS(STICS_RATE.EDB(J).RATE(161),CODE_C)  !MR_FE10
            STX_RATE(2) = WIND_DECOMPRESS(STICS_RATE.EDB(J).RATE(162),CODE_C)  !MR_FE09
          ELSE
            STX_RATE(1) = 0				!No Fe matrix rates when TGS is ON
            STX_RATE(2) = 0
          ENDIF

C
C Decompress STICS rates, and get sum of BR0, BR1 and BR2:
C
          DO 220 I=120,143
            STICS_RATES(I+1)=WIND_DECOMPRESS(STICS_RATE.EDB(J).RATE(I),CODE_C)
 220      CONTINUE
          DO 230 L=0,2
            SUMBR(L) = 0
            DO 225 M=1,8
              SUMBR(L) = SUMBR(L) + STICS_RATES(120+L*8+M)
 225        CONTINUE  
 230      CONTINUE  
          MR_BR0 = SUMBR(0) + MR_BR0

C      *********** Create Fe9 and Fe10 rates from PHA **********************
C
C Do for all STICS PHA words
C
          PHA_FE10 = 0
          PHA_FE09 = 0
          DO 300 I=0, (SCI.EDB(J).STICS_Len-1) * 4, 4
C
C Get parts of PHA word
C
            CALL DISSECT_STICS_PHA(STICS_PHA.EDB(J).PHA(I),
     .                STOP_ID,START_ID_RANGE,ENERGY,SECTOR,
     .                SSD_ID,TIME_OF_FLIGHT,PSTART,RANGE)
C
C Calculate Mass and M/Q using DPU algorithms
C
            CALL MMPQ_DPU_WSTICS(DPPSPLUS,DPPSMINUS,VSTEP,
     .                         ENERGY,TIME_OF_FLIGHT,
     .                         DPU_MASS_OVER_Q,DPU_MASS,
     .                         DPU_NM,DPU_NQ,DPU_RANGE)
	    IF (FIFO .EQ. 'ON ')DPU_RANGE = 2

            IF((DPU_NM.GE.49).AND.(DPU_NM.LE.56).AND.
     .         (DPU_NQ.GE.59).AND.(DPU_NQ.LE.61)) PHA_FE10 = PHA_FE10+1


            IF((DPU_NM.GE.49).AND.(DPU_NM.LE.56).AND.
     .         (DPU_NQ.GE.62).AND.(DPU_NQ.LE.65)) PHA_FE09 = PHA_FE09+1

	    IF(DPU_RANGE.EQ.0)PHA_BR0=PHA_BR0+1
 300	  CONTINUE

          IF(PHA_BR0.GT.0) THEN
            BR0_NORM = FLOAT(MR_BR0)/FLOAT(PHA_BR0)
            IF(BR0_NORM .LT. 1.0)THEN
	       BR0_NORM_FLAG = BR0_NORM_FLAG + 1.0
	       BR0_NORM=1.0
            ELSE
               BR0_NORM_GOOD = BR0_NORM_GOOD + 1.0
            ENDIF
	  ELSE
            BR0_NORM = 1.0
          ENDIF
          
	  
          STX_RATE(3) = PHA_FE10*BR0_NORM
          STX_RATE(4) = PHA_FE09*BR0_NORM

          DVS_HITS(VSTEP) = DVS_HITS(VSTEP) + 1.0			!Increment voltage step hit counter
          DO 600 I = 1,4
            SUM_RATES(VSTEP,I)=SUM_RATES(VSTEP,I) + STX_RATE(I)	!Sum individual rates for each voltage step
 600      CONTINUE
        ENDIF !{startfound}

 900  CONTINUE
C
C Read next Science Record
C
 905  CALL WIND_L1_READ_SR( L1_LUN, CNTR, L1_SR_HEADER, STATUS )

      IF (.NOT. STARTFOUND) GO TO 100
C
C Average rates over interval. Average rates if END_TIME reached or at EOF.
C
 907  IF((L1_SR_HEADER.SR_COUNT .GE. SR_INTERVAL) .OR.
     +   (ENDFOUND) .OR. (STATUS .EQ. RMS$_EOF))THEN
         Q = Q + 1
C
C Initialize matrix to zeros
C
         DO 915 K = 1, 4
           RATE_DATA(K) = 0
           DO 910 I = 0, 31
             AVERAGE_RATES(I,K) = 0
 910       CONTINUE
 915     CONTINUE
         AVE_SPEED = 0
C
C Find average speed
C
         IF(SPEED_HITS.GT.0)AVE_SPEED = SUM_HE2_SPEED/SPEED_HITS	 

C
C Find average of each rate over interval
C
         DO 925 I = 0, 31
           IF(DVS_HITS(I) .GT. 0) DVS_STEPS = DVS_STEPS + 1.0   !count number of voltage steps used 
           IF(DVS_HITS(I) .EQ. 0) DVS_HITS(I) = 1.0             !to avoid dividing by 0
           DO 920 K = 1, 4
             IF(RATEFLAG .EQ. 'H') THEN				!In High Bit rate, rates are in counts per spin
                AVERAGE_RATES(I,K) = SUM_RATES(I,K)/DVS_HITS(I)	!In Low Bit rate, rates are summed over two spins
             ELSE       				        !We want counts per spin, so divide by 2 in LBR.
                AVERAGE_RATES(I,K) = SUM_RATES(I,K)/DVS_HITS(I)/2.0
             ENDIF
 920       CONTINUE
 925     CONTINUE

	 WRITE(6,'(F8.1)') DVS_STEPS
         IF(DVS_STEPS.EQ.0)DVS_STEPS = 1.0			!to avoid dividing by 0 when DPPS stepping is OFF
C
C Correct for efficiencies 
C                         
         IF(EFF_REPLY .EQ. 2) THEN
             DO 930 I = 0,31
               AVERAGE_RATES(I,1) = AVERAGE_RATES(I,1)/MR_EFF(I,162)
               AVERAGE_RATES(I,2) = AVERAGE_RATES(I,2)/MR_EFF(I,163)
               AVERAGE_RATES(I,3) = AVERAGE_RATES(I,3)/MR_EFF(I,162)
               AVERAGE_RATES(I,4) = AVERAGE_RATES(I,4)/MR_EFF(I,163)
 930         CONTINUE
         ENDIF

C
C Sum each rate over all voltage steps then divide by the total number of voltage steps
C
         DO 937 K = 1,4
           DO 936 I = 0,31
             RATE_DATA(K) = RATE_DATA(K) + AVERAGE_RATES(I,K)
 936       CONTINUE
           RATE_DATA(K) = RATE_DATA(K)/DVS_STEPS
 937     CONTINUE
C
C Write rates and interval end time to output file
C
         WRITE(20,940)TIME_TO_USE.YEAR,TIME_TO_USE.DOY,TIME_TO_USE.HR,
     +                TIME_TO_USE.MIN,TIME_TO_USE.SEC,AVE_SPEED,
     +                (RATE_DATA(K),K=1,4),MR_BR0,PHA_BR0
 940     FORMAT(1X,I4,1X,I3,1X,I2,':',I2.2,':',I2.2,3X,F6.1,2X,4(1X,F12.6),2I10)
C
C Data gaps
C
         IF(L1_SR_HEADER.SR_COUNT .GT. SR_INTERVAL) THEN
            Q = Q + 1
            AVE_SPEED= 9999.0
            TIME_TO_USE.SEC = TIME_TO_USE.SEC + 3
            DO 950 K = 1, 4
              RATE_DATA(K) = 88888.888
 950        CONTINUE
            WRITE(20,940)TIME_TO_USE.YEAR,TIME_TO_USE.DOY,TIME_TO_USE.HR,
     +                   TIME_TO_USE.MIN,TIME_TO_USE.SEC,AVE_SPEED,
     +                   (RATE_DATA(K),K=1,4),MR_BR0,PHA_BR0
         ENDIF

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
      CLOSE(UNIT=20)                            ! Close output file
      WRITE(6,*)'TIMES PHA_BR0 > MR_BR0 =',BR0_NORM_FLAG
      WRITE(6,*)'TIMES PHA_BR0 < MR_BR0 =',BR0_NORM_GOOD
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
c***************************************************************************************************
      SUBROUTINE OMR_EFF(EFF_DATA)
c***************************************************************************************************
C This routine calculates the efficiency for the omni-directional matrix rates (OMR) 1-19 at each
C voltage step. These rates are for triple coincidence only, i.e. both mass/charge and non-zero
C mass obtained. Efficiencies for all other rates are set to 1.0. 
c
c Created 27-SEP-1995 	by	K. CHOTOO
c
C Subroutines called:	WSTICSEFF
c Revision History:
C
C***************************************************************************************************
			IMPLICIT NONE
      REAL*4 DCR_FSR,			!DCR/FSR
     .       deltaE,			!Energy bandwidth (=0.019E)
     .       deltaT,			!Net Time for spin (Spin Period - Deadtime)
     .       E,				!Energy
     .       EFF_DATA(0:31,1:178),      !Matrix rate efficiency TCR/Io
     .       EPQ(0:31),            	!Energy/charge for each voltage step
     .       FSR_Io,			!FSR/Io
     .       ION_MASS(1:19),		!Mass of ions
     .       M,				!Mass
     .       MR_ENERGY(0:31,1:19),	!Total energy at each voltage step for rates
     .       TCR_FSR			!TCR/FSR             

      INTEGER I,J			!Counters

      DATA ION_MASS/11.916,11.916,11.916,15.872,15.872,15.872,20.019,24.112,24.112,27.863,
     +              27.863,27.863,55.404,55.404,55.404,55.404,55.404,55.404,55.404/


C		************ STATEMENTS *************

      deltaT = 2.72		!Spin Period - Deadtime (3.00 -  0.28 = 2.72 secs)
      DO 10  I = 0,31
        EPQ(I) = 6.190722 * (1.1225857**I)
  10  CONTINUE

      DO 15 J=1,178			!Initialize efficiency array to 1.0
        DO 14 I=0,31
          EFF_DATA(I,J) = 1.0
  14    CONTINUE
  15  CONTINUE

      DO 20  I = 0,31
        MR_ENERGY(I,1)  = EPQ(I)*6.0	!C+6
        MR_ENERGY(I,2)  = EPQ(I)*5.0	!C+5
        MR_ENERGY(I,3)  = EPQ(I)*4.0	!C+4
        MR_ENERGY(I,4)  = EPQ(I)*7.0	!O+7
        MR_ENERGY(I,5)  = EPQ(I)*6.0	!O+6
        MR_ENERGY(I,6)  = EPQ(I)	!O+1
        MR_ENERGY(I,7)  = EPQ(I)*8.0	!Ne+8
        MR_ENERGY(I,8)  = EPQ(I)*10.0	!Mg+10
        MR_ENERGY(I,9)  = EPQ(I)*8.0	!Mg+8
        MR_ENERGY(I,10) = EPQ(I)*12.0	!Si+12
        MR_ENERGY(I,11) = EPQ(I)*9.0	!Si+9
        MR_ENERGY(I,12) = EPQ(I)*8.0	!Si+8
        MR_ENERGY(I,13) = EPQ(I)*16.0	!Fe+16
        MR_ENERGY(I,14) = EPQ(I)*14.0	!Fe+14
        MR_ENERGY(I,15) = EPQ(I)*12.0	!Fe+12
        MR_ENERGY(I,16) = EPQ(I)*11.0	!Fe+11
        MR_ENERGY(I,17) = EPQ(I)*10.0	!Fe+10
        MR_ENERGY(I,18) = EPQ(I)*9.0	!Fe+9
        MR_ENERGY(I,19) = EPQ(I)*8.0	!Fe+8
  20  CONTINUE 

      DO 40 J=1,19
        DO 30 I=0,31
          M = ION_MASS(J)
          E = MR_ENERGY(I,J)
          deltaE = 0.019*E				!Energy bandwidth
          CALL WSTICSEFF(E,M,FSR_Io,TCR_FSR,DCR_FSR)
          EFF_DATA(I,145+J) = TCR_FSR * FSR_Io * deltaE * deltaT	!TCR/Io only for OMR1-19
  30    CONTINUE
  40  CONTINUE

      RETURN
      END




