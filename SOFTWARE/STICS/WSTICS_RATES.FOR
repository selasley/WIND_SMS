CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      PROGRAM WSTICS_RATES
C This program reads a WIND L1 data file and prompts the user to select rates. 
C These rates are decompressed, averaged over a given number of Science Records
C for each voltage step, then each  rate per spin is averaged over
C all voltage steps for the given Science Record interval. So over a given time
C each rate is written as counts/spin averaged over all voltage steps. The rates
C are then written to a file against the interval end time and solar wind speed.
C 
C  STICS_RATE_TABLE   Created 	3-NOV-1994	JOHN PAQUETTE
C                              21-FEB-1995      most recent version
C
C  CHOOSE_STICS_RATES Created from above  13-MAR-1995      K. CHOTOO
C
C Revision history:
C	16-MAR-1995	K. CHOTOO	TGS mode check done every spin,N=>J,
C					also if HDB flag is bad, skip entire Sci Rec
C	17-APR-1995	K. CHOTOO	Do averages also at End Time and at EOF.
C					Rates are given as counts/spin averaged over
C					all voltage steps.
C	16-AUG-1995	K. CHOTOO	Added solar wind speed to output file
C       21-AUG-1995     K. CHOTOO	Output file rates rounded off to 4 decimal places 
C       22-AUG-1995     K. CHOTOO       Routine can now work for more than one day
C       29-SEP-1995     K. CHOTOO       Add new routine to calculate efficiencies for OMR1-19
C        1-OCT-1995     K. CHOTOO	Change output file format from F10.4 to E13.7
C        1-DEC-1995     K. CHOTOO    	User can now give output filename if desired
C   	14-MAY-1996     K. CHOTOO	Write both H and He average speeds to output file
C  	15-MAY-1996	K. CHOTOO       Corrected values will be omni-directional flux in 
C					units of  { counts/(cm2-sec-sr-keV/amu) }. Output 
C					file now has run time.
C	20-SEP-1996	K. CHOTOO	Add check for bad Science Record number
C	10-OCT-1996	K. CHOTOO	Interval END time replaced with interval START time.
C	11-NOV-1996	K. CHOTOO	Changed MASS subroutine for getting speed
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
      REAL*4  AVERAGE_RATES(0:31,1:12),	!Array for averaging rates per voltage step
     .	      BULK_SPEED,		!Solar wind speed
     .	      BULK_UNCER,		!Uncertainty in solar wind speed
     .        DVS_HITS(0:31),		!Counts # of hits for each voltage step
     .        DVS_STEPS,		!Counts number of voltage steps used
     .        MR_EFF(0:31,1:178),	!Efficiencies for rates at each voltage step
     .        RATE_DATA(12),    	!Stores average rates per spin averaged over all voltage step
     .	      SPEED_AVG,		!Average speed over interval
     .	      SPEED_HITS,		!Number of valid speeds over interval
     .        SPEED_SUM,		!Sum of speeds over interval
     .        SUM_RATES(0:31,1:12),	!Sums rates over Science Record interval
     .	      UNCER_AVG,		!Average error in speed
     .	      UNCER_SUM			!Sum of speed uncertainties

C S/C position
      REAL*4 X1_GSE,Y1_GSE,Z1_GSE,     	!Start position
     .       X2_GSE,Y2_GSE,Z2_GSE,	!Stop position
     .       Re/6371.0/			!1 Re = 6371 km            

      INTEGER*4 EFF_REPLY,              !User response for raw counts or corrected counts
     .		ERR_SPD_FLAG,		!Set to 1 for bad speed
     .		FIRSTSPEED/0/,		!Set to -1 after getting speed at start time
     .          IFILE,			!Number of L1 files used
     .          INTERVAL,               !Number of Science Records for interval
     .          OFILE,			!Output file unit number
     .          PICKS(12),		!Selection numbers for STICS rates
     .          OPENMODE,		!Variable for opening next file
     .          Q,			!Increments for each new interval or data gap
     .          RATENUM,		!Number of rates selected
     .          SR_INTERVAL,		!Boundary for Science Record interval
     .          STX_RATE(12)		!Selected STICS rates
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
C Find out whether user wants raw counts or corrected counts
C
      WRITE(6,*)' FOR RAW COUNTS, ENTER 1'
      WRITE(6,*)' FOR OMNI-DIRECTIONAL FLUX [#/(cm2-sec-sr-keV/amu)], ENTER 2'
      READ(5,*)EFF_REPLY
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
      LISFIL(1:9) = 'WSTX_RATE'
      IF(EFF_REPLY .EQ. 2) THEN
         LISFIL(JSAVE+1:JSAVE+7) = 'FLX.TBL'        
      ELSE
         LISFIL(JSAVE+1:JSAVE+7) = 'RAW.TBL'
      ENDIF
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
c Get S/C position at beginning of time period
c
      IF(IFILE .EQ. 1) THEN
         X1_GSE = L1_SR_HEADER.GSE_POS.X/Re
         Y1_GSE = L1_SR_HEADER.GSE_POS.Y/Re
         Z1_GSE = L1_SR_HEADER.GSE_POS.Z/Re 
         WRITE(6,*)X1_GSE,Y1_GSE,Z1_GSE
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
      IF(ITEMP.EQ.  0)ITEMP=10
      write(6,*)'mode at start = ',itemp
      MODE_AT_START = MODE(ITEMP)
C
      ITEMP = STICS_RATE.EDB(N).RATE(179)
      IF(IBITS(ITEMP,7,1).EQ.1)THEN
        TGS='ON '
      ELSE
        TGS='OFF'
      ENDIF
C
C Display Rate Menu and prompt user to select up to 12 rates
C
      IF(IFILE .EQ. 1) CALL RATE_MENU(TGS,PICKS,RATENUM)
C
C Get matrix rate efficiencies
C
      IF(IFILE .EQ. 1) CALL OMR_EFF(MR_EFF)		!get efficiencies         

C    ******************************************************************************
C    *										  *
C    *                OPEN OUTPUT FILE AND WRITE HEADER INFORMATION               *
C    *										  *
C    ******************************************************************************
      IF(IFILE .EQ. 1) THEN
        OPEN(UNIT= 20, NAME=OUTFILENAME, DEFAULTFILE='TABLE_DIR:', FORM='FORMATTED', 
     &     TYPE='NEW', RECL=300)

	CALL TIME(RUN_TIME)			!Get run time
	CALL DATE(RUN_DATE)			!Get run date

        WRITE(20,30)
  30    FORMAT(' STICS SELECTED RATES')
	WRITE(20,'(A)') ' Run time  '//RUN_TIME//'   '//RUN_DATE

        WRITE(20,35) FULL_L1_NAME(1:NAMELEN)
  35    FORMAT(T10,'Input Level 1 Filename: ',A)
        WRITE(20,37)FIFO,TGS,MODE_AT_START
  37    FORMAT(T10,'FIFO mode is ',A3,T30,'TGS mode is ',A3,T50,' Telemetry mode is ',A17/)
	WRITE(20,*)'Time shown is the START time for each interval'
	WRITE(20,*)

C
C Print names of the selected rates to the output file
C
        OFILE = 20						!Output file unit number
        CALL RATE_HEADER(OFILE,TGS,PICKS,RATENUM)
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
         DO 70 J = 1,RATENUM
            SUM_RATES(I,J) = 0
  70	 CONTINUE
  60  CONTINUE
      DVS_STEPS = 0
      SPEED_HITS = 0
      SPEED_SUM = 0
      UNCER_SUM = 0
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
	  IF((J .EQ. 0) .OR. (FIRSTSPEED .EQ. 0)) THEN
            CALL WIND_L1_GET_MASS(L1_LUN,MASS_RATE,MASS_PHA,STATUS)   ! mass data
            IF (STATUS .NE. 1) GOTO 9085

      	    CALL SMS_MASS_SPEED(HDB, MASS_RATE,  	!input
     .         BULK_SPEED, BULK_UNCER, ERR_SPD_FLAG , ERR_MESSAGE)	!output

            IF(ERR_SPD_FLAG .EQ. 0) THEN
	      IF((BULK_UNCER .GT. 0) .AND. (BULK_UNCER .LT. 150.0)) THEN
	        SPEED_HITS = SPEED_HITS + 1.0
	        SPEED_SUM  = SPEED_SUM + BULK_SPEED
		UNCER_SUM  = UNCER_SUM + BULK_UNCER
	      ENDIF
            ELSE
	      WRITE(6,*) ERR_MESSAGE
            ENDIF
	  ENDIF

  	  FIRSTSPEED = -1		!Change after giving 1st speed

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
C Get voltage step and determine if the deflection systems are on.
C
          ITEMP = STICS_RATE.EDB(J).RATE(179)
          VSTEP = IBITS(ITEMP,0,5)
          ITEMP=HDB.BDH.DAT(24)
          ITEMP2=HDB.BDH.DAT(25)
C
C The logic that follows assumes that both + and - high voltages are on or both are off.
C If both are on, calculate e/q. If both are off, set e/q to 0.
C
          IF(IAND(IBITS(ITEMP,7,1), IBITS(ITEMP2,0,1)).EQ.1)THEN
            E_OVER_Q = D1 * (D2**VSTEP)
          ELSE
            E_OVER_Q = 0
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
C Compression codes for STICS Matrix rates are in the HDB in byte 189, bit 1, and bit 2
C for the Engineering rates (except the UFSR, URSR, and TCRs which is in bit 1) 
C
          ITEMP = HDB.BDH.DAT(189)
          CODE_C = BTEST(ITEMP,1)
	  CODE_C2= BTEST(ITEMP,2)
C
C Get TGS mode
C
          ITEMP = STICS_RATE.EDB(N).RATE(179)
          IF(IBITS(ITEMP,7,1).EQ.1)THEN
            TGS='ON '
          ELSE
            TGS='OFF'
          ENDIF
c
C Decompress only the selected rates
C
          DO 600 K = 1, RATENUM
C
C Set compression codes for different rates in Normal mode
C
	    IF(((PICKS(K).GE.1) .AND. (PICKS(K).LE.164)) .OR.
     +         ((PICKS(K).GE.174) .AND. (PICKS(K).LE.176))) THEN
                COMP_CODE = CODE_C
	    ELSE
	        COMP_CODE = CODE_C2
	    ENDIF
C     
C If TGS mode, make compression code changes, decompress and sum FSR's for each telescope
C
	    IF(TGS .EQ. 'ON ') THEN
		IF((PICKS(K).GE.156) .AND. (PICKS(K).LE.161)) THEN
		   COMP_CODE = CODE_C2
		   DO 150 I = 155,160
		     STICS_RATES(I+1)=WIND_DECOMPRESS(STICS_RATE.EDB(J).RATE(I),COMP_CODE)
 150		   CONTINUE
                   STX_RATE(K) = STICS_RATES(PICKS(K)) + STICS_RATES(PICKS(K)+1)  
		   GO TO 600
	        ENDIF
                IF(PICKS(K).EQ. 164) COMP_CODE = CODE_C2
            ENDIF
C
C Decompress STICS rates and get sum of HMR's:
C
	    IF(PICKS(K).EQ.1) THEN
               SUMHMR = 0
               DO 200 I=0,95
                  STICS_RATES(I+1)=WIND_DECOMPRESS(STICS_RATE.EDB(J).RATE(I),COMP_CODE)
                  SUMHMR = SUMHMR + STICS_RATES(I+1)
 200           CONTINUE
	       STX_RATE(K) = SUMHMR
               GO TO 600
            ENDIF
C
C Decompress STICS rates, and get sum of SMR's:
C
            IF(PICKS(K).EQ.97) THEN
               SUMSMR = 0
               DO 210 I=96,119
                  STICS_RATES(I+1)=WIND_DECOMPRESS(STICS_RATE.EDB(J).RATE(I),COMP_CODE)
                  SUMSMR = SUMSMR + STICS_RATES(I+1)
 210           CONTINUE
               STX_RATE(K) = SUMSMR
               GO TO 600
     	    ENDIF
C
C Decompress STICS rates, and get sum of BR0, BR1 and BR2:
C
            IF((PICKS(K) .GE. 121) .AND. (PICKS(K) .LE. 137)) THEN
               DO 220 I=120,143
                  STICS_RATES(I+1)=WIND_DECOMPRESS(STICS_RATE.EDB(J).RATE(I),COMP_CODE)
 220           CONTINUE
               DO 230 L=0,2
                  SUMBR(L) = 0
                  DO 225 M=1,8
                    SUMBR(L) = SUMBR(L) + STICS_RATES(120+L*8+M)
 225             CONTINUE  
 230          CONTINUE  
               STX_RATE(K) = SUMBR((PICKS(K) - 121)/8)
               GO TO 600
            ENDIF
C
C Decompress any other STICS rate 
C
            P = PICKS(K) - 1
            STX_RATE(K)=WIND_DECOMPRESS(STICS_RATE.EDB(J).RATE(P),COMP_CODE)
 600      CONTINUE	
	    
      
          DVS_HITS(VSTEP) = DVS_HITS(VSTEP) + 1.0			!Increment voltage step hit counter
          DO 700 I= 1,RATENUM
            SUM_RATES(VSTEP,I)=SUM_RATES(VSTEP,I) + STX_RATE(I)	!Sum individual rates for each voltage step
 700      CONTINUE

        ENDIF !{startfound}

 900  CONTINUE
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
         Q = Q + 1
C
C Initialize matrix to zeros
C
         DO 915 K = 1, RATENUM
           RATE_DATA(K) = 0
           DO 910 I = 0, 31
             AVERAGE_RATES(I,K) = 0
 910       CONTINUE
 915     CONTINUE

C
C Find average speed
C
         IF(SPEED_HITS.GT.0) THEN
	   SPEED_AVG = SPEED_SUM/SPEED_HITS
	   UNCER_AVG = UNCER_SUM/SPEED_HITS
	 ELSE 
	   SPEED_AVG = 0
	   UNCER_AVG = 0
	 ENDIF

C
C Find average of each rate over interval
C
         DO 925 I = 0, 31
           IF(DVS_HITS(I) .GT. 0) DVS_STEPS = DVS_STEPS + 1.0   !count number of voltage steps used 
           IF(DVS_HITS(I) .EQ. 0) DVS_HITS(I) = 1.0             !to avoid dividing by 0
           DO 920 K = 1, RATENUM
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
           DO 935 K = 1,RATENUM
             IF((TGS .EQ. 'ON ').AND.(PICKS(K).GE.156).AND.(PICKS(K).LE.164))GOTO 935
             DO 930 I = 0,31
               IF(MR_EFF(I,PICKS(K)).GT.0) AVERAGE_RATES(I,K) = AVERAGE_RATES(I,K)/MR_EFF(I,PICKS(K))
 930         CONTINUE
 935       CONTINUE
         ENDIF

C
C Sum each rate over all voltage steps then divide by the total number of voltage steps
C
         DO 937 K = 1,RATENUM
           DO 936 I = 0,31
             RATE_DATA(K) = RATE_DATA(K) + AVERAGE_RATES(I,K)
 936       CONTINUE
           RATE_DATA(K) = RATE_DATA(K)/DVS_STEPS
 937     CONTINUE
C
C Write rates and interval end time to output file
C
         WRITE(20,940)INT_START_TIME.YEAR,INT_START_TIME.DOY,INT_START_TIME.HR,
     +                INT_START_TIME.MIN,INT_START_TIME.SEC,SPEED_AVG,UNCER_AVG,
     +                (RATE_DATA(K),K=1,RATENUM)
 940     FORMAT(1X,I4,1X,I3,1X,I2,':',I2.2,':',I2.2,2X,F6.1,4X,F6.1,2X,12(2X,E13.7))

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
9080  TYPE *,'ERROR GETTING STICS DATA; STATUS=',STATUS
      STOP
9085  TYPE *,'ERROR GETTING MASS DATA; STATUS=',STATUS
      STOP
9090  TYPE *,'ERROR CLOSING L1 FILE; STATUS=',STATUS
      STOP
      END



C*****************************************************************************80
      SUBROUTINE RATE_MENU(TGS_MODE,SELECTION,NUM)
C This subroutine displays a menu of the available rates and prompts the user
C to choose the ones required. Or the user can go with a list of pre-selected
C rates. When TGS is on there are no Silicon 8, Silicon 9 nor any Iron rates.
C Instead there are more Engineering rates, UFSR, URSR, MFSR, MRSR.....
C
C Created  7-MAR-1995	by  K. CHOTOO
C

      IMPLICIT NONE

      INTEGER NUM,				!counts number of selections made
     +        I,				!subscript
     +        SELECTION(12)			!array to store selections
      CHARACTER*1 REPLY          		!answer for another selection
      CHARACTER*3 TGS_MODE      		!TGS on/off
C
      SELECTION(1) = 97
      SELECTION(2) =  1 
      SELECTION(3) = 145 
      SELECTION(4) = 121   
      SELECTION(5) = 129  
      SELECTION(6) = 137   
      SELECTION(7) = 165  
      SELECTION(8) = 166   
      SELECTION(9) = 167  
      SELECTION(10) = 151   
      SELECTION(11) = 150
      SELECTION(12) = 149 
      
      IF(TGS_MODE .EQ. 'ON ')THEN
         SELECTION(7)  = 156			!make changes to default list		
         SELECTION(8)  = 158			!for TGS ON
         SELECTION(9)  = 160
      ENDIF
      WRITE(6,*)'TGS IS ',TGS_MODE
      TYPE 315
 315  FORMAT(//,1X,'The following is a list of rates that',
     +    ' will be printed out:'//
     +     5X,' (1) H      ',5X,' (2) He2    ',5X,' (3) He1    '/
     +     5X,' (4) BR0    ',5X,' (5) BR1    ',5X,' (6) BR2    '/
     +     5X,' (7) FSR12  ',5X,' (8) FSR34  ',5X,' (9) FSR56  '/
     +     5X,'(10) O1     ',5X,'(11) O6     ',5X,'(12) O7     '//
     +     1X,'Would you like to make your own list [Y/N]?  /N/'/)
           READ(5,'(A1)')REPLY

      NUM = 12
      IF((REPLY .EQ. 'Y') .OR. (REPLY .EQ. 'y')) NUM = 0
      DO WHILE (((REPLY .EQ. 'Y') .OR. (REPLY .EQ. 'y'))
     +           .AND. (NUM .LE. 12)) 
      NUM = NUM + 1
C        main menu
 320     TYPE 330
 330     FORMAT(/,32X,'MATRIX RATES'/
     +     9X,'  [1] He2  ',8X,' [97] H    ',8X,
     +        '[121] BR0  ',8X,'[129] BR1  '/
     +     9X,'[137] BR2  ',8X,'[145] He1  ',8X,
     +        '[146] C6	 ',8X,'[147] C5   '/
     +     9X,'[148] C4   ',8X,'[149] O7   ',8X,
     +        '[150] O6   ',8X,'[151] O1   '/
     +     9X,'[152] Ne8  ',8X,'[153] Mg10 ',8X,	
     +        '[154] Mg8  ',8X,'[155] Si12 ')

	 IF(TGS_MODE .EQ. 'OFF') THEN	!Normal mode
          TYPE 340
 340      FORMAT(
     +     9X,'[156] Si9  ',8X,'[157] Si8  ',8X,	
     +        '[158] Fe16 ',8X,'[159] Fe14 '/
     +     9X,'[160] Fe12 ',8X,'[161] Fe11 ',8X,
     +        '[162] Fe10 ',8X,'[163] Fe9'/	
     +     9X '[164] Fe8  '/
     +     32X,'ENGINEERING RATES'/
     +     9X,'[165] FSR12',8X,'[166] FSR34',8X,	
     +        '[167] FSR56',8X,'[168] RSR1 '/
     +     9X,'[169] RSR2 ',8X,'[170] RSR3 ',8X,
     +        '[171] DCR1 ',8X,'[172] DCR2 '/
     +     9X,'[173] DCR3 ',8X,'[174] TCR1 ',8X,	
     +        '[175] TCR2 ',8X,'[176] TCR3 '/
     +     9X,'[177] SSD1 ',8X,'[178] SSD2 ',8X,	
     +        '[179] SSD3 '/)

         ELSE				!TGS Mode
          TYPE 350
 350      FORMAT(/,32X,'ENGINEERING RATES'/
     +     9X,'[156] FSR12',8X,'[158] FSR34',8X,	
     +        '[160] FSR56',8X,'[162] UFSR '/
     +     9X,'[163] URSR ',8X,'[164] MFSR ',8X,	
     +        '[165] MRSR ',8X,'[166] MPF  '/
     +     9X,'[167] MPR  ',8X,'[168] RSR1 ',8X,	
     +        '[169] RSR2 ',8X,'[170] RSR3 '/
     +     9X,'[171] DCR1 ',8X,'[172] DCR2 ',8X,	
     +        '[173] DCR3 ',8X,'[174] TCR1 '/
     +     9X,'[175] TCR2 ',8X,'[176] TCR3 ',8X,
     +        '[177] SSD1 ',8X,'[178] SSD2 '/
     +     9X,'[179] SSD3 ',/) 
	 
         ENDIF

	 WRITE(6,*) 'Enter rate number'
         READ(5,*) SELECTION(NUM)
C
C Check to make sure selection number is valid.
C
         IF(TGS_MODE .EQ. 'OFF') THEN
          IF(((SELECTION(NUM).GT.1).AND.(SELECTION(NUM).LT.97)).OR.
     +      ((SELECTION(NUM).GT.97).AND.(SELECTION(NUM).LT.121)).OR.
     +      ((SELECTION(NUM).GT.121).AND.(SELECTION(NUM).LT.129)).OR.
     +      ((SELECTION(NUM).GT.129).AND.(SELECTION(NUM).LT.137)).OR.
     +      ((SELECTION(NUM).GT.137).AND.(SELECTION(NUM).LT.145)).OR.
     +       (SELECTION(NUM).GT.179))THEN
              WRITE(6,*)'Ambiguous selection number, please try again'
              GO TO 320
          ENDIF
         ENDIF

         IF(TGS_MODE .EQ. 'ON ') THEN
          IF(((SELECTION(NUM).GT.1).AND.(SELECTION(NUM).LT.97)).OR.
     +      ((SELECTION(NUM).GT.97).AND.(SELECTION(NUM).LT.121)).OR.
     +      ((SELECTION(NUM).GT.121).AND.(SELECTION(NUM).LT.129)).OR.
     +      ((SELECTION(NUM).GT.129).AND.(SELECTION(NUM).LT.137)).OR.
     +      ((SELECTION(NUM).GT.137).AND.(SELECTION(NUM).LT.145)).OR.
     +      (SELECTION(NUM).EQ.157).OR. (SELECTION(NUM).EQ.159) .OR.
     +      (SELECTION(NUM).EQ.161).OR.(SELECTION(NUM).GT.179))THEN
              WRITE(6,*)'Ambiguous selection number, please try again'
              GO TO 320
          ENDIF
         ENDIF

         WRITE(6,*) 'Selections already made:'
         DO 380 I = 1, NUM
         WRITE(6,*)SELECTION(I)
 380     CONTINUE
         WRITE(6,*) 'You may chose up to ',12 - NUM,' more rates.'
 390 	 WRITE(6,*) 'Would you like to make another selection [Y/N]?'
         READ(5,'(A1)') REPLY
C
C Check for valid reply
C
	 IF((REPLY .NE. 'Y') .AND. (REPLY .NE. 'y') .AND. (REPLY .NE. 'N')
     +       .AND. (REPLY .NE. 'n')) THEN
            WRITE(6,*) 'Ambiguos answer. Please try again'
            GO TO 390
         ENDIF
         
      END DO
 
      RETURN
      END

C*******************************************************************************
      SUBROUTINE RATE_HEADER(OUTFILE,TGS_MODE,CHOICES,NUM_OF_RATES)
C This subroutine prints out the header information for the rates that have
C already been selected be the user.
C
C Created 	9-MAR-1995	by 	K. CHOTOO
C
C
      IMPLICIT NONE
      INTEGER I,
     +        NUM_OF_RATES,						!number of rates to be printed
     +        POS, 							!position where rate name is written in LINE
     +        OUTFILE,							!unit number for output file
     +        CHOICES(12)						!array of rate number to be printed
      CHARACTER*9 LABELS(179)						!rate names
      CHARACTER*300 LINE						!stores header labels
      CHARACTER*3 TGS_MODE						!TGS mode on/off    
      DATA LABELS/ '     He2 ',95*'         ',  '       H ',23*'         ',!Rate headers in Normal mode
     +             '     BR0 ', 7*'         ',  '     BR1 ', 7*'         ',
     +             '     BR2 ', 7*'         ',  '     He1 ',   '      C6 ',
     +             '      C5 ',   '      C4 ',  '      07 ',   '      O6 ',
     +             '      O1 ',   '     Ne8 ',  '    Mg10 ',   '     Mg8 ',
     +		   '    Si12 ',   '     Si9 ',  '     Si8 ',   '    Fe16 ',
     +		   '    Fe14 ',   '    Fe12 ',  '    Fe11 ',   '    Fe10 ',
     +             '     Fe9 ',   '     Fe8 ',  '   FSR12 ',   '   FSR34 ',
     +             '   FSR56 ',   '    RSR1 ',  '    RSR2 ',   '    RSR3 ',
     +             '    DCR1 ',   '    DCR2 ',  '    DCR3 ',   '    TCR1 ',
     +             '    TCR2 ',   '    TCR3 ',  '    SSD1 ',   '    SSD2 ',
     +             '    SSD3 '/


      DO 300 I = 1,300
         LINE(I:I) = ' '
 300  CONTINUE          
      IF(TGS_MODE .EQ. 'ON ') THEN					!Changes to headers for TGS mode
        LABELS(156) = '   FSR12 '
        LABELS(157) = '         '
        LABELS(158) = '   FSR34 '
        LABELS(159) = '         '
        LABELS(160) = '   FSR56 '
        LABELS(161) = '         '
        LABELS(162) = '    UFSR '
        LABELS(163) = '    URSR '
        LABELS(164) = '    MFSR '
        LABELS(165) = '    MRSR '
        LABELS(166) = '     MPF '
        LABELS(167) = '     MPR '
      ENDIF
      WRITE(LINE(1:38),'(A)')' YEAR DOY HR:MN:SC   SPEED     UNCER  '
      POS = 39
      DO 400 I = 1, NUM_OF_RATES
         WRITE(LINE(POS+2:POS+14),'(A)') LABELS(CHOICES(I))
         POS = POS + 15
 400  CONTINUE
      WRITE(OUTFILE,'(A)')LINE(1:POS-1)
      
      RETURN
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
     .       DCR_RSR,			!DCR/RSR
     .       deltaE,			!Energy bandwidth (=0.019E)
     .       deltaT,			!Net Time for spin (Spin Period - Deadtime)
     .       E,				!Energy
     .       EFF_DATA(0:31,1:178),      !Matrix rate efficiency TCR/Io
     .       EPQ(0:31),            	!Energy/charge for each voltage step
     .	     GF,			!Overall geometric factor = 0.059 cm2-sr
     .       ION_MASS(1:22),		!Mass of ions
     .       M,				!Mass
     .       MR_ENERGY(0:31,1:22),	!Total energy at each voltage step for rates
     .       TCR_FSR			!TCR/FSR             

      INTEGER I,J			!Counters

      DATA ION_MASS/11.916,11.916,11.916,15.872,15.872,15.872,20.019,24.112,24.112,27.863,
     +              27.863,27.863,55.404,55.404,55.404,55.404,55.404,55.404,55.404,
     +              1.000,3.971,3.971/


C		************ STATEMENTS *************

      deltaT = 2.72		!Spin Period - Deadtime (3.00 -  0.28 = 2.72 secs)
      GF = 0.059
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
        MR_ENERGY(I,20) = EPQ(I)        !H+
        MR_ENERGY(I,21) = EPQ(I)*2.0    !He+2
        MR_ENERGY(I,22) = EPQ(I)        !He+1

  20  CONTINUE 

      DO 40 J=1,22
        DO 30 I=0,31
          M = ION_MASS(J)
          E = MR_ENERGY(I,J)
          deltaE = 0.019*E				!Energy bandwidth
          CALL WSTICSEFF(E,M,DCR_RSR,TCR_FSR,DCR_FSR)
	  IF(J .LE. 19) THEN
            IF(TCR_FSR*DCR_RSR .GT. 0) THEN
              EFF_DATA(I,145+J) = TCR_FSR * DCR_RSR * deltaE * deltaT * GF / M	!TCR/Io only for OMR1-19
            ELSE
              EFF_DATA(I,145+J) = 1.0
            ENDIF
     	  ELSE
            IF(J .EQ. 20) EFF_DATA(I, 97) = DCR_FSR * DCR_RSR * deltaE * deltaT * GF / M 
            IF(J .EQ. 21) EFF_DATA(I,  1) = DCR_FSR * DCR_RSR * deltaE * deltaT * GF / M
            IF(J .EQ. 22) EFF_DATA(I,145) = DCR_FSR * DCR_RSR * deltaE * deltaT * GF / M
	  ENDIF
  30    CONTINUE
  40  CONTINUE

      RETURN
      END

