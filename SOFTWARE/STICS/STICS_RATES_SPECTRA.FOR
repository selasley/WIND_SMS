CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      PROGRAM STICS_RATES_SPECTRA
C This program reads a WIND L1 data file and prompts the user to select rates. 
C These rates are decompressed, averaged over a given number of Science Records
C for each voltage step.  An output file is generated for each rate listing counts
C for each voltage step against the interval end time. The counts can either be
C raw counts or corrected with efficiencies. 
C 
C This program is a modified version of STICS_RATES_SPECTRA
C
C Created 2-APR-1996 	K. CHOTOO
C
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
      LOGICAL CODE_C,CODE_C2,COMP_CODE  !Compression codes 
      REAL*4  AVE_SPEED,		!Average solar wind speed
     .        CORRECTED_RATES(0:31,1:30),!Array for averaging rates per voltage step
     .        DVS_HITS(0:31),		!Counts # of hits for each voltage step
     .        H_SPEED,			!Proton speed
     .        H_UNCER,			!Uncertainty in proton speed from width
     .        HE2_SPEED,		!Alpha speed
     .	      HE2_UNCER,		!Uncertainty in alpha speed  from width
     .        MR_EFF(0:31,1:178),       !Efficiencies for rates at each voltage step
     .        SPEED_HITS,		!Number of valid speed values in interval
     .        SUM_HE2_SPEED,		!Sum of alpha speeds over interval
     .        SUM_RATES(0:31,1:30)	!Sums rates over Science Record interval
            
      INTEGER*4 IFILE,			!Counter for L1 files used
     .          OPENMODE,		!Variable for opening nextfile
     .          PICKS(30),		!Selection numbers for STICS rates
     .          RATENUM,		!Number of rates selected
     .          STX_RATE(30)		!Selected STICS rates
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
      CHARACTER*90 LISFIL,          	!holds the name of the input file
     .             OUTFILENAME1,	!Output file for corrected counts
     .             OUTFILENAME2		!Output file for raw counts
      CHARACTER  INPUTFILES(1:100)*90,	!L1 filename array for multiple inputs
     .           MR_NAMES(30)*5         !names of selected rates
     
 
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
C Use wall time 
C
      USE_SC_CLOCK = 0
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
      LISFIL(1:9) = 'STX_RATES'
      LISFIL(JSAVE+1:JSAVE+7) = 'ION.EFF'
      OUTFILENAME1 = LISFIL
      LISFIL(JSAVE+1:JSAVE+7) = 'ION.RAW'
      OUTFILENAME2 = LISFIL

C
C Open L1 file for input, and read first SR, Core, HDB, and STICS RATES
C
 1000 CONTINUE
      IF(FIRSTFILE .EQ. .FALSE.) THEN
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
      INPUTFILES(IFILE)=FULL_L1_NAME
      WRITE(6,1001)FULL_L1_NAME
 1001 FORMAT(//,'  OPEN FILE ',A)

      NAMELEN = INDEX(FULL_L1_NAME,' ') - 1
      IF (NAMELEN .LT. 1) NAMELEN=LEN(FULL_L1_NAME)
  13  CALL WIND_L1_READ_SR( L1_LUN, CNTR, L1_SR_HEADER, STATUS )
      IF(STATUS.EQ.RMS$_EOF)GO TO 1000
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

C**********************************************************************
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

      IF(IFILE .EQ.1) THEN
C
C Display Rate Menu and prompt user to select up to 12 rates
C
        CALL RATE_MENU(TGS,PICKS,RATENUM)
        CALL RATE_HEADER(TGS,PICKS,RATENUM,MR_NAMES)        		!get name of selected rates
        CALL OMR_EFF(MR_EFF)						!get matrix rate efficiencies
C    ******************************************************************************
C    *										  *
C    *                OPEN OUTPUT FILE AND WRITE HEADER INFORMATION               *
C    *										  *
C    ******************************************************************************
C

        OPEN(UNIT= 30, NAME=OUTFILENAME1, DEFAULTFILE='WIND_STICS_DATA:', FORM='FORMATTED', 
     &       TYPE='NEW', RECL=400)
        OPEN(UNIT= 31, NAME=OUTFILENAME2, DEFAULTFILE='WIND_STICS_DATA:', FORM='FORMATTED', 
     &       TYPE='NEW', RECL=400)

	WRITE(30,40)(MR_NAMES(I),I=1,RATENUM)
  40    FORMAT(/1X,'DVS  ',2X,30(4X,A5,3X))
	WRITE(31,42)(MR_NAMES(I),I=1,RATENUM)
  42    FORMAT(/1X,'DVS   HITS',30(4X,A5,1X))
                          
      ENDIF
C
C Set flags for start and stop
C
      STARTFOUND=.FALSE.
      ENDFOUND=.FALSE.
C
C Initialize arrays to 0
C
      DO 60 I = 0, 31
         DVS_HITS(I) = 0
         DO 70 J = 1,RATENUM
            SUM_RATES(I,J) = 0
  70	 CONTINUE
  60  CONTINUE
      SUM_HE2_SPEED = 0
      SPEED_HITS = 0

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

      IF((HE2_SPEED.GE.315.0).AND.(HE2_SPEED.LE.1000))THEN   !to find average speed		
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
	    
C
C In High Bit Rate, each voltage step is held constant for one spin. In Low Bit Rate
C each voltage step is held constant for two spins while rate bins are incremented.
C
 	  IF(RATEFLAG .EQ. 'H')THEN
            DVS_HITS(VSTEP) = DVS_HITS(VSTEP) + 1.0	
	  ELSE
            DVS_HITS(VSTEP) = DVS_HITS(VSTEP) + 2.0
	  ENDIF

          DO 700 I= 1,RATENUM
            SUM_RATES(VSTEP,I)=SUM_RATES(VSTEP,I) + STX_RATE(I)	        !Accumulate rates for each voltage step
 700      CONTINUE

        ENDIF !{startfound}

 900  CONTINUE
C
C Read next Science Record
C
 905  CALL WIND_L1_READ_SR( L1_LUN, CNTR, L1_SR_HEADER, STATUS )
      IF(STATUS.EQ.RMS$_EOF)GO TO 906
      IF (STATUS .NE. 1) GOTO 9020

      GO TO 100

 906  IF(START.YEAR .GE.0)GO TO 1000
 907  CALL WIND_L1_CLOSE( L1_LUN, STATUS )      ! Close input file
      IF (STATUS .NE. 1) GOTO 9090


C	*********************************************************       
C	*                                                       *
C	*	Average data and make efficiency corrections	*
C	*							*
C	*********************************************************

      DO 915 K = 1, RATENUM		 !Initialize matrix to zeros
        DO 910 I = 0, 31
          CORRECTED_RATES(I,K) = 0
 910    CONTINUE
 915  CONTINUE
      AVE_SPEED = 0
C
C Find average speed
C
      IF(SPEED_HITS.GT.0)AVE_SPEED = SUM_HE2_SPEED/SPEED_HITS	 
C
C Average rates and correct for efficiencies 
C                    
      DO I = 0,31				!To avoid dividing by 0
	IF(DVS_HITS(I).EQ.0)DVS_HITS(I) = 1.0
      END DO
     
      DO 935 K = 1,RATENUM
        IF((TGS .EQ. 'ON ').AND.(PICKS(K).GE.156).AND.(PICKS(K).LE.164))GOTO 935	!No Si9, Si8, and Fe rates when TGS is ON
        DO 930 I = 0,31
          CORRECTED_RATES(I,K) = SUM_RATES(I,K)/(MR_EFF(I,PICKS(K))*DVS_HITS(I))
 930    CONTINUE
 935  CONTINUE
C
C Write rates and interval end time to output files
C
      DO I = 0,31						!Corrected rates
        WRITE(30,939)I,(CORRECTED_RATES(I,K),K=1,RATENUM)
 939    FORMAT(1X,I3,2X,30(1X,E11.4))
      END DO

      DO I = 0,31						!Raw counts
        WRITE(31,940)I,INT(DVS_HITS(I)),(SUM_RATES(I,K),K=1,RATENUM)
 940    FORMAT(1X,I3,1X,I6,30(1X,F9.0))
      END DO

C
C Close output files
C         
      CLOSE(UNIT=30) 
      CLOSE(UNIT=31)                           
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
     +        SELECTION(30)			!array to store selections
      CHARACTER*1 REPLY          		!answer for another selection
      CHARACTER*3 TGS_MODE      		!TGS on/off
C
      SELECTION(1) = 97
      SELECTION(2) =  1 
      SELECTION(3) = 145 
      SELECTION(4) = 146   
      SELECTION(5) = 147  
      SELECTION(6) = 148   
      SELECTION(7) = 149  
      SELECTION(8) = 150   
      SELECTION(9) = 151  
      SELECTION(10) = 152   
      SELECTION(11) = 153
      SELECTION(12) = 154 
      SELECTION(13) = 155   
      SELECTION(14) = 156
      SELECTION(15) = 157 
      SELECTION(16) = 158   
      SELECTION(17) = 159
      SELECTION(18) = 160 
      SELECTION(19) = 161   
      SELECTION(20) = 162
      SELECTION(21) = 163
      SELECTION(22) = 164 
 
      WRITE(6,300)TGS_MODE
 300  FORMAT(//,1X,'TGS MODE IS ',A3)
      TYPE 315
 315  FORMAT(/,1X,'The following is a list of rates that',
     +    ' will be printed out:'/
     +     9X,' [97] H    ',8X,'  [1] He2  ',8X,
     +        '[145] He1  ',8X,'[146] C6   '/
     +     9X,'[147] C5   ',8X,'[148] C4   ',8X,
     +        '[149] O7   ',8X,'[150] O6   '/
     +     9X,'[151] O1   ',8X,'[152] Ne8  ',8X,
     +        '[153] Mg10 ',8X,'[154] Mg8  '/
     +     9X,'[155] Si12 ',8X,'[156] Si9  ',8X,
     +        '[157] Si8  ',8X,'[158] Fe16 '/
     +     9X,'[159] Fe14 ',8X,'[160] Fe12 ',8X,
     +        '[161] Fe11 ',8X,'[162] Fe10 '/
     +     9X,'[163] Fe9  ',8X,'[164] Fe8  '//
     +     1X,'Would you like to make your own list [Y/N]?  /N/'/)
           READ(5,'(A1)')REPLY

      NUM = 22
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
         WRITE(6,*) 'You may chose up to ',20 - NUM,' more rates.'
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
      SUBROUTINE RATE_HEADER(TGS_MODE,CHOICES,NUM_OF_RATES,		!input
     +                                               RATE_NAME)         !output
C This subroutine returns the names of the rates that have already been
C selected be the user.
C
C Created 	9-AUG-1995	by 	K. CHOTOO
C
C
      IMPLICIT NONE
      INTEGER I,
     +        NUM_OF_RATES,					!number of rates to be printed
     +        POS, 						!position where rate name is written in LINE
     +        CHOICES(30)					!array of rate number to be printed
      CHARACTER*5 LABELS(179),					!rate names
     +            RATE_NAME(30)					!names of selected rates
      CHARACTER*3 TGS_MODE					!TGS mode on/off    
      DATA LABELS/ '  He2',95*'     ','    H',23*'     ',     	!Rate headers in Normal mode
     +             '  BR0', 7*'     ','  BR1', 7*'     ',
     +             '  BR2', 7*'     ','  He1',   '   C6',
     +             '   C5',   '   C4','   07',   '   O6',
     +             '   O1',   '  Ne8',' Mg10',   '  Mg8',
     +		   ' Si12',   '  Si9','  Si8',   ' Fe16',
     +		   ' Fe14',   ' Fe12',' Fe11',   ' Fe10',
     +             '  Fe9',   '  Fe8','FSR12',   'FSR34',
     +             'FSR56',   ' RSR1',' RSR2',   ' RSR3',
     +             ' DCR1',   ' DCR2',' DCR3',   ' TCR1',
     +             ' TCR2',   ' TCR3',' SSD1',   ' SSD2',
     +             ' SSD3'/

      DO 300 I = 1,30
        RATE_NAME(I) = ' '
 300  CONTINUE

      IF(TGS_MODE .EQ. 'ON ') THEN					!Changes to headers for TGS mode
        LABELS(156) = 'FSR12'
        LABELS(157) = '     '
        LABELS(158) = 'FSR34'
        LABELS(159) = '     '
        LABELS(160) = 'FSR56'
        LABELS(161) = '     '
        LABELS(162) = 'UFSR'
        LABELS(163) = 'URSR'
        LABELS(164) = 'MFSR'
        LABELS(165) = 'MRSR'
        LABELS(166) = 'MPF'
        LABELS(167) = 'MPR'
      ENDIF

      DO 400 I = 1, NUM_OF_RATES
        RATE_NAME(I) = LABELS(CHOICES(I))  
 400  CONTINUE
      
      RETURN
      END

C***************************************************************************************************
      SUBROUTINE OMR_EFF(EFF_DATA)
c***************************************************************************************************
C This routine calculates the efficiency for the omni-directional matrix rates (OMR) 0-19 and the
C high-resolution and sectored matrix rates (HMR, SMR) at each voltage step. The HMR and SMR are
C treated as OMRs. Efficiencies for all other rates are set to 1.0. 
c
c Created 27-SEP-1995 	by	K. CHOTOO
c
c Revision History:
C	 8-FEB-1995	K. CHOTOO	Include Energy passband to efficiency correction, and added
C					H+, He+, and He+2 efficiencies.
C
C***************************************************************************************************
			IMPLICIT NONE
      REAL*4 DCR_FSR,			!DCR/FSR
     .       DCR_RSR ,			!DCR/RSR
     .       E,				!Energy
     .       EFF_DATA(0:31,1:178),      !Matrix rate efficiency TCR/Io
     .       EPQ(0:31),            	!Energy/charge for each voltage step
     .       ION_MASS(0:21),		!Mass of ions
     .       M,				!Mass
     .       MR_ENERGY(0:31,0:21),	!Total energy at each voltage step for rates
     .       PASSBAND,			!Energy passband of deflection system
     .       TCR_FSR			!TCR/FSR             

      INTEGER I,J			!Counters

      DATA ION_MASS/3.971,11.916,11.916,11.916,15.872,15.872,15.872,20.019,24.112,24.112,27.863,
     +              27.863,27.863,55.404,55.404,55.404,55.404,55.404,55.404,55.404,1.000,3.971/


C		************ STATEMENTS *************

      DO 10  I = 0,31
        EPQ(I) = 6.190722 * (1.1225857**I)
  10  CONTINUE

      DO 15 J=1,178			!Initialize efficiency array to 1.0
        DO 14 I=0,31
          EFF_DATA(I,J) = 1.0
  14    CONTINUE
  15  CONTINUE

      DO 20  I = 0,31
        MR_ENERGY(I,0)  = EPQ(I)        !He+1
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
        MR_ENERGY(I,20) = EPQ(I)        !H+   (SMR)
        MR_ENERGY(I,21) = EPQ(I)*2.0    !He+2 (HMR)
  20  CONTINUE 

      DO 40 J=0,21
        DO 30 I=0,31
          M = ION_MASS(J)
          E = MR_ENERGY(I,J)
          PASSBAND = 0.19 * MR_ENERGY(I,J)
          CALL WSTICSEFF(E,M,DCR_RSR ,TCR_FSR,DCR_FSR)
          IF(J .EQ.  0) EFF_DATA(I,145) = PASSBAND * DCR_FSR * DCR_RSR       	!Since H+,He+ & He+2 has TCR and DCR
          IF(J .EQ. 20) EFF_DATA(I, 97) = PASSBAND * DCR_FSR * DCR_RSR 		!then use DCR/Io
          IF(J .EQ. 21) EFF_DATA(I,  1) = PASSBAND * DCR_FSR * DCR_RSR 
          IF((J.GE.1).AND.(J.LE.19))THEN
	    IF((TCR_FSR*DCR_RSR ).GT. 0) THEN
	       EFF_DATA(I,145+J) = PASSBAND * TCR_FSR * DCR_RSR  		!TCR/Io only for OMR1-19
	    ELSE
	       EFF_DATA(I,145+J) = 1.0
	    ENDIF
          ENDIF
  30    CONTINUE
  40  CONTINUE

      RETURN
      END

