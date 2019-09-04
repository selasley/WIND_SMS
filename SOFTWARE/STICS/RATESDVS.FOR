CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      PROGRAM RATESDVS
C This program reads a WIND L1 data file and prints out H+, He++, and O+6
C rates for 4 different voltage steps. The rates are averaged over a given 
C number of Science Records.
C 
C Routines used from:
C  STICS_RATE_TABLE   Created 	3-NOV-1994	JOHN PAQUETTE
C                              21-FEB-1995      most recent version
C
C  CHOOSE_STICS_RATES Created  13-MAR-1995      K. CHOTOO
C
C  This program:      RATESDVS 22-APR-1995	K. CHOTOO
C
C Revision history:
C
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
      REAL*4  AVERAGE_RATES(0:31,1:20),	!Array for averaging rates per voltage step
     .        DVS_HITS(0:31),		!Counts # of hits for each voltage step
     .        RATE_AVG(20),     	!Stores average rates per spin summed over all voltage step
     .        SUM_RATES(0:31,1:12)	!Sums rates over Science Record interval
            
      INTEGER*4 DV_STEP(4)/14,17,20,25/,	!Different voltage steps printed out
     .          INTERVAL,               !Number of Science Records for interval
     .          II,			!Counter for RATE_AVG
     .          PICKS(5)/97,1,145,150,151/,	!Selection numbers for STICS rates
     .          Q,			!Increments for each new interval or data gap
     .          RATENUM/5/,		!Number of rates selected
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
      CHARACTER OUTFILENAME*90 

C
      RECORD /CORE/ SCI
      RECORD /HDBstr/ HDB
      RECORD /ST_RATE/ STICS_RATE
      RECORD /ST_PHA/ STICS_PHA
      RECORD /T/ START, END, TIME_TO_USE, TIME_LAST
C
C Get input L1 filename
C
      L1FILENAME = 'DATA_DIR:WI*L1*.DAT'
      CALL WIND_GET_FILENAME(L1FILENAME, STATUS)
      IF(STATUS.NE.1)STOP
C
C Ask user to choose time format
C
      USE_SC_CLOCK = 0
      Q = -1
C
C Prompt user for start and stop times
C
      TYPE 10
  10  FORMAT(' Enter Start time( 0/ means whole file) - Year,Month,Day,Hour,Min,Sec: '$) 
      ACCEPT*, START.YEAR,MONTH,DAYOFMONTH,START.HR,START.MIN,START.SEC
      IF(START.YEAR.NE.0)THEN
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
C Get Science Record interval
C
      WRITE(6,*)'ENTER SCIENCE RECORD INTERVAL'
      READ(5,*) INTERVAL
C
C Open L1 file for input, and read first SR, Core, HDB, and STICS RATES
C
      CALL WIND_L1_OPEN( L1_LUN, L1FILENAME, L1_FILE_HEADER, STATUS)
      IF (STATUS .NE. 1) GOTO 9010
      INQUIRE(FILE=L1FILENAME,NAME=FULL_L1_NAME)
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
      LISFIL(1:9) = 'STICS_DVS'
      LISFIL(JSAVE+1:JSAVE+11) = 'HHEO6O1.TBL'        

      OUTFILENAME = LISFIL
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

C    ******************************************************************************
C    *										  *
C    *                OPEN OUTPUT FILE AND WRITE HEADER INFORMATION               *
C    *										  *
C    ******************************************************************************
C
      OPEN(UNIT= 20, NAME=OUTFILENAME, DEFAULTFILE='TABLE_DIR:', FORM='FORMATTED', 
     &     TYPE='NEW', RECL=220)
      WRITE(20,30)
  30  FORMAT(' STICS SELECTED RATES')
      WRITE(20,35) FULL_L1_NAME(1:NAMELEN)
  35  FORMAT(T10,'Input Level 1 Filename: ',A)
C
      WRITE(20,37)FIFO,TGS,MODE_AT_START
  37  FORMAT(T10,'FIFO mode is ',A3,T30,'TGS mode is ',A3,T50,' Telemetry mode is ',A17/)
      WRITE(20,38)
  38  FORMAT(182('=')/,
     +       34X,'H+(keV/e)',23X,'He+2(keV/e)',21X,'He+1(keV/e)',
     +       22X,'O+6(keV/e)',23X,'O+1(keV/e)'/,
     +        1X,'YEAR ','DOY ','HH:MM:SC   ',
     +	      5(5X,'32',6X,'45',6X,'62',6X,'113'),/,182('='))

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
C
C In Normal mode sum Fe14+16 and Fe9+10
C
            ELSE
                IF((PICKS(K).EQ.158) .OR. (PICKS(K).EQ.162)) THEN
                   P = PICKS(K) - 1
		   DO 155 I = 0,1
		     STICS_RATES(P+1+I)=WIND_DECOMPRESS(STICS_RATE.EDB(J).RATE(P+I),COMP_CODE)
 155		   CONTINUE
                   STX_RATE(K) = STICS_RATES(P+1) + STICS_RATES(P+2)
                   GO TO 600
                ENDIF
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
	    
          DVS_HITS(VSTEP) = DVS_HITS(VSTEP) + 1.0		!Increment voltage step hit counter
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
C
C Average rates over interval. Average rates if END_TIME reached or at EOF.
C
 907  IF((L1_SR_HEADER.SR_COUNT .GE. SR_INTERVAL) .OR.
     +   (ENDFOUND) .OR. (STATUS .EQ. RMS$_EOF))THEN
         Q = Q + 1
C
C Initialize matrix to zeros
C
         DO 913 II = 1, RATENUM*4
           RATE_AVG(II) = 0
 913	 CONTINUE
         DO 915 K = 1, RATENUM
           DO 910 I = 0, 31
             AVERAGE_RATES(I,K) = 0
 910       CONTINUE
 915     CONTINUE
C
C Find average of each rate over interval
C
         DO 925 I = 0, 31
           IF(DVS_HITS(I) .EQ. 0) DVS_HITS(I) = 1.0             !to avoid dividing by 0
           DO 920 K = 1, RATENUM
             IF(RATEFLAG .EQ. 'H') THEN				!In High Bit rate, rates are in counts per spin
                AVERAGE_RATES(I,K) = SUM_RATES(I,K)/DVS_HITS(I)	!In Low Bit rate, rates are summed over two spins
             ELSE       				        !We want counts per spin, so divide by 2 in LBR.
                AVERAGE_RATES(I,K) = SUM_RATES(I,K)/DVS_HITS(I)/2.0
             ENDIF
 920       CONTINUE
 925     CONTINUE

C
C Put the average rate for the required voltage steps in a separate array
C
         II = 0
         DO 935 K = 1, RATENUM
	   DO 930 I = 1,4
              II = II + 1
              RATE_AVG(II) = AVERAGE_RATES(DV_STEP(I),K)
 930	   CONTINUE
 935     CONTINUE
C
C Write rates and interval end time to output file
C
         WRITE(20,940)TIME_TO_USE.YEAR,TIME_TO_USE.DOY,TIME_TO_USE.HR,
     +                TIME_TO_USE.MIN,TIME_TO_USE.SEC,
     +                (RATE_AVG(II), II=1,RATENUM*4)
 940     FORMAT(1X,I4,1X,I3,1X,I2.2,':',I2.2,':',I2.2,3X,20(1X,F7.2))
C
C Data gaps
C
         IF(L1_SR_HEADER.SR_COUNT .GT. SR_INTERVAL) THEN
            Q = Q + 1
            TIME_TO_USE.SEC = TIME_TO_USE.SEC + 3
            DO 950 II = 1, RATENUM*4
              RATE_AVG(II) = 9999.99
 950        CONTINUE
            WRITE(20,940)TIME_TO_USE.YEAR,TIME_TO_USE.DOY,TIME_TO_USE.HR,
     +                   TIME_TO_USE.MIN,TIME_TO_USE.SEC,
     +                   (RATE_AVG(II),II=1,RATENUM*4)
         ENDIF

         IF(ENDFOUND) GO TO 1000
         IF(STATUS.EQ.RMS$_EOF)GO TO 1000
         GO TO 50
      ENDIF !{L1_SR_HEADER.SR_COUNT...}

      IF (STATUS .NE. 1) GOTO 9020
         
      GO TO 100
C
C Close files and STOP
C
1000  CALL WIND_L1_CLOSE( L1_LUN, STATUS )      ! Close input file
      IF (STATUS .NE. 1) GOTO 9090
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
9090  TYPE *,'ERROR CLOSING L1 FILE; STATUS=',STATUS
      STOP
      END


