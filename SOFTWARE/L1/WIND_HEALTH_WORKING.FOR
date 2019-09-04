CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      PROGRAM WIND_HEALTH
C This program reads housekeeping (HK and HDB) data and prints the data out.
C Subcommutated data is printed out in tabular form, 1 table for every 64 science
C records. For various analog housekeeping items, an average, a minimum,  and a 
C maximum is found, and the number of times that the value is zero is counted.
C DPU status items are taken from the last science record.
C
C Created 	27-APR-1995     JOHN PAQUETTE
C
C This version:   1.6    2-NOV-1995
C
C Revision history:
C
C       12-MAY-1995      J.PAQUETTE     added the following at Bob Lundgren's request: 
C                                         1)do not count 0 values when finding min,max, 
C                                           or average values; changed FIND_STATS
C                                         2)change F16.6 formats to F12.2 for writing
C                                           data 
C                                         3)add commanded levels for certain items
C                                         4)add CHK # to output
C                                         5)add header to output file
C                                         6)list subcommutation counter in table output
C                                         7)corrected constants for some DPPS values 
C                                         8)count and list # zero values
C                                       also corrected a few errors in constants, variable
C                                       names, etc
C       17-MAY-1995     J.PAQUETTE      added various SC status and DPU status items
C       22-MAY-1995     J.PAQUETTE      added MASS housekeeping items
C       23-MAY-1995     J.PAQUETTE      only 1 page of subcommutated items
C       25-MAY-1995     J.PAQUETTE      added STICS housekeeping items
C	 2-NOV-1995	L. Bleau	added modifications made by Bob Lundgren:
C					give English reason for reset cause
C	 2-NOV-1995	L. Bleau	generate unique output filename, even
C					for NRT L1 files, which have a number
C					as an extension instead of a .DAT
C
CDEC$ IDENT  '1.6'
      INCLUDE	'INC_DIR:EDBDEF.INC/NOLIST'
      INCLUDE   'INC_DIR:L1FMT.INC/NOLIST'
      INCLUDE   '($RMSDEF)'
      INTEGER INCOMPLETE/0/, NCHAR, FIRST_SR, LAST_SR, MR(2, 0:9), BR0_BR1_BORDER 
      INTEGER*4 STATUS,L1_LUN/19/,MONTH,DAYOFMONTH,HK_COUNT,NUM_MF/0/,NUM_SR/0/, OFFSET/0/
      INTEGER*4 STICS_TRIGGER_MODE, SSD_1_THRESHOLD, SSD_2_THRESHOLD, SSD_3_THRESHOLD
      INTEGER*4 SWICS_TRIGGER_MODE, TAC_SLOPE, RESET_CAUSE
      REAL*4 POLY, SPIN_PERIOD, MASS_HPS_MEAS_V, MASS_STOP_MCP_MEAS_V, MASS_START_MCP_MEAS_V
      REAL*4 STICS_MCPPS_MEAS_V(0:5), STICS_TOFPS_MEAS_V, SWICS_PAPS_MEAS_V, SWICS_START_MCPPS_MEAS_V, 
     &       SWICS_STOP_MCPPS_MEAS_V, SWICS_DPPS_MEAS_V
      REAL*8 OLD_TIME
      CHARACTER VERSION*5, RUN_TIME*8, RUN_DATE*9
      CHARACTER*1 COMP_CODE(2), ASCII_NULL
      CHARACTER*3 SWICS_COVER/'OFF'/, SWICS_HEATER/'OFF'/, STICS_COVER/'OFF'/, STICS_HEATER/'OFF'/, 
     &            MASS_COVER/'OFF'/, MASS_MAIN/'OFF'/, STICS_MAIN/'OFF'/, SWICS_MAIN/'OFF'/,
     &            MASS_HPS/'OFF'/, MASS_STOP_MCP/'OFF'/, MASS_START_MCP/'OFF'/, MASS_COM_ADPS/'OFF'/,
     &            MASS_COM_DPPS /'OFF'/, STICS_MCPPS(0:5)/6*'OFF'/, STICS_TOFPS/'OFF'/, 
     &            STICS_PLUS_DPPS/'OFF'/, STICS_MINUS_DPPS/'OFF'/, SSD_1_BIAS /'ON '/,
     &            SSD_2_BIAS/'ON '/, SSD_3_BIAS/'ON '/, ENERGY_PREAMP_1/'OFF'/, ENERGY_PREAMP_2/'OFF'/,
     &            ENERGY_PREAMP_3/'OFF'/, SWICS_DPPS_ENABLE/'OFF'/, SWICS_PAPS_ENABLE/'OFF'/,
     &            SWICS_START_MCPPS_ENABLE/'OFF'/, SWICS_STOP_MCPPS_ENABLE/'OFF'/
      CHARACTER*6 STICS_CONFIG/'Normal'/, STICS_SNAPSHOT/'Normal'/, SWICS_CONFIG/'Normal'/, 
     &            SWICS_SNAPSHOT/'Normal'/, MASS_CONFIG/'Normal'/, MASS_SNAPSHOT/'Normal'/,
     &            LOAD_CONFIG/'Normal'/, LOAD_SNAPSHOT/'Normal'/
      CHARACTER*16 MASS_SUN_SECTOR_DEFINITIONS, SWICS_SUN_SECTOR_DEFINITIONS
      CHARACTER*80 L1FILENAME, OUTPUTFILENAME, FULL_L1_NAME, DEVDIR
      CHARACTER*13 RESET_CAUSE_TEXT(0:3)/
     &            'Power on',
     &            'Latchup',
     &            'Under voltage',
     &            'Watchdog'/
      CHARACTER*132 LINE
      INTEGER UNIT_TABLE(0:2,3)/52,53,54,53,52,54,54,52,53/
      LOGICAL CODE_C,CODE_C2,STARTFOUND,ENDFOUND,COMPARE_TIME_GE,GOODSPIN
      LOGICAL RANGE_BIT_SET, FIRSTFOUND/.FALSE./
      INTEGER I,J,K,L,N,NN,CNTR,ITEMP,NAMELEN,DECOM_CNTR, VALUE, VALUE2, KK, MM, NEW_HK
      INTEGER DEVDIR_LEN, NCH, IPOS
      REAL*4  SWICS_DPPS_OUTV(0:59), MASS_ADPS_OUTV(0:59), MASS_DPPS_OUTV(0:59),       ! Subcommutated
     &        STICS_PLUSDPPS_OUTV(0:59), STICS_MINUSDPPS_OUTV(0:59)                    ! quantities
      INTEGER SWICS_PAPS_CL, SWICS_Start_MCPPS_CL, SWICS_Stop_MCPPS_CL, MASS_HPS_CL,
     &        MASS_Start_MCPPS_CL, MASS_Stop_MCPPS_CL, STICS_MCPPS_0_CL, STICS_MCPPS_1_CL,
     &        STICS_MCPPS_2_CL, STICS_MCPPS_3_CL, STICS_MCPPS_4_CL, STICS_MCPPS_5_CL,
     &        STICS_TOFPS_MCPPS_6_CL
      REAL*4  SWICS_PAPS_CV, SWICS_Start_MCPPS_CV, SWICS_Stop_MCPPS_CV, MASS_HPS_CV,
     &        MASS_Start_MCPPS_CV, MASS_Stop_MCPPS_CV, STICS_MCPPS_0_CV, STICS_MCPPS_1_CV,
     &        STICS_MCPPS_2_CV, STICS_MCPPS_3_CV, STICS_MCPPS_4_CV, STICS_MCPPS_5_CV,
     &        STICS_TOFPS_MCPPS_6_CV
C
      STRUCTURE/HOUSEKEEPING_STATS/
        REAL MIN/255./,MAX/1./,AVER
        INTEGER NZERO/0/
      END STRUCTURE
C
      RECORD /CORE/ SCI
      RECORD /HDBstr/ HDB,LAST_GOOD_HDB
      RECORD /ST_RATE/ STICS_RATE
      RECORD /ST_PHA/ STICS_PHA
      RECORD /HKEEP/ HK
      RECORD /T/ START, END, FIRST, LAST
      RECORD /HOUSEKEEPING_STATS/ DPU_TEMP, SWICS_GET, MASS_GET, STICS_SSD_TEMP, STICS_AEH_TEMP,
     &                            DPU_28V, DPU_5P2V_S, DPU_5P2V_P, DPU_10V, DPU_TEMP_PIFA,
     &                            DPU_TEMP_CTLW, SWICS_PLUS28V_PRIM_CURR, SWICS_PLUS5V,
     &                            SWICS_PLUS5I, SWICS_MINUS5V, SWICS_PLUS8V, SWICS_PLUS8I,
     &                            SWICS_20V, SWICS_20I, SWICS_LVPS_28V, SWICS_DPPS_28I,
     &                            SWICS_PAPS_28I, SWICS_PAPS_OUTV, SWICS_PLUS5V_PR,
     &                            SWICS_PLUS5I_PR, SWICS_MINUS5V_PR, SWICS_MINUS5I_PR,
     &                            SWICS_PLUS12V_PR, SWICS_PLUS12I_PR, SWICS_MINUS12V_PR,
     &                            SWICS_MINUS12I_PR, SWICS_SSD_BIASV_PR, SWICS_START_MCPPSV_PR,
     &                            SWICS_STOP_MCPPSV_PR, SWICS_SSD_TEMP_PR, SWICS_BET_PR,
     &                            MASS_PLUS28I, MASS_PLUS5V, MASS_PLUS5I, MASS_MINUS5V,
     &                            MASS_PLUS8V, MASS_LVPS28V, MASS_ADPS28V, MASS_DPPS28I,
     &                            MASS_AET, MASS_PLUS5V_AR, MASS_PLUS5I_AR, MASS_MINUS5V_AR,
     &                            MASS_MINUS5I_AR, MASS_PLUS6V_AR, MASS_PLUS6I_AR,
     &                            MASS_PLUS12V_AR, MASS_MINUS12V_AR, MASS_LVPS28V_AR,
     &                            MASS_HPS_MCPPS_28I_AR, MASS_HPS_OUTV_AR, MASS_START_MCPPSV_AR,
     &                            MASS_STOP_MCPPSV_AR, STICS_28I, STICS_PLUS5V, STICS_MINUS5V,
     &                            STICS_PLUS6V, STICS_MINUS6V,STICS_PLUS12V, STICS_MINUS12V,
     &                            STICS_MCPPS28I, STICS_DPPS28I, STICS_SSD_BIASV,
     &                            STICS_MCPPS0_OUTV, STICS_MCPPS1_OUTV, STICS_MCPPS2_OUTV,
     &                            STICS_MCPPS3_OUTV, STICS_MCPPS4_OUTV, STICS_MCPPS5_OUTV,
     &                            STICS_TOFPS_OUTV, STICS_T1_AEHT, STICS_T2_FHOPT, STICS_T3_SSDT,
     &                            STICS_T4_BPT
C
      ASCII_NULL = CHAR(0)
C
C Get input L1 filename
C
      L1FILENAME = 'DATA_DIR:WI*L1*.DAT'
      CALL WIND_GET_FILENAME(L1FILENAME, STATUS)
      IF(STATUS.NE.1)STOP
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
C Open L1 file for input, and read first SR, Core, and STICS records.
C
      CALL WIND_L1_OPEN( L1_LUN, L1FILENAME, L1_FILE_HEADER, STATUS)
      IF (STATUS .NE. 1) GOTO 9010
      INQUIRE(FILE=L1FILENAME,NAME=FULL_L1_NAME)
      NAMELEN = INDEX(FULL_L1_NAME,' ') - 1
      IF (NAMELEN .LT. 1) NAMELEN=LEN(FULL_L1_NAME)
  13  CALL WIND_L1_READ_SR( L1_LUN, CNTR, L1_SR_HEADER, STATUS )
      IF(STATUS.EQ.RMS$_EOF)GO TO 900
      IF (STATUS .NE. 1) GOTO 9020
      IF(L1_SR_HEADER.NVALID.NE.60)THEN
        INCOMPLETE = INCOMPLETE + 1
      ELSE
        IF(.NOT.FIRSTFOUND)THEN
          FIRST_SR = L1_SR_HEADER.SR_COUNT
          FIRST = HDB.SC_TIME
          FIRSTFOUND=.TRUE.
        ENDIF
        LAST_SR = L1_SR_HEADER.SR_COUNT
        LAST = HDB.SC_TIME
      ENDIF
      CALL WIND_L1_GET_BASIC( L1_LUN, SCI, STATUS)
C
      IF (STATUS .NE. 1) GOTO 9030
C
C Construct output filename
C
C
C Create output file each time to keep info from diffrent runs separate
C Construct output filename from the L1 filename and make it reside in the
C current directory.  Change the file extension to .DMP for most
C cases.  For NRT data, where the extension is not .DAT but serves to
C distinguish different data sets, make the extension part of the filename (see
C case 2 below) and add .DMP as the new extension.
C
C 1)  WI_L1_SMS_yyyymmdd_Vvv.DAT   WI_L1_SMS_yyyymmdd_Vvv.DMP
C 2)  WI_L1_SMS_yyyymmdd_Vvv.kkkk  WI_L1_SMS_yyyymmdd_Vvv_kkkk.DMP
C 3)  yymmdd_Vvv.DAT               yymmdd_Vvv.DMP
C
	  OUTPUTFILENAME = L1FILENAME
	  IPOS = INDEX( OUTPUTFILENAME, ';' )		! strip off version #
	  IF (IPOS .NE. 0) OUTPUTFILENAME(IPOS:) = ' '
	  NCH = INDEX( OUTPUTFILENAME, ' ') - 1		! get # of chars
	  IF (IPOS .EQ. -1) NCH = LEN(OUTPUTFILENAME)
	  IPOS = INDEX( OUTPUTFILENAME, ']' )		! extract device, directory
	  IF (IPOS .NE. 0) THEN
	    DEVDIR = OUTPUTFILENAME(1:IPOS)		! save device/directory
	    DEVDIR_LEN = IPOS
	    OUTPUTFILENAME = OUTPUTFILENAME(IPOS+1:)
	    NCH = NCH - IPOS
	  ELSE
	    DEVDIR = '[]'
	    DEVDIR_LEN = 2
	  ENDIF
	  IPOS = INDEX( OUTPUTFILENAME, '.' )		! locate file type
	  IF (IPOS .EQ. 0) THEN				! none, add one
	    OUTPUTFILENAME(NCH+1:NCH+4) = '.DMP'
	    IPOS = NCH + 1
	    NCH = NCH + 4
	  ELSE						! if .DAT just replace
	    IF (OUTPUTFILENAME(IPOS:IPOS+3) .EQ. '.DAT' .AND. IPOS+3 .EQ. NCH) THEN
	      OUTPUTFILENAME(IPOS:IPOS+3) = '.DMP'
	      NCH = IPOS + 3
	    ELSE					! not .DAT, keep file type
	      OUTPUTFILENAME(IPOS:IPOS) = '_'		! combine it with filename
	      OUTPUTFILENAME(NCH+1:NCH+4) = '.DMP'	! and use standard file type
	      IPOS = NCH + 1
	      NCH = NCH + 4
	    ENDIF
	  ENDIF
c	type *,'report name = "',outputfilename(1:nch),'"'
c	type *,'device/direcory = "',devdir(1:devdir_len),'"'
C
C      OUTPUTFILENAME = L1FILENAME
C      KK = INDEX(OUTPUTFILENAME, ']')
C      MM = INDEX(OUTPUTFILENAME(KK+1:), '.')
C      IF (MM .NE. 0) THEN
C        OUTPUTFILENAME(KK+MM:) = '.DMP'
C      ELSE
C        MM = INDEX(OUTPUTFILENAME(KK+1:), ' ')
C        OUTPUTFILENAME(KK+MM:) = '.DMP'
C      ENDIF
C      NCH = KK+MM+3
C
C Open output file
C
      OPEN(UNIT=11, NAME=OUTPUTFILENAME(1:NCH), STATUS='NEW')
C
C Find first good spin, get year, and DOY from it.
C
      N=0
      GOODSPIN=.FALSE.
      DO 14 WHILE(.NOT.GOODSPIN)
        K=N/32+1
        NN=MOD(N,31)
        GOODSPIN=BTEST(L1_SR_HEADER.PRESENT(K),NN)
        N=N+1             
        IF(N.GT.59)GOTO 13 ! This SCI-REC all bad; get next
  14  CONTINUE
c      N=N-1
C
C Write dummy header as placeholder
C
      WRITE(11,50)
 50   FORMAT(15X,'WIND SMS Daily Health Check - WIND_HEALTH Output')
      WRITE(11,52)
 52   FORMAT(10X,58('-'))
      WRITE(11,55)L1FILENAME
 55   FORMAT(5X,'Input File:',4X,A)
      WRITE(11,57)VERSION, RUN_TIME, RUN_DATE
 57   FORMAT(5X,'Program Name: WIND_HEALTH       Version:',A5,7X,'Run Date: ',A8,1X,A9)
      WRITE(11,60) FIRST_SR, FIRST.HR, FIRST.MIN, FIRST.SEC
 60   FORMAT(5X,'First complete science record: ',I8,7X,'Time: ',I2,':',I2.2,':',I2.2)
      WRITE(11,65) LAST_SR, LAST.HR, LAST.MIN, LAST.SEC
 65   FORMAT(5X,'Last complete science record:  ',I8,7X,'Time: ',I2,':',I2.2,':',I2.2)
      WRITE(11,70)CNTR-INCOMPLETE,INCOMPLETE
 70   FORMAT(5X,'Number of complete science records: ',I3,7X,'Number incomplete: ',I3)
      WRITE(11,75)
 75   FORMAT()
C
C Set flags for start and stop
C
      STARTFOUND=.FALSE.
      ENDFOUND=.FALSE.
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
      IF(IAND(HDB.DAT(5),3).EQ.3)OFFSET = IAND(HDB.DAT(166), 255)
      IF(HDB.QUAL.EQ.0)LAST_GOOD_HDB = HDB
C
C Get Fixed Housekeeping Data
C
      CALL WIND_L1_GET_HK( L1_LUN, HK, HK_COUNT,STATUS)
      IF (STATUS .NE. 1) GOTO 9060
C
C Get STICS rates (need the last byte)
C
      CALL WIND_L1_GET_STICS( L1_LUN, STICS_RATE, STICS_PHA, STATUS )
      IF (STATUS .NE. 1) GOTO 9080
C
C Compare time read to start time. If the time read is greater than or equal to the user-
C specified start time, then begin processing data.
C
      IF(.NOT.STARTFOUND)THEN
        STARTFOUND=COMPARE_TIME_GE(SCI.EDB(0).SC_TIME,START)
      ENDIF
      IF(STARTFOUND)THEN
        NUM_SR = NUM_SR + 1
C
C Compare time read to end time. If time read is greater than the user-specified end time,
C we're done. Go to the part of the code that closes files and STOP's program.
C
        ENDFOUND=COMPARE_TIME_GE(SCI.EDB(0).SC_TIME,END)
        IF(ENDFOUND)GO TO 900
C
C The first five quantities are formatted by the space craft, and are only available in the
C HK. The loop below loops over HK's
C
        NEW_HK = 1
        IF( HK.HKSET(1).EPOCH .EQ. OLD_TIME )NEW_HK = 2
        DO 200 I = NEW_HK, HK_COUNT
C
          NUM_MF = NUM_MF + 1
C
C Get DPU Temp (Passive)
C
          VALUE = HK.HKSET(I).HK(1).BVAL
          VALUE2 = IAND(VALUE, 255)
          CALL FIND_STATS(VALUE2,DPU_TEMP)
C
C Get SWICS Gnd Electronics Temp (Passive)
C
          VALUE  = HK.HKSET(I).HK(2).BVAL
          VALUE2 = IAND(VALUE, 255)
          CALL FIND_STATS(VALUE2,SWICS_GET)
C
C Get MASS Gnd Electronics Temp (Passive)
C
          VALUE = HK.HKSET(I).HK(3).BVAL
          VALUE2 = IAND(VALUE, 255)
          CALL FIND_STATS(VALUE2,MASS_GET)
C
C Get STICS SSD Temp (Passive)
C
          VALUE = HK.HKSET(I).HK(4).BVAL
          VALUE2 = IAND(VALUE, 255)
          CALL FIND_STATS(VALUE2,STICS_SSD_TEMP)
C
C Get STICS AE Housing Temp (Passive)
C
          VALUE = HK.HKSET(I).HK(5).BVAL
          VALUE2 = IAND(VALUE, 255)
          CALL FIND_STATS(VALUE2,STICS_AEH_TEMP)
C
 200    CONTINUE
C
C Get DPU 28 V Primary Current
C
        VALUE = HDB.DAT(215)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2,DPU_28V)
C
C Get DPU 5.2 V (Switched) Voltage
C
        VALUE = HDB.DAT(216)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2,DPU_5P2V_S)
C
C Get DPU 5.2 V (Permanent) Voltage
C
        VALUE = HDB.DAT(217)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2,DPU_5P2V_P)
C
C Get DPU 10 V Voltage
C
        VALUE = HDB.DAT(218)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2,DPU_10V)
C
C Get DPU Temperature, PIF_A
C
        VALUE = HDB.DAT(219)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2,DPU_TEMP_PIFA)
C
C Get DPU Temperature, CTL_W
C
        VALUE = HDB.DAT(220)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, DPU_TEMP_CTLW)
C
C Get SWICS +28 V Primary Current (Gnd Ref)
C
        VALUE = HDB.DAT(90)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, SWICS_PLUS28V_PRIM_CURR)
C
C Get SWICS +5 V Voltage (Gnd Ref)
C
        VALUE = HDB.DAT(94)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, SWICS_PLUS5V)
C
C Get SWICS +5 V Current (Gnd Ref)
C
        VALUE = HDB.DAT(93)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, SWICS_PLUS5I)
C
C Get SWICS -5 V Voltage (Gnd Ref)
C
        VALUE = HDB.DAT(95)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, SWICS_MINUS5V)
C
C Get SWICS +8 V Voltage (Gnd Ref)
C
        VALUE = HDB.DAT(92)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, SWICS_PLUS8V)
C
C Get SWICS +8 V Current (Gnd Ref)
C
        VALUE = HDB.DAT(91)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, SWICS_PLUS8I)
C
C Get SWICS 20 V Voltage (Gnd Ref)
C
        VALUE = HDB.DAT(67)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, SWICS_20V)
C
C Get SWICS 20 V Current (Gnd Ref)
C
        VALUE = HDB.DAT(66)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, SWICS_20I)
C
C Get SWICS LVPS 28 V Voltage (Gnd Ref)
C
        VALUE = HDB.DAT(69)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, SWICS_LVPS_28V)
C
C Get SWICS DPPS 28 V Current (Gnd Ref)
C
        VALUE = HDB.DAT(71)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, SWICS_DPPS_28I)
C
C Get SWICS PAPS 28 V Current (Gnd Ref)
C
        VALUE = HDB.DAT(68)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, SWICS_PAPS_28I)
C
C Get SWICS PAPS Output Voltage (Gnd Ref)
C
        VALUE = HDB.DAT(70)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, SWICS_PAPS_OUTV)
C
C Get SWICS +5 V Voltage (PAPS Ref)
C
        VALUE = HDB.DAT(73)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, SWICS_PLUS5V_PR)
C
C Get SWICS +5 V Current (PAPS Ref)
C
        VALUE = HDB.DAT(74)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, SWICS_PLUS5I_PR)
C
C Get SWICS -5 V Voltage (PAPS Ref)
C
        VALUE = HDB.DAT(75)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, SWICS_MINUS5V_PR)
C
C Get SWICS -5 V Current (PAPS Ref)
C
        VALUE = HDB.DAT(76)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, SWICS_MINUS5I_PR)
C
C Get SWICS +12 V Voltage (PAPS Ref)
C
        VALUE = HDB.DAT(82)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, SWICS_PLUS12V_PR)
C
C Get SWICS +12 V Current (PAPS Ref)
C
        VALUE = HDB.DAT(83)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, SWICS_PLUS12I_PR)
C
C Get -12 V Voltage (PAPS Ref)
C
        VALUE = HDB.DAT(84)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, SWICS_MINUS12V_PR)
C
C Get SWICS -12 V Current (PAPS Ref)
C
        VALUE = HDB.DAT(85)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, SWICS_MINUS12I_PR)
C
C Get SWICS SSD Bias Voltage (PAPS Ref)
C
        VALUE = HDB.DAT(81)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, SWICS_SSD_BIASV_PR)
C
C Get SWICS Start MCPPS Voltage (PAPS Ref)
C
        VALUE = HDB.DAT(77)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, SWICS_START_MCPPSV_PR)
C
C Get SWICS Stop MCPPS Voltage (PAPS Ref)
C
        VALUE = HDB.DAT(78)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, SWICS_STOP_MCPPSV_PR)
C
C Get SWICS SSD Temp (PAPS Ref)
C
        VALUE = HDB.DAT(79)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, SWICS_SSD_TEMP_PR)
C
C Get SWICS Bubble Elect Temp, (PAPS Ref)
C
        VALUE = HDB.DAT(80)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, SWICS_BET_PR)
C
C Get MASS +28 V Primary Current (Gnd Ref)
C
        VALUE = HDB.DAT(98)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, MASS_PLUS28I)
C
C Get MASS +5 V Voltage (Gnd Ref)
C
        VALUE = HDB.DAT(103)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, MASS_PLUS5V)
C
C Get MASS +5 V Current (Gnd Ref)
C
        VALUE = HDB.DAT(97)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, MASS_PLUS5I)
C
C Get MASS -5 V Voltage (Gnd Ref)
C
        VALUE = HDB.DAT(104)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, MASS_MINUS5V)
C
C Get MASS +8 V Voltage (Gnd Ref)
C
        VALUE = HDB.DAT(102)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, MASS_PLUS8V)
C
C Get MASS LVPS 28 V Voltage (Gnd Ref)
C
        VALUE = HDB.DAT(101)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, MASS_LVPS28V)
C
C Get MASS ADPS 28 V Current (Gnd Ref)
C
        VALUE = HDB.DAT(100)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, MASS_ADPS28V)
C
C Get MASS DPPS 28 V Current (Gnd Ref)
C
        VALUE = HDB.DAT(99)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, MASS_DPPS28I)
C
C Get MASS Analog Electronics Temp (Gnd Ref)
C
        VALUE = HDB.DAT(105)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, MASS_AET)
C
C Get MASS +5 V Voltage (ADPS Ref)
C
        VALUE = HDB.DAT(125)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, MASS_PLUS5V_AR)
C
C Get MASS +5 V Current (ADPS Ref)
C
        VALUE = HDB.DAT(126)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, MASS_PLUS5I_AR)
C
C Get MASS -5 V Voltage (ADPS Ref)
C
        VALUE = HDB.DAT(128)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, MASS_MINUS5V_AR)
C
C Get MASS -5 V Current (ADPS Ref)
C
        VALUE = HDB.DAT(127)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, MASS_MINUS5I_AR)
C
C Get MASS +6 V Voltage (ADPS Ref)
C
        VALUE = HDB.DAT(123)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, MASS_PLUS6V_AR)
C
C Get MASS +6 V Current (ADPS Ref)
C
        VALUE = HDB.DAT(124)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, MASS_PLUS6I_AR)
C
C Get MASS +12 V Voltage (ADPS Ref)
C
        VALUE = HDB.DAT(122)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, MASS_PLUS12V_AR)
C
C Get MASS -12 V Voltage (ADPS Ref)
C
        VALUE = HDB.DAT(129)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, MASS_MINUS12V_AR)
C
C Get MASS LVPS 28 V Voltage (ADPS Ref)
C
        VALUE = HDB.DAT(121)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, MASS_LVPS28V_AR)
C
C Get MASS HPS & MCPPS 28V I (ADPS Ref)
C
        VALUE = HDB.DAT(118)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, MASS_HPS_MCPPS_28I_AR)
C
C Get MASS HPS Output Voltage (ADPS Ref)
C
        VALUE = HDB.DAT(117)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, MASS_HPS_OUTV_AR)
C
C Get MASS Start MCPPS Voltage (ADPS Ref)
C
        VALUE = HDB.DAT(119)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, MASS_START_MCPPSV_AR)
C
C Get MASS Stop MCPPS Voltage (ADPS Ref)
C
        VALUE = HDB.DAT(120)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, MASS_STOP_MCPPSV_AR)
C
C Get STICS 28 V Primary Current
C
        VALUE = HDB.DAT(49)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, STICS_28I)
C
C Get STICS +5 V Voltage
C
        VALUE = HDB.DAT(45)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, STICS_PLUS5V)
C
C Get STICS -5 V Voltage
C
        VALUE = HDB.DAT(46)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, STICS_MINUS5V)
C
C Get STICS +6 V Voltage
C
        VALUE = HDB.DAT(43)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, STICS_PLUS6V)
C
C Get STICS -6 V Voltage
C
        VALUE = HDB.DAT(44)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, STICS_MINUS6V)
C
C Get STICS +12 V Voltage
C
        VALUE = HDB.DAT(47)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, STICS_PLUS12V)
C
C Get STICS -12 V Voltage
C
        VALUE = HDB.DAT(50)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, STICS_MINUS12V)
C
C Get STICS MCPPS 28 V Current
C
        VALUE = HDB.DAT(34)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, STICS_MCPPS28I)
C
C Get STICS DPPS 28 V Current
C
        VALUE = HDB.DAT(35)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, STICS_DPPS28I)
C
C Get STICS SSD Bias Voltage
C
        VALUE = HDB.DAT(48)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, STICS_SSD_BIASV)
C
C Get STICS MCPPS 0 Output Voltage
C
        VALUE = HDB.DAT(27)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, STICS_MCPPS0_OUTV)
C
C Get STICS MCPPS 1 Output Voltage
C
        VALUE = HDB.DAT(28)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, STICS_MCPPS1_OUTV)
C
C Get STICS MCPPS 2 Output Voltage
C
        VALUE = HDB.DAT(29)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, STICS_MCPPS2_OUTV)
C
C Get STICS MCPPS 3 Output Voltage
C
        VALUE = HDB.DAT(30)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, STICS_MCPPS3_OUTV)
C
C Get STICS MCPPS 4 Output Voltage
C
        VALUE = HDB.DAT(31)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, STICS_MCPPS4_OUTV)
C
C Get STICS MCPPS 5 Output Voltage
C
        VALUE = HDB.DAT(32)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, STICS_MCPPS5_OUTV)
C
C Get STICS TOFPS (MCPPS 6) Output Voltage
C
        VALUE = HDB.DAT(33)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, STICS_TOFPS_OUTV)
C
C Get STICS T1, AE Housing Temp
C
        VALUE = HDB.DAT(39)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, STICS_T1_AEHT)
C
C Get STICS T2, Fairing/HOP Temp
C
        VALUE = HDB.DAT(40)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, STICS_T2_FHOPT)
C
C Get STICS T3, SSD Temp
C
        VALUE = HDB.DAT(41)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, STICS_T3_SSDT)
C
C Get STICS T4, Back Panel Temp
C
        VALUE = HDB.DAT(42)
        VALUE2 = IAND(VALUE, 255)
        CALL FIND_STATS(VALUE2, STICS_T4_BPT)
C
C Get commanded levels. Don't need stats for these, as they shouldn't be 
C changing; just find last value which is nonzero( a data gap could give a 
C zero value).
C
        SWICS_PAPS_CL = IAND(HDB.DAT(61), 255)
        SWICS_PAPS_CV = 190.96 - 117.15 * SWICS_PAPS_CL 
        SWICS_Start_MCPPS_CL = IAND(HDB.DAT(62), 255)
        SWICS_Start_MCPPS_CV = 1.129 + 16.303 * SWICS_Start_MCPPS_CL 
        SWICS_Stop_MCPPS_CL = IAND(HDB.DAT(63), 255)
        SWICS_Stop_MCPPS_CV = 1.564 + 12.181 * SWICS_Stop_MCPPS_CL 
        MASS_HPS_CL = IAND(HDB.DAT(110), 255)
        MASS_HPS_CV = -19.5 + 132.08 * MASS_HPS_CL 
        MASS_Start_MCPPS_CL = IAND(HDB.DAT(112), 255)
        MASS_Start_MCPPS_CV = 12.2 + 18.294 * MASS_Start_MCPPS_CL 
        MASS_Stop_MCPPS_CL = IAND(HDB.DAT(114), 255)
        MASS_Stop_MCPPS_CV = 0.2 + 13.452 * MASS_Stop_MCPPS_CL 
        STICS_MCPPS_0_CL = IAND(HDB.DAT(17), 255)
        STICS_MCPPS_0_CV = -11.081 + 12.535 * STICS_MCPPS_0_CL 
        STICS_MCPPS_1_CL = IAND(HDB.DAT(18), 255)
        STICS_MCPPS_1_CV = -7.7568 + 12.535 * STICS_MCPPS_1_CL 
        STICS_MCPPS_2_CL = IAND(HDB.DAT(19), 255)
        STICS_MCPPS_2_CV = -9.4054 + 12.524 * STICS_MCPPS_2_CL
        STICS_MCPPS_3_CL = IAND(HDB.DAT(20), 255)
        STICS_MCPPS_3_CV  = -7.1081 + 12.526 * STICS_MCPPS_3_CL 
        STICS_MCPPS_4_CL = IAND(HDB.DAT(21), 255)
        STICS_MCPPS_4_CV = -8.5135 + 12.541* STICS_MCPPS_4_CL 
        STICS_MCPPS_5_CL = IAND(HDB.DAT(22), 255)
        STICS_MCPPS_5_CV = -10.081 + 12.565 * STICS_MCPPS_5_CL
        STICS_TOFPS_MCPPS_6_CL  = IAND(HDB.DAT(23), 255)
        STICS_TOFPS_MCPPS_6_CV = -3.0764 + 4.5362 * STICS_TOFPS_MCPPS_6_CL
C
C Subcommutated Quantities (i.e. those that come out only once per 64
C HDB's) are to be put into tables. Min, Max, and Average are not calculated
C for them.
C
C The last 5 bits of the 5th byte of the HDB contain the number to be used as
C a decommutation counter; If the counter exceeds 59, these bytes are undefined,
C so do the following only if DECOM_CNTR is in the range 0-59
C
        DECOM_CNTR = IAND(HDB.DAT(5), 63)
        IF(DECOM_CNTR.LE.59)THEN
C
C For SWICS DPPS Output Voltage (Gnd Ref), we need to use bit 1 of hdb byte 89 to
C determine the range
C
          VALUE = HDB.DAT(89)
          VALUE2 = IAND(VALUE, 255)
          RANGE_BIT_SET = BTEST(VALUE2, 1)
C
          VALUE = HDB.DAT(72)
          VALUE2 = IAND(VALUE, 255)
          IF(DECOM_CNTR.LE.26)THEN
C          IF(RANGE_BIT_SET )THEN
            SWICS_DPPS_OUTV(DECOM_CNTR) = -5.317E1 + 1.737E1*FLOAT(VALUE2)
          ELSE
            SWICS_DPPS_OUTV(DECOM_CNTR) = 7.858E-1 +  1.629*FLOAT(VALUE2)
          ENDIF
C
C Get MASS ADPS Output Voltage (Gnd Ref)
C
          VALUE = HDB.DAT(106)
          VALUE2 = IAND(VALUE, 255)
          MASS_ADPS_OUTV(DECOM_CNTR) = -6.634E3 + 3.4185E1*FLOAT(VALUE2)
C
C For MASS DPPS Output Voltage (Gnd Ref), we need to use bit 5 of hdb byte 107 to
C determine the range
C
          VALUE = HDB.DAT(107)
          VALUE2 = IAND(VALUE,255)
          RANGE_BIT_SET = BTEST(VALUE2, 5)
C
          VALUE = HDB.DAT(96)
          VALUE2 = IAND(VALUE, 255)
          IF(DECOM_CNTR.LE.25)THEN
C          IF(RANGE_BIT_SET )THEN
            MASS_DPPS_OUTV(DECOM_CNTR) = 2.626 - 8.056*FLOAT(VALUE2)
          ELSE
            MASS_DPPS_OUTV(DECOM_CNTR) = -2.387 - 1.612*FLOAT(VALUE2)
          ENDIF
C
C Get STICS +DPPS Output Voltage (12 bit field in bytes 36 and 37 of the HDB)
C
          VALUE = IAND(HDB.DAT(36), 255)
          VALUE = ISHFT(VALUE, 4)
          VALUE2 = IAND(HDB.DAT(37), 240)
          VALUE2 = ISHFT(VALUE2, -4)
          VALUE = IOR(VALUE,VALUE2)
          STICS_PLUSDPPS_OUTV(DECOM_CNTR) = -2.453E1 + 2.9562*FLOAT(VALUE)
C
C Get STICS -DPPS Output Voltage (12 bit field in bytes 37 and 38 of the HDB)
C
          VALUE = HDB.DAT(38)
          VALUE = IAND(VALUE, 255)
          VALUE2 = IAND(HDB.DAT(37), 15)
          VALUE2 = ISHFT(VALUE2, 8)
          VALUE = IOR(VALUE,VALUE2)
          STICS_MINUSDPPS_OUTV(DECOM_CNTR) = -2.422E1 - 2.9791*FLOAT(VALUE)
        ENDIF
      ENDIF
C
C Read next Science Record
C
      CALL WIND_L1_READ_SR( L1_LUN, CNTR, L1_SR_HEADER, STATUS )
      IF(STATUS.EQ.RMS$_EOF)GO TO 900
      IF (STATUS .NE. 1) GOTO 9020
      IF(L1_SR_HEADER.NVALID.NE.60)THEN
        INCOMPLETE = INCOMPLETE + 1
      ELSE
        IF(.NOT.FIRSTFOUND)THEN
          FIRST_SR = L1_SR_HEADER.SR_COUNT
          FIRST = HDB.SC_TIME
          FIRSTFOUND=.TRUE.
        ENDIF
        LAST_SR = L1_SR_HEADER.SR_COUNT
        LAST = HDB.SC_TIME
      ENDIF
C
      OLD_TIME = HK.HKSET(HK_COUNT).EPOCH
      GO TO 100
C
C When the program reaches this point, we have a full set of subcommutated values
C (or, at any rate, as close to a full set as we're going to get). Print them out
C in tabular form.
C
 900  CALL TIME(RUN_TIME)
      CALL DATE(RUN_DATE)
      CALL GET_IMAGE_INFO('VERS', VERSION, NCHAR)
      WRITE(11,500)
 500  FORMAT(/,5X,'SUBCOMM|   SWICS DPPS   |   MASS ADPS    |   MASS DPPS    |  STICS +DPPS   |  STICS -DPPS   |')
      WRITE(11,510)
 510  FORMAT(5X,'  NUM  | Output Voltage | Output Voltage | Output Voltage | Output Voltage | Output Voltage |')
      WRITE(11,520)
 520  FORMAT(5X,93('-'))
      DO 600 I=0,59
        WRITE(11,550) I, SWICS_DPPS_OUTV(I), MASS_ADPS_OUTV(I), MASS_DPPS_OUTV(I), 
     &                    STICS_PLUSDPPS_OUTV(I), STICS_MINUSDPPS_OUTV(I)
 550    FORMAT(7X,I2,3X,'|',5(2X,F12.2,2X,'|'))
 600  CONTINUE      
      WRITE(11,520)
C
C Write header
C
      WRITE(11,950)
 950  FORMAT('1','CHK         Parameter Identification            Minimum     Maximum     Average    # 0''s ',
     &       '  Commanded Level    Commanded Voltage')
      WRITE(11,970)
 970  FORMAT(1X,127('-'))
C
C Compute averages, change to practical units, and print out (noncommutated)
C housekeeping data. Only do this if we have some nonzero values; otherwise
C set the average to 0. Do this for ...
C
C DPU Temp (Passive)
C
      IF((NUM_MF - DPU_TEMP.NZERO).LE.0)THEN
        DPU_TEMP.AVER = -999
      ELSE
        DPU_TEMP.AVER = DPU_TEMP.AVER / (NUM_MF - DPU_TEMP.NZERO)
      ENDIF
      CALL POLY( DPU_TEMP, 6.558508E2 ,-1.802378E1, 2.133615E-1, -1.295112E-3, 3.927925E-6, -4.754717E-9)
      WRITE(11, 1000)'  0  DPU Temp (Passive)', DPU_TEMP.MIN, DPU_TEMP.MAX, DPU_TEMP.AVER, DPU_TEMP.NZERO
1000  FORMAT(1X,A,T45,3F12.2,5X,I3)
C
C SWICS Gnd Electronics Temp (Passive)
C
      IF((NUM_MF - SWICS_GET.NZERO).LE.0)THEN
        SWICS_GET.AVER = -999
      ELSE
        SWICS_GET.AVER = SWICS_GET.AVER / (NUM_MF - SWICS_GET.NZERO)
      ENDIF
      CALL POLY( SWICS_GET, 6.558508E2 ,-1.802378E1, 2.133615E-1, -1.295112E-3, 3.927925E-6, -4.754717E-9)
      WRITE(11, 1000)'  1  SWICS Gnd Electronics Temp (Passive)', SWICS_GET.MIN, SWICS_GET.MAX, SWICS_GET.AVER, SWICS_GET.NZERO
C
C MASS Gnd Electronics Temp (Passive)
C
      IF((NUM_MF - MASS_GET.NZERO).LE.0)THEN
        MASS_GET.AVER = -999
      ELSE
        MASS_GET.AVER  = MASS_GET.AVER / (NUM_MF - MASS_GET.NZERO)
      ENDIF
      CALL POLY( MASS_GET, 6.558508E2 ,-1.802378E1, 2.133615E-1, -1.295112E-3, 3.927925E-6, -4.754717E-9)
      WRITE(11, 1000)'  2  MASS Gnd Electronics Temp (Passive)', MASS_GET.MIN, MASS_GET.MAX, MASS_GET.AVER, MASS_GET.NZERO
C
C STICS SSD Temp (Passive)
C
      IF((NUM_MF - STICS_SSD_TEMP.NZERO).LE.0)THEN
        STICS_SSD_TEMP.AVER = -999
      ELSE
        STICS_SSD_TEMP.AVER  = STICS_SSD_TEMP.AVER / (NUM_MF - STICS_SSD_TEMP.NZERO)
      ENDIF
      CALL POLY( STICS_SSD_TEMP, 6.558508E2 ,-1.802378E1, 2.133615E-1, -1.295112E-3, 3.927925E-6, -4.754717E-9)
      WRITE(11, 1000)'  3  STICS SSD Temp (Passive)', STICS_SSD_TEMP.MIN, STICS_SSD_TEMP.MAX, STICS_SSD_TEMP.AVER,
     &               STICS_SSD_TEMP.NZERO
C
C STICS AE Housing Temp (Passive)
C
      IF((NUM_MF - STICS_AEH_TEMP.NZERO).LE.0)THEN
        STICS_AEH_TEMP.AVER = -999
      ELSE
        STICS_AEH_TEMP.AVER  = STICS_AEH_TEMP.AVER  / (NUM_MF - STICS_AEH_TEMP.NZERO)
      ENDIF
      CALL POLY( STICS_AEH_TEMP, 6.558508E2 ,-1.802378E1, 2.133615E-1, -1.295112E-3, 3.927925E-6, -4.754717E-9)
      WRITE(11, 1000)'  4  STICS AE Housing Temp (Passive)', STICS_AEH_TEMP.MIN, STICS_AEH_TEMP.MAX, STICS_AEH_TEMP.AVER,
     &               STICS_AEH_TEMP.NZERO
C
C DPU 28 V Primary Current
C
      IF((NUM_SR - DPU_28V.NZERO).LE.0)THEN
        DPU_28V.AVER = -999
      ELSE
        DPU_28V.AVER = DPU_28V.AVER / (NUM_SR - DPU_28V.NZERO)
      ENDIF
      CALL POLY( DPU_28V, 0.0, 2.040, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)'  5  DPU 28 V Primary Current', DPU_28V.MIN, DPU_28V.MAX, DPU_28V.AVER, DPU_28V.NZERO
C
C DPU 5.2 V (Switched) Voltage
C
      IF((NUM_SR - DPU_5P2V_S.NZERO).LE.0)THEN
        DPU_5P2V_S.AVER = -999
      ELSE
        DPU_5P2V_S.AVER = DPU_5P2V_S.AVER / (NUM_SR - DPU_5P2V_S.NZERO)
      ENDIF
      CALL POLY( DPU_5P2V_S, 0.0, 2.500E-2 , 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)'  6  DPU 5.2 V (Switched) Voltage', DPU_5P2V_S.MIN, DPU_5P2V_S.MAX, DPU_5P2V_S.AVER, DPU_5P2V_S.NZERO
C
C DPU 5.2 V (Permanent) Voltage
C
      IF((NUM_SR - DPU_5P2V_P.NZERO).LE.0)THEN
        DPU_5P2V_P.AVER = -999
      ELSE
        DPU_5P2V_P.AVER = DPU_5P2V_P.AVER / (NUM_SR - DPU_5P2V_P.NZERO)
      ENDIF
      CALL POLY( DPU_5P2V_P, 0.0, 2.500E-2 , 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)'  7  DPU 5.2 V (Permanent) Voltage', DPU_5P2V_P.MIN, DPU_5P2V_P.MAX, DPU_5P2V_P.AVER, DPU_5P2V_P.NZERO
C
C DPU 10 V Voltage
C
      IF((NUM_SR - DPU_10V.NZERO).LE.0)THEN
        DPU_10V.AVER = -999
      ELSE
        DPU_10V.AVER = DPU_10V.AVER / (NUM_SR - DPU_10V.NZERO)
      ENDIF
      CALL POLY( DPU_10V, 0.0, 5.000E-2 , 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)'  8  DPU 10 V Voltage', DPU_10V.MIN, DPU_10V.MAX, DPU_10V.AVER, DPU_10V.NZERO
C
C DPU Temperature, PIF_A
C
      IF((NUM_SR - DPU_TEMP_PIFA.NZERO).LE.0)THEN
        DPU_TEMP_PIFA.AVER = -999
      ELSE
        DPU_TEMP_PIFA.AVER = DPU_TEMP_PIFA.AVER / (NUM_SR - DPU_TEMP_PIFA.NZERO)
      ENDIF
      CALL POLY( DPU_TEMP_PIFA, 1.437601E2, -3.224279, 4.848285E-2, -4.101237E-4, 1.682762E-6, -2.660079E-9)
      WRITE(11, 1000)'  9  DPU Temperature, PIF_A', DPU_TEMP_PIFA.MIN, DPU_TEMP_PIFA.MAX, DPU_TEMP_PIFA.AVER, DPU_TEMP_PIFA.NZERO
C
C DPU Temperature, CTL_W
C
      IF((NUM_SR - DPU_TEMP_CTLW.NZERO).LE.0)THEN
        DPU_TEMP_CTLW.AVER = -999
      ELSE
        DPU_TEMP_CTLW.AVER = DPU_TEMP_CTLW.AVER / (NUM_SR - DPU_TEMP_CTLW.NZERO)
      ENDIF
      CALL POLY( DPU_TEMP_CTLW, 1.437601E2, -3.224279, 4.848285E-2, -4.101237E-4, 1.682762E-6, -2.660079E-9)
      WRITE(11, 1000)'     DPU Temperature, CTL_W', DPU_TEMP_CTLW.MIN, DPU_TEMP_CTLW.MAX, DPU_TEMP_CTLW.AVER, DPU_TEMP_CTLW.NZERO
C
C SWICS +28 V Primary Current (Gnd Ref)
C
      IF((NUM_SR - SWICS_PLUS28V_PRIM_CURR.NZERO).LE.0)THEN
        SWICS_PLUS28V_PRIM_CURR.AVER = -999
      ELSE
        SWICS_PLUS28V_PRIM_CURR.AVER = SWICS_PLUS28V_PRIM_CURR.AVER / (NUM_SR - SWICS_PLUS28V_PRIM_CURR.NZERO)
      ENDIF
      CALL POLY( SWICS_PLUS28V_PRIM_CURR, 0.0, 1.030 , 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 10  SWICS +28 V Primary Current (Gnd Ref)', SWICS_PLUS28V_PRIM_CURR.MIN, SWICS_PLUS28V_PRIM_CURR.MAX, 
     &               SWICS_PLUS28V_PRIM_CURR.AVER, SWICS_PLUS28V_PRIM_CURR.NZERO
C
C SWICS +5 V Voltage (Gnd Ref)
C
      IF((NUM_SR - SWICS_PLUS5V.NZERO).LE.0)THEN
        SWICS_PLUS5V.AVER = -999
      ELSE
        SWICS_PLUS5V.AVER = SWICS_PLUS5V.AVER / (NUM_SR - SWICS_PLUS5V.NZERO)
      ENDIF
      CALL POLY( SWICS_PLUS5V, 0.0, 2.500E-2, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 11  SWICS +5 V Voltage (Gnd Ref)', SWICS_PLUS5V.MIN, SWICS_PLUS5V.MAX, SWICS_PLUS5V.AVER, SWICS_PLUS5V.NZERO
C
C SWICS +5 V Current (Gnd Ref)
C
      IF((NUM_SR - SWICS_PLUS5I.NZERO).LE.0)THEN
        SWICS_PLUS5I.AVER = -999
      ELSE
        SWICS_PLUS5I.AVER = SWICS_PLUS5I.AVER / (NUM_SR - SWICS_PLUS5I.NZERO)
      ENDIF
      CALL POLY( SWICS_PLUS5I, 0.0, 5.000E-1 , 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 12  SWICS +5 V Current (Gnd Ref)', SWICS_PLUS5I.MIN, SWICS_PLUS5I.MAX, SWICS_PLUS5I.AVER, SWICS_PLUS5I.NZERO
C
C SWICS -5 V Voltage (Gnd Ref)
C
      IF((NUM_SR - SWICS_MINUS5V.NZERO).LE.0)THEN
        SWICS_MINUS5V.AVER = -999
      ELSE
        SWICS_MINUS5V.AVER = SWICS_MINUS5V.AVER / (NUM_SR - SWICS_MINUS5V.NZERO)
      ENDIF
      CALL POLY( SWICS_MINUS5V, 0.0, -2.500E-2, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 13  SWICS -5 V Voltage (Gnd Ref)', SWICS_MINUS5V.MIN, SWICS_MINUS5V.MAX, 
     &               SWICS_MINUS5V.AVER, SWICS_MINUS5V.NZERO
C
C SWICS +8 V Voltage (Gnd Ref)
C
      IF((NUM_SR - SWICS_PLUS8V.NZERO).LE.0)THEN
        SWICS_PLUS8V.AVER = -999
      ELSE
        SWICS_PLUS8V.AVER = SWICS_PLUS8V.AVER / (NUM_SR - SWICS_PLUS8V.NZERO)
      ENDIF
      CALL POLY( SWICS_PLUS8V, 0.0, 4.000E-2, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 14  SWICS +8 V Voltage (Gnd Ref)', SWICS_PLUS8V.MIN, SWICS_PLUS8V.MAX, SWICS_PLUS8V.AVER, SWICS_PLUS8V.NZERO
C
C SWICS +8 V Current (Gnd Ref)
C
      IF((NUM_SR - SWICS_PLUS8I.NZERO).LE.0)THEN
        SWICS_PLUS8I.AVER = -999
      ELSE
        SWICS_PLUS8I.AVER = SWICS_PLUS8I.AVER / (NUM_SR - SWICS_PLUS8I.NZERO)
      ENDIF
      CALL POLY( SWICS_PLUS8I, 0.0, 2.000E-1, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)'     SWICS +8 V Current (Gnd Ref)', SWICS_PLUS8I.MIN, SWICS_PLUS8I.MAX, SWICS_PLUS8I.AVER, SWICS_PLUS8I.NZERO
C
C SWICS 20 V Voltage (Gnd Ref)
C
      IF((NUM_SR - SWICS_20V.NZERO).LE.0)THEN
        SWICS_20V.AVER = -999
      ELSE
        SWICS_20V.AVER = SWICS_20V.AVER / (NUM_SR - SWICS_20V.NZERO)
      ENDIF
      CALL POLY( SWICS_20V, -4.0E-1, 1.000E-1, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 15  SWICS 20 V Voltage (Gnd Ref)', SWICS_20V.MIN, SWICS_20V.MAX, SWICS_20V.AVER, SWICS_20V.NZERO
C
C SWICS 20 V Current (Gnd Ref)
C
      IF((NUM_SR - SWICS_20I.NZERO).LE.0)THEN
        SWICS_20I.AVER = -999
      ELSE
        SWICS_20I.AVER = SWICS_20I.AVER / (NUM_SR - SWICS_20I.NZERO)
      ENDIF
      CALL POLY( SWICS_20I, -1.675, 9.990E-1, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 16  SWICS 20 V Current (Gnd Ref)', SWICS_20I.MIN, SWICS_20I.MAX, SWICS_20I.AVER, SWICS_20I.NZERO
C
C SWICS LVPS 28 V Voltage (Gnd Ref)
C
      IF((NUM_SR - SWICS_LVPS_28V.NZERO).LE.0)THEN
        SWICS_LVPS_28V.AVER = -999
      ELSE
        SWICS_LVPS_28V.AVER = SWICS_LVPS_28V.AVER / (NUM_SR - SWICS_LVPS_28V.NZERO)
      ENDIF
      CALL POLY( SWICS_LVPS_28V, -7.143E-1, 1.429E-1, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 17  SWICS LVPS 28 V Voltage (Gnd Ref)', SWICS_LVPS_28V.MIN, SWICS_LVPS_28V.MAX, 
     &               SWICS_LVPS_28V.AVER, SWICS_LVPS_28V.NZERO
C
C SWICS DPPS 28 V Current (Gnd Ref)
C
      IF((NUM_SR - SWICS_DPPS_28I.NZERO).LE.0)THEN
        SWICS_DPPS_28I.AVER = -999
      ELSE
        SWICS_DPPS_28I.AVER = SWICS_DPPS_28I.AVER / (NUM_SR - SWICS_DPPS_28I.NZERO)
      ENDIF
      CALL POLY( SWICS_DPPS_28I, -3.864E-1, 2.078E-1, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 18  SWICS DPPS 28 V Current (Gnd Ref)', SWICS_DPPS_28I.MIN, SWICS_DPPS_28I.MAX, 
     &               SWICS_DPPS_28I.AVER, SWICS_DPPS_28I.NZERO
C
C SWICS PAPS 28 V Current (Gnd Ref)
C
      IF((NUM_SR - SWICS_PAPS_28I.NZERO).LE.0)THEN
        SWICS_PAPS_28I.AVER = -999
      ELSE
        SWICS_PAPS_28I.AVER = SWICS_PAPS_28I.AVER / (NUM_SR - SWICS_PAPS_28I.NZERO)
      ENDIF
      CALL POLY( SWICS_PAPS_28I, -8.454E-1, 2.065E-1, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 19  SWICS PAPS 28 V Current (Gnd Ref)', SWICS_PAPS_28I.MIN, SWICS_PAPS_28I.MAX, 
     &               SWICS_PAPS_28I.AVER, SWICS_PAPS_28I.NZERO
C
C SWICS PAPS Output Voltage (Gnd Ref)
C
      IF((NUM_SR - SWICS_PAPS_OUTV.NZERO).LE.0)THEN
        SWICS_PAPS_OUTV.AVER = -999
      ELSE
        SWICS_PAPS_OUTV.AVER = SWICS_PAPS_OUTV.AVER / (NUM_SR - SWICS_PAPS_OUTV.NZERO)
      ENDIF
      CALL POLY( SWICS_PAPS_OUTV, 4.453E1, -1.464E2, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1100)' 20  SWICS PAPS Output Voltage (Gnd Ref)', SWICS_PAPS_OUTV.MIN, SWICS_PAPS_OUTV.MAX, 
     &               SWICS_PAPS_OUTV.AVER, SWICS_PAPS_OUTV.NZERO, SWICS_PAPS_CL, SWICS_PAPS_CV
1100  FORMAT(1X,A,T45,3F12.2,5X,I3,10X,I5,6X,F12.2)
C
C SWICS +5 V Voltage (PAPS Ref)
C
      IF((NUM_SR - SWICS_PLUS5V_PR.NZERO).LE.0)THEN
        SWICS_PLUS5V_PR.AVER = -999
      ELSE
        SWICS_PLUS5V_PR.AVER = SWICS_PLUS5V_PR.AVER / (NUM_SR - SWICS_PLUS5V_PR.NZERO)
      ENDIF
      CALL POLY( SWICS_PLUS5V_PR, 0.0, 2.483E-2, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 21  SWICS +5 V Voltage (PAPS Ref)', SWICS_PLUS5V_PR.MIN, SWICS_PLUS5V_PR.MAX,  
     &               SWICS_PLUS5V_PR.AVER, SWICS_PLUS5V_PR.NZERO
C
C SWICS +5 V Current (PAPS Ref)
C
      IF((NUM_SR - SWICS_PLUS5I_PR.NZERO).LE.0)THEN
        SWICS_PLUS5I_PR.AVER = -999
      ELSE
        SWICS_PLUS5I_PR.AVER = SWICS_PLUS5I_PR.AVER / (NUM_SR - SWICS_PLUS5I_PR.NZERO)
      ENDIF
      CALL POLY( SWICS_PLUS5I_PR, 0.0, 7.176E-1, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 22  SWICS +5 V Current (PAPS Ref)', SWICS_PLUS5I_PR.MIN, SWICS_PLUS5I_PR.MAX,  
     &               SWICS_PLUS5I_PR.AVER, SWICS_PLUS5I_PR.NZERO
C
C SWICS -5 V Voltage (PAPS Ref)
C
      IF((NUM_SR - SWICS_MINUS5V_PR.NZERO).LE.0)THEN
        SWICS_MINUS5V_PR.AVER = -999
      ELSE
        SWICS_MINUS5V_PR.AVER = SWICS_MINUS5V_PR.AVER / (NUM_SR - SWICS_MINUS5V_PR.NZERO)
      ENDIF
      CALL POLY( SWICS_MINUS5V_PR, 0.0, -2.488E-2, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 23  SWICS -5 V Voltage (PAPS Ref)', SWICS_MINUS5V_PR.MIN, SWICS_MINUS5V_PR.MAX, 
     &               SWICS_MINUS5V_PR.AVER, SWICS_MINUS5V_PR.NZERO
C
C SWICS -5 V Current (PAPS Ref)
C
      IF((NUM_SR - SWICS_MINUS5I_PR.NZERO).LE.0)THEN
        SWICS_MINUS5I_PR.AVER = -999
      ELSE
        SWICS_MINUS5I_PR.AVER = SWICS_MINUS5I_PR.AVER / (NUM_SR - SWICS_MINUS5I_PR.NZERO)
      ENDIF
      CALL POLY( SWICS_MINUS5I_PR, 0.0, 7.353E-1, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 24  SWICS -5 V Current (PAPS Ref)', SWICS_MINUS5I_PR.MIN, SWICS_MINUS5I_PR.MAX, 
     &               SWICS_MINUS5I_PR.AVER, SWICS_MINUS5I_PR.NZERO
C
C SWICS +12 V Voltage (PAPS Ref)
C
      IF((NUM_SR - SWICS_PLUS12V_PR.NZERO).LE.0)THEN
        SWICS_PLUS12V_PR.AVER = -999
      ELSE
        SWICS_PLUS12V_PR.AVER = SWICS_PLUS12V_PR.AVER / (NUM_SR - SWICS_PLUS12V_PR.NZERO)
      ENDIF
      CALL POLY( SWICS_PLUS12V_PR, 0.0, 5.977E-2, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 25  SWICS +12 V Voltage (PAPS Ref)', SWICS_PLUS12V_PR.MIN, SWICS_PLUS12V_PR.MAX, 
     &               SWICS_PLUS12V_PR.AVER, SWICS_PLUS12V_PR.NZERO
C
C SWICS +12 V Current (PAPS Ref)
C
      IF((NUM_SR - SWICS_PLUS12I_PR.NZERO).LE.0)THEN
        SWICS_PLUS12I_PR.AVER = -999
      ELSE
        SWICS_PLUS12I_PR.AVER = SWICS_PLUS12I_PR.AVER / (NUM_SR - SWICS_PLUS12I_PR.NZERO)
      ENDIF
      CALL POLY( SWICS_PLUS12I_PR, 0.0, 6.625E-1, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 26  SWICS +12 V Current (PAPS Ref)', SWICS_PLUS12I_PR.MIN, SWICS_PLUS12I_PR.MAX, 
     &               SWICS_PLUS12I_PR.AVER, SWICS_PLUS12I_PR.NZERO
C
C -12 V Voltage (PAPS Ref)
C
      IF((NUM_SR - SWICS_MINUS12V_PR.NZERO).LE.0)THEN
        SWICS_MINUS12V_PR.AVER = -999
      ELSE
        SWICS_MINUS12V_PR.AVER = SWICS_MINUS12V_PR.AVER / (NUM_SR - SWICS_MINUS12V_PR.NZERO)
      ENDIF
      CALL POLY( SWICS_MINUS12V_PR, 0.0, -5.917E-2, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 27  -12 V Voltage (PAPS Ref)', SWICS_MINUS12V_PR.MIN, SWICS_MINUS12V_PR.MAX, 
     &               SWICS_MINUS12V_PR.AVER, SWICS_MINUS12V_PR.NZERO
C
C SWICS -12 V Current (PAPS Ref)
C
      IF((NUM_SR - SWICS_MINUS12I_PR.NZERO).LE.0)THEN
        SWICS_MINUS12I_PR.AVER = -999
      ELSE
        SWICS_MINUS12I_PR.AVER = SWICS_MINUS12I_PR.AVER / (NUM_SR - SWICS_MINUS12I_PR.NZERO)
      ENDIF
      CALL POLY( SWICS_MINUS12I_PR, 0.0, 1.484E-1, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 28  SWICS -12 V Current (PAPS Ref)', SWICS_MINUS12I_PR.MIN, SWICS_MINUS12I_PR.MAX, 
     &               SWICS_MINUS12I_PR.AVER, SWICS_MINUS12I_PR.NZERO
C
C SWICS SSD Bias Voltage (PAPS Ref)
C
      IF((NUM_SR - SWICS_SSD_BIASV_PR.NZERO).LE.0)THEN
        SWICS_SSD_BIASV_PR.AVER = -999
      ELSE
        SWICS_SSD_BIASV_PR.AVER = SWICS_SSD_BIASV_PR.AVER / (NUM_SR - SWICS_SSD_BIASV_PR.NZERO)
      ENDIF
      CALL POLY( SWICS_SSD_BIASV_PR, 0, 0.5, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)'     SWICS SSD Bias Voltage (PAPS Ref)', SWICS_SSD_BIASV_PR.MIN, SWICS_SSD_BIASV_PR.MAX, 
     &               SWICS_SSD_BIASV_PR.AVER, SWICS_SSD_BIASV_PR.NZERO
C
C SWICS Start MCPPS Voltage (PAPS Ref)
C
      IF((NUM_SR - SWICS_START_MCPPSV_PR.NZERO).LE.0)THEN
        SWICS_START_MCPPSV_PR.AVER = -999
      ELSE
        SWICS_START_MCPPSV_PR.AVER = SWICS_START_MCPPSV_PR.AVER / (NUM_SR - SWICS_START_MCPPSV_PR.NZERO)
      ENDIF
      CALL POLY( SWICS_START_MCPPSV_PR, -4.610, 1.864E1, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1100)'     SWICS Start MCPPS Voltage (PAPS Ref)', SWICS_START_MCPPSV_PR.MIN, SWICS_START_MCPPSV_PR.MAX, 
     &               SWICS_START_MCPPSV_PR.AVER, SWICS_START_MCPPSV_PR.NZERO, SWICS_Start_MCPPS_CL, SWICS_Start_MCPPS_CV 
C
C SWICS Stop MCPPS Voltage (PAPS Ref)
C
      IF((NUM_SR - SWICS_STOP_MCPPSV_PR.NZERO).LE.0)THEN
        SWICS_STOP_MCPPSV_PR.AVER = -999
      ELSE
        SWICS_STOP_MCPPSV_PR.AVER = SWICS_STOP_MCPPSV_PR.AVER / (NUM_SR - SWICS_STOP_MCPPSV_PR.NZERO)
      ENDIF
      CALL POLY( SWICS_STOP_MCPPSV_PR, 4.650, 1.356E1, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1100)'     SWICS Stop MCPPS Voltage (PAPS Ref)', SWICS_STOP_MCPPSV_PR.MIN, SWICS_STOP_MCPPSV_PR.MAX, 
     &               SWICS_STOP_MCPPSV_PR.AVER, SWICS_STOP_MCPPSV_PR.NZERO, SWICS_Stop_MCPPS_CL, SWICS_Stop_MCPPS_CV 
C
C SWICS SSD Temp (PAPS Ref)
C
      IF((NUM_SR - SWICS_SSD_TEMP_PR.NZERO).LE.0)THEN
        SWICS_SSD_TEMP_PR.AVER = -999
      ELSE
        SWICS_SSD_TEMP_PR.AVER = SWICS_SSD_TEMP_PR.AVER / (NUM_SR - SWICS_SSD_TEMP_PR.NZERO)
      ENDIF
      CALL POLY( SWICS_SSD_TEMP_PR, 7.886257E1, -1.848392, 2.143749E-2, -1.483171E-4, 5.149126E-7, -7.140936E-10)
      WRITE(11, 1000)' 29  SWICS SSD Temp (PAPS Ref)', SWICS_SSD_TEMP_PR.MIN, SWICS_SSD_TEMP_PR.MAX, 
     &               SWICS_SSD_TEMP_PR.AVER, SWICS_SSD_TEMP_PR.NZERO
C
C SWICS Bubble Elect Temp, (PAPS Ref)
C
      IF((NUM_SR - SWICS_BET_PR.NZERO).LE.0)THEN
        SWICS_BET_PR.AVER = -999
      ELSE
        SWICS_BET_PR.AVER = SWICS_BET_PR.AVER / (NUM_SR - SWICS_BET_PR.NZERO)
      ENDIF
      CALL POLY( SWICS_BET_PR, 7.886257E1, -1.848392, 2.143749E-2, -1.483171E-4, 5.149126E-7, -7.140936E-10)
      WRITE(11, 1000)'     SWICS Bubble Elect Temp, (PAPS Ref)', SWICS_BET_PR.MIN, SWICS_BET_PR.MAX, 
     &               SWICS_BET_PR.AVER, SWICS_BET_PR.NZERO
C
C MASS +28 V Primary Current (Gnd Ref)
C
      IF((NUM_SR - MASS_PLUS28I.NZERO).LE.0)THEN
        MASS_PLUS28I.AVER = -999
      ELSE
        MASS_PLUS28I.AVER = MASS_PLUS28I.AVER / (NUM_SR - MASS_PLUS28I.NZERO)
      ENDIF
      CALL POLY( MASS_PLUS28I, 0.0, 1.050, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 30  MASS +28 V Primary Current (Gnd Ref)', MASS_PLUS28I.MIN, MASS_PLUS28I.MAX, 
     &               MASS_PLUS28I.AVER, MASS_PLUS28I.NZERO
C
C MASS +5 V Voltage (Gnd Ref)
C
      IF((NUM_SR - MASS_PLUS5V.NZERO).LE.0)THEN
        MASS_PLUS5V.AVER = -999
      ELSE
        MASS_PLUS5V.AVER = MASS_PLUS5V.AVER / (NUM_SR - MASS_PLUS5V.NZERO)
      ENDIF
      CALL POLY( MASS_PLUS5V, 0.0, 2.500E-2, 0.0, 0.0, 0.0, 0.0 )
      WRITE(11, 1000)' 31  MASS +5 V Voltage (Gnd Ref)', MASS_PLUS5V.MIN, MASS_PLUS5V.MAX, MASS_PLUS5V.AVER, MASS_PLUS5V.NZERO
C
C MASS +5 V Current (Gnd Ref)
C
      IF((NUM_SR - MASS_PLUS5I.NZERO).LE.0)THEN
        MASS_PLUS5I.AVER = -999
      ELSE
        MASS_PLUS5I.AVER = MASS_PLUS5I.AVER / (NUM_SR - MASS_PLUS5I.NZERO)
      ENDIF
      CALL POLY( MASS_PLUS5I, 0.0, 5.000E-1, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 32  MASS +5 V Current (Gnd Ref)', MASS_PLUS5I.MIN, MASS_PLUS5I.MAX, MASS_PLUS5I.AVER, MASS_PLUS5I.NZERO
C
C MASS -5 V Voltage (Gnd Ref)
C
      IF((NUM_SR - MASS_MINUS5V.NZERO).LE.0)THEN
        MASS_MINUS5V.AVER = -999
      ELSE
        MASS_MINUS5V.AVER = MASS_MINUS5V.AVER / (NUM_SR - MASS_MINUS5V.NZERO)
      ENDIF
      CALL POLY( MASS_MINUS5V, 0.0, -2.500E-2, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 33  MASS -5 V Voltage (Gnd Ref)', MASS_MINUS5V.MIN, MASS_MINUS5V.MAX, MASS_MINUS5V.AVER, MASS_MINUS5V.NZERO
C
C MASS +8 V Voltage (Gnd Ref)
C
      IF((NUM_SR - MASS_PLUS8V.NZERO).LE.0)THEN
        MASS_PLUS8V.AVER = -999
      ELSE
        MASS_PLUS8V.AVER = MASS_PLUS8V.AVER / (NUM_SR - MASS_PLUS8V.NZERO)
      ENDIF
      CALL POLY( MASS_PLUS8V, 0.0, 4.000E-2, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 34  MASS +8 V Voltage (Gnd Ref)', MASS_PLUS8V.MIN, MASS_PLUS8V.MAX, MASS_PLUS8V.AVER, MASS_PLUS8V.NZERO
C
C MASS LVPS 28 V Voltage (Gnd Ref)
C
      IF((NUM_SR - MASS_LVPS28V.NZERO).LE.0)THEN
        MASS_LVPS28V.AVER = -999
      ELSE
        MASS_LVPS28V.AVER = MASS_LVPS28V.AVER / (NUM_SR - MASS_LVPS28V.NZERO)
      ENDIF
      CALL POLY( MASS_LVPS28V, 0.0, 1.400E-1, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 35  MASS LVPS 28 V Voltage (Gnd Ref)', MASS_LVPS28V.MIN, MASS_LVPS28V.MAX, 
     &               MASS_LVPS28V.AVER, MASS_LVPS28V.NZERO
C
C MASS ADPS 28 V Current (Gnd Ref)
C
      IF((NUM_SR - MASS_ADPS28V.NZERO).LE.0)THEN
        MASS_ADPS28V.AVER = -999
      ELSE
        MASS_ADPS28V.AVER = MASS_ADPS28V.AVER / (NUM_SR - MASS_ADPS28V.NZERO)
      ENDIF
      CALL POLY( MASS_ADPS28V, 0.0, 2.000E-1, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 36  MASS ADPS 28 V Current (Gnd Ref)', MASS_ADPS28V.MIN, MASS_ADPS28V.MAX, 
     &               MASS_ADPS28V.AVER, MASS_ADPS28V.NZERO
C
C MASS DPPS 28 V Current (Gnd Ref)
C
      IF((NUM_SR - MASS_DPPS28I.NZERO).LE.0)THEN
        MASS_DPPS28I.AVER = -999
      ELSE
        MASS_DPPS28I.AVER = MASS_DPPS28I.AVER / (NUM_SR - MASS_DPPS28I.NZERO)
      ENDIF
      CALL POLY( MASS_DPPS28I, 0.0, 2.000E-1, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 38  MASS DPPS 28 V Current (Gnd Ref)', MASS_DPPS28I.MIN, MASS_DPPS28I.MAX, 
     &               MASS_DPPS28I.AVER, MASS_DPPS28I.NZERO
C
C MASS Analog Electronics Temp (Gnd Ref)
C
      IF((NUM_SR - MASS_AET.NZERO).LE.0)THEN
        MASS_AET.AVER = -999
      ELSE
        MASS_AET.AVER = MASS_AET.AVER / (NUM_SR - MASS_AET.NZERO)
      ENDIF
      CALL POLY( MASS_AET, 7.886257E1, -1.848392, 2.143749E-2, -1.483171E-4, 5.149126E-7, -7.140936E-10)
      WRITE(11, 1000)' 39  MASS Analog Electronics Temp (Gnd Ref)', MASS_AET.MIN, MASS_AET.MAX, MASS_AET.AVER, MASS_AET.NZERO
C
C MASS +5 V Voltage (ADPS Ref)
C
      IF((NUM_SR - MASS_PLUS5V_AR.NZERO).LE.0)THEN
        MASS_PLUS5V_AR.AVER = -999
      ELSE
        MASS_PLUS5V_AR.AVER = MASS_PLUS5V_AR.AVER / (NUM_SR - MASS_PLUS5V_AR.NZERO)
      ENDIF
      CALL POLY( MASS_PLUS5V_AR, 0.0, 2.500E-2, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 40  MASS +5 V Voltage (ADPS Ref)', MASS_PLUS5V_AR.MIN, MASS_PLUS5V_AR.MAX, 
     &               MASS_PLUS5V_AR.AVER, MASS_PLUS5V_AR.NZERO
C
C MASS +5 V Current (ADPS Ref)
C
      IF((NUM_SR - MASS_PLUS5I_AR.NZERO).LE.0)THEN
        MASS_PLUS5I_AR.AVER = -999
      ELSE
        MASS_PLUS5I_AR.AVER = MASS_PLUS5I_AR.AVER / (NUM_SR - MASS_PLUS5I_AR.NZERO)
      ENDIF
      CALL POLY( MASS_PLUS5I_AR, 0.0, 1.000, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 41  MASS +5 V Current (ADPS Ref)', MASS_PLUS5I_AR.MIN, MASS_PLUS5I_AR.MAX, 
     &               MASS_PLUS5I_AR.AVER, MASS_PLUS5I_AR.NZERO
C
C MASS -5 V Voltage (ADPS Ref)
C
      IF((NUM_SR - MASS_MINUS5V_AR.NZERO).LE.0)THEN
        MASS_MINUS5V_AR.AVER = -999
      ELSE
        MASS_MINUS5V_AR.AVER = MASS_MINUS5V_AR.AVER / (NUM_SR - MASS_MINUS5V_AR.NZERO)
      ENDIF
      CALL POLY( MASS_MINUS5V_AR, 0.0, -2.500E-2, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 42  MASS -5 V Voltage (ADPS Ref)', MASS_MINUS5V_AR.MIN, MASS_MINUS5V_AR.MAX, 
     &               MASS_MINUS5V_AR.AVER, MASS_MINUS5V_AR.NZERO
C
C MASS -5 V Current (ADPS Ref)
C
      IF((NUM_SR - MASS_MINUS5I_AR.NZERO).LE.0)THEN
        MASS_MINUS5I_AR.AVER = -999
      ELSE
        MASS_MINUS5I_AR.AVER = MASS_MINUS5I_AR.AVER / (NUM_SR - MASS_MINUS5I_AR.NZERO)
      ENDIF
      CALL POLY( MASS_MINUS5I_AR, 0.0, 1.000, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 43  MASS -5 V Current (ADPS Ref)', MASS_MINUS5I_AR.MIN, MASS_MINUS5I_AR.MAX, 
     &               MASS_MINUS5I_AR.AVER, MASS_MINUS5I_AR.NZERO
C
C MASS +6 V Voltage (ADPS Ref)
C
      IF((NUM_SR - MASS_PLUS6V_AR.NZERO).LE.0)THEN
        MASS_PLUS6V_AR.AVER = -999
      ELSE
        MASS_PLUS6V_AR.AVER = MASS_PLUS6V_AR.AVER / (NUM_SR - MASS_PLUS6V_AR.NZERO)
      ENDIF
      CALL POLY( MASS_PLUS6V_AR, 0.0, 3.000E-2, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 44  MASS +6 V Voltage (ADPS Ref)', MASS_PLUS6V_AR.MIN, MASS_PLUS6V_AR.MAX, 
     &               MASS_PLUS6V_AR.AVER, MASS_PLUS6V_AR.NZERO
C
C MASS +6 V Current (ADPS Ref)
C
      IF((NUM_SR - MASS_PLUS6I_AR.NZERO).LE.0)THEN
        MASS_PLUS6I_AR.AVER = -999
      ELSE
        MASS_PLUS6I_AR.AVER = MASS_PLUS6I_AR.AVER / (NUM_SR - MASS_PLUS6I_AR.NZERO)
      ENDIF
      CALL POLY( MASS_PLUS6I_AR, 0.0, 1.000, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 45  MASS +6 V Current (ADPS Ref)', MASS_PLUS6I_AR.MIN, MASS_PLUS6I_AR.MAX, 
     &               MASS_PLUS6I_AR.AVER, MASS_PLUS6I_AR.NZERO
C
C MASS +12 V Voltage (ADPS Ref)
C
      IF((NUM_SR - MASS_PLUS12V_AR.NZERO).LE.0)THEN
        MASS_PLUS12V_AR.AVER = -999
      ELSE
        MASS_PLUS12V_AR.AVER = MASS_PLUS12V_AR.AVER / (NUM_SR - MASS_PLUS12V_AR.NZERO)
      ENDIF
      CALL POLY( MASS_PLUS12V_AR, 0.0, 6.000E-2, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 46  MASS +12 V Voltage (ADPS Ref)', MASS_PLUS12V_AR.MIN, MASS_PLUS12V_AR.MAX, 
     &               MASS_PLUS12V_AR.AVER, MASS_PLUS12V_AR.NZERO
C
C MASS -12 V Voltage (ADPS Ref)
C
      IF((NUM_SR - MASS_MINUS12V_AR.NZERO).LE.0)THEN
        MASS_MINUS12V_AR.AVER = -999
      ELSE
        MASS_MINUS12V_AR.AVER = MASS_MINUS12V_AR.AVER / (NUM_SR - MASS_MINUS12V_AR.NZERO)
      ENDIF
      CALL POLY( MASS_MINUS12V_AR, 0.0, -6.000E-2, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 47  MASS -12 V Voltage (ADPS Ref)', MASS_MINUS12V_AR.MIN, MASS_MINUS12V_AR.MAX, 
     &               MASS_MINUS12V_AR.AVER, MASS_MINUS12V_AR.NZERO
C
C MASS LVPS 28 V Voltage (ADPS Ref)
C
      IF((NUM_SR - MASS_LVPS28V_AR.NZERO).LE.0)THEN
        MASS_LVPS28V_AR.AVER = -999
      ELSE
        MASS_LVPS28V_AR.AVER = MASS_LVPS28V_AR.AVER / (NUM_SR - MASS_LVPS28V_AR.NZERO)
      ENDIF
      CALL POLY( MASS_LVPS28V_AR, 0.0, 1.400E-1, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 48  MASS LVPS 28 V Voltage (ADPS Ref)', MASS_LVPS28V_AR.MIN, MASS_LVPS28V_AR.MAX, 
     &               MASS_LVPS28V_AR.AVER, MASS_LVPS28V_AR.NZERO
C
C MASS HPS & MCPPS 28V I (ADPS Ref)
C
      IF((NUM_SR - MASS_HPS_MCPPS_28I_AR.NZERO).LE.0)THEN
        MASS_HPS_MCPPS_28I_AR.AVER = -999
      ELSE
        MASS_HPS_MCPPS_28I_AR.AVER = MASS_HPS_MCPPS_28I_AR.AVER / (NUM_SR - MASS_HPS_MCPPS_28I_AR.NZERO)
      ENDIF
      CALL POLY( MASS_HPS_MCPPS_28I_AR, 0.0, 2.000E-1, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 49  MASS HPS & MCPPS 28V I (ADPS Ref)', MASS_HPS_MCPPS_28I_AR.MIN, MASS_HPS_MCPPS_28I_AR.MAX,
     &                MASS_HPS_MCPPS_28I_AR.AVER , MASS_HPS_MCPPS_28I_AR.NZERO
C
C MASS HPS Output Voltage (ADPS Ref)
C
      IF((NUM_SR - MASS_HPS_OUTV_AR.NZERO).LE.0)THEN
        MASS_HPS_OUTV_AR.AVER = -999
      ELSE
        MASS_HPS_OUTV_AR.AVER = MASS_HPS_OUTV_AR.AVER / (NUM_SR - MASS_HPS_OUTV_AR.NZERO)
      ENDIF
      CALL POLY( MASS_HPS_OUTV_AR, -3.428E1, 1.5285E2, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1100)' 50  MASS HPS Output Voltage (ADPS Ref)', MASS_HPS_OUTV_AR.MIN, MASS_HPS_OUTV_AR.MAX, 
     &               MASS_HPS_OUTV_AR.AVER, MASS_HPS_OUTV_AR.NZERO, MASS_HPS_CL, MASS_HPS_CV 
C
C MASS Start MCPPS Voltage (ADPS Ref)
C
      IF((NUM_SR - MASS_START_MCPPSV_AR.NZERO).LE.0)THEN
        MASS_START_MCPPSV_AR.AVER = -999
      ELSE
        MASS_START_MCPPSV_AR.AVER = MASS_START_MCPPSV_AR.AVER / (NUM_SR - MASS_START_MCPPSV_AR.NZERO)
      ENDIF
      CALL POLY( MASS_START_MCPPSV_AR, 1.310E1, 1.431E1, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1100)'     MASS Start MCPPS Voltage (ADPS Ref)', MASS_START_MCPPSV_AR.MIN, MASS_START_MCPPSV_AR.MAX,
     &               MASS_START_MCPPSV_AR.AVER , MASS_START_MCPPSV_AR.NZERO, MASS_Start_MCPPS_CL, MASS_Start_MCPPS_CV 
C
C MASS Stop MCPPS Voltage (ADPS Ref)
C
      IF((NUM_SR - MASS_STOP_MCPPSV_AR.NZERO).LE.0)THEN
        MASS_STOP_MCPPSV_AR.AVER = -999
      ELSE
        MASS_STOP_MCPPSV_AR.AVER = MASS_STOP_MCPPSV_AR.AVER / (NUM_SR - MASS_STOP_MCPPSV_AR.NZERO)
      ENDIF
      CALL POLY( MASS_STOP_MCPPSV_AR, 0.0, 3.065E1, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1100)'     MASS Stop MCPPS Voltage (ADPS Ref)', MASS_STOP_MCPPSV_AR.MIN, MASS_STOP_MCPPSV_AR.MAX,
     &                MASS_STOP_MCPPSV_AR.AVER, MASS_STOP_MCPPSV_AR.NZERO, MASS_Stop_MCPPS_CL, MASS_Stop_MCPPS_CV
C
C STICS 28 V Primary Current
C
      IF((NUM_SR - STICS_28I.NZERO).LE.0)THEN
        STICS_28I.AVER = -999
      ELSE
        STICS_28I.AVER = STICS_28I.AVER / (NUM_SR - STICS_28I.NZERO)
      ENDIF
      CALL POLY( STICS_28I, 0.0, 2.040, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 51  STICS 28 V Primary Current', STICS_28I.MIN, STICS_28I.MAX, STICS_28I.AVER, STICS_28I.NZERO
C
C STICS +5 V Voltage
C
      IF((NUM_SR - STICS_PLUS5V.NZERO).LE.0)THEN
        STICS_PLUS5V.AVER = -999
      ELSE
        STICS_PLUS5V.AVER = STICS_PLUS5V.AVER / (NUM_SR - STICS_PLUS5V.NZERO)
      ENDIF
      CALL POLY( STICS_PLUS5V, 0.0, 2.455E-2, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 52  STICS +5 V Voltage', STICS_PLUS5V.MIN, STICS_PLUS5V.MAX, STICS_PLUS5V.AVER, STICS_PLUS5V.NZERO
C
C STICS -5 V Voltage
C
      IF((NUM_SR - STICS_MINUS5V.NZERO).LE.0)THEN
        STICS_MINUS5V.AVER = -999
      ELSE
        STICS_MINUS5V.AVER = STICS_MINUS5V.AVER / (NUM_SR - STICS_MINUS5V.NZERO)
      ENDIF
      CALL POLY( STICS_MINUS5V, 0.0, -2.451E-2, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 53  STICS -5 V Voltage', STICS_MINUS5V.MIN, STICS_MINUS5V.MAX, STICS_MINUS5V.AVER, STICS_MINUS5V.NZERO
C
C STICS +6 V Voltage
C
      IF((NUM_SR - STICS_PLUS6V.NZERO).LE.0)THEN
        STICS_PLUS6V.AVER = -999
      ELSE
        STICS_PLUS6V.AVER = STICS_PLUS6V.AVER / (NUM_SR - STICS_PLUS6V.NZERO)
      ENDIF
      CALL POLY( STICS_PLUS6V, 0.0, 2.876E-2, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 54  STICS +6 V Voltage', STICS_PLUS6V.MIN, STICS_PLUS6V.MAX, STICS_PLUS6V.AVER, STICS_PLUS6V.NZERO
C
C STICS -6 V Voltage
C
      IF((NUM_SR - STICS_MINUS6V.NZERO).LE.0)THEN
        STICS_MINUS6V.AVER = -999
      ELSE
        STICS_MINUS6V.AVER = STICS_MINUS6V.AVER / (NUM_SR - STICS_MINUS6V.NZERO)
      ENDIF
      CALL POLY( STICS_MINUS6V, 0.0, -2.898E-2, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 55  STICS -6 V Voltage', STICS_MINUS6V.MIN, STICS_MINUS6V.MAX, STICS_MINUS6V.AVER, STICS_MINUS6V.NZERO
C
C STICS +12 V Voltage
C
      IF((NUM_SR - STICS_PLUS12V.NZERO).LE.0)THEN
        STICS_PLUS12V.AVER = -999
      ELSE
        STICS_PLUS12V.AVER = STICS_PLUS12V.AVER / (NUM_SR - STICS_PLUS12V.NZERO)
      ENDIF
      CALL POLY( STICS_PLUS12V, 0.0, 5.837E-2, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 56  STICS +12 V Voltage', STICS_PLUS12V.MIN, STICS_PLUS12V.MAX, STICS_PLUS12V.AVER, STICS_PLUS12V.NZERO
C
C STICS -12 V Voltage
C
      IF((NUM_SR - STICS_MINUS12V.NZERO).LE.0)THEN
        STICS_MINUS12V.AVER = -999
      ELSE
        STICS_MINUS12V.AVER = STICS_MINUS12V.AVER / (NUM_SR - STICS_MINUS12V.NZERO)
      ENDIF
      CALL POLY( STICS_MINUS12V, 0.0, -5.880E-2, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 57  STICS -12 V Voltage', STICS_MINUS12V.MIN, STICS_MINUS12V.MAX, STICS_MINUS12V.AVER, STICS_MINUS12V.NZERO
C
C STICS MCPPS 28 V Current
C
      IF((NUM_SR - STICS_MCPPS28I.NZERO).LE.0)THEN
        STICS_MCPPS28I.AVER = -999
      ELSE
        STICS_MCPPS28I.AVER = STICS_MCPPS28I.AVER / (NUM_SR - STICS_MCPPS28I.NZERO)
      ENDIF
      CALL POLY( STICS_MCPPS28I, 0.0, 5.000E-1, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 58  STICS MCPPS 28 V Current', STICS_MCPPS28I.MIN, STICS_MCPPS28I.MAX, 
     &               STICS_MCPPS28I.AVER, STICS_MCPPS28I.NZERO
C
C STICS DPPS 28 V Current
C
      IF((NUM_SR - STICS_DPPS28I.NZERO).LE.0)THEN
        STICS_DPPS28I.AVER = -999
      ELSE
        STICS_DPPS28I.AVER = STICS_DPPS28I.AVER / (NUM_SR - STICS_DPPS28I.NZERO)
      ENDIF
      CALL POLY( STICS_DPPS28I, 0.0, 5.000E-1, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)' 59  STICS DPPS 28 V Current', STICS_DPPS28I.MIN, STICS_DPPS28I.MAX, STICS_DPPS28I.AVER, STICS_DPPS28I.NZERO
C
C STICS SSD Bias Voltage
C
      IF((NUM_SR - STICS_SSD_BIASV.NZERO).LE.0)THEN
        STICS_SSD_BIASV.AVER = -999
      ELSE
        STICS_SSD_BIASV.AVER = STICS_SSD_BIASV.AVER / (NUM_SR - STICS_SSD_BIASV.NZERO)
      ENDIF
      CALL POLY( STICS_SSD_BIASV, 0.0, 0.5, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1000)'     STICS SSD Bias Voltage', STICS_SSD_BIASV.MIN, STICS_SSD_BIASV.MAX, 
     &               STICS_SSD_BIASV.AVER, STICS_SSD_BIASV.NZERO
C
C STICS MCPPS 0 Output Voltage
C
      IF((NUM_SR - STICS_MCPPS0_OUTV.NZERO).LE.0)THEN
        STICS_MCPPS0_OUTV.AVER = -999
      ELSE
        STICS_MCPPS0_OUTV.AVER = STICS_MCPPS0_OUTV.AVER / (NUM_SR - STICS_MCPPS0_OUTV.NZERO)
      ENDIF
      CALL POLY( STICS_MCPPS0_OUTV, -7.806, 1.959E1, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1100)'     STICS MCPPS 0 Output Voltage', STICS_MCPPS0_OUTV.MIN, STICS_MCPPS0_OUTV.MAX, 
     &               STICS_MCPPS0_OUTV.AVER, STICS_MCPPS0_OUTV.NZERO, STICS_MCPPS_0_CL, STICS_MCPPS_0_CV 
C
C STICS MCPPS 1 Output Voltage
C
      IF((NUM_SR - STICS_MCPPS1_OUTV.NZERO).LE.0)THEN
        STICS_MCPPS1_OUTV.AVER = -999
      ELSE
        STICS_MCPPS1_OUTV.AVER = STICS_MCPPS1_OUTV.AVER / (NUM_SR - STICS_MCPPS1_OUTV.NZERO)
      ENDIF
      CALL POLY( STICS_MCPPS1_OUTV, -3.475, 1.950E1, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1100)'     STICS MCPPS 1 Output Voltage', STICS_MCPPS1_OUTV.MIN, STICS_MCPPS1_OUTV.MAX, 
     &               STICS_MCPPS1_OUTV.AVER, STICS_MCPPS1_OUTV.NZERO, STICS_MCPPS_1_CL, STICS_MCPPS_1_CV 
C
C STICS MCPPS 2 Output Voltage
C
      IF((NUM_SR - STICS_MCPPS2_OUTV.NZERO).LE.0)THEN
        STICS_MCPPS2_OUTV.AVER = -999
      ELSE
        STICS_MCPPS2_OUTV.AVER = STICS_MCPPS2_OUTV.AVER / (NUM_SR - STICS_MCPPS2_OUTV.NZERO)
      ENDIF
      CALL POLY( STICS_MCPPS2_OUTV, -9.405, 1.957E1, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1100)'     STICS MCPPS 2 Output Voltage', STICS_MCPPS2_OUTV.MIN, STICS_MCPPS2_OUTV.MAX, 
     &               STICS_MCPPS2_OUTV.AVER, STICS_MCPPS2_OUTV.NZERO, STICS_MCPPS_2_CL, STICS_MCPPS_2_CV 
C
C STICS MCPPS 3 Output Voltage
C
      IF((NUM_SR - STICS_MCPPS3_OUTV.NZERO).LE.0)THEN
        STICS_MCPPS3_OUTV.AVER = -999
      ELSE
        STICS_MCPPS3_OUTV.AVER = STICS_MCPPS3_OUTV.AVER / (NUM_SR - STICS_MCPPS3_OUTV.NZERO)
      ENDIF
      CALL POLY( STICS_MCPPS3_OUTV, -1.950E1, 1.984E1, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1100)'     STICS MCPPS 3 Output Voltage', STICS_MCPPS3_OUTV.MIN, STICS_MCPPS3_OUTV.MAX, 
     &               STICS_MCPPS3_OUTV.AVER, STICS_MCPPS3_OUTV.NZERO, STICS_MCPPS_3_CL, STICS_MCPPS_3_CV 
C
C STICS MCPPS 4 Output Voltage
C
      IF((NUM_SR - STICS_MCPPS4_OUTV.NZERO).LE.0)THEN
        STICS_MCPPS4_OUTV.AVER = -999
      ELSE
        STICS_MCPPS4_OUTV.AVER = STICS_MCPPS4_OUTV.AVER / (NUM_SR - STICS_MCPPS4_OUTV.NZERO)
      ENDIF
      CALL POLY( STICS_MCPPS4_OUTV, -1.436E1, 1.984E1, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1100)'     STICS MCPPS 4 Output Voltage', STICS_MCPPS4_OUTV.MIN, STICS_MCPPS4_OUTV.MAX, 
     &               STICS_MCPPS4_OUTV.AVER, STICS_MCPPS4_OUTV.NZERO, STICS_MCPPS_4_CL, STICS_MCPPS_4_CV 
C
C STICS MCPPS 5 Output Voltage
C
      IF((NUM_SR - STICS_MCPPS5_OUTV.NZERO).LE.0)THEN
        STICS_MCPPS5_OUTV.AVER = -999
      ELSE
        STICS_MCPPS5_OUTV.AVER = STICS_MCPPS5_OUTV.AVER / (NUM_SR - STICS_MCPPS5_OUTV.NZERO)
      ENDIF
      CALL POLY( STICS_MCPPS5_OUTV, -7.293, 1.969E1, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1100)'     STICS MCPPS 5 Output Voltage', STICS_MCPPS5_OUTV.MIN, STICS_MCPPS5_OUTV.MAX, 
     &               STICS_MCPPS5_OUTV.AVER, STICS_MCPPS5_OUTV.NZERO, STICS_MCPPS_5_CL, STICS_MCPPS_5_CV 
C
C STICS TOFPS (MCPPS 6) Output Voltage
C
      IF((NUM_SR - STICS_TOFPS_OUTV.NZERO).LE.0)THEN
        STICS_TOFPS_OUTV.AVER = -999
      ELSE
        STICS_TOFPS_OUTV.AVER = STICS_TOFPS_OUTV.AVER / (NUM_SR - STICS_TOFPS_OUTV.NZERO)
      ENDIF
      CALL POLY( STICS_TOFPS_OUTV, -5.518, 1.961E1, 0.0, 0.0, 0.0, 0.0)
      WRITE(11, 1100)'     STICS TOFPS (MCPPS 6) Output Voltage', STICS_TOFPS_OUTV.MIN, STICS_TOFPS_OUTV.MAX,
     &                STICS_TOFPS_OUTV.AVER, STICS_TOFPS_OUTV.NZERO, STICS_TOFPS_MCPPS_6_CL, STICS_TOFPS_MCPPS_6_CV 
C
C STICS T1, AE Housing Temp
C
      IF((NUM_SR - STICS_T1_AEHT.NZERO).LE.0)THEN
        STICS_T1_AEHT.AVER = -999
      ELSE
        STICS_T1_AEHT.AVER = STICS_T1_AEHT.AVER / (NUM_SR - STICS_T1_AEHT.NZERO)
      ENDIF
      CALL POLY( STICS_T1_AEHT, 7.886257E1, -1.848392, 2.143749E-2, -1.483171E-4, 5.149126E-7, -7.140936E-10)
      WRITE(11, 1000)' 60  STICS T1, AE Housing Temp', STICS_T1_AEHT.MIN, STICS_T1_AEHT.MAX, 
     &               STICS_T1_AEHT.AVER, STICS_T1_AEHT.NZERO
C
C STICS T2, Fairing/HOP Temp
C
      IF((NUM_SR - STICS_T2_FHOPT.NZERO).LE.0)THEN
        STICS_T2_FHOPT.AVER = -999
      ELSE
        STICS_T2_FHOPT.AVER = STICS_T2_FHOPT.AVER / (NUM_SR - STICS_T2_FHOPT.NZERO)
      ENDIF
      CALL POLY( STICS_T2_FHOPT, 7.886257E1, -1.848392, 2.143749E-2, -1.483171E-4, 5.149126E-7, -7.140936E-10)
      WRITE(11, 1000)'     STICS T2, Fairing/HOP Temp', STICS_T2_FHOPT.MIN, STICS_T2_FHOPT.MAX, 
     &               STICS_T2_FHOPT.AVER, STICS_T2_FHOPT.NZERO
C
C STICS T3, SSD Temp
C
      IF((NUM_SR - STICS_T3_SSDT.NZERO).LE.0)THEN
        STICS_T3_SSDT.AVER = -999
      ELSE
        STICS_T3_SSDT.AVER = STICS_T3_SSDT.AVER / (NUM_SR - STICS_T3_SSDT.NZERO)
      ENDIF
      CALL POLY( STICS_T3_SSDT, 7.886257E1, -1.848392, 2.143749E-2, -1.483171E-4, 5.149126E-7, -7.140936E-10)
      WRITE(11, 1000)' 61  STICS T3, SSD Temp', STICS_T3_SSDT.MIN, STICS_T3_SSDT.MAX, STICS_T3_SSDT.AVER, STICS_T3_SSDT.NZERO
C
C STICS T4, Back Panel Temp
C
      IF((NUM_SR - STICS_T4_BPT.NZERO).LE.0)THEN
        STICS_T4_BPT.AVER = -999
      ELSE
        STICS_T4_BPT.AVER = STICS_T4_BPT.AVER / (NUM_SR - STICS_T4_BPT.NZERO)
      ENDIF
      CALL POLY( STICS_T4_BPT, 7.886257E1, -1.848392, 2.143749E-2, -1.483171E-4, 5.149126E-7, -7.140936E-10)
      WRITE(11, 1000)'     STICS T4, Back Panel Temp', STICS_T4_BPT.MIN, STICS_T4_BPT.MAX, STICS_T4_BPT.AVER, STICS_T4_BPT.NZERO
C
C From last good HDB, write Spacecraft Status information. First, Telemetry Format...
C
      WRITE(11, 1500)
1500  FORMAT(/,10X,'SPACECRAFT AND DPU STATUS ITEMS',/)
C      
C Build the next output line internally in the character variable LINE.  It
C will contain the telemetry format, telemetry rate, and spin rate.
C
      HDB = LAST_GOOD_HDB
      IF(BTEST(HDB.DAT(2), 6))THEN
        WRITE(LINE, 2000)
2000    FORMAT(1X,'     Telemetry Format: Science Mode ')
      ELSE
        WRITE(LINE, 2010)
2010    FORMAT(1X,'     Telemetry Format: Maneuver Mode')
      ENDIF
C
C...then Telemetry Rate
C
      IF(BTEST(HDB.DAT(2), 7))THEN
        WRITE(LINE(38:), 2100)
2100    FORMAT(1X,'     Telemetry Rate 11130 BPS')
      ELSE
        WRITE(LINE(38:), 2110)
2110    FORMAT(1X,'     Telemetry Rate 5565 BPS ')
      ENDIF
C
C Get spin period by taking (start time of spin 59 - start time of spin 0)/59, 
C convert it from ms to sec, and write it out
C
      SPIN_PERIOD = (SCI.EDB(59).EPOCH - SCI.EDB(0).EPOCH)/59000.0
      WRITE(LINE(68:), 2200) SPIN_PERIOD
2200  FORMAT(1X,'     Spin Rate: ',F5.2)
C
C Now output the line we've built internally
C
      WRITE(11, 2205) LINE(1:89)
2205  FORMAT(A)
C
C DPU status Items: 
C
      WRITE(11,2300) IAND(HDB.DAT(188), 63)
2300  FORMAT(1X,'     Reset Counter: ',I2)
C
      RESET_CAUSE = ISHFT(IAND(HDB.DAT(188), 192), -6)
      WRITE(11,2310) RESET_CAUSE, RESET_CAUSE_TEXT(RESET_CAUSE)
2310  FORMAT(1X,'     Last Reset Cause: ',I2,' = ',A)
C
      WRITE(11,2320)  IAND(HDB.DAT(236), 15)
2320  FORMAT(1X,'     Latchup Counter - Main: ',I2)
C
      WRITE(11,2330) IAND(HDB.DAT(234), 15)
2330  FORMAT(1X,'     Latchup Counter for Memory: ',I2)
C
      WRITE(11,2340) ISHFT(IAND(HDB.DAT(234), 240), -4)
2340  FORMAT(1X,'     Latchup Counter for MP: ',I2)
C
      WRITE(11,2350) IAND(HDB.DAT(235), 15)
2350  FORMAT(1X,'     Latchup Counter for SWICS Classification',I2)
C
      WRITE(11,2360) ISHFT(IAND(HDB.DAT(235), 240), -4)
2360  FORMAT(1X,'     Latchup Counter for STICS Classification: ',I2)
C
      WRITE(11,2370) ISHFT(IAND(HDB.DAT(236), 240), -4)
2370  FORMAT(1X,'     Latchup Counter for MASS Classification: ',I2)
C
      WRITE(11,2400) IAND(HDB.DAT(169), 255)
2400  FORMAT(1X,'     RAM Error Counter: ',I2)
C
      WRITE(11,2410) IAND(HDB.DAT(170), 255)
2410  FORMAT(1X,'     ROM Error Counter: ',I2)
C
      IF(BTEST(HDB.DAT(186),3))THEN
        WRITE(11, 2500)
2500    FORMAT(1X,'     PROM Bank Selection: B')
      ELSE
        WRITE(11, 2510)
2510    FORMAT(1X,'     PROM Bank Selection: A')
      ENDIF
C
      IF(BTEST(HDB.DAT(186),4))THEN
        WRITE(11, 2520)
2520    FORMAT(1X,'     PROM Toggle Mode Enabled')
      ELSE
        WRITE(11, 2530)
2530    FORMAT(1X,'     PROM Toggle Mode Disabled')
      ENDIF
C
      IF(BTEST(HDB.DAT(190),6))THEN
        WRITE(11, 2600)
2600    FORMAT(1X,'     Program Patch Active')
      ELSE
        WRITE(11, 2610)
2610    FORMAT(1X,'     Program Patch Inactive')
      ENDIF
C
      WRITE(11, 2700) IAND(HDB.DAT(176), 255)
2700  FORMAT(1X,'     Valid Command Counter: ', I3)
      WRITE(11, 2710) IAND(HDB.DAT(180), 255)
2710  FORMAT(1X,'     Invalid Command Counter: ', I3)
C
      IF(BTEST(HDB.DAT(221),4))THEN
        WRITE(11, 2800)
2800    FORMAT(1X,'     Command Repetition for SWICS: Enabled')
      ELSE
        WRITE(11, 2810)
2810    FORMAT(1X,'     Command Repetition for SWICS: Disabled')
      ENDIF
C
      IF(BTEST(HDB.DAT(221),3))THEN
        WRITE(11, 2820)
2820    FORMAT(1X,'     Command Repetition for MASS: Enabled')
      ELSE
        WRITE(11, 2830)
2830    FORMAT(1X,'     Command Repetition for MASS: Disabled')
      ENDIF
C
      IF(BTEST(HDB.DAT(221),5))THEN
        WRITE(11, 2840)
2840    FORMAT(1X,'     Command Repetition for STICS: Enabled')
      ELSE
        WRITE(11, 2850)
2850    FORMAT(1X,'     Command Repetition for STICS: Disabled')
      ENDIF
C
      IF(BTEST(HDB.DAT(189),7))THEN
        WRITE(11, 2900)
2900    FORMAT(1X,'     High Voltage Reconfiguration with HV')
      ELSE
        WRITE(11, 2910)
2910    FORMAT(1X,'     High Voltage Reconfiguration without HV')
      ENDIF
C
      WRITE(11,3000) IAND(HDB.DAT(165), 15), OFFSET
3000  FORMAT(1X,'     Sun Sector/Offset: ',I2,'/',I3)
C
      IF(BTEST(HDB.DAT(190),0))THEN
        WRITE(11, 3020)
3020    FORMAT(1X,'     Sun Sector Clock Source: Internal')
      ELSE
        WRITE(11, 3030)
3030    FORMAT(1X,'     Sun Sector Clock Source: External')
      ENDIF
C
      IF(BTEST(HDB.DAT(190),3))THEN
        WRITE(11, 3040)
3040    FORMAT(1X,'     Sun Sector Clock Lost')
      ELSE
        WRITE(11, 3050)
3050    FORMAT(1X,'     Sun Sector Clock OK')
      ENDIF
C
      IF(BTEST(HDB.DAT(190),4))THEN
        WRITE(11, 3060)
3060    FORMAT(1X,'     Sun Pulse Lost')
      ELSE
        WRITE(11, 3070)
3070    FORMAT(1X,'     Sun Pulse OK')
      ENDIF
C
C Prepare Power Switch Status Table
C
      IF(BTEST(HDB.DAT(173),0))STICS_COVER  = 'ON '
      IF(BTEST(HDB.DAT(173),1))STICS_HEATER = 'ON '
      IF(BTEST(HDB.DAT(173),2))SWICS_COVER  = 'ON '
      IF(BTEST(HDB.DAT(173),3))SWICS_HEATER = 'ON '
      IF(BTEST(HDB.DAT(173),4))MASS_COVER   = 'ON '
      IF(BTEST(HDB.DAT(173),5))STICS_MAIN   = 'ON '
      IF(BTEST(HDB.DAT(173),6))SWICS_MAIN   = 'ON ' 
      IF(BTEST(HDB.DAT(173),7))MASS_MAIN    = 'ON '
C
C Print Power Switch Status Table
C
      WRITE(11,3100) STICS_MAIN, STICS_COVER, STICS_HEATER , SWICS_MAIN, SWICS_COVER, SWICS_HEATER, 
     &               MASS_MAIN, MASS_COVER
3100  FORMAT(1X,//,15X,'POWER SWITCH STATUS'//,11X,'|  Main Power  |  Cover Power | Heater Power |',/,
     &       5X,'----------------------------------------------------',/,
     &       5X,'STICS |      ',A3,'     |      ',A3,'     |      ',A3,'     |',/,
     &       5X,'SWICS |      ',A3,'     |      ',A3,'     |      ',A3,'     |',/,
     &       5X,'MASS  |      ',A3,'     |      ',A3,'     |       -      |')
C
C Prepare table of Configuration Errors
C
      IF(BTEST(HDB.DAT(187),0))STICS_CONFIG   = 'Error '
      IF(BTEST(HDB.DAT(187),1))STICS_SNAPSHOT = 'Error '
      IF(BTEST(HDB.DAT(187),2))SWICS_CONFIG   = 'Error '
      IF(BTEST(HDB.DAT(187),3))SWICS_SNAPSHOT = 'Error '
      IF(BTEST(HDB.DAT(187),4))MASS_CONFIG    = 'Error '
      IF(BTEST(HDB.DAT(187),5))MASS_SNAPSHOT  = 'Error '
      IF(BTEST(HDB.DAT(187),6))LOAD_CONFIG    = 'Error '
      IF(BTEST(HDB.DAT(187),7))LOAD_SNAPSHOT  = 'Error '
C
C Print table of Configuration Errors
C
      WRITE(11,3200) STICS_CONFIG, STICS_SNAPSHOT, SWICS_CONFIG, SWICS_SNAPSHOT, MASS_CONFIG, 
     &               MASS_SNAPSHOT, LOAD_CONFIG, LOAD_SNAPSHOT 
3200  FORMAT(1X,//,10X,'CONFIGURATION ERRORS'//,11X,'| Configuration |    Snapshot   |',/,
     &       5X,'--------------------------------------',/,
     &       5X, 'STICS |     ',A6,'    |     ',A6,'    |',/,5X,'SWICS |     ',A6,'    |     ',A6,'    |',/,
     &       5X, 'MASS  |     ',A6,'    |     ',A6,'    |',/,5X,'LOAD  |     ',A6,'    |     ',A6,'    |')
C
C MASS status items
C
      WRITE(11, 3500)
3500  FORMAT(///,10X,'MASS STATUS ITEMS',/)
C
      IF(BTEST(HDB.DAT(107),0))MASS_HPS       = 'ON '
      IF(BTEST(HDB.DAT(107),1))MASS_STOP_MCP  = 'ON '
      IF(BTEST(HDB.DAT(107),2))MASS_START_MCP = 'ON '
      IF(BTEST(HDB.DAT(107),3))MASS_COM_ADPS  = 'ON '
      IF(BTEST(HDB.DAT(107),4))MASS_COM_DPPS  = 'ON '
C      
      MASS_HPS_MEAS_V = -3.428E1 + 1.5285E2 * IAND(HDB.DAT(117), 255)
      MASS_STOP_MCP_MEAS_V = 3.065E1 * IAND(HDB.DAT(120), 255)
      MASS_START_MCP_MEAS_V = 1.310E1 + 1.431E1 * IAND(HDB.DAT(119), 255)
C
      WRITE(11, 4000) MASS_HPS, IAND(HDB.DAT(111), 255), IAND(HDB.DAT(110), 255), MASS_HPS_MEAS_V,
     &                MASS_STOP_MCP, IAND(HDB.DAT(115), 255), IAND(HDB.DAT(114), 255), MASS_STOP_MCP_MEAS_V, 
     &                MASS_START_MCP, IAND(HDB.DAT(113), 255), IAND(HDB.DAT(112), 255), MASS_START_MCP_MEAS_V, 
     &                MASS_COM_ADPS, IAND(HDB.DAT(232), 255), IAND(HDB.DAT(233), 255), 
     &                MASS_COM_DPPS
4000  FORMAT(1X,//,10X, '       ENABLE    LIMIT     LEVEL   MEASURED VOLTAGE',/,
     &       5X,  '--------------------------------------------------------',/,
     &       5X,  'HPS       |  ',A3,'   |   ',I3,'   |   ', I3,'   |',F12.2,'   |',/,
     &       5X,  'STOP MCP  |  ',A3,'   |   ',I3,'   |   ', I3,'   |',F12.2,'   |',/,
     &       5X,  'START MCP |  ',A3,'   |   ',I3,'   |   ', I3,'   |',F12.2,'   |',/,
     &       5X,  'ADPS      |  ',A3,'   | ',I3,',',I3,' |    -    |        -      |',/,
     &       5X,  'DPPS      |  ',A3,'   |    -    |    -    |        -      |',/)
C
      IF(BTEST(HDB.DAT(109),6))THEN
        WRITE(11, 4100)
4100    FORMAT(1X,'     ADPS automatic calculation: ON')
      ELSE
        WRITE(11, 4110)
4110    FORMAT(1X,'     ADPS automatic calculation: OFF')
      ENDIF
C
      IF(BTEST(HDB.DAT(130),0))THEN
        WRITE(11, 4120)
4120    FORMAT(1X,'     ADPS stepping sequence: RAM')
      ELSE
        WRITE(11, 4130)
4130    FORMAT(1X,'     ADPS stepping sequence: ROM')
      ENDIF
C
      IF(BTEST(HDB.DAT(130),6))THEN
        WRITE(11, 4140)
4140    FORMAT(1X,'     ADPS table: Fixed')
      ELSE
        WRITE(11, 4150)
4150    FORMAT(1X,'     ADPS table: Not Fixed')
      ENDIF
C
      IF(BTEST(HDB.DAT(130),1))THEN
        WRITE(11, 4160)
4160    FORMAT(1X,'     DPPS stepping sequence: RAM')
      ELSE
        WRITE(11, 4170)
4170    FORMAT(1X,'     DPPS stepping sequence: ROM')
      ENDIF
C
      WRITE(11, 4200) IAND(HDB.DAT(133), 255)
4200  FORMAT(1X,'     FSR limit for stop reversal: ',I3)
C
      IF(BTEST(HDB.DAT(223),0))THEN
        WRITE(11, 4300)
4300    FORMAT(1X,'     Discharge occurred')
      ELSE
        WRITE(11, 4310)
4310    FORMAT(1X,'     No discharge')
      ENDIF
C
      IF(BTEST(HDB.DAT(130),5))THEN
        WRITE(11, 4400)
4400    FORMAT(1X,'     Internal callibrator: Active')
      ELSE
        WRITE(11, 4410)
4410    FORMAT(1X,'     Internal Callibrator: Inactive')
      ENDIF
C
      WRITE(11, 4420) IAND(HDB.DAT(116), 15)
4420  FORMAT(1X,'     Callibrator level: ',I2)
C
      IF(BTEST(HDB.DAT(130),4))THEN
        WRITE(11, 4500)
4500    FORMAT(1X,'     Test tables: Active')
      ELSE
        WRITE(11, 4510)
4510    FORMAT(1X,'     Test tables: Inactive')
      ENDIF
C
      IF(BTEST(HDB.DAT(130),7))THEN
        WRITE(11, 4600)
4600    FORMAT(1X,'     Command Echo Check: Enabled')
      ELSE
        WRITE(11, 4610)
4610    FORMAT(1X,'     Command Echo Check: Disabled')
      ENDIF
C
      IF(BTEST(HDB.DAT(130),3))THEN
        WRITE(11, 4700)
4700    FORMAT(1X,'     PHA FIFO mode: Enabled')
      ELSE
        WRITE(11, 4710)
4710    FORMAT(1X,'     PHA FIFO mode: Disabled')
      ENDIF
C
      IF(BTEST(HDB.DAT(116),5))THEN
        WRITE(11, 4800)
4800    FORMAT(1X,'     Stop preamp power: OFF')
      ELSE
        WRITE(11, 4810)
4810    FORMAT(1X,'     Stop preamp power: ON')
      ENDIF
C
      IF(BTEST(HDB.DAT(116),6))THEN
        WRITE(11, 4820)
4820    FORMAT(1X,'     Start preamp 1 power: OFF')
      ELSE
        WRITE(11, 4830)
4830    FORMAT(1X,'     Start preamp 1 power: ON')
      ENDIF
C
      IF(BTEST(HDB.DAT(116),7))THEN
        WRITE(11, 4840)
4840    FORMAT(1X,'     Start preamp 2 power: OFF')
      ELSE
        WRITE(11, 4850)
4850    FORMAT(1X,'     Start preamp 2 power: ON')
      ENDIF
C
      IF(BTEST(HDB.DAT(172),0))THEN
        WRITE(11, 4900)
4900    FORMAT(1X,'     Multiple Stops: Enabled')
      ELSE
        WRITE(11, 4910)
4910    FORMAT(1X,'     Multiple Stops: Disabled')
      ENDIF
C
      IF(BTEST(HDB.DAT(172),1))THEN
        WRITE(11, 4920)
4920    FORMAT(1X,'     Multiple Starts: Enabled')
      ELSE
        WRITE(11, 4930)
4930    FORMAT(1X,'     Multiple Starts: Disabled')
      ENDIF
C
      MASS_SUN_SECTOR_DEFINITIONS='0000000000000000'
      DO 5000 I=1,8
        IF(BTEST(HDB.DAT(132), I-1)) MASS_SUN_SECTOR_DEFINITIONS(I:I)='1'
5000  CONTINUE
      DO 5010 I=1,8
        IF(BTEST(HDB.DAT(131), I-1)) MASS_SUN_SECTOR_DEFINITIONS(I+8:I+8)='1'
5010  CONTINUE
      WRITE(11, 5020)MASS_SUN_SECTOR_DEFINITIONS
5020  FORMAT(1X,'     MASS sun sector definition: ', A)
C
C Create Matrix Rate table 
C
      DO 5100 I=0, 9
        CALL MVBITS(HDB.DAT(134+3*I), 0, 8, MR(1, I), 4)
        CALL MVBITS(HDB.DAT(135+3*I), 4, 4, MR(1, I), 0)
        CALL MVBITS(HDB.DAT(135+3*I), 0, 4, MR(2, I), 8)
        CALL MVBITS(HDB.DAT(136+3*I), 0, 8, MR(2, I), 0)
5100  CONTINUE
C
      WRITE(11, 5110) MR
5110  FORMAT(1X,/,5X,'MATRIX RATE BORDERS',/,10X, 'LOWER     UPPER',/,
     &       5X,  '---------------------',/,
     &       5X,  'MR0 | ',I4,'  | ',I4,'  |',/,
     &       5X,  'MR1 | ',I4,'  | ',I4,'  |',/,
     &       5X,  'MR2 | ',I4,'  | ',I4,'  |',/,
     &       5X,  'MR3 | ',I4,'  | ',I4,'  |',/,
     &       5X,  'MR4 | ',I4,'  | ',I4,'  |',/,
     &       5X,  'MR5 | ',I4,'  | ',I4,'  |',/,
     &       5X,  'MR6 | ',I4,'  | ',I4,'  |',/,
     &       5X,  'MR7 | ',I4,'  | ',I4,'  |',/,
     &       5X,  'MR8 | ',I4,'  | ',I4,'  |',/,
     &       5X,  'MR9 | ',I4,'  | ',I4,'  |',/,
     &       5X,  '---------------------',/)
C
      CALL MVBITS(HDB.DAT(164), 0, 8, BR0_BR1_BORDER, 4)
      CALL MVBITS(HDB.DAT(165), 4, 4, BR0_BR1_BORDER, 0)
C
      WRITE(11, 5120) BR0_BR1_BORDER
5120  FORMAT(1X,'     BR0/BR1 border: ',I4)
C
      IF(BTEST(HDB.DAT(189),4))THEN
        COMP_CODE(1) = 'C'
      ELSE
        COMP_CODE(1) = 'A'
      ENDIF
C
      IF(BTEST(HDB.DAT(189),5))THEN
        COMP_CODE(2) = 'C'
      ELSE
        COMP_CODE(2) = 'A'
      ENDIF
C
      WRITE(11, 5200) COMP_CODE
5200  FORMAT(1X,'     Compression code for rate groups 1 and 2: ',A,',', A) 
C
C STICS status items
C
      WRITE(11, 5300)
5300  FORMAT(//,10X,'STICS STATUS ITEMS',/)
C
      DO 5400 I=0,5
        IF(BTEST(HDB.DAT(24),I))STICS_MCPPS(I) = 'ON '
5400  CONTINUE
C     
      IF(BTEST(HDB.DAT(24),6))STICS_TOFPS  = 'ON '
      IF(BTEST(HDB.DAT(24),7))STICS_PLUS_DPPS  = 'ON '
      IF(BTEST(HDB.DAT(25),0))STICS_MINUS_DPPS  = 'ON '
C      
      STICS_MCPPS_MEAS_V(0) = -7.806   + 1.959E1 * IAND(HDB.DAT(27), 255)
      STICS_MCPPS_MEAS_V(1) = -3.475   + 1.950E1 * IAND(HDB.DAT(28), 255)
      STICS_MCPPS_MEAS_V(2) = -9.405   + 1.957E1 * IAND(HDB.DAT(29), 255)
      STICS_MCPPS_MEAS_V(3) = -1.950E1 + 1.984E1 * IAND(HDB.DAT(30), 255)
      STICS_MCPPS_MEAS_V(4) = -1.436E1 + 1.984E1 * IAND(HDB.DAT(31), 255)
      STICS_MCPPS_MEAS_V(5) = -7.293   + 1.969E1 * IAND(HDB.DAT(32), 255)
      STICS_TOFPS_MEAS_V    = -5.518   + 1.961E1 * IAND(HDB.DAT(33), 255)
C
      WRITE(11, 5500) STICS_PLUS_DPPS, 
     &                STICS_MINUS_DPPS,
     &                STICS_MCPPS(0), IAND(HDB.DAT(10), 255), IAND(HDB.DAT(17), 255), STICS_MCPPS_MEAS_V(0), 
     &                STICS_MCPPS(1), IAND(HDB.DAT(11), 255), IAND(HDB.DAT(18), 255), STICS_MCPPS_MEAS_V(1), 
     &                STICS_MCPPS(2), IAND(HDB.DAT(12), 255), IAND(HDB.DAT(19), 255), STICS_MCPPS_MEAS_V(2), 
     &                STICS_MCPPS(3), IAND(HDB.DAT(13), 255), IAND(HDB.DAT(20), 255), STICS_MCPPS_MEAS_V(3), 
     &                STICS_MCPPS(4), IAND(HDB.DAT(14), 255), IAND(HDB.DAT(21), 255), STICS_MCPPS_MEAS_V(4), 
     &                STICS_MCPPS(5), IAND(HDB.DAT(15), 255), IAND(HDB.DAT(22), 255), STICS_MCPPS_MEAS_V(5), 
     &                STICS_TOFPS, IAND(HDB.DAT(16), 255), IAND(HDB.DAT(23), 255), STICS_TOFPS_MEAS_V
5500  FORMAT(1X,//,10X, '       ENABLE    LIMIT     LEVEL   MEASURED VOLTAGE',/,
     &       5X,  '--------------------------------------------------------',/,
     &       5X,  '+DPPS     |  ',A3,'   |    -    |    -    |        -      |',/,
     &       5X,  '-DPPS     |  ',A3,'   |    -    |    -    |        -      |',/,
     &       5X,  'MCPPS 0   |  ',A3,'   |   ',I3,'   |   ', I3,'   |',F12.2,'   |',/,
     &       5X,  'MCPPS 1   |  ',A3,'   |   ',I3,'   |   ', I3,'   |',F12.2,'   |',/,
     &       5X,  'MCPPS 2   |  ',A3,'   |   ',I3,'   |   ', I3,'   |',F12.2,'   |',/,
     &       5X,  'MCPPS 3   |  ',A3,'   |   ',I3,'   |   ', I3,'   |',F12.2,'   |',/,
     &       5X,  'MCPPS 4   |  ',A3,'   |   ',I3,'   |   ', I3,'   |',F12.2,'   |',/,
     &       5X,  'MCPPS 5   |  ',A3,'   |   ',I3,'   |   ', I3,'   |',F12.2,'   |',/,
     &       5X,  'TOFPS(6)  |  ',A3,'   |   ',I3,'   |   ', I3,'   |',F12.2,'   |',/)
C
      IF(ISHFT(IAND(HDB.DAT(25),192), -6).EQ.0)THEN
        WRITE(11, 5600)
5600    FORMAT(1X,'     DPPS Stepping Sequence: ROM, Fine Mode')
      ELSEIF(ISHFT(IAND(HDB.DAT(25),192), -6).EQ.1)THEN
        WRITE(11, 5610)
5610    FORMAT(1X,'     DPPS Stepping Sequence: ROM, Medium Mode')
      ELSEIF(ISHFT(IAND(HDB.DAT(25),192), -6).EQ.2)THEN
        WRITE(11, 5620)
5620    FORMAT(1X,'     DPPS Stepping Sequence: ROM, Fast Mode')
      ELSEIF(ISHFT(IAND(HDB.DAT(25),192), -6).EQ.3)THEN
        WRITE(11, 5630)
5630    FORMAT(1X,'     DPPS Stepping Sequence: RAM Table')
      ENDIF
C
      IF(BTEST(HDB.DAT(26),1))THEN
        WRITE(11, 5700)
5700    FORMAT(1X,'     Internal Callibrator power: ON')
      ELSE
        WRITE(11, 5710)
5710    FORMAT(1X,'     Internal Callibrator power: OFF')
      ENDIF
C
      IF(BTEST(HDB.DAT(26),0))THEN
        WRITE(11, 5720)
5720    FORMAT(1X,'     Auto Callibrate Sequence: Enabled')
      ELSE
        WRITE(11, 5730)
5730    FORMAT(1X,'     Auto Callibrate Sequence: Disabled')
      ENDIF
C
      IF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.0)THEN
        WRITE(11, 5740)
5740    FORMAT(1X,'     Commanded Time Callibrator Level: 300 ns')
      ELSEIF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.1)THEN
        WRITE(11, 5750)
5750    FORMAT(1X,'     Commanded Time Callibrator Level: 280 ns')
      ELSEIF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.2)THEN
        WRITE(11, 5760)
5760    FORMAT(1X,'     Commanded Time Callibrator Level: 260 ns')
      ELSEIF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.3)THEN
        WRITE(11, 5770)
5770    FORMAT(1X,'     Commanded Time Callibrator Level: 240 ns')
      ELSEIF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.4)THEN
        WRITE(11, 5780)
5780    FORMAT(1X,'     Commanded Time Callibrator Level: 220 ns')
      ELSEIF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.5)THEN
        WRITE(11, 5790)
5790    FORMAT(1X,'     Commanded Time Callibrator Level: 200 ns')
      ELSEIF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.6)THEN
        WRITE(11, 5800)
5800    FORMAT(1X,'     Commanded Time Callibrator Level: 180 ns')
      ELSEIF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.7)THEN
        WRITE(11, 5810)
5810    FORMAT(1X,'     Commanded Time Callibrator Level: 160 ns')
      ELSEIF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.8)THEN
        WRITE(11, 5820)
5820    FORMAT(1X,'     Commanded Time Callibrator Level: 140 ns')
      ELSEIF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.9)THEN
        WRITE(11, 5830)
5830    FORMAT(1X,'     Commanded Time Callibrator Level: 120 ns')
      ELSEIF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.10)THEN
        WRITE(11, 5840)
5840    FORMAT(1X,'     Commanded Time Callibrator Level: 100 ns')
      ELSEIF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.11)THEN
        WRITE(11, 5850)
5850    FORMAT(1X,'     Commanded Time Callibrator Level: 80 ns')
      ELSEIF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.12)THEN
        WRITE(11, 5860)
5860    FORMAT(1X,'     Commanded Time Callibrator Level: 60 ns')
      ELSEIF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.13)THEN
        WRITE(11, 5870)
5870    FORMAT(1X,'     Commanded Time Callibrator Level: 40 ns')
      ELSEIF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.14)THEN
        WRITE(11, 5880)
5880    FORMAT(1X,'     Commanded Time Callibrator Level: 20 ns')
      ELSEIF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.15)THEN
        WRITE(11, 5890)
5890    FORMAT(1X,'     Commanded Time Callibrator Level: Invalid')
      ENDIF
C
      WRITE(11, 6000) ZEXT(HDB.DAT(54))
6000  FORMAT(1X,'     Energy Callibrator Level: ',I3) 
C
      IF(BTEST(HDB.DAT(26),7))THEN
        WRITE(11, 6100)
6100    FORMAT(1X,'     Command Echo Check: Enabled')
      ELSE
        WRITE(11, 6110)
6110    FORMAT(1X,'     Command Echo Check: Disabled')
      ENDIF
C
      IF(BTEST(HDB.DAT(26),2))THEN
        WRITE(11, 6200)
6200    FORMAT(1X,'     Rate Mode: Low')
      ELSE
        WRITE(11, 6210)
6210    FORMAT(1X,'     Rate Mode: High')
      ENDIF
C
      IF(BTEST(HDB.DAT(221),7))THEN
        WRITE(11, 6230)
6230    FORMAT(1X,'     PHA FIFO Mode: Enabled')
      ELSE
        WRITE(11, 6240)
6240    FORMAT(1X,'     PHA FIFO Mode: Disabled')
      ENDIF
C
      IF(BTEST(STICS_RATE.EDB(0).RATE(179),7))THEN
        WRITE(11, 6250)
6250    FORMAT(1X,'     Data Mode: Diagnostic(TGS)')
      ELSE
        WRITE(11, 6260)
6260    FORMAT(1X,'     Data Mode: Normal')
      ENDIF
C
      IF(BTEST(HDB.DAT(26),3))THEN
        WRITE(11, 6270)
6270    FORMAT(1X,'     Main SSD Bias: OFF')
      ELSE
        WRITE(11, 6280)
6280    FORMAT(1X,'     Main SSD Bias: ON')
      ENDIF
C
      IF(BTEST(HDB.DAT(55),2))THEN
        WRITE(11, 6300)
6300    FORMAT(1X,'     Time Preamp "Telescope 1": ON')
      ELSE
        WRITE(11, 6310)
6310    FORMAT(1X,'     Time Preamp "Telescope 1": OFF')
      ENDIF
C
      IF(BTEST(HDB.DAT(55),3))THEN
        WRITE(11, 6320)
6320    FORMAT(1X,'     Time Preamp Start 3: ON')
      ELSE
        WRITE(11, 6330)
6330    FORMAT(1X,'     Time Preamp Start 3: OFF')
      ENDIF
C
      IF(BTEST(HDB.DAT(55),4))THEN
        WRITE(11, 6340)
6340    FORMAT(1X,'     Time Preamp Start 4: ON')
      ELSE
        WRITE(11, 6350)
6350    FORMAT(1X,'     Time Preamp Start 4: OFF')
      ENDIF
C
      IF(BTEST(HDB.DAT(55),1))THEN
        WRITE(11, 6360)
6360    FORMAT(1X,'     Time Preamps "Telescope 3": ON')
      ELSE
        WRITE(11, 6370)
6370    FORMAT(1X,'     Time Preamps "Telescope 3": OFF')
      ENDIF
C
      IF(BTEST(HDB.DAT(55),0))THEN
        WRITE(11, 6380)
6380    FORMAT(1X,'     Time Preamp Stop 2: ON')
      ELSE
        WRITE(11, 6390)
6390    FORMAT(1X,'     Time Preamp Stop 2: OFF')
      ENDIF
C
      IF(BTEST(HDB.DAT(53),0))THEN
        WRITE(11, 6400)
6400    FORMAT(1X,'     Multiple Starts: Enabled')
      ELSE
        WRITE(11, 6410)
6410    FORMAT(1X,'     Multiple Starts: Disabled')
      ENDIF
C
      IF(BTEST(HDB.DAT(53),1))THEN
        WRITE(11, 6420)
6420    FORMAT(1X,'     Multiple Stops: Enabled')
      ELSE
        WRITE(11, 6430)
6430    FORMAT(1X,'     Multiple Stops: Disabled')
      ENDIF
C
      CALL MVBITS(HDB.DAT(25), 1, 3, STICS_TRIGGER_MODE, 0)
      IF(STICS_TRIGGER_MODE.EQ.0)THEN
        WRITE(11, 6500)
6500    FORMAT(1X,'     Trigger Mode: 0 = En')
      ELSEIF(STICS_TRIGGER_MODE.EQ.1)THEN
        WRITE(11, 6510)
6510    FORMAT(1X,'     Trigger Mode: 1 = Tn')
      ELSEIF(STICS_TRIGGER_MODE.EQ.2)THEN
        WRITE(11, 6520)
6520    FORMAT(1X,'     Trigger Mode: 2 = (Tn & !En)|(Tn & En)')
      ELSEIF(STICS_TRIGGER_MODE.EQ.3)THEN
        WRITE(11, 6530)
6530    FORMAT(1X,'     Trigger Mode: 3 = Tn & En')
      ELSEIF(STICS_TRIGGER_MODE.EQ.4)THEN
        WRITE(11, 6540)
6540    FORMAT(1X,'     Trigger Mode: 4 = E|T')
      ELSEIF(STICS_TRIGGER_MODE.EQ.5)THEN
        WRITE(11, 6550)
6550    FORMAT(1X,'     Trigger Mode: 5 = (E & !T)|(Tn & !E)|(Tn & En)')
      ELSEIF(STICS_TRIGGER_MODE.EQ.6)THEN
        WRITE(11, 6560)
6560    FORMAT(1X,'     Trigger Mode: 6 = Cal & (E|T)')
      ELSEIF(STICS_TRIGGER_MODE.EQ.7)THEN
        WRITE(11, 6570)
6570    FORMAT(1X,'     Trigger Mode: 7 = (E & !T)|(Tn & En)')
      ENDIF
C
      IF(BTEST(HDB.DAT(189),0))THEN
        COMP_CODE(1) = 'C'
      ELSE
        COMP_CODE(1) = 'A'
      ENDIF
C
      IF(BTEST(HDB.DAT(189),1))THEN
        COMP_CODE(2) = 'C'
      ELSE
        COMP_CODE(2) = 'A'
      ENDIF
C
      WRITE(11, 6600) COMP_CODE
6600  FORMAT(1X,'     Compression code for rate groups 1 and 2: ',A,',', A) 
C
      IF(BTEST(HDB.DAT(26), 4)) SSD_1_BIAS = 'OFF'      ! For the SSD biases, 0 = ON
      IF(BTEST(HDB.DAT(26), 5)) SSD_2_BIAS = 'OFF'      ! and 1 = OFF, unlike most
      IF(BTEST(HDB.DAT(26), 6)) SSD_3_BIAS = 'OFF'      ! of the other bit quantities
      IF(BTEST(HDB.DAT(55), 5)) ENERGY_PREAMP_1 = 'ON '
      IF(BTEST(HDB.DAT(55), 6)) ENERGY_PREAMP_2 = 'ON '
      IF(BTEST(HDB.DAT(55), 7)) ENERGY_PREAMP_3 = 'ON '
      CALL MVBITS(HDB.DAT(51), 2, 6, SSD_1_THRESHOLD, 0)
      CALL MVBITS(HDB.DAT(51), 0, 2, SSD_2_THRESHOLD, 4)
      CALL MVBITS(HDB.DAT(52), 4, 4, SSD_2_THRESHOLD, 0)
      CALL MVBITS(HDB.DAT(52), 0, 4, SSD_3_THRESHOLD, 2)
      CALL MVBITS(HDB.DAT(53), 6, 2, SSD_3_THRESHOLD, 0)
      WRITE(11, 6700) SSD_1_BIAS, ENERGY_PREAMP_1, SSD_1_THRESHOLD,
     &                SSD_2_BIAS, ENERGY_PREAMP_2, SSD_2_THRESHOLD,
     &                SSD_3_BIAS, ENERGY_PREAMP_3, SSD_3_THRESHOLD
6700  FORMAT(1X,//,10X, '         BIAS    PREAMP POWER   THRESHOLD LEVEL',/,
     &       5X,  '-----------------------------------------------------',/,
     &       5X,  'SSD 1     |   ',A3,'   |      ',A3,'     |      ', I3,'       |',/,
     &       5X,  'SSD 2     |   ',A3,'   |      ',A3,'     |      ', I3,'       |',/,
     &       5X,  'SSD 3     |   ',A3,'   |      ',A3,'     |      ', I3,'       |',/)
C     
C SWICS Status Items
C
      WRITE(11, 7000)
7000  FORMAT(//,10X,'SWICS STATUS ITEMS',/)
C
      IF(BTEST(HDB.DAT(59),3))SWICS_DPPS_ENABLE  = 'ON '
      IF(BTEST(HDB.DAT(58),7))SWICS_PAPS_ENABLE = 'ON '
      IF(BTEST(HDB.DAT(58),4))SWICS_START_MCPPS_ENABLE = 'ON '
      IF(BTEST(HDB.DAT(58),5))SWICS_STOP_MCPPS_ENABLE = 'ON '
C      
      SWICS_PAPS_MEAS_V        = 4.453E1 - 1.464E2 * ZEXT(HDB.DAT(70))
      SWICS_START_MCPPS_MEAS_V = -4.610  + 1.864E1 * ZEXT(HDB.DAT(77))
      SWICS_STOP_MCPPS_MEAS_V  = 4.650   + 1.356E1 * ZEXT(HDB.DAT(78))
C
      WRITE(11, 7100) SWICS_DPPS_ENABLE, 
     &                SWICS_PAPS_ENABLE, ZEXT(HDB.DAT(222)), ZEXT(HDB.DAT(61)), SWICS_PAPS_MEAS_V, 
     &                SWICS_START_MCPPS_ENABLE, ZEXT(HDB.DAT(64)), ZEXT(HDB.DAT(62)), SWICS_START_MCPPS_MEAS_V, 
     &                SWICS_STOP_MCPPS_ENABLE, ZEXT(HDB.DAT(65)), ZEXT(HDB.DAT(63)), SWICS_START_MCPPS_MEAS_V
7100  FORMAT(1X,//,10X, '         ENABLE    LIMIT     LEVEL   MEASURED VOLTAGE',/,
     &       5X,  '--------------------------------------------------------',/,
     &       5X,  'DPPS        |  ',A3,'   |    -    |    -    |        -      |',/,
     &       5X,  'PAPS        |  ',A3,'   |   ',I3,'   |   ',I3,'   |',F12.2,'   |',/,
     &       5X,  'START MCPPS |  ',A3,'   |   ',I3,'   |   ',I3,'   |',F12.2,'   |',/,
     &       5X,  'STOP MCPPS  |  ',A3,'   |   ',I3,'   |   ',I3,'   |',F12.2,'   |',/)
C
      IF(BTEST(HDB.DAT(89),0))THEN
        WRITE(11, 7200)
7200    FORMAT(1X,'     DPPS Stepping Sequence: RAM')
      ELSE
        WRITE(11, 7210)
7210    FORMAT(1X,'     DPPS Stepping Sequence: ROM')
      ENDIF
C
      IF(BTEST(HDB.DAT(89),2))THEN
        WRITE(11, 7300)
7300    FORMAT(1X,'     Discharge Indicator: Discharge')
      ELSE
        WRITE(11, 7310)
7310    FORMAT(1X,'     Discharge Indicator: No Discharge')
      ENDIF
C
      IF(BTEST(HDB.DAT(89),5))THEN
        WRITE(11, 7400)
7400    FORMAT(1X,'     Callibration: Active')
      ELSE
        WRITE(11, 7410)
7410    FORMAT(1X,'     Callibration: Not Active')
      ENDIF
C
      WRITE(11, 7500)ZEXT(HDB.DAT(57))
7500  FORMAT(1X,'     Energy Callibrator Level: ',I3) 
C
      WRITE(11, 7510)IAND(HDB.DAT(58), 15)
7510  FORMAT(1X,'     Time Callibrator Level: ',I2) 
C
      IF(BTEST(HDB.DAT(58),6))THEN
        WRITE(11, 7600)
7600    FORMAT(1X,'     Main  SSD Bias: OFF')
      ELSE
        WRITE(11, 7610)
7610    FORMAT(1X,'     Main  SSD Bias: ON ')
      ENDIF
C
      DO 7640 I=1,3
        IF(BTEST(HDB.DAT(56),I-1))THEN
          WRITE(11, 7620) I
7620      FORMAT(1X,'     SSD ',I1,' Preamp and Bias: ON ')
        ELSE
          WRITE(11, 7630) I
7630      FORMAT(1X,'     SSD ',I1,' Preamp and Bias: OFF')
        ENDIF
7640  CONTINUE
C
      IF(BTEST(HDB.DAT(56),3))THEN
        WRITE(11, 7700)
7700    FORMAT(1X,'     Energy Threshold: High')
      ELSE
        WRITE(11, 7710)
7710    FORMAT(1X,'     Energy Threshold: Low')
      ENDIF
C
      CALL MVBITS(HDB.DAT(56), 4, 4, TAC_SLOPE, 0)
      IF(TAC_SLOPE.EQ.1)THEN
        WRITE(11, 7800)
7800    FORMAT(1X,'     TAC Slope: Nominal')
      ELSEIF(TAC_SLOPE.EQ.5)THEN
        WRITE(11, 7810)
7810    FORMAT(1X,'     TAC Slope: -10%')
      ELSEIF(TAC_SLOPE.EQ.7)THEN
        WRITE(11, 7820)
7820    FORMAT(1X,'     TAC Slope: +10%')
      ELSEIF(TAC_SLOPE.EQ.9)THEN
        WRITE(11, 7830)
7830    FORMAT(1X,'     TAC Slope: +5%')
      ELSEIF(TAC_SLOPE.EQ.13)THEN
        WRITE(11, 7840)
7840    FORMAT(1X,'     TAC Slope: -5%')
      ELSE
        WRITE(11, 7850)
7850    FORMAT(1X,'     TAC Slope: Invalid')
      ENDIF
C
      SWICS_TRIGGER_MODE = IAND(HDB.DAT(59), 7)
      IF(SWICS_TRIGGER_MODE.EQ.0)THEN
        WRITE(11, 7900)
7900    FORMAT(1X,'     Trigger Mode: 0 = E OR T')
      ELSEIF(SWICS_TRIGGER_MODE.EQ.1)THEN
        WRITE(11, 7910)
7910    FORMAT(1X,'     Trigger Mode: 1 = E ')
      ELSEIF(SWICS_TRIGGER_MODE.EQ.2)THEN
        WRITE(11, 7920)
7920    FORMAT(1X,'     Trigger Mode: 2 = E AND T ') 
      ELSEIF(SWICS_TRIGGER_MODE.EQ.3)THEN
        WRITE(11, 7930)
7930    FORMAT(1X,'     Trigger Mode: 3 = T') 
      ELSEIF(SWICS_TRIGGER_MODE.EQ.4)THEN
        WRITE(11, 7940)
7940    FORMAT(1X,'     Trigger Mode: 4 = E OR T During Callibration Cycle') 
      ELSEIF(SWICS_TRIGGER_MODE.EQ.5)THEN
        WRITE(11, 7950)
7950    FORMAT(1X,'     Trigger Mode: 5 = E During Callibration Cycle') 
      ELSEIF(SWICS_TRIGGER_MODE.EQ.6)THEN
        WRITE(11, 7960)
7960    FORMAT(1X,'     Trigger Mode: 6 = E AND T During Callibration Cycle') 
      ELSEIF(SWICS_TRIGGER_MODE.EQ.7)THEN
        WRITE(11, 7970)
7970    FORMAT(1X,'     Trigger Mode: 7 = T During Callibration Cycle') 
      ENDIF
C
      IF(BTEST(HDB.DAT(59),7))THEN
        WRITE(11, 8000)
8000    FORMAT(1X,'     Command Error Check: ON ')
      ELSE
        WRITE(11, 8010)
8010    FORMAT(1X,'     Command Error Check: OFF')
      ENDIF
C
      SWICS_SUN_SECTOR_DEFINITIONS='0000000000000000'
      DO 8100 I=1,8
        IF(BTEST(HDB.DAT(86), I-1)) SWICS_SUN_SECTOR_DEFINITIONS(I:I)='1'
8100  CONTINUE
      DO 8110 I=1,8
        IF(BTEST(HDB.DAT(87), I-1)) SWICS_SUN_SECTOR_DEFINITIONS(I+8:I+8)='1'
8110  CONTINUE
      WRITE(11, 8120)SWICS_SUN_SECTOR_DEFINITIONS
8120  FORMAT(1X,'     SWICS sun sector definition: ', A)
C
      WRITE(11, 8200)ZEXT(HDB.DAT(88))
8200  FORMAT(1X,'     Compressed FSR Limit for Step Reversal: ',I3) 
C
      IF(BTEST(HDB.DAT(89),4))THEN
        WRITE(11, 8300)
8300    FORMAT(1X,'     Reversal Stepping Mode: Rate Counting Stopped After Reversal Stepping ')
      ELSE
        WRITE(11, 8310)
8310    FORMAT(1X,'     Reversal Stepping Mode: Rate Counting Continued After Reversal Stepping ')
      ENDIF
C
      IF(BTEST(HDB.DAT(89),6))THEN
        WRITE(11, 8400)
8400    FORMAT(1X,'     PHA FIFO Mode: Enabled')
      ELSE
        WRITE(11, 8410)
8410    FORMAT(1X,'     PHA FIFO Mode: Disabled')
      ENDIF
C
      IF(BTEST(HDB.DAT(89),7))THEN
        WRITE(11, 8500)
8500    FORMAT(1X,'     Test Tables: Active')
      ELSE
        WRITE(11, 8510)
8510    FORMAT(1X,'     Test Tables: Inactive')
      ENDIF
C
      IF(BTEST(HDB.DAT(189),2))THEN
        COMP_CODE(1) = 'C'
      ELSE
        COMP_CODE(1) = 'A'
      ENDIF
C
      IF(BTEST(HDB.DAT(189),3))THEN
        COMP_CODE(2) = 'C'
      ELSE
        COMP_CODE(2) = 'A'
      ENDIF
C
      WRITE(11, 5200) COMP_CODE
C
C Write actual header, overwriting the dummy that was written at the beginning of the file.
C This is done by closing and reopening the file, then using REWRITE
C
      CLOSE(UNIT=11)                            ! Close output file
      OPEN(UNIT=11, NAME=OUTPUTFILENAME, STATUS='OLD')
      READ(11,*)
      REWRITE(11,50)
      READ(11,*)
      REWRITE(11,52)
      READ(11,*)
      REWRITE(11,55)L1FILENAME
      READ(11,*)
      REWRITE(11,57)VERSION, RUN_TIME, RUN_DATE
      READ(11,*)
      REWRITE(11,60) FIRST_SR, FIRST.HR, FIRST.MIN, FIRST.SEC
      READ(11,*)
      REWRITE(11,65) LAST_SR, LAST.HR, LAST.MIN, LAST.SEC
      READ(11,*)
      REWRITE(11,70)CNTR-INCOMPLETE,INCOMPLETE
C
C Close files and STOP
C
      CALL WIND_L1_CLOSE( L1_LUN, STATUS )      ! Close input file
      IF (STATUS .NE. 1) GOTO 9090
      CLOSE(UNIT=11)                            ! Close output file
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
9060  TYPE *,'ERROR GETTING HK DATA; STATUS=',STATUS
      STOP
9080  TYPE *,'ERROR GETTING STICS DATA; STATUS=',STATUS
      STOP
9090  TYPE *,'ERROR CLOSING L1 FILE; STATUS=',STATUS
      STOP
      END
C-------------------------------------------------------------------------------------------------------------------------------
      SUBROUTINE POLY(X,C0,C1,C2,C3,C4,C5)
C This function computes a polynomial of up to 5th order in each component
C of the X structure. The polynomials' values are retuned in x also
C-------------------------------------------------------------------------------------------------------------------------------
      REAL C0,C1,C2,C3,C4,C5,TEMP
      STRUCTURE/HOUSEKEEPING_STATS/
        REAL MIN,MAX,AVER
        INTEGER NZERO
      END STRUCTURE
      RECORD /HOUSEKEEPING_STATS/ X
C
C Evaluate polynomial of minimum, maximum, and average values of X. Return 0 for the 
C average if X is negative.
C
      X.MIN = C0 + C1*X.MIN + C2*X.MIN**2 + C3*X.MIN**3 + C4*X.MIN**4 + C5*X.MIN**5
      X.MAX = C0 + C1*X.MAX + C2*X.MAX**2 + C3*X.MAX**3 + C4*X.MAX**4 + C5*X.MAX**5
      IF(X.AVER.GE.0)THEN
        X.AVER = C0 + C1*X.AVER + C2*X.AVER**2 + C3*X.AVER**3 + C4*X.AVER**4 + C5*X.AVER**5
      ELSE
        X.AVER = 0
      ENDIF
C
C The polynomials used for WIND housekeeping are monotonic, so that the polynomial 
C evaluated at X.MIN is still an extremum. However, if the polynomial is monotonically
C DECREASING, the minimum and the maximum will exchange places. Allow for this below.
C
      IF(X.MIN.GT.X.MAX)THEN
        TEMP = X.MIN
        X.MIN = X.MAX
        X.MAX = TEMP
      ENDIF
C
      RETURN
      END
C-------------------------------------------------------------------------------------------------------------------------------
      SUBROUTINE FIND_STATS(INPUT,STATS)
C This subroutine finds the minimum,maxixmum, and sum of variable. These are
C stored in a structure.
C-------------------------------------------------------------------------------------------------------------------------------
      INTEGER*4 INPUT
      STRUCTURE/HOUSEKEEPING_STATS/
        REAL MIN,MAX,AVER
        INTEGER NZERO
      END STRUCTURE
      RECORD /HOUSEKEEPING_STATS/ STATS
C
C compare to previously established min and max, and add to accumulator;
C do this iff value isn't 0. If value is 0, simply increment zero counter
C
      IF(INPUT.EQ.0)THEN
        STATS.NZERO =STATS.NZERO + 1
      ELSE
        IF(INPUT .LT. STATS.MIN) STATS.MIN = INPUT
        IF(INPUT .GT. STATS.MAX) STATS.MAX = INPUT
        STATS.AVER = STATS.AVER + INPUT
      ENDIF
C
      RETURN
      END
