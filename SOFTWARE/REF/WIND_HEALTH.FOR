CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      PROGRAM WIND_HEALTH
C This program reads housekeeping (HK and HDB) data and prints the data out.
C Subcommutated data is printed out in tabular form, 1 table for every 64
C science records. For various analog housekeeping items, an average, a
C minimum,  and a maximum is found, and the number of times that the value is
C zero is counted.  DPU status items are taken from the last science record.
C
C Created 	27-APR-1995     JOHN PAQUETTE
C
C This version:   1.12    6-DEC-1995
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
C	10-NOV-1995	L. Bleau	moved FIND_STATS to external file; moved
C					def of HOUSEKEEPING_STATS structure to
C					new LIMITDEF.INC file and inserted
C					INCLUDE statements
C	13-NOV-1995	L. Bleau	renamed POLY to POLY2, added function
C					POLY to do polynomial evaluation, moved
C					computation of engineering value to
C					where data items are extracted, changed
C					calling sequence of FIND_STATS (added
C					EVALUE and THIS_TIME)  
C	14-NOV-1995	L. Bleau	added code to mail summary file to user;
C					added FORM_FEED variable; added COMMON
C					blocks INFO and REPORT; moved L1 and
C					output file open code to WIND_HEALTH_INIT;
C					added internal routine FIND_AVG
C	15-NOV-1995	L. Bleau	changed RECORD variables of type
C					HOUSEKEEPING_STATS to PARAMETERs whose
C					value indexes the RECORD array ENTITY,
C					allowing easy indexing and management of
C					stats; changed multiple IF-THEN-ELSE-IF
C					for callibrator level to single WRITE
C					using array CAL_LEVEL; added call to
C					READ_LIMITS; changed output file to
C					normal listing mode (was Fortran); changed
C					all calls to FIND_STATS to pass ENTITY(n);
C					changed nearly all WRITEs to use ENTITY
C					array; removed commented-out average
C					computation code, which had already been
C					replaced by calls to FIND_AVG; removed
C					duplicate variables which only hold the
C					value from the last HDB, as these are
C					held in the structure field .LAST_VALUE;
C					replaced STOP with CALL EXIT for errors;
C					added internal DUMP_STATS routine;
C					reformatted comments to fit in 80 cols
C	17-NOV-1995	L. Bleau	added PARAMETER MAX_ENTITY; changed some
C					variables to all uppercase; added many
C					more variables to ENTITY array; changed
C					some WRITE stmts to use ENTITY array
C					element rather than local variables
C	20-NOV-1995	L. Bleau	added many more variables to ENTITY
C					array, increased its size to 700; added
C					blocks of 60 entries in ENTITY for sub-
C					commutated values
C	21-NOV-1995	L. Bleau	added TYPE stmts to display filename;
C					added code to parse filenames; fixed
C					command to mail message; fixed a format
C	 5-DEC-1995	L. Bleau	changed output text on WRITE for 
C					'27  -12 V Voltage (PAPS Ref)', added
C					'SWICS' 
C	 6-DEC-1995	L. Bleau	changed SWICS_SSD_1_BIAS to
C					SWICS_MAIN_SSD_BIAS near stmt 7610;
C					swapped HDB(113) with HDB(114) (there
C					was a documentation error); added code
C					to check STICS, MR compression codes
C
CDEC$ IDENT  '1.12'

      INCLUDE	'INC_DIR:EDBDEF.INC/NOLIST'
      INCLUDE   'INC_DIR:L1FMT.INC/NOLIST'
      INCLUDE   'INC_DIR:LIMITDEF.INC/NOLIST'
      INCLUDE   'INC_DIR:LIM.INC/NOLIST'
      INCLUDE   '($RMSDEF)'
C
      INTEGER MAX_ENTITY
      PARAMETER (MAX_ENTITY = 700)
C
      INTEGER INCOMPLETE/0/, NCHAR, FIRST_SR, LAST_SR, MR(2, 0:9), BR0_BR1_BORDER 
      INTEGER*4 STATUS,L1_LUN/19/,MONTH,DAYOFMONTH,HK_COUNT,NUM_MF/0/,NUM_SR/0/, OFFSET/0/
      INTEGER*4 SSD_1_THRESHOLD, SSD_2_THRESHOLD, SSD_3_THRESHOLD
      INTEGER*4 SWICS_TRIGGER_MODE, TAC_SLOPE, RESET_CAUSE
      REAL*4 POLY, SPIN_PERIOD, MASS_HPS_MEAS_V, MASS_STOP_MCP_MEAS_V, MASS_START_MCP_MEAS_V
      REAL*4 STICS_MCPPS_MEAS_V(0:5), STICS_TOFPS_MEAS_V, SWICS_PAPS_MEAS_V, SWICS_START_MCPPS_MEAS_V, 
     &       SWICS_STOP_MCPPS_MEAS_V
      REAL*8 OLD_TIME
      CHARACTER VERSION*5, RUN_TIME*8, RUN_DATE*9
      CHARACTER*1 COMP_CODE(2), ASCII_NULL, FORM_FEED
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
      CHARACTER*80 L1FILENAME, OUTPUTFILENAME
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
      INTEGER NCH,NCH2,N1,N2,ITEMP2
      REAL*4  SWICS_DPPS_OUTV(0:59), MASS_ADPS_OUTV(0:59), MASS_DPPS_OUTV(0:59),       ! Subcommutated
     &        STICS_PLUSDPPS_OUTV(0:59), STICS_MINUSDPPS_OUTV(0:59)                    ! quantities
C      INTEGER SWICS_PAPS_CL, SWICS_START_MCPPS_CL, SWICS_STOP_MCPPS_CL, MASS_HPS_CL,
C     &        MASS_START_MCPPS_CL, MASS_STOP_MCPPS_CL, STICS_MCPPS_0_CL, STICS_MCPPS_1_CL,
C     &        STICS_MCPPS_2_CL, STICS_MCPPS_3_CL, STICS_MCPPS_4_CL, STICS_MCPPS_5_CL,
C     &        STICS_TOFPS_MCPPS_6_CL
C      REAL*4  SWICS_PAPS_CV, SWICS_START_MCPPS_CV, SWICS_STOP_MCPPS_CV, MASS_HPS_CV,
C     &        MASS_START_MCPPS_CV, MASS_STOP_MCPPS_CV, STICS_MCPPS_0_CV, STICS_MCPPS_1_CV,
C     &        STICS_MCPPS_2_CV, STICS_MCPPS_3_CV, STICS_MCPPS_4_CV, STICS_MCPPS_5_CV,
C     &        STICS_TOFPS_MCPPS_6_CV
      INTEGER CAL_LEVEL(0:15)
      DATA CAL_LEVEL/300,280,260,240,220,200,180,160,140,120,100,80,60,40,20,-1/
      REAL*4 EVALUE
C
      RECORD /CORE/ SCI
      RECORD /HDBstr/ HDB,LAST_GOOD_HDB
      RECORD /ST_RATE/ STICS_RATE
      RECORD /ST_PHA/ STICS_PHA
      RECORD /HKEEP/ HK
      RECORD /T/ START, END, FIRST, LAST
      RECORD /HOUSEKEEPING_STATS/ ENTITY(MAX_ENTITY)
C
C COMMON blocks
C
      REAL*8 THIS_TIME
      RECORD /T/ W_THIS_TIME
      COMMON /INFO/ THIS_TIME,W_THIS_TIME,NUM_SR
C
      INTEGER WH_LUN,WH_LUN2,NEXCEED,NCHL1,NCHMAIL
      CHARACTER*80 L1NAME,MAIL_LIST
      COMMON /REPORT/ WH_LUN,WH_LUN2,L1NAME,NCHL1,NEXCEED,MAIL_LIST,NCHMAIL

C
      ASCII_NULL = CHAR(0)
      FORM_FEED = CHAR(12)
C
C Call initialization routines
C
      CALL WIND_HEALTH_INIT(L1_LUN, L1FILENAME, OUTPUTFILENAME, NCH)
      CALL READ_LIMITS(ENTITY,MAX_ENTITY,N1,N2)
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
C Open output file
C
      TYPE *,'WIND SMS instrument health detail report will be written to the file'
      TYPE *,OUTPUTFILENAME(1:NCH)
      OPEN(UNIT=11, NAME=OUTPUTFILENAME(1:NCH), STATUS='NEW', CARRIAGECONTROL='LIST')
C
C Read first SR, Core, and STICS records.
C
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
C Compare time read to start time. If the time read is greater than or equal to
C the user- specified start time, then begin processing data.
C
      IF(.NOT.STARTFOUND)THEN
        STARTFOUND=COMPARE_TIME_GE(SCI.EDB(0).SC_TIME,START)
      ENDIF
      IF(STARTFOUND)THEN
        NUM_SR = NUM_SR + 1
C
C Compare time read to end time. If time read is greater than the user-specified
C end time, we're done. Go to the part of the code that closes files and STOP's
C program.
C
        ENDFOUND=COMPARE_TIME_GE(SCI.EDB(0).SC_TIME,END)
        IF(ENDFOUND)GO TO 900
C
C The first five quantities are formatted by the space craft, and are only
C available in the HK. The loop below loops over HK's
C
        NEW_HK = 1
        IF( HK.HKSET(1).EPOCH .EQ. OLD_TIME )NEW_HK = 2
        DO 200 I = NEW_HK, HK_COUNT
C
          NUM_MF = NUM_MF + 1
C
C Get time of this HK and save it to pass to FIND_STATS routine
C
	  THIS_TIME = HK.HKSET(I).EPOCH
	  W_THIS_TIME = HK.HKSET(I).W_TIME

C
C Get DPU Temp (Passive)
C
          VALUE = HK.HKSET(I).HK(1).BVAL
          VALUE2 = IAND(VALUE, 255)
          EVALUE = POLY(FLOAT(VALUE2), 6.558508E2 ,-1.802378E1, 2.133615E-1, -1.295112E-3, 3.927925E-6, -4.754717E-9)
          CALL FIND_STATS(VALUE2,EVALUE,ENTITY(DPU_TEMP))
C
C Get SWICS Gnd Electronics Temp (Passive)
C
          VALUE  = HK.HKSET(I).HK(2).BVAL
          VALUE2 = IAND(VALUE, 255)
          EVALUE = POLY(FLOAT(VALUE2), 6.558508E2 ,-1.802378E1, 2.133615E-1, -1.295112E-3, 3.927925E-6, -4.754717E-9)
          CALL FIND_STATS(VALUE2,EVALUE,ENTITY(SWICS_GET))
C
C Get MASS Gnd Electronics Temp (Passive)
C
          VALUE = HK.HKSET(I).HK(3).BVAL
          VALUE2 = IAND(VALUE, 255)
          EVALUE = POLY(FLOAT(VALUE2), 6.558508E2 ,-1.802378E1, 2.133615E-1, -1.295112E-3, 3.927925E-6, -4.754717E-9)
          CALL FIND_STATS(VALUE2,EVALUE,ENTITY(MASS_GET))
C
C Get STICS SSD Temp (Passive)
C
          VALUE = HK.HKSET(I).HK(4).BVAL
          VALUE2 = IAND(VALUE, 255)
          EVALUE = POLY(FLOAT(VALUE2), 6.558508E2 ,-1.802378E1, 2.133615E-1, -1.295112E-3, 3.927925E-6, -4.754717E-9)
          CALL FIND_STATS(VALUE2,EVALUE,ENTITY(STICS_SSD_TEMP))
C
C Get STICS AE Housing Temp (Passive)
C
          VALUE = HK.HKSET(I).HK(5).BVAL
          VALUE2 = IAND(VALUE, 255)
          EVALUE = POLY(FLOAT(VALUE2), 6.558508E2 ,-1.802378E1, 2.133615E-1, -1.295112E-3, 3.927925E-6, -4.754717E-9)
          CALL FIND_STATS(VALUE2,EVALUE,ENTITY(STICS_AEH_TEMP))
C
 200    CONTINUE
C
C Get time of this HDB and save it to pass to FIND_STATS routine
C
	THIS_TIME = HDB.EPOCH
	W_THIS_TIME = HDB.W_TIME
C
C Get DPU 28 V Primary Current
C
        VALUE = HDB.DAT(215)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 2.040, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(DPU_28V))
C
C Get DPU 5.2 V (Switched) Voltage
C
        VALUE = HDB.DAT(216)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 2.500E-2 , 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(DPU_5P2V_S))
C
C Get DPU 5.2 V (Permanent) Voltage
C
        VALUE = HDB.DAT(217)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 2.500E-2 , 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(DPU_5P2V_P))
C
C Get DPU 10 V Voltage
C
        VALUE = HDB.DAT(218)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 5.000E-2 , 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(DPU_10V))
C
C Get DPU Temperature, PIF_A
C
        VALUE = HDB.DAT(219)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 1.437601E2, -3.224279, 4.848285E-2, -4.101237E-4, 1.682762E-6, -2.660079E-9)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(DPU_TEMP_PIFA))
C
C Get DPU Temperature, CTL_W
C
        VALUE = HDB.DAT(220)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 1.437601E2, -3.224279, 4.848285E-2, -4.101237E-4, 1.682762E-6, -2.660079E-9)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(DPU_TEMP_CTLW))
C
C Get SWICS +28 V Primary Current (Gnd Ref)
C
        VALUE = HDB.DAT(90)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 1.030 , 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(SWICS_PLUS28V_PRIM_CURR))
C
C Get SWICS +5 V Voltage (Gnd Ref)
C
        VALUE = HDB.DAT(94)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 2.500E-2, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(SWICS_PLUS5V))
C
C Get SWICS +5 V Current (Gnd Ref)
C
        VALUE = HDB.DAT(93)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 5.000E-1 , 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(SWICS_PLUS5I))
C
C Get SWICS -5 V Voltage (Gnd Ref)
C
        VALUE = HDB.DAT(95)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, -2.500E-2, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(SWICS_MINUS5V))
C
C Get SWICS +8 V Voltage (Gnd Ref)
C
        VALUE = HDB.DAT(92)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 4.000E-2, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(SWICS_PLUS8V))
C
C Get SWICS +8 V Current (Gnd Ref)
C
        VALUE = HDB.DAT(91)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 2.000E-1, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(SWICS_PLUS8I))
C
C Get SWICS 20 V Voltage (Gnd Ref)
C
        VALUE = HDB.DAT(67)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), -4.0E-1, 1.000E-1, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(SWICS_20V))
C
C Get SWICS 20 V Current (Gnd Ref)
C
        VALUE = HDB.DAT(66)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), -1.675, 9.990E-1, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(SWICS_20I))
C
C Get SWICS LVPS 28 V Voltage (Gnd Ref)
C
        VALUE = HDB.DAT(69)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), -7.143E-1, 1.429E-1, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(SWICS_LVPS_28V))
C
C Get SWICS DPPS 28 V Current (Gnd Ref)
C
        VALUE = HDB.DAT(71)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), -3.864E-1, 2.078E-1, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(SWICS_DPPS_28I))
C
C Get SWICS PAPS 28 V Current (Gnd Ref)
C
        VALUE = HDB.DAT(68)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), -8.454E-1, 2.065E-1, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(SWICS_PAPS_28I))
C
C Get SWICS PAPS Output Voltage (Gnd Ref)
C
        VALUE = HDB.DAT(70)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 4.453E1, -1.464E2, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(SWICS_PAPS_OUTV))
C
C Get SWICS +5 V Voltage (PAPS Ref)
C
        VALUE = HDB.DAT(73)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 2.483E-2, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(SWICS_PLUS5V_PR))
C
C Get SWICS +5 V Current (PAPS Ref)
C
        VALUE = HDB.DAT(74)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 7.176E-1, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(SWICS_PLUS5I_PR))
C
C Get SWICS -5 V Voltage (PAPS Ref)
C
        VALUE = HDB.DAT(75)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, -2.488E-2, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(SWICS_MINUS5V_PR))
C
C Get SWICS -5 V Current (PAPS Ref)
C
        VALUE = HDB.DAT(76)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 7.353E-1, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(SWICS_MINUS5I_PR))
C
C Get SWICS +12 V Voltage (PAPS Ref)
C
        VALUE = HDB.DAT(82)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 5.977E-2, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(SWICS_PLUS12V_PR))
C
C Get SWICS +12 V Current (PAPS Ref)
C
        VALUE = HDB.DAT(83)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 6.625E-1, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(SWICS_PLUS12I_PR))
C
C Get -12 V Voltage (PAPS Ref)
C
        VALUE = HDB.DAT(84)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, -5.917E-2, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(SWICS_MINUS12V_PR))
C
C Get SWICS -12 V Current (PAPS Ref)
C
        VALUE = HDB.DAT(85)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 1.484E-1, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(SWICS_MINUS12I_PR))
C
C Get SWICS SSD Bias Voltage (PAPS Ref)
C
        VALUE = HDB.DAT(81)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0, 0.5, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(SWICS_SSD_BIASV_PR))
C
C Get SWICS Start MCPPS Voltage (PAPS Ref)
C
        VALUE = HDB.DAT(77)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), -4.610, 1.864E1, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(SWICS_START_MCPPSV_PR))
C
C Get SWICS Stop MCPPS Voltage (PAPS Ref)
C
        VALUE = HDB.DAT(78)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 4.650, 1.356E1, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(SWICS_STOP_MCPPSV_PR))
C
C Get SWICS SSD Temp (PAPS Ref)
C
        VALUE = HDB.DAT(79)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 7.886257E1, -1.848392, 2.143749E-2, -1.483171E-4, 5.149126E-7, -7.140936E-10)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(SWICS_SSD_TEMP_PR))
C
C Get SWICS Bubble Elect Temp, (PAPS Ref)
C
        VALUE = HDB.DAT(80)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 7.886257E1, -1.848392, 2.143749E-2, -1.483171E-4, 5.149126E-7, -7.140936E-10)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(SWICS_BET_PR))
C
C Get MASS +28 V Primary Current (Gnd Ref)
C
        VALUE = HDB.DAT(98)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 1.050, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(MASS_PLUS28I))
C
C Get MASS +5 V Voltage (Gnd Ref)
C
        VALUE = HDB.DAT(103)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 2.500E-2, 0.0, 0.0, 0.0, 0.0 )
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(MASS_PLUS5V))
C
C Get MASS +5 V Current (Gnd Ref)
C
        VALUE = HDB.DAT(97)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 5.000E-1, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(MASS_PLUS5I))
C
C Get MASS -5 V Voltage (Gnd Ref)
C
        VALUE = HDB.DAT(104)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, -2.500E-2, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(MASS_MINUS5V))
C
C Get MASS +8 V Voltage (Gnd Ref)
C
        VALUE = HDB.DAT(102)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 4.000E-2, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(MASS_PLUS8V))
C
C Get MASS LVPS 28 V Voltage (Gnd Ref)
C
        VALUE = HDB.DAT(101)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 1.400E-1, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(MASS_LVPS28V))
C
C Get MASS ADPS 28 V Current (Gnd Ref)
C
        VALUE = HDB.DAT(100)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 2.000E-1, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(MASS_ADPS28V))
C
C Get MASS DPPS 28 V Current (Gnd Ref)
C
        VALUE = HDB.DAT(99)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 2.000E-1, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(MASS_DPPS28I))
C
C Get MASS Analog Electronics Temp (Gnd Ref)
C
        VALUE = HDB.DAT(105)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 7.886257E1, -1.848392, 2.143749E-2, -1.483171E-4, 5.149126E-7, -7.140936E-10)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(MASS_AET))
C
C Get MASS +5 V Voltage (ADPS Ref)
C
        VALUE = HDB.DAT(125)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 2.500E-2, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(MASS_PLUS5V_AR))
C
C Get MASS +5 V Current (ADPS Ref)
C
        VALUE = HDB.DAT(126)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 1.000, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(MASS_PLUS5I_AR))
C
C Get MASS -5 V Voltage (ADPS Ref)
C
        VALUE = HDB.DAT(128)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, -2.500E-2, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(MASS_MINUS5V_AR))
C
C Get MASS -5 V Current (ADPS Ref)
C
        VALUE = HDB.DAT(127)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 1.000, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(MASS_MINUS5I_AR))
C
C Get MASS +6 V Voltage (ADPS Ref)
C
        VALUE = HDB.DAT(123)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 3.000E-2, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(MASS_PLUS6V_AR))
C
C Get MASS +6 V Current (ADPS Ref)
C
        VALUE = HDB.DAT(124)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 1.000, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(MASS_PLUS6I_AR))
C
C Get MASS +12 V Voltage (ADPS Ref)
C
        VALUE = HDB.DAT(122)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 6.000E-2, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(MASS_PLUS12V_AR))
C
C Get MASS -12 V Voltage (ADPS Ref)
C
        VALUE = HDB.DAT(129)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, -6.000E-2, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(MASS_MINUS12V_AR))
C
C Get MASS LVPS 28 V Voltage (ADPS Ref)
C
        VALUE = HDB.DAT(121)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 1.400E-1, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(MASS_LVPS28V_AR))
C
C Get MASS HPS & MCPPS 28V I (ADPS Ref)
C
        VALUE = HDB.DAT(118)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 2.000E-1, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(MASS_HPS_MCPPS_28I_AR))
C
C Get MASS HPS Output Voltage (ADPS Ref)
C
        VALUE = HDB.DAT(117)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), -3.428E1, 1.5285E2, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(MASS_HPS_OUTV_AR))
C
C Get MASS Start MCPPS Voltage (ADPS Ref)
C
        VALUE = HDB.DAT(119)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 1.310E1, 1.431E1, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(MASS_START_MCPPSV_AR))
C
C Get MASS Stop MCPPS Voltage (ADPS Ref)
C
        VALUE = HDB.DAT(120)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 3.065E1, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(MASS_STOP_MCPPSV_AR))
C
C Get STICS 28 V Primary Current
C
        VALUE = HDB.DAT(49)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 2.040, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(STICS_28I))
C
C Get STICS +5 V Voltage
C
        VALUE = HDB.DAT(45)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 2.455E-2, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(STICS_PLUS5V))
C
C Get STICS -5 V Voltage
C
        VALUE = HDB.DAT(46)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, -2.451E-2, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(STICS_MINUS5V))
C
C Get STICS +6 V Voltage
C
        VALUE = HDB.DAT(43)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 2.876E-2, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(STICS_PLUS6V))
C
C Get STICS -6 V Voltage
C
        VALUE = HDB.DAT(44)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, -2.898E-2, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(STICS_MINUS6V))
C
C Get STICS +12 V Voltage
C
        VALUE = HDB.DAT(47)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 5.837E-2, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(STICS_PLUS12V))
C
C Get STICS -12 V Voltage
C
        VALUE = HDB.DAT(50)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, -5.880E-2, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(STICS_MINUS12V))
C
C Get STICS MCPPS 28 V Current
C
        VALUE = HDB.DAT(34)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 5.000E-1, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(STICS_MCPPS28I))
C
C Get STICS DPPS 28 V Current
C
        VALUE = HDB.DAT(35)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 5.000E-1, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(STICS_DPPS28I))
C
C Get STICS SSD Bias Voltage
C
        VALUE = HDB.DAT(48)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 0.0, 0.5, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(STICS_SSD_BIASV))
C
C Get STICS MCPPS 0 Output Voltage
C
        VALUE = HDB.DAT(27)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), -7.806, 1.959E1, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(STICS_MCPPS0_OUTV))
C
C Get STICS MCPPS 1 Output Voltage
C
        VALUE = HDB.DAT(28)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), -3.475, 1.950E1, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(STICS_MCPPS1_OUTV))
C
C Get STICS MCPPS 2 Output Voltage
C
        VALUE = HDB.DAT(29)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), -9.405, 1.957E1, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(STICS_MCPPS2_OUTV))
C
C Get STICS MCPPS 3 Output Voltage
C
        VALUE = HDB.DAT(30)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), -1.950E1, 1.984E1, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(STICS_MCPPS3_OUTV))
C
C Get STICS MCPPS 4 Output Voltage
C
        VALUE = HDB.DAT(31)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), -1.436E1, 1.984E1, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(STICS_MCPPS4_OUTV))
C
C Get STICS MCPPS 5 Output Voltage
C
        VALUE = HDB.DAT(32)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), -7.293, 1.969E1, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(STICS_MCPPS5_OUTV))
C
C Get STICS TOFPS (MCPPS 6) Output Voltage
C
        VALUE = HDB.DAT(33)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), -5.518, 1.961E1, 0.0, 0.0, 0.0, 0.0)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(STICS_TOFPS_OUTV))
C
C Get STICS T1, AE Housing Temp
C
        VALUE = HDB.DAT(39)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 7.886257E1, -1.848392, 2.143749E-2, -1.483171E-4, 5.149126E-7, -7.140936E-10)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(STICS_T1_AEHT))
C
C Get STICS T2, Fairing/HOP Temp
C
        VALUE = HDB.DAT(40)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 7.886257E1, -1.848392, 2.143749E-2, -1.483171E-4, 5.149126E-7, -7.140936E-10)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(STICS_T2_FHOPT))
C
C Get STICS T3, SSD Temp
C
        VALUE = HDB.DAT(41)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 7.886257E1, -1.848392, 2.143749E-2, -1.483171E-4, 5.149126E-7, -7.140936E-10)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(STICS_T3_SSDT))
C
C Get STICS T4, Back Panel Temp
C
        VALUE = HDB.DAT(42)
        VALUE2 = IAND(VALUE, 255)
        EVALUE = POLY(FLOAT(VALUE2), 7.886257E1, -1.848392, 2.143749E-2, -1.483171E-4, 5.149126E-7, -7.140936E-10)
        CALL FIND_STATS(VALUE2,EVALUE,ENTITY(STICS_T4_BPT))

C
C Get commanded levels. Don't need stats for these, as they shouldn't be 
C changing; just find last value which is nonzero (a data gap could give a 
C zero value).
C
C        SWICS_PAPS_CL = IAND(HDB.DAT(61), 255)
C        SWICS_PAPS_CV = 190.96 - 117.15 * SWICS_PAPS_CL 
C        SWICS_START_MCPPS_CL = IAND(HDB.DAT(62), 255)
C        SWICS_START_MCPPS_CV = 1.129 + 16.303 * SWICS_START_MCPPS_CL 
C        SWICS_STOP_MCPPS_CL = IAND(HDB.DAT(63), 255)
C        SWICS_STOP_MCPPS_CV = 1.564 + 12.181 * SWICS_STOP_MCPPS_CL 
        VALUE = IAND(HDB.DAT(61), 255)
	CALL FIND_STATS(VALUE,FLOAT(VALUE),ENTITY(SWICS_PAPS_CL))
        EVALUE = 190.96 - 117.15 * VALUE
	CALL FIND_STATS(VALUE,EVALUE,ENTITY(SWICS_PAPS_CV))
C
        VALUE = IAND(HDB.DAT(62), 255)
	CALL FIND_STATS(VALUE,FLOAT(VALUE),ENTITY(SWICS_START_MCPPS_CL))
        EVALUE = 1.129 + 16.303 * VALUE
	CALL FIND_STATS(VALUE,EVALUE,ENTITY(SWICS_START_MCPPS_CV))
C
        VALUE = IAND(HDB.DAT(63), 255)
	CALL FIND_STATS(VALUE,FLOAT(VALUE),ENTITY(SWICS_STOP_MCPPS_CL))
        EVALUE = 1.564 + 12.181 * VALUE
	CALL FIND_STATS(VALUE,EVALUE,ENTITY(SWICS_STOP_MCPPS_CV))
C
C        MASS_HPS_CL = IAND(HDB.DAT(110), 255)
C        MASS_HPS_CV = -19.5 + 132.08 * MASS_HPS_CL 
C        MASS_START_MCPPS_CL = IAND(HDB.DAT(112), 255)
C        MASS_START_MCPPS_CV = 12.2 + 18.294 * MASS_START_MCPPS_CL 
C        MASS_STOP_MCPPS_CL = IAND(HDB.DAT(113), 255)
C        MASS_STOP_MCPPS_CV = 0.2 + 13.452 * MASS_STOP_MCPPS_CL 
        VALUE = IAND(HDB.DAT(110), 255)
	CALL FIND_STATS(VALUE,FLOAT(VALUE),ENTITY(MASS_HPS_CL))
        EVALUE = -19.5 + 132.08 * VALUE
	CALL FIND_STATS(VALUE,EVALUE,ENTITY(MASS_HPS_CV))
C
        VALUE = IAND(HDB.DAT(112), 255)
	CALL FIND_STATS(VALUE,FLOAT(VALUE),ENTITY(MASS_START_MCPPS_CL))
        EVALUE = 12.2 + 18.294 * VALUE
	CALL FIND_STATS(VALUE,EVALUE,ENTITY(MASS_START_MCPPS_CV))
C
        VALUE = IAND(HDB.DAT(113), 255)
	CALL FIND_STATS(VALUE,FLOAT(VALUE),ENTITY(MASS_STOP_MCPPS_CL))
        EVALUE = 0.2 + 13.452 * VALUE
	CALL FIND_STATS(VALUE,EVALUE,ENTITY(MASS_STOP_MCPPS_CV))
C
C        STICS_MCPPS_0_CL = IAND(HDB.DAT(17), 255)
C        STICS_MCPPS_0_CV = -11.081 + 12.535 * STICS_MCPPS_0_CL 
C        STICS_MCPPS_1_CL = IAND(HDB.DAT(18), 255)
C        STICS_MCPPS_1_CV = -7.7568 + 12.535 * STICS_MCPPS_1_CL 
C        STICS_MCPPS_2_CL = IAND(HDB.DAT(19), 255)
C        STICS_MCPPS_2_CV = -9.4054 + 12.524 * STICS_MCPPS_2_CL
C        STICS_MCPPS_3_CL = IAND(HDB.DAT(20), 255)
C        STICS_MCPPS_3_CV  = -7.1081 + 12.526 * STICS_MCPPS_3_CL 
C        STICS_MCPPS_4_CL = IAND(HDB.DAT(21), 255)
C        STICS_MCPPS_4_CV = -8.5135 + 12.541* STICS_MCPPS_4_CL 
C        STICS_MCPPS_5_CL = IAND(HDB.DAT(22), 255)
C        STICS_MCPPS_5_CV = -10.081 + 12.565 * STICS_MCPPS_5_CL
C        STICS_TOFPS_MCPPS_6_CL  = IAND(HDB.DAT(23), 255)
C        STICS_TOFPS_MCPPS_6_CV = -3.0764 + 4.5362 * STICS_TOFPS_MCPPS_6_CL
        VALUE = IAND(HDB.DAT(17), 255)
	CALL FIND_STATS(VALUE,FLOAT(VALUE),ENTITY(STICS_MCPPS_0_CL))
        EVALUE = -11.081 + 12.535 * VALUE
	CALL FIND_STATS(VALUE,EVALUE,ENTITY(STICS_MCPPS_0_CV))
C
        VALUE = IAND(HDB.DAT(18), 255)
	CALL FIND_STATS(VALUE,FLOAT(VALUE),ENTITY(STICS_MCPPS_1_CL))
        EVALUE = -7.7568 + 12.535 * VALUE
	CALL FIND_STATS(VALUE,EVALUE,ENTITY(STICS_MCPPS_1_CV))
C
        VALUE = IAND(HDB.DAT(19), 255)
	CALL FIND_STATS(VALUE,FLOAT(VALUE),ENTITY(STICS_MCPPS_2_CL))
        EVALUE = -9.4054 + 12.524 * VALUE
	CALL FIND_STATS(VALUE,EVALUE,ENTITY(STICS_MCPPS_2_CV))
C
        VALUE = IAND(HDB.DAT(20), 255)
	CALL FIND_STATS(VALUE,FLOAT(VALUE),ENTITY(STICS_MCPPS_3_CL))
        EVALUE = -7.1081 + 12.526 * VALUE
	CALL FIND_STATS(VALUE,EVALUE,ENTITY(STICS_MCPPS_3_CV ))
C
        VALUE = IAND(HDB.DAT(21), 255)
	CALL FIND_STATS(VALUE,FLOAT(VALUE),ENTITY(STICS_MCPPS_4_CL))
        EVALUE = -8.5135 + 12.541* VALUE
	CALL FIND_STATS(VALUE,EVALUE,ENTITY(STICS_MCPPS_4_CV))
C
        VALUE = IAND(HDB.DAT(22), 255)
	CALL FIND_STATS(VALUE,FLOAT(VALUE),ENTITY(STICS_MCPPS_5_CL))
        EVALUE = -10.081 + 12.565 * VALUE
	CALL FIND_STATS(VALUE,EVALUE,ENTITY(STICS_MCPPS_5_CV))
C
        VALUE = IAND(HDB.DAT(23), 255)
	CALL FIND_STATS(VALUE,FLOAT(VALUE),ENTITY(STICS_TOFPS_MCPPS_6_CL))
        EVALUE = -3.0764 + 4.5362 * VALUE
	CALL FIND_STATS(VALUE,EVALUE,ENTITY(STICS_TOFPS_MCPPS_6_CV))
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
C For SWICS DPPS Output Voltage (Gnd Ref), we need to use bit 1 of hdb byte 89
C to determine the range
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
	    CALL FIND_STATS(VALUE2,SWICS_DPPS_OUTV(DECOM_CNTR),
     &		ENTITY(SWICS_DPPS_OUTV0+DECOM_CNTR))
          ELSE
            SWICS_DPPS_OUTV(DECOM_CNTR) = 7.858E-1 +  1.629*FLOAT(VALUE2)
	    CALL FIND_STATS(VALUE2,SWICS_DPPS_OUTV(DECOM_CNTR),
     &		ENTITY(SWICS_DPPS_OUTV0+DECOM_CNTR))
          ENDIF
C
C Get MASS ADPS Output Voltage (Gnd Ref)
C
          VALUE = HDB.DAT(106)
          VALUE2 = IAND(VALUE, 255)
          MASS_ADPS_OUTV(DECOM_CNTR) = -6.634E3 + 3.4185E1*FLOAT(VALUE2)
	  CALL FIND_STATS(VALUE2,MASS_ADPS_OUTV(DECOM_CNTR),
     &		ENTITY(MASS_ADPS_OUTV0+DECOM_CNTR))
C
C For MASS DPPS Output Voltage (Gnd Ref), we need to use bit 5 of hdb byte 107
C to determine the range
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
	  CALL FIND_STATS(VALUE2,MASS_DPPS_OUTV(DECOM_CNTR),
     &		ENTITY(MASS_DPPS_OUTV0+DECOM_CNTR))
C
C Get STICS +DPPS Output Voltage (12 bit field in bytes 36 and 37 of the HDB)
C
          VALUE = IAND(HDB.DAT(36), 255)
          VALUE = ISHFT(VALUE, 4)
          VALUE2 = IAND(HDB.DAT(37), 240)
          VALUE2 = ISHFT(VALUE2, -4)
          VALUE = IOR(VALUE,VALUE2)
          STICS_PLUSDPPS_OUTV(DECOM_CNTR) = -2.453E1 + 2.9562*FLOAT(VALUE)
	  CALL FIND_STATS(VALUE,STICS_PLUSDPPS_OUTV(DECOM_CNTR),
     &		ENTITY(STICS_PLUSDPPS_OUTV0+DECOM_CNTR))
C
C Get STICS -DPPS Output Voltage (12 bit field in bytes 37 and 38 of the HDB)
C
          VALUE = HDB.DAT(38)
          VALUE = IAND(VALUE, 255)
          VALUE2 = IAND(HDB.DAT(37), 15)
          VALUE2 = ISHFT(VALUE2, 8)
          VALUE = IOR(VALUE,VALUE2)
          STICS_MINUSDPPS_OUTV(DECOM_CNTR) = -2.422E1 - 2.9791*FLOAT(VALUE)
	  CALL FIND_STATS(VALUE,STICS_MINUSDPPS_OUTV(DECOM_CNTR),
     &		ENTITY(STICS_MINUSDPPS_OUTV0+DECOM_CNTR))
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
C When the program reaches this point, we have a full set of subcommutated
C values (or, at any rate, as close to a full set as we're going to get).  Print
C them out in tabular form.
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
      WRITE(11,950) FORM_FEED
 950  FORMAT(A1/' CHK         Parameter Identification            Minimum     Maximum     Average    # 0''s ',
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
      CALL FIND_AVG(ENTITY(DPU_TEMP))
      WRITE(11, 1000)'  0  DPU Temp (Passive)',
     &		ENTITY(DPU_TEMP).MIN, ENTITY(DPU_TEMP).MAX,
     &		ENTITY(DPU_TEMP).AVER, ENTITY(DPU_TEMP).NZERO
1000  FORMAT(1X,A,T45,3F12.2,5X,I3)
C
C SWICS Gnd Electronics Temp (Passive)
C
      CALL FIND_AVG(ENTITY(SWICS_GET))
      WRITE(11, 1000)'  1  SWICS Gnd Electronics Temp (Passive)',
     &		ENTITY(SWICS_GET).MIN, ENTITY(SWICS_GET).MAX,
     &		ENTITY(SWICS_GET).AVER, ENTITY(SWICS_GET).NZERO
C
C MASS Gnd Electronics Temp (Passive)
C
      CALL FIND_AVG(ENTITY(MASS_GET))
      WRITE(11, 1000)'  2  MASS Gnd Electronics Temp (Passive)',
     &		ENTITY(MASS_GET).MIN, ENTITY(MASS_GET).MAX,
     &		ENTITY(MASS_GET).AVER, ENTITY(MASS_GET).NZERO
C
C STICS SSD Temp (Passive)
C
      CALL FIND_AVG(ENTITY(STICS_SSD_TEMP))
      WRITE(11, 1000)'  3  STICS SSD Temp (Passive)',
     &		ENTITY(STICS_SSD_TEMP).MIN, ENTITY(STICS_SSD_TEMP).MAX,
     &		ENTITY(STICS_SSD_TEMP).AVER, ENTITY(STICS_SSD_TEMP).NZERO
C
C STICS AE Housing Temp (Passive)
C
      CALL FIND_AVG(ENTITY(STICS_AEH_TEMP))
      WRITE(11, 1000)'  4  STICS AE Housing Temp (Passive)', 
     &		ENTITY(STICS_AEH_TEMP).MIN, ENTITY(STICS_AEH_TEMP).MAX,
     &		ENTITY(STICS_AEH_TEMP).AVER, ENTITY(STICS_AEH_TEMP).NZERO
C
C DPU 28 V Primary Current
C
      CALL FIND_AVG(ENTITY(DPU_28V))
      WRITE(11, 1000)'  5  DPU 28 V Primary Current', 
     &		ENTITY(DPU_28V).MIN, ENTITY(DPU_28V).MAX,
     &		ENTITY(DPU_28V).AVER, ENTITY(DPU_28V).NZERO
C
C DPU 5.2 V (Switched) Voltage
C
      CALL FIND_AVG(ENTITY(DPU_5P2V_S))
      WRITE(11, 1000)'  6  DPU 5.2 V (Switched) Voltage', 
     &		ENTITY(DPU_5P2V_S).MIN, ENTITY(DPU_5P2V_S).MAX,
     &		ENTITY(DPU_5P2V_S).AVER, ENTITY(DPU_5P2V_S).NZERO
C
C DPU 5.2 V (Permanent) Voltage
C
      CALL FIND_AVG(ENTITY(DPU_5P2V_P))
      WRITE(11, 1000)'  7  DPU 5.2 V (Permanent) Voltage', 
     &		ENTITY(DPU_5P2V_P).MIN, ENTITY(DPU_5P2V_P).MAX,
     &		ENTITY(DPU_5P2V_P).AVER, ENTITY(DPU_5P2V_P).NZERO
C
C DPU 10 V Voltage
C
      CALL FIND_AVG(ENTITY(DPU_10V))
      WRITE(11, 1000)'  8  DPU 10 V Voltage', 
     &		ENTITY(DPU_10V).MIN, ENTITY(DPU_10V).MAX,
     &		ENTITY(DPU_10V).AVER, ENTITY(DPU_10V).NZERO
C
C DPU Temperature, PIF_A
C
      CALL FIND_AVG(ENTITY(DPU_TEMP_PIFA))
      WRITE(11, 1000)'  9  DPU Temperature, PIF_A', 
     &		ENTITY(DPU_TEMP_PIFA).MIN, ENTITY(DPU_TEMP_PIFA).MAX,
     &		ENTITY(DPU_TEMP_PIFA).AVER, ENTITY(DPU_TEMP_PIFA).NZERO
C
C DPU Temperature, CTL_W
C
      CALL FIND_AVG(ENTITY(DPU_TEMP_CTLW))
      WRITE(11, 1000)'     DPU Temperature, CTL_W', 
     &		ENTITY(DPU_TEMP_CTLW).MIN, ENTITY(DPU_TEMP_CTLW).MAX,
     &		ENTITY(DPU_TEMP_CTLW).AVER, ENTITY(DPU_TEMP_CTLW).NZERO
C
C SWICS +28 V Primary Current (Gnd Ref)
C
      CALL FIND_AVG(ENTITY(SWICS_PLUS28V_PRIM_CURR))
      WRITE(11, 1000)' 10  SWICS +28 V Primary Current (Gnd Ref)', 
     &		ENTITY(SWICS_PLUS28V_PRIM_CURR).MIN, ENTITY(SWICS_PLUS28V_PRIM_CURR).MAX,
     &		ENTITY(SWICS_PLUS28V_PRIM_CURR).AVER, ENTITY(SWICS_PLUS28V_PRIM_CURR).NZERO
C
C SWICS +5 V Voltage (Gnd Ref)
C
      CALL FIND_AVG(ENTITY(SWICS_PLUS5V))
      WRITE(11, 1000)' 11  SWICS +5 V Voltage (Gnd Ref)', 
     &		ENTITY(SWICS_PLUS5V).MIN, ENTITY(SWICS_PLUS5V).MAX,
     &		ENTITY(SWICS_PLUS5V).AVER, ENTITY(SWICS_PLUS5V).NZERO
C
C SWICS +5 V Current (Gnd Ref)
C
      CALL FIND_AVG(ENTITY(SWICS_PLUS5I))
      WRITE(11, 1000)' 12  SWICS +5 V Current (Gnd Ref)', 
     &		ENTITY(SWICS_PLUS5I).MIN, ENTITY(SWICS_PLUS5I).MAX,
     &		ENTITY(SWICS_PLUS5I).AVER, ENTITY(SWICS_PLUS5I).NZERO
C
C SWICS -5 V Voltage (Gnd Ref)
C
      CALL FIND_AVG(ENTITY(SWICS_MINUS5V))
      WRITE(11, 1000)' 13  SWICS -5 V Voltage (Gnd Ref)', 
     &		ENTITY(SWICS_MINUS5V).MIN, ENTITY(SWICS_MINUS5V).MAX,
     &		ENTITY(SWICS_MINUS5V).AVER, ENTITY(SWICS_MINUS5V).NZERO
C
C SWICS +8 V Voltage (Gnd Ref)
C
      CALL FIND_AVG(ENTITY(SWICS_PLUS8V))
      WRITE(11, 1000)' 14  SWICS +8 V Voltage (Gnd Ref)', 
     &		ENTITY(SWICS_PLUS8V).MIN, ENTITY(SWICS_PLUS8V).MAX,
     &		ENTITY(SWICS_PLUS8V).AVER, ENTITY(SWICS_PLUS8V).NZERO
C
C SWICS +8 V Current (Gnd Ref)
C
      CALL FIND_AVG(ENTITY(SWICS_PLUS8I))
      WRITE(11, 1000)'     SWICS +8 V Current (Gnd Ref)', 
     &		ENTITY(SWICS_PLUS8I).MIN, ENTITY(SWICS_PLUS8I).MAX,
     &		ENTITY(SWICS_PLUS8I).AVER, ENTITY(SWICS_PLUS8I).NZERO
C
C SWICS 20 V Voltage (Gnd Ref)
C
      CALL FIND_AVG(ENTITY(SWICS_20V))
      WRITE(11, 1000)' 15  SWICS 20 V Voltage (Gnd Ref)', 
     &		ENTITY(SWICS_20V).MIN, ENTITY(SWICS_20V).MAX,
     &		ENTITY(SWICS_20V).AVER, ENTITY(SWICS_20V).NZERO
C
C SWICS 20 V Current (Gnd Ref)
C
      CALL FIND_AVG(ENTITY(SWICS_20I))
      WRITE(11, 1000)' 16  SWICS 20 V Current (Gnd Ref)', 
     &		ENTITY(SWICS_20I).MIN, ENTITY(SWICS_20I).MAX,
     &		ENTITY(SWICS_20I).AVER, ENTITY(SWICS_20I).NZERO
C
C SWICS LVPS 28 V Voltage (Gnd Ref)
C
      CALL FIND_AVG(ENTITY(SWICS_LVPS_28V))
      WRITE(11, 1000)' 17  SWICS LVPS 28 V Voltage (Gnd Ref)', 
     &		ENTITY(SWICS_LVPS_28V).MIN, ENTITY(SWICS_LVPS_28V).MAX,
     &		ENTITY(SWICS_LVPS_28V).AVER, ENTITY(SWICS_LVPS_28V).NZERO
C
C SWICS DPPS 28 V Current (Gnd Ref)
C
      CALL FIND_AVG(ENTITY(SWICS_DPPS_28I))
      WRITE(11, 1000)' 18  SWICS DPPS 28 V Current (Gnd Ref)', 
     &		ENTITY(SWICS_DPPS_28I).MIN, ENTITY(SWICS_DPPS_28I).MAX,
     &		ENTITY(SWICS_DPPS_28I).AVER, ENTITY(SWICS_DPPS_28I).NZERO
C
C SWICS PAPS 28 V Current (Gnd Ref)
C
      CALL FIND_AVG(ENTITY(SWICS_PAPS_28I))
      WRITE(11, 1000)' 19  SWICS PAPS 28 V Current (Gnd Ref)', 
     &		ENTITY(SWICS_PAPS_28I).MIN, ENTITY(SWICS_PAPS_28I).MAX,
     &		ENTITY(SWICS_PAPS_28I).AVER, ENTITY(SWICS_PAPS_28I).NZERO
C
C SWICS PAPS Output Voltage (Gnd Ref)
C
      CALL FIND_AVG(ENTITY(SWICS_PAPS_OUTV))
      WRITE(11, 1100)' 20  SWICS PAPS Output Voltage (Gnd Ref)', 
     &		ENTITY(SWICS_PAPS_OUTV).MIN, ENTITY(SWICS_PAPS_OUTV).MAX,
     &		ENTITY(SWICS_PAPS_OUTV).AVER, ENTITY(SWICS_PAPS_OUTV).NZERO,
     &		INT(ENTITY(SWICS_PAPS_CL).LAST_VALUE),
     &		ENTITY(SWICS_PAPS_CV).LAST_VALUE
1100  FORMAT(1X,A,T45,3F12.2,5X,I3,10X,I5,6X,F12.2)
C
C SWICS +5 V Voltage (PAPS Ref)
C
      CALL FIND_AVG(ENTITY(SWICS_PLUS5V_PR))
      WRITE(11, 1000)' 21  SWICS +5 V Voltage (PAPS Ref)', 
     &		ENTITY(SWICS_PLUS5V_PR).MIN, ENTITY(SWICS_PLUS5V_PR).MAX,
     &		ENTITY(SWICS_PLUS5V_PR).AVER, ENTITY(SWICS_PLUS5V_PR).NZERO
C
C SWICS +5 V Current (PAPS Ref)
C
      CALL FIND_AVG(ENTITY(SWICS_PLUS5I_PR))
      WRITE(11, 1000)' 22  SWICS +5 V Current (PAPS Ref)', 
     &		ENTITY(SWICS_PLUS5I_PR).MIN, ENTITY(SWICS_PLUS5I_PR).MAX,
     &		ENTITY(SWICS_PLUS5I_PR).AVER, ENTITY(SWICS_PLUS5I_PR).NZERO
C
C SWICS -5 V Voltage (PAPS Ref)
C
      CALL FIND_AVG(ENTITY(SWICS_MINUS5V_PR))
      WRITE(11, 1000)' 23  SWICS -5 V Voltage (PAPS Ref)', 
     &		ENTITY(SWICS_MINUS5V_PR).MIN, ENTITY(SWICS_MINUS5V_PR).MAX,
     &		ENTITY(SWICS_MINUS5V_PR).AVER, ENTITY(SWICS_MINUS5V_PR).NZERO
C
C SWICS -5 V Current (PAPS Ref)
C
      CALL FIND_AVG(ENTITY(SWICS_MINUS5I_PR))
      WRITE(11, 1000)' 24  SWICS -5 V Current (PAPS Ref)', 
     &		ENTITY(SWICS_MINUS5I_PR).MIN, ENTITY(SWICS_MINUS5I_PR).MAX,
     &		ENTITY(SWICS_MINUS5I_PR).AVER, ENTITY(SWICS_MINUS5I_PR).NZERO
C
C SWICS +12 V Voltage (PAPS Ref)
C
      CALL FIND_AVG(ENTITY(SWICS_PLUS12V_PR))
      WRITE(11, 1000)' 25  SWICS +12 V Voltage (PAPS Ref)', 
     &		ENTITY(SWICS_PLUS12V_PR).MIN, ENTITY(SWICS_PLUS12V_PR).MAX,
     &		ENTITY(SWICS_PLUS12V_PR).AVER, ENTITY(SWICS_PLUS12V_PR).NZERO
C
C SWICS +12 V Current (PAPS Ref)
C
      CALL FIND_AVG(ENTITY(SWICS_PLUS12I_PR))
      WRITE(11, 1000)' 26  SWICS +12 V Current (PAPS Ref)', 
     &		ENTITY(SWICS_PLUS12I_PR).MIN, ENTITY(SWICS_PLUS12I_PR).MAX,
     &		ENTITY(SWICS_PLUS12I_PR).AVER, ENTITY(SWICS_PLUS12I_PR).NZERO
C
C -12 V Voltage (PAPS Ref)
C
      CALL FIND_AVG(ENTITY(SWICS_MINUS12V_PR))
      WRITE(11, 1000)' 27  SWICS -12 V Voltage (PAPS Ref)', 
     &		ENTITY(SWICS_MINUS12V_PR).MIN, ENTITY(SWICS_MINUS12V_PR).MAX,
     &		ENTITY(SWICS_MINUS12V_PR).AVER, ENTITY(SWICS_MINUS12V_PR).NZERO
C
C SWICS -12 V Current (PAPS Ref)
C
      CALL FIND_AVG(ENTITY(SWICS_MINUS12I_PR))
      WRITE(11, 1000)' 28  SWICS -12 V Current (PAPS Ref)', 
     &		ENTITY(SWICS_MINUS12I_PR).MIN, ENTITY(SWICS_MINUS12I_PR).MAX,
     &		ENTITY(SWICS_MINUS12I_PR).AVER, ENTITY(SWICS_MINUS12I_PR).NZERO
C
C SWICS SSD Bias Voltage (PAPS Ref)
C
      CALL FIND_AVG(ENTITY(SWICS_SSD_BIASV_PR))
      WRITE(11, 1000)'     SWICS SSD Bias Voltage (PAPS Ref)', 
     &		ENTITY(SWICS_SSD_BIASV_PR).MIN, ENTITY(SWICS_SSD_BIASV_PR).MAX,
     &		ENTITY(SWICS_SSD_BIASV_PR).AVER, ENTITY(SWICS_SSD_BIASV_PR).NZERO
C
C SWICS Start MCPPS Voltage (PAPS Ref)
C
      CALL FIND_AVG(ENTITY(SWICS_START_MCPPSV_PR))
      WRITE(11, 1100)'     SWICS Start MCPPS Voltage (PAPS Ref)', 
     &		ENTITY(SWICS_START_MCPPSV_PR).MIN, ENTITY(SWICS_START_MCPPSV_PR).MAX,
     &		ENTITY(SWICS_START_MCPPSV_PR).AVER, ENTITY(SWICS_START_MCPPSV_PR).NZERO,
     &		INT(ENTITY(SWICS_START_MCPPS_CL).LAST_VALUE),
     &		ENTITY(SWICS_START_MCPPS_CV).LAST_VALUE
C
C SWICS Stop MCPPS Voltage (PAPS Ref)
C
      CALL FIND_AVG(ENTITY(SWICS_STOP_MCPPSV_PR))
      WRITE(11, 1100)'     SWICS Stop MCPPS Voltage (PAPS Ref)', 
     &		ENTITY(SWICS_STOP_MCPPSV_PR).MIN, ENTITY(SWICS_STOP_MCPPSV_PR).MAX,
     &		ENTITY(SWICS_STOP_MCPPSV_PR).AVER, ENTITY(SWICS_STOP_MCPPSV_PR).NZERO,
     &		INT(ENTITY(SWICS_STOP_MCPPS_CL).LAST_VALUE),
     &		ENTITY(SWICS_STOP_MCPPS_CV).LAST_VALUE
C
C SWICS SSD Temp (PAPS Ref)
C
      CALL FIND_AVG(ENTITY(SWICS_SSD_TEMP_PR))
      WRITE(11, 1000)' 29  SWICS SSD Temp (PAPS Ref)', 
     &		ENTITY(SWICS_SSD_TEMP_PR).MIN, ENTITY(SWICS_SSD_TEMP_PR).MAX,
     &		ENTITY(SWICS_SSD_TEMP_PR).AVER, ENTITY(SWICS_SSD_TEMP_PR).NZERO
C
C SWICS Bubble Elect Temp, (PAPS Ref)
C
      CALL FIND_AVG(ENTITY(SWICS_BET_PR))
      WRITE(11, 1000)'     SWICS Bubble Elect Temp, (PAPS Ref)', 
     &		ENTITY(SWICS_BET_PR).MIN, ENTITY(SWICS_BET_PR).MAX,
     &		ENTITY(SWICS_BET_PR).AVER, ENTITY(SWICS_BET_PR).NZERO
C
C MASS +28 V Primary Current (Gnd Ref)
C
      CALL FIND_AVG(ENTITY(MASS_PLUS28I))
      WRITE(11, 1000)' 30  MASS +28 V Primary Current (Gnd Ref)', 
     &		ENTITY(MASS_PLUS28I).MIN, ENTITY(MASS_PLUS28I).MAX,
     &		ENTITY(MASS_PLUS28I).AVER, ENTITY(MASS_PLUS28I).NZERO
C
C MASS +5 V Voltage (Gnd Ref)
C
      CALL FIND_AVG(ENTITY(MASS_PLUS5V))
      WRITE(11, 1000)' 31  MASS +5 V Voltage (Gnd Ref)', 
     &		ENTITY(MASS_PLUS5V).MIN, ENTITY(MASS_PLUS5V).MAX,
     &		ENTITY(MASS_PLUS5V).AVER, ENTITY(MASS_PLUS5V).NZERO
C
C MASS +5 V Current (Gnd Ref)
C
      CALL FIND_AVG(ENTITY(MASS_PLUS5I))
      WRITE(11, 1000)' 32  MASS +5 V Current (Gnd Ref)', 
     &		ENTITY(MASS_PLUS5I).MIN, ENTITY(MASS_PLUS5I).MAX,
     &		ENTITY(MASS_PLUS5I).AVER, ENTITY(MASS_PLUS5I).NZERO
C
C MASS -5 V Voltage (Gnd Ref)
C
      CALL FIND_AVG(ENTITY(MASS_MINUS5V))
      WRITE(11, 1000)' 33  MASS -5 V Voltage (Gnd Ref)', 
     &		ENTITY(MASS_MINUS5V).MIN, ENTITY(MASS_MINUS5V).MAX,
     &		ENTITY(MASS_MINUS5V).AVER, ENTITY(MASS_MINUS5V).NZERO
C
C MASS +8 V Voltage (Gnd Ref)
C
      CALL FIND_AVG(ENTITY(MASS_PLUS8V))
      WRITE(11, 1000)' 34  MASS +8 V Voltage (Gnd Ref)', 
     &		ENTITY(MASS_PLUS8V).MIN, ENTITY(MASS_PLUS8V).MAX,
     &		ENTITY(MASS_PLUS8V).AVER, ENTITY(MASS_PLUS8V).NZERO
C
C MASS LVPS 28 V Voltage (Gnd Ref)
C
      CALL FIND_AVG(ENTITY(MASS_LVPS28V))
      WRITE(11, 1000)' 35  MASS LVPS 28 V Voltage (Gnd Ref)', 
     &		ENTITY(MASS_LVPS28V).MIN, ENTITY(MASS_LVPS28V).MAX,
     &		ENTITY(MASS_LVPS28V).AVER, ENTITY(MASS_LVPS28V).NZERO
C
C MASS ADPS 28 V Current (Gnd Ref)
C
      CALL FIND_AVG(ENTITY(MASS_ADPS28V))
      WRITE(11, 1000)' 36  MASS ADPS 28 V Current (Gnd Ref)', 
     &		ENTITY(MASS_ADPS28V).MIN, ENTITY(MASS_ADPS28V).MAX,
     &		ENTITY(MASS_ADPS28V).AVER, ENTITY(MASS_ADPS28V).NZERO
C
C MASS DPPS 28 V Current (Gnd Ref)
C
      CALL FIND_AVG(ENTITY(MASS_DPPS28I))
      WRITE(11, 1000)' 38  MASS DPPS 28 V Current (Gnd Ref)', 
     &		ENTITY(MASS_DPPS28I).MIN, ENTITY(MASS_DPPS28I).MAX,
     &		ENTITY(MASS_DPPS28I).AVER, ENTITY(MASS_DPPS28I).NZERO
C
C MASS Analog Electronics Temp (Gnd Ref)
C
      CALL FIND_AVG(ENTITY(MASS_AET))
      WRITE(11, 1000)' 39  MASS Analog Electronics Temp (Gnd Ref)', 
     &		ENTITY(MASS_AET).MIN, ENTITY(MASS_AET).MAX,
     &		ENTITY(MASS_AET).AVER, ENTITY(MASS_AET).NZERO
C
C MASS +5 V Voltage (ADPS Ref)
C
      CALL FIND_AVG(ENTITY(MASS_PLUS5V_AR))
      WRITE(11, 1000)' 40  MASS +5 V Voltage (ADPS Ref)', 
     &		ENTITY(MASS_PLUS5V_AR).MIN, ENTITY(MASS_PLUS5V_AR).MAX,
     &		ENTITY(MASS_PLUS5V_AR).AVER, ENTITY(MASS_PLUS5V_AR).NZERO
C
C MASS +5 V Current (ADPS Ref)
C
      CALL FIND_AVG(ENTITY(MASS_PLUS5I_AR))
      WRITE(11, 1000)' 41  MASS +5 V Current (ADPS Ref)', 
     &		ENTITY(MASS_PLUS5I_AR).MIN, ENTITY(MASS_PLUS5I_AR).MAX,
     &		ENTITY(MASS_PLUS5I_AR).AVER, ENTITY(MASS_PLUS5I_AR).NZERO
C
C MASS -5 V Voltage (ADPS Ref)
C
      CALL FIND_AVG(ENTITY(MASS_MINUS5V_AR))
      WRITE(11, 1000)' 42  MASS -5 V Voltage (ADPS Ref)', 
     &		ENTITY(MASS_MINUS5V_AR).MIN, ENTITY(MASS_MINUS5V_AR).MAX,
     &		ENTITY(MASS_MINUS5V_AR).AVER, ENTITY(MASS_MINUS5V_AR).NZERO
C
C MASS -5 V Current (ADPS Ref)
C
      CALL FIND_AVG(ENTITY(MASS_MINUS5I_AR))
      WRITE(11, 1000)' 43  MASS -5 V Current (ADPS Ref)', 
     &		ENTITY(MASS_MINUS5I_AR).MIN, ENTITY(MASS_MINUS5I_AR).MAX,
     &		ENTITY(MASS_MINUS5I_AR).AVER, ENTITY(MASS_MINUS5I_AR).NZERO
C
C MASS +6 V Voltage (ADPS Ref)
C
      CALL FIND_AVG(ENTITY(MASS_PLUS6V_AR))
      WRITE(11, 1000)' 44  MASS +6 V Voltage (ADPS Ref)', 
     &		ENTITY(MASS_PLUS6V_AR).MIN, ENTITY(MASS_PLUS6V_AR).MAX,
     &		ENTITY(MASS_PLUS6V_AR).AVER, ENTITY(MASS_PLUS6V_AR).NZERO
C
C MASS +6 V Current (ADPS Ref)
C
      CALL FIND_AVG(ENTITY(MASS_PLUS6I_AR))
      WRITE(11, 1000)' 45  MASS +6 V Current (ADPS Ref)', 
     &		ENTITY(MASS_PLUS6I_AR).MIN, ENTITY(MASS_PLUS6I_AR).MAX,
     &		ENTITY(MASS_PLUS6I_AR).AVER, ENTITY(MASS_PLUS6I_AR).NZERO
C
C MASS +12 V Voltage (ADPS Ref)
C
      CALL FIND_AVG(ENTITY(MASS_PLUS12V_AR))
      WRITE(11, 1000)' 46  MASS +12 V Voltage (ADPS Ref)', 
     &		ENTITY(MASS_PLUS12V_AR).MIN, ENTITY(MASS_PLUS12V_AR).MAX,
     &		ENTITY(MASS_PLUS12V_AR).AVER, ENTITY(MASS_PLUS12V_AR).NZERO
C
C MASS -12 V Voltage (ADPS Ref)
C
      CALL FIND_AVG(ENTITY(MASS_MINUS12V_AR))
      WRITE(11, 1000)' 47  MASS -12 V Voltage (ADPS Ref)', 
     &		ENTITY(MASS_MINUS12V_AR).MIN, ENTITY(MASS_MINUS12V_AR).MAX,
     &		ENTITY(MASS_MINUS12V_AR).AVER, ENTITY(MASS_MINUS12V_AR).NZERO
C
C MASS LVPS 28 V Voltage (ADPS Ref)
C
      CALL FIND_AVG(ENTITY(MASS_LVPS28V_AR))
      WRITE(11, 1000)' 48  MASS LVPS 28 V Voltage (ADPS Ref)', 
     &		ENTITY(MASS_LVPS28V_AR).MIN, ENTITY(MASS_LVPS28V_AR).MAX,
     &		ENTITY(MASS_LVPS28V_AR).AVER, ENTITY(MASS_LVPS28V_AR).NZERO
C
C MASS HPS & MCPPS 28V I (ADPS Ref)
C
      CALL FIND_AVG(ENTITY(MASS_HPS_MCPPS_28I_AR))
      WRITE(11, 1000)' 49  MASS HPS & MCPPS 28V I (ADPS Ref)', 
     &		ENTITY(MASS_HPS_MCPPS_28I_AR).MIN, ENTITY(MASS_HPS_MCPPS_28I_AR).MAX,
     &		ENTITY(MASS_HPS_MCPPS_28I_AR).AVER, ENTITY(MASS_HPS_MCPPS_28I_AR).NZERO
C
C MASS HPS Output Voltage (ADPS Ref)
C
      CALL FIND_AVG(ENTITY(MASS_HPS_OUTV_AR))
      WRITE(11, 1100)' 50  MASS HPS Output Voltage (ADPS Ref)', 
     &		ENTITY(MASS_HPS_OUTV_AR).MIN, ENTITY(MASS_HPS_OUTV_AR).MAX,
     &		ENTITY(MASS_HPS_OUTV_AR).AVER, ENTITY(MASS_HPS_OUTV_AR).NZERO,
     &		INT(ENTITY(MASS_HPS_CL).LAST_VALUE),
     &		ENTITY(MASS_HPS_CV).LAST_VALUE
C
C MASS Start MCPPS Voltage (ADPS Ref)
C
      CALL FIND_AVG(ENTITY(MASS_START_MCPPSV_AR))
      WRITE(11, 1100)'     MASS Start MCPPS Voltage (ADPS Ref)', 
     &		ENTITY(MASS_START_MCPPSV_AR).MIN, ENTITY(MASS_START_MCPPSV_AR).MAX,
     &		ENTITY(MASS_START_MCPPSV_AR).AVER, ENTITY(MASS_START_MCPPSV_AR).NZERO,
     &		INT(ENTITY(MASS_START_MCPPS_CL).LAST_VALUE),
     &		ENTITY(MASS_START_MCPPS_CV).LAST_VALUE
C
C MASS Stop MCPPS Voltage (ADPS Ref)
C
      CALL FIND_AVG(ENTITY(MASS_STOP_MCPPSV_AR))
      WRITE(11, 1100)'     MASS Stop MCPPS Voltage (ADPS Ref)', 
     &		ENTITY(MASS_STOP_MCPPSV_AR).MIN, ENTITY(MASS_STOP_MCPPSV_AR).MAX,
     &		ENTITY(MASS_STOP_MCPPSV_AR).AVER, ENTITY(MASS_STOP_MCPPSV_AR).NZERO,
     &		INT(ENTITY(MASS_STOP_MCPPS_CL).LAST_VALUE),
     &		ENTITY(MASS_STOP_MCPPS_CV).LAST_VALUE
C
C STICS 28 V Primary Current
C
      CALL FIND_AVG(ENTITY(STICS_28I))
      WRITE(11, 1000)' 51  STICS 28 V Primary Current', 
     &		ENTITY(STICS_28I).MIN, ENTITY(STICS_28I).MAX,
     &		ENTITY(STICS_28I).AVER, ENTITY(STICS_28I).NZERO
C
C STICS +5 V Voltage
C
      CALL FIND_AVG(ENTITY(STICS_PLUS5V))
      WRITE(11, 1000)' 52  STICS +5 V Voltage', 
     &		ENTITY(STICS_PLUS5V).MIN, ENTITY(STICS_PLUS5V).MAX,
     &		ENTITY(STICS_PLUS5V).AVER, ENTITY(STICS_PLUS5V).NZERO
C
C STICS -5 V Voltage
C
      CALL FIND_AVG(ENTITY(STICS_MINUS5V))
      WRITE(11, 1000)' 53  STICS -5 V Voltage', 
     &		ENTITY(STICS_MINUS5V).MIN, ENTITY(STICS_MINUS5V).MAX,
     &		ENTITY(STICS_MINUS5V).AVER, ENTITY(STICS_MINUS5V).NZERO
C
C STICS +6 V Voltage
C
      CALL FIND_AVG(ENTITY(STICS_PLUS6V))
      WRITE(11, 1000)' 54  STICS +6 V Voltage', 
     &		ENTITY(STICS_PLUS6V).MIN, ENTITY(STICS_PLUS6V).MAX,
     &		ENTITY(STICS_PLUS6V).AVER, ENTITY(STICS_PLUS6V).NZERO
C
C STICS -6 V Voltage
C
      CALL FIND_AVG(ENTITY(STICS_MINUS6V))
      WRITE(11, 1000)' 55  STICS -6 V Voltage', 
     &		ENTITY(STICS_MINUS6V).MIN, ENTITY(STICS_MINUS6V).MAX,
     &		ENTITY(STICS_MINUS6V).AVER, ENTITY(STICS_MINUS6V).NZERO
C
C STICS +12 V Voltage
C
      CALL FIND_AVG(ENTITY(STICS_PLUS12V))
      WRITE(11, 1000)' 56  STICS +12 V Voltage', 
     &		ENTITY(STICS_PLUS12V).MIN, ENTITY(STICS_PLUS12V).MAX,
     &		ENTITY(STICS_PLUS12V).AVER, ENTITY(STICS_PLUS12V).NZERO
C
C STICS -12 V Voltage
C
      CALL FIND_AVG(ENTITY(STICS_MINUS12V))
      WRITE(11, 1000)' 57  STICS -12 V Voltage', 
     &		ENTITY(STICS_MINUS12V).MIN, ENTITY(STICS_MINUS12V).MAX,
     &		ENTITY(STICS_MINUS12V).AVER, ENTITY(STICS_MINUS12V).NZERO
C
C STICS MCPPS 28 V Current
C
      CALL FIND_AVG(ENTITY(STICS_MCPPS28I))
      WRITE(11, 1000)' 58  STICS MCPPS 28 V Current', 
     &		ENTITY(STICS_MCPPS28I).MIN, ENTITY(STICS_MCPPS28I).MAX,
     &		ENTITY(STICS_MCPPS28I).AVER, ENTITY(STICS_MCPPS28I).NZERO
C
C STICS DPPS 28 V Current
C
      CALL FIND_AVG(ENTITY(STICS_DPPS28I))
      WRITE(11, 1000)' 59  STICS DPPS 28 V Current', 
     &		ENTITY(STICS_DPPS28I).MIN, ENTITY(STICS_DPPS28I).MAX,
     &		ENTITY(STICS_DPPS28I).AVER, ENTITY(STICS_DPPS28I).NZERO
C
C STICS SSD Bias Voltage
C
      CALL FIND_AVG(ENTITY(STICS_SSD_BIASV))
      WRITE(11, 1000)'     STICS SSD Bias Voltage', 
     &		ENTITY(STICS_SSD_BIASV).MIN, ENTITY(STICS_SSD_BIASV).MAX,
     &		ENTITY(STICS_SSD_BIASV).AVER, ENTITY(STICS_SSD_BIASV).NZERO
C
C STICS MCPPS 0 Output Voltage
C
      CALL FIND_AVG(ENTITY(STICS_MCPPS0_OUTV))
      WRITE(11, 1100)'     STICS MCPPS 0 Output Voltage', 
     &		ENTITY(STICS_MCPPS0_OUTV).MIN, ENTITY(STICS_MCPPS0_OUTV).MAX,
     &		ENTITY(STICS_MCPPS0_OUTV).AVER, ENTITY(STICS_MCPPS0_OUTV).NZERO,
     &		INT(ENTITY(STICS_MCPPS_0_CL).LAST_VALUE),
     &		ENTITY(STICS_MCPPS_0_CV).LAST_VALUE
C
C STICS MCPPS 1 Output Voltage
C
      CALL FIND_AVG(ENTITY(STICS_MCPPS1_OUTV))
      WRITE(11, 1100)'     STICS MCPPS 1 Output Voltage', 
     &		ENTITY(STICS_MCPPS1_OUTV).MIN, ENTITY(STICS_MCPPS1_OUTV).MAX,
     &		ENTITY(STICS_MCPPS1_OUTV).AVER, ENTITY(STICS_MCPPS1_OUTV).NZERO,
     &		INT(ENTITY(STICS_MCPPS_1_CL).LAST_VALUE),
     &		ENTITY(STICS_MCPPS_1_CV).LAST_VALUE
C
C STICS MCPPS 2 Output Voltage
C
      CALL FIND_AVG(ENTITY(STICS_MCPPS2_OUTV))
      WRITE(11, 1100)'     STICS MCPPS 2 Output Voltage', 
     &		ENTITY(STICS_MCPPS2_OUTV).MIN, ENTITY(STICS_MCPPS2_OUTV).MAX,
     &		ENTITY(STICS_MCPPS2_OUTV).AVER, ENTITY(STICS_MCPPS2_OUTV).NZERO,
     &		INT(ENTITY(STICS_MCPPS_2_CL).LAST_VALUE),
     &		ENTITY(STICS_MCPPS_2_CV).LAST_VALUE
C
C STICS MCPPS 3 Output Voltage
C
      CALL FIND_AVG(ENTITY(STICS_MCPPS3_OUTV))
      WRITE(11, 1100)'     STICS MCPPS 3 Output Voltage', 
     &		ENTITY(STICS_MCPPS3_OUTV).MIN, ENTITY(STICS_MCPPS3_OUTV).MAX,
     &		ENTITY(STICS_MCPPS3_OUTV).AVER, ENTITY(STICS_MCPPS3_OUTV).NZERO,
     &		INT(ENTITY(STICS_MCPPS_3_CL).LAST_VALUE),
     &		ENTITY(STICS_MCPPS_3_CV).LAST_VALUE
C
C STICS MCPPS 4 Output Voltage
C
      CALL FIND_AVG(ENTITY(STICS_MCPPS4_OUTV))
      WRITE(11, 1100)'     STICS MCPPS 4 Output Voltage', 
     &		ENTITY(STICS_MCPPS4_OUTV).MIN, ENTITY(STICS_MCPPS4_OUTV).MAX,
     &		ENTITY(STICS_MCPPS4_OUTV).AVER, ENTITY(STICS_MCPPS4_OUTV).NZERO,
     &		INT(ENTITY(STICS_MCPPS_4_CL).LAST_VALUE),
     &		ENTITY(STICS_MCPPS_4_CV).LAST_VALUE
C
C STICS MCPPS 5 Output Voltage
C
      CALL FIND_AVG(ENTITY(STICS_MCPPS5_OUTV))
      WRITE(11, 1100)'     STICS MCPPS 5 Output Voltage', 
     &		ENTITY(STICS_MCPPS5_OUTV).MIN, ENTITY(STICS_MCPPS5_OUTV).MAX,
     &		ENTITY(STICS_MCPPS5_OUTV).AVER, ENTITY(STICS_MCPPS5_OUTV).NZERO,
     &		INT(ENTITY(STICS_MCPPS_5_CL).LAST_VALUE),
     &		ENTITY(STICS_MCPPS_5_CV).LAST_VALUE
C
C STICS TOFPS (MCPPS 6) Output Voltage
C
      CALL FIND_AVG(ENTITY(STICS_TOFPS_OUTV))
      WRITE(11, 1100)'     STICS TOFPS (MCPPS 6) Output Voltage', 
     &		ENTITY(STICS_TOFPS_OUTV).MIN, ENTITY(STICS_TOFPS_OUTV).MAX,
     &		ENTITY(STICS_TOFPS_OUTV).AVER, ENTITY(STICS_TOFPS_OUTV).NZERO,
     &		INT(ENTITY(STICS_TOFPS_MCPPS_6_CL).LAST_VALUE),
     &		ENTITY(STICS_TOFPS_MCPPS_6_CV).LAST_VALUE
C
C STICS T1, AE Housing Temp
C
      CALL FIND_AVG(ENTITY(STICS_T1_AEHT))
      WRITE(11, 1000)' 60  STICS T1, AE Housing Temp', 
     &		ENTITY(STICS_T1_AEHT).MIN, ENTITY(STICS_T1_AEHT).MAX,
     &		ENTITY(STICS_T1_AEHT).AVER, ENTITY(STICS_T1_AEHT).NZERO
C
C STICS T2, Fairing/HOP Temp
C
      CALL FIND_AVG(ENTITY(STICS_T2_FHOPT))
      WRITE(11, 1000)'     STICS T2, Fairing/HOP Temp', 
     &		ENTITY(STICS_T2_FHOPT).MIN, ENTITY(STICS_T2_FHOPT).MAX,
     &		ENTITY(STICS_T2_FHOPT).AVER, ENTITY(STICS_T2_FHOPT).NZERO
C
C STICS T3, SSD Temp
C
      CALL FIND_AVG(ENTITY(STICS_T3_SSDT))
      WRITE(11, 1000)' 61  STICS T3, SSD Temp', 
     &		ENTITY(STICS_T3_SSDT).MIN, ENTITY(STICS_T3_SSDT).MAX,
     &		ENTITY(STICS_T3_SSDT).AVER, ENTITY(STICS_T3_SSDT).NZERO
C
C STICS T4, Back Panel Temp
C
      CALL FIND_AVG(ENTITY(STICS_T4_BPT))
      WRITE(11, 1000)'     STICS T4, Back Panel Temp', 
     &		ENTITY(STICS_T4_BPT).MIN, ENTITY(STICS_T4_BPT).MAX,
     &		ENTITY(STICS_T4_BPT).AVER, ENTITY(STICS_T4_BPT).NZERO
C
C From last good HDB, write Spacecraft Status information.  First, Telemetry
C Format... 
C
      WRITE(11, 1500)
1500  FORMAT(/,10X,'SPACECRAFT AND DPU STATUS ITEMS',/)
C      
C Build the next output line internally in the character variable LINE.  It
C will contain the telemetry format, telemetry rate, and spin rate.
C
      HDB = LAST_GOOD_HDB
      IF(BTEST(HDB.DAT(2), 6))THEN
        WRITE(LINE(1:37), 2000)
2000    FORMAT(1X,'     Telemetry Format: Science Mode ')
	VALUE = 1
      ELSE
        WRITE(LINE(1:37), 2010)
2010    FORMAT(1X,'     Telemetry Format: Maneuver Mode')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(TELEMETRY_FORMAT))
C
C...then Telemetry Rate
C
      IF(BTEST(HDB.DAT(2), 7))THEN
        WRITE(LINE(38:66), 2100)
2100    FORMAT('     Telemetry Rate 11130 BPS')
	VALUE = 11130
      ELSE
        WRITE(LINE(38:66), 2110)
2110    FORMAT('     Telemetry Rate 5565 BPS ')
	VALUE = 5565
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(TELEMETRY_RATE))
C
C Get spin period by taking (start time of spin 59 - start time of spin 0)/59, 
C convert it from ms to sec, and write it out
C
      SPIN_PERIOD = (SCI.EDB(59).EPOCH - SCI.EDB(0).EPOCH)/59000.0
      CALL FIND_STATS(1,SPIN_PERIOD,ENTITY(SPIN_PERIOD_IDX))
      WRITE(LINE(67:89), 2200) SPIN_PERIOD
2200  FORMAT('     Spin Period: ',F5.2)
C
C Now output the line we've built internally
C
      WRITE(11, 2205) LINE(1:89)
2205  FORMAT(A)
C
C DPU status Items: 
C
      VALUE = IAND(HDB.DAT(188), 63)
      WRITE(11,2300) VALUE
      CALL FIND_STATS_INT(VALUE,ENTITY(RESET_COUNTER))
2300  FORMAT(1X,'     Reset Counter: ',I2)
C
      RESET_CAUSE = ISHFT(IAND(HDB.DAT(188), 192), -6)
      CALL FIND_STATS_INT(RESET_CAUSE,ENTITY(LAST_RESET_CAUSE))
      WRITE(11,2310) RESET_CAUSE, RESET_CAUSE_TEXT(RESET_CAUSE)
2310  FORMAT(1X,'     Last Reset Cause: ',I2,' = ',A)
C
      VALUE = IAND(HDB.DAT(236), 15)
      CALL FIND_STATS_INT(VALUE,ENTITY(LATCHUP_COUNTER_MAIN))
      WRITE(11,2320)  VALUE
2320  FORMAT(1X,'     Latchup Counter - Main: ',I2)
C
      VALUE = IAND(HDB.DAT(234), 15)
      CALL FIND_STATS_INT(VALUE,ENTITY(LATCHUP_COUNTER_MEMORY))
      WRITE(11,2330) VALUE
2330  FORMAT(1X,'     Latchup Counter for Memory: ',I2)
C
      VALUE = ISHFT(IAND(HDB.DAT(234), 240), -4)
      CALL FIND_STATS_INT(VALUE,ENTITY(LATCHUP_COUNTER_MP))
      WRITE(11,2340) VALUE
2340  FORMAT(1X,'     Latchup Counter for MP: ',I2)
C
      VALUE = IAND(HDB.DAT(235), 15)
      CALL FIND_STATS_INT(VALUE,ENTITY(LATCHUP_COUNTER_SWICS))
      WRITE(11,2350) VALUE
2350  FORMAT(1X,'     Latchup Counter for SWICS Classification',I2)
C
      VALUE = ISHFT(IAND(HDB.DAT(235), 240), -4)
      CALL FIND_STATS_INT(VALUE,ENTITY(LATCHUP_COUNTER_STICS))
      WRITE(11,2360) VALUE
2360  FORMAT(1X,'     Latchup Counter for STICS Classification: ',I2)
C
      VALUE = ISHFT(IAND(HDB.DAT(236), 240), -4)
      CALL FIND_STATS_INT(VALUE,ENTITY(LATCHUP_COUNTER_MASS))
      WRITE(11,2370) VALUE
2370  FORMAT(1X,'     Latchup Counter for MASS Classification: ',I2)
C
      VALUE = IAND(HDB.DAT(169), 255)
      CALL FIND_STATS_INT(VALUE,ENTITY(RAM_ERROR_COUNTER))
      WRITE(11,2400) VALUE
2400  FORMAT(1X,'     RAM Error Counter: ',I2)
C
      VALUE = IAND(HDB.DAT(170), 255)
      CALL FIND_STATS_INT(VALUE,ENTITY(ROM_ERROR_COUNTER))
      WRITE(11,2410) VALUE
2410  FORMAT(1X,'     ROM Error Counter: ',I2)
C
      IF(BTEST(HDB.DAT(186),3))THEN
        WRITE(11, 2500)
2500    FORMAT(1X,'     PROM Bank Selection: B')
	VALUE = 1
      ELSE
        WRITE(11, 2510)
2510    FORMAT(1X,'     PROM Bank Selection: A')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(PROM_BANK_SELECTION))
C
      IF(BTEST(HDB.DAT(186),4))THEN
        WRITE(11, 2520)
2520    FORMAT(1X,'     PROM Toggle Mode Enabled')
	VALUE = 1
      ELSE
        WRITE(11, 2530)
2530    FORMAT(1X,'     PROM Toggle Mode Disabled')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(PROM_TOGGLE_MODE_ENABLED))
C
      IF(BTEST(HDB.DAT(190),6))THEN
        WRITE(11, 2600)
2600    FORMAT(1X,'     Program Patch Active')
	VALUE = 1
      ELSE
        WRITE(11, 2610)
2610    FORMAT(1X,'     Program Patch Inactive')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(PROGRAM_PATCH_ACTIVE))
C
      VALUE = IAND(HDB.DAT(176), 255)
      CALL FIND_STATS_INT(VALUE,ENTITY(VALID_CMD_COUNTER))
      WRITE(11, 2700) VALUE
2700  FORMAT(1X,'     Valid Command Counter: ', I3)
      VALUE = IAND(HDB.DAT(180), 255)
      CALL FIND_STATS_INT(VALUE,ENTITY(INVALID_CMD_COUNTER))
      WRITE(11, 2710) VALUE
2710  FORMAT(1X,'     Invalid Command Counter: ', I3)
C
      IF(BTEST(HDB.DAT(221),4))THEN
        WRITE(11, 2800)
2800    FORMAT(1X,'     Command Repetition for SWICS: Enabled')
	VALUE = 1
      ELSE
        WRITE(11, 2810)
2810    FORMAT(1X,'     Command Repetition for SWICS: Disabled')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(COMMAND_REPEAT_SWICS))
C
      IF(BTEST(HDB.DAT(221),3))THEN
        WRITE(11, 2820)
2820    FORMAT(1X,'     Command Repetition for MASS: Enabled')
	VALUE = 1
      ELSE
        WRITE(11, 2830)
2830    FORMAT(1X,'     Command Repetition for MASS: Disabled')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(COMMAND_REPEAT_MASS))
C
      IF(BTEST(HDB.DAT(221),5))THEN
        WRITE(11, 2840)
2840    FORMAT(1X,'     Command Repetition for STICS: Enabled')
	VALUE = 1
      ELSE
        WRITE(11, 2850)
2850    FORMAT(1X,'     Command Repetition for STICS: Disabled')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(COMMAND_REPEAT_STICS))
C
      IF(BTEST(HDB.DAT(189),7))THEN
        WRITE(11, 2900)
2900    FORMAT(1X,'     High Voltage Reconfiguration with HV')
	VALUE = 1
      ELSE
        WRITE(11, 2910)
2910    FORMAT(1X,'     High Voltage Reconfiguration without HV')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(HV_RECONFIG_WITHOUT_HV))
C
      VALUE = IAND(HDB.DAT(165), 15)
      CALL FIND_STATS_INT(VALUE,ENTITY(SUN_SECTOR))
      CALL FIND_STATS_INT(OFFSET,ENTITY(SUN_SECTOR_OFFSET))
      WRITE(11,3000) VALUE, OFFSET
3000  FORMAT(1X,'     Sun Sector/Offset: ',I2,'/',I3)
C
      IF(BTEST(HDB.DAT(190),0))THEN
        WRITE(11, 3020)
3020    FORMAT(1X,'     Sun Sector Clock Source: Internal')
	VALUE = 1
      ELSE
        WRITE(11, 3030)
3030    FORMAT(1X,'     Sun Sector Clock Source: External')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(SUN_SECTOR_CLOCK_SRC))
C
      IF(BTEST(HDB.DAT(190),3))THEN
        WRITE(11, 3040)
3040    FORMAT(1X,'     Sun Sector Clock Lost')
	VALUE = 1
      ELSE
        WRITE(11, 3050)
3050    FORMAT(1X,'     Sun Sector Clock OK')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(SUN_SECTOR_CLOCK_OK))
C
      IF(BTEST(HDB.DAT(190),4))THEN
        WRITE(11, 3060)
3060    FORMAT(1X,'     Sun Pulse Lost')
	VALUE = 1
      ELSE
        WRITE(11, 3070)
3070    FORMAT(1X,'     Sun Pulse OK')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(SUN_PULSE_OK))
C
C Prepare Power Switch Status Table
C
C      IF(BTEST(HDB.DAT(173),0))STICS_COVER  = 'ON '
C      IF(BTEST(HDB.DAT(173),1))STICS_HEATER = 'ON '
C      IF(BTEST(HDB.DAT(173),2))SWICS_COVER  = 'ON '
C      IF(BTEST(HDB.DAT(173),3))SWICS_HEATER = 'ON '
C      IF(BTEST(HDB.DAT(173),4))MASS_COVER   = 'ON '
C      IF(BTEST(HDB.DAT(173),5))STICS_MAIN   = 'ON '
C      IF(BTEST(HDB.DAT(173),6))SWICS_MAIN   = 'ON ' 
C      IF(BTEST(HDB.DAT(173),7))MASS_MAIN    = 'ON '
      VALUE = IBITS(HDB.DAT(173),0,1)
      CALL FIND_STATS_INT(VALUE,ENTITY(STICS_COVER_IDX))
      IF (VALUE .EQ. 1) STICS_COVER  = 'ON '
      VALUE = IBITS(HDB.DAT(173),1,1)
      CALL FIND_STATS_INT(VALUE,ENTITY(STICS_HEATER_IDX))
      IF (VALUE .EQ. 1) STICS_HEATER = 'ON '
      VALUE = IBITS(HDB.DAT(173),2,1)
      CALL FIND_STATS_INT(VALUE,ENTITY(SWICS_COVER_IDX))
      IF (VALUE .EQ. 1) SWICS_COVER  = 'ON '
      VALUE = IBITS(HDB.DAT(173),3,1)
      CALL FIND_STATS_INT(VALUE,ENTITY(SWICS_HEATER_IDX))
      IF (VALUE .EQ. 1) SWICS_HEATER = 'ON '
      VALUE = IBITS(HDB.DAT(173),4,1)
      CALL FIND_STATS_INT(VALUE,ENTITY(MASS_COVER_IDX))
      IF (VALUE .EQ. 1) MASS_COVER   = 'ON '
      VALUE = IBITS(HDB.DAT(173),5,1)
      CALL FIND_STATS_INT(VALUE,ENTITY(STICS_MAIN_IDX))
      IF (VALUE .EQ. 1) STICS_MAIN   = 'ON '
      VALUE = IBITS(HDB.DAT(173),6,1)
      CALL FIND_STATS_INT(VALUE,ENTITY(SWICS_MAIN_IDX))
      IF (VALUE .EQ. 1) SWICS_MAIN   = 'ON ' 
      VALUE = IBITS(HDB.DAT(173),7,1)
      CALL FIND_STATS_INT(VALUE,ENTITY(MASS_MAIN_IDX))
      IF (VALUE .EQ. 1) MASS_MAIN    = 'ON '
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
C      IF(BTEST(HDB.DAT(187),0))STICS_CONFIG   = 'Error '
C      IF(BTEST(HDB.DAT(187),1))STICS_SNAPSHOT = 'Error '
C      IF(BTEST(HDB.DAT(187),2))SWICS_CONFIG   = 'Error '
C      IF(BTEST(HDB.DAT(187),3))SWICS_SNAPSHOT = 'Error '
C      IF(BTEST(HDB.DAT(187),4))MASS_CONFIG    = 'Error '
C      IF(BTEST(HDB.DAT(187),5))MASS_SNAPSHOT  = 'Error '
C      IF(BTEST(HDB.DAT(187),6))LOAD_CONFIG    = 'Error '
C      IF(BTEST(HDB.DAT(187),7))LOAD_SNAPSHOT  = 'Error '
      VALUE = IBITS(HDB.DAT(187),0,1)
      CALL FIND_STATS_INT(VALUE,ENTITY(STICS_CONFIG_IDX))
      IF (VALUE .EQ. 1) STICS_CONFIG   = 'Error '
      VALUE = IBITS(HDB.DAT(187),1,1)
      CALL FIND_STATS_INT(VALUE,ENTITY(STICS_SNAPSHOT_IDX))
      IF (VALUE .EQ. 1) STICS_SNAPSHOT = 'Error '
      VALUE = IBITS(HDB.DAT(187),2,1)
      CALL FIND_STATS_INT(VALUE,ENTITY(SWICS_CONFIG_IDX))
      IF (VALUE .EQ. 1) SWICS_CONFIG   = 'Error '
      VALUE = IBITS(HDB.DAT(187),3,1)
      CALL FIND_STATS_INT(VALUE,ENTITY(SWICS_SNAPSHOT_IDX))
      IF (VALUE .EQ. 1) SWICS_SNAPSHOT = 'Error '
      VALUE = IBITS(HDB.DAT(187),4,1)
      CALL FIND_STATS_INT(VALUE,ENTITY(MASS_CONFIG_IDX))
      IF (VALUE .EQ. 1) MASS_CONFIG    = 'Error '
      VALUE = IBITS(HDB.DAT(187),5,1)
      CALL FIND_STATS_INT(VALUE,ENTITY(MASS_SNAPSHOT_IDX))
      IF (VALUE .EQ. 1) MASS_SNAPSHOT  = 'Error '
      VALUE = IBITS(HDB.DAT(187),6,1)
      CALL FIND_STATS_INT(VALUE,ENTITY(LOAD_CONFIG_IDX))
      IF (VALUE .EQ. 1) LOAD_CONFIG    = 'Error '
      VALUE = IBITS(HDB.DAT(187),7,1)
      CALL FIND_STATS_INT(VALUE,ENTITY(LOAD_SNAPSHOT_IDX))
      IF (VALUE .EQ. 1) LOAD_SNAPSHOT  = 'Error '
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
C      IF(BTEST(HDB.DAT(107),0))MASS_HPS       = 'ON '
C      IF(BTEST(HDB.DAT(107),1))MASS_STOP_MCP  = 'ON '
C      IF(BTEST(HDB.DAT(107),2))MASS_START_MCP = 'ON '
C      IF(BTEST(HDB.DAT(107),3))MASS_COM_ADPS  = 'ON '
C      IF(BTEST(HDB.DAT(107),4))MASS_COM_DPPS  = 'ON '
      VALUE = IBITS(HDB.DAT(107),0,1)
      CALL FIND_STATS_INT(VALUE,ENTITY(MASS_HPS_ENABLE))
      IF (VALUE .EQ. 1) MASS_HPS       = 'ON '
      VALUE = IBITS(HDB.DAT(107),1,1)
      CALL FIND_STATS_INT(VALUE,ENTITY(MASS_STOP_MCP_ENABLE))
      IF (VALUE .EQ. 1) MASS_STOP_MCP  = 'ON '
      VALUE = IBITS(HDB.DAT(107),2,1)
      CALL FIND_STATS_INT(VALUE,ENTITY(MASS_START_MCP_ENABLE))
      IF (VALUE .EQ. 1) MASS_START_MCP = 'ON '
      VALUE = IBITS(HDB.DAT(107),3,1)
      CALL FIND_STATS_INT(VALUE,ENTITY(MASS_COM_ADPS_ENABLE))
      IF (VALUE .EQ. 1) MASS_COM_ADPS  = 'ON '
      VALUE = IBITS(HDB.DAT(107),4,1)
      CALL FIND_STATS_INT(VALUE,ENTITY(MASS_COM_DPPS_ENABLE))
      IF (VALUE .EQ. 1) MASS_COM_DPPS  = 'ON '
C      
      MASS_HPS_MEAS_V = -3.428E1 + 1.5285E2 * IAND(HDB.DAT(117), 255)
      MASS_STOP_MCP_MEAS_V = 3.065E1 * IAND(HDB.DAT(120), 255)
      MASS_START_MCP_MEAS_V = 1.310E1 + 1.431E1 * IAND(HDB.DAT(119), 255)
C
C      WRITE(11, 4000) MASS_HPS, IAND(HDB.DAT(111), 255), IAND(HDB.DAT(110), 255), MASS_HPS_MEAS_V,
C     &                MASS_STOP_MCP, IAND(HDB.DAT(115), 255), IAND(HDB.DAT(113), 255), MASS_STOP_MCP_MEAS_V, 
C     &                MASS_START_MCP, IAND(HDB.DAT(114), 255), IAND(HDB.DAT(112), 255), MASS_START_MCP_MEAS_V, 
C     &                MASS_COM_ADPS, IAND(HDB.DAT(232), 255), IAND(HDB.DAT(233), 255), 
C     &                MASS_COM_DPPS
C
      CALL FIND_STATS_INT(IAND(HDB.DAT(111), 255), ENTITY(MASS_HPS_LIMIT))
      CALL FIND_STATS_INT(IAND(HDB.DAT(115), 255), ENTITY(MASS_STOP_MCP_LIMIT))
      CALL FIND_STATS_INT(IAND(HDB.DAT(114), 255), ENTITY(MASS_START_MCP_LIMIT))
      CALL FIND_STATS_INT(IAND(HDB.DAT(232), 255), ENTITY(MASS_ADPS_LIMIT))
      CALL FIND_STATS_INT(IAND(HDB.DAT(233), 255), ENTITY(MASS_ADPS_LEVEL))
      WRITE(11, 4000) MASS_HPS, INT(ENTITY(MASS_HPS_LIMIT).LAST_VALUE), 
     &                  INT(ENTITY(MASS_HPS_CL).LAST_VALUE), 
     &                  ENTITY(MASS_HPS_OUTV_AR).LAST_VALUE,
     &                MASS_STOP_MCP, INT(ENTITY(MASS_STOP_MCP_LIMIT).LAST_VALUE), 
     &                  INT(ENTITY(MASS_STOP_MCPPS_CL).LAST_VALUE), 
     &                  ENTITY(MASS_STOP_MCPPSV_AR).LAST_VALUE,
     &                MASS_START_MCP, INT(ENTITY(MASS_START_MCP_LIMIT).LAST_VALUE), 
     &                  INT(ENTITY(MASS_START_MCPPS_CL).LAST_VALUE), 
     &                  ENTITY(MASS_START_MCPPSV_AR).LAST_VALUE,
     &                MASS_COM_ADPS, INT(ENTITY(MASS_ADPS_LIMIT).LAST_VALUE), 
     &                  INT(ENTITY(MASS_ADPS_LEVEL).LAST_VALUE), 
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
	VALUE = 1
      ELSE
        WRITE(11, 4110)
4110    FORMAT(1X,'     ADPS automatic calculation: OFF')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(ADPS_AUTO_CALC))
C
      IF(BTEST(HDB.DAT(130),0))THEN
        WRITE(11, 4120)
4120    FORMAT(1X,'     ADPS stepping sequence: RAM')
	VALUE = 1
      ELSE
        WRITE(11, 4130)
4130    FORMAT(1X,'     ADPS stepping sequence: ROM')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(ADPS_STEPPING_SEQ))
C
      IF(BTEST(HDB.DAT(130),6))THEN
        WRITE(11, 4140)
4140    FORMAT(1X,'     ADPS table: Fixed')
	VALUE = 1
      ELSE
        WRITE(11, 4150)
4150    FORMAT(1X,'     ADPS table: Not Fixed')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(ADPS_TABLE))
C
      IF(BTEST(HDB.DAT(130),1))THEN
        WRITE(11, 4160)
4160    FORMAT(1X,'     DPPS stepping sequence: RAM')
	VALUE = 1
      ELSE
        WRITE(11, 4170)
4170    FORMAT(1X,'     DPPS stepping sequence: ROM')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(MASS_DPPS_STEPPING_SEQ))
C
      VALUE = IAND(HDB.DAT(133), 255)
      CALL FIND_STATS(VALUE,FLOAT(VALUE),ENTITY(FSR_LIMIT))
      WRITE(11, 4200) VALUE
4200  FORMAT(1X,'     FSR limit for stop reversal: ',I3)
C
      IF(BTEST(HDB.DAT(223),0))THEN
        WRITE(11, 4300)
4300    FORMAT(1X,'     Discharge occurred')
	VALUE = 1
      ELSE
        WRITE(11, 4310)
4310    FORMAT(1X,'     No discharge')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(DISCHARGE_OCCURRED))
C
      IF(BTEST(HDB.DAT(130),5))THEN
        WRITE(11, 4400)
4400    FORMAT(1X,'     Internal callibrator: Active')
	VALUE = 1
      ELSE
        WRITE(11, 4410)
4410    FORMAT(1X,'     Internal Callibrator: Inactive')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(INTERNAL_CALLIB))
C
      VALUE = IAND(HDB.DAT(116), 15)
      CALL FIND_STATS_INT(VALUE,ENTITY(CALLIB_LEVEL))
      WRITE(11, 4420) VALUE
4420  FORMAT(1X,'     Callibrator level: ',I2)
C
      IF(BTEST(HDB.DAT(130),4))THEN
        WRITE(11, 4500)
4500    FORMAT(1X,'     Test tables: Active')
	VALUE = 1
      ELSE
        WRITE(11, 4510)
4510    FORMAT(1X,'     Test tables: Inactive')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(MASS_TEST_TABLES))
C
      IF(BTEST(HDB.DAT(130),7))THEN
        WRITE(11, 4600)
4600    FORMAT(1X,'     Command Echo Check: Enabled')
	VALUE = 1
      ELSE
        WRITE(11, 4610)
4610    FORMAT(1X,'     Command Echo Check: Disabled')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(MASS_CMD_ECHO_CHK))
C
      IF(BTEST(HDB.DAT(130),3))THEN
        WRITE(11, 4700)
4700    FORMAT(1X,'     PHA FIFO mode: Enabled')
	VALUE = 1
      ELSE
        WRITE(11, 4710)
4710    FORMAT(1X,'     PHA FIFO mode: Disabled')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(MASS_PHA_FIFO_MODE))
C
      IF(BTEST(HDB.DAT(116),5))THEN
        WRITE(11, 4800)
4800    FORMAT(1X,'     Stop preamp power: OFF')
	VALUE = 1
      ELSE
        WRITE(11, 4810)
4810    FORMAT(1X,'     Stop preamp power: ON')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(STOP_PREAMP_POWER))
C
      IF(BTEST(HDB.DAT(116),6))THEN
        WRITE(11, 4820)
4820    FORMAT(1X,'     Start preamp 1 power: OFF')
	VALUE = 1
      ELSE
        WRITE(11, 4830)
4830    FORMAT(1X,'     Start preamp 1 power: ON')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(START_PREAMP1_POWER))
C
      IF(BTEST(HDB.DAT(116),7))THEN
        WRITE(11, 4840)
4840    FORMAT(1X,'     Start preamp 2 power: OFF')
	VALUE = 1
      ELSE
        WRITE(11, 4850)
4850    FORMAT(1X,'     Start preamp 2 power: ON')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(START_PREAMP2_POWER))
C
      IF(BTEST(HDB.DAT(172),0))THEN
        WRITE(11, 4900)
4900    FORMAT(1X,'     Multiple Stops: Enabled')
	VALUE = 1
      ELSE
        WRITE(11, 4910)
4910    FORMAT(1X,'     Multiple Stops: Disabled')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(MASS_MULT_STOPS))
C
      IF(BTEST(HDB.DAT(172),1))THEN
        WRITE(11, 4920)
4920    FORMAT(1X,'     Multiple Starts: Enabled')
	VALUE = 1
      ELSE
        WRITE(11, 4930)
4930    FORMAT(1X,'     Multiple Starts: Disabled')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(MASS_MULT_STARTS))
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
      VALUE = ISHFT(ZEXT(HDB.DAT(131)),8) + ZEXT(HDB.DAT(132))
      CALL FIND_STATS_INT(VALUE,ENTITY(MASS_SUN_SECTOR_DEF_IDX))
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
C The loop below scans the matrix rate values and loads up the border entity
C data structures.  It assumes they start at index MR_BORDER, with the lower
C border preceeding the upper border with a given rate.  By doing this if any
C of the border values are not what they are expected to be the user will be
C notified.
C
      DO I=0,9
	CALL FIND_STATS_INT(MR(1,I),ENTITY(MR_BORDER+2*I))
	CALL FIND_STATS_INT(MR(2,I),ENTITY(MR_BORDER+2*I+1))
      ENDDO
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
      CALL FIND_STATS_INT(BR0_BR1_BORDER,ENTITY(BR0_BR1_BORDER_IDX))
C
      WRITE(11, 5120) BR0_BR1_BORDER
5120  FORMAT(1X,'     BR0/BR1 border: ',I4)
C
      IF(BTEST(HDB.DAT(189),4))THEN
        COMP_CODE(1) = 'C'
	VALUE = 1
      ELSE
        COMP_CODE(1) = 'A'
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(MR_COMP_CODE_GROUP1))
C
      IF(BTEST(HDB.DAT(189),5))THEN
        COMP_CODE(2) = 'C'
	VALUE = 1
      ELSE
        COMP_CODE(2) = 'A'
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(MR_COMP_CODE_GROUP2))
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
        CALL FIND_STATS_INT(IBITS(HDB.DAT(24),I,1),ENTITY(STICS_MCPPS0_ENABLE_IDX+I))
        CALL FIND_STATS_INT(IAND(HDB.DAT(10+I), 255), ENTITY(STICS_MCPPS0_LIMIT+I))
        CALL FIND_STATS_INT(IAND(HDB.DAT(17+I), 255), ENTITY(STICS_MCPPS0_LEVEL+I))
5400  CONTINUE                                
C     
      IF(BTEST(HDB.DAT(24),6))STICS_TOFPS  = 'ON '
      CALL FIND_STATS_INT(IBITS(HDB.DAT(24),6,1),ENTITY(STICS_TOFPS_ENABLE))
      IF(BTEST(HDB.DAT(24),7))STICS_PLUS_DPPS  = 'ON '
      CALL FIND_STATS_INT(IBITS(HDB.DAT(24),7,1),ENTITY(STICS_PLUS_DPPS_ENABLE))
      IF(BTEST(HDB.DAT(25),0))STICS_MINUS_DPPS  = 'ON '
      CALL FIND_STATS_INT(IBITS(HDB.DAT(25),0,1),ENTITY(STICS_MINUS_DPPS_ENABLE))
C
      CALL FIND_STATS_INT(IAND(HDB.DAT(16), 255), ENTITY(STICS_TOFPS_LIMIT))
      CALL FIND_STATS_INT(IAND(HDB.DAT(23), 255), ENTITY(STICS_TOFPS_LEVEL))
C      
C      STICS_MCPPS_MEAS_V(0) = -7.806   + 1.959E1 * IAND(HDB.DAT(27), 255)
C      STICS_MCPPS_MEAS_V(1) = -3.475   + 1.950E1 * IAND(HDB.DAT(28), 255)
C      STICS_MCPPS_MEAS_V(2) = -9.405   + 1.957E1 * IAND(HDB.DAT(29), 255)
C      STICS_MCPPS_MEAS_V(3) = -1.950E1 + 1.984E1 * IAND(HDB.DAT(30), 255)
C      STICS_MCPPS_MEAS_V(4) = -1.436E1 + 1.984E1 * IAND(HDB.DAT(31), 255)
C      STICS_MCPPS_MEAS_V(5) = -7.293   + 1.969E1 * IAND(HDB.DAT(32), 255)
C      STICS_TOFPS_MEAS_V    = -5.518   + 1.961E1 * IAND(HDB.DAT(33), 255)
      STICS_MCPPS_MEAS_V(0) = ENTITY(STICS_MCPPS0_OUTV).LAST_VALUE
      STICS_MCPPS_MEAS_V(1) = ENTITY(STICS_MCPPS1_OUTV).LAST_VALUE
      STICS_MCPPS_MEAS_V(2) = ENTITY(STICS_MCPPS2_OUTV).LAST_VALUE
      STICS_MCPPS_MEAS_V(3) = ENTITY(STICS_MCPPS3_OUTV).LAST_VALUE
      STICS_MCPPS_MEAS_V(4) = ENTITY(STICS_MCPPS4_OUTV).LAST_VALUE
      STICS_MCPPS_MEAS_V(5) = ENTITY(STICS_MCPPS5_OUTV).LAST_VALUE
      STICS_TOFPS_MEAS_V    = ENTITY(STICS_TOFPS_OUTV).LAST_VALUE
C
C      WRITE(11, 5500) STICS_PLUS_DPPS, 
C     &                STICS_MINUS_DPPS,
C     &                STICS_MCPPS(0), IAND(HDB.DAT(10), 255), IAND(HDB.DAT(17), 255), STICS_MCPPS_MEAS_V(0), 
C     &                STICS_MCPPS(1), IAND(HDB.DAT(11), 255), IAND(HDB.DAT(18), 255), STICS_MCPPS_MEAS_V(1), 
C     &                STICS_MCPPS(2), IAND(HDB.DAT(12), 255), IAND(HDB.DAT(19), 255), STICS_MCPPS_MEAS_V(2), 
C     &                STICS_MCPPS(3), IAND(HDB.DAT(13), 255), IAND(HDB.DAT(20), 255), STICS_MCPPS_MEAS_V(3), 
C     &                STICS_MCPPS(4), IAND(HDB.DAT(14), 255), IAND(HDB.DAT(21), 255), STICS_MCPPS_MEAS_V(4), 
C     &                STICS_MCPPS(5), IAND(HDB.DAT(15), 255), IAND(HDB.DAT(22), 255), STICS_MCPPS_MEAS_V(5), 
C     &                STICS_TOFPS, IAND(HDB.DAT(16), 255), IAND(HDB.DAT(23), 255), STICS_TOFPS_MEAS_V
      WRITE(11, 5500) STICS_PLUS_DPPS, 
     &                STICS_MINUS_DPPS,
     &                ( STICS_MCPPS(I),
     &                  INT(ENTITY(STICS_MCPPS0_LIMIT+I).LAST_VALUE),
     &                  INT(ENTITY(STICS_MCPPS0_LEVEL+I).LAST_VALUE),
     &                  ENTITY(STICS_MCPPS0_OUTV).LAST_VALUE,
     &                I=0,5),
     &                STICS_TOFPS, INT(ENTITY(STICS_TOFPS_LIMIT).LAST_VALUE),
     &                  INT(ENTITY(STICS_TOFPS_LEVEL).LAST_VALUE),
     &                  ENTITY(STICS_TOFPS_OUTV).LAST_VALUE
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
      VALUE = ISHFT(IAND(HDB.DAT(25),192), -6)
      CALL FIND_STATS_INT(VALUE,ENTITY(STICS_DPPS_STEP_SEQ))
      IF(VALUE.EQ.0)THEN
        WRITE(11, 5600)
5600    FORMAT(1X,'     DPPS Stepping Sequence: ROM, Fine Mode')
      ELSEIF(VALUE.EQ.1)THEN
        WRITE(11, 5610)
5610    FORMAT(1X,'     DPPS Stepping Sequence: ROM, Medium Mode')
      ELSEIF(VALUE.EQ.2)THEN
        WRITE(11, 5620)
5620    FORMAT(1X,'     DPPS Stepping Sequence: ROM, Fast Mode')
      ELSEIF(VALUE.EQ.3)THEN
        WRITE(11, 5630)
5630    FORMAT(1X,'     DPPS Stepping Sequence: RAM Table')
      ENDIF
C
      IF(BTEST(HDB.DAT(26),1))THEN
        WRITE(11, 5700)
5700    FORMAT(1X,'     Internal Callibrator power: ON')
	VALUE = 1
      ELSE
        WRITE(11, 5710)
5710    FORMAT(1X,'     Internal Callibrator power: OFF')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(INTERNAL_CALLIB_POWER))
C
      IF(BTEST(HDB.DAT(26),0))THEN
        WRITE(11, 5720)
5720    FORMAT(1X,'     Auto Callibrate Sequence: Enabled')
	VALUE = 1
      ELSE
        WRITE(11, 5730)
5730    FORMAT(1X,'     Auto Callibrate Sequence: Disabled')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(AUTO_CALLIB_SEQ))
C
C      IF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.0)THEN
C        WRITE(11, 5740)
C5740    FORMAT(1X,'     Commanded Time Callibrator Level: 300 ns')
C      ELSEIF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.1)THEN
C        WRITE(11, 5750)
C5750    FORMAT(1X,'     Commanded Time Callibrator Level: 280 ns')
C      ELSEIF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.2)THEN
C        WRITE(11, 5760)
C5760    FORMAT(1X,'     Commanded Time Callibrator Level: 260 ns')
C      ELSEIF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.3)THEN
C        WRITE(11, 5770)
C5770    FORMAT(1X,'     Commanded Time Callibrator Level: 240 ns')
C      ELSEIF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.4)THEN
C        WRITE(11, 5780)
C5780    FORMAT(1X,'     Commanded Time Callibrator Level: 220 ns')
C      ELSEIF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.5)THEN
C        WRITE(11, 5790)
C5790    FORMAT(1X,'     Commanded Time Callibrator Level: 200 ns')
C      ELSEIF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.6)THEN
C        WRITE(11, 5800)
C5800    FORMAT(1X,'     Commanded Time Callibrator Level: 180 ns')
C      ELSEIF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.7)THEN
C        WRITE(11, 5810)
C5810    FORMAT(1X,'     Commanded Time Callibrator Level: 160 ns')
C      ELSEIF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.8)THEN
C        WRITE(11, 5820)
C5820    FORMAT(1X,'     Commanded Time Callibrator Level: 140 ns')
C      ELSEIF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.9)THEN
C        WRITE(11, 5830)
C5830    FORMAT(1X,'     Commanded Time Callibrator Level: 120 ns')
C      ELSEIF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.10)THEN
C        WRITE(11, 5840)
C5840    FORMAT(1X,'     Commanded Time Callibrator Level: 100 ns')
C      ELSEIF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.11)THEN
C        WRITE(11, 5850)
C5850    FORMAT(1X,'     Commanded Time Callibrator Level: 80 ns')
C      ELSEIF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.12)THEN
C        WRITE(11, 5860)
C5860    FORMAT(1X,'     Commanded Time Callibrator Level: 60 ns')
C      ELSEIF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.13)THEN
C        WRITE(11, 5870)
C5870    FORMAT(1X,'     Commanded Time Callibrator Level: 40 ns')
C      ELSEIF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.14)THEN
C        WRITE(11, 5880)
C5880    FORMAT(1X,'     Commanded Time Callibrator Level: 20 ns')
C      ELSEIF(ISHFT(IAND(HDB.DAT(53), 60), -2).EQ.15)THEN
C        WRITE(11, 5890)
C5890    FORMAT(1X,'     Commanded Time Callibrator Level: Invalid')
C      ENDIF
C
C The ten lines of code below duplicate the function of the multiple 
C IF-THEN-ELSEs above
C
      VALUE = ISHFT(IAND(HDB.DAT(53), 60), -2)
      VALUE = CAL_LEVEL(VALUE)
      CALL FIND_STATS_INT(VALUE,ENTITY(CMD_TIME_CALLIB_LVL))
      IF (VALUE .NE. -1) THEN
        WRITE(11, 5895) VALUE
5895    FORMAT(1X,'     Commanded Time Callibrator Level: ',I3,' ns')
      ELSE
        WRITE(11, 5896)
5896    FORMAT(1X,'     Commanded Time Callibrator Level: Invalid')
      ENDIF
C
      VALUE = ZEXT(HDB.DAT(54))
      CALL FIND_STATS_INT(VALUE,ENTITY(STICS_ENERGY_CALLIB_LVL))
      WRITE(11, 6000) VALUE
6000  FORMAT(1X,'     Energy Callibrator Level: ',I3) 
C
      IF(BTEST(HDB.DAT(26),7))THEN
        WRITE(11, 6100)
6100    FORMAT(1X,'     Command Echo Check: Enabled')
	VALUE = 1
      ELSE
        WRITE(11, 6110)
6110    FORMAT(1X,'     Command Echo Check: Disabled')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(STICS_CMD_ECHO_CHECK))
C
      IF(BTEST(HDB.DAT(26),2))THEN
        WRITE(11, 6200)
6200    FORMAT(1X,'     Rate Mode: Low')
	VALUE = 1
      ELSE
        WRITE(11, 6210)
6210    FORMAT(1X,'     Rate Mode: High')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(STICS_RATE_MODE))
C
      IF(BTEST(HDB.DAT(221),7))THEN
        WRITE(11, 6230)
6230    FORMAT(1X,'     PHA FIFO Mode: Enabled')
	VALUE = 1
      ELSE
        WRITE(11, 6240)
6240    FORMAT(1X,'     PHA FIFO Mode: Disabled')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(STICS_PHA_FIFO_MODE))
C
      IF(BTEST(STICS_RATE.EDB(0).RATE(179),7))THEN
        WRITE(11, 6250)
6250    FORMAT(1X,'     Data Mode: Diagnostic(TGS)')
	VALUE = 1
      ELSE
        WRITE(11, 6260)
6260    FORMAT(1X,'     Data Mode: Normal')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(STICS_DATA_MODE))
C
      IF(BTEST(HDB.DAT(26),3))THEN
        WRITE(11, 6270)
6270    FORMAT(1X,'     Main SSD Bias: OFF')
	VALUE = 1
      ELSE
        WRITE(11, 6280)
6280    FORMAT(1X,'     Main SSD Bias: ON')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(STICS_MAIN_SSD_BIAS))
C
      IF(BTEST(HDB.DAT(55),2))THEN
        WRITE(11, 6300)
6300    FORMAT(1X,'     Time Preamp "Telescope 1": ON')
	VALUE = 1
      ELSE
        WRITE(11, 6310)
6310    FORMAT(1X,'     Time Preamp "Telescope 1": OFF')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(STICS_TIME_PREAMP_TELE1))
C
      IF(BTEST(HDB.DAT(55),3))THEN
        WRITE(11, 6320)
6320    FORMAT(1X,'     Time Preamp Start 3: ON')
	VALUE = 1
      ELSE
        WRITE(11, 6330)
6330    FORMAT(1X,'     Time Preamp Start 3: OFF')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(STICS_TIME_PREAMP_START3))
C
      IF(BTEST(HDB.DAT(55),4))THEN
        WRITE(11, 6340)
6340    FORMAT(1X,'     Time Preamp Start 4: ON')
	VALUE = 1
      ELSE
        WRITE(11, 6350)
6350    FORMAT(1X,'     Time Preamp Start 4: OFF')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(STICS_TIME_PREAMP_START4))
C
      IF(BTEST(HDB.DAT(55),1))THEN
        WRITE(11, 6360)
6360    FORMAT(1X,'     Time Preamps "Telescope 3": ON')
	VALUE = 1
      ELSE
        WRITE(11, 6370)
6370    FORMAT(1X,'     Time Preamps "Telescope 3": OFF')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(STICS_TIME_PREAMP_TELE3))
C
      IF(BTEST(HDB.DAT(55),0))THEN
        WRITE(11, 6380)
6380    FORMAT(1X,'     Time Preamp Stop 2: ON')
	VALUE = 1
      ELSE
        WRITE(11, 6390)
6390    FORMAT(1X,'     Time Preamp Stop 2: OFF')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(STICS_TIME_PREAMP_STOP2))
C
      IF(BTEST(HDB.DAT(53),0))THEN
        WRITE(11, 6400)
6400    FORMAT(1X,'     Multiple Starts: Enabled')
	VALUE = 1
      ELSE
        WRITE(11, 6410)
6410    FORMAT(1X,'     Multiple Starts: Disabled')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(STICS_MULTIPLE_STARTS))
C
      IF(BTEST(HDB.DAT(53),1))THEN
        WRITE(11, 6420)
6420    FORMAT(1X,'     Multiple Stops: Enabled')
	VALUE = 1
      ELSE
        WRITE(11, 6430)
6430    FORMAT(1X,'     Multiple Stops: Disabled')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(STICS_MULTIPLE_STOPS))
C
      CALL MVBITS(HDB.DAT(25), 1, 3, VALUE, 0)
      CALL FIND_STATS_INT(VALUE,ENTITY(STICS_TRIGGER_MODE))
      IF(VALUE.EQ.0)THEN
        WRITE(11, 6500)
6500    FORMAT(1X,'     Trigger Mode: 0 = En')
      ELSEIF(VALUE.EQ.1)THEN
        WRITE(11, 6510)
6510    FORMAT(1X,'     Trigger Mode: 1 = Tn')
      ELSEIF(VALUE.EQ.2)THEN
        WRITE(11, 6520)
6520    FORMAT(1X,'     Trigger Mode: 2 = (Tn & !En)|(Tn & En)')
      ELSEIF(VALUE.EQ.3)THEN
        WRITE(11, 6530)
6530    FORMAT(1X,'     Trigger Mode: 3 = Tn & En')
      ELSEIF(VALUE.EQ.4)THEN
        WRITE(11, 6540)
6540    FORMAT(1X,'     Trigger Mode: 4 = E|T')
      ELSEIF(VALUE.EQ.5)THEN
        WRITE(11, 6550)
6550    FORMAT(1X,'     Trigger Mode: 5 = (E & !T)|(Tn & !E)|(Tn & En)')
      ELSEIF(VALUE.EQ.6)THEN
        WRITE(11, 6560)
6560    FORMAT(1X,'     Trigger Mode: 6 = Cal & (E|T)')
      ELSEIF(VALUE.EQ.7)THEN
        WRITE(11, 6570)
6570    FORMAT(1X,'     Trigger Mode: 7 = (E & !T)|(Tn & En)')
      ENDIF
C
      IF(BTEST(HDB.DAT(189),0))THEN
        COMP_CODE(1) = 'C'
	VALUE = 1
      ELSE
        COMP_CODE(1) = 'A'
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(STICS_COMP_CODE_GROUP1))
C
      IF(BTEST(HDB.DAT(189),1))THEN
        COMP_CODE(2) = 'C'
	VALUE = 1
      ELSE
        COMP_CODE(2) = 'A'
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(STICS_COMP_CODE_GROUP2))
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
C
      CALL FIND_STATS_INT(IBITS(HDB.DAT(26),4,1),ENTITY(SSD_1_BIAS_IDX))
      CALL FIND_STATS_INT(IBITS(HDB.DAT(26),5,1),ENTITY(SSD_2_BIAS_IDX))
      CALL FIND_STATS_INT(IBITS(HDB.DAT(26),6,1),ENTITY(SSD_3_BIAS_IDX))
      CALL FIND_STATS_INT(IBITS(HDB.DAT(55),5,1),ENTITY(ENERGY_PREAMP_1_IDX))
      CALL FIND_STATS_INT(IBITS(HDB.DAT(55),6,1),ENTITY(ENERGY_PREAMP_2_IDX))
      CALL FIND_STATS_INT(IBITS(HDB.DAT(55),7,1),ENTITY(ENERGY_PREAMP_3_IDX))
      CALL MVBITS(HDB.DAT(51), 2, 6, SSD_1_THRESHOLD, 0)
      CALL MVBITS(HDB.DAT(51), 0, 2, SSD_2_THRESHOLD, 4)
      CALL MVBITS(HDB.DAT(52), 4, 4, SSD_2_THRESHOLD, 0)
      CALL MVBITS(HDB.DAT(52), 0, 4, SSD_3_THRESHOLD, 2)
      CALL MVBITS(HDB.DAT(53), 6, 2, SSD_3_THRESHOLD, 0)
C
      CALL FIND_STATS_INT(SSD_1_THRESHOLD,ENTITY(SSD_1_THRESHOLD_IDX))
      CALL FIND_STATS_INT(SSD_2_THRESHOLD,ENTITY(SSD_2_THRESHOLD_IDX))
      CALL FIND_STATS_INT(SSD_3_THRESHOLD,ENTITY(SSD_3_THRESHOLD_IDX))
C
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
      CALL FIND_STATS_INT(IBITS(HDB.DAT(59),3,1),ENTITY(SWICS_DPPS_ENABLE_IDX))
      CALL FIND_STATS_INT(IBITS(HDB.DAT(58),7,1),ENTITY(SWICS_PAPS_ENABLE_IDX))
      CALL FIND_STATS_INT(IBITS(HDB.DAT(58),4,1),ENTITY(SWICS_START_MCPPS_ENABLE_IDX))
      CALL FIND_STATS_INT(IBITS(HDB.DAT(58),5,1),ENTITY(SWICS_STOP_MCPPS_ENABLE_IDX))
C      
      SWICS_PAPS_MEAS_V        = 4.453E1 - 1.464E2 * ZEXT(HDB.DAT(70))
      SWICS_START_MCPPS_MEAS_V = -4.610  + 1.864E1 * ZEXT(HDB.DAT(77))
      SWICS_STOP_MCPPS_MEAS_V  = 4.650   + 1.356E1 * ZEXT(HDB.DAT(78))
C
      CALL FIND_STATS_INT(ZEXT(HDB.DAT(222)),ENTITY(SWICS_PAPS_LIMIT))
      CALL FIND_STATS_INT(ZEXT(HDB.DAT(61)),ENTITY(SWICS_PAPS_LEVEL))
      CALL FIND_STATS_INT(ZEXT(HDB.DAT(64)),ENTITY(SWICS_START_MCPPS_LIMIT))
      CALL FIND_STATS_INT(ZEXT(HDB.DAT(62)),ENTITY(SWICS_START_MCPPS_LEVEL))
      CALL FIND_STATS_INT(ZEXT(HDB.DAT(65)),ENTITY(SWICS_STOP_MCPPS_LIMIT))
      CALL FIND_STATS_INT(ZEXT(HDB.DAT(63)),ENTITY(SWICS_STOP_MCPPS_LEVEL))
C
C      WRITE(11, 7100) SWICS_DPPS_ENABLE, 
C     &                SWICS_PAPS_ENABLE, ZEXT(HDB.DAT(222)), ZEXT(HDB.DAT(61)), SWICS_PAPS_MEAS_V, 
C     &                SWICS_START_MCPPS_ENABLE, ZEXT(HDB.DAT(64)), ZEXT(HDB.DAT(62)), SWICS_START_MCPPS_MEAS_V, 
C     &                SWICS_STOP_MCPPS_ENABLE, ZEXT(HDB.DAT(65)), ZEXT(HDB.DAT(63)), SWICS_STOP_MCPPS_MEAS_V
      WRITE(11, 7100) SWICS_DPPS_ENABLE, 
     &                SWICS_PAPS_ENABLE, ZEXT(HDB.DAT(222)), ZEXT(HDB.DAT(61)), ENTITY(SWICS_PAPS_OUTV).LAST_VALUE,
     &                SWICS_START_MCPPS_ENABLE, ZEXT(HDB.DAT(64)), ZEXT(HDB.DAT(62)), ENTITY(SWICS_START_MCPPSV_PR).LAST_VALUE, 
     &                SWICS_STOP_MCPPS_ENABLE, ZEXT(HDB.DAT(65)), ZEXT(HDB.DAT(63)), ENTITY(SWICS_STOP_MCPPSV_PR).LAST_VALUE
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
	VALUE = 1
      ELSE
        WRITE(11, 7210)
7210    FORMAT(1X,'     DPPS Stepping Sequence: ROM')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(SWICS_DPPS_STEP_SEQ))
C
      IF(BTEST(HDB.DAT(89),2))THEN
        WRITE(11, 7300)
7300    FORMAT(1X,'     Discharge Indicator: Discharge')
	VALUE = 1
      ELSE
        WRITE(11, 7310)
7310    FORMAT(1X,'     Discharge Indicator: No Discharge')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(SWICS_DISCHARGE_INDIC))
C
      IF(BTEST(HDB.DAT(89),5))THEN
        WRITE(11, 7400)
7400    FORMAT(1X,'     Callibration: Active')
	VALUE = 1
      ELSE
        WRITE(11, 7410)
7410    FORMAT(1X,'     Callibration: Not Active')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(SWICS_CALLIBRATION))
C
      VALUE = ZEXT(HDB.DAT(57))
      CALL FIND_STATS_INT(VALUE,ENTITY(SWICS_ENERGY_CALLIB_LVL))
      WRITE(11, 7500) VALUE
7500  FORMAT(1X,'     Energy Callibrator Level: ',I3) 
C
      VALUE = IAND(HDB.DAT(58), 15)
      CALL FIND_STATS_INT(VALUE,ENTITY(SWICS_TIME_CALLIB_LVL))
      WRITE(11, 7510) VALUE
7510  FORMAT(1X,'     Time Callibrator Level: ',I2) 
C
      IF(BTEST(HDB.DAT(58),6))THEN
        WRITE(11, 7600)
7600    FORMAT(1X,'     Main  SSD Bias: OFF')
	VALUE = 1
      ELSE
        WRITE(11, 7610)
7610    FORMAT(1X,'     Main  SSD Bias: ON ')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(SWICS_MAIN_SSD_BIAS))
C
      DO 7640 I=1,3
        IF(BTEST(HDB.DAT(56),I-1))THEN
	  VALUE = 1
          WRITE(11, 7620) I
7620      FORMAT(1X,'     SSD ',I1,' Preamp and Bias: ON ')
        ELSE
	  VALUE = 0
          WRITE(11, 7630) I
7630      FORMAT(1X,'     SSD ',I1,' Preamp and Bias: OFF')
        ENDIF
	CALL FIND_STATS_INT(VALUE,ENTITY(SWICS_SSD_1_PREAMP_BIAS+I-1))
7640  CONTINUE
C
      IF(BTEST(HDB.DAT(56),3))THEN
        WRITE(11, 7700)
7700    FORMAT(1X,'     Energy Threshold: High')
	VALUE = 1
      ELSE
        WRITE(11, 7710)
7710    FORMAT(1X,'     Energy Threshold: Low')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(SWICS_ENERGY_THRESHOLD))
C
      CALL MVBITS(HDB.DAT(56), 4, 4, TAC_SLOPE, 0)
      IF(TAC_SLOPE.EQ.1)THEN
        WRITE(11, 7800)
7800    FORMAT(1X,'     TAC Slope: Nominal')
	EVALUE = 0
      ELSEIF(TAC_SLOPE.EQ.5)THEN
        WRITE(11, 7810)
7810    FORMAT(1X,'     TAC Slope: -10%')
	EVALUE = -10
      ELSEIF(TAC_SLOPE.EQ.7)THEN
        WRITE(11, 7820)
7820    FORMAT(1X,'     TAC Slope: +10%')
	EVALUE = 10
      ELSEIF(TAC_SLOPE.EQ.9)THEN
        WRITE(11, 7830)
7830    FORMAT(1X,'     TAC Slope: +5%')
	EVALUE = 5
      ELSEIF(TAC_SLOPE.EQ.13)THEN
        WRITE(11, 7840)
7840    FORMAT(1X,'     TAC Slope: -5%')
	EVALUE = -5
      ELSE
        WRITE(11, 7850)
7850    FORMAT(1X,'     TAC Slope: Invalid')
	EVALUE = -999
      ENDIF
      CALL FIND_STATS(TAC_SLOPE,EVALUE,ENTITY(TAC_SLOPE_IDX))
C
      SWICS_TRIGGER_MODE = IAND(HDB.DAT(59), 7)
      CALL FIND_STATS_INT(SWICS_TRIGGER_MODE,ENTITY(SWICS_TRIGGER_MODE_IDX))
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
	VALUE = 1
      ELSE
        WRITE(11, 8010)
8010    FORMAT(1X,'     Command Error Check: OFF')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(SWICS_CMD_ERR_CHECK))
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
      VALUE = ISHFT(ZEXT(HDB.DAT(87)),8) + ZEXT(HDB.DAT(86))
      CALL FIND_STATS_INT(VALUE,ENTITY(SWICS_SUN_SECTOR_DEF_IDX))
C
      VALUE = ZEXT(HDB.DAT(88))
      CALL FIND_STATS_INT(VALUE,ENTITY(SWICS_COMP_FSR_LIMIT))
      WRITE(11, 8200) VALUE
8200  FORMAT(1X,'     Compressed FSR Limit for Step Reversal: ',I3) 
C
      IF(BTEST(HDB.DAT(89),4))THEN
        WRITE(11, 8300)
8300    FORMAT(1X,'     Reversal Stepping Mode: Rate Counting Stopped After Reversal Stepping ')
	VALUE = 1
      ELSE
        WRITE(11, 8310)
8310    FORMAT(1X,'     Reversal Stepping Mode: Rate Counting Continued After Reversal Stepping ')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(SWICS_REVERSAL_STEP_MODE))
C
      IF(BTEST(HDB.DAT(89),6))THEN
        WRITE(11, 8400)
8400    FORMAT(1X,'     PHA FIFO Mode: Enabled')
	VALUE = 1
      ELSE
        WRITE(11, 8410)
8410    FORMAT(1X,'     PHA FIFO Mode: Disabled')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(SWICS_PHA_FIFO_MODE))
C
      IF(BTEST(HDB.DAT(89),7))THEN
        WRITE(11, 8500)
8500    FORMAT(1X,'     Test Tables: Active')
	VALUE = 1
      ELSE
        WRITE(11, 8510)
8510    FORMAT(1X,'     Test Tables: Inactive')
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(SWICS_TEST_TABLES))
C
      IF(BTEST(HDB.DAT(189),2))THEN
        COMP_CODE(1) = 'C'
	VALUE = 1
      ELSE
        COMP_CODE(1) = 'A'
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(SWICS_COMP_CODE_GROUP1))
C
      IF(BTEST(HDB.DAT(189),3))THEN
        COMP_CODE(2) = 'C'
	VALUE = 1
      ELSE
        COMP_CODE(2) = 'A'
	VALUE = 0
      ENDIF
      CALL FIND_STATS_INT(VALUE,ENTITY(SWICS_COMP_CODE_GROUP2))
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
C Close files, send mail message, and STOP
C
      CALL WIND_L1_CLOSE( L1_LUN, STATUS )      ! Close input file
      IF (STATUS .NE. 1) GOTO 9090
      CLOSE(UNIT=11)                            ! Close output file
      CLOSE(UNIT=WH_LUN)			! Close message file 1
      INQUIRE(UNIT=WH_LUN2,NAME=L1FILENAME)	! Get name of message file 2
      CLOSE(UNIT=WH_LUN2)                       ! Close message file 2
C
      NCH = INDEX(L1NAME, ']')			! Isolate L1 filename
      IF (NCH .EQ. 0) NCH = INDEX(L1NAME, ':')
      NCH = NCH + 1
      NCH2 = INDEX(L1NAME, ';')
      IF (NCH .EQ. 0) NCH2 = INDEX(L1FILENAME, ' ')
      IF (NCH .EQ. 0) NCH2 = LEN(L1FILENAME) + 1
      NCH2 = NCH2 - 1
      NCHAR = INDEX(L1FILENAME, ' ') - 1
      IF (NCHAR .EQ. -1) NCHAR = LEN(L1FILENAME)
C
      IF (NEXCEED .GT. 0) THEN
        TYPE *,'MAIL/NOSELF/SUBJ="Problems with '//L1NAME(NCH:NCH2)
     &		     //'" '//L1FILENAME(1:NCHAR)//' '//MAIL_LIST(1:NCHMAIL)
        CALL LIB$SPAWN('MAIL/NOSELF/SUBJ="Problems with '//L1NAME(NCH:NCH2)
     &		     //'" '//L1FILENAME(1:NCHAR)//' '//MAIL_LIST(1:NCHMAIL))
      ENDIF
      CALL LIB$DELETE_FILE(L1FILENAME(1:NCHAR))
      STOP
C
C Come here for errors
C
9020  TYPE *,'ERROR READING SCIENCE RECORD ',CNTR,'; STATUS=',STATUS
      CALL EXIT(STATUS)
9030  TYPE *,'ERROR GETTING BASIC CORE; STATUS=',STATUS
      CALL EXIT(STATUS)
9050  TYPE *,'ERROR GETTING HDB; STATUS=',STATUS
      CALL EXIT(STATUS)
9060  TYPE *,'ERROR GETTING HK DATA; STATUS=',STATUS
      CALL EXIT(STATUS)
9080  TYPE *,'ERROR GETTING STICS DATA; STATUS=',STATUS
      CALL EXIT(STATUS)
9090  TYPE *,'ERROR CLOSING L1 FILE; STATUS=',STATUS
      CALL EXIT(STATUS)
      END
C-------------------------------------------------------------------------------
      SUBROUTINE POLY2(X,C0,C1,C2,C3,C4,C5)
C This function computes a polynomial of up to 5th order in each component
C of the X structure. The polynomials' values are retuned in x also
C-------------------------------------------------------------------------------
      INCLUDE 'INC_DIR:EDBDEF.INC/NOLIST'
      INCLUDE 'INC_DIR:LIMITDEF.INC/NOLIST'
      REAL C0,C1,C2,C3,C4,C5,TEMP
      REAL*4 POLY
      RECORD /HOUSEKEEPING_STATS/ X
C
C Evaluate polynomial of minimum, maximum, and average values of X. Return 0
C for the average if X is negative.
C
      X.MIN = POLY(X.MIN,C0,C1,C2,C3,C4,C5)
      X.MAX = POLY(X.MAX,C0,C1,C2,C3,C4,C5)
      IF(X.AVER.GE.0)THEN
        X.AVER = POLY(X.AVER,C0,C1,C2,C3,C4,C5)
      ELSE
        X.AVER = 0
      ENDIF
C
C The polynomials used for WIND housekeeping are monotonic, so that the
C polynomial evaluated at X.MIN is still an extremum. However, if the
C polynomial is monotonically DECREASING, the minimum and the maximum will
C exchange places. Allow for this below.
C
      IF(X.MIN.GT.X.MAX)THEN
        TEMP = X.MIN
        X.MIN = X.MAX
        X.MAX = TEMP
      ENDIF
C
      RETURN
      END
C-------------------------------------------------------------------------------
      REAL*4 FUNCTION POLY(X,C0,C1,C2,C3,C4,C5)
C This function computes a polynomial of up to 5th order of X.
C-------------------------------------------------------------------------------
      REAL X,C0,C1,C2,C3,C4,C5
C
C Evaluate polynomial of X. 
C
      POLY = C0 + C1*X + C2*X**2 + C3*X**3 + C4*X**4 + C5*X**5
C
      RETURN
      END
C-------------------------------------------------------------------------------
      SUBROUTINE FIND_AVG(STATS)
C-------------------------------------------------------------------------------
C This subroutine computes the average value of the quantity passed in the
C parameter STATS.  The structure member AVERAGE contains the running sum, and
C the structure member COUNT contains the number of items summed into AVERAGE.
C To avoid having zero-valued items skew the average these are counted in
C structure member NZERO and not used in computing the average.
C-------------------------------------------------------------------------------
      INCLUDE 'INC_DIR:EDBDEF.INC/NOLIST'
      INCLUDE 'INC_DIR:LIMITDEF.INC/NOLIST'
      RECORD /HOUSEKEEPING_STATS/ STATS
C
      IF ((STATS.COUNT - STATS.NZERO) .LE. 0) THEN
        STATS.AVERAGE = -999
        STATS.MIN = -999
        STATS.MAX = -999
      ELSE
        STATS.AVERAGE = STATS.AVERAGE / (STATS.COUNT - STATS.NZERO)
      ENDIF
      RETURN
      END
C-------------------------------------------------------------------------------
	subroutine dump_stats(unit,stats)
	include 'inc_dir:edbdef.inc/nolist'
	include 'inc_dir:limitdef.inc/nolist'
	record /housekeeping_stats/ stats
	integer unit
c
	write(unit,1001) stats.IDENT
1001	format(/' identification of quantity',t40,a)
	write(unit,1002) stats.MIN_VALUE
1002	format(' smallest value',t40,f9.2)
	write(unit,1003) stats.MAX_VALUE
1003	format(' largest value',t40,f9.2)
c	write(unit,1004) stats.MIN_TIME
c1004	format(' Epoch time of smallest value',t40,
c	write(unit,1005) stats.MAX_TIME
c1005	format(' Epoch time of largest value',t40,
c	write(unit,1006) stats.MIN_W_TIME
c1006	format(' wall time of smallest value',t40,a)
c	write(unit,1007) stats.MAX_W_TIME
c1007	format(' wall time of largest values',t40,a)
	write(unit,1008) stats.AVERAGE
1008	format(' average value (not including',t40,f9.2)
	write(unit,1009) stats.LAST_VALUE
1009	format(' last valid value',t40,f9.2)
	write(unit,1010) stats.LO_LIMIT
1010	format(' lower limit value',t40,f9.2)
	write(unit,1011) stats.HI_LIMIT
1011	format(' upper limit value',t40,f9.2)
	write(unit,1012) stats.LO_COUNT
1012	format(' # times exceeded LO_LIMIT',t40,i9)
	write(unit,1013) stats.HI_COUNT
1013	format(' # times exceeded HI_LIMIT',t40,i9)
	write(unit,1014) stats.COUNT
1014	format(' # of values',t40,i9)
	write(unit,1015) stats.NZERO
1015	format(' # of zeros',t40,i9)
	write(unit,1016) stats.REPORT
1016	format(' check for out-of-limit values?',t40,l9)
	write(unit,1017) stats.UNITS
1017	format(' units of quantity',t40,a4)
	return
	end
