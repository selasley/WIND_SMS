!+	EDBDEF.INC
!
!	This version:   10-JAN-1995
!
!	This common block represents the elements and the order of elements
!	of the WIND mission for the Swics, Mass, Stics (SMS) experiments as
!	defined in the Data Processing Unit (DPU) Software Users Guide,
!	Version 2.3 dated 17-Sep-93 for the International Solar-Terrestrial
!	Physics (ISTP) Program for three Science Records. Originally only two
!	science records were represented at a time, but some SWICS Matrix
!	Elements are read out two full science records after they are accumulated.
!
!	used by:
!		EPOCH_TO_ITIME
!		RECORD_PC_DATA
!		WIND_CD_INIT
!		WIND_CD_READ
!		WIND_OPEN_L1
!		WIND_SAVE_HK
!		WIND_STORE_L1
!
!	Creation:
!	14-MAR-94	H. Broer	First Version of EDB.INC
!	31-Jan-93	H. Broer	First Version of HK.INC
!
!	Revision:
!	22-Feb-94	H. Broer	enhanced to two HK Blocks + Time
!	26-MAY-94	H. Broer	changed to structured data
!	26-MAY-94	H. Broer	included QUAL flags for CORE,
!					SWICS, STICS, and MASS CORE + PHA
!	 3-JUN-94	H. Broer	include CORE structure
!	10-JUN-94	H. Broer	combined EBD.INC and HK.INC into single
!					file, merged revision histories
!	14-JUN-94	H. Broer	change _Core to _RATE
!	29-JUN-94	H. Broer	more structures
!	 4-AUG-94	J. Paquette	changed dimension of SCI to 3
!	12-SEP-94	L. Bleau	change HKset from scalar to array of 3,
!					also made HKeep record an array of 3
!	13-SEP-94       L. Bleau	modified HKeep structure definition to
!					include field to hold count of HKs
!	21-SEP-94       L. Bleau	added definition of HKeep_old to match
!					older format of HK (no count field);
!					can be removed when V1 Level 1 files
!					are no longer used
!	26-SEP-1994	J. Paquette 	added a quality flag for each type of
!					rates; neccesary because of shifting
!	4-OCT-1994	L.Bleau     	added L1_sci_rec_cntr as output L1
!					science record counter in new COMMON
!					block L1INFO
!       19-OCT-1994     J. Paquette     corrected error in initial value of
!					SWICS_N_RATE_Size
!	24-OCT-1994	L. Bleau	this file was split out from EDB.INC
!					it contains only structure declarations
!        5-DEC-1994     J.Paquette      added TLM_Mode to structure E, changed 
!					length of CORE, renamed old definitions
!					to E_V1 and CORE_V1 
!	13-DEC-1994	L. Bleau	added parameter MAX_HK to give maximum
!					number of HK records in L1 record
!	10-JAN-1995	L. Bleau	removed initialization of record
!					identifiers from structure declarations;
!					added code in WIND_CD_INIT to do this
!					at runtime
!	22-FEB-1995	L. Bleau	changed def of HDBstr to allow
!					accessing its fields w/out using .BDH
!	 2-MAR-1995	L. Bleau	added comments on meaning of bits in
!					Qual flag
!-----------------------------------------------------------------------------

      STRUCTURE /W_RATE/
          CHARACTER*4	SWRA
          BYTE		RATE(0:115)
      END STRUCTURE
      STRUCTURE /SW_RATE/
          UNION
              MAP
                  RECORD /W_RATE/ EDB(0:59)
              END MAP
              MAP
                  CHARACTER*120		C_SW_RATE(0:59)
              END MAP
              MAP
                  CHARACTER*7200	CC_SW_RATE
              END MAP
          END UNION
      END STRUCTURE
      STRUCTURE /W_PHA/
          CHARACTER*4	SWPH
          BYTE		PHA(0:671)
      END STRUCTURE
      STRUCTURE /SW_PHA/
          UNION
              MAP
                  RECORD /W_PHA/ EDB(0:59)
              END MAP
              MAP
                  CHARACTER*676		C_SW_PHA(0:59)
              END MAP
              MAP
                  CHARACTER*40560	CC_SW_PHA
              END MAP
          END UNION
      END STRUCTURE
      STRUCTURE /M_RATE/
          CHARACTER*4	MARA
          BYTE		RATE(0:26),
     :			SPARE
      END STRUCTURE
      STRUCTURE /MA_RATE/
          UNION
              MAP
                  RECORD /M_RATE/ EDB(0:59)
              END MAP
              MAP
                  CHARACTER*32		C_MA_RATE(0:59)
              END MAP
              MAP
                  CHARACTER*1920	CC_MA_RATE
              END MAP
          END UNION
      END STRUCTURE
      STRUCTURE /M_PHA/
          CHARACTER*4	MAPH
          BYTE		PHA(0:761)
      END STRUCTURE
      STRUCTURE /MA_PHA/
          UNION
              MAP
                  RECORD /M_PHA/ EDB(0:59)
              END MAP
              MAP
                  CHARACTER*766		C_MA_PHA(0:59)
              END MAP
              MAP
                  CHARACTER*45960	CC_MA_PHA
              END MAP
          END UNION
      END STRUCTURE
      STRUCTURE /S_RATE/
          CHARACTER*4	STRA
          BYTE		RATE(0:179)
      END STRUCTURE
      STRUCTURE /ST_RATE/
          UNION
              MAP
                  RECORD /S_RATE/ EDB(0:59)
              END MAP
              MAP
                  CHARACTER*184		C_ST_RATE(0:59)
              END MAP
              MAP
                  CHARACTER*11040	CC_ST_RATE
              END MAP
          END UNION
      END STRUCTURE
      STRUCTURE /S_PHA/
          CHARACTER*4	STPH
          BYTE		PHA(0:607)
      END STRUCTURE
      STRUCTURE /ST_PHA/
          UNION
              MAP
                  RECORD /S_PHA/ EDB(0:59)
              END MAP
              MAP
                  CHARACTER*612		C_ST_PHA(0:59)
              END MAP
              MAP
                  CHARACTER*36720	CC_ST_PHA
              END MAP
          END UNION
      END STRUCTURE
      STRUCTURE /T/
          INTEGER*2	YEAR,	DOY
          BYTE		HR,	MIN,	SEC,	CSEC
      END STRUCTURE ! /T/
      STRUCTURE /E/
          RECORD /T/	W_Time,		! Integer structured Wall-Time in centiseconds
     +			SC_Time		! Integer structured S/C-Time in centiseconds
          REAL*8	EPOCH,		! Wall time in milliseconds since
     +			SC_EPOCH	! S/C time in milliseconds since 3-JAN-0001 BC
          INTEGER*4	BYTE_SF,	! # bytes per SubFrame ( 33 or 40 )
     +                  SFperBlock,	! # SubFrames per block ( EDB or HDB )
     +                  MeaSpin,	! Measured spin ( 0 -> 59 )
     +                  SpinCnt,	! Continuous spincount ( 0 -> 255 )
     +                  SWICS_Len,	! # PHA-words
     +                  STICS_Len,	! # PHA-words
     +                  MASS_Len,	! # PHA-words
     +                  SWICS_RATE_Size,! # RATE-words
     +                  STICS_RATE_Size,! # RATE-words
     +                  SpareTime,	! TIME left to handle EDB in 0.256 s.
     +                  TLM_Mode        ! Telemetry Mode Indicator
                                        ! 1 = science mode      - High Bitrate
                                        ! 2 = engineering mode  - High Bitrate?
                                        ! 3 = maneuver mode     - High Bitrate
                                        ! 4 = contingency mode  - High Bitrate
                                        ! 5 = science mode      - Normal Bitrate
                                        ! 6 = engineering mode  - Normal Bitrate?
                                        ! 7 = maneuver mode     - Normal Bitrate
                                        ! 8 = contingency mode  - Normal Bitrate
                                        ! 128 = Transitional mode
                                        ! 256 = Unknown mode
          BYTE		SYNC(2)		! 14h, 6Fh
          LOGICAL*1	BitRate,	! .F. = NORMAL,	.T. = HIGH BIT RATE
     +                  HDB_FLAG,	! .F. = EDB,	.T. = HDB
     +                  RAMCheck,	! .F. = OFF,	.T. = ON
     +                  SWICSPower,	! .F. = OFF,	.T. = ON
     +                  STICSPower,	! .F. = OFF,	.T. = ON
     +                  MASSPower,	! .F. = OFF,	.T. = ON
     +                  ValCmdFl,	! .F. = BAD,	.T. = GOOD
     +                  InvCmdFl,
     +                  CmdErrFl,
     +                  SWICS_HP,	! .F. = OFF,	.T. = ON
     +                  SWICS_CP,	! .F. = OFF,	.T. = ON
     +                  STICS_HP,	! .F. = OFF,	.T. = ON
     +                  STICS_CP,	! .F. = OFF,	.T. = ON
     +                  MASS_CP,	! .F. = OFF,	.T. = ON
     +                  MDisChar,
     +                  DeadTime,
     +                  WDisChar,
     +                  MHStep,
     +                  MSPStep,
     +                  MSTStep,
     +                  WPStep,
     +                  WSPStep,
     +                  WSTStep,
     +                  SSPStep,
     +                  SSTStep,
     +                  Qual,		! 0 = GOOD                              
     +                  CORE_Qual,	! BIT 0 = FRAME SYNC ERROR              
     +                  SW_BR_Qual,	! BIT 1 = FRAME COUNTER ERROR           
     +                  SW_MR_Qual,	! BIT 2 = FILL FRAME                    
     +                  SW_ME1_Qual,	! BIT 3 = MISSING EDB                   
     +                  SW_ME2_Qual,	! BIT 4 = PARTIAL CYCLE (STICS PHA ONLY)	
     +                  SW_ER_Qual,	! BIT 5 = CORRUPTED EDB OR HDB HEADER   
     +                  ST_HMR_Qual,	! 
     +                  ST_SMR_Qual,	!
     +                  ST_BR_Qual,	!
     +                  ST_OMR_Qual,	!
     +                  ST_ER_Qual,	!
     +                  MA_UER_Qual,	!
     +                  MA_SER_Qual,	!
     +                  MA_BR_Qual,	!
     +                  MA_MR_Qual,	!
     +                  SW_P_Qual,	!
     +                  ST_P_Qual,	!
     +                  MA_P_Qual,	!
     +                  Spare           ! 
      END STRUCTURE ! /E/
      STRUCTURE /CORE/
          UNION
              MAP
                  CHARACTER*4	CORE
                  RECORD /E/ EDB(0:59)
              END MAP
              MAP
                  CHARACTER*7384	CCORE
              END MAP
          END UNION
      END STRUCTURE ! /CORE/
      STRUCTURE /E_V1/
          RECORD /T/	W_Time,		! Integer structured Wall-Time in centiseconds
     +			SC_Time		! Integer structured S/C-Time in centiseconds
          REAL*8	EPOCH,		! Wall time in milliseconds since
     +			SC_EPOCH	! S/C time in milliseconds since 3-JAN-0001 BC
          INTEGER*4	BYTE_SF,	! # bytes per SubFrame ( 33 or 40 )
     +                  SFperBlock,	! # SubFrames per block ( EDB or HDB )
     +                  MeaSpin,	! Measured spin ( 0 -> 59 )
     +                  SpinCnt,	! Continuous spincount ( 0 -> 255 )
     +                  SWICS_Len,	! # PHA-words
     +                  STICS_Len,	! # PHA-words
     +                  MASS_Len,	! # PHA-words
     +                  SWICS_RATE_Size,! # RATE-words
     +                  STICS_RATE_Size,! # RATE-words
     +                  SpareTime	! TIME left to handle EDB in 0.256 s.
          BYTE		SYNC(2)		! 14h, 6Fh
          LOGICAL*1	BitRate,	! .F. = NORMAL,	.T. = HIGH BIT RATE
     +                  HDB_FLAG,	! .F. = EDB,	.T. = HDB
     +                  RAMCheck,	! .F. = OFF,	.T. = ON
     +                  SWICSPower,	! .F. = OFF,	.T. = ON
     +                  STICSPower,	! .F. = OFF,	.T. = ON
     +                  MASSPower,	! .F. = OFF,	.T. = ON
     +                  ValCmdFl,	! .F. = BAD,	.T. = GOOD
     +                  InvCmdFl,
     +                  CmdErrFl,
     +                  SWICS_HP,	! .F. = OFF,	.T. = ON
     +                  SWICS_CP,	! .F. = OFF,	.T. = ON
     +                  STICS_HP,	! .F. = OFF,	.T. = ON
     +                  STICS_CP,	! .F. = OFF,	.T. = ON
     +                  MASS_CP,	! .F. = OFF,	.T. = ON
     +                  MDisChar,
     +                  DeadTime,
     +                  WDisChar,
     +                  MHStep,
     +                  MSPStep,
     +                  MSTStep,
     +                  WPStep,
     +                  WSPStep,
     +                  WSTStep,
     +                  SSPStep,
     +                  SSTStep,
     +                  Qual,		! 0 = GOOD
     +                  CORE_Qual,	! BIT 0 = FRAME SYNC ERROR
     +                  SW_BR_Qual,	! BIT 1 = FRAME COUNTER ERROR
     +                  SW_MR_Qual,	! BIT 2 = FILL FRAME
     +                  SW_ME1_Qual,	! BIT 3 = MISSING EDB
     +                  SW_ME2_Qual,	! BIT 4 = PARTIAL CYCLE (STICS PHA ONLY)		
     +                  SW_ER_Qual,	!
     +                  ST_HMR_Qual,	!
     +                  ST_SMR_Qual,	!
     +                  ST_BR_Qual,	!
     +                  ST_OMR_Qual,	!
     +                  ST_ER_Qual,	!
     +                  MA_UER_Qual,	!
     +                  MA_SER_Qual,	!
     +                  MA_BR_Qual,	!
     +                  MA_MR_Qual,	!
     +                  SW_P_Qual,	!
     +                  ST_P_Qual,	!
     +                  MA_P_Qual,	!
     +                  Spare           ! 
      END STRUCTURE ! /E_V1/
      STRUCTURE /CORE_V1/
          UNION
              MAP
                  CHARACTER*4	CORE
                  RECORD /E_V1/ EDB(0:59)
              END MAP
              MAP
                  CHARACTER*7144	CCORE
              END MAP
          END UNION
      END STRUCTURE ! /CORE_V1/
      STRUCTURE /OLD_E/
          RECORD /T/	W_Time,		! Integer structured Wall-Time in centiseconds
     +	  		SC_Time		! Integer structured S/C-Time in centiseconds
          REAL*8	EPOCH,		! Wall time in milliseconds since
     +			SC_EPOCH	! S/C time in milliseconds since 3-JAN-0001 BC
          INTEGER*4	BYTE_SF,	! # bytes per SubFrame ( 33 or 40 )
     +                  SFperBlock,	! # SubFrames per block ( EDB or HDB )
     +                  MeaSpin,	! Measured spin ( 0 -> 59 )
     +                  SpinCnt,	! Continuous spincount ( 0 -> 255 )
     +                  SWICS_Len,	! # PHA-words
     +                  STICS_Len,	! # PHA-words
     +                  MASS_Len,	! # PHA-words
     +                  SWICS_RATE_Size,! # RATE-words
     +                  STICS_RATE_Size,! # RATE-words
     +                  SpareTime	! TIME left to handle EDB in 0.256 s.
          BYTE		SYNC(2)		! 14h, 6Fh
          LOGICAL*1	BitRate,	! .F. = NORMAL,	.T. = HIGH BIT RATE
     +                  HDB_FLAG,	! .F. = EDB,	.T. = HDB
     +                  RAMCheck,	! .F. = OFF,	.T. = ON
     +                  SWICSPower,	! .F. = OFF,	.T. = ON
     +                  STICSPower,	! .F. = OFF,	.T. = ON
     +                  MASSPower,	! .F. = OFF,	.T. = ON
     +                  ValCmdFl,	! .F. = BAD,	.T. = GOOD
     +                  InvCmdFl,
     +                  CmdErrFl,
     +                  SWICS_HP,	! .F. = OFF,	.T. = ON
     +                  SWICS_CP,	! .F. = OFF,	.T. = ON
     +                  STICS_HP,	! .F. = OFF,	.T. = ON
     +                  STICS_CP,	! .F. = OFF,	.T. = ON
     +                  MASS_CP,	! .F. = OFF,	.T. = ON
     +                  MDisChar,
     +                  DeadTime,
     +                  WDisChar,
     +                  MHStep,
     +                  MSPStep,
     +                  MSTStep,
     +                  WPStep,
     +                  WSPStep,
     +                  WSTStep,
     +                  SSPStep,
     +                  SSTStep,
     +                  Qual,		! 0 = GOOD
     +                  CORE_Qual,	! BIT 0 = FRAME SYNC ERROR
     +                  SW_R_Qual,	! BIT 1 = FRAME COUNTER ERROR
     +                  ST_R_Qual,	! BIT 2 = FILL FRAME
     +                  MA_R_Qual,	! BIT 3 = MISSING EDB
     +                  SW_P_Qual,	! BIT 4 = PARTIAL CYCLE (STICS PHA ONLY)	
     +                  ST_P_Qual,	!
     +                  MA_P_Qual,	!
     +                  SPARE
      END STRUCTURE ! /OLD_E/
      STRUCTURE /OLD_CORE/
          UNION
              MAP
                  CHARACTER*4	CORE
                  RECORD /OLD_E/ EDB(0:59)
              END MAP
              MAP
                  CHARACTER*6484	CCORE
              END MAP
          END UNION
      END STRUCTURE ! /OLD_CORE/

!===============================================================================
      STRUCTURE /HDB2/
          CHARACTER*4	CCHDB
          RECORD /T/	W_TIME,
     :			SC_Time
          REAL*8	EPOCH,
     :			SC_EPOCH
          INTEGER*4	QUAL            !INTEGER (rather than BYTE) for alignment
          BYTE		DAT(0:279)
      END STRUCTURE
      STRUCTURE	/HDBstr/
          UNION
              MAP
                  RECORD /HDB2/ BDH
              END MAP
              MAP
                  CHARACTER*4	CCHDB
                  RECORD /T/	W_TIME,
     :			        SC_Time
                  REAL*8	EPOCH,
     :			        SC_EPOCH
                  INTEGER*4	QUAL
                  BYTE		DAT(0:279)
              END MAP
              MAP
                  CHARACTER*324		CHDB
              END MAP
          END UNION
      END STRUCTURE

!	This common block represents the elements and the order of elements
!	of the HouseKeeping data as defined in the Data Format Control Document (DFCD)
!	560-1DFD/0190  CSC/TR-91/6014 dated MARCH 1993 for the International
!	Solar-Terrestrial Physics (ISTP) Program for one Major Frame.
      STRUCTURE /HOUSE_KEEP/
          BYTE	BVAL,			! BYTE VALUE
     +		QUAL			! QUALITY FLAG
      END STRUCTURE
      STRUCTURE /HKP/
          RECORD /T/	W_TIME,
     +			SC_TIME
          REAL*8	EPOCH,
     +			SC_EPOCH
          RECORD /HOUSE_KEEP/ HK(75)	! HK VALUE + QUAL
      END STRUCTURE	! KHP
      INTEGER MAX_HK
      PARAMETER (MAX_HK = 6)
      STRUCTURE	/HKeep/
          UNION
              MAP
                  CHARACTER*4	CCHK
C SPARE is included to keep byte count of this structure a multiple of 4
		  INTEGER*2	HKcount
		  INTEGER*2	SPARE
                  RECORD /HKP/ HKSET(MAX_HK)
              END MAP
              MAP
                  CHARACTER*1100 CHK
              END MAP
          END UNION
      END STRUCTURE
      STRUCTURE	/HKeep_v02/
          UNION
              MAP
                  CHARACTER*4	CCHK
C SPARE is included to keep byte count of this structure a multiple of 4
		  INTEGER*2	HKcount
		  INTEGER*2	SPARE
                  RECORD /HKP/ HKSET(5)
              END MAP
              MAP
                  CHARACTER*918	CHK
              END MAP
          END UNION
      END STRUCTURE
      STRUCTURE	/HKeep_old/
          UNION
              MAP
                  CHARACTER*4	CCHK
                  RECORD /HKP/ HKSET(5)
              END MAP
              MAP
                  CHARACTER*914	CHK
              END MAP
          END UNION
      END STRUCTURE
