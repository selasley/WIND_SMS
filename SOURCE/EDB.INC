!+	EDB.INC
!
!	This version	29-JUN-1994
!
!	This common block represents the elements and the order of elements
!	of the WIND mission for the Swics, Mass, Stics (SMS) experiments as
!	defined in the Data Processing Unit (DPU) Software Users Guide,
!	Version 2.3 dated 17-Sep-93 for the International Solar-Terrestrial
!	Physics (ISTP) Program for two Science Records.
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
!	 14-MAR-94	H. Broer	First Version
!	Revision:
!	 26-MAY-94	H. BROER	included QUAL flags for CORE,
!					SWICS, STICS, and MASS CORE + PHA
!	  3-JUN-94			include CORE structure
!	 10-JUN-94			combined EBD.inc and HK.inc
!	 14-JUN-94			change _Core to _RATE
!	 29-JUN-94			more structures
!-----------------------------------------------------------------------------
      COMMON /EDB/		! WIND mission - SMS Experiments
     +		SCI,
     +		CORE_data,		! Common data for all instruments
     +		SWICS_RATE,		! SWICS RATE Data, avail. when SWICS on
     +		SWICS_PHA,		! SWICS PHA words, as indicated in CORE
     +		STICS_RATE,		! STICS RATE Data, avail. when STICS on
     +		STICS_PHA,		! STICS PHA words, as indicated in CORE
     +		MASS_RATE,		! MASS  RATE Data, avail. when MASS  on
     +		MASS_PHA,		! MASS  PHA words, as indicated in CORE
     +		EDBset			! indicator for current EDB set
      INTEGER*4	EDBset,			! indicator for current EDB set
     +		EDB_Core_Size/10/,	! MAX. Index
     +		MASS_RATE_Size/26/,	! MAX. Index
     +		STICS_RATE_Size,	! MAX. Index
     +		STICS_H_RATE_Size/179/,	! MAX. Index
     +		STICS_N_RATE_Size/89/,	! MAX. Index
     +		SWICS_RATE_Size,	! MAX. Index
     +		SWICS_H_RATE_Size/115/,	! MAX. Index
     +		SWICS_N_RATE_Size/62/	! MAX. Index
      BYTE	CORE_data( 0:10, 0:59, 2 ) ! RAW EDB DATA

      STRUCTURE /SW_RATE/
          UNION
              MAP
                  STRUCTURE /W_RATE/ EDB(0:59)
                      CHARACTER*4	SWRA/'SWRA'/
                      BYTE		RATE(0:115)
                  END STRUCTURE
              END MAP
              MAP
                  CHARACTER*120		C_SW_RATE(0:59)
              END MAP
              MAP
                  CHARACTER*7200	CC_SW_RATE
              END MAP
          END UNION
      END STRUCTURE
      RECORD /SW_RATE/	SWICS_RATE(2)
      STRUCTURE /SW_PHA/
          UNION
              MAP
                  STRUCTURE /W_PHA/ EDB(0:59)
                      CHARACTER*4	SWPH/'SWPH'/
                      BYTE		PHA(0:671)
                  END STRUCTURE
              END MAP
              MAP
                  CHARACTER*676		C_SW_PHA(0:59)
              END MAP
              MAP
                  CHARACTER*40560	CC_SW_PHA
              END MAP
          END UNION
      END STRUCTURE
      RECORD /SW_PHA/	SWICS_PHA(2)

      STRUCTURE /MA_RATE/
          UNION
              MAP
                  STRUCTURE /M_RATE/ EDB(0:59)
                      CHARACTER*4	MARA/'MARA'/
                      BYTE		RATE(0:26),
     :					SPARE
                  END STRUCTURE
              END MAP
              MAP
                  CHARACTER*32		C_MA_RATE(0:59)
              END MAP
              MAP
                  CHARACTER*1920	CC_MA_RATE
              END MAP
          END UNION
      END STRUCTURE
      RECORD /MA_RATE/	MASS_RATE(2)
      STRUCTURE /MA_PHA/
          UNION
              MAP
                  STRUCTURE /M_PHA/ EDB(0:59)
                      CHARACTER*4	MAPH/'MAPH'/
                      BYTE		PHA(0:761)
                  END STRUCTURE
              END MAP
              MAP
                  CHARACTER*766		C_MA_PHA(0:59)
              END MAP
              MAP
                  CHARACTER*45960	CC_MA_PHA
              END MAP
          END UNION
      END STRUCTURE
      RECORD /MA_PHA/	MASS_PHA(2)

      STRUCTURE /ST_RATE/
          UNION
              MAP
                  STRUCTURE /S_RATE/ EDB(0:59)
                      CHARACTER*4	STRA/'STRA'/
                      BYTE		RATE(0:179)
                  END STRUCTURE
              END MAP
              MAP
                  CHARACTER*184		C_ST_RATE(0:59)
              END MAP
              MAP
                  CHARACTER*11040	CC_ST_RATE
              END MAP
          END UNION
      END STRUCTURE
      RECORD /ST_RATE/	STICS_RATE(2)
      STRUCTURE /ST_PHA/
          UNION
              MAP
                  STRUCTURE /S_PHA/ EDB(0:59)
                      CHARACTER*4	STPH/'STPH'/
                      BYTE		PHA(0:607)
                  END STRUCTURE
              END MAP
              MAP
                  CHARACTER*612		C_ST_PHA(0:59)
              END MAP
              MAP
                  CHARACTER*36720	CC_ST_PHA
              END MAP
          END UNION
      END STRUCTURE
      RECORD /ST_PHA/	STICS_PHA(2)
      STRUCTURE /T/
          INTEGER*2	YEAR,	DOY
          BYTE		HR,	MIN,	SEC,	CSEC
      END STRUCTURE ! /T/

      STRUCTURE /CORE/
          UNION
              MAP
                  CHARACTER*4	CORE/'CORE'/
                  STRUCTURE /E/ EDB(0:59)
                      RECORD /T/ W_Time,	! Integer structured Wall-Time in centiseconds
     +				SC_Time		! Integer structured S/C-Time in centiseconds
                      REAL*8	EPOCH,		! Wall time in milliseconds since
     +				SC_EPOCH	! S/C time in milliseconds since 3-JAN-0001 BC
                      INTEGER*4	BYTE_SF,	! # bytes per SubFrame ( 33 or 40 )
     +                          SFperBlock,	! # SubFrames per block ( EDB or HDB )
     +                          MeaSpin,	! Measured spin ( 0 -> 59 )
     +                          SpinCnt,	! Continuous spincount ( 0 -> 255 )
     +                          SWICS_Len,	! # PHA-words
     +                          STICS_Len,	! # PHA-words
     +                          MASS_Len,	! # PHA-words
     +                          SWICS_RATE_Size,! # RATE-words
     +                          STICS_RATE_Size,! # RATE-words
     +                          SpareTime	! TIME left to handle EDB in 0.256 s.
                      BYTE	SYNC(2)		! 14h, 6Fh
                      LOGICAL*1	BitRate,	! .F. = NORMAL,	.T. = HIGH BIT RATE
     +                          HDB_FLAG,	! .F. = EDB,	.T. = HDB
     +                          RAMCheck,	! .F. = OFF,	.T. = ON
     +                          SWICSPower,	! .F. = OFF,	.T. = ON
     +                          STICSPower,	! .F. = OFF,	.T. = ON
     +                          MASSPower,	! .F. = OFF,	.T. = ON
     +                          ValCmdFl,	! .F. = BAD,	.T. = GOOD
     +                          InvCmdFl,
     +                          CmdErrFl,
     +                          SWICS_HP,	! .F. = OFF,	.T. = ON
     +                          SWICS_CP,	! .F. = OFF,	.T. = ON
     +                          STICS_HP,	! .F. = OFF,	.T. = ON
     +                          STICS_CP,	! .F. = OFF,	.T. = ON
     +                          MASS_CP,	! .F. = OFF,	.T. = ON
     +                          MDisChar,
     +                          DeadTime,
     +                          WDisChar,
     +                          MHStep,
     +                          MSPStep,
     +                          MSTStep,
     +                          WPStep,
     +                          WSPStep,
     +                          WSTStep,
     +                          SSPStep,
     +                          SSTStep,
     +                          Qual,		! 0 = GOOD
     +                          CORE_Qual,	! BIT 0 = FRANE SYNC ERROR
     +                          SW_R_Qual,	! BIT 1 = FRAME COUNTER ERROR
     +                          ST_R_Qual,	! BIT 2 = FILL FRAME
     +                          MA_R_Qual,	! BIT 3 = MISSING EDB
     +                          SW_P_Qual,	!
     +                          ST_P_Qual,	!
     +                          MA_P_Qual,	!
     +                          SPARE
                  END STRUCTURE ! /E/
              END MAP
              MAP
                  CHARACTER*6484	CCORE
              END MAP
          END UNION
      END STRUCTURE ! /CORE/
      RECORD /CORE/	SCI(2)

      COMMON /EDB_HEADER/
     +		SFperBlock,		! Number of subframes per
					!	 EDB ( 5 < NUM_Frame < 21 )
     +		SUBS_ID,		! 0=33 per frame, 1=40 per frame
     +		HDBFlag,		! 0=EDB, 1=HDB
     +		BITRATE,		! 0=low bitrate,  1=high bitrate
     +		SWICSPower,		! .T. = ON,	.F. = OFF
     +		MASSPower,		! .T. = ON,	.F. = OFF
     +		STICSPower,		! .T. = ON,	.F. = OFF
     +		RAMCheck,		! .T. = ON,	.F. = OFF
     *		SparFlags,		! adjust to Longword boundery
     +		MeasSpin,		! Measured spin counter ( 0 -> 59 )
     +		SpinCount		! Spin counter
      INTEGER*4		MeasSpin,	! Measured spin counter ( 0 -> 59 )
     +			SFperBlock,	! Number of Subframes per EDB
     +			SpinCount	! Continuous spin counter ( 0 -> 255 )
      LOGICAL*1		HDBFlag,	! .F. = EDB,	.T. = HDB
     +			BITRATE,	! .F. = low,	.T. = high
     +			SUBS_ID,	! .F. = 33 per frame, .T.= 40 per frame
     +			SWICSPower,	! .T. = ON,	.F. = OFF
     +			STICSPower,	! .T. = ON,	.F. = OFF
     +			MASSPower,	! .T. = ON,	.F. = OFF
     +			RAMCheck,	! .T. = in RAM check mode
     *			SparFlags(1)
!===============================================================================
      COMMON /HDB/			! WIND mission - SMS Experiments
     +			HDB,
     +			HDBset
      INTEGER*4		HDBset		! Pointer to current set
      STRUCTURE	/HDBstr/
          UNION
              MAP
                  STRUCTURE /HDB2/ BDH
                      CHARACTER*4	CCHDB/'HDB '/
                      RECORD /T/	W_TIME,
     :					SC_Time
                      REAL*8		EPOCH,
     :					SC_EPOCH
                      INTEGER*4		QUAL
                      BYTE		DAT(0:279)
                  END STRUCTURE
              END MAP
              MAP
                  CHARACTER*324		CHDB
              END MAP
          END UNION
      END STRUCTURE
      RECORD /HDBstr/	HDB(2)

!+	HK.INC
!
!	This version	 9-JUN-1994
!
!	This common block represents the elements and the order of elements
!	of the HouseKeeping data as defined in the Data Format Control Document (DFCD)
!	560-1DFD/0190  CSC/TR-91/6014 dated MARCH 1993 for the International
!	Solar-Terrestrial Physics (ISTP) Program for one Major Frame.
!
!	Creation:
!	31-Jan-93	H. Broer	First Version
!	22-Feb-94			enhanced to two HK Blocks + Time
!	26-MAY-94			changed to structured data
!	 9-JUN-94			combined with EDB.inc
!-----------------------------------------------------------------------------
      COMMON /HK/	! NASA CDHF LEVEL 0 DATA RECORD FORMAT
     +		HKeep,			! Structure of EPOCH,data,QUAL
     +		HKset,			! INDICATOR of current set used
     +		HKLOCI,			! POSITION within abbr. MinFr 
     +		HKLOCJ			! MinFr. indicator
      INTEGER*4	HKset,			
     +		HKLOCI(75),		!
     +		HKLOCJ(75)		!
      STRUCTURE /HOUSE_KEEP/
          BYTE	BVAL,			! BYTE VALUE
     +		QUAL			! QUALITY FLAG
      END STRUCTURE
      STRUCTURE	/HKeep/
          UNION
              MAP
                  CHARACTER*4	CCHK/'HK  '/
                  STRUCTURE /HKP/ HKSET(5)
                      RECORD /T/	W_TIME,
     +					SC_TIME
                      REAL*8		EPOCH,
     +					SC_EPOCH
                      RECORD /HOUSE_KEEP/ HK(75)	! HK VALUE + QUAL
                  END STRUCTURE
              END MAP
              MAP
                  CHARACTER*914	CHK
              END MAP
          END UNION
      END STRUCTURE
      RECORD	/HKeep/	HKeep
      data	HKLOCI/	1, 2, 2, 1, 2,			! Byte position numbers for SMS
     +			2, 1, 2, 2, 1,	 2, 2, 1, 2, 2,
     +			1, 2, 2, 1, 2,	 2, 1, 2, 2, 1,
     +			2, 2, 1, 2, 2,	 1, 2, 2, 1, 2,
     +			2, 1, 2, 2, 1,	 2, 2, 1, 2, 2,
     +			1, 2, 2, 1, 2,	 2, 1, 2, 2, 1,
     +			2, 2, 1, 2, 2,	 1, 2, 2, 1, 2,
     +			2, 1, 2, 2, 1,	 2, 2, 1, 2, 2 /
      data	HKLOCJ/   1,   3,   7,  11,  13,	! Minor frame numbers for SMS
     +			 17,  21,  23,  27,  31,     33,  37,  41,  43,  47,
     +			 51,  53,  57,  61,  63,     67,  71,  73,  77,  81,
     +			 83,  87,  91,  93,  97,    101, 103, 107, 111, 113,
     +			117, 121, 123, 127, 131,    133, 137, 141, 143, 147,
     +			151, 153, 157, 161, 163,    167, 171, 173, 177, 181,
     +			183, 187, 191, 193, 197,    201, 203, 207, 211, 213,
     +			217, 221, 223, 227, 231,    233, 237, 241, 243, 247 /
