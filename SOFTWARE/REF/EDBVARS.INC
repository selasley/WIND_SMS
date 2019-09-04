C+	EDBVARS.INC
C
C	This version:   10-FEB-1995
C
C	This common block represents the elements and the order of elements
C	of the WIND mission for the Swics, Mass, Stics (SMS) experiments as
C	defined in the Data Processing Unit (DPU) Software Users Guide,
C	Version 2.3 dated 17-Sep-93 for the International Solar-Terrestrial
C	Physics (ISTP) Program for three Science Records. Originally only two
C	science records were represented at a time, but some SWICS Matrix
C	Elements are read out two full science records after they are accumulated.
C
C	used by:
C		EPOCH_TO_ITIME
C		RECORD_PC_DATA
C		WIND_CD_INIT
C		WIND_CD_READ
C		WIND_OPEN_L1
C		WIND_SAVE_HK
C		WIND_STORE_L1
C
C	Creation:
C	14-MAR-94	H. Broer	First Version of EDB.INC
C	31-Jan-93	H. Broer	First Version of HK.INC
C
C	Revision:
C	22-Feb-94	H. Broer	enhanced to two HK Blocks + Time
C	26-MAY-94	H. Broer	changed to structured data
C	26-MAY-94	H. Broer	included QUAL flags for CORE,
C					SWICS, STICS, and MASS CORE + PHA
C	 3-JUN-94	H. Broer	include CORE structure
C	10-JUN-94	H. Broer	combined EBD.INC and HK.INC into single
C					file, merged revision histories
C	14-JUN-94	H. Broer	change _Core to _RATE
C	29-JUN-94	H. Broer	more structures
C	 4-AUG-94	J. Paquette	changed dimension of SCI to 3
C	12-SEP-94	L. Bleau	change HKset from scalar to array of 3,
C					also made HKeep record an array of 3
C	13-SEP-94       L. Bleau	modified HKeep structure definition to
C					include field to hold count of HKs
C	21-SEP-94       L. Bleau	added definition of HKeep_old to match
C					older format of HK (no count field);
C					can be removed when V1 Level 1 files
C					are no longer used
C	26-SEP-1994	J. Paquette 	added a quality flag for each type of
C					rates; neccesary because of shifting
C	4-OCT-1994	L.Bleau     	added L1_sci_rec_cntr as output L1
C					science record counter in new COMMON
C					block L1INFO
C       19-OCT-1994     J. Paquette     corrected error in initial value of
C					SWICS_N_RATE_Size
C	24-OCT-1994	L. Bleau	this file was split out from EDB.INC
C					it contains only variable, common, and
C					record declarations
C       27-OCT-1994     L. Bleau        changed name L1_sci_rec_cntr to sci_rec_cntr
C       27-OCT-1994     L. Bleau        added L1_recs_written
C       15-DEC-1994     L. Bleau        added EDB_sum, EDB_lost, and EDB_skip
C	13-JAN-1995     L. Bleau	add HDBtime2 to common block HDB
C	10-FEB-1995	L. Bleau	added new vars Curr_Sci_Rec_Number and
C					Curr_Spin to common block L1INFO;
C					changed ! to C in comments
C	14-FEB-1995	L. Bleau	added New_Sci_Rec to common block L1INFO
C	15-FEB-1995	L. Bleau	removed HDBset as HDBs are now indexed
C			J.PAQUETTE	by EDBset-1
C-----------------------------------------------------------------------------

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
     +		SWICS_N_RATE_Size/61/	! MAX. Index
      INTEGER*4	Sci_rec_cntr,		! Counter of sci recs before shifting
     +          L1_recs_written,        ! Output L1 science record counter
     +          EDB_Lost,               ! EDBs lost during run
     +          EDB_Skip,               ! EDBs skipped due to command
     +          EDB_Sum,                ! Total number of EDBs handled
     +          Curr_Sci_Rec_Number,    ! Current science record number
     +          Curr_Spin               ! Current spin being processed
      LOGICAL*4 New_Sci_Rec             ! Set if just finished sci rec, starting
					! new one; clear otherwise
      COMMON /L1INFO/
     +		Sci_rec_cntr,		! Counter of sci recs before shifting
     +          L1_recs_written,        ! Output L1 science record counter
     +          EDB_Lost,               ! EDBs lost during run
     +          EDB_Skip,               ! EDBs skipped due to command
     +          EDB_Sum,                ! Total number of EDBs handled
     +          Curr_Sci_Rec_Number,    ! Current science record number
     +          Curr_Spin,              ! Current spin being processed
     +		New_Sci_Rec             ! Set if just finished sci rec, starting
					! new one; clear otherwise
      BYTE	CORE_data( 0:10, 0:59, 3 ) ! RAW EDB DATA
      RECORD /SW_RATE/	SWICS_RATE(3)
      RECORD /SW_PHA/	SWICS_PHA(3)
      RECORD /MA_RATE/	MASS_RATE(3)
      RECORD /MA_PHA/	MASS_PHA(3)
      RECORD /ST_RATE/	STICS_RATE(3)
      RECORD /ST_PHA/	STICS_PHA(3)
      RECORD /CORE/	SCI(3)
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
C===============================================================================
      COMMON /HDB/			! WIND mission - SMS Experiments
     +			HDB,
     +                  HDBTime2
      INTEGER*4 HDBTime2(2,3)           ! Time contained in HDB-Header
      DATA HDBTime2 /6 * 0/
      RECORD /HDBstr/	HDB(3)
C	This common block represents the elements and the order of elements
C	of the HouseKeeping data as defined in the Data Format Control Document
C	(DFCD) 560-1DFD/0190  CSC/TR-91/6014 dated MARCH 1993 for the
C	International Solar-Terrestrial Physics (ISTP) Program for one Major
C	Frame. 
      COMMON /HK/	! NASA CDHF LEVEL 0 DATA RECORD FORMAT
     +		HKeep,			! Structure of EPOCH,data,QUAL
     +		HKset(3),		! INDICATOR of current set used
     +		HKLOCI,			! POSITION within abbr. MinFr 
     +		HKLOCJ			! MinFr. indicator
      INTEGER*4	HKset,			
     +		HKLOCI(75),		!
     +		HKLOCJ(75)		!
      RECORD	/HKeep/	HKeep(3)
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
