!+	EDB.INC
!
!	This version	  4-OCT-1994
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
!	 4-OCT-1994	L. Bleau     	added L1_sci_rec_cntr as output L1
!					science record counter in new COMMON
!					block L1INFO
!       19-OCT-1994     J. Paquette     corrected error in initial value of
!					SWICS_N_RATE_Size
!	24-OCT-1994	L. Bleau	this file was split into two files:
!					EDBDEF.INC and EDBVARS.INC
!-----------------------------------------------------------------------------
      INCLUDE 'INC_DIR:EDBDEF.INC/NOLIST'
      INCLUDE 'INC_DIR:EDBVARS.INC/NOLIST'
