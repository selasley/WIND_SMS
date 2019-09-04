!+	L0_Data_Record.INC
!
!	This version	 25-MAY-1994
!
!	This common block represents the elements and the order of elements
!	of the Data_Record as defined in the Data Format Control Document (DFCD)
!	560-1DFD/0190  CSC/TR-91/6014 dated MARCH 1993 for the International
!	Solar-Terrestrial Physics (ISTP) Program for one Major Frame.
!
!	Creation:
!	  4-Feb-94	H. Broer	First Version
!
!	Revision:
!	 15-Mar-94	H.Broer		added DR_SC_EPOCH
!	 25-MAY-94			added Pref_EPOCH
!
!-----------------------------------------------------------------------------

      COMMON /Data_Record/	! NASA CDHF LEVEL 0 DATA RECORD FORMAT
     +		DR_Instr_Num,		! Instrument Number
     +		DR_Phys_Rec_Num,	! Physical Record Number within File
     +		DR_Beg_MajFr_Cnt,	! Major Frame Count-Beginning of Maj.Fra
     +		DR_Beg_SC_Clock,	! Spacecraft Clock -Beginning of Maj.Fra
     +		DR_Beg_Year,		! ATC: year-Beginning of Major Frame
     +		DR_Beg_DOY,		! ATC: doy -Beginning of Major Frame
     +		DR_Beg_MSec,		! ATC: msec-Beginning of Major Frame
     +		DR_Beg_MicSec,		! ATC: usec-Beginning of Major Frame
     +		DR_Num_MinFr_Fill,	! Number of Minor Frames with Fill
     +		DR_Num_MinFr_Sync_Err,	! Number of Minor Frames with Sync_Err
     +		DR_TLM_Mode,		! Telemetry Mode Indicator
						! 1 = science mode
						! 2 = engineering mode
						! 3 = maneuver mode
						! 4 = contingency mode
     +		DR_MinFr_Qual,		! Minor Frame Quality
						! bit 0 = frame sync error flag
						! bit 1 = frame counter error fllag
						! bit 2 = fill frame flag
						! bit 3-31 spares
     +		DR_Fill,		! adjustment to Longword
     +		DR_MinFr,		! Instrument data array as extracted
					! 	from each Minor Frame
     +		DR_Beg_EPOCH,		! EPOCH time-Beginning of Major Frame ?
     +		DR_SC_EPOCH,		! EPOCH time-ditto  based on S/C clock
     +		Pref_EPOCH,		! Preferred EPOCH either one of above
     +		L0_FILE_REC_pntr,	! FILE POINTER
     +		L0_FILE_READONLY	! FILE CHARACTERISTIC
      REAL*8		DR_Beg_EPOCH,	! EPIC-added time <= Beg_Year,_DOY,_Msec
     +			DR_SC_EPOCH,	! ditto - based on S/C clock
     +			Pref_EPOCH	! the preferred one of above
      INTEGER*4		DR_Instr_Num,
     +			DR_Phys_Rec_Num,
     +			DR_Beg_MajFr_Cnt,
     +			DR_Beg_SC_Clock(2),
     +			DR_Beg_Year,
     +			DR_Beg_DOY,
     +			DR_Beg_MSec,
     +			DR_Beg_MicSec,			! Always zero
     +			DR_Num_MinFr_Fill,
     +			DR_Num_MinFr_Sync_Err,
     +			DR_TLM_Mode,
     +			L0_FILE_REC_pntr
      BYTE		DR_MinFr_Qual(0:249),
     +			DR_Fill(2),
     +			DR_MinFr(12500),		! Max.record size
     +			DR_MinFr_WAV( 45, 0:249 ),
     +			DR_MinFr_EPA( 24, 0:249 ),
     +			DR_MinFr_MFI( 25, 0:249 ),
     +			DR_MinFr_SWE( 45, 0:249 ),
     +			DR_MinFr_SMS( 42, 0:249 ),
     +			DR_MinFr_3DP( 50, 0:249 ),
     +			DR_MinFr_TGR( 21, 0:249 ),
     +			DR_MinFr_KON(  6, 0:249 )
      EQUIVALENCE	(DR_MinFr,	DR_MinFr_WAV,	DR_MinFr_EPA,
     +			DR_MinFr_MFI,	DR_MinFr_SWE,	DR_MinFr_SMS,
     +			DR_MinFr_3DP,	DR_MinFr_TGR,	DR_MinFr_KON )
      LOGICAL*1         L0_FILE_READONLY

      COMMON /Data_Record2/	! NASA CDHF LEVEL 0 DATA RECORD FORMAT
     +		DR2_Instr_Num,		! Instrument Number
     +		DR2_Phys_Rec_Num,	! Physical Record Number within File
     +		DR2_Beg_MajFr_Cnt,	! Major Frame Count-Beginning of Maj.Fra
     +		DR2_Beg_SC_Clock,	! Spacecraft Clock -Beginning of Maj.Fra
     +		DR2_Beg_Year,		! ATC: year-Beginning of Major Frame
     +		DR2_Beg_DOY,		! ATC: doy -Beginning of Major Frame
     +		DR2_Beg_MSec,		! ATC: msec-Beginning of Major Frame
     +		DR2_Beg_MicSec,		! ATC: usec-Beginning of Major Frame
     +		DR2_Num_MinFr_Fill,	! Number of Minor Frames with Fill
     +		DR2_Num_MinFr_Sync_Err,	! Number of Minor Frames with Sync_Err
     +		DR2_TLM_Mode,		! Telemetry Mode Indicator
						! 1 = science mode
						! 2 = engineering mode
						! 3 = maneuver mode
						! 4 = contingency mode
     +		DR2_MinFr_Qual,		! Minor Frame Quality
						! bit 0 = frame sync error flag
						! bit 1 = frame counter error fllag
						! bit 2 = fill frame flag
						! bit 3-31 spares
     +		DR2_Fill,		! adjustment to Longword
     +		DR2_MinFr,		! Instrument data array as extracted
					! 	from each Minor Frame
     +		DR2_Beg_EPOCH,		! EPOCH time-Beginning of Major Frame ?
     +		DR2_SC_EPOCH		! EPOCH time-ditto  based on S/C clock
      REAL*8		DR2_Beg_EPOCH,	! EPIC-added time <= Beg_Year,_DOY,_Msec
     +			DR2_SC_EPOCH	! ditto - based on S/C clock
      INTEGER*4		DR2_Instr_Num,
     +			DR2_Phys_Rec_Num,
     +			DR2_Beg_MajFr_Cnt,
     +			DR2_Beg_SC_Clock(2),
     +			DR2_Beg_Year,
     +			DR2_Beg_DOY,
     +			DR2_Beg_MSec,
     +			DR2_Beg_MicSec,			! Always zero
     +			DR2_Num_MinFr_Fill,
     +			DR2_Num_MinFr_Sync_Err,
     +			DR2_TLM_Mode
      BYTE		DR2_MinFr_Qual(0:249),
     +			DR2_Fill(2),
     +			DR2_MinFr(12500),		! Max.record size
     +			DR2_MinFr_WAV( 45, 0:249 ),
     +			DR2_MinFr_EPA( 24, 0:249 ),
     +			DR2_MinFr_MFI( 25, 0:249 ),
     +			DR2_MinFr_SWE( 45, 0:249 ),
     +			DR2_MinFr_SMS( 42, 0:249 ),
     +			DR2_MinFr_3DP( 50, 0:249 ),
     +			DR2_MinFr_TGR( 21, 0:249 ),
     +			DR2_MinFr_KON(  6, 0:249 )
      EQUIVALENCE	(DR2_MinFr,	DR2_MinFr_WAV,	DR2_MinFr_EPA,
     +			DR2_MinFr_MFI,	DR2_MinFr_SWE,	DR2_MinFr_SMS,
     +			DR2_MinFr_3DP,	DR2_MinFr_TGR,	DR2_MinFr_KON )

      ! Overlay DR_Buffer array and its common block of individual items
      BYTE		DR_Buffer(13300),
     +			DR2_Buffer(13300),
     +			DR_Buffer_Contingency(13300),
     +			DR2_Buffer_Contingency(13300)
      CHARACTER*13300	C_DR_Buffer,
     +			C_DR2_Buffer,
     +			C_NULL_Buffer
      BYTE		NULL_Buffer(13300)/13300*0/
 
      EQUIVALENCE	(DR_Buffer, DR_Instr_Num, DR_Buffer_Contingency,
     +			 C_DR_Buffer),
     +			(DR2_Buffer, DR2_Instr_Num, DR2_Buffer_Contingency,
     +			 C_DR2_Buffer),
     +			(C_NULL_Buffer, NULL_Buffer)
