!+	L0_Labl_Record.INC
!
!	This version:	9-DEC-1994
!
!	This common block represents the elements and the order of elements
!	of the Data_Record_Header (DHR) as defined in the Data Format Control
!	Document (DFCD) 560-1DFD/0190  CSC/TR-91/6014 dated MARCH 1993 for the
!	International Solar-Terrestrial Physics (ISTP) Program for one Major
!	Frame.
!
!	Creation:
!	 25-Jan-94	H. Broer	First Version
!
!	Revision:
!	 9-DEC-1994	L. Bleau	moved LR_Beg_EPOCH and LR_End_EPOCH to
!					common block LR_EPOCH_Times to keep
!					aligned
!
!-----------------------------------------------------------------------------
      COMMON /Label_Record/	! NASA CDHF LEVEL 0 LABEL RECORDFORMAT
     +		LR_SCID,		! Spacecraft ID
     +		LR_Instr_Num,		! Instrument Number
     +		LR_Instr_Name,		! Instrument Name
     +		LR_Phys_Rec_Num,	! Physical Record Number
     +		LR_Phys_Rec_per_MajFr,	! Physical Records Per Major Frame
     +		LR_Phys_Rec_in_File,	! Number of Physical Records in File
     +		LR_Beg_MajFr_Cnt,	! Major Frame Count - First Major Frame
     +		LR_End_MajFr_Cnt,	! Major Frame Count _ Last Major Frame
     +		LR_Beg_SC_Clock,	! S/C Clock-Beginning of First MajFrame
     +		LR_End_SC_Clock,	! S/C Clock-Beginning of Last MajFrame
     +		LR_Beg_Year,		! ATC: year-Beginning of First MajFrame
     +		LR_Beg_DOY,		! ATC: day -Beginning of First MajFrame
     +		LR_Beg_MSec,		! ATC: msec-Beginning of First MajFrame
     +		LR_Beg_MicSec,		! ATC: usec-Beginning of First MajFrame
     +		LR_End_Year,		! ATC: year-Beginning of Last MajFrame
     +		LR_End_DOY,		! ATC: day -Beginning of Last MajFrame
     +		LR_End_MSec,		! ATC: msec-Beginning of Last MajFrame
     +		LR_End_MicSec,		! ATC: usec-Beginning of Last MajFrame
     +		LR_Num_MajFr_Exp,	! Number of Major Frames Expected
     +		LR_Num_MajFr_File,	! Number of Major Frames in File
     +		LR_Num_MajFr_Gaps,	! Major Frame Level Gaps in Coverage
     +		LR_Data_Cov_Typ,	! Data Coverage Type
     +		LR_Decom_Rerun_Num,	! Decommutation Rerun Number
     +		LR_Decom_Prog_V_Num,	! Decommutation Program Version Number
     +		LR_Decom_Char_DB_V_Num,	! Decom.Characteristic Database Vers.Num
     +		LR_Decom_Run_DateTime,	! Decommutation Run Date/Time
     +		LR_Instr_File_Name,	! Instrument File Name
     +		LR_Phys_Rec_Size,	! Physical Record Length
     +		LR_Fill3,		! Spares
     +		LR_Merge_Rerun_Num,	! Merge Rerun Number
     +		LR_Merge_Prog_V_Num,	! Merge Program Version Number
     +		LR_Merge_Run_DateTime,	! Merge Run Date/Time
     +		LR_Num_Edit_Files,	! Number of Edit Files
     +		LR_Edit_File,		! EDIT file 1 KEY ( File Name )
     +		LR_L0_Rerun_Num,	! Edit File 1 Rerun
     +		LR_L0_Prog_V_Num,
     +		LR_L0_Run_DateTime
      COMMON /LR_EPOCH_Times/		! EPOCH times from label record
     +		LR_Beg_EPOCH,
     +		LR_End_EPOCH

      INTEGER*4		LR_SCID,			! GEOTAIL = 24,WIND=25
     +			LR_Instr_Num,			! SMS = 5
     +			LR_Phys_Rec_Num,		! Label Record is #1
     +			LR_Phys_Rec_per_MajFr,		! 1 Phys Rec/Maj Fr
     +			LR_Phys_Rec_in_File,		! SFDU=2702, 
     +			LR_Beg_MajFr_Cnt,
     +			LR_End_MajFr_Cnt,		! changes for L0 file
     +			LR_Beg_SC_Clock(2),
     +			LR_End_SC_Clock(2),		! changes for L0 file
     +			LR_Beg_Year,
     +			LR_Beg_DOY,
     +			LR_Beg_MSec,
     +			LR_Beg_MicSec,			! Always zero
     +			LR_End_Year,			! changes for L0 file
     +			LR_End_DOY,			! changes for L0 file
     +			LR_End_MSec,			! changes for L0 file
     +			LR_End_MicSec,			! Always zero
     +			LR_Num_MajFr_Exp,
     +			LR_Num_MajFr_File,		! changes for L0 file
     +			LR_Num_MajFr_Gaps,		! changes for L0 file
     +			LR_Decom_Rerun_Num,
     +			LR_Phys_Rec_Size,		! Physical Record Length
     +			LR_Num_Edit_Files,
     +			LR_Merge_Rerun_Num
      CHARACTER*4	LR_Instr_Name,			! SMS = 'SIC ' currently
     +			LR_Data_Cov_Typ,
     +			LR_L0_Rerun_Num
      CHARACTER*8	LR_Decom_Prog_V_Num,
     +			LR_Decom_Char_DB_V_Num,
     +			LR_Merge_Prog_V_Num,
     +			LR_L0_Prog_V_Num
      CHARACTER*16	LR_Decom_Run_DateTime,
     +			LR_Merge_Run_DateTime,
     +			LR_L0_Run_DateTime
      CHARACTER*44	LR_Instr_File_Name
      CHARACTER*128	LR_Edit_File(20)
      CHARACTER*20	LR_Fill3
      REAL*8		LR_Beg_EPOCH, ! EPIC-added time <= Beg_Year,_DOY,_Msec
     +			LR_End_EPOCH  ! EPIC-added time <= End_Year,_DOY,_Msec
      ! Overlay LR_Buffer array and its common block of individual items
      BYTE		LR_Buffer(13000)
      CHARACTER*13000	C_LR_Buffer
      EQUIVALENCE	(LR_Buffer, LR_SCID, C_LR_Buffer)
      COMMON /LR_EditFile/
     +		LR_EditFile_Key,
     +		LR_EditFile_Rerun_Num,
     +		LR_EditFile_Prog_V_Num,
     +		LR_EditFile_Run_DateTime,
     +		LR_EditFile_Data_Typ
      CHARACTER*128	LR_EditFile_Buf
      CHARACTER*24	LR_EditFile_Key
      CHARACTER*4	LR_EditFile_Rerun_Num
      CHARACTER*8	LR_EditFile_Prog_V_Num
      CHARACTER*16	LR_EditFile_Run_DateTime
      CHARACTER*4	LR_EditFile_Data_Typ		! 'P/B' = Playback
							! 'R/T' = Real-time
      EQUIVALENCE	(LR_EditFile_Buf,	LR_EditFile_Key)
