!+
!     SW_CODES.INC
!      This common block contains the ... 
!
!     REFERENCE:
!
!     USAGE and PARAMETERS:
!      INCLUDE SW_CODES.INC
!       .	.
!
!     CREATION:
!       8 Apr 1992, M. L. Grimes
!
!     REVISIONS:
!
!-
!==============================================================================
      COMMON	/VERSIONS/      SWVersion, SWVersion_N
      REAL*4	SWVersion(2,125)
!---- General use --------(1-25)-----------------------------------------------
      REAL*4
     +		Ver__PROG(2),
     +		Ver_MAINSEC(2),		! include file
     +		Ver_EPIC_CHECK_SWVERSION(2),
     +		Ver_EPIC_INIT_CHANNELS(2),
     +		Ver_EPIC_CLOSE_USERMSG(2),
     +		Ver_EPIC_OPEN_USERMSG(2),
     +		Ver_EPIC_PUT_USERMSG(2),
     +		Ver_EPIC_TERM(2),
     +		Ver_EPIC_RET_CALIBRATION(2),
     +		Ver_EPIC_RET_CONTROL_PARAMETERS(2),
     +          Ver_EPIC_INTERPRET_USERMSG(2)
      EQUIVALENCE	
     +          (SWVersion(1,1),        Ver__PROG),
     +          (SWVersion(1,2),        Ver_MAINSEC),
     +		(SWVersion(1,3),        Ver_EPIC_CHECK_SWVERSION),
     +		(SWVersion(1,4),	Ver_EPIC_INIT_CHANNELS),
     +		(SWVersion(1,5),	Ver_EPIC_CLOSE_USERMSG),
     +		(SWVersion(1,6),	Ver_EPIC_OPEN_USERMSG),
     +		(SWVersion(1,7),	Ver_EPIC_PUT_USERMSG),
     +		(SWVersion(1,8),	Ver_EPIC_TERM),
     +		(SWVersion(1,9),	Ver_EPIC_RET_CALIBRATION),
     +		(SWVersion(1,10),	Ver_EPIC_RET_CONTROL_PARAMETERS),
     +          (SWVersion(1,11),	Ver_EPIC_INTERPRET_USERMSG)
      DATA Ver_MAINSEC  / 1.00, 0/
!---- Conversion from SFDU to L0 use -------(26-50)----------------------------
      REAL*4
     +		Ver_EPIC_SFDU_to_L0(2),
     +		Ver_EPIC_OPEN_SFDU(2),
     +		Ver_EPIC_READ_SFDU_FILE_HEADER(2),
     +		Ver_EPIC_READ_SFDU_LABL_RECORD(2),
     +		Ver_EPIC_READ_SFDU_DATA_RECORD(2),
     +		Ver_EPIC_CLOSE_SFDU(2),
     +		Ver_EPIC_OPEN_CAL(2),
     +		Ver_EPIC_READ_CAL_LABL_RECORD(2),
     +		Ver_EPIC_READ_CAL_DATA_RECORD(2),
     +		Ver_EPIC_CLOSE_CAL(2),
     +		Ver_ICSS_RET_LZ(2)
      EQUIVALENCE
     +		(SWVersion(1,26),	Ver_EPIC_SFDU_to_L0),
     +		(SWVersion(1,27),	Ver_EPIC_OPEN_SFDU),
     +		(SWVersion(1,28),	Ver_EPIC_READ_SFDU_FILE_HEADER),
     +		(SWVersion(1,29),	Ver_EPIC_READ_SFDU_LABL_RECORD),
     +		(SWVersion(1,30),	Ver_EPIC_READ_SFDU_DATA_RECORD),
     +		(SWVersion(1,31),	Ver_EPIC_CLOSE_SFDU),
     +		(SWVersion(1,32),	Ver_EPIC_OPEN_CAL),
     +		(SWVersion(1,33),	Ver_EPIC_READ_CAL_LABL_RECORD),
     +		(SWVersion(1,34),	Ver_EPIC_READ_CAL_DATA_RECORD),
     +		(SWVersion(1,35),	Ver_EPIC_CLOSE_CAL),
     +		(SWVersion(1,36),	Ver_ICSS_RET_LZ)
!---- L0 use --------(51-75)---------------------------------------------------
      REAL*4
     +		Ver_EPIC_CHECK_SYNC(2),
     +		Ver_EPIC_DECOM(2),
     +		Ver_EPIC_DECOM_DPUCOMMON(2),
     +		Ver_EPIC_RET_LEVEL0(2),
     +		Ver_EPIC_ADJUST_TIME(2),
     +		Ver_EPIC_OPEN_L0(2),
     +		Ver_EPIC_WRITE_L0_FILE_HEADER(2),
     +		Ver_EPIC_WRITE_L0_LABL_RECORD(2),
     +		Ver_EPIC_WRITE_L0_DATA_RECORD(2),
     +		Ver_EPIC_READ_L0_LABL_RECORD(2),
     +		Ver_EPIC_READ_L0_DATA_RECORD(2),
     +		Ver_EPIC_CLOSE_L0(2),
     +		Ver_Julian_doy(2),
     +		Ver_EPIC_RET_HEADS(2),
     +		Ver_EPIC_SETUP_SCI_RECORD(2)
      EQUIVALENCE	
     +          (SWVersion(1,51),       Ver_EPIC_CHECK_SYNC),
     +		(SWVersion(1,52),	Ver_EPIC_DECOM),
     +		(SWVersion(1,53),	Ver_EPIC_DECOM_DPUCOMMON),
     +		(SWVersion(1,54),	Ver_EPIC_RET_LEVEL0),
     +		(SWVersion(1,55),	Ver_EPIC_ADJUST_TIME),
     +		(SWVersion(1,56),	Ver_EPIC_OPEN_L0),
     +		(SWVersion(1,57),	Ver_EPIC_WRITE_L0_FILE_HEADER),
     +		(SWVersion(1,58),	Ver_EPIC_WRITE_L0_LABL_RECORD),
     +		(SWVersion(1,59),	Ver_EPIC_WRITE_L0_DATA_RECORD),
     +		(SWVersion(1,60),	Ver_EPIC_READ_L0_LABL_RECORD),
     +		(SWVersion(1,61),	Ver_EPIC_READ_L0_DATA_RECORD),
     +		(SWVersion(1,62),	Ver_EPIC_CLOSE_L0),
     +		(SWVersion(1,63),	Ver_Julian_doy),
     +		(SWVersion(1,64),	Ver_EPIC_RET_HEADS),
     +		(SWVersion(1,65),	Ver_EPIC_SETUP_SCI_RECORD)
!---- Conversion from L0 to L1 use --------(76-100)----------------------------
      REAL*4
     +		Ver_EPIC_CLEAR_BUFFERS(2),
     +		Ver_EPIC_RET_MODE(2),
     +		Ver_EPIC_GET_FORMAT(2),
     +		Ver_EPIC_GET_CHINDEX(2),
     +		Ver_EPIC_L0_TO_L1(2),
     +		Ver_EPIC_SHIFT_EDBS(2),
     +		Ver_EPIC_IN_FILL(2)
      EQUIVALENCE	
     +          (SWVersion(1,76),       Ver_EPIC_CLEAR_BUFFERS),
     +		(SWVersion(1,77),	Ver_EPIC_RET_MODE),
     +		(SWVersion(1,78),	Ver_EPIC_GET_FORMAT),
     +		(SWVersion(1,79),	Ver_EPIC_GET_CHINDEX),
     +		(SWVersion(1,80),	Ver_EPIC_L0_TO_L1),
     +		(SWVersion(1,81),	Ver_EPIC_SHIFT_EDBS),
     +		(SWVersion(1,82),	Ver_EPIC_IN_FILL)
!---- L1 use --------(101-125)-------------------------------------------------
      REAL*4
     +		Ver_EPIC_INIT_DECOMPRESS(2),
     +		Ver_L1_GET_REC_PREAMBLE(2),
     +		Ver_EPIC_HDR_WRITE(2),
     +		Ver_EPIC_RET_TIME(2),
     +		Ver_EPIC_WRITE_L1(2),
     +		Ver_EPIC_READ_L1(2),
     +		Ver_EPIC_OPEN_L1(2),
     +		Ver_EPIC_CLOSE_L1(2),
     +		Ver_EPIC_READ_EDBMAP(2),
     +		Ver_EPIC_INIT_SCHEMES(2),
     +		Ver_EPIC_FILL_OUT(2),
     +		Ver_EPIC_DECOM_HSKP(2),
     +		Ver_EPIC_UNPACK_HSKP(2)
      EQUIVALENCE	
     +          (SWVersion(1,101),      Ver_EPIC_INIT_DECOMPRESS),
     +		(SWVersion(1,102),	Ver_L1_GET_REC_PREAMBLE),
     +		(SWVersion(1,103),	Ver_EPIC_HDR_WRITE),
     +		(SWVersion(1,104),	Ver_EPIC_RET_TIME),
     +		(SWVersion(1,105),	Ver_EPIC_WRITE_L1),
     +		(SWVersion(1,106),	Ver_EPIC_READ_L1),
     +		(SWVersion(1,107),	Ver_EPIC_OPEN_L1),
     +		(SWVersion(1,108),	Ver_EPIC_CLOSE_L1),
     +		(SWVersion(1,109),	Ver_EPIC_READ_EDBMAP),
     +		(SWVersion(1,110),	Ver_EPIC_INIT_SCHEMES),
     +		(SWVersion(1,111),	Ver_EPIC_FILL_OUT),
     +		(SWVersion(1,112),	Ver_EPIC_DECOM_HSKP),
     +		(SWVersion(1,113),	Ver_EPIC_UNPACK_HSKP)
      INTEGER*4	SWVersion_N	/113/	! Number of software modules
!=============================================================================
!     Message status codes
!
!       FATAL:       Error state from which no or an insufficient recovery was
!                    possible (value is divisible by 8).
!       ERROR:       Error state from which a recovery was possiblE (value is
!                    divisible by 4).
!       WARNING:     Error state for which no recovery was necessary (value is
!                    is divisible by 2).
!       INFORMATION: Non-error state (value is odd).
!
!-----------------------------------------------------------------------------
!---- Information codes (1-25) -----------------------------------------------
      INTEGER*4
     +		EPIC$_CALIB_FILE,
     +		EPIC$_CONTROL_FILE,
     +		EPIC$_FOUND_SYNC,
     +		EPIC$_NEW_ICS_MODE,
     +		EPIC$_NEW_STICS_MODE,
     +		EPIC$_SCI_REC_COUNT,
     +		EPIC$_START_RUN,
     +		EPIC$_START_TIME,
     +		EPIC$_STOP_TIME,
     +		EPIC$_STARTING_ICS_MODE,
     +		EPIC$_STARTING_STICS_MODE,
     +		EPIC$_TERMINATION_BY_DONE_FLAG,
     +		EPIC$_TERMINATION_BY_EOF_FLAG,
     +		EPIC$_TERMINATION_BY_FATAL_ERR,
     +		EPIC$_USER_MSG_FILE_OPENED,
     +		EPIC$_USER_MSG_FILE_CLOSED
      PARAMETER (EPIC$_CALIB_FILE =     		'01000002'X)
      PARAMETER (EPIC$_CONTROL_FILE =   		'02000002'X)
      PARAMETER	(EPIC$_FOUND_SYNC =			'03000002'X)
      PARAMETER (EPIC$_NEW_ICS_MODE =			'04000002'X)
      PARAMETER (EPIC$_NEW_STICS_MODE =			'05000002'X)
      PARAMETER (EPIC$_SCI_REC_COUNT =			'06000002'X)
      PARAMETER (EPIC$_START_RUN =     			'07000002'X)
      PARAMETER (EPIC$_START_TIME =			'08000002'X)
      PARAMETER (EPIC$_STOP_TIME =     			'09000002'X)
      PARAMETER (EPIC$_STARTING_ICS_MODE =		'10000002'X)
      PARAMETER (EPIC$_STARTING_STICS_MODE =		'11000002'X)
      PARAMETER (EPIC$_TERMINATION_BY_DONE_FLAG =	'12000002'X)
      PARAMETER (EPIC$_TERMINATION_BY_EOF_FLAG =	'13000002'X)
      PARAMETER (EPIC$_TERMINATION_BY_FATAL_ERR =	'14000002'X)
      PARAMETER	(EPIC$_USER_MSG_FILE_OPENED =		'15000002'X)
      PARAMETER	(EPIC$_USER_MSG_FILE_CLOSED =		'16000002'X)
!---- Warning codes (26-40) --------------------------------------------------
      INTEGER*4
     +		EPIC$_CLOSE_ERROR,
     +		EPIC$_DIVIDE_ZERO,
     +		EPIC$_DROPPED_SPIN,
     +		EPIC$_ICS_INVALID,
     +		EPIC$_SCI_REC_NOT_SYNCED,
     +		EPIC$_SKIP_TO_NEXT_SCRE,
     +		EPIC$_STICS_INVALID
      PARAMETER	(EPIC$_CLOSE_ERROR =	 		'26000001'X)
      PARAMETER	(EPIC$_DIVIDE_ZERO =	 		'27000003'X)
      PARAMETER	(EPIC$_DROPPED_SPIN =		 	'28000005'X)
      PARAMETER	(EPIC$_ICS_INVALID =			'29000007'X)
      PARAMETER (EPIC$_SCI_REC_NOT_SYNCED =		'30000009'X)
      PARAMETER (EPIC$_SKIP_TO_NEXT_SCRE =		'31000011'X)
      PARAMETER	(EPIC$_STICS_INVALID =			'32000013'X)
!---- Error codes (41-65) -----------------------------------------------------
      INTEGER*4
     +		EPIC$_BAD_ICS_MODE,
     +		EPIC$_EDB_COUNTER_MISMATCH,
     +		EPIC$_FILL_IN_LZ_FILE,
     +		EPIC$_EOF_PR_LZ,
     +		EPIC$_GAP_IN_LZ_FILE,
     +		EPIC$_HK_SUBCOM_OUT_OF_SYNC,
     +		EPIC$_LOST_SYNC,
     +		EPIC$_MIXED_EPIC_PWR_STATE,
     +		EPIC$_MIXED_HV_ENABLE_STATE,
     +		EPIC$_MIXED_ICS_LVPS_STATE,
     +		EPIC$_MIXED_SENSOR_MODE_STATE,
     +		EPIC$_MIXED_STICS_LVPS_STATE,
     +		EPIC$_SCI_REC_CNTR_MISMATCH,
     +		EPIC$_SYNC_ERR_IN_LZ_FILE,
     +		EPIC$_SYNC_NOT_FOUND,
     +		EPIC$_SYNC_SHIFTED,
     +		EPIC$_INVALID_SCRE_NUM
      PARAMETER	(EPIC$_BAD_ICS_MODE =		 	'42000004'X)
      PARAMETER	(EPIC$_EDB_COUNTER_MISMATCH =	 	'43000004'X)
      PARAMETER	(EPIC$_FILL_IN_LZ_FILE =		'44000004'X)
      PARAMETER	(EPIC$_EOF_PR_LZ =			'45000004'X)
      PARAMETER	(EPIC$_GAP_IN_LZ_FILE = 		'46000004'X)
      PARAMETER	(EPIC$_HK_SUBCOM_OUT_OF_SYNC =		'47000004'X)
      PARAMETER	(EPIC$_LOST_SYNC =			'48000004'X)
      PARAMETER	(EPIC$_MIXED_EPIC_PWR_STATE =		'49000004'X)
      PARAMETER	(EPIC$_MIXED_HV_ENABLE_STATE =		'50000004'X)
      PARAMETER	(EPIC$_MIXED_ICS_LVPS_STATE =		'51000004'X)
      PARAMETER	(EPIC$_MIXED_SENSOR_MODE_STATE =	'52000004'X)
      PARAMETER	(EPIC$_MIXED_STICS_LVPS_STATE =		'53000004'X)
      PARAMETER	(EPIC$_SCI_REC_CNTR_MISMATCH =		'54000004'X)
      PARAMETER	(EPIC$_SYNC_ERR_IN_LZ_FILE =		'55000004'X)
      PARAMETER	(EPIC$_SYNC_NOT_FOUND =		 	'56000004'X)
      PARAMETER	(EPIC$_SYNC_SHIFTED =		 	'57000004'X)
      PARAMETER	(EPIC$_INVALID_SCRE_NUM =	 	'58000004'X)
!---- Fatal codes (66-99) -----------------------------------------------------
      INTEGER*4
     +		EPIC$_BAD_CAL_VERSION,
     +		EPIC$_BAD_CP_VERSION,
     +		EPIC$_BAD_DECOMPRESSION_I_CODE,
     +		EPIC$_BAD_DECOMPRESSION_S_CODE,
     +		EPIC$_BAD_SWVersion,
     +		EPIC$_BAD_SWVersion_N,
     +		EPIC$_MAP_RD_FAILURE,
     +		EPIC$_OPEN_FAILURE,
     +		EPIC$_READ_ERROR,
     +		EPIC$_WRONG_CNTRL_SECTION,
     +		EPIC$_BAD_DECOMPRESSION_CODE
      PARAMETER (EPIC$_BAD_CAL_VERSION =		'66000008'X)
      PARAMETER (EPIC$_BAD_CP_VERSION =			'67000008'X)
      PARAMETER	(EPIC$_BAD_DECOMPRESSION_I_CODE =	'68000008'X)
      PARAMETER	(EPIC$_BAD_DECOMPRESSION_S_CODE =	'69000008'X)
      PARAMETER	(EPIC$_BAD_SWVersion =			'70000008'X)
      PARAMETER (EPIC$_BAD_SWVersion_N =		'71000008'X)
      PARAMETER (EPIC$_MAP_RD_FAILURE =			'72000008'X)
      PARAMETER	(EPIC$_OPEN_FAILURE =			'73000008'X)
      PARAMETER	(EPIC$_READ_ERROR =	 		'74000008'X)
      PARAMETER (EPIC$_WRONG_CNTRL_SECTION =		'75000008'X)
      PARAMETER	(EPIC$_BAD_DECOMPRESSION_CODE =		'76000008'X)
!     ------------------------------------------------------------------------
