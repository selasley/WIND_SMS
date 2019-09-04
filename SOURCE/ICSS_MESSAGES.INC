      INTEGER*4	ICSS_INPUT_FILE_CLOSERR,
     +		ICSS_OUTPUT_FILE_CLOSERR,
     +		ICSS_NO_PR_HK,
     +		ICSS_OPEN_PR_HK,
     +		ICSS_OPEN_PR_LZ,
     +		ICSS_NO_PR_LZ,
     +		ICSS_NO_PR_ORB,
     +		ICSS_OPEN_PR_ORB,
     +		ICSS_INV_HK_REQTYPE,
     +		ICSS_READ_PR_HK,
     +		ICSS_EOF_PR_HK,
     +		ICSS_GAP_IN_HK_FILE,
     +		ICSS_EOF_PR_LZ,
     +		ICSS_GAP_IN_LZ_FILE,
     +		ICSS_INV_LZ_REQTYPE,
     +		ICSS_READ_PR_LZ,
     +		ICSS_ORB_READ_ERR,
     +		ICSS_COOR_SYS_INV,
     +		ICSS_GET_NEXT_FILE,
     +          ICSS_GAP_IN_FILE,
     +          ICSS_ERR_CON_EPOCH
      parameter (ICSS_INPUT_FILE_CLOSERR =	'00101002'X)
      parameter (ICSS_OUTPUT_FILE_CLOSERR =	'00102008'X)
      parameter (ICSS_NO_PR_HK =		'00103008'X)
      parameter (ICSS_OPEN_PR_HK =		'00104008'X)
      parameter (ICSS_OPEN_PR_LZ =		'00105008'X)
      parameter (ICSS_NO_PR_LZ =		'00106008'X)
      parameter (ICSS_NO_PR_ORB =		'00107008'X)
      parameter (ICSS_OPEN_PR_ORB =		'00108008'X)
      parameter (ICSS_INV_HK_REQTYPE =		'00109008'X)
      parameter (ICSS_READ_PR_HK =		'00110008'X)
      parameter (ICSS_EOF_PR_HK =		'00111002'X)
      parameter (ICSS_GAP_IN_HK_FILE =		'00112003'X)
      parameter (ICSS_EOF_PR_LZ =		'00113002'X)
      parameter (ICSS_GAP_IN_LZ_FILE =		'00114003'X)
      parameter (ICSS_INV_LZ_REQTYPE =		'00115008'X)
      parameter (ICSS_READ_PR_LZ =		'00116008'X)
      parameter (ICSS_ORB_READ_ERR =		'00117008'X)
      parameter (ICSS_COOR_SYS_INV =		'00118008'X)
      parameter (ICSS_GET_NEXT_FILE =		'00119002'X)
      parameter (ICSS_GAP_IN_FILE =		'00120002'X)
      parameter (ICSS_ERR_CON_EPOCH =		'00121008'X)
!     ------------------------------------------------------------------------
      COMMON /KP_unit/ kp_lun, orb_lun, att_lun, hk_lun, lz_lun
      INTEGER*4 kp_lun  /9/,		! logical unit # for KP file
     +          orb_lun /7/,		! logical unit # for orbit file
     +          att_lun /4/,		! logical unit # for attitude file
     +          hk_lun  /3/,		! logical unit # for housekeeping file
     +          lz_lun  /8/		! logical unit # for level 0 file
 
!     ------------------------------------------------------------------------
