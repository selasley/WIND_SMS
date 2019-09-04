C
C Hold common block and record declarations for various information which will
C eventually be stored in the file or science record header.
C
      COMMON /EPHDATA/
     +		EPH_EPOCH,		! Epoch times from orbit file
     +		GCI_POS,		! GCI position
     +		GCI_VEL,		! GCI velocity
     +		GSE_POS,		! GSE position
     +		GSE_VEL,		! GSE velocity
     +		GSM_POS,		! GSM position
     +		GSM_VEL,		! GSM velocity
     +		GCI_SUN_VECTOR,		! sun vector
     +		GCI_ATT,		! attitude info
     +		GSE_ATT,		! attitude info
     +		GSM_ATT,		! attitude info
     +		SPIN_RATE,		! body spin rate
     +		NREC_OR,		! number of records in orbit file
     +		NREC_AT			! number of records in attitude file
      REAL*8	EPH_EPOCH(144)		! Epoch times from orbit file
      REAL*4	SPIN_RATE(144)		! body spin rate
      RECORD /EPHEMERIS/ GCI_POS(144)	! GCI position
      RECORD /EPHEMERIS/ GCI_VEL(144)	! GCI velocity
      RECORD /EPHEMERIS/ GSE_POS(144)	! GSE position
      RECORD /EPHEMERIS/ GSE_VEL(144)	! GSE velocity
      RECORD /EPHEMERIS/ GSM_POS(144)	! GSM position
      RECORD /EPHEMERIS/ GSM_VEL(144)	! GSM velocity
      RECORD /EPHEMERIS/ GCI_SUN_VECTOR(144) ! sun vector
      RECORD /ATTITUDE/ GCI_ATT(144)	! attitude info
      RECORD /ATTITUDE/ GSE_ATT(144)	! attitude info
      RECORD /ATTITUDE/ GSM_ATT(144)	! attitude info
      INTEGER*4	NREC_OR,		! number of records in orbit file
     +		NREC_AT			! number of records in attitude file
