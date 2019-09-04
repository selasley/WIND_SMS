C
C This file contains the definition for the structure LIMITS.  A record of this
C type will exist for each quantity, whether of not limit checking and
C reporting is enabled for that quantity.
C
C When the program is initially started it reads the file LIMITS.DAT and fills
C in this structure for each quantity in the limits file.
C
	STRUCTURE /HOUSEKEEPING_STATS/
	    CHARACTER*40	IDENT		! identification of quantity
	    UNION
	     MAP
	      REAL*4		MIN_VALUE	! smallest value
	      REAL*4		MAX_VALUE	! largest value
	     END MAP
	     MAP
	      REAL*4		MIN,MAX
	     END MAP
	    END UNION
	    REAL*8		MIN_TIME	! Epoch time of smallest value
	    REAL*8		MAX_TIME	! Epoch time of largest value
	    RECORD /T/		MIN_W_TIME	! wall time of smallest value
	    RECORD /T/		MAX_W_TIME	! wall time of largest values
	    UNION
	     MAP
	      REAL*4		AVERAGE		! average value (not including
						! zeros); also used as running sum
	     END MAP
	     MAP
	      REAL*4		AVER
	     END MAP
	    END UNION
	    REAL*4		LAST_VALUE	! last valid value
	    REAL*4		LO_LIMIT	! lower limit value
	    REAL*4		HI_LIMIT	! upper limit value
	    INTEGER*4		LO_COUNT	! # times exceeded LO_LIMIT
	    INTEGER*4		HI_COUNT	! # times exceeded HI_LIMIT
	    INTEGER*4		COUNT		! # of non-zero values
	    INTEGER*4		NZERO		! # of zeros
	    LOGICAL*4		REPORT		! .TRUE. if out-of-limit values
						! are to be reported
	    CHARACTER*4		UNITS		! units of quantity
	END STRUCTURE ! /HOUSEKEEPING_STATS/
