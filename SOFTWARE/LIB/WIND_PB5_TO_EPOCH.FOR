      SUBROUTINE WIND_PB5_TO_EPOCH ( SCClock, EPOCH, STATUS )
C
C WIND_PB5_TO_EPOCH 
C
C	This version:	19-APR-1995
C
C PURPOSE: Routine to convert time expressed in PB5-code
C	   into EPOCH format (milliseconds AD, 1-JAN-1)
C
C UNIT TYPE: SUBROUTINE
C
C INVOCATION METHOD: CALL WIND_PB5_TO_EPOCH (	SCClock,
C						EPOCH,
C						STATUS )
C 
C ARGUMENT LIST:
C 
C  NAME	                TYPE	USE	DESCRIPTION
C  ----                 ----	---	-----------
C  SCClock		I*4	 I	PB5 CODE IN 2 INTEGERS*4
C  EPOCH		R*8	 O	CDF EPOCH FORMAT
C  STATUS		I*4	 O	STATUS OF TIME REFORMATTING
C 
C FILE/RECORD REFERENCES: NONE
C 
C EXTERNAL VARIABLES: NONE
C 
C ABNORMAL TERMINATION CONDITIONS, ERROR MESSAGES:
C
C PB5-CODE structure
C   First Integer*4 :
C   bit  0 = msb	= 15 bits of Truncated Julian Day ( TJD )
C   bit 15 = msb	= 17 bits of Seconds of Day
C
C   Second Integer*4 :
C   bit  0 = msb	= 10 bits of Milliseconds of Seconds
C   bit 10 = msb	= 10 bits of Microseconds of Seconds
C   bit 20 = msb	=  5 bits of Binary Fraction of Microseconds ( 1/64 th )
C
C   bit notation for Integer*4 :  bit  0 = msb (  most significant bit ),
C                                 bit 31 = lsb ( least significant bit )
C 
C ASSUMPTIONS, CONSTRAINTS, RESTRICTIONS:
C	TIME OF S/C CLOCK IS ASSUMED TO BE BETWEEN 30-APR-1993 AND 17-SEP-2020
C 
C DEVELOPMENT HISTORY:
C 
C  Author       Date		Description of Change 
C  --------	-----------	--------------------- 
C  H. BROER	21-FEB-1994	ORIGINAL IMPLEMENTATION
C  L. BLEAU     16-FEB-1995     add .5 in computing EPOCH time since Julian day
C                               starts at noon; this makes it agree with the
C				results of WIND_GREG_TO_EPOCH
C  L. Bleau	19-APR-1995	remove modification of 16-FEB-1995, reverting
C				routine to its previous state; add one day to
C				make day count match CDF Epoch time; see comments
C
C NOTES: 
C
C This is a change from the earlier version of WIND_GREG_TO_EPOCH in that
C previously all the work was done in the above-listed home-grown routines. 
C Now it is done mainly by the CDF routine compute_EPOCH.  This eliminates the
C 12 hour discrepancy between the CDF Epoch times and the Epoch times
C previously calculated by this routine.
C
C In addition, the routine CNVRT_TO_FLOAT_JULIAN subtracted 12 hours from the
C computed (floating point) Julian Day.  This was to reflect the fact that a
C Julian Day, by definition, starts at noon.  The "Truncated Julian Day" used 
C in the WIND Level Zero file starts at midnight, however.  This had resulted 
C in the Epoch times produced by WIND_GREG_TO_EPOCH being 1/2 day less than the 
C correct value.  This, too, has been fixed.  WIND_GREG_TO_EPOCH now produces a 
C value which matches the CDF Epoch time value for the same day.
C
C
C PDL:
C
C    SET status TO 1
C    SEPARATE BP5 ELEMENTS
C    ADD 2440000 TO TJD TO FORM JDN
C    IF INPUT TJD IS LESS THAN 9106
C    THEN
C        ADD 10000 TO JDN
C    ENDIF
C    
C    CALCULATE EPOCH
C
C    ADD 1 DAY TO ACCOUNT FOR YEAR 0 BEING LEAP YEAR
C
C    RETURN STATUS

      IMPLICIT NONE

      REAL*8	EPOCH		! OUTPUT: Time in milliseconds since 1-JAN-1 AD

      INTEGER*4 SCClock(2),	! INPUT:  S/C clock in PB5 CODE
     +		STATUS,		! OUTPUT: Status of process
     +		JDN,			! Julian Day Number
     +		MICSEC,			! 64th Milliseconds
     +		MSEC,			! Milliseconds
     +		SEC,			! Seconds
     +		TJD			! Truncated Julian Day
C
C  START OF EXECUTABLE CODE
C
      STATUS	= 1
C
C    PARSE PB5 CODE
C
      SEC	= JIAND(  SCCLOCK(1), '1FFFF'X ) 
      TJD	= JIAND( JISHFT( SCCLOCK(1), -17 ), '3FFF'X )
      MSEC	= JIAND( JISHFT( SCCLOCK(2), -22 ), '3FF'X )
      MICSEC	= JIAND( JISHFT( SCCLOCK(2), -12 ), '3FF'X )	! MICROSEC
C
C    EXPAND TRUNCATED JULIAN DAY to JULIAN DAY NUMBER
C
      JDN	= TJD + 2440000		! JDN of 24-May-1968

      IF( TJD .LT. 9106 )   THEN	! TJD OF 30-APR-1993
          JDN	= JDN + 10000		! Base of 9-Oct-1995
      ENDIF
C
C   CALCULATE  EPOCH (in days) since Jan 1, 0 A.D. (aka 1 B.C.)
C
      EPOCH	= JDN - 1721060
C
C The effect of the calculation performed by the CDF Epoch utility routines
C assume year 0 A.D. is a leap year.  For that reason all dates after Feb 28, 0
C A.D. will differ by one day from the CDF Epoch date.  Take that into account
C here by adding 1 to the day count before converting to milliseconds.  In this 
C way the EPOCH time produced by this routine will match the EPOCH times 
C produced by CDF. 
C
      EPOCH	= EPOCH + 1D0
C
C Convert EPOCH to Milliseconds since A.D.
C
      EPOCH	= EPOCH * 864.D2 + SEC	! Convert days to seconds plus SEC
      EPOCH	= EPOCH * 1.D3 + MSEC	! Convert to milliseconds plus MSEC
      EPOCH	= EPOCH + MICSEC*1.D-3	! Add in microseconds

      RETURN

      END
