      SUBROUTINE WIND_PB5_TO_EPOCH ( SCClock, EPOCH, STATUS )
C+
C PB5_TO_EPOCH
C
C Version:	4-APR-1994
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
C  Author       Change ID   Release	   Date		Description of Change 
C  ------       ---------   -------	-----------	--------------------- 
C  H. BROER	   N/A        N/A 	21-FEB-1994	ORIGINAL IMPLEMENTATION
C  L. BLEAU                             16-FEB-1995     add .5 in computing
C                                                       EPOCH time since Julian
C                                                       day starts at noon
C
C NOTES: N/A
C-
C
C PDL:
C
C    SET status TO SS$_NORMAL
C    SEPARATE BP5 ELEMENTS
C    ADD 2440000 TO TJD TO FORM JDN
C    IF INPUT TJD IS LESS THAN 9106
C    THEN
C        ADD 10000 TO JDN
C    ENDIF
C    
C    CALCULATE EPOCH
C
C    ADD .5 TO ACCOUNT FOR JULIAN DAY STARTING AT NOON
C
C    RETURN STATUS

      IMPLICIT NONE

      INCLUDE	'($SSDEF)/NOLIST'

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
      STATUS	= SS$_NORMAL
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
C   CALCULATE  EPOCH in Milliseconds since A.D.
C
      EPOCH	= JDN - 1721060
C
C We have observed that the result of this routine differes from the EPOCH time
C derived from the ATC time by .5 day.  We theorize this is because the
C Julian day starts at noon, which was not accounted for in this routine.
C To fix this error we add .5 to EPOCH at this point, which is still in units
C of days.
C
      EPOCH	= EPOCH + 0.5D0

      EPOCH	= EPOCH * 864.D2 + SEC	! Convert days to seconds plus SEC
      EPOCH	= EPOCH * 1.D3 + MSEC	! Convert to milliseconds plus MSEC
      EPOCH	= EPOCH + MICSEC*1.D-3	! 

      RETURN

      END
