	INTEGER*4 SC(2)
100	ACCEPT 101,SC
101	FORMAT(2Z8)
	CALL FIXUP(SC)
	GOTO 100
	END
      SUBROUTINE FIXUP( SC_Clock )
C
C This routine is called after a READ to possibly reverse the two 32-bit parts
C of the SC clock.  On the older CD (1993) these values come in correctly,
C while on the newer files (both production and NRT) they are in the reverse
C order.  This may change again when the CDs of Level 0 data start being
C produced.
C
C For now FIXUP is called unconditionally.  This will have to be
C reevaluated later, when the CDs of actual data start coming out.
C
C ALGORITHM:
C
C The first time we are called obtain the current date (we're interested in the
C year) and extract the year from the SC clock.  Compare these two values.
C If the year obtained from the SC clock (SC_Year) is later than the
C current year, and obvious impossibility, set the logical variable SWAP_PARTS
C to .TRUE..  Also, if the high bit of SCClock(1) is set - which it never
C should be - set SWAP_PARTS to .TRUE..  If neither of these conditions holds
C set SWAP_PARTS to .FALSE..
C
C For all calls test SWAP_PARTS.  If it is set exchange the two 32-bit parts of
C SC_Clock.
C
C While this will not catch all errors, it will catch the more obvious one
C and allow WIND_CD_READ to operate
C Further refinements will have to wait for a later date.
C
      IMPLICIT NONE
      INTEGER*4 SC_Clock(2), ITEMP
      INTEGER MONTH, DAY, THIS_YEAR
      LOGICAL FIRST_TIME, SWAP_PARTS
      SAVE FIRST_TIME, SWAP_PARTS
      DATA FIRST_TIME, SWAP_PARTS /.TRUE., .FALSE./
C
      REAL*8 SC_EPOCH
      INTEGER*4  SC_Year, SC_DOY, SC_Msec, SC_MicSec, ISTAT
C
C The first time we are called:
C   Obtain the current date
C   Decode the SC clock time into normal calendar date/time
C   Compare the years from the two sources and set SWAP_PARTS accordingly
C
      IF (FIRST_TIME) THEN
C
C Get the date.  Remember to adjust year to proper century, as it only give a
C 2-digit value (0..99).
C
        CALL IDATE(MONTH,DAY,THIS_YEAR)
        IF (THIS_YEAR .LT. 90) THIS_YEAR = THIS_YEAR + 2000
        IF (THIS_YEAR .LT. 100) THIS_YEAR = THIS_YEAR + 1900
C
C First convert SC clock into EPOCH time
C
        CALL WIND_PB5_TO_EPOCH( SC_Clock,         ! UTC: PB5 (2 INTEGER*4)
     +                          SC_EPOCH,         ! UTC: EPOCH as Real*8
     +                          ISTAT )
C
C Now convert the EPOCH time into normal time: calendar year, day of year, etc.
C
        CALL WIND_EPOCH_TO_GREG(  SC_Year,
     +                            SC_DOY,
     +                            SC_Msec,
     +                            SC_MicSec,
     +                            SC_EPOCH,       ! UTC: EPOCH as Real*8
     +                            ISTAT )
C
C Now for the big test.  If the year just obtained (SC_Year) is later than the
C current year, set SWAP_PARTS to .TRUE..  Also, if the high bit of SCClock(1)
C is set - which it never should be - set SWAP_PARTS to .TRUE..  If neither of
C these conditions holds set SWAP_PARTS to .FALSE..
C Reset the first-time-called flag.
C
C While this will not catch all errors, it will catch the more obvious ones.
C Further refinements will have to wait for a later date.
C
        SWAP_PARTS = SC_Year .GT. THIS_YEAR .OR. BTEST(SC_Clock(1),31)
        FIRST_TIME = .FALSE.
        TYPE *,'THIS_YEAR, SC_Year =',THIS_YEAR,SC_Year
        IF (SWAP_PARTS) TYPE *,'SC clock parts will be swapped'
        IF (.NOT. SWAP_PARTS) TYPE *,'SC clock parts will NOT be swapped'
      END IF
C
C At this point SWAP_PARTS has been properly set
C Come here for all calls, test SWAP_PARTS, and exchange the two parts of
C SC_Clock if it is set
C
      IF (SWAP_PARTS) THEN
        ITEMP = SC_Clock(1)
        SC_Clock(1) = SC_Clock(2)
        SC_Clock(2) = ITEMP
      END IF
C
      RETURN
      END
