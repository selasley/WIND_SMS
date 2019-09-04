      subroutine CNVRT_YRMONDAY_TO_YEARDAY( iyr, imo, iday, iday_yr )
c     subroutine to convert from year, month, day to year day of year.
c
C      4-APR-94    H.BROER   enhanced to conform with the Gregorian calender
c
      IMPLICIT NONE

      INTEGER*4	IYR,		! INPUT:  year
     :		IMO,		!	  month
     :		IDAY,		!	  day of month
     :		IDAY_YR,	! OUTPUT: day of year
     :        	IMOL(12),	! internal array
     :		IMONL(12)	! internal array

      LOGICAL*1	IYRL
C
      data imol/ 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335 /
      data imonl/0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 /
C
      iyrl = .FALSE.
      IF( MOD( iyr, 4 ) .EQ. 0 )   THEN
          IF( MOD( iyr, 100 ) .NE. 0 )   THEN
              iyrl = .TRUE.
          ELSE
              IF( MOD( iyr, 400 ) .EQ. 0 )	iyrl = .TRUE.
          END IF
      END IF

      if( iyrl ) then
          iday_yr	= imol(  imo ) + iday
      else
          iday_yr	= imonl( imo ) + iday
      end if

c      write( 6, * )	iyr, imo, iday, iday_yr

      return

      end
