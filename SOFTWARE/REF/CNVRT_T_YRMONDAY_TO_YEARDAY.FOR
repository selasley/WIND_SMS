      SUBROUTINE CNVRT_T_YRMONDAY_TO_YEARDAY( Time, imo, iday )
C
C Subroutine to convert from year, month, day to year day of year using a
C structure of type T, which has no month or day field, as its input.
C
C Uses CNVRT_YRMONDAY_TO_YEARDAY to do the dirty work.
C
C     9-DEC-1994	L. Bleau	created to make life easier for those
C					using structures
C
      IMPLICIT NONE

      INCLUDE 'INC_DIR:EDBDEF.INC/NOLIST'

      INTEGER*4	IMO,		! INPUT:  month
     :		IDAY		!	  day of month
      RECORD /T/ Time		!	  structure of type T containing year

      INTEGER*4	IYR,		! INPUT:  year
     :		IDAY_YR 	! OUTPUT: day of year

      iyr = Time.YEAR
      call CNVRT_YRMONDAY_TO_YEARDAY( iyr, imo, iday, iday_yr )
      Time.DOY = iday_yr

      RETURN
      END
