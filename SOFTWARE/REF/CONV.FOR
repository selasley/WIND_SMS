      subroutine conv(iyr,imo,iday,iday_yr)
c     subroutine to convert from year, month, day to year day of year.
c
C     17-SEP-93    H.BROER   enhanced to conform with the Gregorian calender
c
      IMPLICIT NONE

      INTEGER*4    IDAY,
     :             IDAY_YR,
     :             IMO,
     :             IMOL(12),
     :             IMONL(12),
     :             IYR

      LOGICAL*1    IYRL
C
      data imol/0,31,60,91,121,152,182,213,244,274,305,335/
      data imonl/0,31,59,90,120,151,181,212,243,273,304,334/
C
      IF( MOD( iyr, 4 ) .NE. 0 )   THEN
          IYRL = .FALSE.
      ELSE
          IF( MOD( iyr, 100 ) .NE. 0 )   THEN
              IYRL = .TRUE.
          ELSE
              IF( MOD( iyr, 400 ) .NE. 0 )   THEN
                  IYRL = .FALSE.
              ELSE
                  IYRL = .TRUE.
              END IF
          END IF
      END IF
      if( iyrl ) then
          iday_yr=imol(imo)+iday
      else
          iday_yr=imonl(imo)+iday
      end if
c      write(6,*)iyr,imo,iday,iday_yr

      return

      end

