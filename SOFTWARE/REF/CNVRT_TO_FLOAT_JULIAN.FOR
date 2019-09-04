      REAL*8 FUNCTION CNVRT_TO_FLOAT_JULIAN( JUL, JHR, JMIN, JSEC, MILLI, MICRO )
!
!     This function extents the integer Julian day number to include
!     hours, minutes, seconds, milliseconds, and microseconds as the
!     fraction of the Julian day.
!
!     Version:	25-Mar-1994
!
!     USAGE:
!     XX	= CNVRT_TO_FLOAT_JULIAN( JUL, JHR, JMIN, JSEC, MILLI, MICRO )
!
!     PARAMETERS:
!     REAL*8		XX		! OUTPUT: Floating point JULIAN day
!
!     INTEGER*4		JUL		! INPUT:Julian day
!			JHR		!	Hours
!			JMIN		!	Minutes
!			JSEC		!	Seconds
!			MILLI		!	Milliseconds
!			MICRO		!	Microseconds
!
!     Creation:
!     24-Mar_1994	H.Broer	University of Maryland
!
      IMPLICIT NONE

      REAL*8		FJUL		! internal

      INTEGER*4		JUL,		! INPUT:Julian day
     :			JHR,		!	Hours
     :			JMIN,		!	Minutes
     :			JSEC,		!	Seconds
     :			MILLI,		!	Milliseconds
     :			MICRO		!	Microseconds

      FJUL	= ( ( ( MICRO * 1.D-3 + MILLI ) * 1.D-3 ) + JSEC ) / 6.D1
      FJUL	= ( ( FJUL + JMIN ) / 6.D1 + JHR - 12.D0 ) / 24.D0 + JUL

      CNVRT_TO_FLOAT_JULIAN = FJUL

      RETURN

      END
