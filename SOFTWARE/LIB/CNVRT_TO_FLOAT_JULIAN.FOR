      REAL*8 FUNCTION CNVRT_TO_FLOAT_JULIAN( JUL, JHR, JMIN, JSEC, MILLI, MICRO )
C
C     This function extents the integer Julian day number to include
C     hours, minutes, seconds, milliseconds, and microseconds as the
C     fraction of the Julian day.
C
C     Version:	19-APR-1995
C
C     USAGE:
C     XX	= CNVRT_TO_FLOAT_JULIAN( JUL, JHR, JMIN, JSEC, MILLI, MICRO )
C
C     PARAMETERS:
C     REAL*8		XX		! OUTPUT: Floating point JULIAN day
C
C     INTEGER*4		JUL		! INPUT:Julian day
C			JHR		!	Hours
C			JMIN		!	Minutes
C			JSEC		!	Seconds
C			MILLI		!	Milliseconds
C			MICRO		!	Microseconds
C
C     Creation:
C     24-Mar_1994	H.Broer	University of Maryland
C
C Modification history:
C     19-APR-1995	L. Bleau	remove 12 hour subtraction
C
C NOTE:
C
C This routine originally subtracted 12 hours from the computed (floating
C point) Julian Day.  This was to reflect the fact that a Julian Day, by
C definition, starts at noon.  The "Truncated Julian Day" used in the WIND 
C Level Zero file starts at midnight, however.  This had resulted in the 
C fractional Julian Day being 1/2 day less than the correct value.  This 
C has been fixed. CNVRT_TO_FLOAT_JULIAN now produces a value which, when 
C converted to an Epoch time, matches the CDF Epoch time value for the same 
C day.
C
      IMPLICIT NONE

      REAL*8		FJUL		! internal

      INTEGER*4		JUL,		! INPUT:Julian day
     :			JHR,		!	Hours
     :			JMIN,		!	Minutes
     :			JSEC,		!	Seconds
     :			MILLI,		!	Milliseconds
     :			MICRO		!	Microseconds

      FJUL	= ( ( ( MICRO * 1.D-3 + MILLI ) * 1.D-3 ) + JSEC ) / 6.D1
      FJUL	= ( ( FJUL + JMIN ) / 6.D1 + JHR ) / 24.D0 + JUL

      CNVRT_TO_FLOAT_JULIAN = FJUL

      RETURN

      END
