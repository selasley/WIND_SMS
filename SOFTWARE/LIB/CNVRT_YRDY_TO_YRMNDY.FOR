      SUBROUTINE CNVRT_YRDY_TO_YRMNDY( YEAR, DAY_OF_YEAR, MONTH, DAY )
C+
C	This version:	5-APR-1994
C 
C CNVRT_YRDY_TO_YRMNDY - CONVERT YEAR, DAY OF YEAR TO YEAR, MONTH, DAY 
C 
C PURPOSE: UTILITY TO CONVERT YEAR, DAY OF YEAR TO YEAR, MONTH, AND DAY 
C 
C UNIT TYPE: SUBROUTINE
C 
C USAGE:
C    CALL CNVRT_YRDY_TO_YRMNDY( YEAR,
C				DAY_OF_YEAR,
C				MONTH,
C				DAY )
C 
C ARGUMENT LIST:
C 
C  NAME	                 TYPE	  USE	 DESCRIPTION
C  ----                  ----     ---    -----------
C  YEAR                  I*4      I/O    YEAR, e.g., 1992
C  DAY_OR_YEAR           I*4      I      DAY OF YEAR, e.g., 256
C  MONTH                 I*4      O      MONTH, e.g., 9
C  DAY                   I*4      O      DAY, e.g., 12   
C 
C ABNORMAL TERMINATION CONDITIONS, ERROR MESSAGES: NONE
C 
C ASSUMPTIONS, CONSTRAINTS, RESTRICTIONS: NONE
C 
C DEVELOPMENT HISTORY:
C 
C  Author       Change ID   Release   Date      Description of Change 
C  ------       ---------   -------   ----      --------------------- 
C  M. WONG      N/A         B2R1      6/3/91    ORIGINAL IMPLEMENTATION
C  H. BROER			      5-APR-94	Correct leapyear calculation
C	
C NOTES: N/A
C
C-
C
C PDL:
C
C SET up month table (MONTAB) representing beginning day of each month of 
C  non-leap year
C IF YEAR is not leap year and DAY_OF_YEAR is greater than 59 THEN
C    INCREMENT DAY_OF_YEAR by 1
C ENDIF
C SET up month counter to 1
C DOWHILE month counter is less than 12 and DAY_OF_YEAR is greater than
C  the beginning day of next month
C    INCREMENT month counter by 1
C ENDDO
C SET month counter to MONTH
C SET DAY_OF_YEAR decremented by the beginning day of this month to DAY
C RETURN
C

C
CDEC$ IDENT 'VER 1.02'
C
      IMPLICIT NONE
C
C-- ARGUMENT DECLARATIONS
C
      INTEGER*4		YEAR,
     +			DAY_OF_YEAR,
     +			MONTH,
     +			DAY,
C
C-- LOCAL VARIABLE DECLARATIONS
C
     +			IDAY,
     +			J,
     +			MONTAB(12) /	  0,	 31,	 60,
     .					 91,	121,	152,
     .					182,	213,	244,
     .					274,	305,	335 /

      LOGICAL*1		LEAPYR
C
C---------------------------------------BEGIN EXECUTABLE CODE UTL_YRDY_YRMNDY---
C
      IDAY	= DAY_OF_YEAR
C
C-- CONVERT YEAR, DAY OF YEAR TO YEAR, MONTH, DAY
C
      LEAPYR	= .FALSE.

      IF( MOD( YEAR, 4 ) .EQ. 0 )   THEN
          IF( MOD( YEAR, 100 ) .NE. 0 )   THEN
              LEAPYR	= .TRUE.
          ELSE
              IF( MOD( YEAR, 400 ) .EQ. 0 )   LEAPYR	= .TRUE.
          ENDIF
      ENDIF

      IF( (.NOT.LEAPYR) .AND. DAY_OF_YEAR .GT. 59 )   THEN
          IDAY	= IDAY + 1
      ENDIF
C
      J = 0
   10 CONTINUE
      J = J + 1
      IF ((J.LT.12) .AND. (IDAY.GT.MONTAB(J+1))) GO TO 10
C
      MONTH  = J
      DAY    = IDAY - MONTAB(J)
C
C-- EXIT
C
      RETURN
      END                      
