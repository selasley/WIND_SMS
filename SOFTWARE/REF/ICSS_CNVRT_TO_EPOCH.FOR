C+
C ICSS_CONVRT_TO_EPOCH - ICSS CONVERT TO EPOCH
C
C PURPOSE: UTILITY TO CONVERT THE ATC TIME FORMAT OF THE LEVEL 0 DATA RECORD
C             HEADER TO EPOCH TIME FORMAT
C
C UNIT TYPE: SUBROUTINE
C
C INVOCATION METHOD: CALL ICSS_CNVRT_TO_EPOCH (YEAR,
C                                              DAYOFYEAR,
C                                              MILLISEC,
C                                              MICROSEC,
C                                              EPOCH,
C                                              STATUS,
C 
C ARGUMENT LIST:
C 
C  NAME	                 TYPE	  USE	 DESCRIPTION
C  ----                  ----     ---    -----------
C  YEAR                  I*4       I     YEAR, e.g., 1992
C  DAYORYEAR             I*4       I     DAY OF YEAR, e.g., 256
C  MILLISEC              I*4       I     MILLISECONDS OF DAY
C  MICROSEC              I*4       I     MICROSECONDS OF DAY
C  EPOCH                 R*8       O     CDF EPOCH FORMAT
C  STATUS                I*4       O     STATUS OF TIME REFORMATTING
C
C 
C FILE/RECORD REFERENCES: NONE
C 
C EXTERNAL VARIABLES: NONE
C 
C EXTERNAL REFERENCES: UTL_YRDY_YRMNDY  - CONVERT YEAR AND DAY TO YEAR
C                                           MONTH AND DAY
C                      UTL_CONV_MILLIS  - 
C                      COMPUTE_EPOCH    - COMPUTE EPOCH
C                      ICSS_PUT_MSG     - POAT A MESSAGE
C 
C ABNORMAL TERMINATION CONDITIONS, ERROR MESSAGES:
C    ICSS_INVALID_INPUT_YEAR  - INVALID YEAR INPUT INTO PROGRAM
C    ICSS_INVALID_INPUT_DAY   - INVALID DAY OF YEAR INPUT INTO PROGRAM
C 
C ASSUMPTIONS, CONSTRAINTS, RESTRICTIONS: NONE
C 
C DEVELOPMENT HISTORY:
C 
C  Author       Change ID   Release   Date      Description of Change 
C  ------       ---------   -------   ----      --------------------- 
C  G. BLACKWELL   N/A         B-3   10/05/91    ORIGINAL IMPLEMENTATION
C
C NOTES: N/A
C
C-
C
C PDL:
C
C    SET status TO SS$_NORMAL
C    IF INPUT TIME IS VALID
C    THEN
C        CALL utl_yrdy_yrmndy TO CONVERT year AND dayofyear TO year, month,
C               AND day
C        CALL utl_conv_millis TO CONVERT millisec TO hours, minutes,
C               seconds AND milliseconds
C        CALL compute_epoch TO COMPUTE epoch TIME
C    ELSE
C        CALL ICSS_PUT_MSG TO POST INVALID DAY MESSAGE TO THE msg_handler
C    ENDIF
C    RETURN STATUS

          SUBROUTINE icss_cnvrt_to_epoch (
     #                                   year,       ! input
     #                                   dayofyear,  ! input
     #                                   millisec,   ! input
     #                                   microsec,   ! input
     #                                   epoch,      ! output
     #                                   status)     ! output
      IMPLICIT NONE          
C  DECLARATIONS
C    argument list
      INTEGER*4		year,
     .			dayofyear,
     .			millisec,
     .			microsec,
     .			status,
     .			put_msg_status
      REAL*8		epoch

C  local variables
      INTEGER*4		month,
     .			day,
     .			hour,
     .			minute,
     .			second,
     .			millisecond

      INCLUDE '($SSDEF)'
      INCLUDE 'INC_DIR:ICSS_MESSAGES.INC'
C          EXTERNAL  ICSS_ERR_CON_EPOCH

      STATUS = SS$_NORMAL
      IF (microsec .GE. 500) millisec = millisec + 1

      IF (year .GT. 0 .AND. (dayofyear .GT. 0 .AND. dayofyear .LE. 366)
     .       .AND. (millisec .GE. 0 .AND. millisec .LT. 86400000) )THEN
          CALL UTL_YRDY_YRMNDY (    year,		! input
     .                              dayofyear,		! input
     .                              month,		! output
     .                              day )		! output
          CALL UTL_CONV_MILLIS (    millisec,		! input
     .                              hour,		! output
     .                              minute,		! output
     .                              second,		! output
     .                              millisecond )	! output
          CALL COMPUTE_EPOCH (		year,		! input
     .					month,		! input
     .					day,		! input
     .					hour,		! input
     .					minute,		! input
     .					second,		! input
     .					millisecond,	! input
     .					epoch )		! output
      ELSE
              STATUS = %LOC(ICSS_ERR_CON_EPOCH)
C          CALL ICSS_PUT_MSG ('ICSS_CNVRT_TO_EPOCH',
C     .                            %VAL(status),
C     .                            ' ',' ',' ',
C     .                            put_msg_status)
      ENDIF

      RETURN

      END

c NEXT UNIT   *******************************************************************

C+
C 
C UTL_YRDY_YRMNDY - CONVERT YEAR, DAY OF YEAR TO YEAR, MONTH, DAY 
C 
C PURPOSE: UTILITY TO CONVERT YEAR, DAY OF YEAR TO YEAR, MONTH, AND DAY 
C 
C UNIT TYPE: SUBROUTINE
C 
C INVOCATION METHOD: CALL UTL_YRDY_YRMNDY (YEAR,
C                                          DAY_OF_YEAR,
C                                          MONTH,
C                                          DAY)
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
C FILE/RECORD REFERENCES: NONE
C 
C EXTERNAL VARIABLES: NONE
C 
C EXTERNAL REFERENCES: NONE
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
C  H. BROER			      2-FEB-94	Correct leapyear calculation
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

      SUBROUTINE UTL_YRDY_YRMNDY (YEAR, DAY_OF_YEAR, MONTH, DAY)
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
              IF( MOD( YEAR, 400 ) .EQ. 0 )   THEN
                  LEAPYR	= .TRUE.
              ENDIF
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

C  NEXT UNIT *************************************************************

C+
C 
C UNIT NAME: UTL_CONV_MILLIS   
C 
C PURPOSE: UTILITY TO CONVERT MILLISECONDS OF DAY TO HOUR, MINUTE, SECOND, 
C          AND MILLISECOND
C 
C UNIT TYPE: SUBROUTINE
C 
C INVOCATION METHOD: CALL UTL_CONV_MILLIS (MILLISECOND,
C                                          HOUR,
C                                          MINUTE,
C                                          SECOND,
C                                          MILLIS)
C 
C ARGUMENT LIST:
C 
C  NAME	                 TYPE	  USE	 DESCRIPTION
C  ----                  ----     ---    -----------
C  MILLISECOND           I*4      I      MILLISECONDS OF DAY
C  HOUR                  I*4      O      HOUR
C  MINUTE                I*4      O      MINUTE
C  SECOND                I*4      O      SECOND
C  MILLIS                I*4      O      MILLISECOND
C 
C FILE/RECORD REFERENCES: NONE
C 
C EXTERNAL VARIABLES: NONE
C 
C EXTERNAL REFERENCES: NONE
C 
C ABNORMAL TERMINATION CONDITIONS, ERROR MESSAGES: NONE
C 
C ASSUMPTIONS, CONSTRAINTS, RESTRICTIONS: NONE
C 
C DEVELOPMENT HISTORY:
C 
C  Author       Change ID   Release   Date      Description of Change 
C  ------       ---------   -------   ----      --------------------- 
C  M. WONG      N/A         B3R2      7/23/91   ORIGINAL IMPLEMENTATION
C
C NOTES: N/A
C
C-
C
C PDL:
C
C CONVERT input milliseconds to hour, minute, second, and millisecond
C RETURN
C

      SUBROUTINE UTL_CONV_MILLIS (MILLISECOND, HOUR, MINUTE, SECOND,
     .                            MILLIS)
C
      IMPLICIT NONE
C
C-- ARGUMENT DECLARATIONS
C
      INTEGER*4     MILLISECOND
      INTEGER*4     HOUR
      INTEGER*4     MINUTE
      INTEGER*4     SECOND
      INTEGER*4     MILLIS
C
C-- LOCAL VARIABLE DECLARATIONS
C
      INTEGER*4     THCONS  /1000/                       !MILLISECOND PER SECOND
      INTEGER*4     SECPMN  /60/                         !SECOND PER MINUTE
      INTEGER*4     SECPHR  /3600/                       !SECOND PER HOUR
      INTEGER*4     REMAIN
C
C---------------------------------------BEGIN EXECUTABLE CODE UTL_CONV_MILLIS---
C
      REMAIN = MILLISECOND/THCONS
      MILLIS = MILLISECOND - (REMAIN*THCONS)
      HOUR   = REMAIN/SECPHR
      REMAIN = REMAIN - (HOUR*SECPHR)
      MINUTE = REMAIN/SECPMN
      REMAIN = REMAIN - (MINUTE*SECPMN)
      SECOND = REMAIN
C
C-- EXIT
C
      RETURN
      END                      

C  NEXT UNIT  *******************************************************

	subroutine compute_EPOCH (year, month, day, hour, minute,
     .				  second, msec, EPOCH)
C------------------------------------------------------------------------------
C
C  NSSDC/CDF						Compute EPOCH value.
C
C  Version 1.1, 26-Mar-91, ST Systems (STX)
C
C  Modification history:
C
C   V1.0  Author unknown	Original version.
C   V1.1  26-Mar-91, J Love	Made compatible with SunOS FORTRAN V1.2
C				compiler.
C
C------------------------------------------------------------------------------

      implicit none							! V1.1

      integer*4		year,
     +			month,
     +			day,
     +			hour,
     +			minute,
     +			second,
     +			msec,
     +			julian_date

      real*8		EPOCH

	EPOCH = julian_date(year, month, day) - 1721060
	EPOCH = EPOCH * 24d0 + hour
	EPOCH = EPOCH * 60d0 + minute
	EPOCH = EPOCH * 60d0 + second
	EPOCH = EPOCH * 1000d0 + msec

	end


        integer*4 function julian_date( y, m, d)

	implicit	none

	integer*4	y,		! YEAR
     +			m,		! MONTH
     +			d,		! DAY
     +			jd		! JULIAN_DATE

	jd = int( 367*y - 7*(y+(m+9)/12)/4 - 3*((y+(m-9)/7)/100+1)/4 +
     .		 275*m/9 + d + 1721029 )

	julian_date = jd

	return
	end
