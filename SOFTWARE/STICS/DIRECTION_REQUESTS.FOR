C**********************************************************************
      SUBROUTINE DIRECTION_REQUESTS ( 	FILTER_TEL1,
     .           			FILTER_TEL2,
     .		 			FILTER_TEL3,
     .					AREA_FACTOR,
     .		 			SECTOR_MIN,
     .		 			SECTOR_MAX)

C     user inputs for data filtering by direction
C     19.Jan.95  ABG
C     Revision History:
C	26-MAR-1997	K.CHOTOO	Filter by telescope instead of by START,
C					STOP, or SSD IDs.
C     

      IMPLICIT NONE

C     data filtering options
      INTEGER*4  FILTER_TEL1,	!data filter by telescope 1
     .           FILTER_TEL2,	!data filter by telescope 2
     .           FILTER_TEL3,   !data filter by telescope 3
     .		 SECTOR_MIN,	!data filter by sector, minimum sector
     .		 SECTOR_MAX     !			maximum sector

      REAL*4 AREA_FAC1, 
     .       AREA_FAC2,
     .       AREA_FAC3,
     .       AREA_FACTOR	!Factor by which Geometric Factor increases depending on
C				 number of telescopes used.
      INTEGER*4  I,J,K
      CHARACTER*1 TEL1_REPLY, TEL2_REPLY, TEL3_REPLY, YORN	!yes or no answer


      FILTER_TEL1  = -1
      FILTER_TEL2  = -1
      FILTER_TEL3  = -1
      AREA_FAC1  = 0
      AREA_FAC2  = 0
      AREA_FAC3  = 0

 111  TYPE 7
    7 FORMAT(/,' Do you want data from telescope 1 ?  (y/n)    ',$)
      ACCEPT 20, TEL1_REPLY
  20  FORMAT(A)
      IF(TEL1_REPLY .EQ. 'Y' .OR. TEL1_REPLY .EQ. 'y') THEN
	FILTER_TEL1 = 1
        AREA_FAC1   = 1.0
      ENDIF
      TYPE 8
    8 FORMAT(/,' Do you want data from telescope 2 ?  (y/n)    ',$)
      ACCEPT 20, TEL2_REPLY
      IF(TEL2_REPLY .EQ. 'Y' .OR. TEL2_REPLY .EQ. 'y') THEN
	FILTER_TEL2 = 2
        AREA_FAC2   = 1.0
      ENDIF
      TYPE 9
    9 FORMAT(/,' Do you want data from telescope 3 ?  (y/n)    ',$)	  
      ACCEPT 20, TEL3_REPLY
      IF(TEL3_REPLY .EQ. 'Y' .OR. TEL3_REPLY .EQ. 'y') THEN
	FILTER_TEL3 = 3
        AREA_FAC3   = 1.0
      ENDIF
      IF(FILTER_TEL1*FILTER_TEL2*FILTER_TEL3 .EQ. -1) THEN
        WRITE(6,*)
	WRITE(6,*)'NO TELESCOPE SELECTED. Please try again'
	GOTO 111
      ENDIF

      AREA_FACTOR = AREA_FAC1 + AREA_FAC2 + AREA_FAC3

      TYPE 10
   10 FORMAT(/,' Enter minimum sector, 0-15 inclusive, to sum over (-1 = ALL)',
     .         '      ',$)
      ACCEPT *, SECTOR_MIN
      IF(SECTOR_MIN .GE. 0 .AND. SECTOR_MIN .LE. 15) THEN
        TYPE 11
   11   FORMAT(/,' Enter maximum sector (inclusive) to sum over ',
     .         '      ',$)
        ACCEPT *, SECTOR_MAX
      ELSE
	SECTOR_MIN = 0
	SECTOR_MAX = 15
      ENDIF
      

      RETURN
      END
