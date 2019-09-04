	SUBROUTINE wind_l1_get_stics( L1_unit, rates, pha_words, status ) 
C
C This routine copies the STICS data structures from the Science Record 
C associated with unit L1_unit to the caller's arguments rates and pha_words,
C which are records of type ST_RATE and ST_PHA, respectively.
C
C     This version:	20-OCT-1994
C
C     Creation:
C	L. BLeau	20-OCT-1994
C
C     Revisions:
C
CDEC$	IDENT   '1.0'

	IMPLICIT NONE

	INCLUDE 'INC_DIR:EDBDEF.INC/NOLIST'
	INCLUDE 'INC_DIR:L1FMT.INC/NOLIST'
	INCLUDE 'INC_DIR:L1READ.INC/NOLIST'

	INTEGER*4 L1_unit, status
	RECORD /ST_RATE/ rates
	RECORD /ST_PHA/ pha_words

D	type *,'WIND_L1_GET_STICS: L1_unit=',L1_unit
	rates.cc_st_rate = L1_stics_rate(l1_unit).cc_st_rate
	pha_words.cc_st_pha = L1_stics_pha(l1_unit).cc_st_pha
	status = 1

	RETURN
	END
