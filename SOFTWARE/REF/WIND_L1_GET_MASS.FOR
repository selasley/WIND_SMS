	SUBROUTINE wind_l1_get_mass( L1_unit, rates, pha_words, status ) 
C
C This routine copies the MASS data structures from the Science Record 
C associated with unit L1_unit to the caller's arguments rates and pha_words,
C which are records of type MA_RATE and MA_PHA, respectively.
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
	RECORD /MA_RATE/ rates
	RECORD /MA_PHA/ pha_words

D	type *,'WIND_L1_GET_MASS: L1_unit=',L1_unit
	rates.cc_ma_rate = L1_mass_rate(l1_unit).cc_ma_rate
	pha_words.cc_ma_pha = L1_mass_pha(l1_unit).cc_ma_pha
	status = 1

	RETURN
	END
