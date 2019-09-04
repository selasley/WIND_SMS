	SUBROUTINE WIND_L1_GET_BASIC( L1_unit, basic_rec, status )
C
C This routine copies the Basic (Core) data structure from the Science
C Record associated with unit L1_unit to the caller's argument basic_rec,
C which is a record of type CORE.
C
C     This version:	22-SEP-1994
C
C     Creation:
C	L. BLeau	22-SEP-1994
C
C     Revisions:
C
CDEC$	IDENT   '1.0'

	IMPLICIT NONE

	INCLUDE 'INC_DIR:EDBDEF.INC/NOLIST'
	INCLUDE 'INC_DIR:L1FMT.INC/NOLIST'
	INCLUDE 'INC_DIR:L1READ.INC/NOLIST'

	INTEGER*4 L1_unit, status
	RECORD /CORE/ basic_rec

D	type *,'WIND_L1_GET_BASIC: L1_unit=',L1_unit
	basic_rec.CCORE = L1_SCI(L1_unit).CCORE
	status = 1

	RETURN
	END
