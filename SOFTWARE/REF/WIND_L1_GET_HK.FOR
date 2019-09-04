	SUBROUTINE wind_l1_get_hk( L1_unit, hk_structure, hk_count, status ) 
C
C This routine copies the Housekeeping (HK) data structure from the Science
C Record associated with unit L1_unit to the caller's argument hk_structure,
C which is a record of type Hkeep.  All HK entries are copied.  The number of
C valid HKs in the structure is returned in the argument hk_count. 
C
C     This version:	13-SEP-1994
C
C     Creation:
C	L. BLeau	12-SEP-1994
C
C     Revisions:
C 1.1	13-SEP-1994	L.Bleau		HK component of L1 record structure was
C					changed to include count of valid HKs;
C					return that count to the caller
C
CDEC$	IDENT   '1.1'

	IMPLICIT NONE

	INCLUDE 'INC_DIR:EDBDEF.INC/NOLIST'
	INCLUDE 'INC_DIR:L1FMT.INC/NOLIST'
	INCLUDE 'INC_DIR:L1READ.INC/NOLIST'

	INTEGER*4 L1_unit, hk_count, status
	RECORD /HKeep/ hk_structure

D	type *,'WIND_L1_GET_HK: L1_unit=',L1_unit
	hk_count = L1_HKeep(L1_unit).HKcount
	hk_structure.CHK = L1_HKeep(L1_unit).CHK
	status = 1

	RETURN
	END
