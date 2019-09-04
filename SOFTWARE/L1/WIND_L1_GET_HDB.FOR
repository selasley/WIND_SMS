	SUBROUTINE wind_l1_get_hdb( L1_unit, hdb_structure, status ) 
C
C This routine copies the Housekeeping Data Block (HDB) data structure from the
C Science Record associated with unit L1_unit to the caller's argument
C hdb_structure, which is a record of type HDBstr.  
C
C     This version:	20-SEP-1994
C
C     Creation:
C	L. BLeau	20-SEP-1994
C
C     Revisions:
C
CDEC$	IDENT   '1.0'

	IMPLICIT NONE

	INCLUDE 'INC_DIR:EDBDEF.INC/NOLIST'
	INCLUDE 'INC_DIR:L1FMT.INC/NOLIST'
	INCLUDE 'INC_DIR:L1READ.INC/NOLIST'

	INTEGER*4 L1_unit, status
	RECORD /HDBstr/ hdb_structure

D	type *,'WIND_L1_GET_HDB: L1_unit=',L1_unit
	hdb_structure.CHDB = L1_HDB(L1_unit).CHDB
	status = 1

	RETURN
	END
