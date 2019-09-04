	SUBROUTINE WIND_L1_CLOSE( unit, status )
C+
C
C FUNCTIONAL DESCRIPTION: 
C 
C     This routine closes the Level 1 format file specified by the argument
C     `unit'.  Internal data structures associated with this unit are released
C     or cleared, as appropriate.  A status code is returned in the argument
C     `status'.
C 
C FORMAL PARAMETERS:
C  
C     unit:
C           An INTEGER*4 variable specifying the Fortran unit number of the L1
C           file to be closed.  Allowable values for unit are 1-20.
C      
C     status:
C           An INTEGER*4 variable which will receive, upon return, the status
C           of the close operation.  A 1 indicates success, all other values
C           indicate failure.
C 
C COMMON BLOCKS:
C  
C     The common block L1INFO is updated to reflect information about this file.
C  
C DESIGN:
C  
C     After the file is opened successfully it is flagged internally to be open
C     and its science record counter is set to zero.  The first record is then
C     read.  If it is a valid L1 file header it is returned to the caller; if
C     not the file is rewound and all blanks are returned in the header.
C
C
C THIS VERSION:
C	 31-OCT-1994
C
C AUTHOR(S): 
C 
C	 Larry Bleau
C 
C Created: 
C 
C	 31-OCT-1994    L. Bleau
C 
C Revised:
C 
C	 Date     | Name  | Description
C ----------------+-------+-----------------------------------------------------
C
C-
CDEC$	IDENT   '1.0'

	IMPLICIT NONE

	INCLUDE '($SSDEF)/NOLIST'
	INCLUDE 'INC_DIR:EDBDEF.INC/NOLIST'
	INCLUDE 'INC_DIR:L1FMT.INC/NOLIST'
	INCLUDE 'INC_DIR:L1READ.INC/NOLIST'

	INTEGER*4 unit,status

	INTEGER forterr,rmsstc,rmsstv,lun,vaxerr
C
C First check if value of unit is within range
C
	IF (unit .lt. 1 .or. unit .gt. maxunits) THEN
	  status = SS$_IVCHAN
	  RETURN
	ENDIF
C
C Next check if this data structure is active
C If not, return immediately
C If so, proceed with the CLOSE
C
	IF (L1_units(unit) .eq. 0) THEN
	  status = SS$_NORMAL
	  RETURN
	ENDIF
C
C Close the file
C 
	CLOSE (	UNIT	= L1_units(unit),
     +		ERR	= 2000,			! Return if error
     +		IOSTAT	= status)
	IF (status .ne. 0) GOTO 2000
	L1_units(unit) = 0
	scirecnum(unit) = 0
	status = SS$_NORMAL
	RETURN
C
C Come here for errors
C
2000	TYPE 2001, status
2001	FORMAT(' *** ERROR in WIND_L1_CLOSE, Fortran status =', I6, ' ***')
	CALL ERRSNS(forterr,rmsstc,rmsstv,lun,vaxerr)
	CALL LIB$SIGNAL(%VAL(rmsstc))
	CALL LIB$SIGNAL(%VAL(vaxerr))
	status = rmsstc
	RETURN
	END
