	SUBROUTINE wind_write_l1_header(unit,action)
C+
C 
C FUNCTIONAL DESCRIPTION:	
C 
C    This routine writes out a Level 1 file or Science Record header.
C 
C FORMAL PARAMETERS:
C  
C     unit:
C	  An INTEGER*4 variable specifying the Fortran unit number to write the
C	  header to.
C      
C     action:
C	  An INTEGER*4 variable indicating what action is to be performed.
C	  Its valid values are in the table below.
C
C	  Value  Action                          Length
C	    1	 WRITE file header                  400
C	    2	 WRITE Science Record header        200
C           3    REWRITE file header                400
C  
C COMMON BLOCKS:
C  
C     L1_HEADERS	  Contains the file and Science Record header information
C  
C 
C This version:   1.2   15-DEC-1994
C 
C Created: 
C 
C	 12-OCT-1994    L. Bleau
C 
C Revised:
C 
C	 Date     | Name  | Description
C ----------------+-------+-----------------------------------------------------
C 1.1 14-OCT-1994 |  LRB  | changed field name written to c_all to reference
C                           the entire structure as a CHARACTER variable
C 1.2 15-DEC-1994 |  LRB  | added code to read and rewrite file header to permit
C                           updating header on an existing file, changed some
C                           labels, changed parameter from hdr_type to action
C 
C-
CDEC$   IDENT   '1.2'

	IMPLICIT NONE
	INTEGER*4 unit, action
	INCLUDE 'INC_DIR:L1FMT.INC/NOLIST'
	INCLUDE 'INC_DIR:L0_DATA_RECORD.INC/NOLIST'

	INTEGER iostatus, nbytes
        INTEGER*4 forterr, rmsstc, rmsstv, lun, vaxerr

	IF (action .lt. 1 .or. action .gt. 3) RETURN
	GO TO (100,200,300), action
C
100	WRITE(unit, 1001, ERR=900, IOSTAT=iostatus) l1_file_header.c_all
1001	FORMAT(A)
	RETURN
C
200	WRITE(unit, 1001, ERR=910, IOSTAT=iostatus) l1_sr_header.c_all
	RETURN
C
300     READ(unit, 1002, ERR=920, END=920, IOSTAT=iostatus) nbytes,C_DR_BUFFER(1:nbytes)
1002	FORMAT(Q,A)
	REWRITE(unit, 1001, ERR=930, IOSTAT=iostatus) l1_file_header.c_all
	RETURN
C
900     CALL ERRSNS(forterr,rmsstc,rmsstv,lun,vaxerr)
        iostatus = rmsstc
        CALL LIB$SIGNAL(%val(iostatus))
	RETURN
C
910	TYPE 1003,unit,iostatus
1003	FORMAT(' *** ERROR WRITING L1 HEADER, UNIT=',I3,', STATUS=',I4)
	GOTO 900
C
920	TYPE 1004,unit,iostatus
1004	FORMAT(' *** ERROR READING L1 HEADER, UNIT=',I3,', STATUS=',I4)
	GOTO 900
C
930	TYPE 1005,unit,iostatus
1005	FORMAT(' *** ERROR REWRITING L1 HEADER, UNIT=',I3,', STATUS=',I4)
	GOTO 900
	END
