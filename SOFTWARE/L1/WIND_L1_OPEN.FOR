	SUBROUTINE WIND_L1_OPEN( unit, filename, header, status )
C+
C 
C FUNCTIONAL DESCRIPTION:
C 
C    This routine opens a Level 1 format file, specified by the argument
C    `filename', onto the unit number given by the argument `unit'.  The file's
C    file header record is then read into the argument `header'.  It is also
C    retained in memory for future reference.  The fact that this unit is
C    opened is recorded internally for future access.  A status code is
C    returned in the argument `status'.
C
C FORMAL PARAMETERS:
C  
C     unit:
C	  An INTEGER*4 variable specifying the Fortran unit number onto which
C	  the L1 file will be opened.
C      
C     filename:
C	  A CHARACTER variable specifying the name of the L1 file to be opened.
C
C     header:
C	  A record variable of structure type ST_L1_FILE_HDR which will receive
C	  the file header of the Level 1 file.
C  
C     status:
C	  An INTEGER*4 variable which will receive, upon return, the status of
C	  the open operation.  A 1 indicates success, all other values indicate
C	  failure.
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
C     Declare WIND$_SUCCESS as EXTERNAL to force the message module to be pulled
C     in from the L1LIB library at link time.
C
C
C This version:   1.7   10-MAR-1995
C
C AUTHOR(S): 
C 
C	 Larry Bleau
C 
C Created: 
C 
C	 10-OCT-1994    L. Bleau
C 
C Revised:
C 
C Ver     Date     | Name  | Description
C -----------------+-------+----------------------------------------------------
C 1.1  19-OCT-1994 |  LRB  | changed header parameter from CHAR*400 to a RECORD
C                          | of type ST_L1_FILE_HDR
C 1.2  10-NOV-1994 |  LRB  | changed comments, added diagnostic TYPE statement
C 1.3  13-DEC-1994 |  LRB  | added test of L1 file version, do STOP if too
C                          | large for us to handle; see WIND_L1_READ_SR for
C                          | additional details
C 1.4  10-JAN-1995 |  LRB  | add code to zero out null_buffer the first time we
C                          | are called; removed DATA statement from L1READ.INC
C 1.5   7-FEB-1995 |  LRB  | recode to allow continued operation even if reading
C			   | later L1 formats, assumming the only change is to
C			   | the headers
C 1.6   2-MAR-1995 |  LRB  | test header level for illegal value an give message
C 1.7  10-MAR-1995 |  LRB  | make use of WIND condition codes for errors
C-
CDEC$	IDENT   '1.7'

	IMPLICIT NONE

	INCLUDE '($SSDEF)/NOLIST'
	INCLUDE '($RMSDEF)/NOLIST'
	INCLUDE 'INC_DIR:EDBDEF.INC/NOLIST'
	INCLUDE 'INC_DIR:L1FMT.INC/NOLIST'
	INCLUDE 'INC_DIR:L1READ.INC/NOLIST'

	CHARACTER*(*) filename
	INTEGER*4 unit,status
        RECORD /ST_L1_FILE_HEADER/ header

	INTEGER*4 record_size,nbytes
	INTEGER i,forterr,rmsstc,rmsstv,lun,vaxerr
	LOGICAL first_time/.TRUE./
	SAVE first_time

	EXTERNAL WIND$_SUCCESS 
	EXTERNAL WIND$_OUTRANGE,WIND$_INUSE,WIND$_EMPTY
C
	record_size = 32766			! Max.length of WIND data record
C
C If this is the first time we're called zero out the array null_i4_array,
C which is equivalenced to null_buffer.  null_buffer is used to zero out the
C user's argument(s) when calling the various WIND_L1_GET routines.
C
	IF (first_time) THEN
	  DO i=1,16384
	    null_i4_array(i) = 0
	  END DO
	  DO i=1,maxunits
	    L1_units(i) = 0
	  END DO
	  first_time = .FALSE.
	END IF
C
C First check if value of unit is within range
C
	IF (unit .lt. 1 .or. unit .gt. maxunits) THEN
	  status = %LOC(WIND$_OUTRANGE)
	  RETURN
	ENDIF
C
C Next check if this data structure is already in use
C If so, return an error immediately
C If not, proceed with the OPEN
C Don't bother checking if this Fortran unit number is already in use
C elsewhere, since the OPEN statement will check this and give a different
C error  
C
	IF (L1_units(unit) .ne. 0) THEN
	  status = %LOC(WIND$_INUSE)
	  RETURN
	ENDIF
C
C Open the file
C Take care of other housekeeping chores
C
D	TYPE *,'OPENING ',filename
	OPEN (	UNIT	= unit,
     +		NAME	= filename,
     +		CARRIAGECONTROL='NONE',
     +		FORM	= 'FORMATTED',
     +		READONLY,
     +		STATUS	= 'OLD',
     +		RECL	= record_size,
     +		ERR	= 2000,			! Return if error
     +		IOSTAT	= status)
	IF (status .ne. 0) GOTO 2000
	L1_units(unit) = unit
D	TYPE *,'L1_units(unit), unit=',L1_units(unit),unit
	scirecnum(unit) = 0
C
C Now read the file header
C File headers start with the characters 'HDRF' and are currently 400 bytes
C long.  If the first record fails this test we have a pre-header format of the
C file, so rewind the file and leave it positioned at the first science record.
C
	READ(unit, 1001, ERR=2000, IOSTAT=status, END=2010) nbytes,buffer
1001	FORMAT( Q, A )
D	TYPE *,'first L1 record length=',nbytes,'; chars 1-4 =',buffer(1:4)
	IF (nbytes .eq. LEN(header.c_all) .and. buffer(1:4) .eq. 'HDRF') THEN
	  header.c_all = buffer(1:LEN(header.c_all))
	  L1_header(unit).c_all = header.c_all
	  IF (L1_header(unit).header_vers .gt. max_version) THEN
            TYPE *,' '
	    TYPE *,'*** WARNING:'
	    TYPE *,'*** The L1 header has a higher version than allowed for in the L1 read'
	    TYPE *,'*** routine.  Some header information will not be available.'
	    TYPE *,'*** Recompile and relink your application if you wish to correct this.'
            TYPE *,' '
c            TYPE *,'L1 file version is too large for this program to handle:'
c            TYPE *,'  Version of L1 file                      = ',L1_header(unit).header_vers
c            TYPE *,'  Highest version this program can handle = ',max_version
c            TYPE *,' '
c            TYPE *,'Recompile and relink your program'
c            STOP
          END IF
	  IF (L1_header(unit).header_vers .lt. '01') THEN
            TYPE *,' '
	    TYPE *,'*** WARNING:'
	    TYPE *,'*** The L1 header level has an invalid value'
	    TYPE *,'*** This most likely indicates an incompletely processed L1 file.'
	    TYPE *,'*** Further attempts to read this file may yield unpredictable results'
            TYPE *,' '
          END IF
C
C If file header version is earlier then 04 fill in missing fields with zeros
C
	  IF (L1_header(unit).header_vers .lt. '04') THEN
	    L1_header(unit).L1_records = '   0'
	    L1_header(unit).EDB_count = '     0'
	    L1_header(unit).EDB_lost = '     0'
	    L1_header(unit).EDB_skip = '     0'
          END IF
	ELSE
	  TYPE *,'*** No L1 file header, REWINDing file, blanking header'
	  TYPE *,'Expected ',LEN(header.c_all),' bytes,  actually read ',nbytes,' bytes'
	  REWIND unit
	  header.c_all = ' '
	  L1_header(unit).c_all = ' '
	END IF
	status = SS$_NORMAL
	RETURN
C
C Come here for errors
C 
C Ordinary error
C
2000	TYPE 2001, status
2001	FORMAT(' *** ERROR in WIND_L1_OPEN, Fortran status =', I6, ' ***')
	CALL ERRSNS(forterr,rmsstc,rmsstv,lun,vaxerr)
	status = rmsstc
	RETURN
C
C End of file error, file is empty
C
2010	TYPE *, ' *** ERROR in WIND_L1_OPEN, end-of-file ***'
	status = %LOC(WIND$_EMPTY)
	RETURN
	END
