        SUBROUTINE WIND_OPEN_L1( FIL1NAME, CTYPE, STATUS )
C+
C       This version:   1.10   23-FEB-1995
C
C     REFERENCE:
C
C     USAGE and PARAMETERS:
C       CALL WIND_OPEN_L1( FIL1NAME, CTYPE, STATUS )
C       FIL1NAME  CHAR*(*)	Input string containing the name
C				of the file to be opened.
C       TYPE	  CHAR*3	Input string containing 'NEW' or 'OLD'
C				to indicate what type file to open.
C       STATUS	  INTEGER*4	Output variable containing the completion
C				status of the subroutine's execution.
C
C     CALLS/FUNCTIONS:
C
C     CREATION:
C        6 Feb 1992, S. R. Nylund
C
C     REVISIONS:
C	23-FEB-1994	H.BROER	        adopted from EPIC to WIND
C	 5-APR-1994	H.BROER	        name changed
C	29-JUN-1994	H.BROER	        adapt for L1-file
C	19-JUL-1994	L.Bleau	        added declarations for status variables,
C				        added diagnostic TYPE statement
C	12-SEP-1994	L.Bleau	        added comments
C	28-SEP-1994	L.Bleau	        OPEN with READONLY if CTYPE is 'OLD' or
C					'old' 
C	18-OCT-1994	J.PAQUETTE      added call to WIND_WRITE_L1_HEADER
C	 3-FEB-1995	L. Bleau	removed old commented out code
C	 3-FEB-1995	L. Bleau	create L1 file with large initial
C					allocation, large extend size
C 1.7	 9-FEB-1995     L. Bleau	add common DIAG, check if NRT mode and
C	 				open with smaller allocation if so
C 1.8	 9-FEB-1995     L. Bleau	return proper status to caller on error
C					instead of exiting
C	 9-FEB-1995	L. Bleau	change include stmt to use EDBDEF.INC
C					and EDBVARS.INC instead of EDB.INC only
C 1.9	 9-FEB-1995	L. Bleau	changed FIL1NAME to adjustable length
C 1.10	23-FEB-1995	L. Bleau	removed common DIAG, added INCLUDE
C					DIAG.INC
C-
CDEC$	IDENT '1.10'

      IMPLICIT NONE

      INCLUDE	'INC_DIR:EDBDEF.INC/NOLIST'
      INCLUDE	'INC_DIR:EDBVARS.INC/NOLIST'
      INCLUDE	'INC_DIR:L0_Labl_Record.INC/NOLIST'
      INCLUDE	'INC_DIR:SW_CODES.INC/NOLIST'
      INCLUDE	'INC_DIR:WIND_Lun.INC/NOLIST'		! WIND SYSTEM Logical Unit #s
      INCLUDE   'INC_DIR:DIAG.INC/NOLIST'
      INCLUDE	'($SSDEF)/NOLIST'

      CHARACTER*(*)	FIL1NAME
      CHARACTER*3	CTYPE
      INTEGER*4		STATUS

      INTEGER NBLOCKS,EXTBLOCKS,RECORD_SIZE
      INTEGER FORTERR,RMSSTC,RMSSTV,LUN,VAXERR
C
C   Open old or new file
C
      RECORD_SIZE = 32766		! Maximum record length
      NBLOCKS = 20000			! Initial size for new files
      EXTBLOCKS = 200			! Extend file size for new files
      IF (NRT_MODE) THEN
        NBLOCKS = 2000			! Initial size for new NRT files
        EXTBLOCKS = 50			! Extend file size for new NRT files
      ENDIF
C
C Check cTYPE and do different OPEN command
C
      IF ((CTYPE .EQ. 'OLD') .OR. (CTYPE  .EQ. 'old'))    THEN
        OPEN ( UNIT	= L1_LUN,
     +	       NAME	= FIL1NAME,
     +	       CARRIAGECONTROL='NONE',
     +	       FORM	= 'FORMATTED',
     +	       READONLY,
     +	       STATUS	= CTYPE,
     +	       RECL	= RECORD_SIZE,
     +	       ERR	= 2000,			! Return if error
     +	       IOSTAT	= STATUS)
       ELSE
         OPEN ( UNIT	= L1_Lun,
     +		NAME	= FIL1NAME,
     +		CARRIAGECONTROL='NONE',
     +		FORM	= 'FORMATTED',
     +		STATUS	= cTYPE,
     +		RECL	= Record_Size,
     +          INITIALSIZE = NBLOCKS,
     +          EXTENDSIZE = EXTBLOCKS,
     +		ERR	= 1900,			! Return if error
     +		IOSTAT	= STATUS)
       ENDIF
1200   IF (STATUS .EQ. 0)	STATUS = SS$_NORMAL
C
C If this is a newly-opened file write file header into it
C
      IF ((CTYPE .EQ. 'NEW') .OR. (CTYPE  .EQ. 'new')) THEN
          CALL WIND_WRITE_L1_HEADER(L1_LUN,1)
      ENDIF
C
C Done
C
      RETURN
C
C Come here if opening a new file gets an error.  Assume the error is that VMS
C could not initially allocate sufficient contiguous blocks.  Try another OPEN
C without any initial allocation.  If this, to, fails, then give up.
C
1900     OPEN ( UNIT	= L1_LUN,
     +		NAME	= FIL1NAME,
     +		CARRIAGECONTROL='NONE',
     +		FORM	= 'FORMATTED',
     +		STATUS	= CTYPE,
     +		RECL	= RECORD_SIZE,
     +		ERR	= 2000,			! Return if error
     +		IOSTAT	= STATUS)
      GOTO 1200
C
C We got an error while opening the L1 file.  Translate Fortran error code to
C VMS error code and return it to caller.
C
2000  TYPE 2001, STATUS, FIL1NAME
 2001 FORMAT(' ***ERROR in WIND_OPEN_L1, FORTRAN STATUS =', I6, ' ***'/
     +       ' *** L1 FILENAME = ',A)
      CALL ERRSNS(FORTERR,RMSSTC,RMSSTV,LUN,VAXERR)
C      CALL LIB$SIGNAL(%VAL(RMSSTC))
C      CALL LIB$SIGNAL(%VAL(VAXERR))
      STATUS = RMSSTV
      RETURN
      END
