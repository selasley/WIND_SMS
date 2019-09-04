	SUBROUTINE SIGNON
C+
C
C FUNCTIONAL DESCRIPTION:
C
C     This subroutine writes a header line to the SYS$OUTPUT output stream. 
C
C     The line consists of the currently executing program, the program
C     version, and the current date and time.
C
C     This subroutine can work with any program, as it obtains the program name
C     and its version are obtained from the currently executing image file.  
C
C FORMAL ARGUMENTS:
C
C     None
C
C INTERFACE:
C
!	INTERFACE
!	  SUBROUTINE SIGNON
!	  END SUBROUTINE SIGNON
!	END INTERFACE
C
C COMMON BLOCKS:
C
C     None
C
C INCLUDE FILES:
C
C     None
C
C IMPLICIT INPUTS:
C
C     current date and time
C     name of program being run (obtained from operating system)
C     image header information from program being run
C
C IMPLICIT OUTPUTS:
C
C     None
C
C SIDE EFFECTS:
C
C     One line is written to the standard output stream.
C
C DESIGN:
C
C     Use routine GET_IMAGE_INFO to obtain program name and its identification
C     string (version).  Construct output line using current date and time,
C     program name, and program version.
C
C     We want to display the output line for the user and not have it
C     redirected, as Fortran units can be.  We also can't depend on a Fortran
C     unit having been opened to the output file this early in the program's
C     execution.  To satisfy these requirements use PRINT statement to
C     send a line to the output stream (SYS$OUTPUT).
C
C CREATED:
C
C     4-FEB-1997   Lawrence Bleau
C
C This version:   1.4   19-MAY-2000
C
C DEVELOPMENT HISTORY:
C
C L. Bleau	12-MAR-1999 1.1	change calls to DATE, TIME to DATE_AND_TIME;
C				replace TYPE with PRINT; add MONTHSTR array
C				and conversion logic; change output line written
C				to use 4-digit year
C L. Bleau	15-APR-1999	added comments
C L. Bleau	29-APR-1999	change from Fortran PRINT to LIB$PUT_OUTPUT
C L. Bleau	29-APR-1999 1.3	change from LIB$PUT_OUTPUT back to PRINT to
C				preserve Fortran carriage control attribute
C L. Bleau	 8-NOV-1999	added comments
C L. Bleau	19-MAY-2000 1.4	handle case of no version (ident)
C-
CDEC$	IDENT	'1.4'
C
	IMPLICIT NONE
	INTEGER NCH, NCH1, NCH2, VALUES(8)
	CHARACTER CURR_DATE*8, CURR_TIME*10, NEW_DATE*11, VERSION*8, PROGRAM*12
	CHARACTER ZONE*5, MONTHSTR(12)*3, LINE*132
	DATA MONTHSTR  /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP',
	1		'OCT','NOV','DEC'/
C
        CALL DATE_AND_TIME(CURR_DATE, CURR_TIME, ZONE, VALUES)
	WRITE(NEW_DATE,1001) CURR_DATE(7:8),MONTHSTR(VALUES(2)),CURR_DATE(1:4)
1001	FORMAT(A2,'-',A3,'-',A4)
	VERSION = ' '
	CALL GET_IMAGE_INFO('VERS', VERSION, NCH1)
	CALL GET_IMAGE_INFO('PROG', PROGRAM, NCH2)
	IF (NCH1 .GT. 1 .OR. VERSION(1:1) .NE. ' ') THEN
          WRITE(LINE,1002) PROGRAM(1:NCH2),VERSION(1:NCH1),NEW_DATE,
	1		   CURR_TIME(1:2),CURR_TIME(3:4),CURR_TIME(5:6)
1002	  FORMAT(A,'  V',A,'  ',A11,1X,A2,':',A2,':',A2)
	  NCH = NCH1 + NCH2 + 25
	ELSE
          WRITE(LINE,1003) PROGRAM(1:NCH2),NEW_DATE,
	1		   CURR_TIME(1:2),CURR_TIME(3:4),CURR_TIME(5:6)
1003	  FORMAT(A,'  ',A11,1X,A2,':',A2,':',A2)
	  NCH = NCH2 + 22
	ENDIF
	PRINT *,LINE(1:NCH)
	RETURN
	END
