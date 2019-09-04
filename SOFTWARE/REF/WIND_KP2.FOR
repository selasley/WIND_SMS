       PROGRAM WIND_KP2
C
C Abstract:
C
C   This program read variables from a WIND key parameter CDF-format file
C   and prints the values out in a text matrix for use with Kaleidagraph.
C
C Design:
C
C   This version of WIND_KP is specially designed for Fred Ipavich and
C   is used to compare SoHO-PM data with WIND-SWE data.
C
C Author:
C
C	Written by Jeff Miller, University of Maryland Space Physics Group
C	on or about June 20, 1995 based on ideas from Dr. John Paquette,
C	Eric Salter, and Larry Bleau, all at University of Maryland and upon
C	the NSSDC CDF (Common Data Format) documentation.
C
C Revision history:
C
C  22-FEB-1996	K.CHOTOO	?
C  22-OCT-2002	LRB	1.2	add call to SIGNON, add IDENT line
C
CDEC$   IDENT '1.2'
        IMPLICIT NONE
	INCLUDE 'CDF$INC:CDF.INC'
	REAL*8 	beg_date, end_date
        REAL*4 interval
	INTEGER i
	LOGICAL*4 what_info(4)
	CHARACTER*40 outfilename(4)
C
C  initialize arrays
C
	call signon
	do i = 1,4
	    what_info(i) = 0
	end do			!i = 1,4		
C
C	Find out what information the user wants
C
	CALL what_data2 (beg_date, end_date, what_info, outfilename, interval)
C
C	Grab data from CDF for that data type and write out to text file
C
	CALL get_kp_data2 (beg_date, end_date, what_info, outfilename, interval)

	END       
