	PROGRAM WIND_POSITION
 
C+
C 
C ABSTRACT: 
C 
C	 This program reports, in tabluar form, the position of the WIND
C	 satellite for a given range of dates.
C 
C COMMON BLOCKS:
C  
C     EPHDATA
C  
C DESIGN:
C  
C     initialize the program
C     obtain the orbit request parameters from the user
C     set current day to starting date
C     loop:
C     	  read in the orbit data for the current day into our arrays
C     	  process the data as needed
C     	  display the data for the user
C	  advance to next day
C     endloop
C  
C 
C Current version: 
C 
C	 22-JUN-1995	2.0
C 
C Created: 
C 
C	 2-JUN-1995    Larry Bleau
C 
C Revised:
C 
C	 Date     | Name  | Description
C ----------------+-------+-----------------------------------------------------
C  8-JUN-1995	  |  LRB  | increased size of array to 10000, moved OPEN of
C			    output file to get_orbit_request
C 22-JUN-1995     |  LRB  | add loop to read data once per day and display it
C 			    instead of doing all reads first; add routine
C			    get_orbit_data
C [change_entry]
C-
CDEC$	IDENT '2.0'
	IMPLICIT NONE
	INCLUDE 'INC_DIR:EPHDEF.INC'
	INCLUDE 'WIND_POSITION.INC'
	INTEGER maxsize
	PARAMETER (maxsize=144)
	INTEGER*4 mode, units, system, step, npoints, dump_unit/7/, status,
	1 form, nch
	CHARACTER*80 outfile
	LOGICAL*4 what_info(7)
	REAL*8 beg_date, end_date, todays_date, epoch_time(maxsize), dummy(maxsize)
	RECORD /EPHEMERIS/ gci_pos(maxsize),
	1 gci_vel(maxsize), gse_pos(maxsize), gse_vel(maxsize),
	1 gsm_pos(maxsize), gsm_vel(maxsize), sun_vec(maxsize) 
C
C Initialize the program
C
	mode = 1
	form = POS_FORMAT_3UP
C
C Obtain the orbit request parameters from the user
C
	call get_orbit_request(mode, beg_date, end_date, what_info, units,
	1		       system, step, form, outfile, nch) 
C
C Open output file
C
	IF (nch .eq. 0 .or. outfile .eq. ' ') THEN
	    OPEN (
	1     UNIT = dump_unit,
	1     FILE = 'SYS$OUTPUT:',
	1     CARRIAGECONTROL = 'LIST',
	1     STATUS = 'OLD',
	1     IOSTAT = status,
	1     RECL = 500,
	1     ERR = 900)
	ELSE
	    OPEN (
	1     UNIT = dump_unit,
	1     FILE = outfile(1:nch),
	1     CARRIAGECONTROL = 'LIST',
	1     STATUS = 'NEW',
	1     IOSTAT = status,
	1     RECL = 500,
	1     ERR = 900)
	END IF
C
C Start daily loop
C Read in the orbit data for the current day into our arrays
C Update starting and ending date/time with actual values
C
	todays_date = beg_date
	DO WHILE (todays_date .le. end_date)
	    call get_orbit_data(todays_date, what_info, maxsize, npoints,
	1			epoch_time, gci_pos, gci_vel, gse_pos, gse_vel,
	1			gsm_pos, gsm_vel, sun_vec)
	    IF (npoints .eq. 0) goto 400
C
C Process the data as needed
C
	    if (what_info(1)) call convert_orbit_info(units, system, npoints,
	1			gci_pos, dummy)
	    if (what_info(2)) call convert_orbit_info(units, system, npoints,
	1			gci_vel, dummy)
	    if (what_info(3)) call convert_orbit_info(units, system, npoints,
	1			gse_pos, dummy)
	    if (what_info(4)) call convert_orbit_info(units, system, npoints,
	1			gse_vel, dummy)
	    if (what_info(5)) call convert_orbit_info(units, system, npoints,
	1			gsm_pos, dummy)
	    if (what_info(6)) call convert_orbit_info(units, system, npoints,
	1			gsm_vel, dummy)
	    if (what_info(7)) call convert_orbit_info(units, system, npoints,
	1			sun_vec, dummy)
C
C Display the data for the user
C
	    call display_orbit_info(dump_unit, beg_date, end_date, what_info,
	1			 form, units, system, step, npoints, epoch_time,
	1			 gci_pos, gci_vel, gse_pos, gse_vel, gsm_pos,
	1			 gsm_vel, sun_vec)
C
C End of daily loop
C Bump date to next day so a different CDF file gets accessed
C Change form so header doesn't get repeated
C
400	    todays_date = todays_date + 24.*60.*60.*1000.
	    form = form .or. POS_FORMAT_SUPPRESS_HEADER
	END DO
C
910	CLOSE (UNIT=dump_unit, IOSTAT=status, ERR=920)
920	call exit(1)
C
900	TYPE *, 'Error opening dump file; status =',status
	END
