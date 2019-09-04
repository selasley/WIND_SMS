	SUBROUTINE get_orbit_request(mode, beg_date, end_date, what_info, units,
	1 system, interval, form, output_file, nch_out) 
 
C+
C 
C FUNCTIONAL DESCRIPTION:	
C 
C    This subroutine obtains information from the user about what time interval
C    should be dumped, what coordinates or velocities should be dumped, whether
C    any transformations should be performed prior to dumping, and what
C    interval to use when dumping the data.  The user is prompted for this
C    information, which is returned to the caller through the argument list.
C
C    While initially implemented as an interactive prompt this routine may
C    be changed to some other type of interface, such as command line.
C
C FORMAL PARAMETERS:
C  
C
C     MODE:
C     	  INT*4	    An encoded value indicating the mod ein which this
C		    subroutine is to run.  Currently the only mode is an
C		    interactive prompt, so this argument is ignored.
C	  
C     BEG_DATE:
C	  REAL*8    The starting date in Epoch format for which orbit
C		    information should be retrieved.
C      
C     END_DATE:
C	  REAL*8    The ending date in Epoch format for which orbit
C		    information should be retrieved.
C      
C     WHAT_INFO:
C	  LOG*4	    A 7-element logical array.  Each element indicates to
C		    what information the user requires.  The elements, in order,
C		    are: 
C			1   GCI position
C			2   GCI velocity
C			3   GSE position
C			4   GSE velocity
C			5   GSM position
C			6   GSM velocity
C			7   Sun vector
C      
C     UNITS:
C	  INT*4	    An encoded flag indicating what type of unit conversion the
C		    user wants to be performed on the data.  The possible values
C		    are: 
C			0   no conversion (leave in kilometers)
C			1   convert to Earth radii
C      
C     SYSTEM:
C	  INT*4	    An encoded flag indicating if the user wants the coordinates
C		    which are provided transformed into an alternate coordinate
C		    system.  The possible values are:
C			0   no transformation (leave in Cartesian (x,y,z))
C			1   transform to 3-D spherical (r, theta, phi)
C
C     INTERVAL:
C     	  INT*4	    The length of the interval (in array elements) which is used
C		    by the display routine to dump position and velocity values.
C		    The user is prompted in minutes.  Since the data points are
C		    10 minutes apart this is the smallest interal which can be
C		    used.  Intervals are rounded up to the next whole multiple
C		    of 10 minutes then converted to array index increments by
C		    dividing by 10. 
C	  
C      
C     FORM:
C	  INT*4	    A bitmask, where each bit that is set specifies a partiuclar
C		    option.  This parameter is both input and output.
C		    Additional bits are set according to the user's responses to
C		    questions.
C      
C     OUTPUT_FILE:
C	  CHAR*(*)  The name of the user-selected output file is stored here.
C      
C     NCH_OUT:
C	  INT*4	    The number of characters in the filename in OUTPUT_FILE.
C
C COMMON BLOCKS:
C  
C     None
C  
C DESIGN:
C  
C     prompt user for start, end date, convert internally to Epoch format
C     prompt user for GCI, GSE, or GSM coords, or any combination
C     ask if user wants velocities also
C     prompt user for what conversion and/or transformation is wanted, encode
C  
C 
C This version:    8-JUN-1995	  1.1
C 
C Created: 
C 
C	 1-JUN-1995    Larry Bleau
C 
C Revised:
C 
C	 Date     | Name  | Description
C ----------------+-------+-----------------------------------------------------
C  8-JUN-1995     |  LRB  | added form, output_file, and nch_out to argument
C  			    list, construct default output filename, prompt user
C			    for filename, prompt user to use day-of-year or
C			    normal date
C [change_entry]
C 
C-
CDEC$	IDENT '1.1'

	IMPLICIT NONE
	INCLUDE 'WIND_POSITION.INC'
	REAL*8 beg_date, end_date
	INTEGER*4 mode, units, system, interval, form, nch_out
	LOGICAL*4 what_info(7)
	CHARACTER output_file*(*)
C
	INTEGER i, nch, nch2, year1, month1, day1, year2, month2, day2
	CHARACTER answer*80, coord(4)*10, default_name*29
	DATA coord/'GCI       ','GSE       ','GSM       ','Sun_vector'/
C
C Get starting and ending dates
C Convert the dates to Epoch format.  Fix up ending date to just before midnight
C at end of day (23:59:59) so rest of program will use entire day instead of
C stopping at the previous day.
C
100	write(*,1004) 'starting'
1004	format(' Enter ',A,' date (yyyy,mm,dd): ',$)
	read(*,*,end=900,err=100) year1,month1,day1
	call compute_epoch(year1, month1, day1, 0, 0, 0, 0, beg_date)
C
110	write(*,1004) 'ending'
	read(*,*,end=900,err=110) year2,month2,day2
	call compute_epoch(year2, month2, day2, 23, 59, 59, 0, end_date)
C
C Construct default output filename, then prompt user for output file
C
	write(default_name,1008) year1,month1,day1,year2,month2,day2
1008	format('WINDPOS_',I4.4,2I2.2,'_',I4.4,2I2.2,'.DAT')
	write(*,1009) default_name
1009	format(' Enter output filename (Return = ',A,'): ',$)
	read(*,1002) nch,answer
	IF (nch .eq. 0) THEN
	    output_file = default_name
	    nch_out = LEN(default_name)
	ELSE
	    output_file = answer(1:nch)
	    nch_out = nch
	    IF (answer(1:nch) .eq. 'SYS$OUTPUT' .or. answer .eq. 'sys$output') THEN
		output_file = 'SYS$OUTPUT:'
		nch_out = 11
	    ELSE IF (answer .eq. 'TT' .or. answer .eq. 'tt') THEN
		output_file = 'TT:'
		nch_out = 3
	    END IF
	END IF
C
C Prompt user which coordinate systems s/he wants displayed
C Also ask about velocities in each system
C Allow for a default response of No
C
	DO i=1,4
	    nch = INDEX (coord(i),' ') - 1
	    IF (nch .eq. -1) nch = LEN(coord(i))
	    write(*,1001) coord(i)(1:nch)
1001	format(' Do you want to display ',A,' position (Y/N, Return = No) ? ',$)
	    read(*,1002) nch2,answer
1002	format(Q,A)
	    IF (nch2 .eq. 0) answer = 'N'
	    what_info(2*(i-1)+1) = answer(1:1) .eq. 'Y' .or. answer(1:1) .eq. 'y'
	    IF (i .ne. 4) THEN
		write(*,1003) coord(i)(1:nch)
1003	format(' Do you want to display ',A,' velocities (Y/N, Return = No) ? ',$)
		read(*,1002) nch2,answer
		IF (nch2 .eq. 0) answer = 'N'
		what_info(2*(i-1)+2) = answer(1:1) .eq. 'Y' .or. answer(1:1) .eq. 'y'
	    END IF
	END DO
C
C Ask user about conversion from kilometers to other units, such as Earth radii
C
	write(*,1005)
1005	format(' Do you want to convert distances to Earth radii (Y/N,
	1 Return = No) ? ',$)
	read(*,1002) nch,answer
	IF (nch .eq. 0) answer = 'N'
	IF (answer(1:1) .eq. 'Y' .or. answer(1:1) .eq. 'y') THEN
	    units =  1
	ELSE
	    units = 0
	END IF
C
C Ask user about conversion from cartesian coordinates to spherical
C
	write(*,1006)
1006	format(' Do you want to convert to spherical coordinates (Y/N, Return = No) ? ',$)
	read(*,1002) nch,answer
	IF (nch .eq. 0) answer = 'N'
	IF (answer(1:1) .eq. 'Y' .or. answer(1:1) .eq. 'y') THEN
	    system =  1
	ELSE
	    system = 0
	END IF
C
C Ask user for sampling interval to use for dumping the data
C
200	interval = 10
	write(*,1007)
1007	format(' Interval to use to dump positions (minutes; Return = 10 min.): ',$)
	read(*,1002) nch,answer
	IF (nch .gt. 0) THEN
	    read(answer(1:nch),*,end=900,err=200) interval
	END IF
	interval = (max(interval,10) + 9) / 10
C
C Prompt user for what format date to use
C
	write(*,1010)
1010	format(' Program can display date as dd-mmm-yyyy or yyyy nnn.'/
	1 ' Do you want to display using day of year (Y/N, Return = No) ? ',$)
	read(*,1002) nch,answer
	IF (nch .eq. 0) answer = 'N'
	IF (answer(1:1) .eq. 'Y' .or. answer(1:1) .eq. 'y') THEN
	    form =  form .or. POS_FORMAT_DOY
	END IF
C
900	RETURN
	END
