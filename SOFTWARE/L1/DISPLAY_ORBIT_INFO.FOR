	SUBROUTINE display_orbit_info(d_unit, beg_date, end_date, what_info,
	1 form, units, system, interval, npoints, epoch_time, gci_pos, gci_vel,
	1 gse_pos, gse_vel, gsm_pos, gsm_vel, sun_vec) 
 
C+
C 
C FUNCTIONAL DESCRIPTION:	
C 
C    This subroutine displays the requested coordinate and velocity information
C    in a form easily usable by a spreadsheet program.  The display is prefaced
C    by a heading indicating the range of dates in the dump and the values being
C    displayed.
C 
C FORMAL PARAMETERS:
C  
C     D_UNIT:
C	  INT*4	    The Fortran unit number where the formatted output is to be
C		    written.  This allows the output to be directed to either
C		    the terminal (unit 6) or a file or network link.
C      
C     BEG_DATE:
C	  REAL*8    The date/time, in Epoch format, of the first data point to
C		    be dumped.  This will be displayed as the starting date.
C      
C     END_DATE:
C	  REAL*8    The date/time, in Epoch format, of the last data point to
C		    be dumped.  This will be displayed as the ending date.
C      
C     WHAT_POS:
C	  LOG*4	    A 7-element logical array.  Each element indicates to
C		    DISPLAY_ORBIT_INFO what information is to be formatted and
C		    output.  The elements, in order, are:
C			1   GCI position
C			2   GCI velocity
C			3   GSE position
C			4   GSE velocity
C			5   GSM position
C			6   GSM velocity
C			7   Sun vector
C		    If a particular element is .TRUE. the corresponding position
C		    (or velocity) information is accessed from the argument list
C		    and is written as a separate column in the output.
C
C     FORM:
C     	  INT*4	    A bit-encoded value specifying the output format to use.
C		    The meaning of the bits are given in the file
C		    WIND_POSITION.INC and are also defined below:
C			Bit	Meaning
C			 0	if 1 use tabs to separate columns; if 0 use
C				spaces
C			 1	if 1 suppress display of Epoch time as
C				dd-mmm-yyyy hh:mm:ss; if 0 display in this
C				format 
C			 2	if 1 display Epoch time as a REAL*8 using F15.0;
C				if 0 suppress display in this format
C			 3	if 1 suppress display of header; if 0 display
C				header
C			 4	if 1 display Epoch time on each of the three
C				lines for each triple; if 0 display it only on
C				first line 
C			 5	if 1 display three data values of each entity on
C				the same line; if 0 display the values on three
C				separate lines
C			 6	if 1 display date using day-of-year in yyyy nnn
C				format; if 0 use month-day format; ignored if
C				bit 1 is 1 (suppress date display)
C			 7-31	reserved
C      
C     UNITS:
C	  INT*4	    An encoded value indicating whether or not the units are to
C		    be changed and to what.  The allowable values are:
C			0	no change in units
C			1	Earth radii
C      
C     SYSTEM:
C	  INT*4	    An encoded value indicating whether or not the coordinate
C		    system is to be changed to spherical.  The allowable values are:
C			0	no change to coordinate system
C			1	change to spherical coordinates (r,theta,phi)
C
C     INTERVAL:
C     	  INT*4	    The length of the interval (in array elements) which is to
C		    be used to the display position and velocity values.  If 2
C		    every other element is displayed, if 3 every 3rd, etc.
C	  
C     NPOINTS:
C	  INT*4	    The size of the arrays arrays EPOCH_TIME, GCI_POS, GCI_VEL,
C		    GSE_POS, GSE_POS, GSM_POS, GSM_POS, and SUN_VEC. 
C
C     EPOCH_TIME:
C	  REAL*8    An array of NPOINTS elements containing Epoch times.  Each
C		    element of EPOCH_TIME is associated with the corresponding
C		    position and velocity value(s) stored in the remaining
C		    arguments.
C	
C     GCI_POS:
C	  /EPHEMERIS/	An array of NPOINTS RECORDs of structure type /EPHEMERIS/.
C		    This contains positions in GCI coordinates which were
C		    retrieved from the CDF data file.
C      
C     GCI_VEL:
C	  /EPHEMERIS/	An array of NPOINTS RECORDs of structure type /EPHEMERIS/.
C		    This contains velocities in GCI coordinates which were
C		    retrieved from the CDF data file.
C      
C     GSE_POS:
C	  /EPHEMERIS/	An array of NPOINTS RECORDs of structure type /EPHEMERIS/.
C		    This contains positions in GSE coordinates which were
C		    retrieved from the CDF data file.
C      
C     GSE_VEL:
C	  /EPHEMERIS/	An array of NPOINTS RECORDs of structure type /EPHEMERIS/.
C		    This contains velocities in GSE coordinates which were
C		    retrieved from the CDF data file.
C      
C     GSM_POS:
C	  /EPHEMERIS/	An array of NPOINTS RECORDs of structure type /EPHEMERIS/.
C		    This contains positions in GSM coordinates which were
C		    retrieved from the CDF data file.
C      
C     GSM_VEL:
C	  /EPHEMERIS/	An array of NPOINTS RECORDs of structure type /EPHEMERIS/.
C		    This contains velocities in GSM coordinates which were
C		    retrieved from the CDF data file.
C      
C     SUN_VEC:
C	  /EPHEMERIS/	An array of NPOINTS RECORDs of structure type /EPHEMERIS/.
C		    This contains positions in GCI sun vector coordinates which
C		    were retrieved from the CDF data file.
C  
C COMMON BLOCKS:
C  
C     None
C  
C DESIGN:
C  
C     format and output heading
C     loop through arrays
C     	  build output line internally
C	  write constructed output line to output unit
C
C     The line is built internally to allow us greater control over its format.
C     If bit 0 of the argument FORM is 1 tab characters (hex 9) will be
C     inserted between the output columns to facilitate loading into a
C     spreadsheet program; if bit 0 is 0 spaces will be inserted.  The formats
C     will also be changed depending on the units and coordinate system used to
C     make viewing easier.
C
C     Since the eventual receiving device may  not be one which understands
C     Fortran carriage controls this program does not use them.  It assumes the
C     unit D_UNIT was opened without carriage control (CARRIAGECONTROL='NONE')
C     or with listing carriage control (CARRIAGECONTROL='LIST').  None of the
C     format statements have a leading space (1X format) or top-of-form ('1').
C
C 
C This version:    7-JUN-1995	  1.3
C 
C Created: 
C 
C	 2-JUN-1995    Larry Bleau
C 
C Revised:
C 
C	 Date     | Name  | Description
C ----------------+-------+-----------------------------------------------------
C  5-JUN-1995     |  LRB  | added option of displaying all values of each triple
C  			    on the same line
C  6-JUN-1995     |  LRB  | fixed up header to center it
C  7-JUN-1995     |  LRB  | {tbs}
C [change_entry]
C 
C-
CDEC$	IDENT '1.3'

	IMPLICIT NONE
	INCLUDE 'INC_DIR:EPHDEF.INC'
	INCLUDE 'CDF$INC:CDF.INC/NOLIST'
	INTEGER*4 d_unit, npoints, form, units, system, interval
	REAL*8 beg_date, end_date, epoch_time(npoints)
	LOGICAL*4 what_info(7)
	RECORD /EPHEMERIS/ gci_pos(npoints), gci_vel(npoints), gse_pos(npoints),
	1 gse_vel(npoints), gsm_pos(npoints), gsm_vel(npoints), sun_vec(npoints)
C
	INTEGER i,j,ii/1/,table_width,pos,prefixlen,whicharr,linelen
	INTEGER width,width_list(3,2,2,2)
	INTEGER year,month,day,hour,minute,second,msec,ndays
	REAL*8 epoch_temp
	CHARACTER*500 line,lines(3)
	CHARACTER*7 format_list(3,2,2,2),format_to_use(3)
	CHARACTER*(EPOCH_STRING_LEN) date1,date2
	CHARACTER heading1(7)*8,heading2(7)*8,prefix*42
	CHARACTER*30 title1/'WIND Satellite Position Report'/
	CHARACTER*14 title2/'For the Period'/
	CHARACTER symbol1(3,2)*5,symbol2(2)*4
	LOGICAL use_tab,easy_date,float_epoch,display_header,time_first_only
	LOGICAL separate_lines,use_doy
	STATIC ii
	EQUIVALENCE (line,lines(1))
C
	DATA format_list/3*'(F11.1)',3*'(F07.3)',3*'(F11.3)',3*'(E10.3)',
	1		 '(F11.1)',2*'(F09.3)','(F09.3)',2*'(F09.3)',
	1		 '(F11.3)',2*'(F09.3)','(E10.3)',2*'(F09.3)'/
	DATA width_list/3*11,3*7,3*11,3*10, 11,9,9, 9,9,9, 11,9,9, 10,9,9/
	DATA heading1/'   GCI  ','   GCI  ','   GSE  ','   GSE  ',
	1	      '   GSM  ','   GSM  ',' GCI Sun'/
	DATA heading2/'Position','Velocity','Position','Velocity',
	1	      'Position','Velocity',' Vector '/
	DATA symbol1/'  X  ','  Y  ','  Z  ','  r  ','theta',' phi '/
	DATA symbol2/'(km)','(Re)'/
C
C Initialize some stuff
C 
	use_tab = BTEST (form, 0)
	easy_date = .not. BTEST (form, 1)
	float_epoch = BTEST (form, 2)
	display_header = .not. BTEST (form, 3)
	time_first_only = .not. BTEST (form, 4)
	separate_lines = .not. BTEST (form, 5)
	use_doy = BTEST ( form, 6)
C
C Compute eventual width of output table.
C Count the number of columns occupied by the date/time string, Epoch time, or
C both.
C
	table_width = 1
C Check if we're to use normal, easy-to-read, date; if so, count its width.
C Allow for either day-month-year or year-dayofyear format.
	IF (easy_date) THEN
	    IF (use_doy) THEN
		table_width = table_width + 18 + 2
	    ELSE
		table_width = table_width + 20 + 2
	    END IF
	END IF
C Check if we're to use floating point version of Epoch time; if so, count its
C width
	IF (float_epoch) table_width = table_width + 15 + 2
C
C Leave 2 columns spacing between end of time and first data value
C Also remember where we are for use later on
C
	table_width = table_width + 2
	prefixlen = table_width - 1
C
C Now step through the array what_info.  For each element that is .TRUE.
C add its corresponding field width to table_width.  For now this code assumes
C each component of an entity (x,y,z) will occupy the same number of columns.
C
	DO whicharr=1,7
	    IF (what_info(whicharr)) THEN
		width = width_list(1,mod(whicharr-1,2)+1,units+1,system+1)
		IF (separate_lines) THEN
		    table_width = table_width + width + 3
		ELSE
		    table_width = table_width + 3 * (width + 3)
		END IF
	    END IF
	END DO
C Adjust for blanks after last field
	table_width = table_width - 3
C
C Display title unless disabled
C Make sure they are all centered
C
	IF (display_header) THEN
C
C Center and print first title line, which is report/program name
C
	    write(d_unit,1001,err=900)
	    line = ' '
	    i = (table_width - LEN(title1)) / 2
	    line(i+1:) = title1
	    write(d_unit,1002,err=900) line(1:LEN(title1)+i)
C
C Center and print second title line
C
	    line = ' '
	    i = (table_width - LEN(title2)) / 2
	    line(i+1:) = title2
	    write(d_unit,1002,err=900) line(1:LEN(title2)+i)
C
C Construct third title line, which gives starting and ending date/times of
C interval.
C We'll only use 17 chars from each date/time (the date part plus the hours and
C minutes), plus the ' to ' connector, for a total length of 38.
C
	    call encode_epoch(beg_date, date1)
	    call encode_epoch(end_date, date2)
C
	    line = ' '
	    i = (table_width - 38) / 2
	    line(i+1:) = date1(1:17) // ' to ' // date2(1:17)
	    write(d_unit,1002,err=900) line(1:i+38)
C
C Construct fourth title, which gives sampling interval used
C
	    line = ' '
	    width = INT(ALOG10(FLOAT(interval*10))) + 1
	    i = (table_width - 32 - width) / 2
	    write(line(i+1:i+width+32),1006,err=900) interval*10
	    write(d_unit,1002,err=900) line(1:i+32+width)
C
C Output blank line as separator
C
	    write(d_unit,1001,err=900)
C
	    pos = 1
	    lines(1) = ' '
	    lines(2) = ' '
C
C Insert proper heading if Epoch time is to be displayed in normal date/time
C format.  If user wants day-of-year format use fewer columns.  Leave 2 blanks
C after field end in either case.
C nn-mmm-yyyy hh:mm:ss
C yyyy nnn  hh:mm:ss
C
	    IF (easy_date) THEN
		IF (use_doy) THEN
		    lines(1)(pos:pos+18-1) = ' '
		    lines(2)(pos:pos+18-1) = 'Year / DOY / Time'
		    pos = pos + 18 + 2
		ELSE
		    lines(1)(pos:pos+20-1) = ' '
		    lines(2)(pos:pos+20-1) = '   Date  /  Time    '
		    pos = pos + 20 + 2
		END IF
	    END IF
C
C Insert proper heading if Epoch time is to be displayed as a REAL*8 in F15.0.
C Leave 2 blanks after field end.
C
	    IF (float_epoch) THEN
		lines(1)(pos:pos+15-1) = '   Date/Time   '
		lines(2)(pos:pos+15-1) = '    (Epoch)    '
		pos = pos + 15 + 2
	    END IF
C Advance another 2 columns to leave plenty of room
	    pos = pos + 2
C
C Step through the array what_info.  For each element that is .TRUE. build
C another part of the header line with the appropriate quantity name.
C
	    DO 200 whicharr=1,7
		IF (what_info(whicharr)) THEN
		    width = width_list(1,mod(whicharr-1,2)+1,units+1,system+1)
		    i = (width - LEN(heading1(whicharr))) / 2 + 2
		    IF (separate_lines) THEN
			lines(1)(pos+i-1:) = heading1(whicharr)
			lines(2)(pos+i-1:) = heading2(whicharr)
			pos = pos + width + 3
		    ELSE
			j = LEN(heading1(whicharr)) + LEN(heading2(whicharr))
			i = (3*(width+3) - j - LEN(symbol2(units+1)) - 2) / 2 + 2
			lines(1)(pos+i-1:) = heading1(whicharr) //
	1		    heading2(whicharr) // '  ' // symbol2(units+1)
			i = (width - 1) / 2
			DO j=1,3
			    lines(2)(pos+i-1:) = symbol1(j,system+1)
			    pos = pos + width + 3
			END DO
		    END IF
		END IF
200	    END DO
C
C The heading lines are now built; write them out.
C
	    write(d_unit,1002,err=900) lines(1)(1:pos-1)
	    write(d_unit,1002,err=900) lines(2)(1:pos-1)
	    write(d_unit,1001,err=900)
	END IF
C
C Loop through data points
C Use argument interval as the increment in the loop index so we'll display only
C every interval'th set of values
C The variable pos is the character index in the line variable (or lines array)
C where the next character is to be stored; pos-1 is the number of characters to
C be output.
C Build the prefix line only once per Epoch time (set of data points).  The
C prefix contains one, or both, formats of the Epoch time.  It is prefixlen
C chars long.  When riting out lines write out prefix first concatenated with
C lines(i) *on the same line*.  If (x,y,z) triples are to be displayed on
C separate lines and Epoch times are to be displayed only once per triple, blank
C out prefix after the first write.
C

	DO 400 WHILE (ii .le. npoints)
	    prefix = ' '
	    pos = 1
C Check if we're to use normal, easy-to-read, date; if so, insert it
	    IF (easy_date) THEN
		IF (use_doy) THEN
		    call epoch_breakdown(epoch_time(ii), year, month, day, hour,
	1				 minute, second, msec)
		    call compute_epoch(year, 0, 1, 0, 0, 0, 0, epoch_temp)
		    ndays = INT((epoch_time(ii) - epoch_temp) /
	1			(24. * 60. * 60. * 1000.))
	1		    + 1
		    write(prefix(pos:pos+18-1),1005) year,ndays,hour,minute,second
		    pos = pos + 18 + 2
		ELSE
		    call encode_epoch(epoch_time(ii), date1)
		    prefix(pos:pos+20-1) = date1(1:20)
		    pos = pos + 20 + 2
		END IF
	    END IF
C Check if we're to use floating point version of Epoch time; if so, insert it
	    IF (float_epoch) THEN
		write(prefix(pos:pos+15-1),1003,err=900) epoch_time(ii)
		pos = pos + 15 + 2
	    END IF
C Initialize lines that will hold tables of output
	    pos = 1
	    lines(1) = ' '
	    lines(2) = ' '
	    lines(3) = ' '
C
C Loop through the array what_info, testing each element.  If it is .TRUE. get
C the corresponding value from the proper array.  Pass this record (which passes
C all three values) to the routine insert_in_line, along with the correct format
C to use and where to insert it in the line.  Upon return lines(1)..lines(3)
C have been updated with the proper values.
C
	    DO 300 whicharr=1,7
		IF (what_info(whicharr)) THEN
		    DO i=1,3
			format_to_use(i) =
	1		  format_list(i,mod(whicharr-1,2)+1,units+1,system+1)
		    END DO
		    width = width_list(1,mod(whicharr-1,2)+1,units+1,system+1)
		    GO TO (210,220,230,240,250,260,270), whicharr
C
210		    call insert_in_line(gci_pos(ii),format_to_use,width,use_tab,
	1				separate_lines,pos,lines)
		    goto 280
C
220		    call insert_in_line(gci_vel(ii),format_to_use,width,use_tab,
	1				separate_lines,pos,lines)
		    goto 280
C
230		    call insert_in_line(gse_pos(ii),format_to_use,width,use_tab,
	1				separate_lines,pos,lines)
		    goto 280
C
240		    call insert_in_line(gse_vel(ii),format_to_use,width,use_tab,
	1				separate_lines,pos,lines)
		    goto 280
C
250		    call insert_in_line(gsm_pos(ii),format_to_use,width,use_tab,
	1				separate_lines,pos,lines)
		    goto 280
C
260		    call insert_in_line(gsm_vel(ii),format_to_use,width,use_tab,
	1				separate_lines,pos,lines)
		    goto 280
C
270		    call insert_in_line(sun_vec(ii),format_to_use,width,use_tab,
	1				separate_lines,pos,lines)
		    goto 280
C
280		    CONTINUE
		END IF
300	    END DO
C
C At this point lines(1..3) have been filled with formatted values for whatever
C the user wanted displayed.  If separate_lines is .false. only lines(1) has
C been filled.  In either case it is time to write them/it out.  pos-1 is the
C number of characters inserted in each line.  This includes the trailing <tab>
C or trailing spaces, however, which were inserted by insert_in_line, so
C shorten it appropriately.
C
	    IF (use_tab) THEN
		linelen = pos - 2
	    ELSE
		linelen = pos - 4
	    END IF
	    write(d_unit,1004,err=900) prefix(1:prefixlen),lines(1)(1:linelen)
C
C If separate_lines is .true. then also write out lines(2) and lines(3).
C If Epoch time is to be displayed only on first line of the three lines blank
C out the prefix before doing the writes.
C
	    IF (separate_lines) THEN
		if (time_first_only) prefix = ' '
		write(d_unit,1004,err=900) prefix(1:prefixlen),lines(2)(1:linelen)
		write(d_unit,1004,err=900) prefix(1:prefixlen),lines(3)(1:linelen)
	    END IF
	    ii = ii + interval
400	END DO
C
C Decrease pointer ii by npoints so the next time this routine is entered the
C sampling interval's phase is maintained.  Using the previous method
C (controlled by a simple DO loop) would result in the first value always being
C displayed.
C
	ii = ii - npoints
C
900	RETURN
C
C Here are the format statements
C
1001	format()
1002	format(A)
1003	format(F15.0)
1004	format(A,A)
1005	format(I4,1X,I3,2X,I2,':',I2.2,':',I2.2)
1006	format('Sampling Interval Used: ',I<width>,' Minutes')
	END

	SUBROUTINE insert_in_line(info, format_to_use, width, use_tab,
	1 separate_lines, pos, lines)
 
C+
C 
C FUNCTIONAL DESCRIPTION:	
C 
C    This subroutine accepts a floating point value and formats it into a
C    string.  Using various other parameters to control string formatting, it
C    then stores the formatted string into one of the parameters and keeps track
C    of the total number of characters stored.
C 
C FORMAL PARAMETERS:
C  
C     INFO:
C	  REAL*8    An array of length 3 containing the value which is to be
C		    formatted.  Each entity to be formatted actually has three
C		    components.
C      
C     FORMAT_TO_USE:
C	  CHAR*(*)  A CHARACTER array with three elements containing the Fortran
C		    format string to use in the conversion of INFO.  Each
C		    element of FORMAT_TO_USE corresponds to one of the elements
C		    in INFO.
C      
C     WIDTH:
C	  INT*4	    The number of character positions required by the format
C		    FORMAT_TO_USE.
C      
C     USE_TAB:
C	  LOG*4	    A flag controlling the field separation character.  If
C		    .TRUE. an Ascii horizontal tab character will be inserted 
C		    after each field; if .FALSE. 3 spaces will be inserted.
C      
C     SEPARATE_LINES:
C	  LOG*4	    A flag controller the placement of the 3 values on multiple
C		    lines.  If .TRUE. the values will be placed in the same
C		    columns but on separate lines; if .FALSE. the values will
C		    all be placed in diffrent columns on the same line.
C      
C     POS:
C	  INT*4	    A integer giving the starting character position of this
C		    field within the LINES parameter.  When the formatted value
C		    is stored its first character is stored at position POS
C		    within LINES.  POS is updated after the field and its
C		    trailing separator are inserted, so this variable is both
C		    input and output.
C      
C     LINES:
C	  CHAR*(*)  A CHARACTER array with 3 elements.  The formatted values of
C		    INFO are stored into LINES starting at character position
C		    POS.  If SEPARATE_LINES is .TRUE. the three values of INFO
C		    are stored into LINES(1), LINES(2), LINES(3); if it is
C		    .FALSE. the values are all stored into LINES(1) in
C		    successive positions.
C  
C COMMON BLOCKS:
C  
C     None
C  
C DESIGN:
C  
C     {tbs}
C  
C 
C This version:    5-JUN-1995    1.0
C
C Created: 
C 
C	 2-JUN-1995    Larry Bleau
C 
C Revised:
C 
C	 Date     | Name  | Description
C ----------------+-------+-----------------------------------------------------
C  5-JUN-1995     |  LRB  | added comments
C 
C-
	IMPLICIT NONE
	INCLUDE 'INC_DIR:EPHDEF.INC'
	INTEGER*4 pos, width
	LOGICAL*4 use_tab, separate_lines
	CHARACTER lines(3)*500, format_to_use(3)*(*)
	RECORD /EPHEMERIS/ info
C
	INTEGER i,next_pos
	LOGICAL insert_units/.TRUE./
	CHARACTER*20 stars/'********************'/
C 
C To make life easy use the structure member "member", which is a REAL*8 array
C of length 3.  This effectively allows the (x,y,z) (or the (d,theta,phi))
C triple to be accessed serially as an array.
C
	IF (separate_lines) GOTO 200
C
C This code segment gets executed if the three values are to be placed all on
C the same line
C
	DO 120 i=1,3
	    write(lines(1)(pos:pos+width-1),format_to_use(i),err=910) info.member(i) 
	    GOTO 110
C
910	    type *,'internal format error, pos=',pos,', fmt=',format_to_use(i),', info='
	    type *,'i=',i,', datum=',info.member(i)
	    lines(1)(pos:pos+width-1) = stars(1:width)
C
110	    IF (use_tab) THEN
		lines(1)(pos+width:pos+width) = CHAR(9)
		pos = pos + width + 1	    ! advance pointer to just beyond the
					    ! <tab> we inserted
	    ELSE
		pos = pos + width + 2	    ! advance pointer to leave 2 spaces
					    ! between fields
	    END IF
120	END DO
	IF (.not. use_tab) pos = pos + 1    ! leave additional space between
					    ! triples
	RETURN
C
C This code segment gets executed if the three values are to be placed on
C separate lines
C
200	DO 220 i=1,3
	    write(lines(i)(pos:pos+width-1),format_to_use(i),err=920) info.member(i)
	    IF (use_tab) lines(i)(pos+width:pos+width) = CHAR(9)
	    GOTO 220
C
920	    type *,'internal format error, pos=',pos,', fmt=',format_to_use(i),', info='
	    type *,'i=',i,', datum=',info.member(i)
	    lines(i)(pos:pos+width-1) = stars(1:width)
220	END DO
C
	IF (use_tab) THEN
	    pos = pos + width + 1	! advance pointer to just beyond the
					! <tab> we inserted
	ELSE
	    pos = pos + width + 3	! advance pointer to leave 3 spaces
					! between triples
	END IF
	RETURN
	END
