        SUBROUTINE get_orbit_days(beg_date, end_date, what_pos, maxsize,
     1		nstored, epoch_arr, gci_pos_arr, gci_vel_arr, gse_pos_arr,
     1		gse_vel_arr, gsm_pos_arr, gsm_vel_arr, sun_vec_arr)
 
C+
C 
C FUNCTIONAL DESCRIPTION:	
C 
C    
C    This routine returns all requested orbit information for a given time
C    frame, which is an interval between two dates.
C
C 
C FORMAL PARAMETERS:
C  
C     BEG_DATE:
C	  REAL*8    The date/time, in Epoch format, of the first position
C		    requested.
C      
C     END_DATE:
C	  REAL*8    The date/time, in Epoch format, of the last position
C		    requested.
C      
C     WHAT_POS:
C	  LOG*4	    A 7-element logical array.  Each element indicates to
C		    GET_ORBIT_DAYS what information the calling program
C		    requires.  The elements, in order, are:
C			1   GCI position
C			2   GCI velocity
C			3   GSE position
C			4   GSE velocity
C			5   GSM position
C			6   GSM velocity
C			7   Sun vector
C		    If a particular element is .TRUE. the corresponding position
C		    (or velocity) information is stored in its argument.
C      
C     MAXSIZE:
C	  INT*4	    The maximum number of elements in each of the arrays
C		    GCI_POS_ARR, GCI_VEL_ARR, GSE_POS_ARR, GSE_POS_ARR,
C		    GSM_POS_ARR, GSM_POS_ARR, and SUN_VEC_ARR. 
C      
C     NSTORED:
C	  INT*4	    The number of positions actually stored in each of the
C		    arrays GCI_POS_ARR, GCI_VEL_ARR, GSE_POS_ARR, GSE_POS_ARR,
C		    GSM_POS_ARR, GSM_POS_ARR, and SUN_VEC_ARR. 
C
C     EPOCH_ARR:
C     	  REAL*8    An array of MAXSIZE elements containing the Epoch times.
C		    The first NSTORED elements contain Epoch times associated
C		    with the corresponding position and velocity values stored
C		    in the remaining arguments.
C	
C     GCI_POS_ARR:
C	  /EPHEMERIS/	An array of MAXSIZE RECORDs of structure type /EPHEMERIS/.
C		    The first NSTORED elements contain positions in GCI
C		    coordinates which were retrieved from the CDF data file. 
C      
C     GCI_VEL_ARR:
C	  /EPHEMERIS/	An array of MAXSIZE RECORDs of structure type /EPHEMERIS/.
C		    The first NSTORED elements contain velocities in GCI
C		    coordinates which were retrieved from the CDF data file. 
C      
C     GSE_POS_ARR:
C	  /EPHEMERIS/	An array of MAXSIZE RECORDs of structure type /EPHEMERIS/.
C		    The first NSTORED elements contain positions in GSE
C		    coordinates which were retrieved from the CDF data file. 
C      
C     GSE_VEL_ARR:
C	  /EPHEMERIS/	An array of MAXSIZE RECORDs of structure type /EPHEMERIS/.
C		    The first NSTORED elements contain velocities in GSE
C		    coordinates which were retrieved from the CDF data file. 
C      
C     GSM_POS_ARR:
C	  /EPHEMERIS/	An array of MAXSIZE RECORDs of structure type /EPHEMERIS/.
C		    The first NSTORED elements contain positions in GSM
C		    coordinates which were retrieved from the CDF data file. 
C      
C     GSM_VEL_ARR:
C	  /EPHEMERIS/	An array of MAXSIZE RECORDs of structure type /EPHEMERIS/.
C		    The first NSTORED elements contain velocities in GSM
C		    coordinates which were retrieved from the CDF data file. 
C      
C     SUN_VEC_ARR:
C	  /EPHEMERIS/	An array of MAXSIZE RECORDs of structure type /EPHEMERIS/.
C		    The first NSTORED elements contain positions in GCI sun
C		    vector coordinates which were retrieved from the CDF data
C		    file.  
C
C  
C COMMON BLOCKS:
C  
C     EPHDATA	  where routine GET_EPHEMERIS stores the position and velocity
C     		  data it reads from the CDF; defined in L1_HDR_INFO.INC
C  
C DESIGN:
C  
C     convert Epoch times into dates
C     construct name of CDF file
C     read data from CDF file
C     move data from COMMON into proper parts of arrays
C     check if beyond ending date; if not repeat previous two steps
C  
C 
C This version:    1-JUN-1995	  1.0
C 
C Created: 
C 
C	 1-JUN-1995    Larry Bleau
C 
C Revised:
C 
C	 Date     | Name  | Description
C ----------------+-------+-----------------------------------------------------
C [change_entry]
C 
C-
CDEC$	IDENT '1.0'
	IMPLICIT NONE
	INCLUDE 'INC_DIR:EPHDEF.INC'
	INCLUDE 'INC_DIR:L1_HDR_INFO.INC'
	INCLUDE '($RMSDEF)'
	REAL*8 beg_date,end_date
	LOGICAL*4 what_pos(7)
	INTEGER*4 maxsize,nstored
	REAL*8 epoch_arr(maxsize)
	RECORD /EPHEMERIS/ gci_pos_arr(maxsize), gci_vel_arr(maxsize),
	1 gse_pos_arr(maxsize), gse_vel_arr(maxsize), gsm_pos_arr(maxsize),
	1 gsm_vel_arr(maxsize), sun_vec_arr(maxsize)
C
	INTEGER*4 year, month, day, hour, minute, second, msec, store_here, i
	INTEGER*4 status, context, ncor, lib$find_file, lib$find_file_end, nleft
	CHARACTER or_pattern*39,or_name*80
	REAL*8 this_date
C
	this_date = beg_date
	store_here = 1
	nstored = 0
C
C Start loop.  Process a day at a time, copying data into arrays starting at
C store_here.  Stop when we:
C 1) reach the ending date specified by end_date, or
C 2) run out of space in the array to store the data
C
	DO WHILE (this_date .le. end_date .and. nstored .lt. maxsize)
C 
C Convert Epoch time into date format
C
	    call epoch_breakdown(this_date, year, month, day, hour, minute,
	1     second, msec)
C
C Determine the name of the first ORbit data file for this day.
C This assumes there is only one version of the orbit file present for this
C date.  If there is more than one this code will use the first file, which may
C not be the one desired.
C
	    write(or_pattern,1001) year,month,day
1001	    format('WIND_OR_DATA:WI_OR_DEF_',I4.4,I2.2,I2.2,'_V%%.CDF')
	    context = 0
	    status = lib$find_file (or_pattern, or_name, context, , , ,)
	    if (status .eq. rms$_normal) then	    ! we found a file
		ncor = index( or_name, '.CDF;' ) - 1
	    else
		goto 500			    ! no file, next day
	    endif
	    status = lib$find_file_end(context)
D	type *,'reading from ',or_name(1:ncor)
C
C Now that we have a CDF filename call the routine GET_EPHEMERIS to read orbit
C data from the CDF.  It puts it in common block EPHDATA.  The blank second
C argument tells it we don't want attitude information.
C 
	    call get_ephemeris(or_name(1:ncor), ' ', status)
C
C Now copy the requested information from the common block into the caller's
C arrays at the correct position.
C
	    nleft = maxsize - store_here + 1
	    DO i=1,MIN(nleft,144)
		epoch_arr(store_here) = eph_epoch(i)
		IF (what_pos(1)) gci_pos_arr(store_here) = gci_pos(i)
		IF (what_pos(2)) gci_vel_arr(store_here) = gci_vel(i)
		IF (what_pos(3)) gse_pos_arr(store_here) = gse_pos(i)
		IF (what_pos(4)) gse_vel_arr(store_here) = gse_vel(i)
		IF (what_pos(5)) gsm_pos_arr(store_here) = gsm_pos(i)
		IF (what_pos(6)) gsm_vel_arr(store_here) = gsm_vel(i)
		IF (what_pos(7)) sun_vec_arr(store_here) = gci_sun_vector(i)
		store_here = store_here + 1
	    END DO
	    nstored = store_here - 1
C
C Done copying orbit data for this day; increment Epoch time by one day and test
C if we're done.  If not, loop back and get next day's orbit data.
C	    
500	    this_date = this_date + 24*60*60*1E3
	ENDDO
C
	RETURN
	END
