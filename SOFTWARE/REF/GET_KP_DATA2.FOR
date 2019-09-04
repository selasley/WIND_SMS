CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	SUBROUTINE get_kp_data2 (beg_date, end_date, what_info, outfilename, interval)
C
C	This routine will open up the cdf file specified by what_info
C	and read the pertinent data into memory.  This data will
C	then be sent to a text file output...
C
C
C Created by 	22-FEB-1996	J.MILLER and K.CHOTOO
C
C Revision History:
C
C  9-APR-1996	K.CHOTOO	Get maximum number of records for each day
C				instead of just at beginning of time period.
C  9-MAY-1996 	K.CHOTOO	Routine can now retrieve STEP data as well.
C 22-JUN-1996	K.CHOTOO	GSE position in Re added to SWE output file.
C				Velocity in polar coordinates read from CDF
C				instead of being calculated.
C				SWE data can also be averaged over time interval.
C 26-JUN-1996	K.CHOTOO	Interval End time replaced by interval Start time.
C 29-JUN-1996	K.CHOTOO	If one SWE value is bad for a given time, flag
C				all SWE values with -2000.0.
C  6-JAN-2002	LRB		change limit on GSE position to 2.5E6 (was 2E6)
C 17-OCT-2002	LRB	1.7	change missing file logic so program exits
C				after 7 consecutive missing files; change early
C				exit of DO loops to EXIT instead of overwriting
C				DO index; declare cdf_var_num
C 18-OCT-2002	LRB	1.8	write output filename at end of program
C 18-OCT-2002	LRB	1.9	fix SWE interval_boundary calculation so it
C				won't skip an interval if data start late in
C				interval, add small delta (1000.) to account
C				for possible roundoff; don't fix MFI et al code
C 23-OCT-2002	LRB	2.0	fix bug that prevented start of interval from
C				being other than start of day; major rewrite
C				of SWE control logic, change how intervals are
C				calculated and detected; reduce # of messages
C				printed when file is missing; consolidate many
C				variables; take care of final partial interval
C 25-OCT-2002	LRB	2.1	replicate SWE control logic to MFI, STEP, 3DP;
C				fux bugs in 3DP, STEP code sections
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CDEC$	IDENT '2.1'
	IMPLICIT NONE
	INCLUDE 'CDF$INC:CDF.INC'
	INTEGER*4 cdf_var_num		!Function to return a cdf variable number
C
	REAL*8 beg_date,		!Start Epoch time
     .         end_date,		!End Epoch time
     .         rec_epoch,		!Current record's Epoch time
     .         epoch_temp,		!Temporary Epoch time
     .         interval_boundary,       !Time interval boundary
     .         rinterval,		!Sampling time interval
     .         file_date,		!Date of file to open (Epoch time)
     .         temp_var 		!Temporary variable
C
	REAL*4 bgsea(3),		!B-field in GSE spherical coordinates
     .         bgsec(3),		!B-field in GSE cartesian coordinates
     .         bgsma(3),		!B-field in GSM spherical coordinates
     .         bgsmc(3),		!B-field in GSM cartesian coordinates
     .         elect_dens, 		!Electron density in #/cm^3
     .         elect_qdotb /0/,		!Electron q.B in eV/s cm^2
     .         elect_vel(3),		!Electron velocity in km/s
     .         elect_temp,		!Electron temperature in eV
     .         interval,		!Sampling interval in minutes
     .	       ion_dens,		!Ion density in #/cm^3
     .         ion_vel(3),		!Ion velocity in km/s
     .         ion_temp,		!Ion temperature in eV
     .         pi/3.141593/,
     .         position_gse(3),		!Cartesian position in GSE coordinates, km
     .         prot_dens,		!Proton density in #/cm^3
     .	       Re/6380.0/,		!Earth Radius in km
     .         step1,	        	!He  flux  80-160  keV/n
     .         step2,	        	!He  flux 320-640  keV/n
     .         step3,	        	!CNO flux  80-160  keV/n
     .         step4,	        	!CNO flux 320-640  keV/n
     .         step5,	        	!Fe  flux  80-160  keV/n
     .         step6,	        	!Fe  flux 640-1050 keV/n
     .         v_gse(3),		!Velocity in GSE cartesian coordinates, km/s
     .	       v_gse_p(3),		!Bulk flow in polar coordinates
     .         v_gsm(3),		!Velocity in GSM cartesian coordinates, km/s
     .         vtherm  			!Thermal velocity, km/s = sqrt(2kT/M)
C
	LOGICAL*4 what_info(4)		!Which instrument datasets selected
C
	INTEGER*4 cdf_id,		!Unit number of CDF opened by CDF_OPEN
     . 		  dim_sizes(CDF_MAX_DIMS),
     .            encoding,		!Encoding of CDF file
     .            i,j,			!Counters
     .            indices(1),
     .            majority,
     .            rec_num,		!Current record number in CDF file
     .            num_recs,		!Number of records in CDF
     .            num_attrs, 		!Number of attributes in CDF file
     .            num_dims,
     .            num_vars,		!Number of variables in CDF
     .            status,
     .            which_part,		!Diagnostic aid, which part had error
     .		  swe_sample,		!Reply for sampling or averaging data
     .            doy,			!day of year
     .		  tdp_index(3),
     .            vn_bgsea,        !vn = variable number for use
     .            vn_bgsec,        !in getting values with CDF_VAR_GET
     .            vn_bgsma,
     .            vn_bgsmc,
     .            vn_epoch,
     . 		  vn_edens,
     .		  vn_eqdotb,
     .  	  vn_etemp,
     .            vn_evel,
     .            vn_iondens,
     .            vn_iontemp,
     .            vn_ionvel,
     .    	  vn_position,
     .		  vn_protdens,
     .            vn_step1,
     .            vn_step2,
     .            vn_step3,
     .            vn_step4,
     .            vn_step5,
     .            vn_step6,
     .            vn_vgse,
     .		  vn_vgse_p,
     .            vn_vgsm,
     .            vn_vth
C
	INTEGER*4 max_missing_days/7/,			!Maximum number of missing days allowed
     .   	  missing_days				!Number of missing cdf files
C
	INTEGER, PARAMETER :: msec_in_day = 24*60*60*1000   !# of msec in a day
C
C Averaging arrays
C
   	REAL*4    swe_data(0:13),swe_data_avg(0:13),swe_hits(0:13)

	INTEGER*4 mon, day, year, hr, min, sec, msec,
     .		  st_mon,st_year,st_doy,st_hr,st_min,st_sec

	CHARACTER*40 outfilename(4)			!Output file names
        CHARACTER*36 cdf_name				!Name of CDF file
	CHARACTER*(EPOCH_STRING_LEN) date1, date2       !Character string for output of Epoch time
	CHARACTER*(cdf_statustext_len) message		!Error message description
     	CHARACTER run_date*9, run_time*8		!run date/time
C
C Start here, initialize some stuff
C If user wants SWE data, prompt for sampling vs. average
C
	rinterval=dble(interval*60.0*1000.0)		!Convert sampling time to milliseconds

        CALL DATE(run_date)				!Get date program was run
        CALL TIME(run_time)				!Get time program was run
    5   FORMAT(2X,'Run time:  ',A9,'   ',A8)

	if(what_info(3)) then
	  write(6,'(A,$)')' Do you want to sample(=1) or average(=2) SWE data? '
	  read(5,*)swe_sample
	endif

C*******************************************************************************
C	                                                                       *
C		Get MFI data and write to output files                         *
C                                                                              *
C*******************************************************************************
	if (what_info(1)) then

	  interval_boundary = 0
	  file_date = beg_date
	  missing_days = 0
C
C Open output MFI file and write out headers to file.
C
	  OPEN(unit=10, file=outfilename(1), status='NEW', recl=180)
	  write(10,*) ' WIND Key Parameter CDF -> Text translation '
	  write(10,5)run_date, run_time
	  write(10,*) ' Magnetic Field Data '
	  call encode_epoch(beg_date, date1)
	  write(10,*) ' Beginning Date: ', date1
	  call encode_epoch(end_date, date2)
	  write(10,*) ' End Date: ', date2
	  write(10,15)
   15     format(/,' Year DOY:HR:MN:SC       BGSEr    BGSEt    BGSEp    BGSEx    BGSEy    BGSEz')
C
C Open CDF file
C
C First convert Epoch start time to regular time, then use it to construct
C cdf filename.  If open fails, check for higher data versions (up to V09).
C If there aren't any, increment file_date by one day and access next day's cdf
C file.
C
  20	  if (file_date .gt. end_date) goto 95
 	  call epoch_breakdown(file_date,year,mon,day,hr,min,sec,msec)
	  write(cdf_name,30) year,mon,day
  30	  format('wind_kp_mfi:wi_k0_mfi_',I4.4,I2.2,I2.2,'_v01')
c	  call cdf_open(cdf_name,cdf_id,status)
	  call CDF_lib (OPEN_, CDF_, cdf_name, cdf_id, NULL_, status)
C
C If data version V01 does NOT exist, increment version number up to 09.
C
	  if (status.le.cdf_warn) then
	    do 35 i = 2,9
	      write (cdf_name(34:34), '(I1)') i
c	      call cdf_open(cdf_name,cdf_id,status)
	      call CDF_lib (OPEN_, CDF_, cdf_name, cdf_id, NULL_, status)
	      if (status.eq.cdf_ok) exit
  35	    continue
	  end if
	  if (status.eq.cdf_ok) then
	    write(*,*) cdf_name
	  else
	    write(*,*) ' cannot open ',cdf_name,' skipping day'
	    missing_days = missing_days + 1
            if (missing_days .gt. max_missing_days) goto 95
	    file_date = file_date + msec_in_day
	    goto 20
	  endif
	  missing_days = 0
C
C  Get CDF file ID, maximum number of records, etc., from CDF file.
C
C	  call cdf_inquire(cdf_id,num_dims,dim_sizes,encoding,majority,
C     .      num_recs,num_vars,num_attrs,status)

	  call CDF_lib (SELECT_, CDF_, cdf_id,
     .			GET_, zVARs_MAXREC_, num_recs,
     .			NULL_, status)
C
C  If there is a problem with CDF file or it does not exist then open file
C  for next day.
C
	  if (status .ne. cdf_ok) then
	    write(*,*) ' problem with cdf_inquire... '
	    call cdf_error(status,message)
	    write(*,*) message
c	    call cdf_close(cdf_id,status)
	    call CDF_lib (CLOSE_,CDF_, cdf_id)
	    file_date = file_date + msec_in_day
	    goto 20
	  end if					!status bad
C
C  Get variable numbers for different data
C
C	  vn_epoch = cdf_var_num(cdf_id,'Epoch')
!	  vn_bgsmc = cdf_var_num(cdf_id,'BGSMc')
!	  vn_bgsma = cdf_var_num(cdf_id,'BGSMa')
C	  vn_bgsec = cdf_var_num(cdf_id,'BGSEc')
C	  vn_bgsea = cdf_var_num(cdf_id,'BGSEa')

	  CALL CDF_lib (SELECT_, CDF_, cdf_id,
     .  		GET_, zVAR_NUMBER_, 'Epoch', vn_epoch,
     .			      zVAR_NUMBER_, 'BGSEc', vn_bgsec,
     .			      zVAR_NUMBER_, 'BGSEa', vn_bgsea,
     .			NULL_, status)

C
C	************* Get MFI data ******************
C
C Get first interval Start time
C
	  indices(1) = 1
	  rec_num = 1
c	  call cdf_var_get(cdf_id,vn_epoch,rec_num,indices(1),rec_epoch,
c     .         status)
	  CALL CDF_lib (SELECT_, CDF_, cdf_id,
     .			zVAR_, vn_epoch,
     .			zVAR_RECNUMBER_, rec_num,
     .			zVAR_DIMINDICES_, indices(1),
     .			GET_, zVAR_DATA_, rec_epoch,
     .			NULL_, status)
C
C Read through file until we reach a record that is on or after specified
C start time (beg_date) or we reach end of file.
C
 	  do while ((rec_num .lt. num_recs) .and.
     .	            (rec_epoch .lt. beg_date) .and.
     .	            (rec_epoch .lt. end_date) .and.
     .	            (status .eq. cdf_ok))
	    indices(1) = 1
	    rec_num = rec_num + 1
c	    call cdf_var_get(cdf_id,vn_epoch,rec_num,indices(1),rec_epoch,
c     .         status)
	    CALL CDF_lib (SELECT_, CDF_, cdf_id,
     .			  zVAR_, vn_epoch,
     .			  zVAR_RECNUMBER_, rec_num,
     .			  zVAR_DIMINDICES_, indices(1),
     .			  GET_, zVAR_DATA_, rec_epoch,
     .			  NULL_, status)
	  end do
C
C If we got an error or we read through entire file without reaching the
C start time, ignore file and advance to next day, try again.  Note, though,
C that this should never happen, since we chose a cdf file on the correct date
C to start with.
C
	  if (status .ne. cdf_ok .or. rec_epoch .lt. beg_date) then
	    file_date = file_date + msec_in_day
c	    call cdf_close(cdf_id,status)
	    call CDF_lib (CLOSE_,CDF_, cdf_id)
	    goto 20
	  endif
C
C Calculate interval boundary if nonzero interval.  If ultimate mode, leave
C interval_boundary zero.
C
C Usually beg_date is start of first interval.  If we skipped one or more days
C of data, however, or if the first file is missing some data and the interval
C is shorter than a day, it is possible that we are missing one or more entire
C intervals, and thus the normal computation for the end of interval would be
C incorrect.  The loop below repeatedly increases interval_boundary by
C rinterval until it reaches its largest value that is still less than the
C first time in the file after beg_date; i.e., the following hold:
C
C	first record to process is i
C	if i > 1 then time for record i-1 < beg_date < time for record i
C	if i = 1 then beg_date < time for record i
C	if interval > 0 then interval_boundary < time for record i < interval_boundary+rinterval
C
C This initializing logic will force the end of interval to be detected while
C processing the first record, and thus cause the values for that record to
C be written out, after which interval_boundary will be adjusted forward by
C rinterval.
C
	  interval_boundary = 0
	  if (interval .ne. 0) then
	    interval_boundary = beg_date
	    do while (interval_boundary + rinterval .lt. rec_epoch)
	      interval_boundary = interval_boundary + rinterval
	    end do
	  endif
C
C Start main loop, keep going until we reach stop time (end_date)
C
 	  do while (rec_epoch .le. end_date)
C
C Start processing loop for this file, keep going until we run out of records
C in the file.  Within this loop test for end of interval condition.
C
	    do while (rec_num .le. num_recs)
C
C Get epoch time for data and convert to year,doy,etc
C
	      indices(1) = 1

c	      call cdf_var_get(cdf_id,vn_epoch,rec_num,indices(1),rec_epoch,
c     .          status)
	      CALL CDF_lib (SELECT_, CDF_, cdf_id,
     .			    	     zVAR_, vn_epoch,
     .				     zVAR_RECNUMBER_, rec_num,
     .				     zVAR_DIMINDICES_, indices(1),
     .			    GET_, zVAR_DATA_, rec_epoch,
     .			    NULL_, status)

	      call epoch_breakdown(rec_epoch,year,mon,day,hr,min,sec,msec)
	      call compute_epoch(year,0,1,0,0,0,0,epoch_temp)
	      doy = int((rec_epoch - epoch_temp)/(msec_in_day)) + 1
C
C Check if past end of interval or past stop date.  If so we are at the start
C of a new interval.
C If we are dumping every value (ultimate resolution), treat as if always
C end of interval.
C
	      if ((rec_epoch .ge. interval_boundary) .or.
     .		  (rec_epoch .ge. end_date) .or.
     .		  (interval .eq. 0)) then
C
C We are past the end of averaging interval - i.e., at start of new interval
C Extract data from this record and write it out
C
C Call internal subroutine to extract MFI data
C It returns status = 0 for success
C
	        call get_mfi_data(status)
C
C Write values out to file if successful
C
	        if (status .eq. 0) then
  60	          format(1X,I4,1X,I3,3(':',I2.2),3X,6(1X,F8.2))
	          write(10,60) year,doy,hr,min,sec,(bgsea(i),i=1,3),(bgsec(i),i=1,3)
		endif
C
C Advance interval boundary (end of interval) by exactly the amount
C the user specified (stored in rinterval).  This way there will be no "creep"
C in the interval start time.
C
C In case there is a large stretch of missing data, keep advancing interval
C boundary until it is within rinterval of the next data point (rec_epoch).
C
C If we are in ultimate mode (all data points), don't bother to advance
C interval_boundary, leave it 0 to force dump of every data point.
C
	        if (interval .ne. 0) then
	          interval_boundary = interval_boundary + rinterval
	          do while (interval_boundary .lt. rec_epoch)
	            interval_boundary = interval_boundary + rinterval
	          end do
	        endif
	      endif
C
              if (rec_epoch .ge. end_date) goto 95
C
C Done processing this record
C Advance to next record in cdf file
C Loop back to check if we are at maxrecs, and if so drop out of inner WHILE
C loop, close old cdf, and open new one.
C
  75	      rec_num = rec_num + 1
	    end do 			! rec_num .le. num_recs
C
C We exited inner loop, which means we've run out of records in the cdf file.
C Close old cdf and open new one, the go back to inner loop to process data.
C
c 80	    call cdf_close(cdf_id,status)
 80	    CALL CDF_lib (CLOSE_, CDF_, cdf_id)
 85	    file_date = file_date + msec_in_day
	    if (file_date .gt. end_date) goto 95
	    call epoch_breakdown(file_date,year,mon,day,hr,min,sec,msec)
	    write(cdf_name,30) year,mon,day
c	    call cdf_open(cdf_name,cdf_id,status)
	    CALL CDF_lib (OPEN_, CDF_, cdf_name, cdf_id, NULL_, status)
	    if (status.le.cdf_warn) then
	      do 90 i = 2,9
	        write (cdf_name(34:34), '(I1)') i
c	        call cdf_open(cdf_name,cdf_id,status)
	    	CALL CDF_lib (OPEN_, CDF_, cdf_name, cdf_id, NULL_, status)
	        if (status.eq.cdf_ok) exit
 90	      continue
	    end if                              !status.le.cdf_warn
C
C Write out name of new file.
C Get num_recs for new file.
C Assume variable numbers for other quantities we want do not change.
C
	    if (status .eq. cdf_ok) then
	      write(*,*) cdf_name
c     	      call cdf_inquire(cdf_id,num_dims,dim_sizes,encoding,majority,
c     .         num_recs,num_vars,num_attrs,status)
	      CALL CDF_lib (SELECT_, CDF_, cdf_id,
     .			GET_, zVARs_MAXREC_, num_recs,
     .			NULL_, status)
	    endif
C
	    if (status.ne.cdf_ok) then
	      write(*,*) ' cannot open ',cdf_name,' skipping...'
c	      call cdf_close(cdf_id,status)
	      CALL CDF_lib (CLOSE_, CDF_, cdf_id)
	      missing_days = missing_days + 1
              if (missing_days .gt. max_missing_days) goto 95
	      goto 85
	    end if					!status bad
C
C We successfully opened next cdf file.  Reset missing file counter,
C reinitialize record counter, and loop back to process it.
C
	    missing_days = 0
	    rec_num = 1

	  end do				!rec_epoch .le. end_date

c  95	  call cdf_close(cdf_id,status)
  95	  CALL CDF_lib (CLOSE_, CDF_, cdf_id)
	  CLOSE(10)

	end if					!what_info(1)mfi

C*******************************************************************************
C                                                                              *
C		Get 3DP data and write to output files                         *
C                                                                              *
C*******************************************************************************

	if (what_info(2)) then						!3DP

	  interval_boundary = 0
	  file_date = beg_date
	  missing_days = 0
C
C  Open output 3DP file and write out headers to file.
C
	  OPEN(unit=100, file=outfilename(2), status='NEW',recl=180)
	  write(100,*) ' WIND Key Parameter CDF -> Text translation '
	  write(100,5)run_date, run_time
	  write(100,*) ' 3D-Plasma Instrument Data '
	  call encode_epoch(beg_date, date1)
	  write(100,*) ' Beginning Date: ', date1
	  call encode_epoch(end_date, date2)
	  write(100,*) ' End Date: ', date2
	  write(100,*) ' Values of -2000.0 are null values (no data code) '
	  write(100,*) ' Units: '
	  write(100,*) ' Ne,Ni	-	#/cm^3 '
	  write(100,*) ' Ve_n,Vi_n -	km/s '
	  write(100,*) ' Te,Ti 	-	eV '
	  write(100,*) ' e_q.B	-	eV/s cm^2	no values yet '
 	  write(100,115)
 115      format(/,' Year DOY:HR:MIN:SC      Vi_x      Vi_y     Vi_z       Ni      Ti    ',
     .                                      'Ve_x      Ve_y     Ve_z       Ne      Te ')
C
C Open CDF file
C
C First convert Epoch start time to regular time, then use it to construct
C cdf filename.  If open fails, check for higher data versions (up to V09).
C If there aren't any, increment file_date by one day and access next day's cdf
C file.
C
 120	  if (file_date .gt. end_date) goto 195
 	  call epoch_breakdown(file_date,year,mon,day,hr,min,sec,msec)
	  write(cdf_name,130) year,mon,day
 130	  format('wind_kp_3dp:wi_k0_3dp_',I4.4,I2.2,I2.2,'_v01')
	  call cdf_open(cdf_name,cdf_id,status)
C
C If data version V01 does NOT exist, increment version number up to 09.
C
	  if (status.le.cdf_warn) then
	    do 135 i = 2,9
	      write (cdf_name(34:34), '(I1)') i
	      call cdf_open(cdf_name,cdf_id,status)
	      if (status.eq.cdf_ok) exit
 135	    continue
	  end if
	  if (status.eq.cdf_ok) then
	    write(*,*) cdf_name
	  else
	    write(*,*) ' cannot open ',cdf_name,' skipping day'
	    missing_days = missing_days + 1
            if (missing_days .gt. max_missing_days) goto 195
	    file_date = file_date + msec_in_day
	    goto 120
	  endif
	  missing_days = 0
C
C  Get CDF file ID, maximum number of records, etc., from CDF file.
C
	  call cdf_inquire(cdf_id,num_dims,dim_sizes,encoding,majority,
     .      num_recs,num_vars,num_attrs,status)
C
C  If there is a problem with CDF file or it does not exist then open file
C  for next day.
C
	  if (status .ne. cdf_ok) then
	    write(*,*) ' problem with cdf_inquire... '
	    call cdf_error(status,message)
	    write(*,*) message
	    call cdf_close(cdf_id,status)
	    file_date = file_date + msec_in_day
	    goto 120
	  end if					!status bad
C
C Get variable number for different data
C
	  vn_epoch = cdf_var_num(cdf_id,'Epoch')
	  vn_edens = cdf_var_num(cdf_id,'elect_density')
	  vn_evel = cdf_var_num(cdf_id,'elect_vel')
	  vn_etemp = cdf_var_num(cdf_id,'elect_temp')
!	  vn_eqdotb = cdf_var_num(cdf_id,'elect_qdotb')
	  vn_iondens = cdf_var_num(cdf_id,'ion_density')
	  vn_ionvel = cdf_var_num(cdf_id,'ion_vel')
	  vn_iontemp = cdf_var_num(cdf_id,'ion_temp')
C
C	*************** Get 3DP Data ***********************
C
C Get first interval Start time
C
          tdp_index(1) = 1
	  tdp_index(2) = 1
	  tdp_index(3) = 1
	  rec_num = 1
	  call cdf_var_get(cdf_id,vn_epoch,rec_num,tdp_index,rec_epoch,
     .         status)
C
C Read through file until we reach a record that is on or after specified
C start time (beg_date) or we reach end of file.
C
 	  do while ((rec_num .lt. num_recs) .and.
     .	            (rec_epoch .lt. beg_date) .and.
     .	            (rec_epoch .lt. end_date) .and.
     .	            (status .eq. cdf_ok))
            tdp_index(1) = 1
	    tdp_index(2) = 1
	    tdp_index(3) = 1
	    rec_num = rec_num + 1
	    call cdf_var_get(cdf_id,vn_epoch,rec_num,tdp_index,rec_epoch,
     .         status)
	  end do
C
C If we got an error or we read through entire file without reaching the
C start time, ignore file and advance to next day, try again.  Note, though,
C that this should never happen, since we chose a cdf file on the correct date
C to start with.
C
	  if (status .ne. cdf_ok .or. rec_epoch .lt. beg_date) then
	    file_date = file_date + msec_in_day
	    call cdf_close(cdf_id,status)
	    goto 120
	  endif
C
C Calculate interval boundary if nonzero interval.  If ultimate mode, leave
C interval_boundary zero.
C
C Usually beg_date is start of first interval.  If we skipped one or more days
C of data, however, or if the first file is missing some data and the interval
C is shorter than a day, it is possible that we are missing one or more entire
C intervals, and thus the normal computation for the end of interval would be
C incorrect.  The loop below repeatedly increases interval_boundary by
C rinterval until it reaches its largest value that is still less than the
C first time in the file after beg_date; i.e., the following hold:
C
C	first record to process is i
C	if i > 1 then time for record i-1 < beg_date < time for record i
C	if i = 1 then beg_date < time for record i
C	if interval > 0 then interval_boundary < time for record i < interval_boundary+rinterval
C
C This initializing logic will force the end of interval to be detected while
C processing the first record, and thus cause the values for that record to
C be written out, after which interval_boundary will be adjusted forward by
C rinterval.
C
	  interval_boundary = 0
	  if (interval .ne. 0) then
	    interval_boundary = beg_date
	    do while (interval_boundary + rinterval .lt. rec_epoch)
	      interval_boundary = interval_boundary + rinterval
	    end do
	  endif
C
C Start main loop, keep going until we reach stop time (end_date)
C
 	  do while (rec_epoch .le. end_date)
C
C Start processing loop for this file, keep going until we run out of records
C in the file.  Within this loop test for end of interval condition.
C
	    do while (rec_num .le. num_recs)
              tdp_index(1) = 1
	      tdp_index(2) = 1
	      tdp_index(3) = 1
C
C Get epoch time for data and convert to year,doy,etc
C
	      call cdf_var_get(cdf_id,vn_epoch,rec_num,tdp_index,rec_epoch,
     .           status)

	      call epoch_breakdown(rec_epoch,year,mon,day,hr,min,sec,msec)
	      call compute_epoch(year,0,1,0,0,0,0,epoch_temp)
	      doy = int((rec_epoch - epoch_temp)/(msec_in_day)) + 1
C
C Check if end of interval or past stop date.
C If we are dumping every value (ultimate resolution), treat as if always
C end of interval.
C
	      if ((rec_epoch .ge. interval_boundary) .or.
     .		  (rec_epoch .ge. end_date) .or.
     .		  (interval .eq. 0)) then
C
C We are past the end of averaging interval, which means we're at the start of
C a new interval.
C
C Extract data from this record and write it out
C
	        call get_3dp_data(status)
	        if (status .eq. 0) then
C
C Write them all out to file
C
 160	          format(1X,I4,1X,I3,3(':',I2.2),3X,10(1X,F8.2))
 170	          write(100,160) year,doy,hr,min,sec,(ion_vel(i),i=1,3),ion_dens,ion_temp,
     .                        (elect_vel(i),i=1,3),elect_dens,elect_temp
	        endif
C
C Advance interval boundary (end of interval) by exactly the amount
C the user specified (stored in rinterval).  This way there will be no "creep"
C in the interval start time.
C
C In case there is a large stretch of missing data, keep advancing interval
C boundary until it is within rinterval of the next data point (rec_epoch).
C
C If we are in ultimate mode (all data points), don't bother to advance
C interval_boundary, leave it 0 to force dump of every data point.
C
	        if (interval .ne. 0) then
	          interval_boundary = interval_boundary + rinterval
	          do while (interval_boundary .lt. rec_epoch)
	            interval_boundary = interval_boundary + rinterval
	          end do
	        endif
C
	      endif
C
    	      if (rec_epoch .ge. end_date) goto 195
C
C Done processing this record
C Advance to next record in cdf file
C Loop back to check if we are at maxrecs, and if so drop out of inner WHILE
C loop.
C
	      rec_num = rec_num + 1
	    end do 			! rec_num .le. num_recs
C
C We exited inner loop, which means we've run out of records in the cdf file.
C Close old cdf and open new one, the go back to inner loop to process data.
C
 180	    call cdf_close(cdf_id,status)
 185	    file_date = file_date + msec_in_day
	    if (file_date .gt. end_date) goto 195
	    call epoch_breakdown(file_date,year,mon,day,hr,min,sec,msec)
	    write(cdf_name,130) year,mon,day
            call cdf_open(cdf_name,cdf_id,status)
	    if (status.le.cdf_warn) then
	      do 190 i = 2,9
	        write (cdf_name(34:34), '(I1)') i
	        call cdf_open(cdf_name,cdf_id,status)
	        if (status.eq.cdf_ok) exit
 190	      continue
	    end if                              !status.le.cdf_warn
C
C Write out name of new file.
C Get num_recs for new file.
C Assume variable numbers for other quantities we want do not change.
C
	    if (status .eq. cdf_ok) then
	      write(*,*) cdf_name
     	      call cdf_inquire(cdf_id,num_dims,dim_sizes,encoding,majority,
     .         num_recs,num_vars,num_attrs,status)
	    endif
C
	    if (status.ne.cdf_ok) then
	      write(*,*) ' cannot open ',cdf_name,' skipping...'
	      call cdf_close(cdf_id,status)
	      missing_days = missing_days + 1
              if (missing_days .gt. max_missing_days) goto 195
	      goto 185
	    end if					!status bad
C
C We successfully opened next cdf file.  Reset missing file counter,
C reinitialize record counter, and loop back to process it.
C
	    missing_days = 0
	    rec_num = 1

	  end do				!rec_epoch .le. end_date
C
C End of main loop
C
C We have gone past the user specified stop time
C
 195	call cdf_close(cdf_id,status)
	close(unit=100)				!3dp output file
      end if					!what_info(2)3dp

C*******************************************************************************
C                                                                              *
C		     Get SWE data and write to output file                     *
C                                                                              *
C*******************************************************************************
	if (what_info(3)) then                        			!SWE
C
C Clear averaging arrays, other initialization
C
	  swe_data = 0
	  swe_data_avg = 0
	  swe_hits = 0

	  interval_boundary = 0
	  file_date = beg_date
	  missing_days = 0
C
C Open SWE output file and write header information
C
	  OPEN(unit=200, file=outfilename(3), status='NEW',recl=180)
	  write(200,*) ' WIND Key Parameter CDF -> Text translation '
	  write(200,5) run_date, run_time
	  write(200,*) ' SWE Instrument Data '
	  if(swe_sample .eq. 1) then
	    write(200,*) ' Data is sampled at each time'
	  else
	    write(200,*) ' Data is averaged over each time interval'
	    if (interval .ne. 0) then
	      write(200,*) ' Averaging Interval (minutes):',interval
	    else
	      write(200,*) ' Averaging Interval: ultimate'
	    endif
	    write(200,*) ' Interval Start time given'
	  endif
	  call encode_epoch(beg_date, date1)
	  write(200,*) ' Beginning Date: ', date1
	  call encode_epoch(end_date, date2)
	  write(200,*) ' End Date: ', date2
	  write(200,*) ' Values of -2000.0 are null values (no data code) '
	  write(200,*) ' Units: '
	  write(200,*) '   Np	-	#/cm^3 '
    	  write(200,*) '   X_GSE,Y_GSE,Z_GSE 	-  Re'
	  write(200,*) '   VGSEx, VGSEy, VGSEz, |v| -  km/s '
	  write(200,*) '   Vtherm  = sqrt(2kT/M)	km/s '
          write(200,205)
 205	  format(/,4x,'S+N- is the polar angle relative to ecliptic plane with S+ and N-'/,
     .            10x,'S+N- = arctan(Vz/sqrt(Vx**2 + Vy**2))  in degrees'/,
     .             4x,'W+E- is the azimuthal angle relative to the +x-axis with W+ and E- wrt Sun'/
     .            10x,'W+E- =  arctan(Vy/-Vx)    in degrees' /)

	  write(200,215)
 215      format(/,' Year DOY:HR:MN:SC      X_GSE    Y_GSE    Z_GSE    ',
     .                                '  |v|  S+N-(deg) W+E-(deg)   Vth      Np     #_of_pts')
C
C Open CDF file.
C
C First convert Epoch start time to regular time, then use it to construct
C cdf filename.  If open fails, check for higher data versions (up to V09).
C If there aren't any, increment file_date by one day and access next day's cdf
C file.
C
 220	  if (file_date .gt. end_date) goto 295
 	  call epoch_breakdown(file_date,year,mon,day,hr,min,sec,msec)
	  write(cdf_name,230) year,mon,day
 230	  format('wind_kp_swe:wi_k0_swe_',I4.4,I2.2,I2.2,'_v01')
	  call cdf_open(cdf_name,cdf_id,status)
C
C If data version V01 does NOT exist, increment version number up to 09.
C
	  if (status.le.cdf_warn) then
	    do 235 i = 2,9
	      write (cdf_name(34:34), '(I1)') i
	      call cdf_open(cdf_name,cdf_id,status)
	      if (status.eq.cdf_ok) exit
 235	    continue
	  end if
	  if (status.eq.cdf_ok) then
	    write(*,*) cdf_name
	  else
	    write(*,*) ' cannot open ',cdf_name,' skipping day'
	    missing_days = missing_days + 1
            if (missing_days .gt. max_missing_days) goto 295
	    file_date = file_date + msec_in_day
	    goto 220
	  endif
	  missing_days = 0
C
C Get CDF file ID, maximum number of records, etc., for current CDF file
C
	  call cdf_inquire(cdf_id,num_dims,dim_sizes,encoding,majority,
     .      num_recs,num_vars,num_attrs,status)
C
C If there is a problem with the CDF file or it does not exist then
C open file for next day.
C
	  if (status .ne. cdf_ok) then
	    write(*,*) ' problem with cdf_inquire... '
	    call cdf_error(status,message)
	    write(*,*) message
	    call cdf_close(cdf_id,status)
	    file_date = file_date + msec_in_day
	    goto 220
	  end if					!status bad
C
C Get variable number for different data.
C
	  vn_epoch = cdf_var_num(cdf_id,'Epoch')
	  vn_vgse = cdf_var_num(cdf_id,'V_GSE')
	  vn_vgsm = cdf_var_num(cdf_id,'V_GSM')
	  vn_vth = cdf_var_num(cdf_id,'THERMAL_SPD')
	  vn_protdens = cdf_var_num(cdf_id,'Np')
          vn_position = cdf_var_num(cdf_id,'SC_pos_gse')
    	  vn_vgse_p = cdf_var_num(cdf_id,'V_GSE_p')
C
C	****************  Get SWE data ******************
C
C Get first interval Start time
C
	  indices(1) = 1
	  rec_num = 1
	  call cdf_var_get(cdf_id,vn_epoch,rec_num,indices(1),rec_epoch,
     .         status)
C
C Read through file until we reach a record that is on or after specified
C start time (beg_date) or we reach end of file.
C
 	  do while ((rec_num .lt. num_recs) .and.
     .	            (rec_epoch .lt. beg_date) .and.
     .	            (rec_epoch .lt. end_date) .and.
     .	            (status .eq. cdf_ok))
	    indices(1) = 1
	    rec_num = rec_num + 1
	    call cdf_var_get(cdf_id,vn_epoch,rec_num,indices(1),rec_epoch,
     .         status)
	  end do
C
C If we got an error or we read through entire file without reaching the
C start time, ignore file and advance to next day, try again.  Note, though,
C that this should never happen, since we chose a cdf file on the correct date
C to start with.
C
	  if (status .ne. cdf_ok .or. rec_epoch .lt. beg_date) then
	    file_date = file_date + msec_in_day
	    call cdf_close(cdf_id,status)
	    goto 220
	  endif
C
C Save first interval start time
C
	  call epoch_breakdown(rec_epoch,year,mon,day,hr,min,sec,msec)
	  call compute_epoch(year,0,1,0,0,0,0,epoch_temp)
	  doy = int((rec_epoch - epoch_temp)/(msec_in_day)) + 1
C
          st_year = year
	  st_doy = doy
	  st_hr = hr
	  st_min = min
	  st_sec = sec
C
C Calculate interval boundary if nonzero interval.  If ultimate mode, leave
C interval_boundary zero.
C
C Usually beg_date is start of first interval.  If we skipped one or more days
C of data, however, or if the first file is missing some data and the interval
C is shorter than a day, it is possible that we are missing one or more entire
C intervals, and thus the normal computation for the end of interval would be
C incorrect.  The loop below repeatedly increases interval_boundary by
C rinterval until it reaches a value that is later (greater) than the first
C time in the file after beg_date. 
C
	  interval_boundary = 0
	  if (interval .ne. 0) then
	    interval_boundary = beg_date + rinterval
	    do while (interval_boundary .lt. rec_epoch)
	      interval_boundary = interval_boundary + rinterval
	    end do
	  endif
C
C Start main loop, keep going until we reach stop time (end_date)
C
 	  do while (rec_epoch .le. end_date)
C
C Start processing loop for this file, keep going until we run out of records
C in the file.  Within this loop test for end of interval condition.
C
	    do while (rec_num .le. num_recs)
C
C Get epoch time for data and convert to year,doy,etc
C
	      indices(1) = 1

	      call cdf_var_get(cdf_id,vn_epoch,rec_num,indices(1),rec_epoch,
     .           status)

	      call epoch_breakdown(rec_epoch,year,mon,day,hr,min,sec,msec)
	      call compute_epoch(year,0,1,0,0,0,0,epoch_temp)
	      doy = int((rec_epoch - epoch_temp)/(msec_in_day)) + 1
C
C Check if end of interval or past stop date.
C If we are dumping every value (ultimate resolution), treat as if always
C end of interval.
C
	      if ((rec_epoch .ge. interval_boundary) .or.
     .		  (rec_epoch .ge. end_date) .or.
     .		  (interval .eq. 0)) then
C
C We are past the end of averaging interval
C Compute averages, use -2000.0 if no hits
C Write out averages or sample, as selected by swe_sample
C
		call swe_average
C
C Advance interval boundary (end of averaging interval) by exactly the amount
C the user specified (stored in rinterval).  This way there will be no "creep"
C in the interval start time.
C
C In case there is a large stretch of missing data, keep advancing interval
C boundary until it is within rinterval of the next data point (rec_epoch).
C
C If we are in ultimate mode (all data points), don't bother to advance
C interval_boundary, leave it 0 to force dump of every data point.
C
	        if (interval .ne. 0) then
	          interval_boundary = interval_boundary + rinterval
	          do while (interval_boundary .lt. rec_epoch)
	            interval_boundary = interval_boundary + rinterval
	          end do
	        endif
C
C The value in rec_epoch is now the time of the first data point in the new
C interval.  Save its components to be used when we write out the averages for
C the new interval.
C Also reset arrays back to zero for next interval
C
               	st_year = year                	!Save new interval's start time
	       	st_doy = doy
	       	st_hr = hr
	       	st_min = min
	        st_sec = sec
C
		swe_data = 0			!Clear averaging arrays
		swe_hits = 0
C
    	        if (rec_epoch .ge. end_date) goto 295
	      end if
C
C At this point we are either in the middle of an averaging interval or a new
C interval has just been started.
C Process this cdf record, extract SWE values and sum into arrays
C If there is an error, skip the rest of the file and go on to next file.
C
	      call sum_swe_data(status)
	      if (status .ne. 1) goto 280
C
C Done processing this record
C Advance to next record in cdf file
C Loop back to check if we are at maxrecs, and if so drop out of inner WHILE
C loop.
C
 275	      rec_num = rec_num + 1
	    end do 			! rec_num .le. num_recs
C
C We exited inner loop, which means we've run out of records in the cdf file.
C Close old cdf and open new one, the go back to inner loop to process data.
C
 280	    call cdf_close(cdf_id,status)
 285	    file_date = file_date + msec_in_day
	    if (file_date .gt. end_date) goto 295
	    call epoch_breakdown(file_date,year,mon,day,hr,min,sec,msec)
	    write(cdf_name,230) year,mon,day
            call cdf_open(cdf_name,cdf_id,status)
	    if (status.le.cdf_warn) then
	      do 290 i = 2,9
	        write (cdf_name(34:34), '(I1)') i
	        call cdf_open(cdf_name,cdf_id,status)
	        if (status.eq.cdf_ok) exit
 290	      continue
	    end if                              !status.le.cdf_warn
C
C Write out name of new file.
C Get num_recs for new file.
C Assume variable numbers for other quantities we want do not change.
C
	    if (status .eq. cdf_ok) then
	      write(*,*) cdf_name
     	      call cdf_inquire(cdf_id,num_dims,dim_sizes,encoding,majority,
     .         num_recs,num_vars,num_attrs,status)
	    endif
C
	    if (status.ne.cdf_ok) then
	      write(*,*) ' cannot open ',cdf_name,' skipping...'
	      call cdf_close(cdf_id,status)
	      missing_days = missing_days + 1
              if (missing_days .gt. max_missing_days) goto 295
	      goto 285
	    end if					!status bad
C
C We successfully opened next cdf file.  Reset missing file counter,
C reinitialize record counter, and loop back to process it.
C
	    missing_days = 0
	    rec_num = 1

	  end do					!rec_epoch .le. end_date
C
C End of main loop
C
C We have gone past the user specified stop time.  There may be a partial
C interval, though; take care of this by a final call to swe_average.
C
 295	  call swe_average
	  call cdf_close(cdf_id,status)
	  CLOSE(200)

	end if					!what_info(3)swe?

C*******************************************************************************
C                                                                              *
C		     Get STEP data and write to output file                    *
C                                                                              *
C*******************************************************************************
	if (what_info(4)) then                        			!STEP

	  interval_boundary = 0
	  file_date = beg_date
	  missing_days = 0
C
C  Open STEP output file and write header information
C
	  OPEN(unit=300, file=outfilename(4), status='NEW',recl=180)
	  write(300,*) ' WIND Key Parameter CDF -> Text translation '
	  write(300,5)run_date, run_time
	  write(300,*) ' STEP Instrument Data '
	  call encode_epoch(beg_date, date1)
	  write(300,*) ' Beginning Date: ', date1
	  call encode_epoch(end_date, date2)
	  write(300,*) ' End Date: ', date2
	  write(300,*) ' Values of -2000.0 are null values (no data code) '
          write(300,*)
	  write(300,*) ' Units: #/(sec-cm^2-sr-(MeV/amu)) '
          write(300,305)
 305	  format(/,2x,'STEP1. He(a)  : He  flux   80-160 keV/n'/,
     .             2x,'STEP2. He(b)  : He  flux  320-640 keV/n'/,
     .             2x,'STEP3. CNO(a) : CNO flux   80-160 keV/n'/
     .             2x,'STEP4. CNO(b) : CNO flux  320-160 keV/n'/
     .             2x,'STEP5. Fe(a)  : Fe  flux   80-160 keV/n'/
     .             2x,'STEP6. Fe(b)  : Fe  flux 640-1050 keV/n'/)

	  write(300,315)
 315      format(/,' Year DOY:HR:MN:SC          He(a)          He(b)        CNO(a)',
     .                                  '        CNO(b)        Fe(a)         Fe(b) ')
C
C Open EPACT CDF file which contains STEP data.
C
C First convert Epoch start time to regular time, then use it to construct
C cdf filename.  If open fails, check for higher data versions (up to V09).
C If there aren't any, increment file_date by one day and access next day's cdf
C file.
C
 320	  if (file_date .gt. end_date) goto 395
 	  call epoch_breakdown(file_date,year,mon,day,hr,min,sec,msec)
	  write(cdf_name,330) year,mon,day
 330	  format('wind_kp_epa:wi_k0_epa_',I4.4,I2.2,I2.2,'_v01')
	  call cdf_open(cdf_name,cdf_id,status)
C
C If data version V01 does NOT exist, increment version number up to 09.
C
	  if (status.le.cdf_warn) then
	    do 335 i = 2,9
	      write (cdf_name(34:34), '(I1)') i
	      call cdf_open(cdf_name,cdf_id,status)
	      if (status.eq.cdf_ok) exit
 335	    continue
	  end if
	  if (status.eq.cdf_ok) then
	    write(*,*) cdf_name
	  else
	    write(*,*) ' cannot open ',cdf_name,' skipping day'
	    missing_days = missing_days + 1
            if (missing_days .gt. max_missing_days) goto 395
	    file_date = file_date + msec_in_day
	    goto 320
	  endif
	  missing_days = 0
C
C  Get CDF file ID, maximum number of records, etc., for current CDF file
C
	  call cdf_inquire(cdf_id,num_dims,dim_sizes,encoding,majority,
     .      num_recs,num_vars,num_attrs,status)
C
C  If there is a problem with the CDF file or it does not exist then
C  open file for next day.
C
	  if (status .ne. cdf_ok) then
	    write(*,*) ' problem with cdf_inquire... '
	    call cdf_error(status,message)
	    write(*,*) message
	    call cdf_close(cdf_id,status)
	    file_date = file_date + msec_in_day
	    goto 320
	  end if					!status bad
C
C  Get variable number for different data.
C
	  vn_epoch = cdf_var_num(cdf_id,'Epoch')
	  vn_step1 = cdf_var_num(cdf_id,'STEP1')
	  vn_step2 = cdf_var_num(cdf_id,'STEP2')
	  vn_step3 = cdf_var_num(cdf_id,'STEP3')
	  vn_step4 = cdf_var_num(cdf_id,'STEP4')
	  vn_step5 = cdf_var_num(cdf_id,'STEP5')
	  vn_step6 = cdf_var_num(cdf_id,'STEP6')

C
C	****************  Get STEP data ******************
C
C Get first interval Start time
C
	  indices(1) = 1
	  rec_num = 1
	  call cdf_var_get(cdf_id,vn_epoch,rec_num,indices(1),rec_epoch,
     .         status)
C
C Read through file until we reach a record that is on or after specified
C start time (beg_date) or we reach end of file.
C
 	  do while ((rec_num .lt. num_recs) .and.
     .	            (rec_epoch .lt. beg_date) .and.
     .	            (rec_epoch .lt. end_date) .and.
     .	            (status .eq. cdf_ok))
	    indices(1) = 1
	    rec_num = rec_num + 1
	    call cdf_var_get(cdf_id,vn_epoch,rec_num,indices(1),rec_epoch,
     .         status)
	  end do
C
C If we got an error or we read through entire file without reaching the
C start time, ignore file and advance to next day, try again.  Note, though,
C that this should never happen, since we chose a cdf file on the correct date
C to start with.
C
	  if (status .ne. cdf_ok .or. rec_epoch .lt. beg_date) then
	    file_date = file_date + msec_in_day
	    call cdf_close(cdf_id,status)
	    goto 320
	  endif
C
C Calculate interval boundary if nonzero interval.  If ultimate mode, leave
C interval_boundary zero.
C
C Usually beg_date is start of first interval.  If we skipped one or more days
C of data, however, or if the first file is missing some data and the interval
C is shorter than a day, it is possible that we are missing one or more entire
C intervals, and thus the normal computation for the end of interval would be
C incorrect.  The loop below repeatedly increases interval_boundary by
C rinterval until it reaches its largest value that is still less than the
C first time in the file after beg_date; i.e., the following hold:
C
C	first record to process is i
C	if i > 1 then time for record i-1 < beg_date < time for record i
C	if i = 1 then beg_date < time for record i
C	if interval > 0 then interval_boundary < time for record i < interval_boundary+rinterval
C
C This initializing logic will force the end of interval to be detected while
C processing the first record, and thus cause the values for that record to
C be written out, after which interval_boundary will be adjusted forward by
C rinterval.
C
	  interval_boundary = 0
	  if (interval .ne. 0) then
	    interval_boundary = beg_date
	    do while (interval_boundary + rinterval .lt. rec_epoch)
	      interval_boundary = interval_boundary + rinterval
	    end do
	  endif
C
C Start main loop, keep going until we reach stop time (end_date)
C
 	  do while (rec_epoch .le. end_date)
C
C Start processing loop for this file, keep going until we run out of records
C in the file.  Within this loop test for end of interval condition.
C
	    do while (rec_num .le. num_recs)
C
C Get epoch time for data and convert to year,doy,etc
C
	      indices(1) = 1

	      call cdf_var_get(cdf_id,vn_epoch,rec_num,indices(1),rec_epoch,
     .           status)

	      call epoch_breakdown(rec_epoch,year,mon,day,hr,min,sec,msec)
	      call compute_epoch(year,0,1,0,0,0,0,epoch_temp)
	      doy = int((rec_epoch - epoch_temp)/(msec_in_day)) + 1
C
C Check if end of interval or past stop date.
C If we are dumping every value (ultimate resolution), treat as if always
C end of interval.
C
	      if ((rec_epoch .ge. interval_boundary) .or.
     .		  (rec_epoch .ge. end_date) .or.
     .		  (interval .eq. 0)) then
C
C We are past the end of interval, which means we are at the start of a new
C interval
C
C Call internal subroutine to extract and process STEP (EPA) data
C
	        call get_step_data(status)
C
C Write all values out to file
C
	        if (status .eq. 0) then
 360	          format(1X,I4,1X,I3,3(':',I2.2),3X,6(1X,F13.5))
	          write(300,360) year,doy,hr,min,sec,step1,step2,step3,
     .                step4,step5,step6
		endif
C
C Advance interval boundary (end of interval) by exactly the amount
C the user specified (stored in rinterval).  This way there will be no "creep"
C in the interval start time.
C
C In case there is a large stretch of missing data, keep advancing interval
C boundary until it is within rinterval of the next data point (rec_epoch).
C
C If we are in ultimate mode (all data points), don't bother to advance
C interval_boundary, leave it 0 to force dump of every data point.
C
	        if (interval .ne. 0) then
	          interval_boundary = interval_boundary + rinterval
	          do while (interval_boundary .lt. rec_epoch)
	            interval_boundary = interval_boundary + rinterval
	          end do
	        endif
C
                if(rec_epoch .ge. end_date) goto 395
	      end if
C
C Done processing this record
C Advance to next record in cdf file
C Loop back to check if we are at maxrecs, and if so drop out of inner WHILE
C loop.
C
 375	      rec_num = rec_num + 1
	    end do 			! rec_num .le. num_recs
C
C We exited inner loop, which means we've run out of records in the cdf file.
C Close old cdf and open new one, the go back to inner loop to process data.
C
 380	    call cdf_close(cdf_id,status)
 385	    file_date = file_date + msec_in_day
	    if (file_date .gt. end_date) goto 395
	    call epoch_breakdown(file_date,year,mon,day,hr,min,sec,msec)
	    write(cdf_name,330) year,mon,day
            call cdf_open(cdf_name,cdf_id,status)
	    if (status.le.cdf_warn) then
	      do 390 i = 2,9
	        write (cdf_name(34:34), '(I1)') i
	        call cdf_open(cdf_name,cdf_id,status)
	        if (status.eq.cdf_ok) exit
 390	      continue
	    end if                              !status.le.cdf_warn
C
C Write out name of new file.
C Get num_recs for new file.
C Assume variable numbers for other quantities we want do not change.
C
	    if (status .eq. cdf_ok) then
	      write(*,*) cdf_name
     	      call cdf_inquire(cdf_id,num_dims,dim_sizes,encoding,majority,
     .         num_recs,num_vars,num_attrs,status)
	    endif
C
	    if (status.ne.cdf_ok) then
	      write(*,*) ' cannot open ',cdf_name,' skipping...'
	      call cdf_close(cdf_id,status)
	      missing_days = missing_days + 1
              if (missing_days .gt. max_missing_days) goto 395
	      goto 385
	    end if					!status bad
C
C We successfully opened next cdf file.  Reset missing file counter,
C reinitialize record counter, and loop back to process it.
C
	    missing_days = 0
	    rec_num = 1

	  end do				!rec_epoch .le. end_date

 395	  call cdf_close(cdf_id,status)
	  CLOSE(300)

	end if					!what_info(4)step?

	write(*,*) ' Output filename is:'
	do i=1,4
	  if (what_info(i)) write(*,401) outfilename(i)
 401	  format(1x,a)
	end do

	return

	contains
	subroutine get_mfi_data(status)
	implicit none
	integer status
C
C  get first set of vectors
C
!	call cdf_var_get(cdf_id,vn_bgsmc,rec_num,indices(1),bgsmc(1),
!     .	        status)
!	if (status.ne.cdf_ok) then
!	  which_part = 1
!	  goto 50
!	end if
!	call cdf_var_get(cdf_id,vn_bgsma,rec_num,indices(1),bgsma(1),
!     .         status)
!	if (status.ne.cdf_ok) then
!	  which_part = 2
!	  goto 50
!	end if
C	call cdf_var_get(cdf_id,vn_bgsec,rec_num,indices(1),bgsec(1),
C     .          status)
	CALL CDF_lib (SELECT_, CDF_, cdf_id,
     .		      zVAR_, vn_bgsec,
     .		      zVAR_RECNUMBER_, rec_num,
     .		      zVAR_DIMINDICES_, indices(1),
     .		      GET_, zVAR_DATA_, bgsec(1),
     .		      NULL_, status)
	if (status.ne.cdf_ok) then
	  which_part = 3
	  goto 50
	end if
	if(abs(bgsec(1)).gt.500.0) bgsec(1) = -2000.0		       	!flag bad values
C	      call cdf_var_get(cdf_id,vn_bgsea,rec_num,indices(1),bgsea(1),
C     .          status)
	CALL CDF_lib (SELECT_, CDF_, cdf_id,
     .		      zVAR_, vn_bgsea,
     .		      zVAR_RECNUMBER_, rec_num,
     .		      zVAR_DIMINDICES_, indices(1),
     .		      GET_, zVAR_DATA_, bgsea(1),
     .		      NULL_, status)
	if (status.ne.cdf_ok) then
	  which_part = 4
	  goto 50
	end if
	if(abs(bgsea(1)).gt.500.0) bgsea(1) = -2000.0			!flag bad values
C
C  get second set of vectors
C
	indices(1) = 2
!	call cdf_var_get(cdf_id,vn_bgsmc,rec_num,indices(1),bgsmc(2),
!     .	        status)
!	if (status.ne.cdf_ok) then
!	  which_part = 5
!	  goto 50
!	end if
!	call cdf_var_get(cdf_id,vn_bgsma,rec_num,indices(1),bgsma(2),
!     .         status)
!	if (status.ne.cdf_ok) then
!	  which_part = 6
!	  goto 50
!	end if
C	call cdf_var_get(cdf_id,vn_bgsec,rec_num,indices(1),bgsec(2),
C     .          status)
	CALL CDF_lib (SELECT_, CDF_, cdf_id,
     .		      zVAR_, vn_bgsec,
     .		      zVAR_RECNUMBER_, rec_num,
     .		      zVAR_DIMINDICES_, indices(1),
     .		      GET_, zVAR_DATA_, bgsec(2),
     .		      NULL_, status)
	if (status.ne.cdf_ok) then
	  which_part = 7
	  goto 50
	end if
	if(abs(bgsec(2)).gt.500.0) bgsec(2) = -2000.0                    !flag bad values
C
C	call cdf_var_get(cdf_id,vn_bgsea,rec_num,indices(1),bgsea(2),
C     .          status)
	CALL CDF_lib (SELECT_, CDF_, cdf_id,
     .		      zVAR_, vn_bgsea,
     .		      zVAR_RECNUMBER_, rec_num,
     .		      zVAR_DIMINDICES_, indices(1),
     .		      GET_, zVAR_DATA_, bgsea(2),
     .		      NULL_, status)
	if (status.ne.cdf_ok) then
	  which_part = 8
	  goto 50
	end if
	if(abs(bgsea(2)).gt.370.0) bgsea(2) = -2000.0			!flag bad values
C
C get third set
C
	indices(1) = 3
!	call cdf_var_get(cdf_id,vn_bgsmc,rec_num,indices(1),bgsmc(3),
!     .	        status)
!	if (status.ne.cdf_ok) then
!	  which_part = 9
!	  goto 50
!	end if
!	call cdf_var_get(cdf_id,vn_bgsma,rec_num,indices(1),bgsma(3),
!     .         status)
!	if (status.ne.cdf_ok) then
!	  which_part = 10
!	  goto 50
!	end if
C	call cdf_var_get(cdf_id,vn_bgsec,rec_num,indices(1),bgsec(3),
C     .          status)
	CALL CDF_lib (SELECT_, CDF_, cdf_id,
     .		      zVAR_, vn_bgsec,
     .		      zVAR_RECNUMBER_, rec_num,
     .		      zVAR_DIMINDICES_, indices(1),
     .		      GET_, zVAR_DATA_, bgsec(3),
     .		      NULL_, status)
	if (status.ne.cdf_ok) then
	  which_part = 11
	  goto 50
	end if
	if(abs(bgsec(3)).gt.500.0) bgsec(3) = -2000.0			!flag bad values
C
C	call cdf_var_get(cdf_id,vn_bgsea,rec_num,indices(1),bgsea(3),
C     .          status)
	CALL CDF_lib (SELECT_, CDF_, cdf_id,
     .		      zVAR_, vn_bgsea,
     .		      zVAR_RECNUMBER_, rec_num,
     .		      zVAR_DIMINDICES_, indices(1),
     .		      GET_, zVAR_DATA_, bgsea(3),
     .		      NULL_, status)
	if (status.ne.cdf_ok) then
	  which_part = 12
	  goto 50
	end if
	if(abs(bgsea(3)).gt.370.0) bgsea(3) = -2000.0                   !flag bad values
C
C Success, return
C
	status = 0
	return
C
C Come here for error
C
50	write(*,*) ' var get failed.  advancing record '
	call cdf_error(status,message)
	write(*,*) message
	status = -1
	return
	end subroutine get_mfi_data

	subroutine get_3dp_data(status)
	implicit none
	integer status
C
C  get first set of vectors
C
	call cdf_var_get(cdf_id,vn_edens,rec_num,tdp_index,elect_dens,
     .	       status)
	if (status.ne.cdf_ok) then
	  which_part = 1
	  goto 150
	end if
	if ((elect_dens.lt.0).or.(elect_dens.gt.500.0)) elect_dens = -2000.0	!flag bad values
	call cdf_var_get(cdf_id,vn_evel,rec_num,tdp_index,elect_vel(1),
     .         status)
	if (status.ne.cdf_ok) then
	  which_part = 2
	  goto 150
	end if
	if (ABS(elect_vel(1)) .gt. 5000) elect_vel(1) = -2000.0			!flag bad values
	call cdf_var_get(cdf_id,vn_etemp,rec_num,tdp_index,elect_temp,
     .         status)
	if (status.ne.cdf_ok) then
	  which_part = 3
	  goto 150
	end if
	if ((elect_temp.lt.0).or.(elect_temp.gt.5000.0)) elect_temp = -2000.0	!flag bad values
!	call cdf_var_get(cdf_id,vn_eqdotb,rec_num,tdp_index,elect_qdotb,
!     .         status)
!	if (status.ne.cdf_ok) then
!	  which_part = 4
!	  goto 150
!	end if
	call cdf_var_get(cdf_id,vn_iondens,rec_num,tdp_index,ion_dens,
     .         status)
	if (status.ne.cdf_ok) then
	  which_part = 5
	  goto 150
	end if
	if ((ion_dens.lt.0).or.(ion_dens.gt.500.0)) ion_dens = -2000.0     		!flag bad values
	call cdf_var_get(cdf_id,vn_ionvel,rec_num,tdp_index,ion_vel(1),
     .         status)
	if (status.ne.cdf_ok) then
	  which_part = 6
	  goto 150
	end if
	if (ABS(ion_vel(1)) .gt. 5000) ion_vel(1) = -2000.0                     	!flag bad values
	call cdf_var_get(cdf_id,vn_iontemp,rec_num,tdp_index,ion_temp,
     .         status)
	if (status.ne.cdf_ok) then
	  which_part = 7
	  goto 150
	end if
	if ((ion_temp.lt.0).or.(ion_temp.gt.500.0)) ion_temp = -2000.0		!flag bad values
C
C  get second set of vectors
C
	tdp_index(1) = 2
	call cdf_var_get(cdf_id,vn_evel,rec_num,tdp_index,elect_vel(2),
     .         status)
	if (status.ne.cdf_ok) then
	  which_part = 8
	  goto 150
	end if
	if (ABS(elect_vel(2)) .gt. 5000) elect_vel(2) = -2000.0			!flag bad values
	call cdf_var_get(cdf_id,vn_ionvel,rec_num,tdp_index,ion_vel(2),
     .         status)
	if (status.ne.cdf_ok) then
	  which_part = 9
	  goto 150
	end if
	if (ABS(ion_vel(2)) .gt. 5000) ion_vel(2) = -2000.0				!flag bad values
C
C get third set
C
	tdp_index(1) = 3
	call cdf_var_get(cdf_id,vn_evel,rec_num,tdp_index,elect_vel(3),
     .         status)
	if (status.ne.cdf_ok) then
	  which_part = 10
	  goto 150
	end if
	if (ABS(elect_vel(3)) .gt. 5000) elect_vel(3) = -2000.0			!flag bad values
	call cdf_var_get(cdf_id,vn_ionvel,rec_num,tdp_index,ion_vel(3),
     .         status)
	if (status.ne.cdf_ok) then
	  which_part = 11
	  goto 150
	end if
	if (ABS(ion_vel(3)) .gt. 5000) ion_vel(3) = -2000.0            		!flag bad values
C
	status = 0
	return
C
C Error detected, report it
C
150	write(*,*) ' var get failed.  advancing record '
	call cdf_error(status,message)
	write(*,*) message
	status = -1
	return
	end subroutine get_3dp_data

	subroutine sum_swe_data(status)
	implicit none
	integer status
C
C Get GSE position information
C
	call cdf_var_get(cdf_id,vn_position,rec_num,indices(1),position_gse(1),
     .     status)
	if (status.ne.cdf_ok) then
	  which_part = 4
	  goto 250
	end if
	if(abs(position_gse(1)).gt. 2.5e+06) then
	  goto 260
        else
 	  position_gse(1) = position_gse(1)/Re                      !Convert to Earth Radii
	  swe_data(4) = swe_data(4) + position_gse(1)
	  swe_hits(4) = swe_hits(4) + 1.0
	endif
        
	indices(1) = 2
	call cdf_var_get(cdf_id,vn_position,rec_num,indices(1),position_gse(2),
     .     status)
	if (status.ne.cdf_ok) then
	  which_part = 8
	  goto 250
	end if
	if(abs(position_gse(2)).gt. 2.5e+06) then
	  goto 260
        else
          position_gse(2) = position_gse(2)/Re                      !Convert to Earth Radii
	  swe_data(8) = swe_data(8) + position_gse(2)
	  swe_hits(8) = swe_hits(8) + 1.0
	endif
        
	indices(1) = 3
	call cdf_var_get(cdf_id,vn_position,rec_num,indices(1),position_gse(3),
     .     status)
	if (status.ne.cdf_ok) then
	  which_part = 12
	  goto 250
	end if
	if(abs(position_gse(3)).gt. 2.5e+06) then
	  goto 260
        else
	  position_gse(3) = position_gse(3)/Re                      !Convert to Earth Radii
	  swe_data(12) = swe_data(12) + position_gse(3)
	  swe_hits(12) = swe_hits(12) + 1.0
	endif
C       
C Get SWE data: get first vector components and scalar values
C
	indices(1) = 1

	call cdf_var_get(cdf_id,vn_vgse,rec_num,indices(1),v_gse(1),
     .     status)
	if (status.ne.cdf_ok) then
	  which_part = 0
	  goto 250
	end if
	if(abs(v_gse(1)).gt. 1100.0) then
	  goto 260
	else
	  swe_data(0) = swe_data(0) + v_gse(1)
	  swe_hits(0) = swe_hits(0) + 1.0
        endif
        
	call cdf_var_get(cdf_id,vn_vgsm,rec_num,indices(1),v_gsm(1),
     .     status)
	if (status.ne.cdf_ok) then
	  which_part = 1
	  goto 250
	end if
	if(abs(v_gsm(1)).gt. 1100.0) then
	  goto 260
	else
	  swe_data(1) = swe_data(1) + v_gsm(1)
	  swe_hits(1) = swe_hits(1) + 1.0
        endif
        
	call cdf_var_get(cdf_id,vn_vth,rec_num,indices(1),vtherm,
     .     status)
	if (status.ne.cdf_ok) then
	  which_part = 2
	  goto 250
	end if
	if((vtherm .lt. 0).or.(vtherm.gt. 200.0)) then
	  goto 260
	else
	  swe_data(2) = swe_data(2) + vtherm
	  swe_hits(2) = swe_hits(2) + 1.0
        endif
        
	call cdf_var_get(cdf_id,vn_protdens,rec_num,indices(1),prot_dens,
     .     status)
	if (status.ne.cdf_ok) then
	  which_part = 3
	  goto 250
	end if
	if((prot_dens .lt. 0).or.(prot_dens .gt. 200.0)) then
	  goto 260
	else
	  swe_data(3) = swe_data(3) + prot_dens
	  swe_hits(3) = swe_hits(3) + 1.0
        endif
        
	call cdf_var_get(cdf_id,vn_vgse_p,rec_num,indices(1),v_gse_p(1),
     .     status)
	if (status.ne.cdf_ok) then
	  which_part = 5
	  goto 250
	end if
	if((v_gse_p(1) .lt. 100.0) .or. (v_gse_p(1).gt. 1100.0)) then
	  goto 260
	else
	  swe_data(5) = swe_data(5) + v_gse_p(1)
	  swe_hits(5) = swe_hits(5) + 1.0
        endif
C       
C Get second vector components
C
        indices(1) = 2
	call cdf_var_get(cdf_id,vn_vgse,rec_num,indices(1),v_gse(2),
     .     status)
	if (status.ne.cdf_ok) then
	  which_part = 6
	  goto 250
	end if
	if(abs(v_gse(2)).gt. 1100.0) then
	  goto 260
	else
	  swe_data(6) = swe_data(6) + v_gse(2)
	  swe_hits(6) = swe_hits(6) + 1.0
        endif
        
	call cdf_var_get(cdf_id,vn_vgsm,rec_num,indices(1),v_gsm(2),
     .     status)
	if (status.ne.cdf_ok) then
	  which_part = 7
	  goto 250
	end if
	if(abs(v_gsm(2)).gt. 1100.0) then
	  goto 260
	else
	  swe_data(7) = swe_data(7) + v_gsm(2)
	  swe_hits(7) = swe_hits(7) + 1.0
        endif
        
	call cdf_var_get(cdf_id,vn_vgse_p,rec_num,indices(1),v_gse_p(2),
     .     status)
	if (status.ne.cdf_ok) then
	  which_part = 9
	  goto 250
	end if
	if(abs(v_gse_p(2)).gt. 50.0) then
	  goto 260
	else
	  swe_data(9) = swe_data(9) + v_gse_p(2)
	  swe_hits(9) = swe_hits(9) + 1.0
        endif
C       
C Get third vector components
C
	indices(1) = 3
	call cdf_var_get(cdf_id,vn_vgse,rec_num,indices(1),v_gse(3),
     .     status)
	if (status.ne.cdf_ok) then
	  which_part = 10
	  goto 250
	end if
	if(abs(v_gse(3)).gt. 1100.0) then
	  goto 260
	else
	  swe_data(10) = swe_data(10) + v_gse(3)
	  swe_hits(10) = swe_hits(10) + 1.0
        endif
        
	call cdf_var_get(cdf_id,vn_vgsm,rec_num,indices(1),v_gsm(3),
     .     status)
	if (status.ne.cdf_ok) then
	  which_part = 11
	  goto 250
	end if
	if(abs(v_gsm(3)).gt. 1100.0) then
	  goto 260
	else
	  swe_data(11) = swe_data(11) + v_gsm(3)
	  swe_hits(11) = swe_hits(11) + 1.0
        endif
        
	call cdf_var_get(cdf_id,vn_vgse_p,rec_num,indices(1),v_gse_p(3),
     .     status)
	if (status.ne.cdf_ok) then
	  which_part = 13
	  goto 250
	end if
	if(abs(v_gse_p(3)).gt. 50.0) then
	  goto 260
	else
	  swe_data(13) = swe_data(13) + v_gse_p(3)
	  swe_hits(13) = swe_hits(13) + 1.0
        endif
	status = 1
	return
C
C Error handling code in case cdf_get_var fails
C Print message and advance one record
C
 250 	rec_num = rec_num + 1
	write(*,*) ' var get failed.  advancing record '
	call cdf_error(status,message)
	write(*,*) message,' which = ',which_part
	status = 2
	return
C
C If one SWE value is bad, flag all SWE values with -2000.0
C
 260	v_gse   = -2000.0		!Array assignment
        v_gsm   = -2000.0
	v_gse_p = -2000.0
	vtherm = -2000.0
	prot_dens = -2000.0
	status = 1
	return
	end subroutine sum_swe_data

	subroutine swe_average
	implicit none
	integer i
C
	if (any(swe_hits .ne. 0)) then
	  do i = 0,13
	    if (swe_hits(i) .gt. 0) then
	      swe_data_avg(i) = swe_data(i)/swe_hits(i)	!average SWE data over interval
            else
	      swe_data_avg(i) = -2000.0
	    endif
          end do
	  if (swe_sample .eq. 1) then
 271        format(1X,I4,1X,I3,3(':',I2.2),3X,3(1X,F8.3),5(1X,F8.2),1X,I8)
            write(200,271) year,doy,hr,min,sec,(position_gse(i),i=1,3),
     .			v_gse_p(1),v_gse_p(3),v_gse_p(2),vtherm,prot_dens
          else
            write(200,271) st_year,st_doy,st_hr,st_min,st_sec,(swe_data_avg(4*i),i=1,3),
     .		        swe_data_avg(5),swe_data_avg(13),swe_data_avg(9),swe_data_avg(2),
     .                  swe_data_avg(3),int(swe_hits(0))
          endif
        endif
	return
	end subroutine swe_average

	subroutine get_step_data(status)
	implicit none
	integer status
C
C  get STEP fluxes with valid range 1.0E-05 to 1.0E+05.
C
	call cdf_var_get(cdf_id,vn_step1,rec_num,indices(1),step1,
     .         status)
	if (status.ne.cdf_ok) then
	  which_part = 1
	  goto 350
	end if
C	if((step1 .lt. 0).or.(step1.gt. 1.0e+05)) step1 = -2000.0  			!flag bad values
	if((step1 .lt. 0)) step1 = -2000.0
C
	call cdf_var_get(cdf_id,vn_step2,rec_num,indices(1),step2,
     .         status)
	if (status.ne.cdf_ok) then
	  which_part = 2
	  goto 350
	end if
C	if((step2 .lt. 0).or.(step2.gt. 1.0e+05)) step2 = -2000.0			!flag bad values
	if((step2 .lt. 0)) step2 = -2000.0
C
	call cdf_var_get(cdf_id,vn_step3,rec_num,indices(1),step3,
     .         status)
	if (status.ne.cdf_ok) then
	  which_part = 3
	  goto 350
	end if
C	if((step3 .lt. 0).or.(step3.gt. 1.0e+05)) step3 = -2000.0			!flag bad values
	if((step3 .lt. 0)) step3 = -2000.0
C
	call cdf_var_get(cdf_id,vn_step4,rec_num,indices(1),step4,
     .         status)
	if (status.ne.cdf_ok) then
	  which_part = 4
	  goto 350
	end if
C	if((step4 .lt. 0).or.(step4.gt. 1.0e+05)) step4 = -2000.0			!flag bad values
	if((step4 .lt. 0)) step4 = -2000.0
C
	call cdf_var_get(cdf_id,vn_step5,rec_num,indices(1),step5,
     .         status)
	if (status.ne.cdf_ok) then
	  which_part = 5
	  goto 350
	end if
C	if((step5 .lt. 0).or.(step5.gt. 1.0e+05)) step5 = -2000.0			!flag bad values
	if((step5 .lt. 0)) step5 = -2000.0

	call cdf_var_get(cdf_id,vn_step6,rec_num,indices(1),step6,
     .         status)
	if (status.ne.cdf_ok) then
	  which_part = 6
	  goto 350
	end if
C	if((step6 .lt. 0).or.(step6.gt. 1.0e+05)) step6 = -2000.0			!flag bad values
	if((step6 .lt. 0)) step6 = -2000.0
C
	status = 0
	return
C
C Come here for errors
C
350	write(*,*) ' var get failed.  advancing record '
	call cdf_error(status,message)
	write(*,*) message,' which = ',which_part
	status = -1
	return
	end subroutine get_step_data
	end
