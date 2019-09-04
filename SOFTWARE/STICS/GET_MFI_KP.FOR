	SUBROUTINE get_mfi_kp (in_yr,in_mon,in_day,in_hr,in_min,in_sec,		!input
     .   			position_gse,bgsea,bgsec,bgsma,bgsmc,deltaT)	!output
C
C	The minimum time between MFI key parameter data points is ~45 secs
C 	in High Bit Rate and ~90 secs in Low Bit Rate.
C	This routine will open up the cdf file specified by the input date
C	and return the MFI key parameter data for the given time +/- 45 secs.
C
C	Routine must be linked to the following libraries:
C                  CDF$LIB:LIBCDF/LIBRARY, SYS$LIBRARY:VAXCRTL/LIB
C
C	Created by 	 16-OCT-1997	BY 	K.CHOTOO
C
C	Revision History:
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	IMPLICIT NONE
	INCLUDE 'CDF$INC:CDF.INC'
	REAL*8 beg_date,		!Start Epoch time
     .         dec_year, 
     .         end_date,		!End Epoch time 
     .         epoch_temp,		!Temporary Epoch time
     .         interval_boundary,       !Time interval boundary
     .         mfi_date,		!Start of day MFI Epoch time
     .         mfi_epoch,		!MFI current Epoch time
     .         mfi_epoch0,mfi_epoch1,in_epoch,
     .	       mfi_time
c
	REAL*4 deltaT,deltaT0,deltaT1,  !Time difference
     .	       bgsea(3),		!B-field in GSE spherical coordinates
     .         bgsec(3),		!B-field in GSE cartesian coordinates
     .         bgsma(3),		!B-field in GSM spherical coordinates
     .         bgsmc(3),		!B-field in GSM cartesian coordinates
     .         interval,		!Sampling interval in minutes
     .         pi/3.141593/,
     .         position_gse(3),		!Cartesian position in GSE coordinates, Re
     .	       Re/6380.0/		!Earth Radius in km
c
	INTEGER*4 cdf_id,
     .            dim_sizes,
     .            encoding,		!Encoding of CDF file
     .            i,j,k,		!Counters
     .            indices(1),
     .            majority,
     .            mfi_doy,		!MFI day of year
     . 		  mfi_id,		!Unit number of CDF opened by CDF_OPEN
     .            mfi_max,              !Maximum number of records in MFI CDF
     .            mfi_rec,		!Current record number in CDF file 
     .            mfi_which /0/,
     .            num_attrs, 		!Number of attributes in CDF file
     .            num_dims,		
     .            num_vars,		!Number of variables in CDF
     .            rec_num,
     .            status, 
     .            var_num_bgsea,        !var_num = vn = variable number for use
     .            var_num_bgsec,        !in getting values with CDF_VAR_GET
     .            var_num_bgsma,
     .            var_num_bgsmc,
     .            var_num_epoch,
     .            var_num_pgse

	INTEGER*4 in_yr,in_mon,in_day,in_hr,in_min,in_sec,
     .            year,mon,day,hr,min,sec,msec,low,high
	
        CHARACTER*36 cdf_name                         	!Name of CDF files
	CHARACTER*(EPOCH_STRING_LEN) date1, date2       !Character string for output of Epoch time
	CHARACTER*(cdf_statustext_len) message		!Error message description
C
C*********************************************************************************
C	                                                                         *
C				Get MFI data                         		 *
C                                                                                *
C*********************************************************************************
C 
C Initialize output variables
C
	do i =1,3
	  position_gse(i) = -3000.0
	  bgsea(i)	  = -3000.0
	  bgsec(i)	  = -3000.0
	  bgsma(i)	  = -3000.0
	  bgsmc(i)	  = -3000.0
	end do
C
C Make sure that input hour is less than 24
C
 	if(in_hr .ge. 24) then
	  in_hr = 23
	  in_min= 59
	  in_sec= 59
	endif
C
C Convert input time to Epoch time. 
C
  	call compute_epoch(in_yr,in_mon,in_day,in_hr,in_min,in_sec,0,in_epoch)
C
C  Open CDF file
C
   20	write(cdf_name,30) in_yr,in_mon,in_day
   30	format('wind_kp_mfi:wi_k0_mfi_',I4.4,I2.2,I2.2,'_v01')
C	write(*,*) cdf_name
	call CDF_lib (OPEN_, CDF_, cdf_name, mfi_id, NULL_, status)	
C
C  If version 01 does NOT exist, increment version number up to 09
C
	if (status.le.cdf_warn) then
C	  write(*,*) ' V01 failed.  Trying other versions '
	  do 35 i = 2,9
	    write (cdf_name(34:34), '(I1)') i
C	    write(*,*) cdf_name
	    CALL CDF_lib (OPEN_, CDF_, cdf_name, mfi_id, NULL_, status)	
	    if (status.eq.cdf_ok) i=35
   35	  continue
	end if
C
C  Get CDF file ID, maximum number of records, etc., from CDF file.
C
	call CDF_lib (SELECT_, CDF_, mfi_id,
     .		      GET_, zVARs_MAXREC_, mfi_max,
     .		      NULL_, status)

C
C  If there is a problem with CDF file or it does not exist then open file
C  for next day.
C
	if (status.ne.cdf_ok) then
	  write(*,*) ' problem with cdf_inquire... '
	  call cdf_error(status,message)
	  write(*,*) message
	  goto 95
	end if					!status bad
C
C  Get variable numbers for different data
C
	CALL CDF_lib (SELECT_, CDF_, mfi_id,
     .  		GET_, zVAR_NUMBER_, 'Epoch', var_num_epoch,
     .			      zVAR_NUMBER_, 'BGSMc', var_num_bgsmc,
     .			      zVAR_NUMBER_, 'BGSMa', var_num_bgsma,
     .			      zVAR_NUMBER_, 'BGSEc', var_num_bgsec,
     .			      zVAR_NUMBER_, 'BGSEa', var_num_bgsea,
     .			      zVAR_NUMBER_, 'PGSE',  var_num_pgse,
     .			NULL_, status)


C
C Get record number for data closest to input time using binary search
C
	low = 0
	high = mfi_max
	indices(1) = 1
	do while (low .le. high)
          rec_num = (low + high)/2
	  CALL CDF_lib (SELECT_, CDF_, mfi_id,
     .			    	  zVAR_, var_num_epoch,
     .				  zVAR_RECNUMBER_, rec_num,
     .				  zVAR_DIMINDICES_, indices(1),
     .			    GET_, zVAR_DATA_, mfi_epoch,
     .			    NULL_, status)

	  if(in_epoch .eq. mfi_epoch) then
	    mfi_time = mfi_epoch
            mfi_rec = rec_num
	    deltaT = 0
	    goto 45
	  else if(in_epoch .lt. mfi_epoch) then
            high = rec_num - 1
    	  else
   	    low = rec_num + 1
      	  endif
	end do 			!low .le. high

	deltaT = abs(in_epoch - mfi_epoch) / 1000.0
        mfi_time = mfi_epoch
        mfi_rec = rec_num
C
C Check first higher time to see if it is closer to requested time
C
        if(rec_num .lt. mfi_max) then
	  CALL CDF_lib (SELECT_, CDF_, mfi_id,
     .			    zVAR_, var_num_epoch,
     .			    zVAR_RECNUMBER_, rec_num+1,
     .			    zVAR_DIMINDICES_, indices(1),
     .		       GET_, zVAR_DATA_, mfi_epoch1,
     .		       NULL_, status)

          deltaT1 = abs(in_epoch - mfi_epoch1) / 1000.0
	  if(deltaT1 .lt. deltaT) then
            mfi_rec = rec_num + 1
            mfi_time = mfi_epoch1
	    deltaT = deltaT1
          endif
        endif
C
C Check first lower time to see if it is closer to requested time
C
        if(rec_num .gt. 0) then
	  CALL CDF_lib (SELECT_, CDF_, mfi_id,
     .			      zVAR_, var_num_epoch,
     .			      zVAR_RECNUMBER_, rec_num-1,
     .			      zVAR_DIMINDICES_, indices(1),
     .		       GET_, zVAR_DATA_, mfi_epoch0,
     .		       NULL_, status)

          deltaT0 = abs(in_epoch - mfi_epoch0) / 1000.0
	  if(deltaT0 .lt. deltaT) then
            mfi_rec = rec_num - 1
            mfi_time = mfi_epoch0
	    deltaT = deltaT0
          endif
        endif
C
C ************** Get Position information first *****************
C
	indices(1) = 1
	CALL CDF_lib (SELECT_, CDF_, mfi_id,
     .			    zVAR_, var_num_pgse,
     .			    zVAR_RECNUMBER_, mfi_rec,
     .			    zVAR_DIMINDICES_, indices(1),
     .		      GET_, zVAR_DATA_, position_gse(1),
     .		      NULL_, status)
	if (status.ne.cdf_ok) then
	  mfi_which = 0
	  write(*,*) ' var get failed.'
	  call cdf_error(status,message)
	  write(*,*) message,' which = ',mfi_which
	end if
	if(abs(position_gse(1)).gt. 260.0) goto 40		!Values already in Earth Radii
C
	indices(1) = 2
	CALL CDF_lib (SELECT_, CDF_, mfi_id,
     .			    zVAR_, var_num_pgse,
     .			    zVAR_RECNUMBER_, mfi_rec,
     .			    zVAR_DIMINDICES_, indices(1),
     .		      GET_, zVAR_DATA_, position_gse(2),
     .		      NULL_, status)
	if (status.ne.cdf_ok) then
	  mfi_which = 1
	  write(*,*) ' var get failed.'
	  call cdf_error(status,message)
	  write(*,*) message,' which = ',mfi_which
	end if
	if(abs(position_gse(2)).gt. 260.0) goto 40		
C	    
	indices(1) = 3
	CALL CDF_lib (SELECT_, CDF_, mfi_id,
     .			    zVAR_, var_num_pgse,
     .			    zVAR_RECNUMBER_, mfi_rec,
     .			    zVAR_DIMINDICES_, indices(1),
     .		      GET_, zVAR_DATA_, position_gse(3),
     .		      NULL_, status)
	if (status.ne.cdf_ok) then
	  mfi_which = 2
	  write(*,*) ' var get failed.'
	  call cdf_error(status,message)
	  write(*,*) message,' which = ',mfi_which
	end if
	if(abs(position_gse(3)).gt. 260.0) goto 40		

        goto 45		!if position values are good
C
C If one position component is bad, flag all three components
C
  40 	do i =1,3
	  position_gse(i) = -2000.0
	end do
C
C	************* Now get MFI data at specified record number ******************
C
C  get first set of vectors
C
  45  	indices(1) = 1	
	CALL CDF_lib (SELECT_, CDF_, mfi_id,
     .			    zVAR_, var_num_bgsmc,
     .			    zVAR_RECNUMBER_, mfi_rec,
     .			    zVAR_DIMINDICES_, indices(1),
     .		      GET_, zVAR_DATA_, bgsmc(1),
     .		      NULL_, status)
	if (status.ne.cdf_ok) then
	  mfi_which = 4
	  write(*,*) ' var get failed.'
	  call cdf_error(status,message)
	  write(*,*) message,' which = ',mfi_which
 	end if
        if(abs(bgsmc(1)).gt.500.0) goto 50		       	!flag bad values

	CALL CDF_lib (SELECT_, CDF_, mfi_id,
     .			    zVAR_, var_num_bgsma,
     .			    zVAR_RECNUMBER_, mfi_rec,
     .			    zVAR_DIMINDICES_, indices(1),
     .		      GET_, zVAR_DATA_, bgsma(1),
     .		      NULL_, status)
	if (status.ne.cdf_ok) then
	  mfi_which = 5
	  write(*,*) ' var get failed.'
	  call cdf_error(status,message)
	  write(*,*) message,' which = ',mfi_which
 	end if
        if(abs(bgsma(1)).gt.500.0) goto 50		       	!flag bad values

	CALL CDF_lib (SELECT_, CDF_, mfi_id,
     .			    zVAR_, var_num_bgsec,
     .			    zVAR_RECNUMBER_, mfi_rec,
     .			    zVAR_DIMINDICES_, indices(1),
     .		      GET_, zVAR_DATA_, bgsec(1),
     .		      NULL_, status)
	if (status.ne.cdf_ok) then
	  mfi_which = 6
	  write(*,*) ' var get failed.'
	  call cdf_error(status,message)
	  write(*,*) message,' which = ',mfi_which
 	end if
        if(abs(bgsec(1)).gt.500.0) goto 50		       	!flag bad values

	CALL CDF_lib (SELECT_, CDF_, mfi_id,
     .			    zVAR_, var_num_bgsea,
     .			    zVAR_RECNUMBER_, mfi_rec,
     .			    zVAR_DIMINDICES_, indices(1),
     .		      GET_, zVAR_DATA_, bgsea(1),
     .		      NULL_, status)

	if (status.ne.cdf_ok) then
	  mfi_which = 7
	  write(*,*) ' var get failed.'
	  call cdf_error(status,message)
	  write(*,*) message,' which = ',mfi_which
	end if
        if(abs(bgsea(1)).gt.500.0) goto 50			!flag bad values
C
C  get second set of vectors
C
	indices(1) = 2
	CALL CDF_lib (SELECT_, CDF_, mfi_id,
     .			    zVAR_, var_num_bgsmc,
     .			    zVAR_RECNUMBER_, mfi_rec,
     .			    zVAR_DIMINDICES_, indices(1),
     .		      GET_, zVAR_DATA_, bgsmc(2),
     .		      NULL_, status)
	if (status.ne.cdf_ok) then
	  mfi_which = 8
	  write(*,*) ' var get failed.'
	  call cdf_error(status,message)
	  write(*,*) message,' which = ',mfi_which
 	end if
        if(abs(bgsmc(2)).gt.500.0) goto 50		       	!flag bad values

	CALL CDF_lib (SELECT_, CDF_, mfi_id,
     .			    zVAR_, var_num_bgsma,
     .			    zVAR_RECNUMBER_, mfi_rec,
     .			    zVAR_DIMINDICES_, indices(1),
     .		      GET_, zVAR_DATA_, bgsma(2),
     .		      NULL_, status)
	if (status.ne.cdf_ok) then
	  mfi_which = 9
	  write(*,*) ' var get failed.'
	  call cdf_error(status,message)
	  write(*,*) message,' which = ',mfi_which
 	end if
        if(abs(bgsma(2)).gt.500.0) goto 50		       	!flag bad values

	CALL CDF_lib (SELECT_, CDF_, mfi_id,
     .			    zVAR_, var_num_bgsec,
     .			    zVAR_RECNUMBER_, mfi_rec,
     .			    zVAR_DIMINDICES_, indices(1),
     .		      GET_, zVAR_DATA_, bgsec(2),
     .		      NULL_, status)
	if (status.ne.cdf_ok) then
	  mfi_which = 10
	  write(*,*) ' var get failed.'
	  call cdf_error(status,message)
	  write(*,*) message,' which = ',mfi_which
	end if
        if(abs(bgsec(2)).gt.500.0) goto 50                    !flag bad values

	CALL CDF_lib (SELECT_, CDF_, mfi_id,
     .			    zVAR_, var_num_bgsea,
     .			    zVAR_RECNUMBER_, mfi_rec,
     .			    zVAR_DIMINDICES_, indices(1),
     .		      GET_, zVAR_DATA_, bgsea(2),
     .		      NULL_, status)
	if (status.ne.cdf_ok) then
	  mfi_which = 11
	  write(*,*) ' var get failed.'
	  call cdf_error(status,message)
	  write(*,*) message,' which = ',mfi_which
	end if
        if(abs(bgsea(2)).gt.370.0) goto 50			!flag bad values
C
C get third set
C
	indices(1) = 3
	CALL CDF_lib (SELECT_, CDF_, mfi_id,
     .			    zVAR_, var_num_bgsmc,
     .			    zVAR_RECNUMBER_, mfi_rec,
     .			    zVAR_DIMINDICES_, indices(1),
     .		      GET_, zVAR_DATA_, bgsmc(3),
     .		      NULL_, status)
	if (status.ne.cdf_ok) then
	  mfi_which = 12
	  write(*,*) ' var get failed.'
	  call cdf_error(status,message)
	  write(*,*) message,' which = ',mfi_which
 	end if
        if(abs(bgsmc(3)).gt.500.0) goto 50		       	!flag bad values

	CALL CDF_lib (SELECT_, CDF_, mfi_id,
     .			    zVAR_, var_num_bgsma,
     .			    zVAR_RECNUMBER_, mfi_rec,
     .			    zVAR_DIMINDICES_, indices(1),
     .		      GET_, zVAR_DATA_, bgsma(3),
     .		      NULL_, status)
	if (status.ne.cdf_ok) then
	  mfi_which = 13
	  write(*,*) ' var get failed.'
	  call cdf_error(status,message)
	  write(*,*) message,' which = ',mfi_which
 	end if
        if(abs(bgsma(3)).gt.500.0) goto 50		       	!flag bad values

	CALL CDF_lib (SELECT_, CDF_, mfi_id,
     .			    zVAR_, var_num_bgsec,
     .			    zVAR_RECNUMBER_, mfi_rec,
     .			    zVAR_DIMINDICES_, indices(1),
     .		      GET_, zVAR_DATA_, bgsec(3),
     .		      NULL_, status)
	if (status.ne.cdf_ok) then
	  mfi_which = 14
	  write(*,*) ' var get failed.'
	  call cdf_error(status,message)
	  write(*,*) message,' which = ',mfi_which
	end if
        if(abs(bgsec(3)).gt.500.0) goto 50			!flag bad values

	CALL CDF_lib (SELECT_, CDF_, mfi_id,
     .			    zVAR_, var_num_bgsea,
     .			    zVAR_RECNUMBER_, mfi_rec,
     .			    zVAR_DIMINDICES_, indices(1),
     .		      GET_, zVAR_DATA_, bgsea(3),
     .		      NULL_, status)
	if (status.ne.cdf_ok) then
	  mfi_which = 15
	  write(*,*) ' var get failed.'
	  call cdf_error(status,message)
	  write(*,*) message,' which = ',mfi_which
	end if
        if(abs(bgsea(3)).gt.370.0) goto 50                   	!flag bad values

       	goto 95			!If data points are good
C
C If one MFI value is bad, Flag all MFI values with -2000.0
C

  50	do i =1,3
	  bgsea(i)	  = -2000.0
	  bgsec(i)	  = -2000.0
	  bgsma(i)	  = -2000.0
	  bgsmc(i)	  = -2000.0
	end do
C
C write them all out to screen
C
C  55	call epoch_breakdown(mfi_time,year,mon,day,hr,min,sec,msec)			
C	call compute_epoch(year,0,1,0,0,0,0,epoch_temp)
C	mfi_doy = int((mfi_epoch - epoch_temp)/(24*60*60*1000)) + 1
C	write(6,57) 
C  57	format(/,' Year DOY:HR:MN:SC       BGSEr    BGSEt    BGSEp    BGSEx    BGSEy    BGSEz')
C   
C  60	format(1X,I4,1X,I3,3(':',I2.2),3X,6(1X,F8.2))
C  70	write(6,60) year,mfi_doy,hr,min,sec,(bgsea(i),i=1,3),(bgsec(i),i=1,3)
C        write(6,72) in_hr,in_min,in_sec,deltaT, position_gse
C  72	format(1x,8x,3(':',i2.2),4x,F8.2,2x,3(1x,f8.3)//)
C
  95    CALL CDF_lib (CLOSE_, CDF_, mfi_id)	  
      	return
      	end
