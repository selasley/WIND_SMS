	SUBROUTINE get_swe_kp (in_yr,in_mon,in_day,in_hr,in_min,in_sec,           !input
     .			    position_gse,v_gse,v_gse_p,vtherm,prot_dens,deltaT)	  !output
C
C	The minimum time between SWE key parameter data points is ~90 secs
C	irregardless of High or Low Bit Rate.
C	This routine will open up the cdf file specified by the input date
C	and return the SWE key parameter data for the given time +/- 45 secs.
C
C	This is basically the same as GET_SWE but the search for the closest time is done 
C	using the Binary Search method instead.
C
C	Routine must be linked to the following libraries:
C                  CDF$LIB:LIBCDF/LIBRARY, SYS$LIBRARY:VAXCRTL/LIB
C
C	Created 	23-JUN-1996	K.CHOTOO
C
C	Revision History:
C	  25-SEP-1996 		K.CHOTOO	If any SWE value is bad, flag all SWE values
C						with -2000.0 for that time.
C	  26-SEP-1996		K.CHOTOO	If one position value is bad, flag all 3 with
C						-2000.0
C	  17-OCT-1997		K.CHOTOO	Adjust time if in_hr = 24.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	IMPLICIT NONE
	INCLUDE 'CDF$INC:CDF.INC'
	REAL*8 epoch_temp,		!Temporary Epoch time
     .         in_epoch,       		!Input Epoch Time
     .         swe_epoch,swe_epoch0,	!SWE current Epoch time
     .         swe_epoch1,
     .	       swe_time,		!SWE Epoch time
     .	       temp			!Temporary variable
c
	REAL*4 deltaT,deltaT0,deltaT1,  !Time difference
     .	       pi/3.141593/,
     .         position_gse(3),		!Cartesian position in GSE coordinates, km
     .         prot_dens,		!Proton density in #/cm^3
     .	       Re/6380.0/,		!Earth Radius in km
     .         v_gse(3),		!Velocity in GSE cartesian coordinates, km/s
     .	       v_gse_p(3),		!Bulk flow in polar coordinates
     .         v_gsm(3),		!Velocity in GSM cartesian coordinates, km/s
     .         vtherm  			!Thermal velocity, km/s = sqrt(2kT/M)
c
	INTEGER*4 cdf_id,
     .            dim_sizes,
     .            encoding,		!Encoding of CDF file
     .            i,j,			!Counters
     .            indices(1),
     .            majority,
     .            num_attrs, 		!Number of attributes in CDF file
     .            num_dims,		
     .            num_vars,		!Number of variables in CDF
     .            rec_num,
     .            status, 
     . 		  swe_id,		!Unit number of CDF opened by CDF_OPEN
     .            swe_max,              !Maximum number of records in SWE CDF
     .            swe_rec /1/,		!Current record number in CDF file 
     .		  swe_which /0/,
     .    	  vn_position,
     .		  vn_protdens,
     .            vn_swe_epoch,
     .            vn_tdp_epoch, 
     .            vn_vgse,
     .		  vn_vgse_p,
     .            vn_vgsm,
     .            vn_vth


	INTEGER*4 in_yr,in_mon,in_day,in_hr,in_min,in_sec,
     .            year,mon,day,hr,min,sec,msec,swe_doy,low,high

        CHARACTER*36 cdf_name                        	!Name of CDF files
	CHARACTER*(EPOCH_STRING_LEN) date1, date2       !Character string for output of Epoch time
	CHARACTER*(cdf_statustext_len) message		!Error message description


C 
C Initialize output variables
C
	do i =1,3
	  position_gse(i) = -3000.0
	  v_gse(i)	  = -3000.0
	  v_gse_p(i)	  = -3000.0
	end do
	vtherm = -3000.0
	prot_dens = -3000.0
C
C Make sure that input hour is less than 24
C
 	if(in_hr .ge. 24) then
	  in_hr = 23
	  in_min= 59
	  in_sec= 59
	endif
C
C Convert input time to Epoch time 
C
	call compute_epoch (in_yr,in_mon,in_day,in_hr,in_min,in_sec,0,in_epoch)

C
C  Open CDF file.
C
 220	write(cdf_name,230) in_yr,in_mon,in_day
 230	format('wind_kp_swe:wi_k0_swe_',I4.4,I2.2,I2.2,'_v01')
!	write(*,*) cdf_name
	call cdf_open(cdf_name,swe_id,status)
C
C  If version 01 does NOT exist, increment version number up to 09.
C
	if (status.le.cdf_warn) then
!	    write(*,*) ' V01 failed.  Trying other versions '
	    do 235 i = 2,9
	      write (cdf_name(34:34), '(I1)') i
!	      write(*,*) cdf_name
	      call cdf_open(cdf_name,swe_id,status)
	      if (status.eq.cdf_ok) i=235
 235	    continue
	end if
C
C  Get CDF file ID, maximum number of records, etc., for current CDF file
C
	call cdf_inquire(swe_id,num_dims,dim_sizes,encoding,majority,
     .      swe_max,num_vars,num_attrs,status)
C
C  If there is a problem with the CDF file or it does not exist then 
C  exit.
C
	if (status.ne.cdf_ok) then
	    write(*,*) ' problem with cdf_inquire... '
	    call cdf_error(status,message)
	    write(*,*) message
	    call cdf_close(swe_id,status)
	    goto 300
	end if					!status bad
C
C  Get variable number for different data.
C
	vn_swe_epoch = cdf_var_num(swe_id,'Epoch')
	vn_vgse = cdf_var_num(swe_id,'V_GSE')
!	vn_vgsm = cdf_var_num(swe_id,'V_GSM')
	vn_vth = cdf_var_num(swe_id,'THERMAL_SPD')
	vn_protdens = cdf_var_num(swe_id,'Np')
        vn_position = cdf_var_num(swe_id,'SC_pos_gse')
    	vn_vgse_p = cdf_var_num(swe_id,'V_GSE_p')
C
C Get record number for data closest to input time using binary search
C
	 low = 0
	 high = swe_max
	 indices(1) = 1
	 do while (low .le. high)
             rec_num = (low + high)/2
	     call cdf_var_get(swe_id,vn_swe_epoch,rec_num,indices(1),swe_epoch,
     .          status)

	     if(in_epoch .eq. swe_epoch) then
	       swe_time = swe_epoch
               swe_rec = rec_num
	       deltaT = 0
	       goto 250
	     else if(in_epoch .lt. swe_epoch) then
               high = rec_num - 1
    	     else
   	       low = rec_num + 1
      	     endif
	 end do 			!low .le. high

	 deltaT = abs(in_epoch - swe_epoch) / 1000.0
         swe_time = swe_epoch
         swe_rec = rec_num
C
C Check first higher time to see if it is closer to requested time
C
	 if(rec_num .lt. swe_max) then
	   call cdf_var_get(swe_id,vn_swe_epoch,rec_num+1,indices(1),swe_epoch1,
     .          status)
           deltaT1 = abs(in_epoch - swe_epoch1) / 1000.0
	   if(deltaT1 .lt. deltaT) then
             swe_rec = rec_num + 1
             swe_time = swe_epoch1
	     deltaT = deltaT1
           endif
         endif
C
C Check first lower time to see if it is closer to requested time
C
	 if(rec_num .gt. 0) then
	   call cdf_var_get(swe_id,vn_swe_epoch,rec_num-1,indices(1),swe_epoch0,
     .          status)
           deltaT0 = abs(in_epoch - swe_epoch0) / 1000.0
	   if(deltaT0 .lt. deltaT) then
             swe_rec = rec_num - 1
             swe_time = swe_epoch0
	     deltaT = deltaT0
           endif
         endif
C
C ************** Get Position information first *****************
C
	    indices(1) = 1
	    call cdf_var_get(swe_id,vn_position,swe_rec,indices(1),position_gse(1),
     .         status)
	    if (status.ne.cdf_ok) then
	      swe_which = 0
	      write(*,*) ' var get failed.'
	      call cdf_error(status,message)
	      write(*,*) message,' which = ',swe_which
	    end if
	    if(abs(position_gse(1)).gt. 2.0e+06) then 
	      goto 240						!flag bad values
 	    else
 	      position_gse(1) = position_gse(1)/Re			!Convert to Earth Radii
	    endif

	    indices(1) = 2
	    call cdf_var_get(swe_id,vn_position,swe_rec,indices(1),position_gse(2),
     .         status)
	    if (status.ne.cdf_ok) then
	      swe_which = 1
	      write(*,*) ' var get failed.'
	      call cdf_error(status,message)
	      write(*,*) message,' which = ',swe_which
	    end if
	    if(abs(position_gse(2)).gt. 2.0e+06) then
	      goto 240						!flag bad values
            else
              position_gse(2) = position_gse(2)/Re                   	!Convert to Earth Radii
	    endif
	    
	    indices(1) = 3
	    call cdf_var_get(swe_id,vn_position,swe_rec,indices(1),position_gse(3),
     .         status)
	    if (status.ne.cdf_ok) then
	      swe_which = 2
	      write(*,*) ' var get failed.'
	      call cdf_error(status,message)
	      write(*,*) message,' which = ',swe_which
	    end if
	    if(abs(position_gse(3)).gt. 2.0e+06) then
 	      goto 240							!flag bad values
            else
	      position_gse(3) = position_gse(3)/Re			!Convert to Earth Radii
	    endif

            goto 250		!if position values are good
C
C If one position component is bad, flag all three components
C
 240 	    do i =1,3
	      position_gse(i) = -2000.0
	    end do
	   
C
C *************** Now get SWE data at specified record number ***********
C
C
C  get first vector components and scalar values
C
 250	    indices(1) = 1
	    call cdf_var_get(swe_id,vn_vgse,swe_rec,indices(1),v_gse(1),
     .         status)
	    if (status.ne.cdf_ok) then
	      swe_which = 3
	      write(*,*) ' var get failed.'
	      call cdf_error(status,message)
	      write(*,*) message,' which = ',swe_which
	    end if
	    if(abs(v_gse(1)).gt. 1100.0) goto 260				!flag bad values

!	    call cdf_var_get(swe_id,vn_vgsm,swe_rec,indices(1),v_gsm(1),
!     .         status)
!	    if (status.ne.cdf_ok) then
!	      swe_which = 4
!	      write(*,*) ' var get failed.'
!	      call cdf_error(status,message)
!	      write(*,*) message,' which = ',swe_which
!	    end if
!	    if(abs(v_gsm(1)).gt. 1100.0) goto 260				!flag bad values

	    call cdf_var_get(swe_id,vn_vth,swe_rec,indices(1),vtherm,
     .         status)
	    if (status.ne.cdf_ok) then
	      swe_which = 5
	      write(*,*) ' var get failed.'
	      call cdf_error(status,message)
	      write(*,*) message,' which = ',swe_which
	    end if
	    if((vtherm .lt. 0).or.(vtherm.gt. 200.0)) goto 260			!flag bad values

	    call cdf_var_get(swe_id,vn_protdens,swe_rec,indices(1),prot_dens,
     .         status)
	    if (status.ne.cdf_ok) then
	      swe_which = 6
	      write(*,*) ' var get failed.'
	      call cdf_error(status,message)
	      write(*,*) message,' which = ',swe_which
	    end if
	    if((prot_dens .lt. 0).or.(prot_dens .gt. 200.0)) goto 260		!flag bad values


	    call cdf_var_get(swe_id,vn_vgse_p,swe_rec,indices(1),v_gse_p(1),
     .         status)
	    if (status.ne.cdf_ok) then
	      swe_which = 7
	      write(*,*) ' var get failed.'
	      call cdf_error(status,message)
	      write(*,*) message,' which = ',swe_which
	    end if
	    if(abs(v_gse_p(1)).gt. 1100.0) goto 260			!flag bad values

C
C  get second vector components
C
            indices(1) = 2
	    call cdf_var_get(swe_id,vn_vgse,swe_rec,indices(1),v_gse(2),
     .         status)
	    if (status.ne.cdf_ok) then
	      swe_which = 8
	      write(*,*) ' var get failed.'
	      call cdf_error(status,message)
	      write(*,*) message,' which = ',swe_which
	    end if
	    if(abs(v_gse(2)).gt. 1100.0) goto 260				!flag bad values

!	    call cdf_var_get(swe_id,vn_vgsm,swe_rec,indices(1),v_gsm(2),
!     .         status)
!	    if (status.ne.cdf_ok) then
!	      swe_which = 9
!	      write(*,*) ' var get failed.'
!	      call cdf_error(status,message)
!	      write(*,*) message,' which = ',swe_which
!	    end if
!	    if(abs(v_gsm(2)).gt. 1100.0) goto 260				!flag bad values


	    call cdf_var_get(swe_id,vn_vgse_p,swe_rec,indices(1),v_gse_p(2),
     .         status)
	    if (status.ne.cdf_ok) then
	      swe_which = 10
	      write(*,*) ' var get failed.'
	      call cdf_error(status,message)
	      write(*,*) message,' which = ',swe_which
	    end if
	    if(abs(v_gse_p(2)).gt. 50.0) goto 260 				!flag bad values


C
C  get third vector components
C
	    indices(1) = 3
	    call cdf_var_get(swe_id,vn_vgse,swe_rec,indices(1),v_gse(3),
     .         status)
	    if (status.ne.cdf_ok) then
	      swe_which = 11
	      write(*,*) ' var get failed.'
	      call cdf_error(status,message)
	      write(*,*) message,' which = ',swe_which
	    end if
	    if(abs(v_gse(3)).gt. 1100.0) goto 260				!flag bad values

!	    call cdf_var_get(swe_id,vn_vgsm,swe_rec,indices(1),v_gsm(3),
!     .         status)
!	    if (status.ne.cdf_ok) then
!	      swe_which = 12
!	      write(*,*) ' var get failed.'
!	      call cdf_error(status,message)
!	      write(*,*) message,' which = ',swe_which
!	    end if
!	    if(abs(v_gsm(3)).gt. 1100.0) goto 260				!flag bad values


	    call cdf_var_get(swe_id,vn_vgse_p,swe_rec,indices(1),v_gse_p(3),
     .         status)
	    if (status.ne.cdf_ok) then
	      swe_which = 13
	      write(*,*) ' var get failed.'
	      call cdf_error(status,message)
	      write(*,*) message,' which = ',swe_which
	    end if
	    if(abs(v_gse_p(3)).gt. 50.0) goto 260				!flag bad values

	    goto 300 		!if data points are good 

C
C If one SWE value is bad, flag all SWE values with -2000.0
C
 260	    do i = 1,3
	      v_gse(i)   = -2000.0							
!              v_gsm(i)   = -2000.0
	      v_gse_p(i) = -2000.0
	    end do
	    vtherm = -2000.0
	    prot_dens = -2000.0


! 270	    call epoch_breakdown(swe_time,year,mon,day,hr,min,sec,msec)			
!	    call compute_epoch(year,0,1,0,0,0,0,epoch_temp)
!	    swe_doy = int((swe_epoch - epoch_temp)/(24*60*60*1000)) + 1
!
!	    write(6,215) 
! 215        format(/,' Year DOY:HR:MN:SC      X_GSE    Y_GSE    Z_GSE    VGSEx     VGSEy    VGSEz   ',
!     .                                '  |v|  S+N-(deg) W+E-(deg)   Vth      Np   ')
!
! 271        format(1X,I4,1X,I3,3(':',I2.2),3X,3(1X,F8.3),8(1X,F8.2))
!            write(6,271) year,swe_doy,hr,min,sec,(position_gse(i),i=1,3),
!     .			(v_gse(j),j=1,3),v_gse_p(1),v_gse_p(3),v_gse_p(2),vtherm,prot_dens
! 
!            write(6,272) in_hr,in_min,in_sec,deltaT
! 272	    format(//,1x,8x,3(':',i2.2),4x,F8.2,//)
!
!
 300  call cdf_close(swe_id,status)
      return
      end
