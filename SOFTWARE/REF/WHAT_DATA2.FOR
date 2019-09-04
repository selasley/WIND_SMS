	SUBROUTINE WHAT_DATA2 (beg_date, end_date, what_info, outfilename, interval)
C
C       Created    	21-FEB-1996	by	J.MILLER and K.CHOTOO
C	Design:
C		find out what WIND KP data the user wants
C		find out beginning and end dates for data
C		creates output filenames using start and stop dates
C		pass back information to main program
C
C	Changes:
C
C
	IMPLICIT NONE
	REAL*8	beg_date, end_date		!Start and stop times converted to Epoch time
        REAL*4  interval			!Time interval in minutes between data points
	LOGICAL*4 what_info(4)			!Array used for instrument selection:
     						!    1.MFI  2.3DP  3.SWE  4.STEP  5.SMS
	INTEGER choice,				!User's instrument selection
     .          date_difference,		!Check if more than one day
     .          end_year,end_month,end_day,	!User's end time
     .          end_hr,end_min,end_sec,
     .          i,                           	!Counter
     .          start_year,start_month,start_day,	!User's start time     	
     .          start_hr,start_min,start_sec		

	CHARACTER*40 outfilename(4)		! 1.MFI 2.3DP 3.SWE 4.EPA output file name
        CHARACTER*1 response
C
C  get start and end dates and times?
C  convert to EPOCH format
C
        write(*,*)' '
        write(*,*)' '
	write(*,*)'Enter start time (yyyy,mm,dd,hr,mn,sc):'
	read(*,*) start_year,start_month,start_day,start_hr,start_min,start_sec
	if(start_hr .eq. 24) then		! 24:00:00 is invalid for compute_epoch
           start_hr = 23			! so change to 23:59:59
           start_min= 59
           start_sec= 59
        endif
	call compute_epoch (start_year,start_month,start_day,start_hr,start_min,
     .                      start_sec,0,beg_date)

        write(*,*)'Enter end time (yyyy,mm,dd,hr,mn,sc):'
	read(*,*) end_year,end_month,end_day,end_hr,end_min,end_sec
	if(end_hr .eq. 24) then		! 24:00:00 is invalid for compute_epoch
           end_hr = 23			! so change to 23:59:59
           end_min= 59
           end_sec= 59
        endif
	call compute_epoch (end_year,end_month,end_day,end_hr,end_min,end_sec,0,
     .                      end_date)

C
C  Create output filenames using start and stop times. If one day or less output filename
C  would have the form  "WI_KP_XXX_YYYYMMDD_HHMM_HHMM.DAT" where XXX=MFI,3DP, or SWE and 
C  HHMM represents start and stop hour and min.  For files longer than one day, the format
C  is "WI_KP_XXX_YYYYMMDD_YYYYMMDD.DAT" where the start and stop DATES are used.
C
	date_difference = (end_year - start_year) + (end_month - start_month)	!Check if more than one day
     .                   +(end_day - start_day)  

	if (date_difference .eq. 0) then
           write(outfilename(1),50)start_year,start_month,start_day,
     .                             start_hr,start_min,end_hr,end_min
  50	   format('wi_kp_mfi_',i4.4,i2.2,i2.2,'_',2(i2.2),'_',2(i2.2),'.dat')
           write(outfilename(2),60)start_year,start_month,start_day,
     .                             start_hr,start_min,end_hr,end_min
  60	   format('wi_kp_3dp_',i4.4,i2.2,i2.2,'_',2(i2.2),'_',2(i2.2),'.dat')
           write(outfilename(3),70)start_year,start_month,start_day,
     .                             start_hr,start_min,end_hr,end_min
  70	   format('wi_kp_swe_',i4.4,i2.2,i2.2,'_',2(i2.2),'_',2(i2.2),'.dat')
           write(outfilename(4),80)start_year,start_month,start_day,
     .                             start_hr,start_min,end_hr,end_min
  80	   format('wi_kp_epa_',i4.4,i2.2,i2.2,'_',2(i2.2),'_',2(i2.2),'.dat')

	else
	   write(outfilename(1),100)start_year,start_month,start_day,
     .                           end_year,end_month,end_day
 100       format('wi_kp_mfi_',i4.4,i2.2,i2.2,'_',i4.4,i2.2,i2.2,'.dat')
           write(outfilename(2),200)start_year,start_month,start_day,
     .                           end_year,end_month,end_day
 200       format('wi_kp_3dp_',i4.4,i2.2,i2.2,'_',i4.4,i2.2,i2.2,'.dat')
	   write(outfilename(3),300)start_year,start_month,start_day,
     .                           end_year,end_month,end_day
 300       format('wi_kp_swe_',i4.4,i2.2,i2.2,'_',i4.4,i2.2,i2.2,'.dat')

	   write(outfilename(4),400)start_year,start_month,start_day,
     .                           end_year,end_month,end_day
 400       format('wi_kp_epa_',i4.4,i2.2,i2.2,'_',i4.4,i2.2,i2.2,'.dat')
	endif
C
C  ask user what time increment they want between printouts of data
C
        write(*,*)' '
	write(*,'(A,$)') ' Enter time in minutes required between data points (0 for all data):  '
	read(*,*) interval
C
C  ask user which instrument they want data from
C
        response = 'y'
	do while ((response .eq. 'y').or.(response .eq. 'Y'))
       	  write(*,600)
 600	  format(///,10x, ' Please select an instrument to get data from: ',
     .	           /,10x, '		(1)	MFI (Mag. Field) '
     .	           /,10x, '		(2)	3DP (3D Plasma) '
     .             /,10x, '		(3)	SWE (Solar Wind) '
     .             /,10x, '		(4)	EPACT (Energetic Particles) '
     .            //,10x, ' Enter number of choice: ',$)
	  read(*,*) choice
	  if (choice .eq. 1) what_info(1) = 1
	  if (choice .eq. 2) what_info(2) = 1
	  if (choice .eq. 3) what_info(3) = 1   
	  if (choice .eq. 4) what_info(4) = 1   
          write(*,*)' '
          write(*,'(A,$)') ' Do you want to get data from another instrument (y/n)?   '
          read(*,'(a)')response
	end do					
C
C  return to main program
C
	return
	end
