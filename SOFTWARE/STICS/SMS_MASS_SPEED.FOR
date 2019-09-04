        SUBROUTINE SMS_MASS_SPEED(HDB,MASS_RATE,	!input
     .            BULK_SPEED,BULK_UNCER,SPD_FLAG,ERR_MESSAGE) 	!output

c	Program to collect WIND/MASS composition data

c	For each science record (3.2 min) --

c		1. Solar wind speed is determined (alpha speed, if possible)
c		2. Every DPPS step is assigned an m/q value
c		3. PHA events are collected in a 2D array: 2048 TOF bins (TOF/2)
c		   by 200 logarithmic m/q bins (0.80 - 32.0 amu/e, 1.862% bins)

c	author: D.C. Hamilton (5 January 1996)
c	version: 1/5/96

c	based on the program PULSE (2/24/95 version) by Michael R. Collier    
c	and on program SLOW_PH (7/4/95) by D. Hamilton
c       and on program MASS_ABUNDANCE (12/10/95) by D. Hamilton


c       compile for/check=(nounderflow,bounds)/warn=decl/extend comp
c
c       link comp,lib_dir:wind_lib/lib,lib_dir:l1lib/lib,
c            [wind.software.l1]compare_time,mrqmin_sw,mrqcof_sw,covsrt_sw,
c            gaussj_sw


c       notes: 1. the E/Q value for the analyzer assumes E/Q=0.5156*
c                 (1.05135)^dpps step as per an e-mail message from Dr.
c                 Hamilton dated 29 November 1994
c              2. mrqmin_sw and gaussj_sw are modified mrqmin and gaussj to
c                 transfer the flag sm_flag which indicates a singular matrix
c                 if set.
c              3. mass_rate.edb(j).rate(i) holds rates numbering 0-26 whereas
c                 rates(j,i) holds rates numbering 1-27
c              4. For the efficiency calculations for the fluxes, the program
c                 assumes the limits on the adps voltage are -3.1 and 1.7. If
c                 these limits are changed, the program will have to modified
c                 and re-compiled.



c       *** required include files ***

        include 'inc_dir:edbdef.inc/nolist'   ! required include files for data
        include 'inc_dir:l1fmt.inc/nolist'    ! structures req. by call prog.
        include '($rmsdef)'

        record /core/ sci   ! receives basic core data assoc w/current sr
        record /hdbstr/ hdb
        record /ma_rate/ mass_rate   ! receives data on rates from MASS sensor
        record /ma_pha/ mass_pha  ! receives data on tof's from the mass sensor
        record /t/ start, end, time(0:59)   ! allow sixty steps in time variable
c
c       notes: 1. time is structure type t. data from the call wind_l1_get_basic
c                 routine is deposited into this structure.
c
c
c
c       *** initialize variables ***

        character err_message*80,    ! error message
     2		  comp_mode*1,       ! compression mode a or c
     3            buffer*256,        ! buffer array for internal read/write
     4            ans*8,             ! eight character answer string for queries
     5		  fmultday*8	     ! eight character answer string for query

        integer*4 status,            ! non-zero indicates an i/o error
     1		  spd_flag,	     ! error flag 
     2            ios,               ! non-zero indicates i/o error on write
     3            sr_num,            ! number of the science record read
     4            status_sr,         ! science record status flag
     5            status_ma,         ! mass read routine status flag
     6            status_ba,         ! basic core data read status
     7            status_hk,         ! house keeping data read status
     8            val_spin,          ! number of valid spins in science record
     9            i,j,k,l,jj,        ! looping variables
     1            dpps_cyc(0:59),    ! dpps step corresponding to s/c spin num
     2            spin_num,          ! spacecraft spin number
     3            itemp,             ! temp I*4 for converting byte variables
     4            rates(0:59,27),    ! contains decompressed mass rates/cycle
     5            wind_decompress,   ! decompresses the compressed MASS rates
     6            nca,               ! variables that the fitting routine uses
     7            lista(6),          ! numbers the parameters for fitting
     8            iter,              ! number of iterations in curve fit routine
     9            fit_elements,      ! total number of data elements fit
     9		  startstep,	     ! starting voltage step number for peak search
     1            i_max_loc,         ! element location for maximum FSR2 counts
     1		  i_he_max_loc,	     ! spin of helium maximum
     2            he_pk_strt_ch,     ! starting helium peak channel
     3            he_fit_elements,   ! number of helium elements fit
     4            he_tot,            ! total number of helium elements in peak
     5            dpps_step,         ! DPPS step (0-59)
     8            sector,            ! sector location of pha (0=nsun, 1=sun)
     9            range,             ! range of pha (0 is protons, 1 is heavies)
     1            anode,             ! 0-inval., 1-start, 2-stop, 3-both
     2            time_of_flight,    ! channel number of tof signal
     3		  itof2,	     ! tof channel number / 2
     3            pha_sum,	     ! total Mass pha events in one science rec
     5            adps_step,         ! contains the adps step of current spin
     6            he_max_dpps,       ! dpps step corresponding to helium max
     7            he_max_adps,       ! adps step corresponding to helium max
     8            raw_he_cnts,       ! keeps running total of raw helium counts
     9            he_adps_step,      ! adps step num for the helium target dpps
     1            he_counts,         ! number of helium counts in this sci rec
     2		  hps_level,	     ! current commanded HPS level
     3		  fsr2(0:59),	     ! fsr2(i) =  counts for spin 59-i
     4		  ilo,		     ! low dpps step for prton peak
     5		  ihi,		     ! high dpps step for proton peak	
     6		  idelta,	     ! hwhm of proton peak in dpps steps
     7		  i_peak,	     ! guess for dpps step of helium peak
     1		  icountday,	     ! numebr of days that has been processing
     2		  iday,		     ! number of days for the data set
     3		  index		     ! just a running index for loops.

        byte      pha(2)             ! contains the two bytes of the pha word
        logical   code,              ! comp mode-set is mode c, clear is mode a
     2            start_flag,        ! set to true when the start time found
     3            end_flag,          ! true indicates terminate reading file
     4            compare_time_ge,   ! routine is true if first time .ge. second
     5            sm_flag,           ! returned true indicates sing matrix error
     6            first_elem_flag,   ! false indicates first he elem not found
     7            he_sp_flag,        ! true indicates an unreasonable he speed
     8            eight_flag,        ! false indicates no info written to unit 8
     8		  proshoulder,	     ! peak FSR is on proton shoulder
     9		  he_peak	     ! true if proton peak is below threshold

        real*4    x(60),y(60),sig(60),   ! fit variables-x,y, and uncertainty
     2            a(6),b(6),             ! the parameters to be fit
     3            covar(10,10),      	 ! working space for fitting routine
     4            alpha(10,10),      	 ! working space for fitting routine
     5            chisqr,                ! chi-squared for fit
     6            alamda,                ! <0 on intial call, final call =0
     7            max_cnt,               ! contains maximum counts in array
     8            max_loc,               ! contains E/Q location of maximum
     8		  bulk_speed,		 ! bulk speed = H+ speed for 315 < speed < 1275 km/s
C					              = He++ "    "  240 < speed < 315 km/s
     8  	  bulk_uncer,	 	 ! bulk speed uncertainty
     9            prot_speed,            ! proton speed from E/Q mean
     1            prot_uncer,            ! uncert. on proton speed from width
     2            alph_speed,            ! alpha speed from E/Q mean
     3            alph_uncer,            ! uncert. on alpha speed from width
     4            fwhm,                  ! FWHM of proton distribution in keV/e
     +		  hwhm,			 ! HWHM of proton distribution in keV/e
     5            he_max_cnt,            ! contains maximum helium counts
     6            he_max_loc,            ! contains E/Q of helium maximum
     7            he_pk_strt_eq,         ! starting helium pk E/Q location
     8            factor,                ! frac of He fwhm searched on left
     9            he_x(60),he_y(60),
     1            he_sig(60),            ! the helium E/Q, FSR2 rates, and sigma
     2            epm,		         ! keV/amu assoc with fitted alpha speed
     3		  mpq,			 ! amu/e assoc. with a given dpps step
     5            nano,                  ! time of flight in nanoseconds
     6            mass,                  ! mass in amu assuming Q=+1
     7            sw_speed,              ! speed used for targeting species
     1            he_pseudo_cnts,        ! efficiency modified number of cnts
     2            step_above,            ! num stps above pk prog looks for data
     3            he_v_sub_f,            ! adps voltage during target he step
     4            he_e_total,            ! total energy in the target he step
     5            he_defect,             ! factor to account for v_f dep. eff.
     6            he_eta_st,
     7            he_eta_sp,             ! start/stop eff in he target he step
     8            norm,                  ! 2.9/360. * 3.2 sec * 0.35 cm^2
     9            he_peak_tt,            ! helium peak to total ratio
     1            tot_he_e_over_q,       ! for averaging E/Q over whole period
     2		  epq(0:59),		 ! E/Q values for 60 values of dpps step
     3		  lowbound, 		 ! lower solar wind speed for the data
     4		  highbound		 ! higher solar wind speed for the data

        external  funcs,             ! subroutine to evaluate fitting function
     2            mrqmin             ! first subroutine of the fitting program
c
c       notes: 1. rates(xx,2) contains FSR2
c

c
c   initialize variables
c	
	spd_flag = 0
	bulk_speed = 0
	bulk_uncer = 0
	err_message = ' '
c
c   fill epq array with the 60 E/Q values in keV/e
c
	do i=0,59
	  epq(i) = 0.5156*1.05135**i
	enddo


	val_spin = l1_sr_header.nvalid   ! # of valid spins per Science Record
	if(val_spin .ne. 60) then
	  write(6,45) val_spin
  45	  format(1x,'Incomplete Science Record: ',i3,' spins')
	  write (err_message,45) val_spin
	  spd_flag = 1
	  goto 500
        endif

c   check for hps level = 176.  If not type a message and skip to next science 
c   record

	hps_level = hdb.bdh.dat(110)
	if (hps_level .lt. 0) hps_level=hps_level+256  !take care of sign bit
	if (hps_level .ne. 176) then
	   write (6,50) hps_level
  50	   format(1x,'Invalid MASS HPS level =>',i5)
	   write (err_message,50) hps_level
	   spd_flag = 1
	   go to 500
	endif

c
c
c       *** get the mass data ***
c
c
      	do spin_num=0,59   ! determine DPPS steps for each s/c spin number
           itemp=mass_rate.edb(spin_num).rate(25)
           dpps_cyc(spin_num)=ibits(itemp,0,6)
c          --- bit 7 is calibration mode status, bits 0-5 are DPPS step ---
c          ---ibits expects an I*2 or I*4 and ...rate(25) is a byte     ---
c          ---setting to itemp before ibits converts it to corr form    ---
       	enddo
c
     	itemp = hdb.bdh.dat(189)   ! compression codes for all MASS rates
        code = btest(itemp,4)      ! are in the hdb byte 189 bit 4
        if (code .eq. .true.)  comp_mode='c'
        if (code .eq. .false.) comp_mode='a'
c
      	do spin_num=0,59
	  fsr2(59-spin_num) = wind_decompress(mass_rate.edb(spin_num).rate(1),code)
	  if (fsr2(59-spin_num) .le. 0) then
	    type *, 'MASS fsr2=0, no speed'
	    write(err_message,'(a)') ' MASS fsr2=0, no speed'
	    spd_flag = 1
	    go to 500
	  endif 

          rates(spin_num,26) = mass_rate.edb(spin_num).rate(25)   ! dpps step
          rates(spin_num,27) = mass_rate.edb(spin_num).rate(26)   ! adps step
        enddo
c
c
c       ***find the distribution maximum and location ***
c
        max_cnt = -1   ! contains maximum FSR2 counts
        do i=0,59
           if (fsr2(i) .gt. max_cnt) then  ! if a loc w/counts gt max_cnt
              max_cnt = fsr2(i)		   ! then set max_cnt to the new value
              i_max_loc = i    ! save vstep of max_cnt
           endif
        enddo
        max_loc = epq(i_max_loc)	   ! and save E/Q of max

c**********************************************************************************************
c
c   See if we have slow speed where proton peak is below minimum MASS E/Q. There are two cases.
c
c   CASE 1: we must find the alpha peak at higher channels
c  
	he_peak = .false.

	if ((i_max_loc .ge. 0) .and. (i_max_loc .le. 2)) then	
 						
c   check to see if the proton peak shoulder still has more counts than the helium peak

	   startstep = 2
	   proshoulder = .true.
	   do while (proshoulder .eq. .true.)
	      max_cnt = -1
	      do i=startstep,59		
		if (fsr2(i).gt.max_cnt) then
		  max_cnt = fsr2(i)
		  i_max_loc = i
		endif
	      end do
	      if (i_max_loc .eq. startstep) then
		 proshoulder = .true.
		 startstep = startstep + 1
	      else
		 proshoulder = .false.
	      endif
	   enddo
	
	   max_loc = epq(i_max_loc)
	   he_peak = .true.	
	   go to 300
	endif

c						
c CASE 2: i_max_loc already holds the helium peak location
c
	if ((fsr2(0) .gt. 100) .and. (fsr2(0).gt.fsr2(1)) .and. (fsr2(1).gt.fsr2(2))) then  
	  he_peak = .true.			
	  write(6,*) '2. he_peak = true'
	endif

300	if (.not. he_peak) go to 450		!we must have a proton peak


c   *** begin section to fit alpha peak only when SW wind speed is ****
c   *** less than 315 km/s

c  load up array with three fsr2 values on each side of peak

        fit_elements = 7  ! counts the number of elements to be fit
	j = 0

	do i=i_max_loc-3,i_max_loc+3
	  if (i.lt.0 .or. i.gt.59) then
	    write(6,305) i
 305	    format(1x,'He++ Peak out of range: i =',i3)
	    write(err_message,305) i
	    spd_flag = 1
	    go to 500
	  endif

           j = j+1
	   x(j) = epq(i)			   ! E/Q value
           y(j) = fsr2(i)
           sig(j) = float(fsr2(i))**0.5
        enddo
c
c
c       *** estimate the fitting parameters ***
c
	fwhm = 0.162*max_loc	 	!guess fwhm = 3 steps
        a(3) = fwhm**2/22.2/max_loc  	!derived approx. width
        a(1) = max_cnt*a(3)**0.5*2.5066283/max_loc	!amplitude
        a(2) = max_loc					!mean
c
        a(6) = 1e6
        a(4) = 0
        a(5) = 1e6      ! fix amplitude of proton fit at zero to do alpha fit
c
c
c       *** fit the alpha peak ***
c
        nca = 7
        alamda = -1
        lista(1) = 1  ! adjust the first three of the parameters
        lista(2) = 2
        lista(3) = 3  
        lista(4) = 4 
        lista(5) = 5 
        lista(6) = 6
c
        iter = 0   ! reset the iteration counter
        sm_flag = .false.   ! true indicates a singular matrix error
        do while ((alamda .lt. 1e4) .and. (iter .lt. 1000) .and.
     +            (fit_elements .ge. 3))


           call mrqmin(x,y,sig,fit_elements,a,3,lista,3,covar,alpha,
     +                 nca,chisqr,funcs,alamda,sm_flag,max_cnt,max_loc,
     +                 fwhm,he_max_cnt)   
c                      ---fit_ele data pts, 3 fit para, 3 parameters adjusted---
c                      ---sm_flag returned set if singular matrix error  ---
           if (sm_flag .eq. .true.) then   ! indicates a singular matrix problem
              type *,'singular matrix 1'
              a(1)=0
              a(2)=5.2245e6
              a(3)=5.2245e6   ! these values will set speed/width to 1e6 to flag
              a(4)=0
              a(5)=1.0437e7
              a(6)=1.0437e7   ! these values will set speed/width to 1e6 to flag
              iter = 1000   ! exit loop
           endif
c
           iter = iter + 1   ! increment the iteration counter
        enddo


c
	alph_speed = 309.53*a(2)**0.5	!alpha speed in km/s
	alph_uncer = 309.53*a(3)**0.5	!uncertainty on alpha speed in km/s

	if ((alph_speed .gt. 260.0) .and. (alph_speed .lt. 400.0)) then
	  bulk_speed = alph_speed
	  bulk_uncer = alph_uncer
	else
	  spd_flag = 1
	  write(6,60) alph_speed, alph_uncer
	  write(err_message,60) alph_speed, alph_uncer
 60	  format(1x,' Alpha speed out of range => ', f10.1, ' +/- ', f10.1, ' km/s')
	endif


c   *** end section for alpha peak only with SW speed less than 315 km/s ***
c   *** jump to section where fitted speed results are written ***

	go to 500


c   **** begin section of original Collier PULSE.FOR code to fit H and He peaks

c       *** determine the half width for protons ***

450     i = i_max_loc
        do while ((fsr2(i) .gt. max_cnt/2.0) .and.
     +            (i .le. 59))   ! search for HWHM of dist
	   ihi = i
           i = i+1   ! search to the rt of the peak for step with half max cnts
        enddo
	idelta = ihi - i_max_loc
	ilo = max(0,i_max_loc-idelta)		!step at hwhm below max
	hwhm = (epq(ihi) - max_loc)   
	fwhm = 2. * hwhm			! fwhm holds the H FWHM in E/Q

c       *** load up array one fwhm around the proton peak ***

        fit_elements = 0  ! counts the number of elements to be fit

        do i=ilo,ihi
c       ---look within one fwhm of the proton peak for data to be fit---
              fit_elements = fit_elements + 1   ! increment no. fit elem
              x(fit_elements) = epq(i)		! E/Q value
              y(fit_elements) = fsr2(i)
              sig(fit_elements) = fsr2(i)**0.5
        enddo
c
c
c       *** estimate the fitting parameters ***
c
        a(3) = fwhm**2/22.2/max_loc  ! derived approximation
        a(1) = max_cnt*a(3)**0.5*2.5066283/max_loc
        a(2) = max_loc
c
        a(6) = 1e6
        a(4) = 0
        a(5) = 1e6      ! fix amplitude of helium fit at zero to do proton
c
c
c       *** fit the proton peak ***
c
        nca = 7
        alamda = -1
        lista(1) = 1  ! adjust the first three of the parameters
        lista(2) = 2
        lista(3) = 3  
        lista(4) = 4 
        lista(5) = 5 
        lista(6) = 6
c
        iter = 0   ! reset the iteration counter
        sm_flag = .false.   ! true indicates a singular matrix error
        do while ((alamda .lt. 1e4) .and. (iter .lt. 1000) .and.
     +            (fit_elements .ge. 3))
c
           call mrqmin(x,y,sig,fit_elements,a,6,lista,3,covar,alpha,
     +                 nca,chisqr,funcs,alamda,sm_flag,max_cnt,max_loc,
     +                 fwhm,he_max_cnt)   
c                      ---fit_ele data pts, 3 fit para, 3 parameters adjusted---
c                      ---sm_flag returned set if singular matrix error  ---
           if (sm_flag .eq. .true.) then   ! indicates a singular matrix problem
              type *,'singular matrix 1'
              a(1)=0
              a(2)=5.2245e6
              a(3)=5.2245e6   ! these values will set speed/width to 1e6 to flag
              a(4)=0
              a(5)=1.0437e7
              a(6)=1.0437e7   ! these values will set speed/width to 1e6 to flag
              iter = 1000   ! exit loop
           endif
c
           iter = iter + 1   ! increment the iteration counter
        enddo

c   7/4/95  Am always getting singular matrix with helium fit so skip it for now


490	prot_speed = 437.74*a(2)**0.5  ! proton speed in km/s
        prot_uncer = 437.74*a(3)**0.5  ! uncertainty on proton speed in km/s

	if ((prot_speed .gt. 300.0) .and. (prot_speed .lt. 1100.0)) then
	  bulk_speed = prot_speed
	  bulk_uncer = prot_uncer
	else
	  spd_flag = 1
	  write(6,65) prot_speed, prot_uncer
	  write(err_message,60) prot_speed, prot_uncer
 65	  format(1x,' Proton speed out of range => ', f10.1, ' +/- ', f10.1, ' km/s')
	endif

500	end

c
c       *** subroutine funcs for the fitting program ***
c
        subroutine funcs(x,a,y,dyda,na,max_cnt,max_loc,fwhm,he_max_cnt)
c
        real*4     x, y,           ! x and y values to be evaluated
     2             a(6), dyda(6),  ! fitted parameters and their derivatives
     3             max_cnt,        ! maximum counts (protons) in array
     4             max_loc,        ! location in E/Q of the maximum counts
     5             fwhm,           ! fwhm in E/Q of the proton peak
     6             he_max_cnt      ! maximum counts in the helium peak
        integer*4  na,             ! variable used in the fitting routine
     2             i,j,k,l         ! variables for use in looping
c
       do i = 1,6   ! assure that all fit parameters remain positive
          a(i) = abs(a(i))
       enddo
c
c      --- keep values within reasonable bounds ---
c
       if (a(2) .gt. 1e18) a(2)=1e18
       if (a(3) .gt. 1e18) a(3)=1e18   
       if (a(5) .gt. 1e18) a(5)=1e18
       if (a(6) .gt. 1e18) a(6)=1e18   ! to avoid overflow errors--day 26 fix
c
c      --- evaluate the function ---
c

       y =(a(1)*exp(-(x-2*x**0.5*a(2)**0.5+a(2))/2/a(3))/(a(3)**0.5*2.5066283)) 
     +     *x   ! delta(E/Q) prop to E/Q
c
c    
c      ---first Gaussian function derivatives---
c
       dyda(1) = x*exp(-(x-2*x**0.5*a(2)**0.5+a(2))/2/a(3))/
     2           (a(3)**0.5*2.5066283)
       dyda(2) = ( ((x/a(2))**0.5-1)/(2*a(3)) )*
     2           ( x*a(1)*exp(-(x-2*x**0.5*a(2)**0.5+a(2))/(2*a(3))) ) /
     2           (a(3)**0.5*2.5066283)
       dyda(3) = -( x*a(1)*exp(-(x-2*x**0.5*a(2)**0.5+a(2))/(2*a(3))) ) /
     2           (a(3)**1.5*2*2.5066283) + 
     3            ( x*a(1)*exp(-(x-2*x**0.5*a(2)**0.5+a(2))/(2*a(3))) ) /
     4           (a(3)**0.5*2.5066283) *
     5           ( (x-2*a(2)**0.5*x**0.5+a(2))/(2*a(3)**2) )
c
c    
c      ---second Gaussian function derivatives---
c
c       dyda(4) = x*exp(-(x-2*x**0.5*a(5)**0.5+a(5))/2/a(6))/
c     2           (a(6)**0.5*2.5066283)*x
c       dyda(5) = ( ((x/a(5))**0.5-1)/(2*a(6)) )*
c     2           ( x*a(4)*exp(-(x-2*x**0.5*a(5)**0.5+a(5))/(2*a(6))) ) /
c     2           (a(6)**0.5*2.5066283)*x
c       dyda(6) = -( x*a(4)*exp(-(x-2*x**0.5*a(5)**0.5+a(5))/(2*a(6))) ) /
c     2           (a(6)**1.5*2*2.5066283) + 
c     3            ( x*a(4)*exp(-(x-2*x**0.5*a(5)**0.5+a(5))/(2*a(6))) ) /
c     4           (a(6)**0.5*2.5066283) *
c     5           ( (x-2*a(5)**0.5*x**0.5+a(5))/(2*a(6)**2) )*x
c
        return
        end
