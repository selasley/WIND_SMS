        SUBROUTINE MASS_SPEED(HDB,MASS_RATE,                !input
     .            PROT_SPEED,PROT_UNCER,ALPH_SPEED,ALPH_UNCER) !output
c
c       ************************************************************************
c       *** This program uses a level one WIND file to determine the solar   ***
c       *** wind speed using both the proton and alpha particle peaks in the ***
c       *** DPPS versus FSR plot.                                            ***
c       ************************************************************************
c
c       author: Michael R. Collier                date: 13 December 1994
c       date: 15 December 1994   -added kappa function fitting in place of
c                                 Guassian fitting for the H and alpha peaks.
c                                 didn't work well so went back to Gaussians
c       date: 19 December 1994   -added routines to concentrate fitting on
c                                 just proton and helium peaks
c       date: 21 December 1994   -corrected compression mode output values,
c                                 orig. had output backwards although data
c                                 decompression was still done correctly
c       date: 22 December 1994   -implemented fitting in square root of E/Q
c                                 rather than in E/Q
c       date:  3 January  1995   -implemented saved results from earlier fits
c                                 when a singular matrix is obtained
c       date: 19 January  1995   -implemented check on helium speed to elim.
c                                 very rare overflow error in fitting
c       date:  1 February 1995   -added check on a5 and a6 in function routine
c                                 to elim overflow error in day 26 1995 data
c       date: 24 February 1995   -corrected sqrt error
c       date: 19 April    1995   -added output file with helium stats in it
c       date: 24 April    1995   -added factor of 1.28 into helium density to
c                                 account for DPPS spacing over acceptance
c       date: 17 August   1995   -changed dimension of "covar" and "alpha"
c   				  from 1000 to 10
c       date:  3 April    1996   -added check on a2 and a3 in function routine
c                                 to elim overflow error in day 30 1996 data
c       compile for/check=(nounderflow,bounds)/warn=decl/extend sw_speed
c
c       link sw_speed,lib_dir:wind_lib/lib,lib_dir:l1lib/lib,
c            [wind.software.l1]compare_time,mrqmin_sw,mrqcof_sw,covsrt_sw,
c            gaussj_sw
c
c       notes: 1. the E/Q value for the analyzer assumes E/Q=0.5156*
c                 (1.05135)^dpps step as per an e-mail message from Dr.
c                 Hamilton dated 29 November 1994
c              2. mrqmin_sw and gaussj_sw are modified mrqmin and gaussj to
c                 transfer the flag sm_flag which indicates a singular matrix
c                 if set.
c              3. if there is no identifiable helium peak, the proton peak
c                 will be fit and results read to file with the helium amp.
c                 zeroed and other para set to 1e6. If a singular matrix error
c                 occurs anywhere in the three fits performed, then both H and
c                 He amplitudes will be zeroed and other para set to 1e6.
c              4. minimum proton velocity: 314 km/s
c                 minimum helium velocity: 222 km/s
c              5. The sqrt error corrected on 24 February 1995 appears to affect
c                 the fits by a few km/s in both the convective and thermal
c                 speeds.
c
c
c
c       *** required include files ***
c
        include 'inc_dir:edbdef.inc/nolist'   ! required include files for data
        include 'inc_dir:l1fmt.inc/nolist'    ! structures req. by call prog.
        include '($rmsdef)'
c
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
c
        character l1_file_name*80,   ! contains the name of the input l1 file
     2            comp_mode*1        ! compression mode a or c
        integer*4 status,            ! non-zero indicates an i/o error
     2            ios,               ! non-zero indicates i/o error on write
     3            sr_num,            ! number of the science record read
     4            status_sr,         ! science record status flag
     5            status_ma,         ! mass read routine status flag
     6            status_ba,         ! basic core data read status
     7            status_hk,         ! house keeping data read status
     8            val_spin,          ! number of valid spins in science record
     9            i,j,k,l,           ! looping variables
     1            dpps_cyc(0:59),    ! dpps step corresponding to s/c spin num
     2            spin_num,          ! spacecraft spin number
     3            itemp,             ! temp I*4 for converting byte variables
     4            rates(0:59,27),    ! contains decompressed mass rates/cycle
     5            wind_decompress,   ! decompresses the compressed MASS rates
     6            nca,               ! variables that the fitting routine uses
     7            lista(6),          ! numbers the parameters for fitting
     8            iter,              ! number of iterations in curve fit routine
     9            fit_elements,      ! total number of data elements fit
     1            i_max_loc,         ! element location for maximum FSR2 counts
     2            he_pk_strt_ch,     ! starting helium peak channel
     3            he_fit_elements,   ! number of helium elements fit
     4            he_tot,            ! total number of helium elements in peak
     5            he_adps_step       ! the adps step of the fit helium peak
        logical   code,              ! comp mode-set is mode c, clear is mode a
     2            start_flag,        ! set to true when the start time found
     3            end_flag,          ! true indicates terminate reading file
     4            compare_time_ge,   ! routine is true if first time .ge. second
     5            sm_flag,           ! returned true indicates sing matrix error
     6            first_elem_flag,   ! false indicates first he elem not found
     7            he_sp_flag         ! true indicates unreasonable He speed
        real*4    x(60),y(60),sig(60),   ! fit variables-x,y, and uncertainty
     2            a(6),                  ! the parameters to be fit
     3            covar(10,10),      ! working space for fitting routine
     4            alpha(10,10),      ! working space for fitting routine
     5            chisqr,                ! chi-squared for fit
     6            alamda,                ! <0 on intial call, final call =0
     7            max_cnt,               ! contains maximum counts in array
     8            max_loc,               ! contains E/Q location of maximum
     9            prot_speed,            ! proton speed from E/Q mean
     1            prot_uncer,            ! uncert. on proton speed from width
     2            alph_speed,            ! alpha speed from E/Q mean
     3            alph_uncer,            ! uncert. on alpha speed from width
     4            fwhm,                  ! holds proton FWHM of the distribution
     5            he_max_cnt,            ! contains maximum helium counts
     6            he_max_loc,            ! contains E/Q location of maximum
     7            he_pk_strt_eq,         ! starting helium pk E/Q location
     8            factor,                ! frac of He fwhm searched on left
     9            he_x(60),he_y(60),
     1            he_sig(60),            ! the helium E/Q, FSR2 rates, and sigma
     2            save_H_a1,
     3            save_H_a2,
     4            save_H_a3,             ! proton prelim. fit res. saved
     5            save_He_a4,            ! (in case of sing. matrix error)
     6            save_He_a5,
     7            save_He_a6,            ! helium prelim. fit res. saved
     8            he_e_tot,              ! tot helium energy for eff. calc.
     9            eta_st_eta_h,          ! product of the start and sphere eff.
     1            alph_den               ! alpha particle density
        external  funcs,             ! subroutine to evaluate fitting function
     2            mrqmin             ! first subroutine of the fitting program
c
c       notes: 1. rates(xx,2) contains FSR2
c
c
           do spin_num=0,59   ! determine DPPS steps for each s/c spin number
              itemp=mass_rate.edb(spin_num).rate(25)
              dpps_cyc(spin_num)=ibits(itemp,0,6)
c             --- bit 7 is calibration mode status, bits 0-5 are DPPS step ---
c             ---ibits expects an I*2 or I*4 and ...rate(25) is a byte     ---
c             ---setting to itemp before ibits converts it to corr form    ---
           enddo
c
           itemp = hdb.bdh.dat(189)   ! compression codes for all MASS rates
           code = btest(itemp,4)      ! are in the hdb byte 189 bit 4
           if (code .eq. .true.)  comp_mode='c'
           if (code .eq. .false.) comp_mode='a'
c
           do spin_num=0,59
           do i=0,24   ! only decompress rates (i.e. only up to 24 not 26)
              rates(spin_num,i+1) = wind_decompress(
     2                              mass_rate.edb(spin_num).rate(i),code)
c             ---one routine decompresses both modes a and c---
           enddo
           rates(spin_num,26) = mass_rate.edb(spin_num).rate(25)   ! dpps step
           rates(spin_num,27) = mass_rate.edb(spin_num).rate(26)   ! adps step
           enddo
c
c
c       ***find the distribution maximum and location ***
c
        max_cnt = -1   ! contains maximum FSR2 counts
        max_loc =  1   ! contains the location of the maximum FSR2 counts
        do i=0,59
           if (rates(59-i,2) .gt. max_cnt) then   ! if a loc w/counts gt max_cnt
              max_cnt = rates(59-i,2)   ! then set max_cnt to the new value
              max_loc = 0.5156*(1.05135)**i   ! and save E/Q where stored
              i_max_loc = i    ! save element number of max_cnt
           endif
        enddo
c
c
c       *** determine the half width for protons ***
c
        i = i_max_loc
        do while ((rates(59-i,2) .gt. max_cnt/2.0) .and.
     2            (i .le. 59))   ! search for HWHM of dist
           i = i+1   ! search to the right of the peak for element with half max
        enddo
        fwhm = 2*(0.5156*(1.05135)**i-max_loc)   ! fwhm holds the H FWHM in E/Q
c
c
c       *** load up array one fwhm around the proton peak ***
c
        fit_elements = 0  ! counts the number of elements to be fit
        i = 0
        do while ( (0.5156*(1.05135)**i .le. max_loc+fwhm/2.0) .and.
     2             (i .le. 59) )
c       ---look within one fwhm of the proton peak for data to be fit---
           if ((0.5156*(1.05135)**i .ge. max_loc-fwhm/2.0) .and.
     2         (0.5156*(1.05135)**i .le. max_loc+fwhm/2.0) .and.   ! within fwhm
     3         (rates(59-i,2) .ne. 0) .and.   ! and FSR2 rate non-zero
     4         (i .le. 59)) then   
              fit_elements = fit_elements + 1   ! increment number of fit elem
              x(fit_elements)=0.5156*(1.05135)**i   ! E/Q value
              y(fit_elements)=rates(59-i,2)
              sig(fit_elements)=(rates(59-i,2))**0.5
            endif
            i = i+1   ! increment i in preparation for next step
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
     2            (fit_elements .ge. 3))
c
           call mrqmin(x,y,sig,fit_elements,a,6,lista,3,covar,alpha,
     2                 nca,chisqr,funcs,alamda,sm_flag,max_cnt,max_loc,
     3                 fwhm,he_max_cnt)   
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
c
c       *** find a reasonable helium peak ***
c
c       --- section 1: search for everything within fac of fwhm either side---
c
        he_tot = 0   ! total number of helium elements fit
        do while ( (0.5156*(1.05135)**i .le. 4*max_loc) .and.
     2             (i .le. 59) )   ! make sure not past end of array
           if ((0.5156*(1.05135)**i .ge. 2*max_loc-fwhm/0.50) .and.
     2         (0.5156*(1.05135)**i .le. 4*max_loc) .and. 
     3         (rates(59-i,2) .ne. 0)) then   ! FSR2 rate non-zero
              he_tot = he_tot + 1   ! increment total in helium peak
              he_x(he_tot) = 0.5156*(1.05135)**i   ! sqrt(E/Q) of hel dat
              he_y(he_tot) = rates(59-i,2)         ! FSR2 cnts of helium datum
           endif
           i = i+1   ! increment i
        enddo
c
c       --- section 2: find the maximum in the helium array ---
c
        he_max_cnt = -1   ! for comparison, set initial value low
        do j=1,he_tot   ! cycle through all the elements
           if (he_y(j) .gt. he_max_cnt) then   ! if new maximum found then
              he_max_cnt = he_y(j)   ! set the variable equal to its value
           endif
        enddo
c
c       --- section 3: eliminate maxima at the edges ---
c
        if ((he_max_cnt .eq. he_y(1)) .or.
     2      (he_max_cnt .eq. max_cnt)) then   ! indicates a maxima at the edge
           do while((he_max_cnt .eq. he_y(1)) .or. (he_max_cnt .eq. max_cnt))
              do j=1,he_tot-1   ! cycle through all helium elements save one
                 he_x(j)=he_x(j+1)   ! move every element down one in the array
                 he_y(j)=he_y(j+1)
              enddo
              he_tot=he_tot-1        ! decrease total number of elements by one
c             --- recalculate the maximum value ---
              he_max_cnt = -1   ! for comparison, set initial value low
              do j=1,he_tot   ! cycle through all the elements
                 if (he_y(j) .gt. he_max_cnt) then   ! if new maximum found then
                    he_max_cnt = he_y(j)   ! set the variable equal to its value
                 endif
              enddo
c
           enddo
        endif
c
c       --- section 4: calculate helium sigmas ---
c       
        do j=1,he_tot
           he_sig(j) = he_y(j)**0.5
        enddo
c
c
c       *** estimate the fitting parameters ***
c
        if (sm_flag .eq. .false.) then   ! only change values if no sing. matrix
        if (he_tot .gt. 0) then   ! if there's an iden. he peak
           a(6) = fwhm**2/22.2/(2*max_loc)  ! derived approximation
           a(4) = he_max_cnt*a(3)**0.5*2.5066283/(2*max_loc)
           a(5) = 2*max_loc
        elseif (he_tot .eq. 0) then   ! if no peak, set amplitude to zero
           a(4)=0
           a(5)=1.0437e7
           a(6)=1.0437e7   ! these values will set speed/width to 1e6 to flag
        endif
        endif
c
c
c       *** fit the helium peak ***
c
        if (he_tot .gt. 0) then   ! only fit the he if there's an ident. peak
        nca = 7
        alamda = -1
        lista(1)=4
        lista(2)=5
        lista(3)=6
        lista(4)=1
        lista(5)=2
        lista(6)=3    ! set up the lista array to fit the helium data a4-a6
c
        iter = 0   ! reset the iteration counter
        do while ((alamda .lt. 1e4) .and. (iter .lt. 1000) .and.
     2            (sm_flag .eq. .false.) .and.   ! no singular matrix
     3            (he_tot .ge. 3))
c
           call mrqmin(he_x,he_y,he_sig,he_tot,a,6,lista,3,covar,
     2                    alpha,nca,chisqr,funcs,alamda,sm_flag,max_cnt,max_loc,
     3                    fwhm,he_max_cnt)   
c                      ---fit_ele data pts, 3 fit para, 3 parameters adjusted---
c                      ---sm_flag returned set if singular matrix error  ---
c          --- check for reasonable helium speed ---
           he_sp_flag = .false.   ! originally assume He speed reasonable
           if ((a(5) .lt. 0.5) .or. (a(5) .gt. 13.0)) then   ! a(5) is He speed
              he_sp_flag = .true.   ! flag indicates an unreasonable He speed
              sm_flag = .true.
           endif   ! helium fit peak out of range of energy per charge analyzer
           if (sm_flag .eq. .true.) then   ! indicates a singular matrix problem
c             if (he_sp_flag .eq. .false.) type *,'singular matrix 2'
c             if (he_sp_flag .eq. .true.)  type *,'unreasonable He speed'
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
        endif   ! endif for only fit the helium if iden. peak
c
c       --- section 4: insert into the standard fitting array ---
c
        do j = fit_elements+1,fit_elements+he_tot
           x(j) = he_x(j-fit_elements)
           y(j) = he_y(j-fit_elements)
           sig(j) = (he_y(j-fit_elements))**0.5
        enddo
        fit_elements = fit_elements+he_tot   ! increase number of fit elements
c
c
c       *** perform fitting ***
c
        if (he_tot .gt. 0) then   ! only fit again if data have been added
        nca = 7
        alamda = -1
        do i=1,6   ! adjust all of the parameters
           lista(i)=i
        enddo
c
        iter = 0   ! reset the iteration counter
        do while ((alamda .lt. 1e4) .and. (iter .lt. 1000) .and.
     2            (sm_flag .eq. .false.) .and.
     3            (fit_elements .ge. 3))
c
           call mrqmin(x,y,sig,fit_elements,a,6,lista,6,covar,alpha,
     2                 nca,chisqr,funcs,alamda,sm_flag,max_cnt,max_loc,
     3                 fwhm,he_max_cnt)   
c                      ---fit_ele data pts, 6 fit para, 6 parameters adjusted---
c                      ---sm_flag returned set if singular matrix error  ---
           if (sm_flag .eq. .true.) then   ! indicates a singular matrix problem
              type *,'singular matrix 3'
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
        endif   ! endif for only fit again if data have been added
c
        prot_speed = 437.74*a(2)**0.5  ! proton speed in km/s
        prot_uncer = 437.74*a(3)**0.5  ! uncertainty on proton speed in km/s
        alph_speed = 309.53*a(5)**0.5  ! alpha speed in km/s
        alph_uncer = 309.53*a(6)**0.5  ! uncertainty on alpha speed in km/s


           if ((sm_flag .eq. .false.) .and. (he_sp_flag .eq. .false.) .and.
     2         (he_tot .gt. 3) .and.
     3         (nint(alog(a(5)/0.5156)/0.050075) .ge. 0) .and.
     4         (nint(alog(a(5)/0.5156)/0.050075) .le. 59)) then
c          --- start if for only if no sing. matrix and no speed flag and
c              enough of a helium peak and dpps step is in range ---
           he_adps_step = rates(59-nint(alog(a(5)/0.5156)/0.050075),27)
c          --- a(5) is E/Q of the helium peak, nint conv. to dpps step ---
           if (he_adps_step .lt. 0) he_adps_step=he_adps_step+256   ! neg num
           if (he_adps_step .lt. 11) he_adps_step = 11
           if (he_adps_step .gt. 160) he_adps_step = 160   ! assumed limits
           he_e_tot = 2*a(5) - 2*(2.054-0.0322*he_adps_step)
           eta_st_eta_h = 0.442*(1-exp(-0.162*he_e_tot))
           alph_den = 28.125*50.0*3.23e-8*2.86*1.28*a(4)/eta_st_eta_h
c          --- the individual factors correspond to:
c              28.125  - 1/duty cycle = 1/(4*3.2/360)
c              50.0    - v/delta v
c              3.23e-8 - conversion from sart(E/Q) to km/s for helium
c              2.86    - 1/area of sphere ---
c              1.28    - DPPS spacing/DPPS passband width
           endif   ! endif for no singular matrix, no speed flag, enough of
c                  ! a helium peak and the dpps is in range
c
c
        RETURN
        END
c
c**********************************************************************************
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
c      --- keep the values within reasonable bounds ---
c
        if (a(2) .gt. 1e18) a(2)=1e18
        if (a(3) .gt. 1e18) a(3)=1e18
        if (a(5) .gt. 1e18) a(5)=1e18
        if (a(6) .gt. 1e18) a(6)=1e18   ! to avoid overflow errors
c
c       if ( (a(2) .lt. 1e6) .and. (a(2) .lt. 0.5) ) a(2)=0.5
c       if ( (a(2) .lt. 1e6) .and. (a(2) .gt. 10) ) a(2)=10
c       if ( (a(3) .lt. 1e6) .and. (a(3) .lt. 0.5) ) a(3)=0.5
c       if ( (a(3) .lt. 1e6) .and. (a(3) .gt. 10) ) a(3)=10
c       if ( (a(5) .lt. 1e6) .and. (a(5) .lt. 0.5) ) a(5)=0.5
c       if ( (a(5) .lt. 1e6) .and. (a(5) .gt. 10) ) a(5)=10
c       if ( (a(6) .lt. 1e6) .and. (a(6) .lt. 0.5) ) a(6)=0.5
c       if ( (a(6) .lt. 1e6) .and. (a(6) .gt. 10) ) a(6)=10
c
c       if (a(1) .gt. 2*max_cnt*(fwhm/2.35**0.5)*2.5066283) 
c     2     a(1)=max_cnt*(fwhm/2.35**0.5)*2.5066283   ! amp lim
c       if (a(2) .gt. 10) a(2)=max_loc        ! mean must be less than E/Q range
c       if (a(3) .gt. 10) a(3)=fwhm**2/2.35   ! width must be less than E/Q range
c       if (a(4) .ne. 0) then   ! a(4)=0 implies fitting only the proton peak
c          if (a(4) .gt. he_max_cnt*(fwhm/2.35**0.5)*2.5066283) 
c     2    a(4)=he_max_cnt*(fwhm/2.35**0.5)*2.5066283 ! amp lim
c          if (a(5) .gt. 10) a(5)=2*max_loc   ! mean must be less than E/Q range
c          if (a(6) .gt. 10) a(6)=fwhm**2/2.35! width must be less than E/Q range
c       endif
c
c      --- evaluate the function ---
c
       y =(a(1)*exp(-(x-2*x**0.5*a(2)**0.5+a(2))/2/a(3))/(a(3)**0.5*2.5066283) +
     2     a(4)*exp(-(x-2*x**0.5*a(5)**0.5+a(5))/2/a(6))/(a(6)**0.5*2.5066283))
     3     *x   ! delta(E/Q) prop to E/Q
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
       dyda(4) = x*exp(-(x-2*x**0.5*a(5)**0.5+a(5))/2/a(6))/
     2           (a(6)**0.5*2.5066283)*x
       dyda(5) = ( ((x/a(5))**0.5-1)/(2*a(6)) )*
     2           ( x*a(4)*exp(-(x-2*x**0.5*a(5)**0.5+a(5))/(2*a(6))) ) /
     2           (a(6)**0.5*2.5066283)*x
       dyda(6) = -( x*a(4)*exp(-(x-2*x**0.5*a(5)**0.5+a(5))/(2*a(6))) ) /
     2           (a(6)**1.5*2*2.5066283) + 
     3            ( x*a(4)*exp(-(x-2*x**0.5*a(5)**0.5+a(5))/(2*a(6))) ) /
     4           (a(6)**0.5*2.5066283) *
     5           ( (x-2*a(5)**0.5*x**0.5+a(5))/(2*a(6)**2) )*x
c
        return
        end
