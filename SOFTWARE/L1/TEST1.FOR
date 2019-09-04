	include 'inc_dir:ephdef.inc'
	integer maxsize
	parameter (maxsize=5000)
	real*8 beg_date,end_date, epoch_arr(maxsize)
	integer year,month,day,nsize,nfilled
	logical what_pos(7)/7*.true./
	record /ephemeris/ gci_pos_arr(maxsize),
	1 gci_vel_arr(maxsize), gse_pos_arr(maxsize), gse_vel_arr(maxsize),
	1 gsm_pos_arr(maxsize), gsm_vel_arr(maxsize), sun_vec_arr(maxsize) 
C
10	type *,'enter maximum # of values to retirieve (<=500)'
	accept *,nsize
	type *,'enter starting date (year,month,day)'
	accept *,year,month,day
	call compute_epoch(year,month,day,0,0,0,0,beg_date)
	type *,'enter ending date (year,month,day)'
	accept *,year,month,day
	call compute_epoch(year,month,day,23,59,59,0,end_date)

	call get_orbit_days(beg_date,end_date,what_pos,nsize,nfilled,
	1 epoch_arr,gci_pos_arr,gci_vel_arr,gse_pos_arr,gse_vel_arr,gsm_pos_arr,
	1 gsm_vel_arr,sun_vec_arr)

	type *,'filled ',nfilled,', entries'
	goto 10
	end
