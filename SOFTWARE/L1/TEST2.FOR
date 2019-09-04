	integer mode/1/, units, system, step
	logical what_info(7)
	real*8 beg_date, end_date
10	call get_orbit_request(mode, beg_date, end_date, what_info, units,
	1 system, step) 
	type *, beg_date, end_date, what_info, units,
	1 system, step
	goto 10
	end
