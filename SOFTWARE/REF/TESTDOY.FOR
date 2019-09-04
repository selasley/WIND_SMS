	integer*4 year,doy,month,day
	character*11 date_str
10      type *,'Enter year, doy'
        accept *,year,doy
	call doy_to_month_day( year, doy, month, day, date_str )
        type *,'month, day, datestr=',month,day,' ',date_str
        goto 10
        end
