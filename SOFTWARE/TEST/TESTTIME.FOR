	integer*4 sctime(2), status
	real*8 epoch1
	integer*4 year, doy, msec, micro
	data sctime/'4c32'x,'a130'x/
c
	type 101,sctime
101	format(' SC input time =',2x,z8.8,2x,z8.8)
	call wind_pb5_to_epoch( sctime, epoch1, status )
	type 102,epoch1
102	format(' computed EPOCH time = ',f20.1)
	call wind_epoch_to_greg( year, doy, msec, micro, epoch1, status )
	type 103, year, doy, msec, micro
103	format(' year = ',i5,', doy = ',i5,', msec = ',i6,', micro = ',i5)
	end
