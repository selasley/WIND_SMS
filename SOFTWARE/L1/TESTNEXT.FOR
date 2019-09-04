	include 'inc_dir:l1fmt.inc'
	character filename*80,line*80
C
	filename = 'DATA_DIR:WI_L1_SMS_*.DAT'
	call wind_get_filename(filename,istat)
	call wind_l1_open(2,filename,l1_file_header,istat)
	type *,'opened ',filename(1:60)
	type *,'istat=',istat
	type *,'enter mode'
	accept *,mode
100	call wind_l1_open_next(2,mode,filename,l1_file_header,istat)
	type *,'opened ',filename(1:60)
	type *,'istat=',istat
	call lib$signal(%val(istat))
	read(*,101) line
101	format(a)
	goto 100
	end
