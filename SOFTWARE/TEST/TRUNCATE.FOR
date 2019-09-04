	character*80 filename
	type *,'enter filename'
	accept 101,nch,filename
101	format(q,a)
	open(15,file=filename(1:nch),initialsize=5000,
     &		status='new',recl=512,carriagecontrol='list')
	do i=1,100
	  write(15,102) i,i*i,2*i,(i*2)*2
102	  format(4(108x,i20))
	enddo
	close(15)
	call lib$spawn('dir/siz=all '//filename(1:nch))
	call shorten(filename(1:nch))
	call lib$spawn('dir/siz=all '//filename(1:nch))
	end

	subroutine shorten(filename)
	character*(*) filename
	include '($fabdef)'
	include '($rabdef)'
	record /fabdef/ myfab
	record /rabdef/ myrab
	integer*4 for$rab,rabadr,truncate,istat
	character*16384 buf

	open(14,file=filename,status='old')
	rabadr = for$rab(14)
	read(14,1001) nbytes,buf
1001	format(q,a)
	type *,'read first rec, nbytes=',nbytes
	rewind 14
	istat = truncate(%val(rabadr))
        if (.not.istat) call lib$signal(%val(istat))
	return
	end

	integer*4 function truncate(myrab)
	include '($rabdef)'
	record /rabdef/ myrab
	integer*4 sys$truncate

	truncate = sys$truncate(myrab,,)
	return
	end
