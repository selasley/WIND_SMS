	subroutine dump_hdb(jout,hdbrec)
	implicit none
        INCLUDE 'INC_DIR:EDBDEF.INC/NOLIST'
	record /HDBstr/ hdbrec
	integer jout,ii
	character*30 format_time
c
	write(jout,1001) 
1001	format(1x,'DUMP OF HDB')
	write(jout,1002) format_time(hdbrec.w_time),hdbrec.epoch
1002	format(1x,'  w_time: ',a,5x,f15.0)
	write(jout,1003) format_time(hdbrec.sc_time),hdbrec.sc_epoch
1003	format(1x,' sc_time: ',a,5x,f15.0)
	write(jout,1005) hdbrec.qual
1005	format(1x,'qual: ',z8)
	write(jout,1006) (hdbrec.dat(ii),ii=0,279)
1006	format(9x,10z3.2,5x,10z3.2/9x,10z3.2,5x,10z3.2/)
	return
	end
	character*30 function format_time(timestrc)
	implicit none
        include 'inc_dir:edb.inc/nolist'
	record /t/ timestrc
	character*30 temp
	write(temp,1001) timestrc.year,timestrc.doy,timestrc.hr,timestrc.min,
     &			 timestrc.sec,timestrc.csec
1001	format(6i5)
	format_time = temp
	return
	end
