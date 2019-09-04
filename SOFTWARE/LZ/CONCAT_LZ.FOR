	PROGRAM CONCAT_LZ
 
C+
C 
C ABSTRACT: 
C 
C	 This program creates a complete Level Zero file for a given date by
C	 combining with that date's Level Zero file major frames from the
C	 preceeding and following days, thus ensuring complete Level 1 records
C	 can be formed at the start and end of the target day.
C
C	 CONCAT_LZ reads three LZ files and creates another LZ file.  The Label
C	 Record for the output file is updated to reflect information about the
C	 three input LZ files.
C 
C COMMON BLOCKS:
C  
C     {tbs}
C  
C DESIGN:
C  
C     We can optimize operations by reading and ignoring all but the last
C     READ_FROM_PREV major frames from the preceeding day.  These are written to
C     the output LZ file, followed by all records in the main day's LZ file,
C     followed by the first READ_FROM_NEXT major frames for the following day's
C     LZ file.  The label record is updated appropriately and is rewritten into
C     the output file.
C
C     Use the Label Record from the main day as a starting point for the output
C     file's Label Record.  Update its starting and ending times (both
C     spacecraft and wall), first major frame count, and last major frame
C     count.  Store the actual number of major frames written.  Increase the
C     expected number of major frames by the sum of the number of major frames
C     read from the previous and following days.  Leave the rest alone as
C     there's no dependable way of determining what their "correct" values
C     should be.
C
C 
C Current version: 
C 
C	 16-MAR-1995
C 
C Created: 
C 
C	 16-MAR-1995    Larry Bleau
C 
C Revised:
C 
C-
	IMPLICIT NONE
	include 'INC_DIR:L0_LABL_RECORD.INC'
	include 'INC_DIR:L0_DATA_RECORD.INC'
C
	integer read_from_last/5/
	integer read_from_next/15/
C
	integer istat,nbytes,recs_written,nskip,i,j,nch,nblocks
	character*80 lz_name,lz_nameout
	character*13000 lr_buffer1,lr_buffer2,lr_buffer3
	common /openfile/ nblocks
C
C Open all three input files
C
	write(*,1001) 'LZ FILENAME FOR MAIN DAY = '
1001	format(1x,a,$)
	lz_name = 'WIND_LZ_DATA:WI*LZ*.DAT'
	call wind_get_filename( lz_name, istat )
	if (istat .ne. 1) call lib$stop(%val(istat))
	call open_lz_file( 2, lz_name, 'OLD', istat )
	if (istat .ne. 1) call lib$stop(%val(istat))
	lz_nameout = lz_name
C
	write(*,1001) 'LZ FILENAME FOR PREVIOUS DAY = '
	lz_name = 'WIND_LZ_DATA:WI*LZ*.DAT'
	call wind_get_filename( lz_name, istat )
	if (istat .ne. 1) call lib$stop(%val(istat))
	call open_lz_file( 1, lz_name, 'OLD', istat )
	if (istat .ne. 1) call lib$stop(%val(istat))
C
	write(*,1001) 'LZ FILENAME FOR NEXT DAY = '
	lz_name = 'WIND_LZ_DATA:WI*LZ*.DAT'
	call wind_get_filename( lz_name, istat )
	if (istat .ne. 1) call lib$stop(%val(istat))
	call open_lz_file( 3, lz_name, 'OLD', istat )
	if (istat .ne. 1) call lib$stop(%val(istat))
C
C Read the main day's Label Record (second input file) and save it for later.
C Using the record count in the header record compute the expected size of the
C output file.  Use this information to allocate a proper size output file when
C we open it.
C
	read(2,1002,err=900,end=900,iostat=istat) nbytes,lr_buffer2(1:nbytes)
	c_lr_buffer = lr_buffer2
	type *,LR_Phys_Rec_in_File,READ_FROM_LAST,READ_FROM_NEXT
	nblocks = ((LR_Phys_Rec_in_File + READ_FROM_LAST + READ_FROM_NEXT) *
     *            10800) / 512
	type *,nblocks
C
C Construct the name of the output LZ file; give it data set number 00
C Open the output LZ file
C
	nch = INDEX( lz_nameout, ';' )
	lz_nameout(nch:) = ' '
	nch = nch - 1
	i = INDEX( lz_nameout, ']' )
	j = INDEX( lz_nameout(i+1:), '.' )
	lz_nameout(i+j-2:i+j-1) = '00'
	write(*,1001) 'OUTPUT LZ FILENAME = '
	call wind_get_filename( lz_nameout, istat )
	if (istat .ne. 1 .and. istat .ne. 0) call lib$stop(%val(istat))
	nch = INDEX( lz_nameout, ';' )
	if (nch .ne. 0) then
	  lz_nameout(nch:) = ' '
	  nch = nch - 1
	endif
	call open_lz_file( 4, lz_nameout, 'NEW', istat )
	if (istat .ne. 1) call lib$stop(%val(istat))
C
C Read the Label Record from the first input file.
C We'll need the number of major frames in the file to know when to stop
C discarding major frames and start writing them out.
C
	read(1,1002,err=900,end=900,iostat=istat) nbytes,lr_buffer1(1:nbytes)
1002	format(q,a)
	c_lr_buffer = lr_buffer1
	nskip = LR_Phys_Rec_in_File - READ_FROM_LAST
C
C Bypass the label record on the third input file.
C
	read(3,1002,err=930,end=930,iostat=istat) nbytes,lr_buffer3(1:nbytes)
C
C Write the main file's Label Record to the output file and
C This will be modified later on and rewritten to the start of the output file.
C
	WRITE( 4, 1003, ERR=950 ) lr_buffer2(1:nbytes)
1003	format(a)
C
C Read the first LZ file and discard all but the last READ_FROM_LAST major
C frames (records). Then read one more, so that we have in memory the 1st data 
C record that we want to keep.
C
	type *,'skipping ',nskip,' records from 1st file'
	DO i = 1, nskip + 1
	  read(1,1002,err=910,end=920,iostat=istat) nbytes,c_dr_buffer(1:nbytes)
	END DO
C
C Restore the contents of the main file's label record into C_LR_BUFFER.
C Copy the information from the 1st data record that we want to keep into the
C label record, replacing those values from the main input file.  Label Record
C values not replaced will contain the values from the main input file.
C
	c_lr_buffer = lr_buffer2
	LR_Beg_MajFr_Cnt  = DR_Beg_MajFr_Cnt       ! Major Frame Count-Beginning of Maj.Fra
	LR_Beg_SC_Clock(1) = DR_Beg_SC_Clock(1)    ! Spacecraft Clock -Beginning of Maj.Fra
	LR_Beg_SC_Clock(2) = DR_Beg_SC_Clock(2)    ! Spacecraft Clock -Beginning of Maj.Fra
	LR_Beg_Year       = DR_Beg_Year            ! ATC: year-Beginning of Major Frame
	LR_Beg_DOY        = DR_Beg_DOY             ! ATC: doy -Beginning of Major Frame
	LR_Beg_MSec       = DR_Beg_MSec            ! ATC: msec-Beginning of Major Frame
	LR_Beg_MicSec     = DR_Beg_MicSec          ! ATC: usec-Beginning of Major Frame
C
C Write this data record to the output LZ file.
C Finish reading the previous day's LZ file, copying the major frames to the
C output file. 
C Keep a counter of how many records are written.
C
	write(4,1003,err=950,iostat=istat) c_dr_buffer(1:nbytes)
	recs_written = 1
	type *,'copying remainder of 1st file'
	call copy_records( 1, 4, -1, recs_written )
C
C Copy all major frames (data records) from the main day's LZ file to the
C output file.
C
	type *,'copying 2nd file'
	i = recs_written
	call copy_records( 2, 4, -1, recs_written )
	type *,'  copied',recs_written-i,' records'
C
C Read READ_FROM_NEXT major frames from the third input file and copy them to
C the output file. 
C
	type *,'copying 3rd file'
	call copy_records( 3, 4, READ_FROM_NEXT, recs_written )
	type *,'total # of records copied =',recs_written
C
C At this point the output file is complete as far as data records.  Update the
C memory copy of the Label Record with the most current information.  In cases
C where that is not available use a best guess.
C
	LR_End_MajFr_Cnt  = DR_Beg_MajFr_Cnt       ! Major Frame Count-Beginning of Maj.Fra
	LR_End_SC_Clock(1) = DR_Beg_SC_Clock(1)    ! Spacecraft Clock -Beginning of Maj.Fra
	LR_End_SC_Clock(2) = DR_Beg_SC_Clock(2)    ! Spacecraft Clock -Beginning of Maj.Fra
	LR_End_Year       = DR_Beg_Year            ! ATC: year-Beginning of Major Frame
	LR_End_DOY        = DR_Beg_DOY             ! ATC: doy -Beginning of Major Frame
	LR_End_MSec       = DR_Beg_MSec            ! ATC: msec-Beginning of Major Frame
	LR_End_MicSec     = DR_Beg_MicSec          ! ATC: usec-Beginning of Major Frame
	LR_Num_MajFr_File = recs_written           ! Number of Major Frames in File
	LR_Phys_Rec_in_File = recs_written         ! Number of records in file
C
C Close the output file and open it again to update (rewrite) its label record.
C
	CLOSE( 4 )

	OPEN( 4, FILE=LZ_NAMEOUT, STATUS='OLD', RECORDSIZE=10800, IOSTAT=istat, ERR=940)
	READ( 4, 1002, ERR=950, END=950, IOSTAT=istat ) nbytes,lr_buffer2(1:nbytes)
	REWRITE( 4, 1003, ERR=950 ) c_lr_buffer(1:nbytes)
	CLOSE( 4 )
C
C All done
C Close files and exit
C
	CLOSE( 1 )
	CLOSE( 2 )
	CLOSE( 3 )
	CALL EXIT
C
C Come here for errors
C
900	write(*,*) 'Error reading Label Record from 1st input file; istat=',istat
	STOP
910	write(*,*) 'Error reading data record',i,' from 1st input file; istat=',istat
	STOP
920	write(*,*) 'End-of-file reading data record',i,' from previous day''s LZ file'
	STOP
930	write(*,*) 'Error reading Label Record from 3rd input file; istat=',istat
	STOP
940	write(*,*) 'Error reopening output file; istat=',istat
	STOP
950	write(*,*) 'Error rewriting output file''s label record; istat=',istat
	STOP
	END
 
	subroutine open_lz_file( unit, filename, status, istat )
	integer unit,istat
	character*(*) filename,status
	integer forterr,rmsstc,rmsstv,lun,vaxerr,nblocks
	common /openfile/ nblocks
C
	if (status .eq. 'OLD') then
	open( unit=unit, file=filename, status=status, recordsize=10800,
     *		form='FORMATTED', carriagecontrol='NONE',
     *		readonly, iostat=istat, err=900 )
	endif
	if (status .eq. 'NEW') then
	  open( unit=unit, file=filename, status=status, recordsize=10800,
     *		recordtype='FIXED', form='FORMATTED', carriagecontrol='NONE',
     *		initialsize=10000, iostat=istat, err=900 )
	endif
	istat = 1
	return
C
C Come here for errors
C
900	call errsns(forterr,rmsstc,rmsstv,lun,vaxerr)
	write(*,*) 'Error opening ',status,' file; istat=',istat,', filename='
	write(*,*) filename
	istat = rmsstv
	return
	end

	subroutine copy_records( jin, jout, nrecords, recs_written )
	implicit none
	include 'INC_DIR:L0_DATA_RECORD.INC'
C
	integer jin, jout, nrecords, recs_written
	integer count,nbytes,istat,i
C
	count = nrecords
	if (count .le. 0) count = 999999
	do i = 1, count
	  read( jin, 1001, err=900, end=910, iostat=istat ) nbytes,c_dr_buffer(1:nbytes)
1001	  format(q,a)
	  write( jout, 1002, err=920, iostat=istat ) c_dr_buffer(1:nbytes)
1002	  format(a)
	  recs_written = recs_written + 1
	end do
	return
C
C Come here for errors
C
900	write(*,*) 'Error reading input LZ file, unit=',jin,', istat=',istat
	write(*,*) 'Attempted read on record ',i
910	return
920	write(*,*) 'Error writing output LZ file, unit=',jout,', istat=',istat
	write(*,*) 'Attempted write on record ',recs_written
	return
	end
