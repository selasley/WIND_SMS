$!********************************************************************
$! Filename:   WIND_STICS_PLOT.COM
$!
$! Author:     Larry Bleau
$!
$! Date:       1-SEP-2005
$!
$! Purpose:
$!   Extract WIND STICS information from a WIND Level 1 (L1) file and create
$! a plot in a PNG file.
$!
$! Description:
$!   Control logic allows the procedure to be run on a single or a set of L1
$! files.  For each L1 file construct DCL commands to run several programs.
$! First run the three programs STICS_MMQ_TABLE, STICS_MQET_MATRIX, and
$! WSTICS_RATES to extract and write out selected WIND information.  Next
$! run IDL to generate a PNG plot.
$!
$! Parameters:
$!   P1 - option keywords:
$!          REGULAR  submit this job to run again at 9:00am on the next day
$!          DIRECT   run the constructed batch file immediately
$!          TEST     test mode; do not SUBMIT the command file as a batch job;
$!                   just build it and allow the user to inspect and possibly
$!                   modify it 
$!	    DEBUG    do not turn verify mode off during main loop
$!
$!   P2 - File selection.  This can be a numeric date, the name of file, or one
$!        of the keywords AUTO, ALL or RECENT.  If a numeric date construct the
$!        corresponding L1 filename and use it.  If a filename only that file
$!        is processed.  If omitted assume RECENT.
$!        Keywords:
$!          AUTO     process only L1 files that do not have corresponding STICS
$!                   PNG plot files
$!          ALL      process all L1 files
$!          RECENT   process only those L1 files created since 9:00am of the
$!                   preceeding day
$!	    yymmdd   use specified date, search for filename
$!	    yyyymmdd use specified date, search for filename
$!	    filename use specified filename
$!          omitted  same as RECENT
$!
$!********************************************************************
$ dirlist := SYS$LOGIN:STICS_DIRLIST.TMP
$ msgfile := SYS$LOGIN:STICS_GEN_LIST.TMP
$ batchfile := SYS$LOGIN:STICS_GEN.TMP
$ tempfile := SYS$LOGIN:FTP_STICS_FILES.TMP
$ logfile := SYS$LOGIN:FTP_STICS_FILES.LOG
$ l1pattern := WIND_L1_DATA:WI_L1_SMS_%%%%%%%%_V%%.DAT
$ wwwdir := SMS1:[WIND.WWW.STICS]
$ starttime := 9:00
$ maillist := PAQUETTE
$ test = f$locate("TEST",p1) .ne. f$length(p1)
$ regular = f$locate("REGULAR",p1) .ne. f$length(p1)
$ direct = f$locate("DIRECT",p1) .ne. f$length(p1)
$ debug = f$locate("DEBUG",p1) .ne. f$length(p1)
$
$ nfiles = 0
$ ydt = f$cvtime("YESTERDAY","ABSOLUTE","DATE")  !yesterday
$ ydt = ydt + " ''starttime'"                    !at 9 am
$
$ regular_entry = ""
$ if regular
$ then
$   SUBMIT/NOPRINT/AFTER="TOMORROW+''starttime'" -
   'f$parse(";",f$environment("PROCEDURE"))' /PARA=("''p1'","''p2'")
$   regular_entry := '$entry
$ endif
$!
$! If P2 is ALL, RECENT, or absent use DIRECTORY to generate a list of
$! filenames.
$! If P2 is given use it as the filename/date to process.
$!   If it is yymmdd or yyyymmdd construct pattern for L1 filename and search
$!   for it. 
$!   If it is a filename just do a search.
$!
$ if p2 .eqs. "" then p2 := RECENT
$ if p2 .eqs. "ALL" .or. -
     p2 .eqs. "AUTO" .or. -
     p2 .eqs. "RECENT" then goto get_file_list
$
$ if f$type(p2) .eqs. "INTEGER" .and. f$length(p2) .eq. 6
$ then
$   if f$integer(p2) .lt. 90
$   then
$     p2 = "20" + p2
$   else
$     p2 = "19" + p2
$   endif
$ endif
$ if f$type(p2) .eqs. "INTEGER" .and. f$length(p2) .eq. 8
$ then
$   l1pattern := WIND_L1_DATA:WI_L1_SMS_'p2'_V%%.DAT;
$ else
$   l1pattern := 'f$parse(p2,"WIND_L1_DATA:;")
$ endif
$ l1file = f$search(l1pattern)
$ if l1file .eqs. "" then goto no_filename
$!
$! If numeric date format was used l1pattern will contain "%%"
$! If it does keep searching until we get highest Vnn number
$!
$ if f$locate("%%",l1pattern) .ne. f$length(l1pattern)
$ then
$searchloop:
$ prev_l1file = l1file
$ l1file = f$search(l1pattern)
$ if l1file .nes. "" then goto searchloop
$ l1file = prev_l1file
$ endif
$ full_l1file = l1file
$ l1file := 'f$extract(f$locate("]",l1file)+1,999,l1file)'
$ write sys$output "Using ",l1file
$ read_list := N
$ goto process
$
$get_file_list:
$!
$! Generate list of L1 files to process.
$! If P2 is RECENT list L1 files created since 9:00am yesterday.
$! If P2 is ALL or AUTO list all L1 files.
$! If output file is empty (0 length) do nothing.
$!
$ selection = "/SINCE=""" + ydt + """"
$ if p2 .eqs. "ALL" .or. p2 .eqs. "AUTO" then selection :=
$ write sys$output "Using DIRECTORY /COLUMN=1 ",selection," /OUTPUT=''dirlist' ''l1pattern'"
$ DIRECTORY /COLUMN=1 'selection' /OUTPUT='dirlist' 'l1pattern'
$ fsize = f$file_attributes("''dirlist'","EOF")
$ if fsize .eqs. 0 then goto no_files
$!
$! Open file for reading and skip first three lines (header)
$!
$ i = 0
$ on control_y then goto closeup1
$ open/read list 'dirlist'                       !read headers
$ read/error=end/end=end list l1file
$ read/error=end/end=end list l1file
$ read/error=end/end=end list l1file
$ read_list := Y
$process:
$!
$! Open .COM file and start building batch runstream
$!
$ on control_y then goto closeup2
$ open/write bat 'batchfile'
$ write bat "$ SET NOON"
$ write bat "$ SET PREFIX ""(!5%T) """
$ write bat "$ SET DEFAULT SMSDISK4:[WIND.L2DATA.STICS]"
$!
$! Open temporary file to contain list of files processed
$!
$ on control_y then goto closeup3
$ open/write msg 'msgfile'
$!
$! Perform this loop once for every L1 file we selected.
$! Generate a series of DCL commands that will process the file and write them
$! to the batch runstream.  At this time the operations performed are:
$!
$! Run the program STICS_MMQ_TABLE to extract and write out WIND STICS PHA information
$! Run the program STICS_MQET_MATRIX to extract and write out WIND STICS matrix information
$! Run the program WSTICS_RATES to extract and write out WIND STICS rate information
$! Run IDL to generate a PNG plot
$!
$! If we are AUTO mode check if corresponding STICS PNG file already exists.  If
$! it does skip processing this L1 file.
$!
$! Also turn off verification to reduce size of log file unless DEBUG option
$! was given
$!
$ if .not. debug then v = f$verify(0)
$loop:
$ if read_list then read/error=end/end=end list l1file
$ if l1file .eqs. "" then goto done
$ l1name := 'f$parse(l1file,,,"NAME") 
$ pngfile := STICS_'f$extract(10,8,l1name)'.PNG
$!
$! Check for existence of the STICS PNG file if we are in AUTO mode
$! If it is there we don't need to reprocess this L1 file
$!
$ if p2 .eqs. "AUTO" .and. -
     f$search(wwwdir+pngfile) .nes. "" then goto loop
$ write sys$output "Output filename ",pngfile
$ write bat "$!"
$ write bat "$!                  PHA"
$ write bat "$!"
$ write bat "$ RUN SMS1:[WIND.SOFTWARE.STICS]STICS_MMQ_TABLE"
$ write bat "DATA_DIR:", l1name
$ write bat "y"
$ write bat "1"
$ write bat "y"
$ write bat "2"
$ write bat "-1"
$ write bat "0"
$ write bat "n"
$ write bat "-1"
$ write bat "n"
$ write bat "PHA_TABLE.OUT"
$ write bat "-1/"
$ write bat "$!"
$ write bat "$!                    MATRIX"
$ write bat "$!"
$ write bat "$ RUN SMS1:[WIND.SOFTWARE.STICS]STICS_MQET_MATRIX"
$ write bat "DATA_DIR:", l1name
$ write bat "y"
$ write bat "n"
$ write bat "n"
$ write bat "n"
$ write bat "MATRIX.OUT"
$ write bat "-1/"
$ write bat "$!"
$ write bat "$!                 WSTICS_RATES   DEFAULT RATE LIST"
$ write bat "$!"
$ write bat "$ RUN SMS1:[WIND.SOFTWARE.STICS]WSTICS_RATES"
$ write bat "DATA_DIR:", l1name
$ write bat "y"
$ write bat "-1/"
$ write bat "1"    		!without efficiencies
$ write bat "2"			!2 Sci Rec averaging interval
$ write bat "SYS$DISK:[]WSTXRATE.OUT"	!output filename
$ write bat "n"			!default rate list
$!
$! Now invoke IDL to generate a graph
$! For not skip it, since IDL code has problems
$!
$ write bat "$!"
$ write bat "$!                    IDL"
$ write bat "$!"
$ write bat "$ IDL"
$ write bat " "
$ write bat ".RUN  SMS1:[WIND.SOFTWARE.STICS]AUTO_STICS_SCATTER"
$ write bat "MATRIX.OUT"
$ write bat "PHA_TABLE.OUT"
$ write bat "1" 			!create PNG file
$ write bat "y"
$ write bat ".RUN SMS1:[WIND.SOFTWARE.STICS]DAILYRATEPLT"
$ write bat "WSTXRATE.OUT"
$ write bat "1" 			!create PNG file and put in SMS1:[WIND.WWW.STICS]
$ write bat "exit"
$!
$! Write out L1 filename to message file and increment counter.
$! If we are processing a single file only exit loop, otherwise go back to
$! start of loop and get next filename to process.
$!
$ write msg l1name," -> ",pngfile
$ nfiles = nfiles + 1
$ if .not. read_list then goto done
$ goto loop
$!
$! All done creating batch run stream.
$! Close it and submit it as a batch job.
$! Close temporary message file and send email to interested parties.
$!
$done:
$ if .not. debug then v1 = f$verify(v)
$ if read_list then close list
$!
$! We are creating a command procedure, STICS_GEN.COM, to be run either directly
$! by us or as a batch job.
$!
$ if nfiles .gt. 0
$ then
$   write bat "$ exit 1"
$   close bat
$   if direct
$   then
$     write msg ""
$     write msg "Processing started at ",f$time()
$     if .not. test then @'batchfile'
$     write msg "Processing finished at ",f$time()
$     close msg
$     MAIL/NOSELF/SUBJECT="Finished generating ''nfiles' STICS plot file(s)" 'msgfile' 'maillist'
$   else
$     if .not. test then SUBMIT /NOTIFY 'batchfile'
$     write msg ""
$     write msg "Submitted job ''$entry' to generate STICS plot file(s)"
$     close msg
$     MAIL/NOSELF/SUBJECT="Generating ''nfiles' STICS plot file(s)" 'msgfile' 'maillist'
$   endif
$ endif
$!
$! Delete temporary file.
$!
$no_files:
$ if read_list
$ then
$   DELETE 'dirlist';*,'msgfile';*
$ else
$   DELETE 'msgfile';*
$ endif
$ exit 1
$
$no_filename:
$ write sys$output "No such file: ",p2
$ exit 1
$
$closeup3:
$ close msg
$closeup2:
$ close bat
$closeup1:
$ close list
$ exit 1
