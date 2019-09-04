$!
$! Procedure: WIND_GET_KP
$!
$! Purpose:
$!   Retrieve, using anonymous ftp, one or more WIND Key Parameter Zero (KP)
$!   files from a remote site and place them into the correct directory on the
$!   local system.
$!
$! Paremeters:
$!    P1 - options.  This can be one of the keywords listed below, a date,
$!         or a filename.  If omitted NEW is assummed.
$!             REGULAR   Submit this job to run at 5:30 the next day.
$!             ALL       Retrieve all KP files from the remote system
$!             NEW       Retrieve only those KP files that do not already exist
$!                       on the local system.
$!             TEST      Do all operations except actually copying the data sets.
$!             date      Retrive only the KP file for the particular date.  The
$!                       date must be in the format yyyymmdd (8 digits).  You
$!                       must also specify P2 to uniquely determine the data
$!                       set to be retrieved.
$!             filename  Retrive only the KP file for the particular date and
$!                       data type.  The filename must be of the format
$!                       WI_K0_type_yyyymmdd_Vnn.  Since the filename contains
$!                       all the required information, parameter P2 is not
$!                       needed and is ignored.
$!    P2 - Data type selection.  This can be one of the values listed below.
$!         If omitted files of all data types will be retrieved.  If an
$!         explicit filename is given in P1, P2 is ignored.
$!             3DP      KP files for 
$!             EPA      KP files for 
$!             MFI      KP files for 
$!             SMS      KP files for SWICS/MASS/SWIMS data
$!             SPHA     KP files for 
$!             SWE      KP files for 
$!             WAV      KP files for 
$!    P3 - Destination directory.  If omitted WIND_KP_type: is used, where
$!         'type' is one of the valid types listed above.  For example,
$!         if SWE KP data sets are retrieved, they will be stored into
$!         WIND_KP_SWE: by default.  If P2 is omitted so that multiple data
$!         types are selected, they will each be placed into the appropriate
$!         directory.
$!
$! Design:
$!    The key parameter CDF files on the remote Unix system are organized by
$!    year.  They are located under the path /pub/uncompressed/wi/swe/k0/yyyy/,
$!    where yyyy is the year, and swe is given only as an example.  The
$!    filenames, which are case sensitive, have the form
$!    wi_k0_swe_yyyymmdd_vnn.cdf .  There are no sfdu files. 
$!
$!    Since the KP files are copied using binary ftp, their record lengths will
$!    be 512 bytes.  This does not matter, however, since they are in CDF
$!    format, and CDF files have record lengths of 512.
$!
$!    Assume the user wants all types of key parameter files unless
$!    specified otherwise using parameter P2.
$!
$!    Pay close attention to the data version (_vnn) part of the name in case
$!    any revisions are made to the data sets.  
$!
$!    KP files are kept in the uncompressed area for a limited period, after
$!    which they are deleted.  Older KP files can be found in
$!    /pub/compressed/wi/swe/k0/yyyy/ ; these are stored in gzip format.
$!
$! Detail Design:
$!    validate parameters
$!    if date given in P1 then
$!      construct pathname, filename
$!      set up parameters so inner loop executes exactly once
$!    endif
$!    if explicit filename given in P1 then
$!      construct pathname, filename
$!      extract data type from filename, save as user specified data type
$!      set up parameters so inner loop executes exactly once
$!    endif
$!    if data type given or explicit filename given then
$!      set up outer loop to execute only once for this data type
$!    else
$!      set up outer loop to execute for each data type
$!    endif
$!    loop over data types:
$!      set up path and pattern for this data type
$!      generate list of filenames on remote system
$!      loop:
$!        get next filename from list
$!        construct name file should have on local system
$!        if ALL or an explicit filename was given or
$!         the file does not already exist then
$!          retrieve file, place into proper directory, give it correct name
$!        endif
$!      endloop
$!    endloop
$!    
$ remote_system  =  "pwgdata.gsfc.nasa.gov"
$ valid_types  := 3DP,EPA,MFI,SMS,SPHA,SWE,WAV
$ remote_path_3dp = "/pub/uncompressed/wi/3dp/k0"
$ remote_path_epa = "/pub/uncompressed/wi/epa/k0"
$ remote_path_mfi = "/pub/uncompressed/wi/mfi/k0"
$ remote_path_sms = "/pub/uncompressed/wi/sms/k0"
$ remote_path_spha = "/pub/uncompressed/wi/spha/k0"
$ remote_path_swe = "/pub/uncompressed/wi/swe/k0"
$ remote_path_wav = "/pub/uncompressed/wi/wav/k0"
$ tempfile := SYS$LOGIN:WIND_GET_KP.TMP
$ msgfile := SYS$LOGIN:WIND_GET_KP.MSG
$ maillist := UMTOF2::PAQUETTE
$ runtime := 5:30
$!
$ p1 = f$edit(p1,"UPCASE")
$ p2 = f$edit(p2,"UPCASE")
$ regular = f$locate("REGULAR",p1) .ne. f$length(p1)
$ do_all = f$locate("ALL",p1) .ne. f$length(p1)
$ get_new = f$locate("NEW",p1) .ne. f$length(p1)
$ test = f$locate("TEST",p1) .ne. f$length(p1)
$ one_file = (f$length(p1) .eq. 23 .and. f$extract(0,3,p1) .eqs. "WI_") .or. -
             (f$length(p1) .eq. 8 .and. f$type(p1) .eqs. "INTEGER")
$ if .not. do_all .and. .not. get_new .and. .not. one_file then get_new = 1
$!
$! Submit job to run again tomorrow if REGULAR keyword given
$!
$ if regular
$ then
$ SUBMIT 'f$element(0,";",f$environment("PROCEDURE"))' -
 /PARA=("''p1'","''p2'") /AFTER="TOMORROW+''runtime'" 
$ endif
$!
$! If user gave date in 8-digit format, save it and set ONE_FILE flag.
$! If he gave the full filename (wi_k0_swe_yyyymmdd_vnn.cdf) extract date and
$! data type. 
$! Set up parameters so inner loop executes exactly once.
$!
$ year := 'f$cvtime("",,"YEAR")'
$ if f$length(p1) .eq. 8 .and. f$type(p1) .eqs. "INTEGER"
$ then
$   date_to_get := 'p1'
$   year := 'f$extract(0,4,date_to_get)'
$   one_file = 1
$ endif
$ if (f$length(p1) .eq. 22 .or. f$length(p1) .eq. 23) .and. -
     f$extract(0,5,p1) .eqs. "WI_K0"
$ then
$   p2 := 'f$element(2,"_",p1)'
$   date_to_get := 'f$element(3,"_",p1)'
$   year := 'f$extract(0,4,date_to_get)'
$   one_file = 1
$ endif
$
$ if one_file then goto one_file_only
$!
$! Check which data type given.  If none, set up to use all data types.
$!
$ if p2 .eqs. ""
$ then
$   type_list = valid_types
$ else
$   type_list = p2
$ endif
$ i = -1
$!
$! Open message file
$!
$ open/write msg 'msgfile'
$ count = 0
$!
$! Loop over each data type in turn, since a different pattern will be needed
$! for each one.
$!
$loop1:
$ i = i + 1
$ dtype := 'f$element(i,",",type_list)'
$ if dtype .eqs. "," then goto endloop1
$ dtype2 = f$edit(dtype,"LOWERCASE")
$!
$! Depending on the data type, select the proper destination directory on the
$! local system, unless a destination directory was already specified in p3
$!
$ if p3 .eqs. ""
$ then
$   target_dir := WIND_KP_'dtype':
$ else
$   target_dir = p3
$ endif
$!
$! Depending on the data type, select the proper remote path on the Unix system
$!
$ remote_path = remote_path_'dtype2'
$ remote_path = remote_path + "/" + year
$!
$! Get list of files available for this data type and year.  Sample line:
$! /pub/uncompressed/wi/swe/k0/2002/wi_k0_swe_20020228_v01.cdf
$!
$ SET NOON
$ DEFINE/USER SYS$OUTPUT 'tempfile'
$ DIRECTORY/FTP/ANONYMOUS 'remote_system'::"''remote_path'"
$ if $status .eqs. "%X18018202" then goto no_files
$ SET ON
$!
$! Go through list we just retrieved.  For each filename check which of
$! the following cases applies:
$! 1) no file with that type and date exists on the      transfer it
$!    local system
$! 2) a file with that type and date but with the        don't transfer it
$!    same or higher data version exists on local
$!    system
$! 3) a file with that type and date but with a          transfer it
$!    lower data version exists on local system
$! 4) the ALL keyword was given                          always transfer file,
$!                                                       don't bother checking
$!
$! If a lower data version exists but this data version does not, then the
$! symbol FILE_EXISTS will be set false, so we'll copy it; i.e., we can treat
$! cases 1 and 3 identically.  Don't bother checking if a higher data version
$! exists (case 2), just leave copy_it flag alone.  If ALL was specified (case
$! 4) it will be true; if not, it will be false, which is what we want anyway.
$!
$ open/read list 'tempfile'
$loop2:
$ read/end=endloop2/err=endloop2 list line
$ line2 = f$edit(line,"COMPRESS,TRIM")
$ if f$extract(0,1,line2) .nes. "/" then goto loop2
$!
$! Massage filename into format used on local system
$! (wi_k0_swe_yyyymmdd_vnn.cdf)
$!
$ pos = f$locate("/wi_",line2)
$ remote_file = f$extract(pos+1,99,line2)
$ filename := 'remote_file'
$ file_exists = "''f$search(target_dir+filename)'" .nes. ""
$!
$ copy_it = 0
$ if do_all
$ then
$   copy_it = 1
$ else
$   if .not. file_exists then copy_it = 1
$ endif
$!
$! Now copy the file to the local system if it is needed
$!
$ if copy_it
$ then
$ write sys$output "Copying ''remote_path'/''remote_file' to"
$ write sys$output "  ''target_dir'''filename'"
$ if .not. test
$ then
$ COPY/FTP/ANONYMOUS/BINARY/LOG 'remote_system'::"''remote_path'/''remote_file'" 'target_dir''filename'
$ endif
$ write msg "  ''remote_file'"
$ count = count + 1
$ endif
$ goto loop2
$!
$! Come here when we reach end of list
$!
$endloop2:
$ close list
$ write msg ""
$ goto loop1
$!
$! Come here if there are no files for the given data type
$!
$no_files:
$ write msg "There are no uncompressed ''dtype' files on the remote system"
$ write msg "Pattern: ",remote_path
$ write msg ""
$ goto loop1
$!
$! Come here when we're through with all data types
$!
$endloop1:
$ close msg
$ if count .ne. 0
$ then
$ plural = ""
$ if count .gt. 0 then plural = "s"
$ MAIL/NOSELF/NOSIG/SUBJECT="Copied ''count' WIND KP file''plural'" -
 'msgfile' 'maillist'
$ endif
$ DELETE 'tempfile';*,'msgfile';*
$ exit 1
$!
$! Come here to retrieve one file only
$! We still need to determine the proper target directory if P3 is omitted and
$! the remote path to use to retrieve the file.
$!
$one_file_only:
$ dtype = f$element(2,"_",p1)
$!
$! Depending on the data type, select the proper destination directory on the
$! local system, unless a destination directory was already specified in p3
$!
$ if p3 .nes. ""
$ then
$   target_dir := WIND_KP_'dtype':
$ else
$   target_dir = p3
$ endif
$!
$! Depending on the data type, select the proper remote path on the Unix system
$!
$ remote_path = remote_path_'dtype2'
$ remote_path = remote_path + "/" + year
$
$ filename = p2
$ if f$parse(filename,,,"TYPE") .eqs. "."
$ then
$   filename = filename + ".CDF"
$ endif
$ remote_file = f$edit(filename,"LOWERCASE")
$ write sys$output "Copying ''remote_path'/''remote_file' to"
$ write sys$output "  ''target_dir'''filename'"
$ if .not. test
$ then
$ COPY/FTP/ANONYMOUS/BINARY/LOG 'remote_system'::"''remote_path'/''remote_file" 'target_dir''filename'
$ endif
$ exit 1
