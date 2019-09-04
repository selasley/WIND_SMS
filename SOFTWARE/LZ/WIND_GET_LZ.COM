$!
$! Procedure: WIND_GET_LZ
$!
$! Purpose:
$!   Retrieve, using anonymous ftp, one or more WIND Level Zero (LZ) files
$!   from a remote site and place them into the correct directory on the local
$!   system.
$!
$! Paremeters:
$!    P1 - options.  This can be one of the keywords listed below, a date,
$!         or a filename.  If omitted NEW is assummed.
$!             REGULAR   Submit this job to run at 5:00 the next day.
$!             ALL       Retrieve all LZ files from the remote system
$!             NEW       Retrieve only those LZ files that do not already exist
$!                       on the local system.
$!             TEST      Do all operations except actually copying the data sets.
$!             date      Retrive only the LZ file for the particular date.  The
$!                       date must be in the format yyyymmdd (8 digits).  You
$!                       must also specify P2 to uniquely determine the data
$!                       set to be retrieved.
$!             filename  Retrive only the LZ file for the particular date and
$!                       data type.  The filename must be of the format
$!                       WI_LZ_SMS_yyyymmdd_Vnn.  Since the filename contains
$!                       all the required information, parameter P2 is not
$!                       needed and is ignored.
$!    P2 - Data type selection.  This can be one of the values listed below.
$!         If omitted files of all data types will be retrieved.  If an
$!         explicit filename is given in P1, P2 is ignored.
$!             SMS      SWICS/MASS/SWIMS data sets
$!             OR       CDF files for pre orbit data
$!             AT       CDF files for pre attitude data
$!    P3 - Destination directory.  If omitted WIND_LZ_DATA: is used for SMS
$!         LZ data sets, WIND_OR_DATA: for OR data sets, and WIND_AT_DATA: for
$!         AT data sets.
$!
$! Design:
$!    The Level Zero data sets on the remote Unix system are organized by year. 
$!    They are located under the path /pub/uncompressed/wi/sms/lz/yyyy/, where
$!    yyyy is the year.  The filenames, which are case sensitive, have the form
$!    wi_lz_sms_yyyymmdd_vnn.dat .  There are no sfdu files.
$!
$!    Since the LZ files are copied using binary ftp, their record lengths will
$!    be 512 bytes.  The original LZ files have record lengths of 10800 bytes,
$!    so this attribute will have to be changed before the file can be
$!    processed locally.
$!
$!    The WIND orbit and attitude files (or and at) are kept in 
$!    /pub/uncompressed/wi/pre/or/yyyy/  and  /pub/uncompressed/wi/pre/at/yyyy/,
$!    respectively.  These are in CDF format, which has 512 byte records, so no
$!    changes to these files' attributes are needed for programs to use them.
$!
$!    Assume the user wants all files (Level Zero, orbit, and attitude) unless
$!    specified otherwise using parameter P2 value of SMS, OR, or AT,
$!    respectively. 
$!
$!    Since only six months' of LZ files are kept on the local system, we
$!    need to check if there is already an L1 file for a given date.  If there
$!    if, we don't need to retrieve the LZ data set, since most likely we
$!    already processed it and deleted it.  If the L1 file does not exist,
$!    check if the LZ file exists locally.
$!
$!    Pay close attention to the data version (_vnn) part of the name in case
$!    any revisions are made to the data sets.  On the local system the L1
$!    file's data set number (_Vnn) is the same as its corresponding LZ data
$!    set number, so it is straightforward to test if a revised data set has
$!    been processed.
$!
$!    LZ files are kept in the uncompressed area for a limited period, after
$!    which they are deleted.  Older LZ files can be found in
$!    /pub/compressed/wi/sms/lz/yyyy/ ; these are stored in gzip format.
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
$!         (this is not an LZ file and the file does not already exist) or
$!         (this is an LZ file and neither the LZ or L1 files already exist) then
$!          retrieve file, place into proper directory, give it correct name
$!          if this is an LZ file then change its record length
$!        endif
$!      endloop
$!    endloop
$!    
$ remote_system  =  "pwgdata.gsfc.nasa.gov"
$ valid_types  := SMS,OR,AT
$ remote_path_lz = "/pub/uncompressed/wi/sms/lz/"
$ remote_path_or = "/pub/uncompressed/wi/pre/or/"
$ remote_path_at = "/pub/uncompressed/wi/pre/at/"
$ tempfile := SYS$LOGIN:WIND_GET_LZ.TMP
$ msgfile := SYS$LOGIN:WIND_GET_LZ.MSG
$ maillist := UMTOF2::PAQUETTE
$ l1_dir := WIND_L1_DATA:
$ runtime := 5:0
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
$! If he gave the full filename, extract date and data type.
$! Set up parameters so inner loop executes exactly once.
$!
$ year := 'f$cvtime("",,"YEAR")'
$ if f$length(p1) .eq. 8 .and. f$type(p1) .eqs. "INTEGER"
$ then
$   date_to_get := 'p1'
$   year := 'f$extract(0,4,date_to_get)'
$   one_file = 1
$ endif
$ if f$length(p1) .eq. 23 .and. f$extract(0,3,p1) .eqs. "WI_"
$ then
$   p2 := 'f$extract(6,4,p1)'
$   date_to_get := 'f$extract(11,8,p1)'
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
$ target_dir = p3
$ if target_dir .eqs. ""
$ then
$   if dtype .eqs. "SMS" then target_dir := WIND_LZ_DATA:
$   if dtype .eqs. "OR" then target_dir := WIND_OR_DATA:
$   if dtype .eqs. "AT" then target_dir := WIND_AT_DATA:
$ endif
$!
$! Depending on the data type, select the proper remote path on the Unix system
$!
$ if dtype .eqs. "SMS" then remote_path = remote_path_lz
$ if dtype .eqs. "OR" then remote_path = remote_path_or
$ if dtype .eqs. "AT" then remote_path = remote_path_at
$ remote_path = remote_path + year
$!
$! Get list of files available for this data type and year.  Sample line:
$! /pub/uncompressed/wi/sms/lz/2002/wi_lz_sms_20020228_v01.dat
$!
$ DEFINE/USER SYS$OUTPUT 'tempfile'
$ DIRECTORY/FTP/ANONYMOUS 'remote_system'::"''remote_path'"
$!
$! Go through list we just retrieved.  For LZ files, for each filename check
$! which of the following cases applies:
$! 1) no LZ file or L1 file with that type and date      transfer it
$!    exists on the local system
$! 2) a LZ or L1 file with that type, date, and data     don't transfer it
$!    version exists on the local system
$! 3) a LZ or L1 file with that type and date but        don't transfer it
$!    with a higher data version exists on local the system
$! 4) a LZ or L1 file with that type and date but        transfer it
$!    with a lower data version exists on local the system
$! 5) the ALL keyword was given                          always transfer file,
$!                                                       don't bother checking
$!
$! If this request is for an orbit (OR) or attitude (AT) file, for each
$! filename check which of the following cases applies:
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
$ open/read list 'tempfile'
$loop2:
$ read/end=endloop2/err=endloop2 list line
$ line2 = f$edit(line,"COMPRESS,TRIM")
$ if f$extract(0,1,line2) .nes. "/" then goto loop2
$!
$! Massage filename into format used on local system
$!
$ pos = f$locate("/wi_",line2)
$ remote_file = f$extract(pos+1,99,line2)
$ filename := 'remote_file'
$ file_exists = "''f$search(target_dir+filename)'" .nes. ""
$
$ copy_it = do_all
$ if .not. do_all
$ then
$   if dtype .eqs. "SMS"
$   then 
$     l1_filename := WI_L1_SMS_'f$extract(10,12,filename)'.DAT
$     l1_exists = "''f$search(l1_dir+l1_filename)'" .nes. ""
$     if .not. file_exists .and. .not. l1_exists then copy_it = 1
$   endif
$   if dtype .eqs. "OR" .or. dtype .eqs. "AT"
$   then 
$     if .not. file_exists then copy_it = 1
$   endif
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
$ if dtype .eqs. "SMS" then SET FILE/ATTR=LRL:10800/LOG 'target_dir''filename'
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
$! Come here when we're through with all data types
$!
$endloop1:
$ close msg
$ if count .ne. 0
$ then
$ plural = ""
$ if count .gt. 0 then plural = "s"
$ MAIL/NOSELF/NOSIG/SUBJECT="Copied ''count' WIND file''plural'" -
 'msgfile' 'maillist'
$ endif
$! DELETE 'tempfile';*,'msgfile';*
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
$ target_dir = p3
$ if target_dir .eqs. ""
$ then
$   if dtype .eqs. "SMS" then target_dir := WIND_LZ_DATA:
$   if dtype .eqs. "OR" then target_dir := WIND_OR_DATA:
$   if dtype .eqs. "AT" then target_dir := WIND_AT_DATA:
$ endif
$!
$! Depending on the data type, select the proper remote path on the Unix system
$!
$ if dtype .eqs. "SMS" then remote_path = remote_path_lz
$ if dtype .eqs. "OR" then remote_path = remote_path_or
$ if dtype .eqs. "AT" then remote_path = remote_path_at
$
$ filename = p2
$ if f$parse(filename,,,"TYPE") .eqs. "."
$ then
$   if dtype .eqs. "SMS" then filename = filename + ".DAT"
$   if dtype .eqs. "OR" .or. dtype .eqs. "AT" then filename = filename + ".CDF"
$ endif
$ remote_file = f$edit(p2,"LOWERCASE")
$ write sys$output "Copying ''remote_path'/''remote_file' to"
$ write sys$output "  ''target_dir'''filename'"
$ if .not. test
$ then
$ COPY/FTP/ANONYMOUS/BINARY/LOG 'remote_system'::"''remote_path'/''remote_file" 'target_dir''filename'
$ if dtype .eqs. "SMS" then SET FILE/ATTR=LRL:10800/LOG 'target_dir''filename'
$ endif
$ exit 1
