$!   This procedure processes one or more WIND Level Zero (LZ) files, generating
$! a Level 1 (L1) file for each LZ file.  It can be run in one of several modes 
$! as determined by parameter P1 (see below).
$!
$! Parameters:
$!
$! P1 - Operation type, can be "ONCE", "AUTO", "FORCE", or "REPLACE".  See
$!	table below for additional details. 
$! P2 - Filename or pattern, depending on operation mode
$!      ONCE      exact name of LZ file
$!      AUTO      name of directory containing LZ files, no filename part
$!      FORCE     name of directory containing LZ files, no filename part
$!      REPLACE   name of directory containing LZ files, no filename part
$! P3 - Options flag.  The allowable options are given in the table below.
$!	REGULAR	Submit WIND_CD_RUN_BATCH to run again the following day at
$!		the time given in the symbol START_TIME.
$!	DEBUG	Execute the program from SMS1:[WIND.SOFTWARE.LZ], otherwise
$!		execute the program from WIND_EXE:, which is currently defined
$!		as SMS1:[WIND.SOFTWARE.EXE]. 
$!	USEORB	Do not process an LZ file unless its corresponding orbit and
$!		attitude files are also available and can be processed.
$!	TEST	Do not generate a new L1 file.  Used for debugging purposes
$!		only. If more than one option is given separate them with
$!		commas. 
$!
$! Operation modes:
$!
$! Mode    Description
$! ONCE    Process a single LZ file, whose name must be specified, and generate
$!         a L1 file for it.
$! AUTO    Scan a directory containing LZ files and process only those which
$!         have no corresponding L1 file.
$! FORCE   Scan a directory containing LZ files and process each file, even if
$!         the corresponding L1 file already exists.
$! REPLACE Same as FORCE, but first delete each data set's L1 file if it exists.
$!
$ testmode := N
$ force := N
$ delete_first := N
$ start_time := 6:00
$ err_file := SYS$LOGIN:WIND_CD_READ.ERR
$ msg_file := SYS$LOGIN:WIND_CD_READ.TMP
$ summ_file := SYS$LOGIN:WIND_CD_READ.SUMM
$ mail_list := PAQUETTE
$ user := 'f$getjpi("","USERNAME")
$ exe_dir := WIND_EXE:
$ nrt_mode = f$trnlnm("WIND_NRT_MODE") .nes. ""
$ if nrt_mode then mail_list := PAQUETTE
$
$ debug = f$locate("DEBUG",p3) .ne. f$length(p3)
$ use_orbit = f$locate("USEORB",p3) .ne. f$length(p3)
$ testmode = f$locate("TEST",p3) .ne. f$length(p3)
$ regular = f$locate("REGULAR",p3) .ne. f$length(p3)
$!
$! If REGULAR option given submit job to run again tomorrow
$!
$ if regular
$ then
$ SUBMIT /AFTER="TOMORROW+''start_time'" 'f$element(0,";",f$environment("PROCEDURE"))' -
    /PARA=("''p1'","''p2'","''p3'")
$ endif
$
$ if debug then exe_dir := SMS1:[WIND.SOFTWARE.LZ]
$
$ if f$search(summ_file) .nes. "" then DELETE 'summ_file';*
$
$ if p1 .eqs. "FORCE" .or. p1 .eqs. "REPLACE" then force := Y
$ if p1 .eqs. "REPLACE" then delete_first := Y
$ if p1 .eqs. "ONCE" then goto one_shot
$ if p1 .eqs. "AUTO" .or. p1 .eqs. "FORCE" .or. p1 .eqs. "REPLACE" then goto auto
$ write sys$output "P1 must be ONCE, AUTO, FORCE, or REPLACE"
$ exit 1
$!
$! Come here to process a single LZ file
$!
$one_shot:
$ from_cd := N
$ lz_input_file = p2
$ if lz_input_file .eqs. "" then lz_input_file := LZF:
$ if f$extract(0,3,f$getdvi(lz_input_file,"MEDIA_NAME")) .eqs. "RRD" then from_cd := Y
$ show symbol lz_input_file
$ dir/siz/date 'lz_input_file'
$ if f$search(lz_input_file) .eqs. "" then exit 1
$
$ gosub process_lz_file
$ if $status .eq. 3 then gosub send_mail
$ exit 1
$!
$! Come here to automatically process multiple LZ files
$!
$auto:
$ from_cd := N
$ lz_dir = p2
$ if p2 .eqs. "" then lz_dir := WIND_LZ_DATA:
$ if f$extract(0,3,f$getdvi(lz_dir,"MEDIA_NAME")) .eqs. "RRD" then from_cd := Y
$ pattern := 'lz_dir'WI_LZ_SMS_*.DAT
$ if from_cd then pattern := 'lz_dir'*.DAT
$ if nrt_mode
$ then
$   if p2 .eqs. "" then lz_dir := WIND_NRT_DATA:
$   pattern := 'lz_dir'WI_LZ_SMS_*.*
$ endif
$ num_done = 0
$ CREATE 'summ_file'
  WIND Level Zero File Processing

   Date / Time       LZ Filename
$
$loop:
$!
$! get next LZ filename
$!
$ lz_input_file := 'f$search(pattern,2)
$ if lz_input_file .eqs. "" then goto done
$!
$! construct L1 filename
$!
$ pos = f$locate("]",lz_input_file)
$ if pos .eq. f$length(lz_input_file) then pos = -1
$ if .not. from_cd
$ then
$ l1_file = lz_input_file
$ l1_file := 'f$extract(0,f$locate(";",l1_file),l1_file)
$ l1_file[pos+4,2] := L1
$ if nrt_mode
$ then
$   l1_file = "WIND_NRT_DATA:" + f$extract(pos+1,999,l1_file)
$ else
$   l1_file = "WIND_L1_DATA:" + f$extract(pos+1,999,l1_file)
$ endif
$ else
$ year = f$extract(pos+1,2,lz_input_file)
$ cent := 20
$ if f$integer(year) .gt. 90 then cent := 19
$ l1_file = "WIND_L1_DATA:" + "WI_L1_SMS_" + -
	  cent + year + -
	  f$extract(pos+3,4,lz_input_file) + -
	  "_V" + f$extract(pos+7,2,lz_input_file) + ".DAT"
$ endif
$ if testmode then write sys$output "Found an LZ file:           ",lz_input_file
$ if testmode then write sys$output "Checking if L1 file exists: ",l1_file
$!
$! check if L1 file exists
$!
$ sho sym l1_file
$ fullname := 'f$search(l1_file)
$!
$! if L1 file doesn't exist or we're being forced to reprocess the data set
$! (mode is FORCE or REPLACE) process the LZ file
$!
$ if fullname .nes. "" .and. .not. force then goto loop
$!
$! The L1 file does not exist, or we're in FORCE or REPLACE mode.
$! If the USEORB option was given check if the orbit and attitude files also
$! exist.  If they do not, then skip this LZ file.
$!
$ if .not. use_orbit then goto skip_orbit
$ if .not. from_cd
$ then
$ orbit_name = f$parse(".CDF","WIND_OR_DATA:",lz_input_file)
$ i = f$locate("]",orbit_name)
$ orbit_name[i+4,6] := OR_PRE
$ pos = f$locate("]",orbit_name)
$ orbit_name[pos+21,2] := %%
$ else
$ orbit_name = f$parse(".CDF","[DATA.WI.DEF.OR]",lz_input_file)
$ pos = f$locate("]",orbit_name)
$ orbit_name[pos+7,2] := %%
$ endif
$ if testmode then write sys$output "Checking for orbit file:    ",orbit_name
$ if f$search(orbit_name) .eqs. "" then goto loop
$
$ if .not. from_cd
$ then
$ attitude_name = f$parse(".CDF","WIND_AT_DATA:",lz_input_file)
$ i = f$locate("]",attitude_name)
$ attitude_name[i+4,6] := AT_PRE
$ pos = f$locate("]",attitude_name)
$ attitude_name[pos+21,2] := %%
$ else
$ attitude_name = f$parse(".CDF","[DATA.WI.DEF.AT]",lz_input_file)
$ pos = f$locate("]",attitude_name)
$ attitude_name[pos+7,2] := %%
$ endif
$ if testmode then write sys$output "Checking for attitude file: ",attitude_name
$ if f$search(attitude_name) .eqs. "" then goto loop
$!
$! All necessary files are present.  Process the LZ file.
$!
$skip_orbit:
$ date = f$cvtime(,"ABSOLUTE","DATE")
$ if f$extract(1,1,date) .eqs. "-" then date = " " + date
$ time = f$extract(0,5,f$cvtime(,"ABSOLUTE","TIME"))
$ short_name = f$parse(lz_input_file,,,"NAME")
$ ftype = f$parse(lz_input_file,,,"TYPE")
$ if ftype .nes. ".DAT" .and. ftype .nes. "." then short_name = short_name + ftype
$ output_line = date + " " + time + "    " + short_name
$!
$! if L1 file exists and we're in mode REPLACE delete the L1 file first
$!
$ if fullname .nes. "" .and. delete_first
$ then
$ if testmode then write sys$output "Will delete file ''l1_file';*"
$ if .not. testmode then DELETE/LOG 'l1_file';*
$ endif
$!
$! call a DCL subroutine to process this LZ file
$!
$ gosub process_lz_file
$!
$ status = $status
$ open/append sum 'summ_file'
$ write sum output_line
$ close sum
$ if status .eq. 3 then gosub send_mail
$ num_done = num_done + 1
$ goto loop
$
$done:
$ write sys$output "Processed ''num_done' LZ files"
$ if num_done .gt. 0
$ then
$ MAIL/NOSELF/SUBJ="Processed ''num_done' LZ files" 'summ_file' 'user'
$ else
$ MAIL/NOSELF/SUBJ="Processed ''num_done' LZ files" SYS$INPUT 'user'
$ endif
$ exit 1
$
$process_lz_file:
$ if f$search(err_file) .nes. "" then DELETE 'err_file';*
$ if f$search(msg_file) .nes. "" then DELETE 'msg_file';*
$ set ver=image
$ write sys$output "About to process file ",lz_input_file
$ out_file := 'f$parse(f$trnlnm("SYS$LOGIN"),".LIS;",lz_input_file)
$ out_file = out_file - ";"
$ if testmode then return 1
$ if f$search(out_file) .nes. "" then DELETE 'out_file';*
$ on error then goto run_error
$ DEFINE /USER LZ_INPUT 'lz_input_file'
$ DEFINE /USER SYS$ERROR 'err_file'
$ DEFINE /USER SYS$OUTPUT 'out_file'
$ RUN /NODEBUG 'exe_dir'WIND_CD_READ

LZ_INPUT:




-9
$ set ver=noimage
$
$ APPEND 'out_file';2,'out_file';1 'out_file';3/NEW_FILE
$ DELETE 'out_file';2,'out_file';1
$ return 1
$run_error:
$ output_line = output_line + "  error; check log file"
$ return 3
$
$send_mail:
$ if f$search(msg_file) .nes. "" then DELETE 'msg_file';*
$ APPEND 'err_file','out_file';2,'out_file';1 'msg_file'/NEW_FILE
$ MAIL/NOSELF/SUBJ="WIND_CD_READ error!" 'msg_file' 'mail_list'
$ return 1
