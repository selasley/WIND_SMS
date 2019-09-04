$!
$! This procedure searches, using a user-specified pattern, for data files
$! which have multiple data versions.  If multiple copies are found all are
$! deleted except for the latest (highest numbered) file.
$!
$! Filenames usually have the format WI_LZ_SMS_yyyymmdd_Vnn.DAT , where
$! yyyymmdd specifies the date of the data set.  Combined with nn, the data
$! version, this uniquely determines the filename.  
$!
$! NOTE: This is a data set version, NOT a VMS file version.
$!
$! The procedure deletes all data sets of a given date except for the highest
$! numbered data set.
$!
$! P1 - Pattern to use to search for files to be deleted
$!
$ pattern := 'p1'
$ if pattern .eqs. "" then pattern := WIND_LZ_DATA:WI_LZ_SMS_*.DAT;
$ if pattern .eqs. "LZ" then pattern := WIND_LZ_DATA:WI_LZ_SMS_*.DAT;
$ if pattern .eqs. "L1" then pattern := WIND_L1_DATA:WI_L1_SMS_*.DAT;
$ lzfile :=
$ datepart :=
$ doit := Y
$ if p2 .eqs. "TEST" then doit := N
$ if p2 .eqs. "REGULAR" then SUBMIT /AFTER="TOMORROW+5:30" -
    WIND_EXE:DELETE_OLD_DATA_VERSIONS.COM  /PARA=("",REGULAR)
$
$loop:
$!
$! Search for next filename
$!
$ old_lzfile := 'lzfile'
$ lzfile := 'f$search(pattern)
$ if lzfile .eqs. "" then goto done
$ old_datepart := 'datepart'
$ filepart := 'f$parse(lzfile,,,"NAME")
$ i = f$length(filepart)
$ datepart := 'f$extract(i-12,8,filepart)
$!
$! If the date extracted from the current filename is the same as the date
$! extracted from the previous filename, there are multiple data sets for this
$! date.  Delete the earlier data set.
$!
$ if old_datepart .eqs. datepart
$ then
$ if p1 .eqs. "L1" then reportname := 'f$parse(".REPORT;*",old_lzfile)
$ if doit
$ then
$ DELETE/LOG 'old_lzfile'
$ if p1 .eqs. "L1" then DELETE/LOG 'reportname'
$ else
$ write sys$output "DELETE ''old_lzfile'"
$ if p1 .eqs. "L1" then write sys$output "DELETE ''reportname'"
$ endif
$ endif
$ goto loop
$
$done:
$ exit 1
