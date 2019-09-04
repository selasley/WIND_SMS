$ if p1 .eqs. "" then exit
$ pattern := WIND_'p1'_DATA:%%%%%%%%.CDF;*
$loop:
$ fname1 = f$search(pattern)
$ if fname1 .eqs. "" then exit
$ namepart = f$parse(fname1,,,"NAME")
$ newname = "WI_''p1'_DEF_19" + f$extract(0,6,namepart) + "_V" + f$extract(6,2,namepart) 
$ fname2 = f$parse(newname,fname1)
$ write sys$output "Renaming ''namepart' to ''newname'"
$! write sys$output "Renaming ''fname1' to ''fname2'"
$ RENAME 'fname1' 'fname2'
$ goto loop
