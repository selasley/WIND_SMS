$!
$! P1 - Year and month to archive (6 digits in yyyymm format) or
$!      the keyword INIT
$! P2 - Optical device to archive files to or to initialize
$! P3 - Label of optical disk to archive to or to initialize.  If empty assumes
$!      device given in P2 is already mounted and can be written to.
$!
$ if p1 .eqs. "" .or. p2 .eqs. ""
$ then
$ write sys$output "Usage:"
$ write sys$output "    @L1_ARCHIVE  yearmonth  device  label"
$ write sys$output ""
$ write sys$output " P1 - Year and month to archive (6 digits in yyyymm format)"
$ write sys$output " P2 - Optical device to archive files to"
$ write sys$output " P3 - Label of optical disk to archive to.  If empty assumes device given in"
$ write sys$output "      P2 is already mounted and can be written to."
$ write sys$output ""
$ exit 1
$ endif
$
$ archive_disk = (p2 - ":") + ":"
$ archive_label = p3
$ ss archive_disk
$
$ archive_dst := 'archive_disk'[ARCHIVE]
$ if p1 .eqs. "INIT" then goto init_disk
$
$ year = f$extract(0,4,p1)
$ month = f$extract(4,2,p1)
$
$ pattern := WIND_L1_DATA:WI_L1_SMS_'year''month'%%_V%%.*
$
$ on warning then goto cantdoit1
$ if archive_label .nes. "" then MOUNT/SHARE 'archive_disk' 'archive_label'
$ blocks_avail = f$getdvi(archive_disk,"FREEBLOCKS")
$
$ on warning then goto cantdoit2
$ blocks_needed = 0
$loop:
$ fname = f$search(pattern)
$ if fname .eqs. "" then goto doneloop
$ blocks_needed = blocks_needed + f$file(fname,"ALQ")
$ goto loop
$doneloop:
$ sho sym blocks_needed
$ sho sym blocks_avail 
$ if blocks_avail .lt. blocks_needed then goto not_enough
$
$ COPY/LOG 'pattern' 'archive_dst'/PROT=W:RE
$ blocks_avail = f$getdvi(archive_disk,"FREEBLOCKS")
$ write sys$output "Archiving of L1 files for ''p1' complete."
$ write sys$output "There are ''blocks_avail' left on ''archive_disk'"
$ goto alldone
$
$not_enough:
$ write sys$output "There is not sufficient room on this device for the L1 files"
$ write sys$output "of the specified month.  The operation was not started."
$
$alldone:
$ if archive_label .nes. "" then DISMOUNT 'archive_disk'
$ exit 1
$cantdoit1:
$ write sys$output "Cannot perform archive operation; check the archive device"
$ exit 1
$cantdoit2:
$ write sys$output "Cannot perform archive operation; one or more L1 files cannot be read."
$ exit 1
$
$init_disk:
$ INIT/OWNER=WIND/PROT=(S:RWED,O:RWED,G:RE,W:RE) 'archive_disk' 'archive_label'
$ MOUNT 'archive_disk' 'archive_label'
$ CREATE/DIRECTORY/OWNER=WIND/PROT=(S:RWED,O:RWED,G:RE,W:RE) 'archive_dst'
$ DISMOUNT 'archive_disk'
$ exit 1
