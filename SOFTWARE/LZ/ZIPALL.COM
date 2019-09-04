$ delete/log wind_cd_read.zip;*,include.zip;*,wind_lib.zip;*,xti_lib.zip;*,local_lib.zip;*
$ write sys$output "Zipping file wind_cd_read.zip"
$ zip wind_cd_read.zip *.for wind_cd_read.mms -x test*.for date.for julday.for readcdf.for truncate.for writebitrate.for
$ write sys$output "Zipping file include.zip"
$ zip include.zip [wind.software.inc]*.inc
$ write sys$output "Zipping file wind_lib.zip"
$ zip wind_lib.zip [wind.software.lib]*.for [wind.software.lib]*.com
$ write sys$output "Zipping file xti_lib.zip"
$ zip xti_lib.zip [wind.software.xti]*.for [wind.software.xti]*.inc [wind.software.xti]*.com
$ write sys$output "Zipping file local_lib.zip"
$ zip local_lib.zip [grplib.umdcommon]*.for [grplib.umdcommon.util]ch.for [grplib.umdcommon.util]vxdcl.for 
