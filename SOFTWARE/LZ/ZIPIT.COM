$ delete [sheldon.ftp]wind_cd_read.zip;*
$ zip [sheldon.ftp]wind_cd_read.zip *.for wind_cd_read.mms -x test*.for date.for julday.for readcdf.for truncate.for writebitrate.for
