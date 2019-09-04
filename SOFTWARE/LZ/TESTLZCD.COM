$ set proc/priv=volpro
$ mount/over=id cd1
$ def/job wind_l1_data smsdisk2:[wind.temp]
$ @[wind.software.lz]wind_cd_read_batch auto cd1:[data.wi.sms.lz] useorb
