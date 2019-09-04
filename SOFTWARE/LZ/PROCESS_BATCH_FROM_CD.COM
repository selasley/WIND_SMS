$ if p1 .eqs. "" then exit 1
$ SET PROC/PRIV=VOLPRO
$ DEFINE WIND_HALT_NO_EPH TRUE
$ DEFINE WIND_COMMENT "Reprocessed to add orbit, attitude info; fix 12-hr time problem"
$ MOUNT/OVER=ID 'p1'
$ @[WIND.SOFTWARE.LZ]WIND_CD_READ_BATCH REPLACE 'p1':[DATA.WI.SMS.LZ] USEORB
