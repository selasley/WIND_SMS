$ DEFINE WIND_HALT_NO_EPH TRUE
$ DEFINE WIND_COMMENT "Reprocessed to add orbit, attitude info; fix 12-hr time problem"
$ @[WIND.SOFTWARE.LZ]WIND_CD_READ_BATCH AUTO "" USEORB
