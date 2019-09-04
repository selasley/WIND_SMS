$!
$! This procedure processes NRT LZ files in to L1 files.
$! It uses the command procedure WIND_CD_READ_BATCH in automatic mode so only
$! those LZ files which have no corresponding L1 file are processed.
$!
$! In order for WIND_CD_READ to work properly on NRT files we define the
$! logical name WIND_NRT_MODE.  This causes both WIND_CD_READ_BATCH.COM and
$! WIND_CD_READ.EXE to modify their behaviour slightly to accomodate NRT data.
$!
$ mydir = f$environment("DEFAULT")
$ on error then goto back
$ SET DEFAULT WIND_NRT_DATA:
$ DEFINE WIND_NRT_MODE TRUE
$ @SMS1:[WIND.SOFTWARE.LZ]WIND_CD_READ_BATCH AUTO
$back:
$ SET DEFAULT 'mydir'
