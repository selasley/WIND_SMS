$define sys$batch umsms_single_batch       !use single job batch queue as the
$                                          !default batch queue
$delete dir.lis.*
$purge idl.ps
$ydt = f$cvtime("YESTERDAY","ABSOLUTE","DATE")  !yesterday
$ydt = ydt + ":07:00:00.00"                      !at seven am
$directory/column=1 wind_l1_data:wi_l1*.dat/sin='ydt'
$fsize = f$file_attributes("dir.lis","EOF")
$if fsize .eqs. 0 then goto no_files
$@sms1:[wind.software.l1]process 7 dir.lis
$!wait 02:00:00
$!@sms1:[wind.software.l1]fe_pro 7 dir.lis
$!wait 00:30:00
$@sms1:[wind.software.l1]stack_pro 7 dir.lis
$@sms1:[wind.software.l1]co_process 7 dir.lis
$!mail/subject="new l1 files" dir.lis cohen
$mail/subject="new l1 files" dir.lis umdsp::jwilliams
$no_files:
$cdt = f$cvtime("TOMORROW","ABSOLUTE","DATE")  !next day
$cdt = cdt + ":07:00:00.00"                      !at seven am
$submit/noprint sms1:[wind.software.l1]routine.com/after='cdt'
$exit 1
