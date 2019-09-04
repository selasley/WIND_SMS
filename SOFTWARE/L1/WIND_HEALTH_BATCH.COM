$ SET NOON
$ maillist := 'f$getjpi("","USERNAME")
$ tempfile := SYS$LOGIN:L1LIST.TMP
$ summfile := SYS$LOGIN:SUMMARY.TMP
$
$ DIRECTORY/NOHEADER/NOTRAILER/COLUMN=1/CREATED/SINCE=TODAY/OUTPUT='tempfile' -
NRT_DIR:WI_L1_SMS*.%%%%;
$
$ count = 0
$ CREATE 'summfile'
   Date / Time       Filename
$ open/read tmp 'tempfile'
$
$loop:
$ read/end=done/err=done tmp line
$ l1file := 'f$parse(line,,,"NAME")
$ l1type := 'f$parse(line,,,"TYPE")
$ write sys$output "Ready to process  L1 file ",l1file,l1type
$ now = f$time()
$ DEFINE/USER L1_INPUT 'line'
$ RUN WIND_EXE:WIND_HEALTH
L1_INPUT:

0/
$ open/append summ 'summfile'
$ time_tag := 'f$cvtime(now,"ABSOLUTE","DATE")' 'f$extract(0,5,f$cvtime(now,"ABSOLUTE","TIME"))
$ write summ time_tag,"    ",l1file,l1type
$ close summ
$ count = count + 1
$ goto loop
$
$done:
$ close tmp
$ if count .eq. 0
$ then
$ MAIL/NOSELF/SUBJ="WIND_HEALTH processed ''count' L1 NRT files" -
SYS$INPUT 'maillist'
$ else
$ MAIL/NOSELF/SUBJ="WIND_HEALTH processed ''count' L1 NRT files" -
'summfile' 'maillist'
$ endif
$ DELETE 'tempfile';*,'summfile';*
$ exit 1
