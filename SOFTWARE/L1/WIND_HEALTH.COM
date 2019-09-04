$ if p1 .eqs. ""
$ then
$ write sys$output "Usage: @WIND_HEALTH name-of-L1-file"
$ exit
$ endif
$ l1file = f$parse(p1,"WIND_L1_DATA:.DAT")
$ if f$search(l1file) .eqs. ""
$ then
$ write sys$output "File ''p1' does not exist"
$ exit
$ endif
$
$ tempfile := SYS$LOGIN:WIND_HEALTH_'f$getjpi("","PID")'.TMP
$
$ open/write tmp 'tempfile'
$ write tmp "DEFINE/USER L1INPUT ",l1file
$ write tmp "$ RUN/NODEBUG WIND_HEALTH"
$ write tmp "L1INPUT:"
$ write tmp ""
$ write tmp "0/"
$ close tmp
$
$ on control_y then goto done
$ @'tempfile'
$
$done:
$ DELETE 'tempfile';
$ exit 1
