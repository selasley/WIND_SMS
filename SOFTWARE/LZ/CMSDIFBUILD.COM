$ if p1 .nes. "" then SET DEFAULT 'p1'
$ target := *.FOR;
$ if p2 .nes. "" then target := 'p2
$ again := T
$ open/write cmd CMSDIF.COM
$ write cmd "$ SET NOON"
$loop:
$ fname = f$search(target)
$ if fname .eqs. "" then goto done1
$ elename = f$parse(fname,,,"NAME") + f$parse(fname,,,"TYPE") 
$ write cmd "$ CMS DIFF ''elename' /GEN ''fname' /OUT=SYS$OUTPUT:"
$ goto loop
$done1:
$ if p2 .nes. "" .or. .not. again then goto close
$ target := *.INC
$ again := F
$ goto loop
$close:
$ close cmd
$ exit 1
