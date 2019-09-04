$!
$! P1 = input filename; assummed file type .LIS
$! P2 = output filename; assummed file type .INC
$!
$ if p1 .eqs. ""
$ then
$ write sys$output "Filename required"
$ exit 1
$ endif
$ inpfile = f$parse(p1,".LIS")
$ outfile = f$parse(p2,".INC",inpfile)
$ if f$search(inpfile) .eqs. "" then goto noinp
$ open/read/err=noinp in 'inpfile'
$ CREATE 'outfile'
$ open/append out 'outfile'
$ write out "C"
$ write out "C WIND include file to define Error Status Codes"
$ write out "C"
$
$loop1:
$ read/end=rderr/err=rderr in line
$ line = f$edit(line,"UPCASE,COLLAPSE")
$ i = f$locate("PREFIX=",line)
$ if i .eq. f$length(line) then goto loop1
$ i = i + 7
$ prefix = f$extract(i,f$length(line)-i,line)
$
$loop2:
$ read/err=rderr/end=rdend in line
$ if f$extract(0,25,line) .nes. "                         " then goto loop2
$ if f$extract(25,1,line) .eqs. " " then goto loop2
$ value = f$extract(25,8,line)
$ rest = f$extract(40,255,line)
$ i = f$locate("<",rest)
$ if i .ne. f$length(rest)
$ then
$ text = f$extract(i+1,f$length(rest)-(i+2),rest)
$ else
$ text = ""
$ endif
$ rest = f$edit(rest,"COMPRESS")
$ symbol = f$element(0," ",rest)
$ symbol = prefix + symbol
$ write out "        INTEGER ",symbol
$ lineout = "        PARAMETER (" + symbol + " = '" + value + "'X)"
$ if text .nes. ""
$ then
$ lineout = lineout + "                                                  "
$ i = f$locate(")",lineout)
$ if i .lt. 50 then i = 50
$ lineout = f$extract(0,i+1,lineout)
$ lineout = lineout + " ! " + text
$ endif
$ write out lineout
$ goto loop2
$
$rdend:
$ close in
$ close out
$ exit 1
$
$noinp:
$ write sys$output "Error opening input file ''inpfile'"
$ exit 1
$rderr:
$ write sys$output "Error reading input file"
$ close in
$ close out
$ exit 1
