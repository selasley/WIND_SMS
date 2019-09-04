$ set noon
$ pattern := *.FOR;
$ gosub checkit
$ pattern := *.MMS;
$ gosub checkit
$ pattern := *.INC;
$ gosub checkit
$ exit 1
$checkit:
$loop:
$ fname := 'f$search(pattern)
$ if fname .eqs. "" then return 1
$ fname = f$element(0,";",fname)
$ fname = f$element(1,"]",fname)
$ cms sho ele 'fname' /out=nl:
$ goto loop
