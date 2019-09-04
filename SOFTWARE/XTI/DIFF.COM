$ set noon
$ cms diff TI.INC/gen [geotail.l1.src]TI.INC/out=tt:
$ cms diff TIBYTE.FOR/gen [geotail.l1.src]TIBYTE.FOR/out=tt:
$ cms diff TICHAR.FOR/gen [geotail.l1.src]TICHAR.FOR/out=tt:
$ cms diff TIINT.FOR/gen [geotail.l1.src]TIINT.FOR/out=tt:
$ cms diff TIINT2.FOR/gen [geotail.l1.src]TIINT2.FOR/out=tt:
$ cms diff TIINT4.FOR/gen [geotail.l1.src]TIINT4.FOR/out=tt:
$ cms diff TILOG.FOR/gen [geotail.l1.src]TILOG.FOR/out=tt:
$ cms diff TILRN.FOR/gen [geotail.l1.src]TILRN.FOR/out=tt:
$ cms diff TIREAL.FOR/gen [geotail.l1.src]TIREAL.FOR/out=tt:
$ cms diff TIUSER.FOR/gen [geotail.l1.src]TIUSER.FOR/out=tt:
$ cms diff TIXDRC.FOR/gen [geotail.l1.src]TIXDRC.FOR/out=tt:
$ cms diff TIXEIL.FOR/gen [geotail.l1.src]TIXEIL.FOR/out=tt:
$ cms diff TIXENT.FOR/gen [geotail.l1.src]TIXENT.FOR/out=tt:
$ cms diff TIXLRN.FOR/gen [geotail.l1.src]TIXLRN.FOR/out=tt:
