$ if f$search("L1LIB.ZIP") .nes. "" then DELETE L1LIB.ZIP;*
$ ZIP L1LIB.ZIP WIND_L1*.FOR COMPARE_TIME.FOR WIND_DISSECT*PHA.FOR windmsg.msg windmsg.inc make_param.com make_l1_lib.com [wind.software.lib]wind_compress.for
