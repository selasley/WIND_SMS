$ i = 1
$ open/write pro pro.com
$ write pro "$ set def SMSDISK4:[WIND.L2DATA.STICS]"
$ if p1 .eqs. "" then inquire p1 "Enter number of files to run"
$ if p2 .nes. "" 
$ then 
$ open/read list 'p2                    !read headers
$ read list filename
$ read list filename
$ read list filename
$ endif
$ loop:
$ if p2 .eqs. "" 
$ then
$ inquire filename "Enter filename"
$ else
$ read/end=end/err=end list filename
$ if filename .eqs. "" then goto end
$ endif
$ write pro "$!                  PHA"
$ write pro "$ RUN SMS1:[WIND.SOFTWARE.STICS]STICS_MMQ_TABLE"
$ write pro "DATA_DIR:", filename
$ write pro "y"
$ write pro "1"
$ write pro "y"
$ write pro "2"
$ write pro "-1"
$ write pro "0"
$ write pro "n"
$ write pro "-1"
$ write pro "n"
$ write pro "PHA_TABLE.OUT"
$ write pro "-1/"
$ write pro "$!                    MATRIX"
$ write pro "$ RUN SMS1:[WIND.SOFTWARE.STICS]STICS_MQET_MATRIX"
$ write pro "DATA_DIR:", filename
$ write pro "y"
$ write pro "n"
$ write pro "n"
$ write pro "n"
$ write pro "MATRIX.OUT"
$ write pro "-1/"
$ write pro "$!                 DEFAULT RATE LIST"
$ write pro "$ RUN SMS1:[WIND.SOFTWARE.STICS]WSTICS_RATES"
$ write pro "DATA_DIR:", filename
$ write pro "y"
$ write pro "-1/"
$ write pro "1"    		!without efficiencies
$ write pro "2"			!2 Sci Rec averaging interval
$ write pro "WSTXRATE.OUT"	!output filename
$ write pro "n"			!default rate list
$ write pro "$!                         IDL"
$ write pro "$ IDL"
$ write pro " "
$ write pro ".run  sms1:[wind.software.stics]auto_stics_scatter"
$ write pro "MATRIX.OUT"
$ write pro "PHA_TABLE.OUT"
$ write pro "1" 			!create GIF file
$ write pro "y"
$ write pro ".run sms1:[wind.software.stics]dailyrateplt"
$ write pro "WSTXRATE.OUT"
$ write pro "1" 			!create GIF file and put in SMS1:[WIND.WWW.STICS]
$! write pro ".run sms1:[wind.software.stics]dailyrateplt"
$! write pro "WSTXRATE.OUT"
$! write pro "2"          	!create postscript file
$ write pro "exit"
$! write pro "$ psprint idl.ps"    !print postscript file
$ write pro "$!"
$ i = i + 1
$ if i .le. p1 then goto loop
$!
$ end:
$ close pro
$ if p2 .nes. "" then close list 
$ SUBMIT/NOTIFY PRO
$ exit
