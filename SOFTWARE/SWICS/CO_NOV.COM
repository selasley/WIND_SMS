$!
$! Procedure:  CO_NOV.COM
$! Author:     Chris Cohen
$! Purpose:    Generate a monthly plot for WIND webpage from all the WIND L1
$!             files in the month November of a given year
$!
$! This file runs the program CO_OUTPUT located in sms1:[cohen.wind.swics]
$!    The program CO_OUTPUT generates a data file which contains alpha velocity,
$!    oxygen and carbon ionization temperatures and C/O abundance ratio.
$!    Output files are named WI_L1_SMS_yyyy11_V01.co 
$!    (e.g. WI_L1_SMS_199811_V01.co) written in the directory
$!    smsdisk4:[wind.l2data.swics.routine] 
$!
$! The idl program COPLOT.PRO is then used to generate a GIF file named
$!    yyyy11_v01.gif which is placed in the directory SMS1:[WIND.WWW.SWICS] for 
$!    access by the Wind homepage.
$!
$! After the monthly GIF file has been created, one should delete the daily GIF
$!    files of this month to save disk space, except for some dates which have 
$!    special events (e.g. CME) happening and worth saving for public browsing.
$!
$! set sms1:[wind.software.swics.scratch] as the working directory
$!
$! **Before submitting this job for a given year, one just needs to do the 
$!   following:
$!   (1) replace all year (4-digit number) with the year needed for processing
$!   (2) check in WIND_L1_DATA: to make sure the latest version number for each
$!       date is used (since some dates have L1 files with more than one
$!       version).
$!
$set def sms1:[wind.software.swics.scratch]
$run sms1:[cohen.wind.swics]co_output
smsdisk4:[wind.l2data.swics.routine]WI_L1_SMS_199811_v01.co
WIND_L1_DATA:WI_L1_SMS_19981101_V01.DAT;1
y
n
240
n
y
WIND_L1_DATA:WI_L1_SMS_19981102_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981103_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981104_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981105_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981106_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981107_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981108_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981109_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981110_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981111_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981112_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981113_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981114_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981115_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981116_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981117_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981118_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981119_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981120_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981121_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981122_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981123_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981124_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981125_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981126_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981127_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981128_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981129_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981130_V01.DAT;1
y
n
$!                         idl
$idl
.run sms1:[wind.software.swics]coplot
2
smsdisk4:[wind.l2data.swics.routine]WI_L1_SMS_199811_V01.co
sms1:[wind.www.swics]199811_V01.gif
exit
