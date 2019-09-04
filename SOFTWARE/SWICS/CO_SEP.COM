$!
$! Procedure:  CO_SEP.COM
$! Author:     Chris Cohen
$! Purpose:    Generate a monthly plot for WIND webpage from all the WIND L1
$!             files in the month September of a given year
$!
$! This file runs the program CO_OUTPUT located in sms1:[cohen.wind.swics]
$!    The program CO_OUTPUT generates a data file which contains alpha velocity,
$!    oxygen and carbon ionization temperatures and C/O abundance ratio.
$!    Output files are named WI_L1_SMS_yyyy09_V01.co 
$!    (e.g. WI_L1_SMS_199809_V01.co) written in the directory
$!    smsdisk4:[wind.l2data.swics.routine] 
$!
$! The idl program COPLOT.PRO is then used to generate a GIF file named
$!    yyyy09_v01.gif which is placed in the directory SMS1:[WIND.WWW.SWICS] for 
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
smsdisk4:[wind.l2data.swics.routine]WI_L1_SMS_199809_v01.co
WIND_L1_DATA:WI_L1_SMS_19980901_V01.DAT;1
y
n
240
n
y
WIND_L1_DATA:WI_L1_SMS_19980902_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980903_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980904_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980905_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980906_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980907_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980908_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980909_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980910_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980911_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980912_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980913_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980914_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980915_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980916_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980917_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980918_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980919_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980920_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980921_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980922_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980923_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980924_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980925_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980926_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980927_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980928_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980929_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980930_V01.DAT;1
y
n
$!                         idl
$idl
.run sms1:[wind.software.swics]coplot
2
smsdisk4:[wind.l2data.swics.routine]WI_L1_SMS_199809_V01.co
sms1:[wind.www.swics]199809_V01.gif
exit
