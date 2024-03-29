$!
$! Procedure:  CO_MAR.COM
$! Author:     Chris Cohen
$! Purpose:    Generate a monthly plot for WIND webpage from all the WIND L1
$!             files in the month March of a given year
$!
$! This file runs the program CO_OUTPUT located in sms1:[cohen.wind.swics]
$!    The program CO_OUTPUT generates a data file which contains alpha velocity,
$!    oxygen and carbon ionization temperatures and C/O abundance ratio.
$!    Output files are named WI_L1_SMS_yyyy03_V01.co 
$!    (e.g. WI_L1_SMS_199803_V01.co) written in the directory
$!    smsdisk4:[wind.l2data.swics.routine] 
$!
$! The idl program COPLOT.PRO is then used to generate a GIF file named
$!    yyyy03_v01.gif which is placed in the directory SMS1:[WIND.WWW.SWICS] for 
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
smsdisk4:[wind.l2data.swics.routine]WI_L1_SMS_199803_v01.co
WIND_L1_DATA:WI_L1_SMS_19980301_V01.DAT;1
y
n
240
n
y
WIND_L1_DATA:WI_L1_SMS_19980302_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980303_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980304_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980305_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980306_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980307_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980308_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980309_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980310_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980311_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980312_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980313_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980314_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980315_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980316_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980317_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980318_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980319_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980320_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980321_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980322_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980323_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980324_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980325_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980326_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980327_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980328_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980329_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980330_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980331_V01.DAT;1
y
n
$!                         idl
$idl
.run sms1:[wind.software.swics]coplot
2
smsdisk4:[wind.l2data.swics.routine]WI_L1_SMS_199803_V01.co
sms1:[wind.www.swics]199803_V01.gif
exit
