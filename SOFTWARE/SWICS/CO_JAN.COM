$!
$! Procedure:  CO_JAN.COM
$! Author:     Chris Cohen
$! Purpose:    Generate a monthly plot for WIND webpage from all the WIND L1
$!             files in the month January of a given year
$!
$! This file runs the program CO_OUTPUT located in sms1:[cohen.wind.swics]
$!    The program CO_OUTPUT generates a data file which contains alpha velocity,
$!    oxygen and carbon ionization temperatures and C/O abundance ratio.
$!    Output files are named WI_L1_SMS_yyyy01_V01.co 
$!    (e.g. WI_L1_SMS_199801_V01.co) written in the directory
$!    smsdisk4:[wind.l2data.swics.routine] 
$!
$! The idl program COPLOT.PRO is then used to generate a GIF file named
$!    yyyy01_v01.gif which is placed in the directory SMS1:[WIND.WWW.SWICS] for 
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
smsdisk4:[wind.l2data.swics.routine]WI_L1_SMS_199801_v01.co
WIND_L1_DATA:WI_L1_SMS_19980101_V01.DAT;1
y
n
240
n
y
WIND_L1_DATA:WI_L1_SMS_19980102_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980103_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980104_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980105_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980106_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980107_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980108_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980109_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980110_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980111_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980112_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980113_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980114_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980115_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980116_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980117_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980118_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980119_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980120_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980121_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980122_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980123_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980124_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980125_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980126_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980127_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980128_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980129_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980130_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980131_V01.DAT;1
y
n
$!                         idl
$idl
.run sms1:[wind.software.swics]coplot
2
smsdisk4:[wind.l2data.swics.routine]WI_L1_SMS_199801_V01.co
sms1:[wind.www.swics]199801_V01.gif
exit
