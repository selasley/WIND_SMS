$!
$! Procedure:  CO_APR.COM
$! Author:     Chris Cohen
$! Purpose:    Generate a monthly plot for WIND webpage from all the WIND L1
$!             files in the month April of a given year
$!
$! This file runs the program CO_OUTPUT located in sms1:[cohen.wind.swics]
$!    The program CO_OUTPUT generates a data file which contains alpha velocity,
$!    oxygen and carbon ionization temperatures and C/O abundance ratio.
$!    Output files are named WI_L1_SMS_yyyy04_V01.co 
$!    (e.g. WI_L1_SMS_199804_V01.co) written in the directory
$!    smsdisk4:[wind.l2data.swics.routine] 
$!
$! The idl program COPLOT.PRO is then used to generate a GIF file named
$!    yyyy04_v01.gif which is placed in the directory SMS1:[WIND.WWW.SWICS] for 
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
smsdisk4:[wind.l2data.swics.routine]WI_L1_SMS_199804_v01.co
WIND_L1_DATA:WI_L1_SMS_19980401_V01.DAT;1
y
n
240
n
y
WIND_L1_DATA:WI_L1_SMS_19980402_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980403_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980404_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980405_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980406_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980407_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980408_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980409_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980410_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980411_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980412_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980413_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980414_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980415_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980416_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980417_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980418_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980419_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980420_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980421_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980422_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980423_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980424_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980425_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980426_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980427_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980428_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980429_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980430_V01.DAT;1
y
n
$!                         idl
$idl
.run sms1:[wind.software.swics]coplot
2
smsdisk4:[wind.l2data.swics.routine]WI_L1_SMS_199804_V01.co
sms1:[wind.www.swics]199804_V01.gif
exit
