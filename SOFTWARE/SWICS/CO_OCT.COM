$!
$! Procedure:  CO_OCT.COM
$! Author:     Chris Cohen
$! Purpose:    Generate a monthly plot for WIND webpage from all the WIND L1
$!             files in the month October of a given year
$!
$! This file runs the program CO_OUTPUT located in sms1:[cohen.wind.swics]
$!    The program CO_OUTPUT generates a data file which contains alpha velocity,
$!    oxygen and carbon ionization temperatures and C/O abundance ratio.
$!    Output files are named WI_L1_SMS_yyyy10_V01.co 
$!    (e.g. WI_L1_SMS_199810_V01.co) written in the directory
$!    smsdisk4:[wind.l2data.swics.routine] 
$!
$! The idl program COPLOT.PRO is then used to generate a GIF file named
$!    yyyy10_v01.gif which is placed in the directory SMS1:[WIND.WWW.SWICS] for 
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
smsdisk4:[wind.l2data.swics.routine]WI_L1_SMS_199810_v01.co
WIND_L1_DATA:WI_L1_SMS_19981001_V01.DAT;1
y
n
240
n
y
WIND_L1_DATA:WI_L1_SMS_19981002_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981003_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981004_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981005_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981006_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981007_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981008_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981009_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981010_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981011_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981012_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981013_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981014_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981015_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981016_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981017_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981018_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981019_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981020_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981021_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981022_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981023_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981024_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981025_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981026_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981027_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981028_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981029_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981030_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19981031_V01.DAT;1
y
n
$!                         idl
$idl
.run sms1:[wind.software.swics]coplot
2
smsdisk4:[wind.l2data.swics.routine]WI_L1_SMS_199810_V01.co
sms1:[wind.www.swics]199810_V01.gif
exit
