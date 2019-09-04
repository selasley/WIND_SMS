$!
$! Procedure:  CO_JUL.COM
$! Author:     Chris Cohen
$! Purpose:    Generate a monthly plot for WIND webpage from all the WIND L1
$!             files in the month July of a given year
$!
$! This file runs the program CO_OUTPUT located in sms1:[cohen.wind.swics]
$!    The program CO_OUTPUT generates a data file which contains alpha velocity,
$!    oxygen and carbon ionization temperatures and C/O abundance ratio.
$!    Output files are named WI_L1_SMS_yyyy07_V01.co 
$!    (e.g. WI_L1_SMS_199807_V01.co) written in the directory
$!    smsdisk4:[wind.l2data.swics.routine] 
$!
$! The idl program COPLOT.PRO is then used to generate a GIF file named
$!    yyyy07_v01.gif which is placed in the directory SMS1:[WIND.WWW.SWICS] for 
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
smsdisk4:[wind.l2data.swics.routine]WI_L1_SMS_199807_v01.co
WIND_L1_DATA:WI_L1_SMS_19980701_V01.DAT;1
y
n
240
n
y
WIND_L1_DATA:WI_L1_SMS_19980702_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980703_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980704_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980705_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980706_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980707_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980708_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980709_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980710_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980711_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980712_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980713_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980714_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980715_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980716_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980717_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980718_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980719_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980720_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980721_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980722_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980723_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980724_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980725_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980726_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980727_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980728_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980729_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980730_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980731_V01.DAT;1
y
n
$!                         idl
$idl
.run sms1:[wind.software.swics]coplot
2
smsdisk4:[wind.l2data.swics.routine]WI_L1_SMS_199807_V01.co
sms1:[wind.www.swics]199807_V01.gif
exit
