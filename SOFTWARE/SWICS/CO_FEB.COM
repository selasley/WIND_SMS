$!
$! Procedure:  CO_FEB.COM
$! Author:     Chris Cohen
$! Purpose:    Generate a monthly plot for WIND webpage from all the WIND L1
$!             files in the month February of a given year
$!
$! This file runs the program CO_OUTPUT located in sms1:[cohen.wind.swics]
$!    The program CO_OUTPUT generates a data file which contains alpha velocity,
$!    oxygen and carbon ionization temperatures and C/O abundance ratio.
$!    Output files are named WI_L1_SMS_yyyy02_V01.co 
$!    (e.g. WI_L1_SMS_199802_V01.co) written in the directory
$!    smsdisk4:[wind.l2data.swics.routine] 
$!
$! The idl program COPLOT.PRO is then used to generate a GIF file named
$!    yyyy02_v01.gif which is placed in the directory SMS1:[WIND.WWW.SWICS] for 
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
$!   (3) add the processing for Feb. 29 if exists in this year 
$!
$set def sms1:[wind.software.swics.scratch]
$run sms1:[cohen.wind.swics]co_output
smsdisk4:[wind.l2data.swics.routine]WI_L1_SMS_199802_v01.co
WIND_L1_DATA:WI_L1_SMS_19980201_V01.DAT;1
y
n
240
n
y
WIND_L1_DATA:WI_L1_SMS_19980202_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980203_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980204_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980205_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980206_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980207_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980208_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980209_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980210_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980211_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980212_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980213_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980214_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980215_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980216_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980217_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980218_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980219_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980220_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980221_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980222_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980223_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980224_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980225_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980226_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980227_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980228_V01.DAT;1
y
n
$!                         idl
$idl
.run sms1:[wind.software.swics]coplot
2
smsdisk4:[wind.l2data.swics.routine]WI_L1_SMS_199802_V01.co
sms1:[wind.www.swics]199802_V01.gif
exit
