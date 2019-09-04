$!
$! Procedure:  CO_AUG.COM
$! Author:     Chris Cohen
$! Purpose:    Generate a monthly plot for WIND webpage from all the WIND L1
$!             files in the month August of a given year
$!
$! This file runs the program CO_OUTPUT located in sms1:[cohen.wind.swics]
$!    The program CO_OUTPUT generates a data file which contains alpha velocity,
$!    oxygen and carbon ionization temperatures and C/O abundance ratio.
$!    Output files are named WI_L1_SMS_yyyy08_V01.co 
$!    (e.g. WI_L1_SMS_199808_V01.co) written in the directory
$!    smsdisk4:[wind.l2data.swics.routine] 
$!
$! The idl program COPLOT.PRO is then used to generate a GIF file named
$!    yyyy08_v01.gif which is placed in the directory SMS1:[WIND.WWW.SWICS] for 
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
smsdisk4:[wind.l2data.swics.routine]WI_L1_SMS_199808_v01.co
WIND_L1_DATA:WI_L1_SMS_19980801_V01.DAT;1
y
n
240
n
y
WIND_L1_DATA:WI_L1_SMS_19980802_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980803_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980804_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980805_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980806_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980807_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980808_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980809_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980810_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980811_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980812_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980813_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980814_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980815_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980816_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980817_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980818_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980819_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980820_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980821_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980822_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980823_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980824_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980825_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980826_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980827_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980828_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980829_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980830_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980831_V01.DAT;1
y
n
$!                         idl
$idl
.run sms1:[wind.software.swics]coplot
2
smsdisk4:[wind.l2data.swics.routine]WI_L1_SMS_199808_V01.co
sms1:[wind.www.swics]199808_V01.gif
exit
