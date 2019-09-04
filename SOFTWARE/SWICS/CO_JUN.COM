$!
$! Procedure:  CO_JUN.COM
$! Author:     Chris Cohen
$! Purpose:    Generate a monthly plot for WIND webpage from all the WIND L1
$!             files in the month June of a given year
$!
$! This file runs the program CO_OUTPUT located in sms1:[cohen.wind.swics]
$!    The program CO_OUTPUT generates a data file which contains alpha velocity,
$!    oxygen and carbon ionization temperatures and C/O abundance ratio.
$!    Output files are named WI_L1_SMS_yyyy06_V01.co 
$!    (e.g. WI_L1_SMS_199806_V01.co) written in the directory
$!    smsdisk4:[wind.l2data.swics.routine] 
$!
$! The idl program COPLOT.PRO is then used to generate a GIF file named
$!    yyyy06_v01.gif which is placed in the directory SMS1:[WIND.WWW.SWICS] for 
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
smsdisk4:[wind.l2data.swics.routine]WI_L1_SMS_199806_v01.co
WIND_L1_DATA:WI_L1_SMS_19980601_V01.DAT;1
y
n
240
n
y
WIND_L1_DATA:WI_L1_SMS_19980602_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980603_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980604_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980605_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980606_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980607_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980608_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980609_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980610_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980611_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980612_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980613_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980614_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980615_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980616_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980617_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980618_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980619_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980620_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980621_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980622_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980623_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980624_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980625_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980626_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980627_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980628_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980629_V01.DAT;1
y
y
WIND_L1_DATA:WI_L1_SMS_19980630_V01.DAT;1
y
n
$!                         idl
$idl
.run sms1:[wind.software.swics]coplot
2
smsdisk4:[wind.l2data.swics.routine]WI_L1_SMS_199806_V01.co
sms1:[wind.www.swics]199806_V01.gif
exit
