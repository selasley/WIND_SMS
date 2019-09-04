	SUBROUTINE WIND_L1_OPEN_DATE(unit, year, month, day, data_version,
	1 header, status)
 
C+
C 
C FUNCTIONAL DESCRIPTION:	
C 
C    This routine opens a Level 1 format file onto the unit number given by the
C    argument `unit'.  The filename is determined by parameters `year',
C    `month', `day', and `data_version'.  The file is assumed to be located in
C    a directory pointed to by the logical name DATA_DIR.  The file's file
C    header record is then read into the argument `header'.  It is also
C    retained in memory for future reference.  The fact that this unit is
C    opened is recorded internally for future access.  A status code is
C    returned in the argument `status'.
C
C FORMAL PARAMETERS:
C  
C     unit:
C	  An INTEGER*4 variable specifying the Fortran unit number onto which
C	  the L1 file will be opened.
C      
C     year:
C	  An INTEGER*4 variable giving the year of the data set to be opened.
C      
C     month:
C	  An INTEGER*4 variable giving the month of the data set to be opened or
C	  a zero.
C      
C     day:
C	  An INTEGER*4 variable giving the day of the month of the data set to
C	  be opened.  If month is zero this parameter instead gives the day of
C	  year of the data set.
C      
C     data_version:
C	  An INTEGER*4 variable giving the version of the data set to be opened.
C	  Valid values are 0-99.  There are two special case values for
C	  data_version:
C	    -1	use the highest data set version available for this file
C	    -2	use the lowest data set version available for this file
C      
C     header:
C	  A record variable of structure type ST_L1_FILE_HDR which will receive
C	  the file header of the Level 1 file.
C  
C     status:
C	  An INTEGER*4 variable which will receive, upon return, the status of
C	  the open operation.  A 1 indicates success, all other values indicate
C	  failure.
C  
C COMMON BLOCKS:
C  
C     None
C  
C DESIGN:
C  
C     If the parameter `month' is zero the parameter `day' is assumed to be the
C     day of year.
C     The filename will be constructed in the format WI_L1_SMS_yyyymmdd_Vvv.DAT.
C     If data_version is 0 or -1 do a wildcard search for the filename.
C     The L1 file is assumed to be located in the directory pointed to by the
C     logical name DATA_DIR:.
C     After the full filename with directory is constructed the routine
C     WIND_L1_OPEN is called to perform the actual open.  See documentation on
C     that routine for additional details.
C  
C 
C This version:    28-SEP-1995
C 
C Created: 
C 
C	 28-SEP-1995    Larry Bleau
C 
C Revised:
C 
C	 Date     | Name  | Description
C ----------------+-------+-----------------------------------------------------
C [change_entry]
C 
C-
CDEC$	IDENT '1.0'
	IMPLICIT NONE
	INCLUDE '($SSDEF)/NOLIST'
	INCLUDE '($RMSDEF)/NOLIST'
	INCLUDE 'INC_DIR:EDBDEF.INC/NOLIST'
	INCLUDE 'INC_DIR:L1FMT.INC/NOLIST'
	INCLUDE 'INC_DIR:L1READ.INC/NOLIST'
C
	INTEGER*4 unit,year,month,day,data_version,status
        RECORD /ST_L1_FILE_HEADER/ header
C
	INTEGER actual_month,actual_day,actual_version,context,nch
	INTEGER*4 LIB$FIND_FILE 
	CHARACTER*120 filename,filename2,pattern
	CHARACTER directory*9
	LOGICAL more_files
	DATA directory/'DATA_DIR:'/
C
	EXTERNAL WIND$_FILNOTOPN
C
C If month is zero assume day is the day of year.  Compute month and day from it
C and the current year.
C
	IF (month .eq. 0) THEN
c	    {executable_statement}...
	ELSE
	    actual_month = month
	    actual_day = day
	END IF
	actual_version = data_version
	IF (data_version .le. 0) THEN
C
C Construct a search pattern for the filename in the format
C WI_L1_SMS_yyyymmdd_V%%.DAT and search for it.
C If data_version is -1 caller wants highest (last) data set of this date; keep
C looping until we found the last file or there is an error.
C If data_version is -2 caller wants lowest (first) data set of this date; exit
C loop after finding first file.
C
	    WRITE (pattern, 1001) directory,year,actual_month,actual_day
1001	    FORMAT(A,'WI_L1_SMS_',I4,I2.2,I2.2,'_V%%.DAT')
	    nch = INDEX (pattern,' ') - 1
	    filename2 = ' '
C
	    more_files = .TRUE.
	    DO WHILE (more_files)
		status = LIB$FIND_FILE (pattern(1:nch), filename, context, , , , )
	type '(z8,1x,a)',status,filename(1:70)
		IF (status .eq. SS$_NORMAL) filename2 = filename
		IF (data_version .eq. -2    .or.
	1	    status .eq. RMS$_NMF    .or.
	1	    IAND (status, 1) .ne. 1) more_files = .FALSE.
	    END DO
C
C Done search, close out file search context
C If filename2 is non-blank it con tains the name of the file we want; otherwise
C there was an error of some type and status contains the appropriate condition
C code.
C
120	    CALL LIB$FIND_FILE_END (context)
	    IF (filename2 .eq. ' ') RETURN
	    filename = filename2
	ELSE
C
C Construct the filename in the format WI_L1_SMS_yyyymmdd_Vvv.DAT
C The L1 file is assumed to be located in the directory pointed to by the
C logical name DATA_DIR:.
C
	    WRITE (filename, 1002) directory,year,actual_month,actual_day,
	1     actual_version
1002	    FORMAT(A,'WI_L1_SMS_',I4,I2.2,I2.2,'_V',I2.2,'.DAT')
	ENDIF
C
C We now have the full filename with directory constructed.
C Call the routine WIND_L1_OPEN to perform the actual open.
C
	nch = INDEX (filename,' ') - 1
D	TYPE *,'Calling WIND_L1_OPEN with filename=<',filename(1:nch),'>'
	CALL WIND_L1_OPEN( unit, filename, header, status )
	RETURN
	END
