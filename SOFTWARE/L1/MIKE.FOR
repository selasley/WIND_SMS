        SUBROUTINE WIND_L1_OPEN_NEXT(unit,mode,filename,file_header,status)
 
C+
C 
C FUNCTIONAL DESCRIPTION:       
C
C    Open the next L1 file in sequence after the current L1 file.  The current
C    L1 file is indicated by the unit number passed in <unit>.  The current file
C    is closed and the new file is opened on the same unit number.
C
C FORMAL PARAMETERS:
C
C     unit:
C         An INTEGER*4 variable giving the Fortran unit on which the current L1
C         file is open.
C
C     mode:
C         The open mode to use.  The allowable values are given below:
C
C         Value     Meaning
C           1       Attempt to open the next file in sequence by incrementing
C                   the date (and possibly year) portion of the filename.  If
C                   the file does not exist return an error status.
C           2       Open the next file in sequence by incrementing the date (and
C                   possibly year) portion of the filename.  If the file does
C                   not exist keep incrementing the date until a file is found
C                   or MAX_ATTEMPT attempts have been made.
C
C     filename:
C         A CHARACTER variable which, upon return, will contain the name of the
C         L1 file which has been opened.  If the open failed filename will be
C         blank.
C
C     file_header:
C         A record of structure type ST_L1_FILE_HEADER.  Upon successful return
C         this will contain the file header of the newly opened file.
C
C     status:
C         An INTEGER*4 variable which, upon return, will contain the status of
C         the open next operation.  An 1 indicates success.
C
C  
C COMMON BLOCKS:
C  
C     L1READ	  holds headers, etc.
C  
C DESIGN:
C  
C     Use INQUIRE to find the current filename associated with the given unit,
C     then close the unit.  Decode the year, month, and day from the filename.
C     Calculate the next day's date and encode it into the filename again.  Open
C     the next file on the same unit number.  Use LIB$FIND_FILE routine to
C     search directory for first data set (lowest Vnn number) of that day to
C     handle case where only latest version is retained.
C
C     If the file does not exist and mode is 1, return with the status
C     WIND$_NONEXTFILE.  If mode is 2, keep incrementing the date (up to a
C     maximum of 10 days) and open to the first data set which does exist.
C  
C 
C This version:   1.2   13-MAR-1995
C 
C Created: 
C 
C        18-JAN-1995    Lawrence Bleau
C 
C Revised:
C 
C        Date     | Name  | Description
C ----------------+-------+-----------------------------------------------------
C 1.1 10-MAR-1995 |  LRB  | make use of WIND condition codes for errors, finish
C			    coding logic and testing
C 1.2 13-MAR-1995 |  LRB  | added call to lib$find_file to have program search
C                           for first data set of a given day (lowest Vnn
C			    number)
C 
C-
CDEC$   IDENT   '1.2'

        IMPLICIT NONE
        INCLUDE '($SSDEF)/NOLIST'
        INCLUDE '($RMSDEF)/NOLIST'
        INCLUDE 'INC_DIR:EDBDEF.INC /NOLIST'
        INCLUDE 'INC_DIR:L1FMT.INC /NOLIST'
        INCLUDE 'INC_DIR:L1READ.INC /NOLIST'
        INTEGER MAX_ATTEMPTS
        PARAMETER (MAX_ATTEMPTS = 10)
C
        INTEGER unit,mode,status
        CHARACTER*(*) filename
        RECORD /ST_L1_FILE_HEADER/ file_header
C
        INTEGER i,i2,pos,max_tries,count,year,month,day,file_format,nch,leap
        INTEGER num_days(12,2),patlen
	INTEGER*4 lib$find_file,lib$find_file_end,context
        LOGICAL opened,found,there
        CHARACTER this_file*100,new_file*100,pattern*100
        DATA num_days/31,28,31,30,31,30,31,31,30,31,30,31,
     &                31,29,31,30,31,30,31,31,30,31,30,31/

        EXTERNAL WIND$_FILNOTOPN,WIND$_FILNAMFMT,WIND$_NONEXTFILE
C
C Get the full filename associated with the given unit.  Parse it to locate the
C filename proper and where the date is located within the filename.  Finally,
C close the original L1 file.
C
        INQUIRE (UNIT = unit, OPENED = opened)
        IF (.not. opened) THEN
            status = %LOC(WIND$_FILNOTOPN)
            RETURN
        ENDIF
        INQUIRE (UNIT = unit, NAME = this_file)
        i = INDEX (this_file, ']') + 1
        i2 = INDEX (this_file(i:), '.') - 1
        CALL WIND_L1_CLOSE(unit, status)
        IF (status .ne. 1) RETURN
C
C Filename can be in one of two formats; check both and set pos to point to
C start of year portion of date.
C 
C Format 1: WI_L1_SMS_yyyymmdd_Vvv.DAT
C Format 2: yymmddvv.DAT

C
        IF (this_file(i:i+4) .eq. 'WI_L1') THEN
            file_format = 1
            pos = i + 10
        ELSE
            file_format = 2
            pos = i
        END IF
C
C Convert month, day to integers
C
        IF (file_format .eq. 1) THEN
            READ (this_file(pos:pos+7), 1001, ERR=900, IOSTAT=status)
     1        year,month,day
1001        FORMAT(I4.4, I2.2, I2.2)
        ELSE
            READ (this_file(pos:pos+5), 1002, ERR=900, IOSTAT=status)
     1        year,month,day
1002        FORMAT(I2.2, I2.2, I2.2)
        END IF
C
C Set up loop by getting a copy of the original filename, setting the maximum
C number of times to go around the loop, and setting the file-found flag to
C false.
C Go into loop, looking for next filename in sequence.  First compute new date,
C then construct new filename, and finally use INQUIRE to check if the file
C exists.  If it does we can exit the loop; if not we go around again.
C When constructing new filename replace the year, month, and day of month. 
C Also reset the data set version to '01' just in case the previous file was a
C higher data set version.  There is no current mechanism to handle accessing
C later data set versions.
C
C If mode=1 we go through loop only once; if file for next date isn't there we
C quit.  If mode=2 we keep trying, up to a reasonable maximum number of times.
C
        pattern = this_file
        patlen = INDEX (pattern, ';')
        IF (patlen .ne. 0) pattern(patlen:) = ' '
        patlen = INDEX (pattern, ' ') - 1
        max_tries = 1
        IF (mode .eq. 2) max_tries = MAX_ATTEMPTS
        count = 1
        found = .FALSE.
        leap = 1
        IF (MOD(year,4) .eq. 0 .and. MOD(year,100) .ne. 0) leap = 2
C
        DO WHILE (count .le. max_tries .and. .not. found)
            day = day + 1
            IF (day .gt. num_days(month,leap)) THEN
                month = month + 1
                day = 1
                IF (month .gt. 12) THEN
                    year = year + 1
                    leap = 1
                    IF (MOD(year,4) .eq. 0 .and. MOD(year,100) .ne. 0) leap = 2
                    month = 1
                END IF
            END IF
            IF (file_format .eq. 1) THEN
                WRITE (pattern(pos:pos+7), 1001) year,month,day
                pattern(pos+10:pos+11) = '%%'
            ELSE
                WRITE (pattern(pos:pos+5), 1002) MOD(year,100),month,day
                pattern(pos+6:pos+7) = '%%'
            END IF
C
C Now search for the filename, matching the lowest data set version (Vnn)
C If we found it get its length and set found to true
C If we didn't find it this day's data set is missing, set found to false
C
	character*8 ymd
	character pettern*80
	integer*4 context, status
C
	context = 0
	write(ymd,101) year,month,day
101	format(i4,2i2.2)
	pattern = 'WIND_L1_DATA:WI_L1_SMS_' // ymd // '_V%%.DAT'
	patlen = INDEX( pattern, ' ') - 1

      	status = lib$find_file (pattern(1:patlen), new_file, context, , , ,)
	    IF (status .eq. RMS$_NORMAL) THEN	    ! we found a file
		nch = INDEX( new_file, ' ' ) - 1    ! get filename's length
      	        INQUIRE (FILE = new_file(1:nch), EXIST = found)
      	    ELSE			      	    ! not found,
		found = .false.			    ! st flag accordingly
	    END IF
	    status = lib$find_file_end (context)
            count = count + 1
        END DO
C
C Done with loop.  We either have a file we can open (found is true) or we ran
C out of possibilities (found=false).  If found open it and return filename to
C caller, otherwise return blank filename and an error status.
C
        IF (found) THEN
            CALL WIND_L1_OPEN(unit, new_file(1:nch), file_header, status)
        ELSE
            status = %LOC(WIND$_NONEXTFILE)
        END IF
        IF (IAND(status,1) .eq. 1 .or. IAND(status,7) .eq. 0) THEN
            filename = new_file(1:nch)
        ELSE
            filename = ' '
        END IF
        RETURN
C
C Error handling code
C
900     filename = ' '
        status = %LOC(WIND$_FILNAMFMT)
        RETURN
        END
