        SUBROUTINE WIND_BUILD_FILE_HEADER
C+
C 
C FUNCTIONAL DESCRIPTION:       
C 
C  This subroutine fills in the various fields of the Level 1 file header with
C  data which was stored by the main program in preparation for it being
C  written to disk.
C
C FORMAL PARAMETERS:
C  
C   None
C  
C COMMON BLOCKS:
C  
C   FILE        contains name of input and output files
C   L1_HEADERS  contains L1 header structures
C  
C DESIGN:
C  
C   Access the various COMMON blocks maintained by WIND_CD_READ to obtain the
C   needed information.  Use system calls to get the remaining information.
C
C   The comment field can have two sources: a program-generated value and a
C   user-specified value.  The user can specify a value by defining the logical
C   name WIND_COMMENT before running the program.  WIND_CD_READ can also store
C   a comment directly in the file header structure.  The user comment is thus
C   overwritten and superceded if WIND_CD_READ stores anything there.  The
C   length of the comment field is computed by WIND_BUILD_FILE_HEADER. 
C
C This version:   1.16   12-APR-1995
C 
C Created: 
C 
C      14-OCT-1994    Larry Bleau
C 
C Revised:
C 
C Ver      Date       | Name  | Description
C --------------------+------------+------------------------------------------------
C 1.1	18-OCT-1994   |  JP   | Replaced version number with Creation Date of
C			        .EXE file 
C 1.2	 8-NOV-1994   |  LRB  | added system call to fill in architecture type
C 1.3	10-NOV-1994   |  LRB  | added system call to fill in medium field
C 1.4	 5-DEC-1994   |  LRB  | changed header version to '02' to reflect new
C			        /CORE/ definition
C 1.5	13-DEC-1994   |  LRB  | changed header version to '03' to reflect new
C			        /HKeep/ definition
C 1.6	15-DEC-1994   |  LRB  | filled in new file header fields, changed
C			        header version to '04'
C 1.7	 6-FEB-1995   |  LRB  | changed header version to '05' to reflect new
C			        fields in sci rec hdr
C 1.10	15-FEB-1995   |  LRB  | corrected code to put ending time into header
C 1.11	 8-MAR-1995   |  LRB  | copy LINK_DATE_TIME to date_of_EXE field in
C			        file header; add program_vers field to file
C			        header, did not update header version
C 1.13	15-MAR-1995   |  LRB  | change names of vars in common block DATES
C 1.15	10-APR-1995   |  LRB  | added field eph_avail to file header
C 1.16	12-APR-1995   |  LRB  | removed LIB$GET_DVI system call, access FROM_CD
C				flag instead; add include L1INFO.INC
C-
CDEC$   IDENT   '1.16'

        IMPLICIT NONE
        INCLUDE 'INC_DIR:EDBDEF.INC/NOLIST'
        INCLUDE 'INC_DIR:EDBVARS.INC/NOLIST'
        INCLUDE 'INC_DIR:L1FMT.INC/NOLIST'
        INCLUDE 'INC_DIR:DIAG.INC/NOLIST'
        INCLUDE 'INC_DIR:L1INFO.INC/NOLIST'
        INCLUDE '($SYIDEF)/NOLIST'
	INCLUDE '($DVIDEF)/NOLIST'
C
        INTEGER year,hrs,mins,secs,month,day,pos
        INTEGER*4 nch,doy
        CHARACTER date1*9,date_str*11,ascii_date*23
        CHARACTER*80 lz_filename, l1_filename
        CHARACTER arch_name*15,media_name*10
        INTEGER*4 Clock_Beg_Year, Clock_Beg_DOY, Clock_Beg_Msec, Clock_Beg_MicSec
        INTEGER*4 Clock_End_Year, Clock_End_DOY, Clock_End_Msec, Clock_End_MicSec
C
        COMMON /FILE/ lz_filename, l1_filename
	COMMON /DATES/ Clock_Beg_Year, Clock_Beg_DOY, Clock_Beg_Msec, Clock_Beg_MicSec,
     &                 Clock_End_Year, Clock_End_DOY, Clock_End_Msec, Clock_End_MicSec
C
C Start by filling in the ident field (always the constant 'HDRF') and the
C header version number
C
        l1_file_header.ident = 'HDRF'
        l1_file_header.header_vers = '06'
C
C Compute length of comment field and store it in comment_len field
C A length of zero indicates there was no comment
C
        nch = LEN(l1_file_header.comment)
        DO WHILE (nch .ge. 1)
          IF (l1_file_header.comment(nch:nch) .ne. ' ') GOTO 110
          nch = nch - 1
        END DO
110	WRITE(l1_file_header.comment_len, 111) nch
111	FORMAT(i3)
C
C Get today's date and fill in the run_date field
C Adjust year for current century
C
        CALL date (date1)
        READ(date1(8:9), 112) year
112	FORMAT(i2)
        IF (year .lt. 90) THEN
          year = 2000 + year
        ELSE
          year = year + 1900
        END IF
        WRITE(l1_file_header.run_date, 113) date1(1:7),year
113	FORMAT(a7,i4)
C
C Get input filename and save in header
C
        l1_file_header.input_file = lz_filename
C
C Set medium code in header: 'CD' = CDROM, 'DK' = magnetic disk
C
        l1_file_header.medium = '  '
	IF (from_cd) THEN
	  l1_file_header.medium = 'CD'
	ELSE
	  l1_file_header.medium = 'DK'
	ENDIF
C
C Get system hardware (architecture) type and save it
C Currently only types are 'VAX' and 'Alpha', so saving first letter is enough
C
        arch_name(1:1) = ' '		      ! just in case it fails
	CALL lib$getsyi (
	1	 syi$_arch_name,, 
	1	 arch_name, 
	1	 nch,,) 
        l1_file_header.hw_type = arch_name(1:1)
C
C Fill in start and stop times
C
	CALL doy_to_month_day( Clock_Beg_Year, Clock_Beg_DOY, month, day, date_str )
	hrs = Clock_Beg_Msec/(60*60*1000)
	mins = Clock_Beg_Msec/(60*1000) - hrs*60
	secs = Clock_Beg_Msec/1000 - (hrs*60+mins)*60
        write(l1_file_header.start_time, 1001) date_str, hrs, mins, secs
C
	CALL doy_to_month_day( Clock_End_Year, Clock_End_DOY, month, day, date_str )
	hrs = Clock_End_Msec/(60*60*1000)
	mins = Clock_End_Msec/(60*1000) - hrs*60
	secs = Clock_End_Msec/1000 - (hrs*60+mins)*60
        write(l1_file_header.stop_time, 1001) date_str, hrs, mins, secs
C
C Save time this program was linked
C Also save program's version number
C
	l1_file_header.date_of_EXE = link_date_time
	l1_file_header.program_vers = prog_version
C
C Save creation date of LZ file
C
	CALL GET_CDT(lz_filename,ascii_date,doy)
	WRITE(l1_file_header.credate_doy, 1005) doy
	l1_file_header.credate_year = ascii_date(8:11)
	l1_file_header.credate = ascii_date(1:11)
C
C Save number of L1 records written, number of good EDBs processed, number of
C EDBs lost, and number of EDBs skipped
C
        WRITE(l1_file_header.L1_records, 1003) MIN(L1_recs_written,99999)
        WRITE(l1_file_header.EDB_count, 1004) MIN(EDB_Sum,999999)
        WRITE(l1_file_header.EDB_lost, 1004) MIN(EDB_Lost,999999)
        WRITE(l1_file_header.EDB_skip, 1004) MIN(EDB_Skip,999999)
C
1001	FORMAT(A11,1H ,I2.2,1H:,I2.2,1H:,I2.2)
1002    FORMAT(I4,1H ,I3.3,1H.,I8.8,I4.4)
1003    FORMAT(I4)
1004    FORMAT(I6)
1005	FORMAT(I3)
C
C If ephemeris and attitude information is available set eph_avail field in the
C file header to 'T'; otherwise set it to 'F'.
C
	IF (eph_avail) THEN
	  l1_file_header.eph_avail = 'T'
	ELSE
	  l1_file_header.eph_avail = 'F'
	ENDIF
C
        RETURN
        END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        SUBROUTINE WIND_BUILD_SR_HEADER
C+
C 
C FUNCTIONAL DESCRIPTION:       
C 
C  This subroutine fills in the various fields of the Level 1 Science Record
C  header in preparation for it being written to disk.
C 
C FORMAL PARAMETERS:
C  
C   None
C  
C COMMON BLOCKS:
C  
C   See EDBVARS.INC, L1_HDR_INFO.INC
C  
C DESIGN:
C  
C     Instead of having a long parameter list, information will be gathered from
C     various common blocks which contain the science record.  Only info on the
C     first science record (the SCI(1) structure and its partners HDB(1) and
C     HKeep(1)).
C  
C This version:  1.16    10-APR-1995
C 
C Created: 
C 
C      18-OCT-1994    Larry Bleau
C 
C Revised:
C 
C        Date       |     Name    | Description
C ------------------+-------------+-----------------------------------------------------
C      18-OCT-1994  | J.PAQUETTE  | Corrected use of IBSET
C       2-NOV-1994  | L. Bleau    | added sci rec counter (from HDB) to header
C 1.7  10-JAN-1995  | L. Bleau    | added statement to always fill in ident
C                                 | field of SR header
C 1.8   6-FEB-1995  | L. Bleau    | add code to scan for zero PHA words in each
C                                 | spin and set a sensor-specific flag if there
C                                 | are any 
C 1.9  14-FEB-1995  | L. Bleau    | correct code which scanned for zero MASS PHA
C                       	    words
C 1.10  6-APR-1995  | J. Paquette | added code in preparation to including
C                                   ephemeris data in record header
C       7-APR-1995  | L. Bleau    | added include stmt for L1_HDR_INFO.INC
C 1.15 10-APR-1995  | L. Bleau    | added code to transfer orbit info into
C				    header 
C 1.16 12-APR-1995  | L. Bleau    | add include L1INFO.INC
C 
C-
      IMPLICIT NONE
      INCLUDE 'INC_DIR:EDBDEF.INC/NOLIST'
      INCLUDE 'INC_DIR:EDBVARS.INC/NOLIST'
      INCLUDE 'INC_DIR:L1FMT.INC/NOLIST'
      INCLUDE 'INC_DIR:L1_HDR_INFO.INC/NOLIST'
      INCLUDE 'INC_DIR:L1INFO.INC/NOLIST'
      INTEGER*4 SPIN,TEMP,SRCOUNT,ZERO_COUNT,WHERE(100),FIRST_VALID,EPH_INDEX/1/
      INTEGER*4 MAX_EPH_INDEX/144/
      LOGICAL*1 LOG_VAR
      BYTE BYTE_VAR
      EQUIVALENCE (LOG_VAR,BYTE_VAR)
C
      L1_SR_HEADER.IDENT = 'HDRR'
      L1_SR_HEADER.NVALID = 0
      L1_SR_HEADER.PRESENT(1) = 0
      L1_SR_HEADER.PRESENT(2) = 0
      L1_SR_HEADER.BITRATE_CODE = 0
      L1_SR_HEADER.ZERO_SWICS_PHA(1) = 0
      L1_SR_HEADER.ZERO_SWICS_PHA(2) = 0
      L1_SR_HEADER.ZERO_MASS_PHA(1) = 0
      L1_SR_HEADER.ZERO_MASS_PHA(2) = 0
      L1_SR_HEADER.ZERO_STICS_PHA(1) = 0
      L1_SR_HEADER.ZERO_STICS_PHA(2) = 0
C
C Scan through spins and do the following:
C 1) count number of valid spins (qual flag zero)
C 2) if a spin is present set corresponding bit in present bit array
C 3) note if low, high, or mixed bitrate
C 4) set flag in sensor-specific bit array if any of its PHA words are zero
C 5) find first valid spin for use in time comparison below
      FIRST_VALID = -1
      DO 100 SPIN=0,59
        LOG_VAR = SCI(1).EDB(SPIN).QUAL
        IF (BYTE_VAR .EQ. 0) THEN                     ! valid spin?
C
C Count number of valid spins.
C
          L1_SR_HEADER.NVALID = L1_SR_HEADER.NVALID + 1
C
C Set presence flag for this spin.
C
          L1_SR_HEADER.PRESENT(SPIN/32+1) = IBSET( L1_SR_HEADER.PRESENT(SPIN/32+1), MOD(SPIN,32) )
C
C Set bit rate code for Science Record. If ALL spins in SCI-REC are at high bit
C rate, L1_SR_HEADER.BITRATE_CODE is 2; if ALL spins in SCI-REC were at low bit
C rate, then L1_SR_HEADER.BITRATE_CODE is 1. If the SCI-REC contained a mixture
C of bit rates (some high, some low), then L1_SR_HEADER.BITRATE_CODE is 3.
C
          IF (SCI(1).EDB(SPIN).BitRate) THEN
C
C If a spin is high bit rate set bit 1 of L1_SR_HEADER.BITRATE_CODE.
C
            L1_SR_HEADER.BITRATE_CODE = IBSET(L1_SR_HEADER.BITRATE_CODE,1)
          ELSE
C
C If a spin is low bit rate set bit 0  of L1_SR_HEADER.BITRATE_CODE.
C
            L1_SR_HEADER.BITRATE_CODE = IBSET(L1_SR_HEADER.BITRATE_CODE,0)
          ENDIF
C
C If there are any PHA words scan them for any zeros
C If a zero PHA word is found (an illegal value) set the corresponding bit
C in the bit array ZERO_SWICS_PHA
C
	  IF (SCI(1).EDB(SPIN).SWICS_Len .NE. 0) THEN
	    CALL SCAN_ZERO_PHA( SWICS_PHA(1).EDB(SPIN).PHA, 
     &				SCI(1).EDB(SPIN).SWICS_Len*4,
     &				4, ZERO_COUNT, WHERE )
	    IF (ZERO_COUNT .NE. 0) L1_SR_HEADER.ZERO_SWICS_PHA(SPIN/32+1) =
     &	      IBSET( L1_SR_HEADER.ZERO_SWICS_PHA(SPIN/32+1), MOD(SPIN,32) ) 
          ENDIF
C
C If there are any PHA words scan them for any zeros
C If a zero PHA word is found (an illegal value) set the corresponding bit
C in the bit array ZERO_MASS_PHA
C
	  IF (SCI(1).EDB(SPIN).MASS_Len .NE. 0) THEN
	    CALL SCAN_ZERO_PHA( MASS_PHA(1).EDB(SPIN).PHA, 
     &				SCI(1).EDB(SPIN).MASS_Len*2,
     &				2, ZERO_COUNT, WHERE )
	    IF (ZERO_COUNT .NE. 0) L1_SR_HEADER.ZERO_MASS_PHA(SPIN/32+1) =
     &	      IBSET( L1_SR_HEADER.ZERO_MASS_PHA(SPIN/32+1), MOD(SPIN,32) ) 
          ENDIF
C
C If there are any PHA words scan them for any zeros
C If a zero PHA word is found (an illegal value) set the corresponding bit
C in the bit array ZERO_STICS_PHA
C
	  IF (SCI(1).EDB(SPIN).STICS_Len .NE. 0) THEN
	    CALL SCAN_ZERO_PHA( STICS_PHA(1).EDB(SPIN).PHA, 
     &				SCI(1).EDB(SPIN).STICS_Len*4,
     &				4, ZERO_COUNT, WHERE )
	    IF (ZERO_COUNT .NE. 0) L1_SR_HEADER.ZERO_STICS_PHA(SPIN/32+1) =
     &	      IBSET( L1_SR_HEADER.ZERO_STICS_PHA(SPIN/32+1), MOD(SPIN,32) ) 
          ENDIF
C
C Set FIRST_VALID if this spin has good quality, but only do it once, i.e.
C FIRST_VALID points to the first good spin in the science record
C
          IF(FIRST_VALID.EQ.-1)FIRST_VALID=SPIN
        ENDIF
 100  CONTINUE
C
C Construct the Science Record counter as given in the HDB
C
      SRCOUNT = HDB(1).BDH.DAT( 5 )
      SRCOUNT = SRCOUNT .AND. 'FF'X
      TEMP    = HDB(1).BDH.DAT( 4 )
      SRCOUNT = IOR( ISHFT( (TEMP .AND. 'FF'X), 8 ), SRCOUNT )
      TEMP    = HDB(1).BDH.DAT( 3 )
      SRCOUNT = IOR( ISHFT( (TEMP .AND. 'FF'X),16 ), SRCOUNT )
      L1_SR_HEADER.SR_COUNT = SRCOUNT
C
C If FIRST_VALID is -1, skip what follows, because there are no valid spins in
C this science record. Also, if the epoch data is not available at processing
C time, skip what follows.
C
      IF((FIRST_VALID.NE.-1).AND.(EPH_AVAIL))THEN
C
C Since EPOCH and EPH_EPOCH are monotonically increasing, we don't need to reset
C EPH_INDEX each time.
C
c	type 204,'looking for match to time ',SCI(1).EDB(FIRST_VALID).EPOCH 
c204	format(1x,a,f20.0)
        DO 200 WHILE( (EPH_INDEX .LE. MAX_EPH_INDEX) .AND.
     &                (SCI(1).EDB(FIRST_VALID).EPOCH .GE. EPH_EPOCH(EPH_INDEX)))
C	  TYPE 201,EPH_INDEX,EPH_EPOCH(EPH_INDEX)
C201	  FORMAT(1X,I3,2X,F20.0)
          EPH_INDEX = EPH_INDEX + 1
 200    CONTINUE
	EPH_INDEX = MAX(EPH_INDEX - 1, 1)
c	type 203,eph_index,EPH_EPOCH(EPH_INDEX)
c203	format(1x,'Found matching time at index ',i3,', value = ',f20.0)
C
        L1_SR_HEADER.GCI_POS = GCI_POS(EPH_INDEX)
        L1_SR_HEADER.GCI_VEL = GCI_VEL(EPH_INDEX)
        L1_SR_HEADER.GSE_POS = GSE_POS(EPH_INDEX)
        L1_SR_HEADER.GSE_VEL = GSE_VEL(EPH_INDEX)
        L1_SR_HEADER.GSM_POS = GSM_POS(EPH_INDEX)
        L1_SR_HEADER.GSM_VEL = GSM_VEL(EPH_INDEX)
        L1_SR_HEADER.GCI_SUN_VECTOR = GCI_SUN_VECTOR(EPH_INDEX)
        L1_SR_HEADER.SPIN_RATE = SPIN_RATE(EPH_INDEX)
        L1_SR_HEADER.GCI_ATT = GCI_ATT(EPH_INDEX)
        L1_SR_HEADER.GSE_ATT = GSE_ATT(EPH_INDEX)
        L1_SR_HEADER.GSM_ATT = GSM_ATT(EPH_INDEX)
      ENDIF
C
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	SUBROUTINE DOY_TO_MONTH_DAY( YEAR, DOY, MONTH, DAY, DATE_STR )
C+
C 
C FUNCTIONAL DESCRIPTION:	
C 
C    This routine converts a given day of year to a (numeric) month and day.
C    The `YEAR' argument is needed to distinguish leap from non-leap years.  In
C    addition, the argument `DATE_STR' is filled with a formatted date string of
C    the form DD-MMM-YYYY.
C 
C FORMAL PARAMETERS:
C  
C     YEAR:
C	  An INTEGER*4 variable containing the year of the date.
C      
C     DOY:
C	  An INTEGER*4 variable containing the day of the year.
C      
C     MONTH:
C	  An INTEGER*4 variable which will receive the computed month (1
C	  origin).
C      
C     DAY:
C	  An INTEGER*4 variable which will receive the computed day-of-month.
C      
C     DATE_STR:
C	  A CHARACTER*11 variable which will receive a formatted date string of
C	  the form DD-MMM-YYYY, where DD is the number day of the month (with
C	  leading blank if necessary) and MMM is the month's 3-letter
C	  abbreviation. 
C  
C COMMON BLOCKS:
C  
C     None.
C  
C DESIGN:
C  
C     Scan array of days-in-year-to-date, locate month whose beginning
C     day-of-year is the largest one less than the given day-of-year.
C     Subtract this value from given day-of-year to get day-of-month.
C     Take into account leap years by using alternate set of values.
C  
C 
C This version:   9-FEB-1995 
C 
C Created: 
C 
C	 14-OCT-1994    L. Bleau
C 
C Revised:
C 
C	 Date     |    Name    | Description
C ----------------+------------+-----------------------------------------------------
C     17-OCT-1994 | J.PAQUETTE | Altered logic to determine MONTH and DAY
C
C-
	IMPLICIT NONE
	INTEGER*4 YEAR, DOY, MONTH, DAY
	CHARACTER*11 DATE_STR
C
	CHARACTER*3 MONTH_ABBREV(12)
	INTEGER MONTHS_END(12,2),LEAP
	DATA MONTH_ABBREV/'JAN','FEB','MAR','APR','MAY','JUN',
     &			  'JUL','AUG','SEP','OCT','NOV','DEC'/
	DATA MONTHS_END/31,59,90,120,151,181,212,243,273,304,334,365,
     &	                31,60,91,121,152,182,213,244,274,305,335,366/
C
	LEAP = 1                                                     ! Not a leap year
	IF (MOD(YEAR,4) .EQ. 0 .AND. MOD(YEAR,100) .NE. 0) LEAP = 2  ! Leap year
C
C Test for error condition
C
        IF((DOY.LT.1).OR.(DOY.GT.364+LEAP))THEN
          DAY   = -1
          MONTH = -1
          RETURN
        ENDIF
C
	DO MONTH=1,12
	  IF (DOY .LE. MONTHS_END(MONTH,LEAP)) GOTO 100
	END DO
C
100	IF(MONTH.EQ.1)THEN
C
C If this is the first month, the day-of-year and day-of-month are the same.
C
          DAY=DOY
        ELSE
C
C If this is not the first month, subtract the day-of-year on which the previous 
C month ended to get the day-of-month.
C
          DAY = DOY - MONTHS_END(MONTH - 1,LEAP)        ! Subtract DOY of
        ENDIF
C
	WRITE(DATE_STR,1001) DAY,MONTH_ABBREV(MONTH),YEAR
1001  	FORMAT(I2,1H-,A3,1H-,I4)
C
	RETURN
	END

	SUBROUTINE GET_CDT(filename,ascii_date,day_of_year)
C+
C 
C FUNCTIONAL DESCRIPTION:       
C 
C  This subroutine obtains the creation date of the specified file in ASCII
C  format (VMS external format, 23 characters).  It also computes the day of
C  the year for that date.
C
C FORMAL PARAMETERS:
C  
C     filename:
C	  A CHARACTER variable containing the name of the file whose creation
C	  date is desired.
C
C     ascii_date:
C	  A CHARACTER*23 variable array which, upon return, will contain the
C	  creation date in VMS external date format (NN-MMM-YYYY HH:MM:SS.HH).
C
C     day_of_year:
C	  An INTEGER*4 variable which, upon return, will contain the day of
C	  year of the creation date.
C  
C COMMON BLOCKS:
C  
C   None
C  
C DESIGN:
C  
C   Use routine GET_CREATE_DATE to do the hard work.  Use library routine
C   LIB$CONVERT_DATE_STRING to obtain the internal time format for the first
C   day of the year in which the file was created.  Now call LIB$DAY twice to
C   get the number of days from November 17, 1858 (which DEC uses as zero
C   origin) to the 1) start of the year in which the file was created, and
C   2) to the date the file was created.  Subtract these two and add one to get
C   the day of year value. 
C  
C This version:   1.0    15-MAR-1995
C 
C Created: 
C 
C	15-MAR-1995	Larry Bleau
C 
C-
	IMPLICIT NONE
	CHARACTER*(*) filename,ascii_date
	INTEGER*4 day_of_year
C
	CHARACTER year_start*11
	INTEGER*4 ios,quad_date(2),year_start_internal(2)
	INTEGER*2 ndays1,ndays2
C
C Get the creation date in quadword (internal) format and in ASCII (external)
C format
C
	CALL GET_CREATE_DATE(filename,quad_date,ascii_date)
D	type *,'Creation: ',ascii_date
D	type *,'Year:     ',ascii_date(8:11)
C
C Compute day of year.
C First get quadword time for first day of year.  Then get number of days from
C November 17, 1858 to start of year and to file's creation date.  Difference
C the two to get day of year file was created.
C NOTE: Since the value returned by LIB$DAY is an *unsigned* 16-bit quantity
C just TYPEing it out may produce a negative value.  The arithmetic still
C works, though.
C
	year_start = ' 1-JAN-' // ascii_date(8:11)
	CALL LIB$CONVERT_DATE_STRING (year_start, year_start_internal,,,,)
	CALL LIB$DAY (ndays1, year_start_internal, )
	CALL LIB$DAY (ndays2, quad_date, )
D	type *,ndays1,ndays2
	day_of_year = ndays2 - ndays1 + 1
D	type *,'Doy:      ',day_of_year
	RETURN
	END

	SUBROUTINE get_create_date(filename,quad_date,ascii_date)
C+
C 
C FUNCTIONAL DESCRIPTION:       
C 
C  This routine obtains the creation date of the specified file and returns it
C  to the caller in two formats: quadword (VMS internal) and ASCII (VMS external,
C  23 characters).
C
C FORMAL PARAMETERS:
C
C     filename:
C	  A CHARACTER variable containing the name of the file whose creation
C	  date is desired.
C
C     quad_date:
C	  An INTEGER*4 array of length 2 (8 bytes total) which, upon return,
C	  will contain the creation date in VMS internal date format.
C
C     ascii_date:
C	  A CHARACTER*23 variable array which, upon return, will contain the
C	  creation date in VMS external date format (NN-MMM-YYYY HH:MM:SS.HH).
C  
C COMMON BLOCKS:
C
C   GETDATEINFO     contains the XAB data structure which holds the creation
C		    date
C
C DESIGN:
C  
C   Do an OPEN with USEROPEN=MYOPEN so the routine MYOPEN gets called.  It
C   grabs the creation date and puts it in common block GETDATEINFO.  Close the
C   file as we don't need to access it any more.  Copy the quadword time into
C   the caller's argument.  Also translate it to ASCII and put it into the
C   third argument.
C  
C This version:   1.0    15-MAR-1995
C 
C Created: 
C 
C	15-MAR-1995	Larry Bleau
C			Algorithm provided by William P. Bame (bbame@trauma.ab.umd.edu)
C-
	IMPLICIT NONE
	CHARACTER filename*(*),ascii_date*(*)
	INTEGER*4 myopen,quad_date(2)
C
	INTEGER*4 quadword(2),lun
	INTEGER*2 nbytes/8/
	INCLUDE '($XABDEF)'
	INCLUDE '($XABDATDEF)'
	EXTERNAL MYOPEN
C
	STRUCTURE /XABUNION/
	  UNION
	    MAP
	      RECORD /XABDEF/ XAB
	    END MAP
	    MAP
	      RECORD /XABDATDEF/ XABDAT
	    END MAP
	  END UNION
	END STRUCTURE
	RECORD /XABUNION/ xabu
C
	COMMON /GETDATEINFO/ xabu
C
C Open file so useropen routine MYOPEN gets called, then close it again as we
C don't need it.  Use a temporary unit number (one returned by LIB$GET_LUN)
C since we don't know what's open or not. 
C
	call LIB$GET_LUN(lun)
	OPEN(UNIT=lun,FILE=filename,STATUS='OLD',ERR=300,
     *	     READONLY,USEROPEN=myopen)
	CLOSE(lun)
	call LIB$FREE_LUN(lun)
C
C Copy the quadword date-time to the caller's argument.
C Also translate the time into ASCII format for caller.
C
	CALL LIB$MOVC3 (nbytes, xabu.xabdat.xab$q_cdt, quad_date)
	CALL SYS$ASCTIM(,ascii_date,quad_date,)
	RETURN
C
C Come here if we got an error on the open attempt.  Set the date to zero and
C the ascii string to blanks and return.
C
300	quad_date(1) = 0
	quad_date(2) = 0
	ascii_date = ' '
	RETURN
	END

	INTEGER*4 FUNCTION MYOPEN(FAB,RAB,LUN)
C+
C 
C FUNCTIONAL DESCRIPTION:       
C 
C   This routine obtains a file's creation date and makes it available in the
C   common block GETDATEINFO.
C
C   This is a useropen routine.  It is not called directly.  Rather, it is
C   called when Fortran does an OPEN on a file and this routine name is
C   specified by the USEROPEN= keyword. 
C
C FORMAL PARAMETERS:
C  
C     FAB:
C	  A structure variable of type FABDEF containing the File Access Block
C	  data structure of the file to be opened.
C  
C     RAB:
C	  A structure variable of type RABDEF containing the Record Access Block
C	  data structure of the file to be opened.
C      
C     LUN:
C	  An INTEGER*4 variable containing the Fortran unit number of the file
C	  to be opened.
C  
C COMMON BLOCKS:
C  
C   GETDATEINFO     contains the XAB data structure which holds the creation
C		    date
C  
C DESIGN:
C  
C   This routine must complete the open operation by calling the SYS$OPEN and
C   SYS$CONNECT system services.  Since the only purpose of this routine is to
C   obtain the file's creation date no call to SYS$CONNECT is done and the
C   caller closes the file.
C
C   The creation date is obtained by linking an XAB (eXtended Attribute BLock)
C   data structure to the list of XABs pointed to by field FAB$L_XAB of the FAB
C   structure.  The new XAB is filled in with a code telling SYS$OPEN to store
C   the file's creation date in the XAB.  
C  
C This version:   1.0    15-MAR-1995
C 
C Created: 
C 
C	15-MAR-1995	Larry Bleau
C			Algorithm provided by William P. Bame (bbame@trauma.ab.umd.edu)
C-
	IMPLICIT NONE
	INCLUDE '($FABDEF)'
	INCLUDE '($RABDEF)'
	INCLUDE '($XABDEF)'
	INCLUDE '($XABDATDEF)'
	RECORD /FABDEF/ FAB
	RECORD /RABDEF/ RAB
	INTEGER*4 LUN
C
	STRUCTURE /XABUNION/
	  UNION
	    MAP
	      RECORD /XABDEF/ XAB
	    END MAP
	    MAP
	      RECORD /XABDATDEF/ XABDAT
	    END MAP
	  END UNION
	END STRUCTURE
	RECORD /XABUNION/ XABU
C
	COMMON /GETDATEINFO/ XABU
C
	INTEGER*4 SYS$OPEN,SAVE
C
	SAVE = FAB.FAB$L_XAB			!\ Insert my XAB at the start
	FAB.FAB$L_XAB = %LOC(XABU)		!/ of the XAB chain.

	XABU.XAB.XAB$B_COD = XAB$C_DAT		!\ Tell RMS that I want the
	XABU.XAB.XAB$B_BLN = XAB$C_DATLEN	!/ XABDAT stuff.

	XABU.XAB.XAB$L_NXT = SAVE		! Link in the original XAB
						! chain.

	MYOPEN = SYS$OPEN(FAB)			! Do the $OPEN, note that
						! this example doesn't do
						! a $CONNECT; not needed since
						! we're going to close the file
						! immediately anyway
	IF (MYOPEN) THEN
	ENDIF
	RETURN
	END
