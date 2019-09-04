      SUBROUTINE WIND_CD_OPEN_FILES(LZ_NAME,L1_NAME,NCF)
C+
C 
C FUNCTIONAL DESCRIPTION:	
C 
C    This routine calls the appropriate subroutines to open the LZ (Level Zero)
C    file for reading and to create and open the L1 (Level 1) file for writing.
C    Its major chore is constructing the proper filenames and search patterns.
C
C 
C FORMAL PARAMETERS:
C  
C     LZ_NAME:
C	  A CHARACTER variable of sufficient size to hold a complete filename,
C         including device and directory spec.  Upon return this parameter
C         will contain the full filename of the input LZ file.
C      
C     L1_NAME:
C	  A CHARACTER variable of sufficient size to hold a complete filename,
C         including device and directory spec.  Upon return this parameter
C         will contain the full filename of the output L1 file.
C      
C     NCF:
C	  An INTEGER*4 variable which will, upon return, contain the number of
C         non-blank characters in L1_NAME.
C  
C COMMON BLOCKS:
C  
C     DIAG
C  
C DESIGN:
C  
C     Use WIND_GET_FILENAME to prompt user for the proper LZ filename.
C     Convert LZ filename into L1 filename in another device/directory.
C     Use WIND_GET_FILENAME to prompt user for the proper L1 filename, using
C     the constructed filename as the default.
C  
C 
C This version:   1.7   12-JUN-1995
C 
C Created: 
C 
C	 17-FEB-1995    Larry Bleau
C 
C Revised:
C 1.1	21-FEB-1995	L. Bleau	fixed bug in LZ to L1 filename change
C	23-FEB-1995	L. Bleau	remove common DIAG, added INCLUDE
C					DIAG.INC
C 1.2	23-FEB-1995	L. Bleau	add code to open to dump file, not L1
C					file, if in PRINTLZ mode
C 1.3	 7-APR-1995	L. Bleau	corrected code constructing L1 filename
C					so it'd work on short names as on CDROM
C 1.4   12-APR-1995     L.Bleau         Ensure L1 file name will be the same 
C                                       regardless of whether LZ file comes from
C                                       CD or disk
C 1.5   12-APR-1995     J.PAQUETTE      added calls to GET_EPHEMERIS, added
C					include L1FMT.INC 
C 1.6	26-APR-1995	L. Bleau	modified CDF file access, use wildcard
C					to search for first orbit/attitude file;
C					conditionally continue if not found;
C					create L1 file at end of subroutine
C 1.7	12-JUN-1995	L. Bleau	added IPOS=1; changed substring
C					reference of L1_NAME to (1:NCF)
C-
CDEC$ IDENT '1.7'
      IMPLICIT NONE

      CHARACTER*(*) LZ_NAME,L1_NAME
      INTEGER NCF
C
      INCLUDE   'INC_DIR:WIND_Lun.INC/NOLIST'
      INCLUDE   'INC_DIR:L1FMT.INC'
      INCLUDE   'INC_DIR:L1_HDR_INFO.INC'
      INCLUDE   'INC_DIR:L1INFO.INC'
      INCLUDE   'INC_DIR:DIAG.INC/NOLIST'
      INCLUDE   'CDF$INC:CDF.INC'
      INCLUDE   '($SSDEF)/NOLIST'
      INCLUDE   '($RMSDEF)/NOLIST'
      INCLUDE   '($SYIDEF)/NOLIST'
      INCLUDE   '($DVIDEF)/NOLIST'
C
      INTEGER IM,ID,IY,IPOS,IPOS2,MM,CHLAST,ISTAT,VERS,NCH,NCOR,NCAT
      INTEGER*4 LIB$FIND_FILE,LIB$FIND_FILE_END,CONTEXT,STATUS
      CHARACTER MEDIA_NAME*10
      CHARACTER OR_NAME*80, AT_NAME*80, OR_PATTERN*80, AT_PATTERN*80
C
C Determine if logical name WIND_LZ_DATA points to a CDROM device or not.
C
      FROM_CD = .FALSE.
      CALL LIB$GETDVI (
     1	   DVI$_MEDIA_NAME,, 
     1	   'WIND_LZ_DATA',, 
     1	   MEDIA_NAME, 
     1	   NCH)                        
      IF (MEDIA_NAME(1:3) .EQ. 'RRD') FROM_CD = .TRUE.
C
C Set proper pattern to use to search for input LZ file.
C If we are running in NRT mode point to a different directory. If we 
C are reading from CD, then alter our name pattern appropriately.
C
      LZ_NAME = 'WIND_LZ_DATA:WI_LZ_SMS*.DAT'
      IF (NRT_MODE) LZ_NAME = 'NRT_DIR:WI_LZ_SMS*.*'
      IF (FROM_CD) LZ_NAME = 'WIND_LZ_DATA:*.DAT'
C
C Prompt user for filename
C
      WRITE( TTOUT, 100 )   'LZ-FILENAME = '
100   FORMAT(1H , A, $)
      CALL WIND_GET_FILENAME( LZ_NAME, ISTAT )
      IF (ISTAT .NE. SS$_NORMAL)   STOP 0001
C
C Open the LZ file
C
      CALL WIND_OPEN_LZ_DATA( LZ_NAME, 'OLD', ISTAT )
C
C Get actual filename and save it
C
      INQUIRE( L0_Lun, NAME=LZ_NAME )
      NCF    = CHLAST( LZ_NAME )
      IF(DIAG .LE. 5) WRITE( TTOUT, 110 )   LZ_NAME(1:NCF), ISTAT
110   FORMAT( 1H , 'OPENED FILE: ', A, ',  STATUS IS', I3 )
      IF (ISTAT .NE. SS$_NORMAL) STOP 0002
C
C Determine if logical name WIND_LZ_DATA points to a CDROM device or not.
C Update value of FROM_CD accordingly, just in case user entered a different
C device than we'd prompted him with.
C
      FROM_CD = .FALSE.
      IPOS = INDEX( LZ_NAME, ':')
      CALL LIB$GETDVI (
     1	   DVI$_MEDIA_NAME,, 
     1	   LZ_NAME(1:IPOS),, 
     1	   MEDIA_NAME, 
     1	   NCH)                        
      IF (MEDIA_NAME(1:3) .EQ. 'RRD') THEN
        FROM_CD = .TRUE.
        NRT_MODE = .FALSE.
      ENDIF
C
C Using LZ filename, construct L1 filename by replacing 'LZ' with 'L1'. If
C 'LZ' isn't present, file probably comes from CD-ROM and has shorter name,
C so construct the longer format filename.  If filename isn't either format
C construct longer L1 filename using the LZ filename with a suffix of 'L1'
C on the filename proper.
C Be sure to start search for 'LZ' after the device/directory name.  
C
      IPOS = INDEX( LZ_NAME(1:NCF), ']' )
      IF (IPOS .EQ. 0) IPOS = INDEX( LZ_NAME(1:NCF), ':' )
C If neither ] nor : found IPOS is a 0
      IPOS = IPOS + 1
      IPOS2 = INDEX( LZ_NAME(IPOS:NCF), 'LZ' )
      IF (IPOS2 .NE. 0) THEN
        L1_NAME = LZ_NAME(IPOS:NCF)
        L1_NAME(4:5) = 'L1'
C
C No 'LZ' in filename, probably has 8-digit filename format.
C Check if it is 8-digit format by trying to decode the first 8 characters of
C the filename.  If this works it's a short filename format; if it fails we
C have no idea.
C
      ELSE
	READ(LZ_NAME(IPOS:IPOS+7), 111, ERR=115) IY,IM,ID,VERS
111	FORMAT(4I2)
C
C Adjust the year to be in the correct century, then encode the date into L1
C filename.  Use 1990 as an arbitrary deciding criterion for the century.  This
C won't work in year 2090 or later.
C
	IF (IY .GT. 90) THEN
	  IY = IY + 1900
	ELSE
	  IY = IY + 2000
	ENDIF
C
        WRITE( L1_NAME, 112 ) IY,IM,ID,VERS
112     FORMAT( 'WI_L1_SMS_',I4.4,2I2.2,'_V',I2.2,'.DAT' )
	IPOS = 1
	GOTO 120
C
C The internal read failed, so we've no idea what the filename format is.  Use
C the LZ filename but add a L1 on its end.
C
115	CONTINUE
        IPOS2 = INDEX( LZ_NAME(IPOS:NCF), '.' )
        L1_NAME = LZ_NAME(IPOS:IPOS+IPOS2-1) // '_L1' // LZ_NAME(IPOS+IPOS2:NCF)
120	CONTINUE
      ENDIF ! (IPOS .EQ. 0)
      NCF = CHLAST(L1_NAME)
C
C If we are in PRINTLZ mode construct dump filename instead.
C Use name in L1_NAME, but change 'L1' to 'LZ' and give it a file type of .DMP
C
      IF (PRINTLZ_MODE) THEN
        IPOS = INDEX( L1_NAME(1:NCF), 'L1' )
        L1_NAME(IPOS:IPOS+1) = 'LZ'
        IPOS = INDEX( L1_NAME, '.' )
        IF (IPOS .EQ. 0) IPOS = INDEX( L1_NAME, ' ' )
        L1_NAME(IPOS:) = '.DMP'
        NCF = IPOS + 3
      ENDIF
C
C Remove explicit version number from filename (starting at ';')
C
      IPOS = INDEX( L1_NAME(1:NCF), ';' )
      IF (IPOS .NE. 0) THEN
        L1_NAME(IPOS:)  = ' '
        NCF = IPOS - 1
      ENDIF
C
C Locate end of directory name (a ']').  If not present, locate end of device
C name (a ':').  If neither is present, set pointer to start of string.
C Remove complete device/directory name and substitute in its place the logical
C string 'WIND_L1_DATA:'.  This is the logical name of the directory which is
C to contain the created L1 file, which may be different from the LZ file's
C device/directory.
C
C If we are in NRT mode don't change output device/directory, generate the NRT
C L1 file in the same place as the LZ NRT file.
C
C If we are in PRINTLZ mode leave the contents of L1_NAME alone as it already
C contains the dump file name
C
      IPOS = INDEX( L1_NAME(1:NCF), ']' )
      IF (IPOS .EQ. 0) IPOS = INDEX( L1_NAME(1:NCF), ':' )
C If neither found IPOS is a 0
      IPOS = IPOS + 1
      IF (.NOT. NRT_MODE) L1_NAME = 'WIND_L1_DATA:' // L1_NAME(IPOS:NCF)
      NCF = CHLAST( L1_NAME )
C
C Determine the name of the first ORbit data file.
C This assumes there is only one version of the orbit file present for this
C date.  If there is more than one this code will use the first file, which may
C not be the one desired.
C
      EPH_AVAIL = .TRUE.
C
      CALL CONSTRUCT_CDF_NAME(LZ_NAME, 'OR', ' ', OR_PATTERN, NCOR)
      OR_PATTERN(NCOR-1:NCOR) = '%%'
      OR_PATTERN(NCOR+1:NCOR+4) = '.CDF'
      CONTEXT = 0
      STATUS = LIB$FIND_FILE (OR_PATTERN(1:NCOR+4), OR_NAME, CONTEXT, , , ,)
      IF (STATUS .EQ. RMS$_NORMAL) THEN	    ! we found a file
        NCOR = INDEX( OR_NAME, '.CDF;' ) - 1
      ELSE
        EPH_AVAIL = .FALSE.
      ENDIF
      STATUS = LIB$FIND_FILE_END(CONTEXT)
C
C Determine the name of the first ATtitude data file
C This assumes there is only one version of the attitude file present for this
C date.  If there is more than one this code will use the first file, which may
C not be the one desired.
C
      CALL CONSTRUCT_CDF_NAME(LZ_NAME, 'AT', ' ', AT_PATTERN, NCAT)
      AT_PATTERN(NCAT-1:NCAT) = '%%'
      AT_PATTERN(NCAT+1:NCAT+4) = '.CDF'
      CONTEXT = 0
      STATUS = LIB$FIND_FILE (AT_PATTERN(1:NCAT+4), AT_NAME, CONTEXT, , , ,)
      IF (STATUS .EQ. RMS$_NORMAL) THEN	    ! we found a file
        NCAT = INDEX( AT_NAME, '.CDF;' ) - 1
      ELSE
        EPH_AVAIL = .FALSE.
      ENDIF
      STATUS = LIB$FIND_FILE_END(CONTEXT)
C
C If both ORbit and ATtitude data files exist get the ephemeris data from them
C
      IF (EPH_AVAIL) THEN
        CALL GET_EPHEMERIS(OR_NAME(1:NCOR),AT_NAME(1:NCAT),ISTAT)
        IF (ISTAT .NE. CDF_OK) EPH_AVAIL = .FALSE.
      ENDIF
      IF (.NOT. EPH_AVAIL) THEN
	IF (HALT_IF_NO_EPH) THEN
          TYPE *,'Ephemeris data unavailable.  The program will abort.'
	  CALL LIB$STOP(%VAL(SS$_NOSUCHFILE))
	ELSE
          TYPE *,'Ephemeris data unavailable.  The program will continue unless interrupted'
	ENDIF
      ENDIF
C
C Display and verify name of L1 file
C
      WRITE( TTOUT, 101 )   'L1-FILENAME = ',L1_NAME(1:NCF)
101   FORMAT(1X,A,A)

      CALL WIND_GET_FILENAME( L1_NAME, ISTAT )
      IF (ISTAT .NE. SS$_NORMAL .AND. ISTAT .NE. 0) CALL LIB$STOP(%VAL(ISTAT))
C
C Remove explicit version number from filename (starting at ';') before opening
C If we don't Fortran OPEN gives error 43 when file already exists
C
      IPOS = INDEX( L1_NAME, ';' )
      IF (IPOS .NE. 0) L1_NAME(IPOS:)=' '
C
C Open the L1 file, creating it
C
C If we are in PRINTLZ mode open the dump file to unit 28
C
C In both cases get complete name of output file and display it
C
      IF (.NOT. PRINTLZ_MODE) THEN
        IF (ISTAT .EQ. SS$_NORMAL) WRITE( TTOUT, * ) 'NEW VERSION OF L1 FILE BEING CREATED'
        CALL WIND_OPEN_L1( L1_NAME, 'NEW', ISTAT )
        IF (ISTAT .NE. SS$_NORMAL) CALL LIB$STOP(%VAL(ISTAT))
        INQUIRE( L1_Lun, NAME=L1_NAME )
      ELSE
        IF (ISTAT .EQ. SS$_NORMAL) WRITE( TTOUT, * ) 'NEW VERSION OF DUMP FILE BEING CREATED'
        OPEN(  UNIT    = 28,
     &         NAME    = L1_NAME,
     &         STATUS  = 'NEW',
     &         RECL    = 132 )

        INQUIRE( 28, NAME=L1_NAME )
      ENDIF

      NCF = CHLAST( L1_NAME )
      IF (DIAG .LE. 5) WRITE( TTOUT, 110 ) L1_NAME(1:NCF), ISTAT
C
      RETURN
      END
