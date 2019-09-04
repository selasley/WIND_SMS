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
C This version:   1.2   23-FEB-1995
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
C-
CDEC$ IDENT '1.2'
      IMPLICIT NONE

      CHARACTER*(*) LZ_NAME,L1_NAME
      INTEGER NCF
C
      INCLUDE   'INC_DIR:WIND_Lun.INC/NOLIST'
      INCLUDE   'INC_DIR:DIAG.INC/NOLIST'
      INCLUDE   '($SSDEF)/NOLIST'
C
      INTEGER IM,ID,IY,IPOS,IPOS2,MM,CHLAST,ISTAT
C
C Set proper pattern to use to search for input LZ file.
C If we are running in NRT mode point to a different directory.
C
      LZ_NAME = 'WIND_LZ_DATA:WI_LZ_SMS*.DAT'
      IF (NRT_MODE) LZ_NAME = 'NRT_DIR:WI_LZ_SMS*.*'
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
C Using LZ filename, construct L1 filename by replace 'LZ' with 'L1'
C If 'LZ' isn't present file probably comes from CD-ROM and has shorter name.
C Be sure to start search for 'LZ' after the device/directory name.
C In this case construct longer L1 filename using current date.  This behavior
C is to be changed later, though.
C
      IPOS = INDEX( LZ_NAME(1:NCF), ']' )
      IF (IPOS .EQ. 0) IPOS = INDEX( LZ_NAME(1:NCF), ':' )
C If neither found IPOS is a 0
      IPOS = IPOS + 1
      IPOS2 = INDEX( LZ_NAME(IPOS:NCF), 'LZ' )

      IF (IPOS2 .NE. 0) THEN
        L1_NAME = LZ_NAME
        L1_NAME(IPOS+IPOS2:IPOS+IPOS2) = '1'

      ELSE				! no 'LZ' in filename

        CALL IDATE( IM, ID, IY )
        IF (IY .LT. 100) THEN
          IF (IY .GT. 90) THEN
            IY = IY + 1900
          ELSE
            IY = IY + 2000
          ENDIF ! (IY .GT. 90)
        ENDIF ! (IY .LT. 100)

        WRITE( L1_NAME, 112 ) IY,IM,ID
112     FORMAT( 'WI_L1_SMS_',I4.4,2I2.2,'_V01.DAT' )

      ENDIF ! (IPOS .EQ. 0)
C
C If we are in PRINTLZ mode construct dump filename instead
C Name it same as LZ file but with .DMP file type, and put it in the current
C directory, not in WIND_LZ_DATA or WIND_L1_DATA.
C
      IF (PRINTLZ_MODE) THEN
        L1_NAME = LZ_NAME(IPOS:NCF)
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
      IF (.NOT. PRINTLZ_MODE) THEN
        IPOS = INDEX( L1_NAME(1:NCF), ']' )
        IF (IPOS .EQ. 0) IPOS = INDEX( L1_NAME(1:NCF), ':' )
C If neither found IPOS is a 0
        IPOS = IPOS + 1
        IF (.NOT. NRT_MODE) L1_NAME = 'WIND_L1_DATA:' // L1_NAME(IPOS:NCF)
        NCF = CHLAST( L1_NAME )
      ENDIF

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
C Open the L1 file
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

      RETURN
      END
