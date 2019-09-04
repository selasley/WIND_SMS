      SUBROUTINE WIND_CD_INIT
C
C     Serves to initialize values for the use of WIND_CD_READ
C
C     This version:   1.12   15-MAR-1995
C
C     Creation:
C     H.BROER   8-APR-1994
C
C     Revisions:
C      9-JUN-1994  H. Broer     update include files
C     29-JUN-1994  H. Broer     revise HKset staring value
C      9-SEP-1994  L. Bleau     change HKset to be array reference
C     21-SEP-1994  L. BLeau     change HDBset initialization to 1
C      4-OCT-1994  L. Bleau     initialize output science L1 record to 1
C     27-OCT-1994  L. Bleau     changed name L1_sci_rec_cntr to sci_rec_cntr
C     27-OCT-1994  L. Bleau     initialized L1_recs_written
C     17-NOV-1994  L. Bleau     add code to translate logical name WIND_DIAG and
C	                        set diagnostic level (DIAG variable)
C 1.5  6-DEC-1994  L. Bleau     add call to JOB_MODE to obtain interactive flag
C 1.6 15-DEC-1994  L. Bleau     initialize EDB counters to 0
C 1.7 10-JAN-1995  L. Bleau  
C 1.8  8-FEB-1995  L. Bleau     add code to translate logical name WIND_NRT_MODE
C			        and set logical variable NRT_MODE if so; change
C			        comment delimiters
C 1.9 15-FEB-1995  J.PAQUETTE   removed refrence to HDBset since HDB is now
C                  L. Bleau     indexed by EDBset
C 1.10 23-FEB-1995 L. Bleau	check program name, set/clear PRINTLZ_MODE flag;
C				remove common DIAG def, add INCLUDE DIAG.INC
C 1.11  8-MAR-1995 L. Bleau	added cal to GET_IMAGE_INFO to get program
C				version, link date/time
C 1.12 15-MAR-1995 L. Bleau	translate logical WIND_COMMENT to fill in file
C				header comment field
C
CDEC$ IDENT '1.12'

      IMPLICIT NONE

      INCLUDE 'INC_DIR:EDBDEF.INC/NOLIST'
      INCLUDE 'INC_DIR:EDBVARS.INC/NOLIST'
      INCLUDE 'INC_DIR:WIND_L0_RECORD.INC/NOLIST'
      INCLUDE 'INC_DIR:WIND_Lun.INC/NOLIST'
      INCLUDE 'INC_DIR:L1FMT.INC/NOLIST'
      INCLUDE 'INC_DIR:DIAG.INC/NOLIST'
C
      INTEGER*4 GET_LOGICAL,ITEMP,NCH,MODE,SPIN,I
      LOGICAL*4 JOB_MODE
      CHARACTER*80 VALUE
C
C     INITIATE VALUES FOR PC-RECORDING
C
      EDBset = 1
      HKset(1) = 0
      Sci_rec_cntr = 1
      L1_recs_written = 0
      EDB_Sum = 0
      EDB_Lost = 0
      EDB_Skip = 0
C
C Translate logical name WIND_DIAG
C If it's defined decode as if an integer and store into common variable DIAG
C
      NCH = GET_LOGICAL( 'WIND_DIAG', VALUE )
      IF (NCH .NE. 0) THEN
        READ( VALUE, 101, ERR=110 ) ITEMP
101     FORMAT(I<NCH>)
        DIAG = ITEMP
110     CONTINUE
      ENDIF
C
C Translate logical name WIND_NRT_MODE
C If it's defined set common variable NRT_MODE to .TRUE., else leave it .FALSE.
C
      NRT_MODE = .FALSE.
      NCH = GET_LOGICAL( 'WIND_NRT_MODE', VALUE )
      IF (NCH .NE. 0) NRT_MODE = .TRUE.
C
C Get job mode
C Returns .TRUE. if interactive job, so store that info in INTER in common
C block DIAG
C
      INTER = JOB_MODE(MODE)
C
C Check program name.  If it is PRINTLZ set the logical variable PRINTLZ_MODE
C to .TRUE.
C
      PRINTLZ_MODE = .FALSE.
      CALL GET_PROGRAM_NAME( VALUE, NCH )
      IF (NCH .NE. 0) THEN
        IF (VALUE(1:7) .EQ. 'PRINTLZ') PRINTLZ_MODE = .TRUE.
      ENDIF
C
C Get date and time program was linked and its version
C
      CALL GET_IMAGE_INFO( 'LINK', LINK_DATE_TIME, NCH )
      CALL GET_IMAGE_INFO( 'VERS', PROG_VERSION, NCH )
C
C Get user-specified comment.  This will be in the logical name WIND_COMMENT.
C
C The comment field can have two sources: a program-generated value and a
C user-specified value.  The user can specify a value by defining the logical
C name WIND_COMMENT before running the program.  WIND_CD_READ can also store
C a comment directly in the file header structure.  The user comment is thus
C overwritten and superceded if WIND_CD_READ stores anything there.  The length
C of the comment field is computed by WIND_BUILD_FILE_HEADER. 
C
      l1_file_header.comment = ' '
      NCH = GET_LOGICAL( 'WIND_COMMENT', VALUE )
      IF (NCH .NE. 0) THEN
        NCH = MIN(LEN(l1_file_header.comment),NCH)
        l1_file_header.comment = VALUE(1:NCH)
      ENDIF
C
C Initialize 4-character header in each of the data structures
C
      DO I=1,3
        DO SPIN=0,59                            
          SWICS_RATE(I).EDB(SPIN).SWRA = 'SWRA'   
          SWICS_PHA(I).EDB(SPIN).SWPH  = 'SWPH'     
          MASS_RATE(I).EDB(SPIN).MARA  = 'MARA'    
          MASS_PHA(I).EDB(SPIN).MAPH   = 'MAPH'      
          STICS_RATE(I).EDB(SPIN).STRA = 'STRA'   
          STICS_PHA(I).EDB(SPIN).STPH  = 'STPH'     
        END DO                                  
        SCI(I).CORE       = 'CORE'
        HDB(I).BDH.CCHDB  = 'HDB '
        HKeep(I).CCHK     = 'HK  '
      END DO

      RETURN
      END
