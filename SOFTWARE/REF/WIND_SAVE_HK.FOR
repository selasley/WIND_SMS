      SUBROUTINE WIND_SAVE_HK ( Major_Frame_Count, SET_NO, SYNC_WORD )
C
C  This version:   1.8   23-FEB-1995
C
C       This subroutine extracts the housekeeping data from the LZ-file
C       and builds the HK array including the qual-flag for every value.
C       This routine should be called whenever a new major frame is
C       accessed.
C       2 sets of housekeeping data are maintained, currently set #2 is
C       the older one.
C
C  USAGE and PARAMETERS:
C       CALL WIND_SAVE_HK( MajorFrame_Count, SET_NO )
C
C               MajorFrame_Count        = NEW major frame counter
C               SET_NO                  = 0 -> MAX_HK 
C
C  CREATION:
C       16-FEB-1994     H.BROER
C  REVISIONS:
C       25-MAY-1994     H.BROER  UMD    new HK.INC
C       26-MAY-1994     H.BROER  UMD    save previous dataset in other set
C        9-JUN-1994     H.BROER  UMD    changed HK.inc to EDB.inc
C       29-JUN-1994                     new structures
C        9-SEP-1994     L.Bleau         altered HK structure; changed
C                                       references to HKset and Hkeep to array
C                                       references using EDBset as subscript
C       13-SEP-1994     L.Bleau         added line to save SET_NO (number of
C                                       valid HKs) in HKcount field of structure
C       27-OCT-1994     L. Bleau        made WRITE statements to KB conditional,
C                                       added common block DIAG, added IMPLICIT
C                                       NONE
C        4-NOV-1994     L. Bleau        write message if more than 5 HKs instead
C                                       of doing a STOP
C        7-NOV-1994     J. PAQUETTE     allow the writing of HK data when the 
C                                       experiment DPU is off
C	18-NOV-1994	L. Bleau	add extra WRITE statement
C       13-DEC-1994     L. Bleau        move SET_NO=1 to after L1 record is
C                                       written (ELSE part of IF), otherwise we
C                                       lose HKs if we somehow get more than 5;
C					change max number of HKs to MAX_HK,
C					which is a PARAMETER currently set to 6
C	13-JAN-1995	L. Bleau	change call to WIND_STORE_L1 to
C					WIND_OUTPUT_L1 so that L1 records will
C					be output even if only HKs are present
C                                 NOTE: This change did not completely correct 
C                                       the problem it was intended to fix
C	 9-FEB-1995	L. Bleau	change include stmt to use EDBDEF.INC
C					and EDBVARS.INC instead of EDB.INC only
C	23-FEB-1995	L. Bleau	removed common DIAg, added INCLUDE
C					DIAG.INC
C 1.8	23-FEB-1995	L. Bleau	modified to dump HK if in PRINTLZ mode
CDEC$ IDENT   '1.8'

      IMPLICIT NONE

      INCLUDE   'INC_DIR:EDBDEF.INC/NOLIST'
      INCLUDE   'INC_DIR:EDBVARS.INC/NOLIST'
      INCLUDE   'INC_DIR:L0_DATA_Record.INC/NOLIST'
      INCLUDE   'INC_DIR:WIND_LUN.INC/NOLIST'
      INCLUDE   'INC_DIR:DIAG.INC/NOLIST'
      INCLUDE   '($SSDEF)/NOLIST'

      INTEGER*4         II,                     ! internal pointer
     +                  IJ,                     ! internal pointer
     +                  ISTAT,                  ! FUNCTION STATUS
     +                  JJ,                     ! internal pointer
     +                  Major_Frame_Count,      ! Input Variable
     +                  SET_NO,                 ! number of HKs
     +                  jjj
      LOGICAL           SYNC_WORD

      SET_NO = SET_NO + 1
      IF( SET_NO .GT. MAX_HK ) THEN
        SET_NO=MAX_HK
        HKeep( EDBset ).HKcount = SET_NO
C
C If we have more than MAX_HK HK's and we have found a sync word, then we have
C an error condition; inform the user.  MAX_HK is currently 6.
C
        IF(SYNC_WORD)THEN
          WRITE( TTOUT, *) '*** ERROR: MORE THAN ',MAX_HK,' HKs; SET_NO=',SET_NO
          WRITE( TTOUT, *) ' '
C
C If we have more than MAX_HK HK's and we have not found a sync word, then
C there is HK data, but no science data; the DPU is off.  Flag the rest of the
C current science record as bad, then output the data in SCI(1).  Since we call
C the same output routine as used by WIND_CD_READ all the proper operations
C will get done.
C Remember to reset the HK counter.  
C Don't call WIND_RESET_HK since there are no EDBs with which to compare times.
C
        ELSE
          CALL WIND_DPU_OFF
          IF (PRINTLZ_MODE) WRITE(28,9995) (HKeep( EDBset ).HKSET( SET_NO ).HK(jjj).BVAL,JJJ=1,75)
 9995     FORMAT(' HK',/,(1X,20Z3.2))
          CALL WIND_OUTPUT_L1
          SET_NO = 1
        ENDIF
      ENDIF
C
      HKeep( EDBset ).HKcount = SET_NO

      DO JJ = 1, 75
          II    = HKLOCI( JJ )
          IJ    = HKLOCJ( JJ )
          HKeep( EDBset ).HKSET( SET_NO ).HK(JJ).BVAL   = DR_MinFr_SMS( II, IJ )
          HKeep( EDBset ).HKSET( SET_NO ).HK(JJ).QUAL   = DR_MinFr_QUAL( IJ )
      END DO

      HKeep( EDBset ).HKSET( SET_NO ).EPOCH     = DR_BEG_EPOCH
      HKeep( EDBset ).HKSET( SET_NO ).SC_EPOCH  = DR_SC_EPOCH
      CALL EPOCH_TO_ITIME( DR_BEG_EPOCH, HKeep( EDBset ).HKSET( SET_NO ).W_Time, ISTAT )
      IF( ISTAT .NE. SS$_NORMAL )       STOP 0902
      CALL EPOCH_TO_ITIME( DR_SC_EPOCH, HKeep( EDBset ).HKSET( SET_NO ).SC_Time, ISTAT )
      IF( ISTAT .NE. SS$_NORMAL )       STOP 0903

      IF(DIAG .LE. 1) WRITE( 20, 197 )  HKeep( EDBset ).HKSET( SET_NO ).EPOCH,
     :                  HKeep( EDBset ).HKSET( SET_NO ).SC_EPOCH,
     :                  HKeep( EDBset ).HKSET( SET_NO ).W_Time.YEAR,
     :                  HKeep( EDBset ).HKSET( SET_NO ).SC_Time.YEAR,
     :                  HKeep( EDBset ).HKSET( SET_NO ).W_Time.DOY,
     :                  HKeep( EDBset ).HKSET( SET_NO ).SC_Time.DOY,
     :                  HKeep( EDBset ).HKSET( SET_NO ).W_Time.HR,
     :                  HKeep( EDBset ).HKSET( SET_NO ).SC_Time.HR,
     :                  HKeep( EDBset ).HKSET( SET_NO ).W_Time.MIN,
     :                  HKeep( EDBset ).HKSET( SET_NO ).SC_Time.MIN,
     :                  HKeep( EDBset ).HKSET( SET_NO ).W_Time.SEC,
     :                  HKeep( EDBset ).HKSET( SET_NO ).SC_Time.SEC,
     :                  HKeep( EDBset ).HKSET( SET_NO ).W_Time.CSEC,
     :                  HKeep( EDBset ).HKSET( SET_NO ).SC_Time.CSEC,
     :                  Major_Frame_Count, SET_NO,
     :                  ( HKeep( EDBset ).HKSET( SET_NO ).HK(JJ).BVAL, JJ = 1, 75 )
  197 FORMAT( 1H0, ' HOUSE KEEPING *** TIME:', 2F20.3 /,
     :                  T19,'YEAR:', I16, I20, /,
     :                  T20,'DOY:',  I16, I20, /,
     :                  T19,'HOUR:', I16, I20, /,
     :                  T16,'MINUTES:',I16,I20,/,
     :                  T16,'SECONDS:',I16,I20,/,
     :                  T21, 'cs:',  I16, I20, /,
     :                  ' MAJOR FRAME COUNT =', I11, ',  SET_NO =', I3, 
     :                  /, 1X, 5Z4.2, /, 7( /, 1X, 5Z4.2, 3X, 5Z4.2 ) ) 

      IF(DIAG .LE. 1) WRITE( 19, 198 )  Major_Frame_Count, SET_NO,
     :                  HKeep( EDBset ).HKSET( SET_NO ).EPOCH,
     :                  HKeep( EDBset ).HKSET( SET_NO ).W_Time.YEAR,
     :                  HKeep( EDBset ).HKSET( SET_NO ).W_Time.DOY,
     :                  HKeep( EDBset ).HKSET( SET_NO ).W_Time.HR,
     :                  HKeep( EDBset ).HKSET( SET_NO ).W_Time.MIN,
     :                  HKeep( EDBset ).HKSET( SET_NO ).W_Time.SEC,
     :                  HKeep( EDBset ).HKSET( SET_NO ).W_Time.CSEC,
     :                  HKeep( EDBset ).HKSET( SET_NO ).SC_EPOCH,
     :                  HKeep( EDBset ).HKSET( SET_NO ).SC_Time.YEAR,
     :                  HKeep( EDBset ).HKSET( SET_NO ).SC_Time.DOY,
     :                  HKeep( EDBset ).HKSET( SET_NO ).SC_Time.HR,
     :                  HKeep( EDBset ).HKSET( SET_NO ).SC_Time.MIN,
     :                  HKeep( EDBset ).HKSET( SET_NO ).SC_Time.SEC,
     :                  HKeep( EDBset ).HKSET( SET_NO ).SC_Time.CSEC
  198 FORMAT( 1H0, ' HOUSE KEEPING *** MAJOR FRAME COUNT =',I11,',  SET_NO =', I3,  /,
     :                  1X,F20.3,5X,6I5/1X,F20.3,5X,6I5)
      IF(DIAG .LE. 2) WRITE(19,*) 'SAVE_HK: HKSET ARRAY=',(HKSET(II),II=1,3)
      RETURN

      END
