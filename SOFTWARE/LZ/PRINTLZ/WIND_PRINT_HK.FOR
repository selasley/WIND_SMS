      SUBROUTINE WIND_PRINT_HK ( Major_Frame_Count, SET_NO, SYNC_WORD )
C
C  This version:   1.0   25-JAN-1995
C
C       This subroutine (taken from WIND_SAVE_HK) extracts the housekeeping 
C       data from the LZ-file and builds the HK array including the quality
C       flag for every value.
C       This routine should be called by PRINTLZ whenever a new major frame is
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
C       25-JAN-1995     J.PAQUETTE
C  REVISIONS:
C
CDEC$ IDENT   '1.0'

      IMPLICIT NONE

      INCLUDE   'INC_DIR:EDB.INC/NOLIST'
      INCLUDE   'INC_DIR:L0_DATA_Record.INC/NOLIST'
      INCLUDE   'INC_DIR:WIND_LUN.INC/NOLIST'
      INCLUDE   '($SSDEF)/NOLIST'

      INTEGER*4         II,                     ! internal pointer
     +                  IJ,                     ! internal pointer
     +                  ISTAT,                  ! FUNCTION STATUS
     +                  JJ,                     ! internal pointer
     +                  Major_Frame_Count,      ! Input Variable
     +                  SET_NO,                  ! number of HKs
     +                  jjj
      LOGICAL           SYNC_WORD

      INTEGER DIAG
      COMMON /DIAG/ DIAG

      SET_NO = SET_NO + 1
      IF( SET_NO .GT. MAX_HK ) THEN
        SET_NO=MAX_HK
        HKeep( EDBset ).HKcount = SET_NO
C
C If we have more than MAX_HK HK's and we have found a sync word, then we have an error
C condition; inform the user.  MAX_HK is currently 6.
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
          WRITE(28,9995) (HKeep( EDBset ).HKSET( SET_NO ).HK(jjj).BVAL,JJJ=1,75)
 9995     FORMAT(' HK',/,(1X,20Z3.2))
          CALL WIND_MOVE_DOWN_L1
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
