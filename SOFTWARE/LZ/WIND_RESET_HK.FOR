      SUBROUTINE WIND_RESET_HK( sr_index )
C
C	Check if second HK is in sync with this science record.  If it is,
C	remove the first HK and adjust the HK counter.
C	Also store last HK block from this Science Record into HK block area
C	for next Science Record
C
C     Parameters:
C	sr_index  INTEGER giving index of Science Record to examine for HK
C		synchronicity.  This is one less than the science record which
C		should receive a duplicate copy of this record's last HK. 
C
C     This version:   1.9   12-APR-1995
C
C     Creation:
C	L. Bleau  9-SEP-1994
C
C     Revisions:
C	16-SEP-1994	L.Bleau		added code to check if 1st HK is dup-
C					licate and remove it if it is; change
C					meaning of parameter to which sci rec
C					to operate upon
C	20-SEP-1994	L.Bleau		made removal of first HK contingent
C					upon there being more than one HK
C       27-OCT-1994     L. Bleau        made WRITE statements to KB conditional,
C                                       added common block DIAG
C       27-OCT-1994     L. Bleau        changed name L1_sci_rec_cntr to sci_rec_cntr
C        4-NOV-1994     L. Bleau        change DO loop index to go no higher
C                                       than 5
C	13-DEC-1994	L. Bleau	replace max number of HKs (was 5) with
C					parameter MAX_HK (now 6)
C 1.7	13-JAN-1995	L. Bleau	set TIMEDIFF to 0.0 if EDB(0).SC_EPOCH
C					is zero; see comments for more details
C	 9-FEB-1995	L. Bleau	change comment delimiters
C	 9-FEB-1995	L. Bleau	change include stmt to use EDBDEF.INC
C					and EDBVARS.INC instead of EDB.INC only
C 1.8	23-FEB-1995	L. Bleau	removed common DIAG, added INCLUDE
C					DIAG.INC
C 1.9   12-APR-1995	L. Bleau	add include L1INFO.INC
CDEC$ IDENT   '1.9'

      IMPLICIT NONE
 
      INCLUDE	'INC_DIR:EDBDEF.INC/NOLIST'
      INCLUDE	'INC_DIR:EDBVARS.INC/NOLIST'
      INCLUDE	'INC_DIR:L1INFO.INC/NOLIST'
      INCLUDE	'INC_DIR:DIAG.INC/NOLIST'
 
      INTEGER sr_index
      INTEGER hk_index, II
      REAL*8 TIMEDIFF
C
C Science record sr_index has the completed copy of the most recent
C science record.  It may also have an extra HK at its beginning which should
C not have been duplicated from the previous science record.
C
C This routine examines this science record and compares the times of the EDB
C and the second HK (the first HK was duplicated from the previous science
C record and is guaranteed to have a much earlier time stamp).  If the times
C reasonably match (within 1 sec) remove the first HK by copying the rest of the
C HKs down one position.  Remember to adjust the counter of number of valid HKs
C (HKset(sr_index)).
C
C We're safe accessing science record sr_index+1, by the way, since we are
C either called with a 1 or a 2 as an argument, so sr_index+1 will never be
C larger than 3, which is the number of science records kept in memory.
C
C The EDB's and the HK's are asynchronous.  Because of this, the last HK of the
C previous Science Record is copied to the first HK of the current Science
C Record by this routine.  However, as time goes on, the start times of the HK
C and the EDB will periodically come into sync for one Science Record. If the
C start times of the HK and the EDB are nearly identical, then ensure that the
C last HK of the previous science record is not repeated in this science
C record.  This is done by copying the HK records down one position and
C decrementing HK count.  If, however, for some strange reason, there is only
C one HK in the sr_index'th science record, leave it alone.
C
C NOTE: If there is no science data the SC_EPOCH field of EDB(0) will have been
C zeroed, resulting in a large value for TIMEDIFF.  This will cause an HK to
C be duplicated, which is not what is wanted.  Set TIMEDIFF to 0.0 to force the
C duplicate HK to be overwritten by later HKs.
C
      TIMEDIFF = ABS(SCI(sr_index).EDB(0).SC_EPOCH - HKeep(sr_index).HKset(2).SC_EPOCH)
      IF (SCI(sr_index).EDB(0).SC_EPOCH .EQ. 0.0) TIMEDIFF = 0.0
      IF(DIAG .LE. 1) write(19,*) 'RESET_HK: edb,hk times=',sci(sr_index).edb(0).sc_epoch,hkeep(sr_index).hkset(2).sc_epoch
      IF(DIAG .LE. 1) write(19,*) 'RESET_HK: edb,hk time diff=',TIMEDIFF
 
      IF(TIMEDIFF .LT. 1000.0 .AND. HKset(sr_index) .GT. 1) THEN
 
        DO hk_index=2,MIN( HKset(sr_index), MAX_HK)
          HKeep( sr_index ).HKSET(hk_index-1).W_TIME   = HKeep( sr_index ).HKSET(hk_index).W_TIME
          HKeep( sr_index ).HKSET(hk_index-1).SC_TIME  = HKeep( sr_index ).HKSET(hk_index).SC_TIME
          HKeep( sr_index ).HKSET(hk_index-1).EPOCH    = HKeep( sr_index ).HKSET(hk_index).EPOCH
          HKeep( sr_index ).HKSET(hk_index-1).SC_EPOCH = HKeep( sr_index ).HKSET(hk_index).SC_EPOCH
          DO II = 1, 75
            HKeep( sr_index ).HKSET(hk_index-1).HK(II) = HKeep( sr_index ).HKSET(hk_index).HK(II)
          END DO
        END DO
        IF(DIAG .LE. 1) WRITE(19,*) ' DECREMENTING HKset; was ',HKset(sr_index)
        HKset( sr_index ) = HKset(sr_index) - 1
        HKeep( sr_index ).HKcount = HKset( sr_index )
      ENDIF
C 
C At this point we have a consistent science record with no duplicate HKs in the
C science record pointed to by sr_index.  The record at index sr_index+1 was
C recently vacated by a copy operation which the caller did and will be filled
C in later on.  Since we don't know at this point if the next EDB and next HK
C will be synchronous, we'll assume they will not be and save the last HK of the
C just-completed science record into the first HK position of the science record
C being formed.  This routine, when next called, will detect when the HK we
C copied over is actually a duplicate (ie, not needed) and remove it (see
C comments above), so we needn't worry about that just now.
C 
      HKeep( sr_index+1 ).HKSET(1).W_TIME   = HKeep( sr_index ).HKSET(HKset(sr_index)).W_TIME
      HKeep( sr_index+1 ).HKSET(1).SC_TIME  = HKeep( sr_index ).HKSET(HKset(sr_index)).SC_TIME
      HKeep( sr_index+1 ).HKSET(1).EPOCH    = HKeep( sr_index ).HKSET(HKset(sr_index)).EPOCH
      HKeep( sr_index+1 ).HKSET(1).SC_EPOCH = HKeep( sr_index ).HKSET(HKset(sr_index)).SC_EPOCH
      DO II = 1, 75
          HKeep( sr_index+1 ).HKSET(1).HK(II) = HKeep( sr_index ).HKSET(HKset(sr_index)).HK(II)
      END DO
 
      IF(DIAG .LE. 1) WRITE(19,*) ' SCI-REC=',SCI_REC_CNTR
      IF(DIAG .LE. 1) WRITE(19,*) 'RESET_HK: MOVING LAST HK TO NEXT SCI REC, COPIED FROM HK INDEX=',HKset(sr_index)
      HKset(sr_index+1) = 1
      IF(DIAG .LE. 1) write(19,*) 'RESET_HK: HKset array=',(hkset(ii),ii=1,3)
      RETURN
      END
