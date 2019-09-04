	SUBROUTINE WIND_MOVE_DOWN_L1
C
C       This routine is mainly bookkeeping. Taken from the routine WIND_OUTPUT_L1,
C       it moves  data down, but does not shift, conactenate, or output it.
C
C       1) concatenate the STICS rate data in the 3rd science record
C       If EDBset is 3 (this is 3rd science record or later) perform steps 2-8
C         2) shift data into/within the first science record
C         3) fill in the wall times in the HDB with those of spin 0
C         4) build the science record header
C         5) write the completed science record to the output file and keep a
C            count of the number of records written
C         6) move science data down within their various arrays
C         7) concatenate STICS PHA data
C         8) clear 3rd science record to zero
C       9) copy latest HK to first position in newly cleared science record
C      10) increment EDBset, but to no higher than 3
C
C     This version:   25-JAN-1995
C
C     CREATION:
C       25-JAN-1995     J.PAQUETTE
C
C     REVISIONS:
C
CDEC$ IDENT     '1.0'
	IMPLICIT NONE
	INCLUDE   'INC_DIR:EDBDEF.INC/NOLIST'
	INCLUDE   'INC_DIR:EDBVARS.INC/NOLIST'
	INCLUDE   'INC_DIR:L0_DATA_Record.INC/NOLIST'
C
	INTEGER ISTAT
C
	INTEGER DIAG
	LOGICAL INTER
	COMMON /DIAG/ DIAG,INTER
C
C At low bit rate, a STICS voltage step lasts for two spins, and the data is cut
C in half and output over two spins. Thus, for low bit rate, we must concatenate
C the two halves of the data. Rate data must be concatenated before shifting.
C
        Sci_rec_cntr=Sci_rec_cntr+1
C
        IF (EDBset .EQ. 3) THEN
C
C The wall clock time is not given as part of the HDB, so copy it from the
C EDB of spin zero and store in the HDB structure.
C Also copy it in ITIME format, and store it, too, in structure 
C
          HDB(1).BDH.EPOCH = SCI(1).EDB(0).EPOCH
          HDB(1).BDH.W_Time = SCI(1).EDB(0).W_Time
          L1_recs_written = L1_recs_written + 1
          IF(DIAG .LE. 5 .OR. .NOT.INTER) THEN               
            WRITE(6,1001) L1_recs_written
1001        FORMAT('  Done with L1 Sci Rec ',I4)
          ELSE                               
            WRITE(6,1002) L1_recs_written
1002        FORMAT('+ Done with L1 Sci Rec ',I4)
          ENDIF                              
C
C Move the data. Contents of 2nd science record --> 1st science record.
C Contents of 3rd science record --> 2nd science record.  New data will go into
C the 3rd science record in the future.  We also have to do the same thing with
C the HK's and their pointers and the HDBs and their times.
C Also zero out the structures of the third Science Record to logically clear them.
C This has to be done to allow for missing spins.
C
          CALL WIND_POST_PROCESS
        END IF
C
C At this point we have a completed copy of the most recent science record.  If
C EDBset is a 1 or a 2 then the science record just completed is in position
C EDBset (that is, 1 or 2).  If, however, EDBset is 3 (the normal case after the
C first two records have been processed) then the science record just completed
C in is position 2, since it has just been shifted down one position. The
C science record just completed (except for the first one) may have an extra HK
C at its beginning which should  not have been duplicated from the previous
C science record.  Call routine WIND_RESET_HK to examine this science record and
C compare the times of the EDB and the second HK (the first HK was duplicated
C from the previous science record and is guaranteed to have a much earlier time
C stamp).  If the times reasonably match (within 1 sec) remove the first HK by
C copying the rest of the HKs down one position.  Remember to adjust the counter
C of number of valid HKs (array HKset).  
C
        IF(EDBSet.EQ.3)THEN
          CALL WIND_RESET_HK(EDBSet-1)
        ELSE
          CALL WIND_RESET_HK(EDBSet)
        END IF
C
C Formerly simply had EDBset=2; thus if EDBset was 1 it was set to 2. If it was 2 
C already, it remained so. Now If its 1 or 2 its incremented; if its 3 it remains 
C 3.
C
        IF(EDBset.LT.3)THEN
          EDBset =  EDBset+1
        ENDIF
C
	RETURN
	END
