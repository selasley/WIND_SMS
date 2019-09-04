	SUBROUTINE WIND_OUTPUT_L1
C
C       This routine prepares a science record for output and writes it out.
C       Doing this requires several steps, some of which are science related,
C       others mainly bookkeeping.
C
C	1) save science record counter in Curr_Sci_Rec_Number, set Curr_Spin
C	  to -1
C       2) concatenate the STICS rate data in the EDBset'th science record
C       If EDBset is 3 (this is 3rd science record or later) perform steps 2-8
C         3) shift data into/within the first science record
C         4) fill in the wall times in the HDB with those of spin 0
C         5) build the science record header
C         6) write the completed science record to the output file and keep a
C            count of the number of records written
C         7) move science data down within their various arrays
C         8) concatenate STICS PHA data
C         9) clear 3rd science record to zero
C      10) copy latest HK to first position in newly cleared science record
C      11) increment EDBset, but to no higher than 3
C      12) increment Sci_rec_cntr, save into Curr_Sci_Rec_Number
C      13) set flag New_Sci_Rec to .TRUE.
C
C     This version:   1.6   14-MAR-1995
C
C     CREATION:
C       13-JAN-1995    L. Bleau
C
C     REVISIONS:
C       19-JAN-1995     J.PAQUETTE      call to CONCATENATE_STICS split into
C                                       calls to CONCATENATE_STICS_RATES and
C                                       CONCATENATE_STICS_PHA 
C 1.2	 7-FEB-1995	PAQUETTE	if found zero-valued PHA word decrement
C					count of PHA words for that sensor
C 1.3	13-FEB-1995	L. Bleau	move where Sci_Rec_Cntr is incremented,
C					add code to save it in Curr_Sci_Rec_Number,
C					add calls to WIND_REPORT to inform user
C					of zero-valued PHA words
C 1.4   14-FEB-1995	L. Bleau	set New_Sci_Rec .TRUE. before returning
C	23-FEB-1995	L. Bleau	remove comman DIAG and add INCLUDE
C					DIAG.INC
C 1.5	23-FEB-1995	L. Bleau	if PRINTLZ_MODE is set do everything
C					except write the L1 record
C	27-FEB-1995	L. Bleau	updated comments
C 1.6	14-MAR-1995	L. Bleau	limit output file to 600 L1 records in
C					case there's an infinite loop
C
CDEC$ IDENT     '1.6'
	IMPLICIT NONE
	INCLUDE   'INC_DIR:EDBDEF.INC/NOLIST'
	INCLUDE   'INC_DIR:EDBVARS.INC/NOLIST'
	INCLUDE   'INC_DIR:L0_DATA_Record.INC/NOLIST'
	INCLUDE   'INC_DIR:DIAG.INC/NOLIST'
	INCLUDE   'INC_DIR:L1FMT.INC/NOLIST'
C
	INTEGER ISTAT, SPIN
C
C Update current science record number and current spin info
C Set Curr_Spin to -1 to indicate this processing isn't associated with any
C particular spin
C
        Curr_Sci_Rec_Number = Sci_Rec_Cntr
	Curr_Spin = -1
C
C At low bit rate, a STICS voltage step lasts for two spins, and the data is
C cut in half and output over two spins. Thus, for low bit rate, we must
C concatenate the two halves of the data. Rate data must be concatenated before
C shifting.
C
        CALL WIND_CONCATENATE_STICS_RATES(EDBSet)
C
        IF (EDBset .EQ. 3) THEN
C
C Now we must shift the data elements in time so that they will be associated
C with the times (and voltage steps) during which they were accumulated, not
C those during which they were read out.
C
          CALL WIND_SHIFT_DATA
C
C At low bit rate, a STICS voltage step lasts for two spins, and the data is
C cut in half and output over two spins. Thus, for low bit rate, we must
C concatenate the two halves of the data. PHA data must be concatenated after
C shifting.
C
          CALL WIND_CONCATENATE_STICS_PHA(1)
C
C The wall clock time is not given as part of the HDB, so copy it from the
C EDB of spin zero and store in the HDB structure.
C Also copy it in ITIME format, and store it, too, in structure 
C
          HDB(1).BDH.EPOCH = SCI(1).EDB(0).EPOCH
          HDB(1).BDH.W_Time = SCI(1).EDB(0).W_Time
C
C Prepare a header for the record to be stored.
C
          CALL WIND_BUILD_SR_HEADER
C
C If we are in PRINTLZ mode don't do any reports or writes
C
          IF (.NOT. PRINTLZ_MODE) THEN
C
C Test for PHA word that are 0 using the flags in the header record; if we have
C a 0 PHA word decrement the PHA word counter, effectively eliminating it.
C
            DO 100 SPIN=0,59
              Curr_Spin = SPIN
              IF(BTEST( L1_SR_HEADER.ZERO_SWICS_PHA(SPIN/32+1), MOD(SPIN,32) ) )THEN
                CALL WIND_REPORT( 'zero-valued SWICS PHA word found', 0, Curr_Spin )
                SCI(1).EDB(SPIN).SWICS_Len = SCI(1).EDB(SPIN).SWICS_Len - 1
              ENDIF
              IF(BTEST( L1_SR_HEADER.ZERO_STICS_PHA(SPIN/32+1), MOD(SPIN,32) ) )THEN
                CALL WIND_REPORT( 'zero-valued STICS PHA word found', 0, Curr_Spin  )
                SCI(1).EDB(SPIN).STICS_Len = SCI(1).EDB(SPIN).STICS_Len - 1
              ENDIF
              IF(BTEST( L1_SR_HEADER.ZERO_MASS_PHA(SPIN/32+1), MOD(SPIN,32) ) )THEN
                CALL WIND_REPORT( 'zero-valued MASS PHA word found', 0, Curr_Spin  )
                SCI(1).EDB(SPIN).MASS_Len = SCI(1).EDB(SPIN).MASS_Len - 1
              ENDIF
 100        CONTINUE
          ENDIF                              
C
C Now store the L1 data
C Write out 1st science record, which is the one we just corrected by the calls
C to WIND_CONCATENATE_STICS and WIND_SHIFT_DATA
C
          IF (.NOT. PRINTLZ_MODE) CALL WIND_STORE_L1( ISTAT )
          L1_recs_written = L1_recs_written + 1
          IF(DIAG .LE. 5 .OR. .NOT.INTER) THEN
            WRITE(6,1001) L1_recs_written
1001        FORMAT('  Done with L1 Sci Rec ',I4)
          ELSE
            WRITE(6,1002) L1_recs_written
1002        FORMAT('+ Done with L1 Sci Rec ',I4)
          ENDIF
C
C Test number of L1 records written; there should be only 480.
C If it's greater than 600 assume there's an infinite loop and stop the program.
C
	  IF (L1_recs_written .GT. 600) THEN
	    TYPE *,' '
	    TYPE *,'*** WIND_CD_READ is in an infinite loop ***'
	    TYPE *,'*** Stopping program ***'
	    CALL WIND_REPORT('probable infinite loop, stopping program',
     &        0, L1_recs_written )
	    STOP
	  ENDIF
C
C Move the data. Contents of 2nd science record --> 1st science record.
C Contents of 3rd science record --> 2nd science record.  New data will go into
C the 3rd science record in the future.  We also have to do the same thing with
C the HK's and their pointers and the HDBs and their times.
C Also zero out the structures of the third Science Record to logically clear
C them.  This has to be done to allow for missing spins.
C
          CALL WIND_POST_PROCESS
        END IF
C
C At this point we have a completed copy of the most recent science record.  If
C EDBset is a 1 or a 2 then the science record just completed is in position
C EDBset (that is, 1 or 2).  If, however, EDBset is 3 (the normal case after
C the first two records have been processed) then the science record just
C completed in is position 2, since it has just been shifted down one position.
C The science record just completed (except for the first one) may have an
C extra HK at its beginning which should  not have been duplicated from the
C previous science record.  Call routine WIND_RESET_HK to examine this science
C record and compare the times of the EDB and the second HK (the first HK was
C duplicated from the previous science record and is guaranteed to have a much
C earlier time stamp).  If the times reasonably match (within 1 sec) remove the
C first HK by copying the rest of the HKs down one position.  Remember to
C adjust the counter of number of valid HKs (array HKset).
C
        IF(EDBSet.EQ.3)THEN
          CALL WIND_RESET_HK(EDBSet-1)
        ELSE
          CALL WIND_RESET_HK(EDBSet)
        END IF
C
C Formerly simply had EDBset=2; thus if EDBset was 1 it was set to 2. If it was
C 2 already, it remained so. Now If its 1 or 2 its incremented; if its 3 it
C remains 3.
C
        IF(EDBset.LT.3)THEN
          EDBset =  EDBset+1
        ENDIF
C
C Update current science record number and current spin info (again)
C Also set New_Sci_Rec to let other parts of the program know we've started
C processing a new science record
C
        Sci_rec_cntr = Sci_rec_cntr+1
        Curr_Sci_Rec_Number = Sci_Rec_Cntr
	Curr_Spin = -1
        New_Sci_Rec = .TRUE.

	RETURN
	END
