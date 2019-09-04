      INTEGER*4	Sci_rec_cntr,		! Counter of sci recs before shifting
     +          L1_recs_written,        ! Output L1 science record counter
     +          EDB_Lost,               ! EDBs lost during run
     +          EDB_Skip,               ! EDBs skipped due to command
     +          EDB_Sum,                ! Total number of EDBs handled
     +          Curr_Sci_Rec_Number,    ! Current science record number
     +          Curr_Spin               ! Current spin being processed
      LOGICAL*4 New_Sci_Rec,            ! Set if just finished sci rec, starting
					! new one; clear otherwise
     +          eph_avail,              ! Set true if orbit and attitude data
					! is available; false otherwise
     +		from_cd			! Set if LZ file is on CDROM
      COMMON /L1INFO/
     +		Sci_rec_cntr,		! Counter of sci recs before shifting
     +          L1_recs_written,        ! Output L1 science record counter
     +          EDB_Lost,               ! EDBs lost during run
     +          EDB_Skip,               ! EDBs skipped due to command
     +          EDB_Sum,                ! Total number of EDBs handled
     +          Curr_Sci_Rec_Number,    ! Current science record number
     +          Curr_Spin,              ! Current spin being processed
     +		New_Sci_Rec,            ! Set if just finished sci rec, starting
					! new one; clear otherwise
     +          eph_avail,              ! Set true if orbit and attitude data
					! is available; false otherwise
     +		from_cd			! Set if LZ file is on CDROM
