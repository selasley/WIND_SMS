	SUBROUTINE WIND_POST_PROCESS
C
C         This routine moves the data from 2nd and 3rd science records and
C         and their associated HKs and HDBs into their 1st and 2nd positions,
C         respectively; zeroes out all data associated with 3rd science record.
C
C     This version:   1.1   15-FEB-1995
C
C     CREATION:
C       13-JAN-1995      L. Bleau/J. Paquette
C
C     REVISIONS:
C	 9-FEB-1995	L. Bleau	change include stmt to use EDBDEF.INC
C					and EDBVARS.INC instead of EDB.INC only
C       15-FEB-1995     J.PAQUETTE      removed references to HDBset since HDB
C                       L. Bleau        now indexed by EDBset
C
C
CDEC$ IDENT     '1.1'
	IMPLICIT NONE
	INCLUDE   'INC_DIR:EDBDEF.INC/NOLIST'
	INCLUDE   'INC_DIR:EDBVARS.INC/NOLIST'
	INCLUDE   'INC_DIR:L0_DATA_Record.INC/NOLIST'
	INCLUDE   'INC_DIR:WIND_Lun.INC/NOLIST'
C
	INTEGER SPIN
C
C Move the data.  Contents of 2nd science record --> 1st science record.
C Contents of 3rd science record --> 2nd science record.  New data will go into
C the 3rd science record in the future.  We also have to do the same thing with
C the HK's and their pointers.
C
        SCI(1)              = SCI(2)
        SWICS_RATE(1)       = SWICS_RATE(2)
        SWICS_PHA(1)        = SWICS_PHA(2)
        STICS_RATE(1)       = STICS_RATE(2)
        STICS_PHA(1)        = STICS_PHA(2)
        MASS_RATE(1)        = MASS_RATE(2)
        MASS_PHA(1)         = MASS_PHA(2)

        HKEEP(1)            = HKEEP(2)
        HKset(1)            = HKset(2)
        HDBTime2( 1, 1 )    = HDBTime2( 1, 2 )
        HDBTime2( 2, 1 )    = HDBTime2( 2, 2 )
        HDB(1)              = HDB(2)

        SCI(2)              = SCI(3)
        SWICS_RATE(2)       = SWICS_RATE(3)
        SWICS_PHA(2)        = SWICS_PHA(3)
        STICS_RATE(2)       = STICS_RATE(3)
        STICS_PHA(2)        = STICS_PHA(3)
        MASS_RATE(2)        = MASS_RATE(3)
        MASS_PHA(2)         = MASS_PHA(3)

        HKEEP(2)            = HKEEP(3)
        HKset(2)            = HKset(3)
        HDBTime2( 1, 2 )    = HDBTime2( 1, 3 )
        HDBTime2( 2, 2 )    = HDBTime2( 2, 3 )
        HDB(2)              = HDB(3)

C
C Now zero out the structures of the third Science Record to logically clear
C them.  This has to be done to allow for missing spins.
C
        SCI(3).CCORE(5:) = C_BIG_NULL_BUFFER(1:LEN(SCI(3).CCORE)-4)
        DO SPIN=0,59
          SWICS_RATE(3).C_SW_RATE(SPIN)(5:) = C_BIG_NULL_BUFFER(1:LEN(SWICS_RATE(3).C_SW_RATE(SPIN))-4)
          SWICS_PHA(3).C_SW_PHA(SPIN)(5:) = C_BIG_NULL_BUFFER(1:LEN(SWICS_PHA(3).C_SW_PHA(SPIN))-4)
          STICS_RATE(3).C_ST_RATE(SPIN)(5:) = C_BIG_NULL_BUFFER(1:LEN(STICS_RATE(3).C_ST_RATE(SPIN))-4)
          STICS_PHA(3).C_ST_PHA(SPIN)(5:) = C_BIG_NULL_BUFFER(1:LEN(STICS_PHA(3).C_ST_PHA(SPIN))-4)
          MASS_RATE(3).C_MA_RATE(SPIN)(5:) = C_BIG_NULL_BUFFER(1:LEN(MASS_RATE(3).C_MA_RATE(SPIN))-4)
          MASS_PHA(3).C_MA_PHA(SPIN)(5:) = C_BIG_NULL_BUFFER(1:LEN(MASS_PHA(3).C_MA_PHA(SPIN))-4)
        ENDDO
C
        HKEEP(3).CHK(5:) = C_BIG_NULL_BUFFER(1:LEN(HKEEP(3).CHK)-4)
        HKset(3) = 0
        HDBTime2( 1, 3 ) = 0
        HDBTime2( 2, 3 ) = 0
        HDB(3).CHDB(5:) = C_BIG_NULL_BUFFER(1:LEN(HDB(3).CHDB)-4)
C
	RETURN
	END
