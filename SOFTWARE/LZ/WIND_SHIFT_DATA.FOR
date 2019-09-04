      SUBROUTINE WIND_SHIFT_DATA
C
C	The time associated with the data in the level 0 data is the time at
C	which the data was read out, not the time at which it was accumulated.
C	This routine shifts the data elements so that the data is associated
C	with the time period during which it was accumulated. The reference
C	used to determine the amount shift, and the rate format for each
C	instrument was the WIND SMS Commands and Data Formats for the SMS
C	Instruments Revision 3.0.
C
C	To understand the logic of this program, it is important to note that
C	we have two science records in memory at a time. Every science record
C	is initially read into SCI(3). When SCI(1) is stored, SCI(2) -->
C	SCI(1), SCI(3) --> SCI(2), and a new science record is read into
C	SCI(3).
C
C
C	This version:   9-FEB-1995 
C
C     Creation:
C	JOHN PAQUETTE	 3-AUG-1994
C
C     Revisions:
C        4-AUG-1994	J.PAQUETTE	added code to handle STICS data 
C	 5-AUG-1994	J.PAQUETTE	added code to handle SWICS data 
C	26-SEP-1994	J.PAQUETTE	added code to handle quality flags 
C	28-SEP-1994	L.Bleau		changed bitrate test to examine SCI
C                                       structure instead of flag in common
C	29-SEP-1994	L.Bleau		removed errant logic re low spin
C       				numbers in all sensors
C        4-OCT-1994	J.PAQUETTE      use good Qual spin for Bit Rate
C       10-OCT-1994	J.PAQUETTE,     complete change in logic to deal with
C                       L. Bleau        changes in Bit Rate
C       12-OCT-1994     J.PAQUETTE      added calls to COMPRESS and DECOMPRESS
C       19-JAN-1995     J.PAQUETTE      at low bitrate, STICS PHA data is still 
C                                       only shifted by 1 spin, not by 2 (even
C                                       rates that are shifted by 1 spin at HBR
C                                       are shifted by 2 at LBR). Changed code
C                                       accordingly
C       20-JAN-1995     J.PAQUETTE      STICS PHA given their own DO loop 
C                                       because the DO 500 loop steps by a
C                                       variable ammount, not by 1 each time
C	 9-FEB-1995	L. Bleau	change include stmt to use EDBDEF.INC
C					and EDBVARS.INC instead of EDB.INC only
C
CDEC$ IDENT   '1.8'

      IMPLICIT NONE

      INCLUDE 'INC_DIR:EDBDEF.INC/NOLIST'
      INCLUDE 'INC_DIR:EDBVARS.INC/NOLIST'
      INCLUDE 'INC_DIR:WIND_L0_RECORD.INC/NOLIST'
      INCLUDE 'INC_DIR:WIND_Lun.INC/NOLIST'
      INCLUDE '($SSDEF)/NOLIST'

      INTEGER*4	II,	  ! Spin index
     :	        JJ,	  ! Byte index
     :          KK,       ! Pointer to first good spin
     :          MRSTART,  ! Index of last Matrix Rate for spin II     
     :          MESTART,  ! Index of last Matrix Element for spin II  
     :          ERSTART,  ! Index of last Engineering Rate for spin II
     :          MRSTART2, ! Index of last Matrix Rate for spin II+2
     :          MESTART2, ! Index of last Matrix Element for spin II SCI-REC 2 or 3
     :          ERSTART2, ! Index of last Engineering Rate for spin II+2
     :          INC1,INC2,              ! (STICS) Amount to increment to reach next, 2nd spin
     :          NEXTSPIN,NEXTSPIN2,     ! Pointer to next, 2nd spin
     :          RECNO,RECNO2,           ! SCI-REC of next, 2nd spin
     :          NEXTSPIN_PHA,           ! Pointer to next spin with PHA
     :          RECNO_PHA,              ! SCI-REC of next spin
     :          ACTIVE_PHA_RANGE,       ! Active PHA range ID; 0,1, or 2
     :          APHASTART(3),           ! Start index for PHA ranges
     :          HOUSEKEEPING,           ! House keeping byte for SWICS
     :          SUM,SUM2,               ! Used as accumulators
     :          ITEMP
      INTEGER*4 INCREMENT,WIND_DECOMPRESS    ! Functions
      BYTE      WIND_COMPRESS                ! Function 
      LOGICAL   HIGHLOW,LOWHIGH,CODE_C
C
C In some science records, spins may be absent, fill, or otherwise of low
C quality.  Determine first good quality spin, and use it for bit rate
C determination.
C
      KK=0
      DO 5 WHILE((SCI( 1 ).EDB(KK).Qual.NE.0).AND.(KK.LT.59))
        KK=KK+1
  5   CONTINUE    
C
C Shift SWICS data. Loop over the 60 spins in a science record. Since some
C rates are read out 1 spin after accumulation, some are read out 2 spins, some
C are read out 1 full science record after accumulation, and some are read out
C 2 full science records after accumulation, we have several possible
C conditions.
C
      DO 200 II = 0, 59
C
C RECNO, and RECNO2 are the numbers of the science records containing NEXTSPIN
C and NEXTSPIN2, respectively.
C
        NEXTSPIN=MOD(II+1,60)
        NEXTSPIN2=MOD(II+2,60)
        RECNO=(II+1)/60+1
        RECNO2=(II+2)/60+1
C
        IF(SCI( 1 ).EDB( II ).BitRate)THEN      		! High bit rate (current spin)
          MRSTART=80
          MESTART=101
          ERSTART=109
          LOWHIGH=.FALSE.
          IF(.NOT.(SCI( RECNO ).EDB( NEXTSPIN ).BitRate))THEN	! High bit rate (next spin)
            HIGHLOW=.TRUE.
          ELSE
            HIGHLOW=.FALSE.
          ENDIF
        ELSE                                                    ! Low bit rate (current spin)
          MRSTART=26
          MESTART=47
          ERSTART=55
          HIGHLOW=.FALSE.
          IF(SCI( RECNO ).EDB( NEXTSPIN ).BitRate)THEN	        ! High bit rate (next spin)
            LOWHIGH=.TRUE.
          ELSE
            LOWHIGH=.FALSE.
          ENDIF
        ENDIF
C
        IF(SCI( RECNO2 ).EDB( NEXTSPIN2).BitRate)THEN	        ! High bit rate
          MRSTART2=80
          ERSTART2=109
        ELSE                                                    ! Low bit rate
          MRSTART2=26
          ERSTART2=55
        ENDIF
C
C Basic Rates (bytes 0 to MRSTART-1) are read out 1 spin after accumulation.
C Shift them back by one spin, except in various special cases.
C
        IF(LOWHIGH)THEN
C
C When the current record is in low bit rate, and the record that follows is in
C high bit rate, we cannot simply shift the Basic Rates. The Basic Rates at low
C bit rate are summed over several sectors, while the Basic Rates at high bit
C rate are each specific to a sector. Thus we must sum the data together
C appropriately before we shift. Since the SWICS rates are "compressed" (i.e.
C logarithmically encoded so as to fit into a byte), we must decompress them, 
C add the decompressed values and recompress them.
C
          DO 10 JJ = 0, 15
            ITEMP = HDB(RECNO).BDH.DAT(189)
            CODE_C = BTEST(ITEMP,0)
            SUM = WIND_DECOMPRESS(SWICS_RATE(RECNO).EDB(NEXTSPIN).RATE(2*JJ),CODE_C) +
     &            WIND_DECOMPRESS(SWICS_RATE(RECNO).EDB(NEXTSPIN).RATE(2*JJ+1),CODE_C)
            SWICS_RATE( 1 ).EDB( II ).RATE(JJ) = WIND_COMPRESS(SUM,CODE_C)
  10      CONTINUE
C
C The active PHA range determines which data are more fully transmitted.
C
          HOUSEKEEPING = SWICS_RATE(RECNO).EDB(NEXTSPIN).RATE(61)
          ACTIVE_PHA_RANGE = IBITS(HOUSEKEEPING,6,2)
C
C Based on which range is active, get indices needed for summing.
C
          IF(ACTIVE_PHA_RANGE.EQ.0)THEN      ! Active range=2a
            APHASTART(1)=32
            APHASTART(2)=48
            APHASTART(3)=64
          ELSEIF(ACTIVE_PHA_RANGE.EQ.1)THEN  ! Active range=2b
            APHASTART(1)=48
            APHASTART(2)=32
            APHASTART(3)=64
          ELSE                               ! Active range=2c
            APHASTART(1)=64
            APHASTART(2)=32
            APHASTART(3)=48
          ENDIF  
C
C For active PHA range, sum rates in pairs and shift.
C
          DO 20 JJ = 0, 7                 
            ITEMP = HDB(RECNO).BDH.DAT(189)
            CODE_C = BTEST(ITEMP,0)
            SUM = 
     &       WIND_DECOMPRESS(SWICS_RATE(RECNO).EDB(NEXTSPIN).RATE(2*JJ + APHASTART(1)), CODE_C) + 
     &       WIND_DECOMPRESS(SWICS_RATE(RECNO).EDB(NEXTSPIN).RATE(2*JJ + APHASTART(1) + 1),CODE_C) 
            SWICS_RATE(1).EDB(II).RATE(JJ+8) = WIND_COMPRESS(SUM,CODE_C)
  20      CONTINUE
C
C For other two PHA ranges, sum all sectors and shift.
C      
          SUM=0
          SUM2=0                  
          DO 40 JJ = 0, 15
            ITEMP = HDB(RECNO).BDH.DAT(189)
            CODE_C = BTEST(ITEMP,0)
            SUM = SUM + 
     &            WIND_DECOMPRESS(SWICS_RATE(RECNO).EDB(NEXTSPIN).RATE(JJ + APHASTART(2)),CODE_C)
            SUM2= SUM2 + 
     &            WIND_DECOMPRESS(SWICS_RATE(RECNO).EDB(NEXTSPIN).RATE(JJ + APHASTART(3)),CODE_C)
  40      CONTINUE
          SWICS_RATE(1).EDB(II).RATE(24) = WIND_COMPRESS(SUM, CODE_C)
          SWICS_RATE(1).EDB(II).RATE(25) = WIND_COMPRESS(SUM2,CODE_C)
C
C Adjust quality flags for Basic Rates. Note that this depends on LOGICAL*1
C being equivalent to BYTE.
C
          SCI(1).EDB(II).SW_BR_Qual  = SCI(RECNO).EDB(NEXTSPIN).SW_BR_Qual
C
        ELSEIF(HIGHLOW)THEN
C
C Specific Sector information is lost in low bit rate, as the numbers
C transmitted are accumulated over several sectors. In high bit rate, specific
C sector information is needed. Therefore, when a spin in high bit rate is
C immediately followed by a spin in low bit rate, we cannot shift the basic
C rates. Thus, we zero them and set the quality flags to indicate that the
C Basic Rates are missing.
C
          DO 60 JJ = 0, 15
            SWICS_RATE( 1 ).EDB( II ).RATE(JJ) = 0
  60      CONTINUE
C
C Set quality flags for Basic Rates to 8 (missing data). Note that this depends
C on LOGICAL*1 being equivalent to BYTE.
C
          SCI(1).EDB(II).SW_BR_Qual  = 8
        ELSE
C
C If the spin that follows the current one has the same bitrate as the current
C one, then the Basic Rates may easily be shifted.
C
          DO 110 JJ = 0, MRSTART-1
            SWICS_RATE( 1 ).EDB( II ).RATE(JJ) = SWICS_RATE(RECNO).EDB(NEXTSPIN).RATE(JJ)
 110      CONTINUE
C
C Adjust quality flags for Basic Rates. Note that this depends on LOGICAL*1
C being equivalent to BYTE.
C
          SCI(1).EDB(II).SW_BR_Qual  = SCI(RECNO).EDB(NEXTSPIN).SW_BR_Qual
        ENDIF
C
C PHA data is read out 1 spin after being accumulated also. Transfer SWICS PHA
C data from spin II+1 to spin II.
C
        SWICS_PHA( 1 ).C_SW_PHA( II ) = SWICS_PHA(RECNO).C_SW_PHA(NEXTSPIN)
C
C Adjust the # of PHA words for this spin to reflect the change
C
        SCI( 1 ).EDB( II ).SWICS_Len = SCI(RECNO).EDB(NEXTSPIN).SWICS_Len
C
C Adjust quality flags for PHA. Note that this depends on LOGICAL*1 being
C equivalent to BYTE.
C
        SCI(1).EDB(II).SW_P_Qual = SCI(RECNO).EDB(NEXTSPIN).SW_P_Qual
C     
C Matrix Rates (bytes MRSTART to MESTART-1) are read out 2 spins after
C accumulation. Shift them back by two spins.
C
        DO 120 JJ = 0,20
          SWICS_RATE(1).EDB(II).RATE(JJ+MRSTART)=SWICS_RATE(RECNO2).EDB(NEXTSPIN2).RATE(JJ+MRSTART2)
 120    CONTINUE
C
C Adjust quality flags for Matrix Rates. Note that this depends on LOGICAL*1
C being equivalent to BYTE.
C
        SCI(1).EDB(II).SW_MR_Qual  = SCI(RECNO2).EDB(NEXTSPIN2).SW_MR_Qual
C       
C Engineering Rates (bytes MESTART+1 to ERSTART) are read out 2 spins after
C accumulation. Shift them back by two spins.
C
        DO 130 JJ = 0,5
          SWICS_RATE(1).EDB(II).RATE(JJ+ERSTART)=SWICS_RATE(RECNO2).EDB(NEXTSPIN2).RATE(JJ+ERSTART2)
 130    CONTINUE
C
C Adjust quality flags for Engineering Rates. Note that this depends on
C LOGICAL*1 being equivalent to BYTE.
C
        SCI(1).EDB(II).SW_ER_Qual = SCI(RECNO2).EDB(NEXTSPIN2).SW_ER_Qual
C
C Now shift Matrix Elements. Matrix Elements 0-121 are shifted by 1 science
C record. Since the Matrix Elements are subcommutated over 60 spins, eight of
C them are read out each spin (appearing in bytes MESTART to MESTART+7). Thus,
C the first 15 spins hold the Matrix Elements 0-119, all of which are shifted
C by one science record.
C
        IF(II.LT.15)THEN
C
          IF(SCI( 2 ).EDB( II ).BitRate)THEN		! High bit rate
            MESTART2=101
          ELSE                                          ! Low bit rate
            MESTART2=47
          ENDIF
C     
          DO 170 JJ = 0,7
            SWICS_RATE(1).EDB(II).RATE(JJ+MESTART) = SWICS_RATE(2).EDB(II).RATE(JJ+MESTART2)
 170      CONTINUE
C
C Adjust quality flags for Matrix Elements 0-121. Note that this depends on
C LOGICAL*1 being equivalent to BYTE.
C
          SCI(1).EDB(II).SW_ME1_Qual = SCI(2).EDB(II).SW_ME1_Qual
C
C Spin 15 is an unusual one. Bytes MESTART and MESTART+1 hold Matrix Elements
C 120, and 121, which are shifted by one science record, while bytes MESTART+2
C to MESTART+7 hold Matrix Elements 122-127, which are shifted by two science
C records.
C
        ELSEIF(II.EQ.15)THEN
C
          IF(SCI( 2 ).EDB( 15 ).BitRate)THEN		! High bit rate
            MESTART2=101
          ELSE                                          ! Low bit rate
            MESTART2=47
          ENDIF
C
          SWICS_RATE(1).EDB(15).RATE(MESTART+1) = SWICS_RATE(2).EDB(15).RATE(MESTART2+1)
          SWICS_RATE(1).EDB(15).RATE(MESTART+2) = SWICS_RATE(2).EDB(15).RATE(MESTART2+2)
C
          IF(SCI( 3 ).EDB( 15 ).BitRate)THEN		! High bit rate
            MESTART2=101
          ELSE                                          ! Low bit rate
            MESTART2=47
          ENDIF
C
          DO 180 JJ = 2,7
            SWICS_RATE(1).EDB(15).RATE(JJ+MESTART) = SWICS_RATE(3).EDB(15).RATE(JJ+MESTART2)
 180      CONTINUE
        ELSE
C
C The remaining spins (16-59) hold Matrix Elements 128-479, which are all
C shifted by 2 science records.
C
          IF(SCI( 3 ).EDB( II ).BitRate)THEN		! High bit rate
            MESTART2=101
          ELSE                                          ! Low bit rate
            MESTART2=47
          ENDIF
C
          DO 190 JJ = 0,7
            SWICS_RATE(1).EDB(II).RATE(JJ+MESTART) = SWICS_RATE(3).EDB(II).RATE(JJ+MESTART2)
 190      CONTINUE
C
C Adjust quality flags for Matrix Elements 122-479. Note that this depends on
C LOGICAL*1 being equivalent to BYTE.
C
          SCI(1).EDB(II).SW_ME2_Qual = SCI(3).EDB(II).SW_ME2_Qual
        ENDIF
200   CONTINUE
C
C Shift MASS data. Loop over the 60 spins in a science record. Since some rates
C are read out 1 spin after accumulation, and some are read out 2 spins after
C accumulation, we have several possible conditions.
C
      DO 300 II = 0, 59
        IF(II.LT.59)THEN
C
C Sectored Engineering Rates (bytes 4-10) and Basic Rates (bytes 11-14) are
C read out 1 spin after accumulation. Shift them back by one spin.
C
          DO 210 JJ = 4, 14
            MASS_RATE( 1 ).EDB( II ).RATE(JJ) = MASS_RATE( 1 ).EDB( II + 1 ).RATE(JJ)
 210      CONTINUE
C
C Adjust quality flags for Sectored Engineering Rates and for Basic Rates. Note
C that this depends on LOGICAL*1 being equivalent to BYTE.
C
          SCI(1).EDB(II).MA_SER_Qual = SCI(1).EDB(II+1).MA_SER_Qual
          SCI(1).EDB(II).MA_BR_Qual  = SCI(1).EDB(II+1).MA_BR_Qual
C
C PHA data is read out 1 spin after being accumulated also. Transfer MASS PHA
C data from spin II+1 to spin II.
C
          MASS_PHA( 1 ).C_MA_PHA( II ) = MASS_PHA( 1 ).C_MA_PHA( II + 1 )
C
C Adjust the # of PHA words for this spin to reflect the change
C
          SCI( 1 ).EDB( II ).MASS_Len = SCI( 1 ).EDB( II + 1 ).MASS_Len
C
C Adjust quality flags for PHA. Note that this depends on LOGICAL*1 being
C equivalent to BYTE.
C
          SCI(1).EDB(II).MA_P_Qual = SCI(1).EDB(II+1).MA_P_Qual
        ENDIF
C
        IF(II.LT.58)THEN
C
C Unsectored Engineering Rates (bytes 0-3) are read out 2 spins after
C accumulation.  Shift them back by two spins.
C
          DO 220 JJ = 0, 3
            MASS_RATE( 1 ).EDB( II ).RATE(JJ) = MASS_RATE( 1 ).EDB( II + 2 ).RATE(JJ)
 220      CONTINUE
C
C Adjust quality flags for Unsectored Engineering Rates. Note that this depends
C on LOGICAL*1 being equivalent to BYTE.
C
          SCI(1).EDB(II).MA_UER_Qual = SCI(1).EDB(II+2).MA_UER_Qual
C
C Matrix Rates(bytes 15-24) are read out 2 spins after accumulation. Shift them
C back by two spins.
C
          DO 230 JJ = 15,24
            MASS_RATE( 1 ).EDB( II ).RATE(JJ) = MASS_RATE( 1 ).EDB( II + 2 ).RATE(JJ)
 230      CONTINUE
C
C Adjust quality flags for Matrix Rates. Note that this depends on LOGICAL*1
C being equivalent to BYTE.
C
          SCI(1).EDB(II).MA_MR_Qual = SCI(1).EDB(II+2).MA_MR_Qual
        ENDIF
C
        IF(II.GE.58)THEN
C
C When we near the boundary between science records, the process is the same,
C but the indexing is somewhat different. As before, sectored Engineering Rates
C (bytes 0-3) are read out 2 spins after accumulation; but now, in the process
C of shifting them back by two spins, we reach into the next science record.
C
          DO 250 JJ = 0, 3
            MASS_RATE( 1 ).EDB( II ).RATE(JJ) = MASS_RATE( 2 ).EDB( II-58 ).RATE(JJ)
 250      CONTINUE
C
C Adjust quality flags for Unsectored Engineering Rates. Note that this depends
C on LOGICAL*1 being equivalent to BYTE.
C
          SCI(1).EDB(II).MA_SER_Qual = SCI(2).EDB(II-58).MA_SER_Qual
C
          DO 260 JJ = 15, 24
C
C When we near the boundary between science records, the process is the same,
C but the indexing is somewhat different. As before, Matrix Rates(bytes 15-24)
C are read out 2 spins after accumulation; but now, in the process of shifting
C them back by two spins, we reach into the next science record.
C
            MASS_RATE( 1 ).EDB( II ).RATE(JJ) = MASS_RATE( 2 ).EDB( II-58 ).RATE(JJ)
 260      CONTINUE
C
C Adjust quality flags for Matrix Rates. Note that this depends on LOGICAL*1
C being equivalent to BYTE.
C
          SCI(1).EDB(II).MA_MR_Qual = SCI(2).EDB(II-58).MA_MR_Qual
        ENDIF
C
        IF(II.EQ.59)THEN
C
C When we near the boundary between science records, the process is the same,
C but the indexing is somewhat different. As before, Sectored Engineering Rates
C (bytes 4-10) and Basic Rates (bytes 11-14) are read out 1 spin after
C accumulation; but now, in the process of shifting them back by one spin, we
C reach into the next science record.
C
          DO 280 JJ = 4, 14
            MASS_RATE( 1 ).EDB( 59 ).RATE(JJ) = MASS_RATE( 2 ).EDB( 0 ).RATE(JJ)
 280      CONTINUE
C
C Adjust quality flags for Sectored Engineering Rates and Basic Rates. Note
C that this depends on LOGICAL*1 being equivalent to BYTE.
C
          SCI(1).EDB(59).MA_SER_Qual = SCI(2).EDB(0).MA_SER_Qual
          SCI(1).EDB(59).MA_BR_Qual = SCI(2).EDB(0).MA_BR_Qual
C
C When we near the boundary between science records, the process is the same,
C but the indexing is somewhat different. As before, PHA data is read out 1
C spin after being accumulated; but now, in the process of shifting them back
C by one spin, we reach into the next science record.
C
          MASS_PHA( 1 ).C_MA_PHA( 59 ) = MASS_PHA( 2 ).C_MA_PHA( 0 )
C
C Adjust the # of PHA words for this spin to reflect the change
C
          SCI( 1 ).EDB( 59 ).MASS_Len = SCI( 2 ).EDB( 0 ).MASS_Len
C
C Adjust quality flags for PHA. Note that this depends on LOGICAL*1 being
C equivalent to BYTE.
C
          SCI(1).EDB(59).MA_P_Qual = SCI(2).EDB(0).MA_P_Qual
        ENDIF
 300  CONTINUE
C
C Shift STICS data. Loop over the 60 spins in a science record. Since some
C rates are read out 1 spin after accumulation, and some are read out 2 spins
C after accumulation we have several possible conditions. Also, the format of
C STICS data is dependent on the bit rate.
C

      II=0
      DO 500 WHILE(II .LE. 59)
C
C The number of spins by which the data must be shifted depends on bit rate and
C the type of rate. In high bit rate, there is data in each spin, so rates have
C to be shifted by one or two spins, depending on type. In low bit rate, there
C is data in every other spin (since this program is called only after
C WIND_CONCATENATE_STICS) so that the rates have to be shifted by 2 or 4 spins,
C depending on type.  When we near the boundary between science records, the
C process is the same, but the indexing is somewhat different; in the process
C of shifting back we may reach into the next science record.
C
C INC1 is the number to be added to II (the spin counter) to get the spin
C number of the next record. INC2 is the number to be added to II+INC1 (the
C spin counter of the next record) to get the spin number of the record after
C that.
C
        INC1 = INCREMENT(II)
        INC2 = INCREMENT(II+INC1)
C
C NEXTSPIN is the spin number of the next spin which contains data. The MODULO
C 60 is neccesary because the last few spins in a science record may need data
C from the low numbered spins of the next science record. NEXTSPIN2 is similar,
C but it points to the next spin containing data after NEXTSPIN.
C
C RECNO, and RECNO2 are the numbers of the science records containing NEXTSPIN
C and NEXTSPIN2, respectively.
C
        NEXTSPIN=MOD(II+INC1,60)
        NEXTSPIN2=MOD(II+INC1+INC2,60)
        RECNO=(II+INC1)/60+1
        RECNO2=(II+INC1+INC2)/60+1
C
C High Resolution Rates (bytes 0-95), Sectored Matrix Rates (bytes 96-119) and
C Basic Rates (bytes 120-143) are read out 1 spin after accumulation (2 spins 
C at low bit rate). Shift them back by one spin (2 spins at LBR).
C
        DO 400 JJ = 0, 143
          STICS_RATE(1).EDB( II ).RATE(JJ) = STICS_RATE(RECNO).EDB( NEXTSPIN ).RATE(JJ)
 400    CONTINUE
C
C Adjust quality flags for High Resolution Rates, Sectored Matrix Rates, and
C Basic Rates.  Note that this depends on LOGICAL*1 being equivalent to BYTE.
C
        SCI(1).EDB( II ).ST_HMR_Qual = SCI(RECNO).EDB( NEXTSPIN ).ST_HMR_Qual
        SCI(1).EDB( II ).ST_SMR_Qual = SCI(RECNO).EDB( NEXTSPIN ).ST_SMR_Qual
        SCI(1).EDB( II ).ST_BR_Qual  = SCI(RECNO).EDB( NEXTSPIN ).ST_BR_Qual
C
C Omnidirectional Matrix Rates (bytes 144-163), and Engineering Rates (bytes
C 164-178) are read out 2 spins after accumulation. Shift them back by two
C spins.
C
        DO 420 JJ = 144, 178
          STICS_RATE(1).EDB(II).RATE(JJ) = STICS_RATE(RECNO2).EDB( NEXTSPIN2 ).RATE(JJ)
 420    CONTINUE
C
C Adjust quality flags for Omnidirectional Matrix Rates and Engineering Rates.
C Note that this depends on LOGICAL*1 being equivalent to BYTE.
C
        SCI(1).EDB(II).ST_OMR_Qual = SCI(RECNO2).EDB( NEXTSPIN2 ).ST_OMR_Qual
        SCI(1).EDB(II).ST_ER_Qual  = SCI(RECNO2).EDB( NEXTSPIN2 ).ST_ER_Qual
C
C Its possible for good data to be shifted into a spin that originally
C contained no data. If this happens, reset the RATE_Size variable for the
C appropriate instrument. The spin was missing if its RATE_Size is 0; good data
C was shifted into it if one of its data quality flags is good (meaning 0).
C
        IF(SCI(1).EDB(II).SWICS_RATE_Size.EQ.0)THEN
          IF(.NOT.SCI(1).EDB(II).SW_BR_Qual)SCI(1).EDB(II).SWICS_RATE_Size  = SWICS_N_RATE_Size
          IF(.NOT.SCI(1).EDB(II).SW_MR_Qual)SCI(1).EDB(II).SWICS_RATE_Size  = SWICS_N_RATE_Size
          IF(.NOT.SCI(1).EDB(II).SW_ME1_Qual)SCI(1).EDB(II).SWICS_RATE_Size = SWICS_N_RATE_Size
          IF(.NOT.SCI(1).EDB(II).SW_ME2_Qual)SCI(1).EDB(II).SWICS_RATE_Size = SWICS_N_RATE_Size
          IF(.NOT.SCI(1).EDB(II).SW_ER_Qual)SCI(1).EDB(II).SWICS_RATE_Size  = SWICS_N_RATE_Size
        ENDIF
        IF(SCI(1).EDB(II).STICS_RATE_Size.EQ.0)THEN
          IF(.NOT.SCI(1).EDB(II).ST_HMR_Qual)SCI(1).EDB(II).STICS_RATE_Size = STICS_N_RATE_Size
          IF(.NOT.SCI(1).EDB(II).ST_SMR_Qual)SCI(1).EDB(II).STICS_RATE_Size = STICS_N_RATE_Size
          IF(.NOT.SCI(1).EDB(II).ST_BR_Qual)SCI(1).EDB(II).STICS_RATE_Size  = STICS_N_RATE_Size
          IF(.NOT.SCI(1).EDB(II).ST_OMR_Qual)SCI(1).EDB(II).STICS_RATE_Size = STICS_N_RATE_Size
          IF(.NOT.SCI(1).EDB(II).ST_ER_Qual)SCI(1).EDB(II).STICS_RATE_Size  = STICS_N_RATE_Size
        ENDIF
C
C Increment spin counter
C
        II=II+INC1
C
 500  CONTINUE
C
C Shift STICS PHA data. Since STICS PHA data are always shifted by one spin
C (regardless of bit rate), and since the STICS PHA have not yet been
C concatenated, there is potentially PHA in each and every spin.
C
      DO 600 II = 0, 59
C
C NEXTSPIN_PHA is the spin number of the next spin which contains PHA data. The
C MODULO 60 is neccesary because the last few spins in a science record may
C need data from the low numbered spins of the next science record. Since the
C PHA data have not been concatenated yet, INC1=INC2=1 always when calculating
C NEXTSPIN_PHA and RECNO_PHA.
C
C RECNO_PHA is the number of the science record containing NEXTSPIN_PHA
C
        NEXTSPIN_PHA=MOD(II+1,60)
        RECNO_PHA=(II+1)/60+1
C
C PHA data is read out 1 spin after being accumulated, regardless of bitrate. 
C Transfer STICS PHA data from spin II+1 to spin II.
C
        STICS_PHA( 1 ).C_ST_PHA( II ) = STICS_PHA( RECNO_PHA ).C_ST_PHA( NEXTSPIN_PHA )
C
C Adjust the # of PHA words for this spin to reflect the change
C
        SCI( 1 ).EDB( II ).STICS_Len = SCI( RECNO_PHA ).EDB( NEXTSPIN_PHA ).STICS_Len
C
C Adjust quality flags for PHA. Note that this depends on LOGICAL*1 being
C equivalent to BYTE.
C
        SCI(1).EDB( II ).ST_P_Qual = SCI(RECNO_PHA).EDB( NEXTSPIN_PHA ).ST_P_Qual
C
 600  CONTINUE
C
      RETURN
C
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INTEGER FUNCTION INCREMENT(SPIN)
C
C Given a spin number, this function returns the value that must be added to it
C to point to the next spin that contains SWICS data. This value is strictly
C and simply dependent on bit rate. At high bit rate, there is data in the next
C spin, so the increment is 1.  At low bit rate, there is no data in the next
C spin (by the time that this routine has been called the data from pairs of
C spins has already been concatenated, and data appears only in alternate
C spins, so increment is 2.
C
      INCLUDE 'INC_DIR:EDB.INC/NOLIST'
      INTEGER*4 SPIN,NEXTSPIN,RECNO
C      
C
C NEXTSPIN is the spin number of the next spin which contains data. The MODULO
C 60 is neccesary because the last few spins in a science record may need data
C from the low- numbered spins of the next science record. RECNO is the number
C of the science record containing NEXTSPIN.
C
      NEXTSPIN=MOD( SPIN , 60)
      RECNO= SPIN/60 + 1
C
      IF(SCI( RECNO ).EDB( NEXTSPIN ).BitRate)THEN
        INCREMENT=1
      ELSE
        INCREMENT=2
      ENDIF
C
      RETURN
      END
