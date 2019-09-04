	SUBROUTINE WIND_L1_READ_SR( unit, scirec_number, header, status )
C+
C
C FUNCTIONAL DESCRIPTION:
C
C    This program reads the next Science Record from the specified L1 data file
C    and stores it internally.  It returns the header record to the call.  It
C    does not transfer any other data to the caller; this is done by the
C    various WIND_L1_GET* routines.
C
C FORMAL PARAMETERS:
C
C     unit:
C	  An INTEGER*4 variable indicating the Level 1 file to be read.
C
C     scirec_number:
C	  An INTEGER*4 variable which will receive the number of the science
C	  record which is read, if successful.
C
C     header:
C	  A record variable of structure type ST_L1_SR_HEADER which will
C	  receive the header of the next science record.
C
C     status:
C	  An INTEGER*4 variable which will receive the read completion status.
C	  A 1 indicates success.
C
C COMMON BLOCKS:
C
C     L1INFO	  contains arrays of unit numbers, each one's current science
C		  record, and other control information
C
C DESIGN:
C
C     A complete science record is read in.  If reading any part of a science
C     record gives an error no further read operations are done and the error
C     code is returned.  If the first record is of the wrong record type it is
C     discarded and reads are done until a record of the correct type is read.
C
C     Older format L1 files with different structures are accomodated.  They are
C     detected by having an alternate record length for a particular record.
C     For older formats the field elements of the older structure are copied
C     into the newer structure.  This is done transparently to the calling
C     program.
C
C
C AUTHOR(S):
C
C	 L. Bleau
C
C Created:
C
C	 12-SEP-1994    L. Bleau
C
C This version:   1.9    7-FEB-1995
C
C Revised:
C
C Ver     Date     | Name  | Description
C -----------------+-------+----------------------------------------------------
C 1.1  27-SEP-1994 |  LRB  | changed code to allow reading of older format basic
C                            core records; read into a buffer, then transfer to
C                            fields of the structures (old or new), if old copy
C                            fields individually into new structure
C 1.1  13-OCT-1994 |  LRB  | changed comments; return error if record incorrect
C                            length
C 1.2  20-OCT-1994 |  LRB  | add extra read for header, change parameter list to
C                            return header to caller
C 1.3  24-OCT-1994 |  LRB  | zero out structures before reading in record
C 1.4  24-OCT-1994 |  LRB  | for rates, use substring spec to do READs so char
C                            variable doesn't get blank filled
C 1.5  25-OCT-1994 |  LRB  | take care of special-case zero-length rates records
C 1.6   5-DEC-1994 |  LRB  | L1 format has been changed (is now v 02), so add
C                            capability of reading previous L1 formats (v 01
C                            and before); change adds TLM_Mode field to CORE
C 1.7  13-DEC-1994 |  LRB  | L1 format has been changed (is now v 03), so add
C                            capability of reading previous L1 formats; change
C                            adds room for a sixth HK (parameter MAX_HK); add
C                            DATA statement for new variable max_version, which
C                            is declared in L1READ.INC
C 1.8  18-JAN-1995 |  LRB  | removed DATA stmt for max_version, put in L1READ.INC
C 1.9   7-FEB-1995 |  LRB  | L1 format has been changed (is now v 04), only the
C                            sci rec hdr has been changed, add code for backward
C                            compatability; allow routine to continue process-
C                            ing even with later version headers
C-
CDEC$	IDENT   '1.9'

	IMPLICIT NONE

	INCLUDE '($SSDEF)/NOLIST'
	INCLUDE '($RMSDEF)/NOLIST'
	INCLUDE 'INC_DIR:EDBDEF.INC/NOLIST'
	INCLUDE 'INC_DIR:L1FMT.INC/NOLIST'
	INCLUDE 'INC_DIR:L1READ.INC/NOLIST'

	INTEGER*4 unit, scirec_number, status
	RECORD /ST_L1_SR_HEADER/ header
	CHARACTER version*2

	INTEGER ii,jj,iii,nbytes,spin,L1_unit,transfer_bytes
	INTEGER forterr,rmsstc,rmsstv,lun,vaxerr
	RECORD /HKeep_old/ hkold
	RECORD /CORE_V1/ core_v1
	RECORD /OLD_CORE/ old_core
C
C First check if value of unit is within range
C
	IF (unit .lt. 1 .or. unit .gt. maxunits) THEN
	  status = SS$_IVCHAN
	  RETURN
	ENDIF
C
C Read one (1) science record
C First fetch internal Fortran unit number (for now they're the same)
C Then increment science record number and store into user's argument
C
	L1_unit = L1_units(unit)
D	TYPE *,'READ_SR: L1_unit, L1_units(unit), unit=',L1_unit,L1_units(unit),unit
	scirecnum(unit) = scirecnum(unit) + 1
	scirec_number = scirecnum(unit)
D	type *,'Reading science record ',scirec_number,' from unit ',L1_unit
C
C Read in the header record
C Check if it has 'HDRR' as its first 4 characeters.  If so it is a science
C record header; give it to the caller, store it internally, and read next
C record.  Also check header record's length.  If it is the same length as the
C caller's structure it is safe to pass it back to him.  If the header record
C is *longer* than the caller's data area pass back only the number of bytes
C that will fit in the caller's structure.  A warning message has already been
C output telling the user some header information will be lost, and to
C recompile and relink the application to remedy the problem.
C
C If this record does not begin with 'HDRR'  it does not have a science record
C headers or is improperly positioned.  Skip ahead until we reach a valid
C science record header.
C 
100	READ( L1_unit, 1002, IOSTAT=forterr, ERR=3000 ) nbytes,buffer(1:nbytes)
	  IF (forterr .ne. 0) GOTO 3000
	  IF (buffer(1:4) .eq. 'HDRR') THEN
            transfer_bytes = MIN(nbytes,LEN(header.c_all))
	    header.c_all = buffer(1:transfer_bytes)
	    READ( L1_unit, 1002, IOSTAT=forterr, ERR=3000 ) nbytes,buffer
	    IF (forterr .ne. 0) GOTO 3000
	  ENDIF
C 
C Now check if the next record, which has already been read, starts with
C 'CORE'.  If it does not it is not a valid start of a science record.  Throw
C it away and read ahead until we find a record of type 'HDRR' or 'CORE'.
C
	IF (buffer(1:4) .ne. 'CORE') GOTO 100
C
C Check the version of hte header record, and fill in missing fields if
C we are reading an earlier version.
C
	version = L1_header(unit).header_vers 
	IF (version .LT. '05') THEN
	  header.zero_swics_pha(1) = 0
	  header.zero_swics_pha(2) = 0
	  header.zero_mass_pha(1) = 0
	  header.zero_mass_pha(2) = 0
	  header.zero_stics_pha(1) = 0
	  header.zero_stics_pha(2) = 0
	ENDIF
	IF (version .LT. '03') THEN
	  header.present(1) = 0
	  header.present(2) = 0
	ENDIF
	L1_sci_rec_header(unit) = header
C
C The Core data record, also known as Basic, has now been read into buffer.
C Copy over depending on format, which is determined by length:
C Length  Ver  Format
C   7384   02  current format, just copy over into L1_SCI record
C   7144   01  previous format, copy into record CORE_V1 then move fields one
C              by one into new format structure
C   6484       old format, copy into record OLD_CORE then move fields one by one
C              into new format structure
C
	IF (forterr .ne. 0) GOTO 3000
	IF (nbytes .eq. 7384 .and. version .ge. '02') THEN
	  L1_SCI(unit).CCORE = buffer(1:nbytes)
	ELSE IF (nbytes .eq. 7144 .and. version .eq. '01') THEN
	  core_v1.CCORE = buffer(1:nbytes)
          DO spin=0,59
	    L1_SCI(unit).EDB(spin).W_Time = core_v1.EDB(spin).W_Time
	    L1_SCI(unit).EDB(spin).SC_Time = core_v1.EDB(spin).SC_Time
	    L1_SCI(unit).EDB(spin).EPOCH = core_v1.EDB(spin).EPOCH
	    L1_SCI(unit).EDB(spin).SC_EPOCH = core_v1.EDB(spin).SC_EPOCH
	    L1_SCI(unit).EDB(spin).BYTE_SF = core_v1.EDB(spin).BYTE_SF
	    L1_SCI(unit).EDB(spin).SFperBlock = core_v1.EDB(spin).SFperBlock
	    L1_SCI(unit).EDB(spin).MeaSpin = core_v1.EDB(spin).MeaSpin
	    L1_SCI(unit).EDB(spin).SpinCnt = core_v1.EDB(spin).SpinCnt
	    L1_SCI(unit).EDB(spin).SWICS_Len = core_v1.EDB(spin).SWICS_Len
	    L1_SCI(unit).EDB(spin).STICS_Len = core_v1.EDB(spin).STICS_Len
	    L1_SCI(unit).EDB(spin).MASS_Len = core_v1.EDB(spin).MASS_Len
	    L1_SCI(unit).EDB(spin).SWICS_RATE_Size = core_v1.EDB(spin).SWICS_RATE_Size
	    L1_SCI(unit).EDB(spin).STICS_RATE_Size = core_v1.EDB(spin).STICS_RATE_Size
	    L1_SCI(unit).EDB(spin).SpareTime = core_v1.EDB(spin).SpareTime
	    L1_SCI(unit).EDB(spin).TLM_Mode = 256
	    L1_SCI(unit).EDB(spin).BitRate = core_v1.EDB(spin).BitRate
	    L1_SCI(unit).EDB(spin).HDB_FLAG = core_v1.EDB(spin).HDB_FLAG
	    L1_SCI(unit).EDB(spin).RAMCheck = core_v1.EDB(spin).RAMCheck
	    L1_SCI(unit).EDB(spin).SWICSPower = core_v1.EDB(spin).SWICSPower
	    L1_SCI(unit).EDB(spin).STICSPower = core_v1.EDB(spin).STICSPower
	    L1_SCI(unit).EDB(spin).MASSPower = core_v1.EDB(spin).MASSPower
	    L1_SCI(unit).EDB(spin).ValCmdFl = core_v1.EDB(spin).ValCmdFl
	    L1_SCI(unit).EDB(spin).InvCmdFl = core_v1.EDB(spin).InvCmdFl
	    L1_SCI(unit).EDB(spin).CmdErrFl = core_v1.EDB(spin).CmdErrFl
	    L1_SCI(unit).EDB(spin).SWICS_HP = core_v1.EDB(spin).SWICS_HP
	    L1_SCI(unit).EDB(spin).SWICS_CP = core_v1.EDB(spin).SWICS_CP
	    L1_SCI(unit).EDB(spin).STICS_HP = core_v1.EDB(spin).STICS_HP
	    L1_SCI(unit).EDB(spin).STICS_CP = core_v1.EDB(spin).STICS_CP
	    L1_SCI(unit).EDB(spin).MASS_CP = core_v1.EDB(spin).MASS_CP
	    L1_SCI(unit).EDB(spin).MDisChar = core_v1.EDB(spin).MDisChar
	    L1_SCI(unit).EDB(spin).DeadTime = core_v1.EDB(spin).DeadTime
	    L1_SCI(unit).EDB(spin).WDisChar = core_v1.EDB(spin).WDisChar
	    L1_SCI(unit).EDB(spin).MHStep = core_v1.EDB(spin).MHStep
	    L1_SCI(unit).EDB(spin).MSPStep = core_v1.EDB(spin).MSPStep
	    L1_SCI(unit).EDB(spin).MSTStep = core_v1.EDB(spin).MSTStep
	    L1_SCI(unit).EDB(spin).WPStep = core_v1.EDB(spin).WPStep
	    L1_SCI(unit).EDB(spin).WSPStep = core_v1.EDB(spin).WSPStep
	    L1_SCI(unit).EDB(spin).WSTStep = core_v1.EDB(spin).WSTStep
	    L1_SCI(unit).EDB(spin).SSPStep = core_v1.EDB(spin).SSPStep
	    L1_SCI(unit).EDB(spin).SSTStep = core_v1.EDB(spin).SSTStep
	    L1_SCI(unit).EDB(spin).Qual = core_v1.EDB(spin).Qual
	    L1_SCI(unit).EDB(spin).CORE_Qual = core_v1.EDB(spin).CORE_Qual
	    L1_SCI(unit).EDB(spin).SW_BR_Qual = core_v1.EDB(spin).SW_BR_Qual
	    L1_SCI(unit).EDB(spin).SW_MR_Qual = core_v1.EDB(spin).SW_MR_Qual
	    L1_SCI(unit).EDB(spin).SW_ME1_Qual = core_v1.EDB(spin).SW_ME1_Qual
	    L1_SCI(unit).EDB(spin).SW_ME2_Qual = core_v1.EDB(spin).SW_ME2_Qual
	    L1_SCI(unit).EDB(spin).SW_ER_Qual = core_v1.EDB(spin).SW_ER_Qual
	    L1_SCI(unit).EDB(spin).ST_HMR_Qual = core_v1.EDB(spin).ST_HMR_Qual
	    L1_SCI(unit).EDB(spin).ST_SMR_Qual = core_v1.EDB(spin).ST_SMR_Qual
	    L1_SCI(unit).EDB(spin).ST_BR_Qual = core_v1.EDB(spin).ST_BR_Qual
	    L1_SCI(unit).EDB(spin).ST_OMR_Qual = core_v1.EDB(spin).ST_OMR_Qual
	    L1_SCI(unit).EDB(spin).ST_ER_Qual = core_v1.EDB(spin).ST_ER_Qual
	    L1_SCI(unit).EDB(spin).MA_UER_Qual = core_v1.EDB(spin).MA_UER_Qual
	    L1_SCI(unit).EDB(spin).MA_SER_Qual = core_v1.EDB(spin).MA_SER_Qual
	    L1_SCI(unit).EDB(spin).MA_BR_Qual = core_v1.EDB(spin).MA_BR_Qual
	    L1_SCI(unit).EDB(spin).MA_MR_Qual = core_v1.EDB(spin).MA_MR_Qual
	    L1_SCI(unit).EDB(spin).SW_P_Qual = core_v1.EDB(spin).SW_P_Qual
	    L1_SCI(unit).EDB(spin).ST_P_Qual = core_v1.EDB(spin).ST_P_Qual
	    L1_SCI(unit).EDB(spin).MA_P_Qual = core_v1.EDB(spin).MA_P_Qual
	  ENDDO
	ELSE IF (nbytes .eq. 6484) THEN
	  old_core.CCORE = buffer(1:nbytes)
          DO spin=0,59
	    L1_SCI(unit).EDB(spin).W_Time = old_core.EDB(spin).W_Time
	    L1_SCI(unit).EDB(spin).SC_Time = old_core.EDB(spin).SC_Time
	    L1_SCI(unit).EDB(spin).EPOCH = old_core.EDB(spin).EPOCH
	    L1_SCI(unit).EDB(spin).SC_EPOCH = old_core.EDB(spin).SC_EPOCH
	    L1_SCI(unit).EDB(spin).BYTE_SF = old_core.EDB(spin).BYTE_SF
	    L1_SCI(unit).EDB(spin).SFperBlock = old_core.EDB(spin).SFperBlock
	    L1_SCI(unit).EDB(spin).MeaSpin = old_core.EDB(spin).MeaSpin
	    L1_SCI(unit).EDB(spin).SpinCnt = old_core.EDB(spin).SpinCnt
	    L1_SCI(unit).EDB(spin).SWICS_Len = old_core.EDB(spin).SWICS_Len
	    L1_SCI(unit).EDB(spin).STICS_Len = old_core.EDB(spin).STICS_Len
	    L1_SCI(unit).EDB(spin).MASS_Len = old_core.EDB(spin).MASS_Len
	    L1_SCI(unit).EDB(spin).SWICS_RATE_Size = old_core.EDB(spin).SWICS_RATE_Size
	    L1_SCI(unit).EDB(spin).STICS_RATE_Size = old_core.EDB(spin).STICS_RATE_Size
	    L1_SCI(unit).EDB(spin).SpareTime = old_core.EDB(spin).SpareTime
	    L1_SCI(unit).EDB(spin).TLM_Mode = 256
	    L1_SCI(unit).EDB(spin).BitRate = old_core.EDB(spin).BitRate
	    L1_SCI(unit).EDB(spin).HDB_FLAG = old_core.EDB(spin).HDB_FLAG
	    L1_SCI(unit).EDB(spin).RAMCheck = old_core.EDB(spin).RAMCheck
	    L1_SCI(unit).EDB(spin).SWICSPower = old_core.EDB(spin).SWICSPower
	    L1_SCI(unit).EDB(spin).STICSPower = old_core.EDB(spin).STICSPower
	    L1_SCI(unit).EDB(spin).MASSPower = old_core.EDB(spin).MASSPower
	    L1_SCI(unit).EDB(spin).ValCmdFl = old_core.EDB(spin).ValCmdFl
	    L1_SCI(unit).EDB(spin).InvCmdFl = old_core.EDB(spin).InvCmdFl
	    L1_SCI(unit).EDB(spin).CmdErrFl = old_core.EDB(spin).CmdErrFl
	    L1_SCI(unit).EDB(spin).SWICS_HP = old_core.EDB(spin).SWICS_HP
	    L1_SCI(unit).EDB(spin).SWICS_CP = old_core.EDB(spin).SWICS_CP
	    L1_SCI(unit).EDB(spin).STICS_HP = old_core.EDB(spin).STICS_HP
	    L1_SCI(unit).EDB(spin).STICS_CP = old_core.EDB(spin).STICS_CP
	    L1_SCI(unit).EDB(spin).MASS_CP = old_core.EDB(spin).MASS_CP
	    L1_SCI(unit).EDB(spin).MDisChar = old_core.EDB(spin).MDisChar
	    L1_SCI(unit).EDB(spin).DeadTime = old_core.EDB(spin).DeadTime
	    L1_SCI(unit).EDB(spin).WDisChar = old_core.EDB(spin).WDisChar
	    L1_SCI(unit).EDB(spin).MHStep = old_core.EDB(spin).MHStep
	    L1_SCI(unit).EDB(spin).MSPStep = old_core.EDB(spin).MSPStep
	    L1_SCI(unit).EDB(spin).MSTStep = old_core.EDB(spin).MSTStep
	    L1_SCI(unit).EDB(spin).WPStep = old_core.EDB(spin).WPStep
	    L1_SCI(unit).EDB(spin).WSPStep = old_core.EDB(spin).WSPStep
	    L1_SCI(unit).EDB(spin).WSTStep = old_core.EDB(spin).WSTStep
	    L1_SCI(unit).EDB(spin).SSPStep = old_core.EDB(spin).SSPStep
	    L1_SCI(unit).EDB(spin).SSTStep = old_core.EDB(spin).SSTStep
	    L1_SCI(unit).EDB(spin).Qual = old_core.EDB(spin).Qual
	    L1_SCI(unit).EDB(spin).CORE_Qual = old_core.EDB(spin).CORE_Qual
	    L1_SCI(unit).EDB(spin).SW_BR_Qual = old_core.EDB(spin).SW_R_Qual
	    L1_SCI(unit).EDB(spin).SW_MR_Qual = old_core.EDB(spin).SW_R_Qual
	    L1_SCI(unit).EDB(spin).SW_ME1_Qual = old_core.EDB(spin).SW_R_Qual
	    L1_SCI(unit).EDB(spin).SW_ME2_Qual = old_core.EDB(spin).SW_R_Qual
	    L1_SCI(unit).EDB(spin).SW_ER_Qual = old_core.EDB(spin).SW_R_Qual
	    L1_SCI(unit).EDB(spin).ST_HMR_Qual = old_core.EDB(spin).ST_R_Qual
	    L1_SCI(unit).EDB(spin).ST_SMR_Qual = old_core.EDB(spin).ST_R_Qual
	    L1_SCI(unit).EDB(spin).ST_BR_Qual = old_core.EDB(spin).ST_R_Qual
	    L1_SCI(unit).EDB(spin).ST_OMR_Qual = old_core.EDB(spin).ST_R_Qual
	    L1_SCI(unit).EDB(spin).ST_ER_Qual = old_core.EDB(spin).ST_R_Qual
	    L1_SCI(unit).EDB(spin).MA_UER_Qual = old_core.EDB(spin).MA_R_Qual
	    L1_SCI(unit).EDB(spin).MA_SER_Qual = old_core.EDB(spin).MA_R_Qual
	    L1_SCI(unit).EDB(spin).MA_BR_Qual = old_core.EDB(spin).MA_R_Qual
	    L1_SCI(unit).EDB(spin).MA_MR_Qual = old_core.EDB(spin).MA_R_Qual
	    L1_SCI(unit).EDB(spin).SW_P_Qual = old_core.EDB(spin).SW_P_Qual
	    L1_SCI(unit).EDB(spin).ST_P_Qual = old_core.EDB(spin).ST_P_Qual
	    L1_SCI(unit).EDB(spin).MA_P_Qual = old_core.EDB(spin).MA_P_Qual
	  ENDDO
        ELSE
	  status = 0
	  RETURN
	ENDIF
C
C Read in the Housekeeping data blocks (HKs, MAX_HK of them) and the HDB
C Put them into BUFFER, then transfer them to proper structure
C Check number of bytes read to determine which structure to use
C
C If this is a version 01 or 02 L1 file there are only five HKs per L1 record.
C Copy this from buffer into record L1_HK since the first 918 bytes are
C identical in structure.
C
C If this is an old version of the Level 1 file, read the Housekeeping
C structures (HK records, to be exact) into a record of type HKeep_old, which
C is 4 bytes shorter than later versions.  Then copy its components into a
C newer record of type HKeep, which includes the count field.  Set the count of
C the number of HKs to 5, the maximum, since we don't know how many there were.
C
C Although there are always MAX_HK HKs in this record, not all of them are
C always valid.
C Length     Ver  Format
C 1100 + 324  03  6 HKs plus 1 HDB
C  918 + 324  01  5 HKs plus 1 HDB
C  914 + 324      5 HKs plub 1 HDB, no HK count field
C
	READ( L1_unit, 1002, IOSTAT=forterr, ERR=3000 ) nbytes,buffer(1:nbytes)
	IF (version .ge. '03' .and. nbytes .eq. 1100 + 324) THEN
C New structure, copy it straight over
	  L1_HKeep(unit).CHK = buffer(1:1100)
	  L1_HDB(unit).CHDB = buffer(1+1100:1100+324)
	ELSE IF (version .ge. '01' .and. nbytes .eq. 918 + 324) THEN
C Version 01 structure, only 5 HKs.  Can copy straight over to version 03
C structure since first part of record is the same.
	  L1_HKeep(unit).CHK(1:918) = buffer(1:918)
	  L1_HDB(unit).CHDB     = buffer(1+918:918+324)
	ELSE IF (nbytes .eq. 914 + 324) THEN
C Old structure, copy into record of old type, then copy the data between old
C and current HK structures for later L1 files; see comment above.
	  hkold.CHK             = buffer(1:914)
	  L1_HDB(unit).CHDB     = buffer(1+914:914+324)
	  L1_HKeep(unit).HKcount = 5
	  DO iii=1,5
	    L1_HKeep(unit).hkset(iii) = hkold.hkset(iii)
	  ENDDO
	ELSE
          TYPE *,'*** INVALID VERSION (',version,') OR HK+HDB RECORD LENGTH (',nbytes,')'
	  status = 0
	  RETURN
	ENDIF
D	type *,'Num of bytes in HK record: ',nbytes
D	type *,'HKcount=',L1_HKeep(unit).HKcount
C
C Read in SWICS rates and PHA by voltage step for all 60 voltage steps
C
	DO II = 0, 59
	  L1_SWICS_RATE(unit).C_SW_RATE(II) = null_buffer(1:LEN(L1_SWICS_RATE(unit).C_SW_RATE(II)))
	  jj = L1_SCI(unit).edb(ii).SWICS_rate_size
	  IF (jj .eq. 0) THEN
	    jj = 4
	  ELSE
	    jj = jj + 5
	  ENDIF
	  READ(L1_unit,1001,IOSTAT=forterr,ERR=3000) L1_SWICS_RATE(unit).C_SW_RATE(II)(1:jj)
	  IF (forterr .ne. 0) GOTO 3000
	  JJ = L1_SCI( unit ).EDB( II ).SWICS_Len * 4 + 4
	  L1_SWICS_PHA(unit).C_SW_PHA(II) = null_buffer(1:LEN(L1_SWICS_PHA(unit).C_SW_PHA(II)))
	  READ( L1_unit, 1001, IOSTAT=forterr, ERR=3000 ) L1_SWICS_PHA(unit).C_SW_PHA(II)( 1:JJ )
	  IF (forterr .ne. 0) GOTO 3000
	END DO
C
C Read in MASS rates and PHA by voltage step for all 60 voltage steps
C
	DO II = 0, 59
	  L1_MASS_RATE(unit).C_MA_RATE(II) = null_buffer(1:LEN(L1_MASS_RATE(unit).C_MA_RATE(II)))
	  READ( L1_unit, 1001, IOSTAT=forterr, ERR=3000 ) L1_MASS_RATE(unit).C_MA_RATE(II)
	  IF (forterr .ne. 0) GOTO 3000
	  JJ = L1_SCI( unit ).EDB( II ).MASS_Len * 2 + 4
	  L1_MASS_PHA(unit).C_MA_PHA(II) = null_buffer(1:LEN(L1_MASS_PHA(unit).C_MA_PHA(II)))
	  READ( L1_unit, 1001, IOSTAT=forterr, ERR=3000 ) L1_MASS_PHA(unit).C_MA_PHA(II)( 1:JJ )
	  IF (forterr .ne. 0) GOTO 3000
	END DO
C
C Read in STICS rates and PHA by voltage step for all 60 voltage steps
C Since STICS rates at low bit rate have been concatenated, the rate records for
C successive spins will contain alternately 180 and zero values.  The core value
C STICS_rate_size, however, gives the index of the highest value stored, NOT
C the number of values.  So, a STICS_rate_size value of 179 indicates 180
C values.  Since rate values are concatenated there must be some means of
C indicating no rate values in a record; a zero is used.  In this case zero
C does NOT indicate one rate value, but none.  (Confused? So was I.)
C
	DO II = 0, 59
	  L1_STICS_RATE(unit).C_ST_RATE(II) = null_buffer(1:LEN(L1_STICS_RATE(unit).C_ST_RATE(II)))
	  jj = L1_SCI(unit).edb(ii).STICS_rate_size
	  IF (jj .eq. 0) THEN
	    jj = 4
	  ELSE
	    jj = jj + 5
	  ENDIF
	  READ( L1_unit, 1001, IOSTAT=forterr, ERR=3000 ) L1_STICS_RATE(unit).C_ST_RATE(II)(1:jj)
	  IF (forterr .ne. 0) GOTO 3000
	  jj = L1_SCI( unit ).EDB( II ).STICS_Len * 4 + 4
	  L1_STICS_PHA(unit).C_ST_PHA(II) = null_buffer(1:LEN(L1_STICS_PHA(unit).C_ST_PHA(II)))
	  READ( L1_unit, 1001, IOSTAT=forterr, ERR=3000 ) L1_STICS_PHA(unit).C_ST_PHA(II)( 1:JJ )
	  IF (forterr .ne. 0) GOTO 3000
	END DO

	status = SS$_NORMAL
	RETURN
C
C Come here for read errors
C Convert Fortran error code into VMS error code and return to caller
C
3000	lun = L1_unit
	CALL ERRSNS(forterr,rmsstc,rmsstv,lun,vaxerr)
	status = rmsstc
D	type *,'Error while reading unit ',L1_unit,', error code=',forterr
	RETURN
C
C Format statements
C
1001	FORMAT( A )
1002	FORMAT( Q, A )
	END
