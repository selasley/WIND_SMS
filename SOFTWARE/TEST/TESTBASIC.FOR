	PROGRAM TESTBASIC
        INCLUDE 'INC_DIR:EDB.INC/LIST'
        INCLUDE	'INC_DIR:SW_CODES.INC/LIST'
        INCLUDE	'INC_DIR:WIND_Lun.INC/LIST'
        INCLUDE 'INC_DIR:L1READ.INC/LIST'
        INTEGER*4 SCI_REC_NUMN,SCI_REC_NUMO,STATUS,Record_Size,L1_LUNN/14/,
     &            L1_LUNO/15/
        CHARACTER CTYPE*3/'OLD'/,FIL1NAMEN*80/'L1FNEW'/,FIL1NAMEO*80/'L1FOLD'/
C
        INCLUDE	'($SSDEF)/LIST'

	RECORD /CORE/ basicO, basicN

        INTEGER FORTERR,RMSSTC,RMSSTV,LUN,VAXERR
	character*30 format_time
	integer spin
	common/junk/l1fmt
	integer l1fmt/2/
	logical differ(0:59)

!     ------------------------------------------------------------------------


        Record_Size	= 32766			! Max.length of WIND data record

        TYPE *,'OPENING ',FIL1NAMEN
        OPEN (  UNIT	= L1_LunN,
     +		NAME	= FIL1NAMEN,
     +		CARRIAGECONTROL='NONE',
     +		READONLY,
     +		FORM	= 'FORMATTED',
     +		TYPE	= cTYPE,
     +		RECL	= Record_Size,
     +		ERR	= 2000,			! Return if error
     +		IOSTAT	= STATUS)

        IF (STATUS .EQ. 0)	STATUS = SS$_NORMAL

        TYPE *,'OPENING ',FIL1NAMEO
        OPEN (  UNIT	= L1_LunO,
     +		NAME	= FIL1NAMEO,
     +		CARRIAGECONTROL='NONE',
     +		READONLY,
     +		FORM	= 'FORMATTED',
     +		TYPE	= cTYPE,
     +		RECL	= Record_Size,
     +		ERR	= 2000,			! Return if error
     +		IOSTAT	= STATUS)

        IF (STATUS .EQ. 0)	STATUS = SS$_NORMAL

        DO 100 I=1,1000
c	  l1fmt=2
  	  CALL WIND_L1_READ_SR(L1_LUNN,SCI_REC_NUMN,STATUS)
	  if (status .ne. 1) stop 1
	  CALL WIND_L1_GET_BASIC(L1_LUNN,basicN,STATUS)
C
c	  l1fmt=1
	  CALL WIND_L1_READ_SR(L1_LUNO,SCI_REC_NUMO,STATUS)
	  if (status .ne. 1) stop 2
	  CALL WIND_L1_GET_BASIC(L1_LUNO,basicO,STATUS)
C
          IF(basicO.CCORE .NE. basicN.CCORE)THEN
  	    TYPE*, '********** Record ',SCI_REC_NUMN,' not equal**********'
	    do spin=0,59
	      differ(spin) = .false.
	      if (format_time(basicn.edb(spin).W_Time) .ne. 
     &		format_time(basico.edb(spin).W_Time)) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  W_Time differs'
	        write(12,1012) i,spin,'W_Time ',format_time(basicn.edb(spin).W_Time),
     &		format_time(basico.edb(spin).W_Time)
	      endif
	    if (format_time(basicn.edb(spin).SC_Time) .ne. 
     &		format_time(basico.edb(spin).SC_Time)) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  SC_Time differs'
	        write(12,1012) i,spin,'SC_Time ',format_time(basicn.edb(spin).SC_Time),
     &		format_time(basico.edb(spin).SC_Time)
	      endif
	      if (basicn.edb(spin).EPOCH .ne. basico.edb(spin).EPOCH) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  EPOCH differs'
		write(12,1010) i,spin,'EPOCH ',basicn.edb(spin).EPOCH, basico.edb(spin).EPOCH 
1010	format(1x,'Rec ',i4,'  Spin ',i2,2x,a9,' : ',2f25.2)
1011	format(1x,'Rec ',i4,'  Spin ',i2,2x,a9,' : ',2i10)
1012	format(1x,'Rec ',i4,'  Spin ',i2,2x,a9,' : ',a,5x,a)
	      endif
	      if (basicn.edb(spin).SC_EPOCH .ne. basico.edb(spin).SC_EPOCH) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  SC_EPOCH differs'
		write(12,1010) i,spin,'SC_EPOCH ',basicn.edb(spin).SC_EPOCH, basico.edb(spin).SC_EPOCH 
	      endif
	      if (basicn.edb(spin).BYTE_SF .ne. basico.edb(spin).BYTE_SF) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  BYTE_SF differs'
		write(12,1011) i,spin,'BYTE_SF ',basicn.edb(spin).BYTE_SF, basico.edb(spin).BYTE_SF 
	      endif
	      if (basicn.edb(spin).SFperBlock .ne. basico.edb(spin).SFperBlock) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  SFperBlock differs'
		write(12,1011) i,spin,'SFperBlock ',basicn.edb(spin).SFperBlock, basico.edb(spin).SFperBlock 
	      endif
	      if (basicn.edb(spin).MeaSpin .ne. basico.edb(spin).MeaSpin) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  MeaSpin differs'
		write(12,1011) i,spin,'MeaSpin ',basicn.edb(spin).MeaSpin, basico.edb(spin).MeaSpin 
	      endif
	      if (basicn.edb(spin).SpinCnt .ne. basico.edb(spin).SpinCnt) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  SpinCnt differs'
		write(12,1011) i,spin,'SpinCnt ',basicn.edb(spin).SpinCnt, basico.edb(spin).SpinCnt 
	      endif
	      if (basicn.edb(spin).SWICS_Len .ne. basico.edb(spin).SWICS_Len) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  SWICS_Len differs'
		write(12,1011) i,spin,'SWICS_Len ',basicn.edb(spin).SWICS_Len, basico.edb(spin).SWICS_Len 
	      endif
	      if (basicn.edb(spin).STICS_Len .ne. basico.edb(spin).STICS_Len) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  STICS_Len differs'
		write(12,1011) i,spin,'STICS_Len ',basicn.edb(spin).STICS_Len, basico.edb(spin).STICS_Len 
	      endif
	      if (basicn.edb(spin).MASS_Len .ne. basico.edb(spin).MASS_Len) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  MASS_Len differs'
		write(12,1011) i,spin,'MASS_Len ',basicn.edb(spin).MASS_Len, basico.edb(spin).MASS_Len 
	      endif
	      if (basicn.edb(spin).SWICS_RATE_Size .ne. basico.edb(spin).SWICS_RATE_Size) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  SWICS_RATE_Size differs'
		write(12,1011) i,spin,'SWICS_RATE_Size ',basicn.edb(spin).SWICS_RATE_Size, basico.edb(spin).SWICS_RATE_Size 
	      endif
	      if (basicn.edb(spin).STICS_RATE_Size .ne. basico.edb(spin).STICS_RATE_Size) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  STICS_RATE_Size differs'
		write(12,1011) i,spin,'STICS_RATE_Size ',basicn.edb(spin).STICS_RATE_Size, basico.edb(spin).STICS_RATE_Size 
	      endif
	      if (basicn.edb(spin).SpareTime .ne. basico.edb(spin).SpareTime) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  SpareTime differs'
		write(12,1011) i,spin,'SpareTime ',basicn.edb(spin).SpareTime, basico.edb(spin).SpareTime 
	      endif
	      if (basicn.edb(spin).SYNC(1) .ne. basico.edb(spin).SYNC(1)) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  SYNC(1) differs'
		write(12,1011) i,spin,'SYNC(1) ',basicn.edb(spin).SYNC(1), basico.edb(spin).SYNC(1) 
	      endif
	      if (basicn.edb(spin).SYNC(2) .ne. basico.edb(spin).SYNC(2)) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  SYNC(2) differs'
		write(12,1011) i,spin,'SYNC(2) ',basicn.edb(spin).SYNC(2), basico.edb(spin).SYNC(2) 
	      endif
	      if (basicn.edb(spin).BitRate .ne. basico.edb(spin).BitRate) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  BitRate differs'
		write(12,1011) i,spin,'BitRate ',basicn.edb(spin).BitRate, basico.edb(spin).BitRate 
	      endif
	      if (basicn.edb(spin).HDB_FLAG .ne. basico.edb(spin).HDB_FLAG) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  HDB_FLAG differs'
		write(12,1011) i,spin,'HDB_FLAG ',basicn.edb(spin).HDB_FLAG, basico.edb(spin).HDB_FLAG 
	      endif
	      if (basicn.edb(spin).RAMCheck .ne. basico.edb(spin).RAMCheck) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  RAMCheck differs'
		write(12,1011) i,spin,'RAMCheck ',basicn.edb(spin).RAMCheck, basico.edb(spin).RAMCheck 
	      endif
	      if (basicn.edb(spin).SWICSPower .ne. basico.edb(spin).SWICSPower) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  SWICSPower differs'
		write(12,1011) i,spin,'SWICSPower ',basicn.edb(spin).SWICSPower, basico.edb(spin).SWICSPower 
	      endif
	      if (basicn.edb(spin).STICSPower .ne. basico.edb(spin).STICSPower) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  STICSPower differs'
		write(12,1011) i,spin,'STICSPower ',basicn.edb(spin).STICSPower, basico.edb(spin).STICSPower 
	      endif
	      if (basicn.edb(spin).MASSPower .ne. basico.edb(spin).MASSPower) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  MASSPower differs'
		write(12,1011) i,spin,'MASSPower ',basicn.edb(spin).MASSPower, basico.edb(spin).MASSPower 
	      endif
	      if (basicn.edb(spin).ValCmdFl .ne. basico.edb(spin).ValCmdFl) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  ValCmdFl differs'
		write(12,1011) i,spin,'ValCmdFl ',basicn.edb(spin).ValCmdFl, basico.edb(spin).ValCmdFl 
	      endif
	      if (basicn.edb(spin).InvCmdFl .ne. basico.edb(spin).InvCmdFl) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  InvCmdFl differs'
		write(12,1011) i,spin,'InvCmdFl ',basicn.edb(spin).InvCmdFl, basico.edb(spin).InvCmdFl 
	      endif
	      if (basicn.edb(spin).CmdErrFl .ne. basico.edb(spin).CmdErrFl) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  CmdErrFl differs'
		write(12,1011) i,spin,'CmdErrFl ',basicn.edb(spin).CmdErrFl, basico.edb(spin).CmdErrFl 
	      endif
	      if (basicn.edb(spin).SWICS_HP .ne. basico.edb(spin).SWICS_HP) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  SWICS_HP differs'
		write(12,1011) i,spin,'SWICS_HP ',basicn.edb(spin).SWICS_HP, basico.edb(spin).SWICS_HP 
	      endif
	      if (basicn.edb(spin).SWICS_CP .ne. basico.edb(spin).SWICS_CP) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  SWICS_CP differs'
		write(12,1011) i,spin,'SWICS_CP ',basicn.edb(spin).SWICS_CP, basico.edb(spin).SWICS_CP 
	      endif
	      if (basicn.edb(spin).STICS_HP .ne. basico.edb(spin).STICS_HP) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  STICS_HP differs'
		write(12,1011) i,spin,'STICS_HP ',basicn.edb(spin).STICS_HP, basico.edb(spin).STICS_HP 
	      endif
	      if (basicn.edb(spin).STICS_CP .ne. basico.edb(spin).STICS_CP) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  STICS_CP differs'
		write(12,1011) i,spin,'STICS_CP ',basicn.edb(spin).STICS_CP, basico.edb(spin).STICS_CP 
	      endif
	      if (basicn.edb(spin).MASS_CP .ne. basico.edb(spin).MASS_CP) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  MASS_CP differs'
		write(12,1011) i,spin,'MASS_CP ',basicn.edb(spin).MASS_CP, basico.edb(spin).MASS_CP 
	      endif
	      if (basicn.edb(spin).MDisChar .ne. basico.edb(spin).MDisChar) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  MDisChar differs'
		write(12,1011) i,spin,'MDisChar ',basicn.edb(spin).MDisChar, basico.edb(spin).MDisChar 
	      endif
	      if (basicn.edb(spin).DeadTime .ne. basico.edb(spin).DeadTime) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  DeadTime differs'
		write(12,1011) i,spin,'DeadTime ',basicn.edb(spin).DeadTime, basico.edb(spin).DeadTime 
	      endif
	      if (basicn.edb(spin).WDisChar .ne. basico.edb(spin).WDisChar) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  WDisChar differs'
		write(12,1011) i,spin,'WDisChar ',basicn.edb(spin).WDisChar, basico.edb(spin).WDisChar 
	      endif
	      if (basicn.edb(spin).MHStep .ne. basico.edb(spin).MHStep) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  MHStep differs'
		write(12,1011) i,spin,'MHStep ',basicn.edb(spin).MHStep, basico.edb(spin).MHStep 
	      endif
	      if (basicn.edb(spin).MSPStep .ne. basico.edb(spin).MSPStep) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  MSPStep differs'
		write(12,1011) i,spin,'MSPStep ',basicn.edb(spin).MSPStep, basico.edb(spin).MSPStep 
	      endif
	      if (basicn.edb(spin).MSTStep .ne. basico.edb(spin).MSTStep) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  MSTStep differs'
		write(12,1011) i,spin,'MSTStep ',basicn.edb(spin).MSTStep, basico.edb(spin).MSTStep 
	      endif
	      if (basicn.edb(spin).WPStep .ne. basico.edb(spin).WPStep) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  WPStep differs'
		write(12,1011) i,spin,'WPStep ',basicn.edb(spin).WPStep, basico.edb(spin).WPStep 
	      endif
	      if (basicn.edb(spin).WSPStep .ne. basico.edb(spin).WSPStep) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  WSPStep differs'
		write(12,1011) i,spin,'WSPStep ',basicn.edb(spin).WSPStep, basico.edb(spin).WSPStep 
	      endif
	      if (basicn.edb(spin).WSTStep .ne. basico.edb(spin).WSTStep) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  WSTStep differs'
		write(12,1011) i,spin,'WSTStep ',basicn.edb(spin).WSTStep, basico.edb(spin).WSTStep 
	      endif
	      if (basicn.edb(spin).SSPStep .ne. basico.edb(spin).SSPStep) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  SSPStep differs'
		write(12,1011) i,spin,'SSPStep ',basicn.edb(spin).SSPStep, basico.edb(spin).SSPStep 
	      endif
	      if (basicn.edb(spin).SSTStep .ne. basico.edb(spin).SSTStep) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  SSTStep differs'
		write(12,1011) i,spin,'SSTStep ',basicn.edb(spin).SSTStep, basico.edb(spin).SSTStep 
	      endif
	      if (basicn.edb(spin).Qual .ne. basico.edb(spin).Qual) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  Qual differs'
		write(12,1011) i,spin,'Qual ',basicn.edb(spin).Qual, basico.edb(spin).Qual 
	      endif
	      if (basicn.edb(spin).CORE_Qual .ne. basico.edb(spin).CORE_Qual) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  CORE_Qual differs'
		write(12,1011) i,spin,'CORE_Qual ',basicn.edb(spin).CORE_Qual, basico.edb(spin).CORE_Qual 
	      endif
c	      if (basicn.edb(spin).SW_R_Qual .ne. basico.edb(spin).SW_R_Qual) then
c		differ(spin) = .true.
c		write(6,*) '*** Spin',spin,'  SW_R_Qual differs'
c		write(12,1011) i,spin,'SW_R_Qual ',basicn.edb(spin).SW_R_Qual, basico.edb(spin).SW_R_Qual 
c	      endif
c	      if (basicn.edb(spin).ST_R_Qual .ne. basico.edb(spin).ST_R_Qual) then
c		differ(spin) = .true.
c		write(6,*) '*** Spin',spin,'  ST_R_Qual differs'
c		write(12,1011) i,spin,'ST_R_Qual ',basicn.edb(spin).ST_R_Qual, basico.edb(spin).ST_R_Qual 
c	      endif
c	      if (basicn.edb(spin).MA_R_Qual .ne. basico.edb(spin).MA_R_Qual) then
c		differ(spin) = .true.
c		write(6,*) '*** Spin',spin,'  MA_R_Qual differs'
c		write(12,1011) i,spin,'MA_R_Qual ',basicn.edb(spin).MA_R_Qual, basico.edb(spin).MA_R_Qual 
c	      endif
	      if (basicn.edb(spin).SW_P_Qual .ne. basico.edb(spin).SW_P_Qual) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  SW_P_Qual differs'
		write(12,1011) i,spin,'SW_P_Qual ',basicn.edb(spin).SW_P_Qual, basico.edb(spin).SW_P_Qual 
	      endif
	      if (basicn.edb(spin).ST_P_Qual .ne. basico.edb(spin).ST_P_Qual) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  ST_P_Qual differs'
		write(12,1011) i,spin,'ST_P_Qual ',basicn.edb(spin).ST_P_Qual, basico.edb(spin).ST_P_Qual 
	      endif
	      if (basicn.edb(spin).MA_P_Qual .ne. basico.edb(spin).MA_P_Qual) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  MA_P_Qual differs'
		write(12,1011) i,spin,'MA_P_Qual ',basicn.edb(spin).MA_P_Qual, basico.edb(spin).MA_P_Qual 
	      endif
	    enddo
c	    if (differ(spin)) then
c	      call dump_spin('NEW',i,basicn.edb(spin),spin)
c	      call dump_spin('OLD',i,basico.edb(spin),spin)
c	    endif
	  ENDIF
c	  call dump_basic('NEW',i,basicn)
c	  call dump_basic('OLD',i,basico)
  100   CONTINUE       
        STOP
2000    TYPE 2001, STATUS
2001    FORMAT(' ***ERROR in WIND_OPEN_L1, STATUS =', I6, ' ***')
        CALL ERRSNS(FORTERR,RMSSTC,RMSSTV,LUN,VAXERR)
        CALL LIB$SIGNAL(%VAL(RMSSTC))
        CALL LIB$SIGNAL(%VAL(VAXERR))
C
	END
	subroutine dump_basic(which,recn,basicrec)
	implicit none
        INCLUDE 'INC_DIR:EDB.INC/NOLIST'
	RECORD /CORE/ basicrec
	character*(*) which
	integer recn,spin
	character*30 format_time
c
	return
	end
	subroutine dump_spin(which,recn,edbrec,spin)
        INCLUDE 'INC_DIR:EDB.INC/LIST'
	character which*(*)
	integer recn,spin
	record /e/ edbrec
	return
	end
	character*30 function format_time(timestrc)
	implicit none
        INCLUDE 'INC_DIR:EDB.INC/NOLIST'
	record /t/ timestrc
	character*30 temp
	write(temp,1001) timestrc.year,timestrc.doy,timestrc.hr,timestrc.min,
     &			 timestrc.sec,timestrc.csec
1001	format(6i5)
	format_time = temp
	return
	end
