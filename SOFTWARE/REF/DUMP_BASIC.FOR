	subroutine dump_basic(iout,basicrec,spin)
	implicit none
        INCLUDE 'INC_DIR:EDBDEF.INC/NOLIST'
	RECORD /CORE/ basicrec
	integer iout,spin
	character*30 format_time
C
        write(iout,1012) 'W_Time ',format_time(basicrec.edb(spin).W_Time)
        write(iout,1012) 'SC_Time ',format_time(basicrec.edb(spin).SC_Time)
	write(iout,1010) 'EPOCH ',basicrec.edb(spin).EPOCH
	write(iout,1010) 'SC_EPOCH ',basicrec.edb(spin).SC_EPOCH
	write(iout,1011) 'BYTE_SF ',basicrec.edb(spin).BYTE_SF
	write(iout,1011) 'SFperBlock ',basicrec.edb(spin).SFperBlock
	write(iout,1011) 'MeaSpin ',basicrec.edb(spin).MeaSpin
	write(iout,1011) 'SpinCnt ',basicrec.edb(spin).SpinCnt
	write(iout,1011) 'SWICS_Len ',basicrec.edb(spin).SWICS_Len
	write(iout,1011) 'STICS_Len ',basicrec.edb(spin).STICS_Len
	write(iout,1011) 'MASS_Len ',basicrec.edb(spin).MASS_Len
	write(iout,1011) 'SWICS_RATE_Size ',basicrec.edb(spin).SWICS_RATE_Size
	write(iout,1011) 'STICS_RATE_Size ',basicrec.edb(spin).STICS_RATE_Size
	write(iout,1011) 'SpareTime ',basicrec.edb(spin).SpareTime
	write(iout,1011) 'SYNC(1) ',basicrec.edb(spin).SYNC(1)
	write(iout,1011) 'SYNC(2) ',basicrec.edb(spin).SYNC(2)
	write(iout,1011) 'BitRate ',basicrec.edb(spin).BitRate
	write(iout,1011) 'HDB_FLAG ',basicrec.edb(spin).HDB_FLAG
	write(iout,1011) 'RAMCheck ',basicrec.edb(spin).RAMCheck
	write(iout,1011) 'SWICSPower ',basicrec.edb(spin).SWICSPower
	write(iout,1011) 'STICSPower ',basicrec.edb(spin).STICSPower
	write(iout,1011) 'MASSPower ',basicrec.edb(spin).MASSPower
	write(iout,1011) 'ValCmdFl ',basicrec.edb(spin).ValCmdFl
	write(iout,1011) 'InvCmdFl ',basicrec.edb(spin).InvCmdFl
	write(iout,1011) 'CmdErrFl ',basicrec.edb(spin).CmdErrFl
	write(iout,1011) 'SWICS_HP ',basicrec.edb(spin).SWICS_HP
	write(iout,1011) 'SWICS_CP ',basicrec.edb(spin).SWICS_CP
	write(iout,1011) 'STICS_HP ',basicrec.edb(spin).STICS_HP
	write(iout,1011) 'STICS_CP ',basicrec.edb(spin).STICS_CP
	write(iout,1011) 'MASS_CP ',basicrec.edb(spin).MASS_CP
	write(iout,1011) 'MDisChar ',basicrec.edb(spin).MDisChar
	write(iout,1011) 'DeadTime ',basicrec.edb(spin).DeadTime
	write(iout,1011) 'WDisChar ',basicrec.edb(spin).WDisChar
	write(iout,1011) 'MHStep ',basicrec.edb(spin).MHStep
	write(iout,1011) 'MSPStep ',basicrec.edb(spin).MSPStep
	write(iout,1011) 'MSTStep ',basicrec.edb(spin).MSTStep
	write(iout,1011) 'WPStep ',basicrec.edb(spin).WPStep
	write(iout,1011) 'WSPStep ',basicrec.edb(spin).WSPStep
	write(iout,1011) 'WSTStep ',basicrec.edb(spin).WSTStep
	write(iout,1011) 'SSPStep ',basicrec.edb(spin).SSPStep
	write(iout,1011) 'SSTStep ',basicrec.edb(spin).SSTStep
	write(iout,1011) 'Qual ',basicrec.edb(spin).Qual
	write(iout,1011) 'CORE_Qual ',basicrec.edb(spin).CORE_Qual
	write(iout,1011) 'SW_BR_Qual',basicrec.edb(spin).SW_BR_Qual
	write(iout,1011) 'SW_MR_Qual',basicrec.edb(spin).SW_MR_Qual
	write(iout,1011) 'SW_ME1_Qual',basicrec.edb(spin).SW_ME1_Qual
	write(iout,1011) 'SW_ME2_Qual',basicrec.edb(spin).SW_ME2_Qual
	write(iout,1011) 'SW_ER_Qual',basicrec.edb(spin).SW_ER_Qual
	write(iout,1011) 'ST_HMR_Qual',basicrec.edb(spin).ST_HMR_Qual
	write(iout,1011) 'ST_SMR_Qual',basicrec.edb(spin).ST_SMR_Qual
	write(iout,1011) 'ST_BR_Qual',basicrec.edb(spin).ST_BR_Qual
	write(iout,1011) 'ST_OMR_Qual',basicrec.edb(spin).ST_OMR_Qual
	write(iout,1011) 'ST_ER_Qual',basicrec.edb(spin).ST_ER_Qual
	write(iout,1011) 'MA_UER_Qual',basicrec.edb(spin).MA_UER_Qual
	write(iout,1011) 'MA_SER_Qual',basicrec.edb(spin).MA_SER_Qual
	write(iout,1011) 'MA_BR_Qual',basicrec.edb(spin).MA_BR_Qual
	write(iout,1011) 'MA_MR_Qual',basicrec.edb(spin).MA_MR_Qual
	write(iout,1011) 'SW_P_Qual',basicrec.edb(spin).SW_P_Qual
	write(iout,1011) 'ST_P_Qual',basicrec.edb(spin).ST_P_Qual
	write(iout,1011) 'MA_P_Qual',basicrec.edb(spin).MA_P_Qual
1010	format(1x,2x,a12,' = ',f25.2)
1011	format(1x,2x,a12,' = ',i10)
1012	format(1x,2x,a12,' = ',a)
1013	format(1x,2x,a12,' = ',l10)
	return
	END
