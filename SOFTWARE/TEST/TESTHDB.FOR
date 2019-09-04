	PROGRAM TESTHDB
        INCLUDE 'INC_DIR:EDB.INC/LIST'
        INCLUDE	'INC_DIR:SW_CODES.INC/LIST'
        INCLUDE	'INC_DIR:WIND_Lun.INC/LIST'
        INCLUDE 'INC_DIR:L1READ.INC/LIST'
        INTEGER*4 SCI_REC_NUMN,SCI_REC_NUMO,STATUS,hkcountN,hkcountO,Record_Size,L1_LUNN/14/,
     &            L1_LUNO/15/
        CHARACTER CTYPE*3/'OLD'/,FIL1NAMEN*80/'L1FNEW'/,FIL1NAMEO*80/'L1FOLD'/
	logical differ
C
        INCLUDE	'($SSDEF)/LIST'

	RECORD /HDBstr/ HDBN, HDBO

        INTEGER FORTERR,RMSSTC,RMSSTV,LUN,VAXERR
	character*30 format_time

	integer l1fmt
	common/junk/l1fmt

!     ------------------------------------------------------------------------


        Record_Size	= 32766			! Max.length of WIND data record

        TYPE *,'OPENING ',FIL1NAMEN
        OPEN (  UNIT	= L1_LunN,
     +		NAME	= FIL1NAMEN,
     +		CARRIAGECONTROL='NONE',
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
     +		FORM	= 'FORMATTED',
     +		TYPE	= cTYPE,
     +		RECL	= Record_Size,
     +		ERR	= 2000,			! Return if error
     +		IOSTAT	= STATUS)

        IF (STATUS .EQ. 0)	STATUS = SS$_NORMAL

c	l1fmt=2
c	CALL WIND_L1_READ_SR(L1_LUNN,SCI_REC_NUMN,STATUS)
c	if (status .ne. 1) stop 2

        DO 100 I=1,1000
	  l1fmt=2
  	  CALL WIND_L1_READ_SR(L1_LUNN,SCI_REC_NUMN,STATUS)
	  if (status .ne. 1) stop 1
	  CALL WIND_L1_GET_HDB(L1_LUNN,HDBN,STATUS)
          K=0
          DO 20 WHILE((K.LE.59).AND.(L1_SCI(L1_LUNN).EDB(K).EPOCH.EQ.0))
            K=K+1
  20      CONTINUE
          IF(K.GE.60)K=59
          WRITE(12,50) L1_SCI(L1_LUNN).EDB(K).EPOCH,L1_SCI(L1_LUNN).EDB(K).SC_EPOCH
  50      FORMAT(1X,' EDB times ',2F25.2)
	  call dump_hdb('NEW',i,hdbn)
C
	  l1fmt=1
	  CALL WIND_L1_READ_SR(L1_LUNO,SCI_REC_NUMO,STATUS)
	  if (status .ne. 1) stop 2
	  CALL WIND_L1_GET_HDB(L1_LUNO,HDBO,STATUS)
          K=0
          DO 70 WHILE((K.LE.59).AND.(L1_SCI(L1_LUNO).EDB(K).EPOCH.EQ.0))
            K=K+1
  70      CONTINUE
          IF(K.GE.60)K=59
          WRITE(12,50) L1_SCI(L1_LUNO).EDB(K).EPOCH,L1_SCI(L1_LUNO).EDB(K).SC_EPOCH
	  call dump_hdb('OLD',i,hdbo)
C
          IF(HDBO.CHDB .NE. HDBN.CHDB)THEN
  	    TYPE*, '********** Record ',SCI_REC_NUMN,' not equal**********'
	  if (format_time(hdbn.bdh.w_time) .ne. format_time(hdbo.bdh.w_time))
     &		type *,'*** W_TIME differs'
	  if (format_time(hdbn.bdh.sc_time) .ne. format_time(hdbo.bdh.sc_time))
     &		type *,'*** SC_TIME differs'
	  if (hdbn.bdh.epoch .ne. hdbo.bdh.epoch)
     &	        type *,'*** EPOCH differs'
	  if (hdbn.bdh.sc_epoch .ne. hdbo.bdh.sc_epoch)
     &	        type *,'*** SC_EPOCH differs'
	  if (hdbn.bdh.qual .ne. hdbo.bdh.qual)
     &	        type *,'*** QUAL differs'
	  differ = .false.
	  do ii=0,279
	    if (hdbn.bdh.dat(ii) .ne. hdbo.bdh.dat(ii)) differ = .true.
	  end do
	  if (differ) type *,'*** DAT differs'
	  endif
  100   CONTINUE       
        STOP
2000    TYPE 2001, STATUS
2001    FORMAT(' ***ERROR in WIND_OPEN_L1, STATUS =', I6, ' ***')
        CALL ERRSNS(FORTERR,RMSSTC,RMSSTV,LUN,VAXERR)
        CALL LIB$SIGNAL(%VAL(RMSSTC))
        CALL LIB$SIGNAL(%VAL(VAXERR))
C
	END
	subroutine dump_hdb(which,recn,hdbrec)
	implicit none
        INCLUDE 'INC_DIR:EDB.INC/NOLIST'
	record /HDBstr/ hdbrec
	character*(*) which
	integer recn,ii
	character*30 format_time
c
	write(12,1001) which,recn
1001	format(1x,a3,i8)
	write(12,1002) format_time(hdbrec.bdh.w_time)
1002	format(1x,' w_time: ',a)
	write(12,1003) format_time(hdbrec.bdh.sc_time)
1003	format(1x,'sc_time: ',a)
	write(12,1004) hdbrec.bdh.epoch,hdbrec.bdh.sc_epoch
1004	format(1x,2f25.2)
	write(12,1005) hdbrec.bdh.qual
1005	format(1x,'qual: ',z8)
	write(12,1006) (hdbrec.bdh.dat(ii),ii=0,279)
1006	format(9x,10z3.2,5x,10z3.2/9x,10z3.2,5x,10z3.2/)
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
