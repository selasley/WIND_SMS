	PROGRAM TESTBITRATE
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
1010	format(1x,'Rec ',i4,'  Spin ',i2,2x,a9,' : ',2f25.2)
1011	format(1x,'Rec ',i4,'  Spin ',i2,2x,a9,' : ',2i10)
1012	format(1x,'Rec ',i4,'  Spin ',i2,2x,a9,' : ',a,5x,a)
	      if (basicn.edb(spin).BitRate .ne. basico.edb(spin).BitRate) then
		differ(spin) = .true.
		write(6,*) '*** Spin',spin,'  BitRate differs'
		write(12,1011) i,spin,'BitRate ',basicn.edb(spin).BitRate, basico.edb(spin).BitRate 
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
