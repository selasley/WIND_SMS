	PROGRAM WRITEBITRATE
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

C     ------------------------------------------------------------------------


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


        DO 100 I=1,1000
  	  CALL WIND_L1_READ_SR(L1_LUNN,SCI_REC_NUMN,STATUS)
	  if (status .ne. 1) stop 1
	  CALL WIND_L1_GET_BASIC(L1_LUNN,basicN,STATUS)
C
    	    DO SPIN=0,59
 	      WRITE(12,1011) I,SPIN,'BITRATE ',BASICN.EDB(SPIN).BITRATE
1011          FORMAT(1X,'REC ',I4,'  SPIN ',I2,2X,A9,' : ',I10)
	    ENDDO
  100   CONTINUE       
        STOP
2000    TYPE 2001, STATUS
2001    FORMAT(' ***ERROR in WIND_OPEN_L1, STATUS =', I6, ' ***')
        CALL ERRSNS(FORTERR,RMSSTC,RMSSTV,LUN,VAXERR)
        CALL LIB$SIGNAL(%VAL(RMSSTC))
        CALL LIB$SIGNAL(%VAL(VAXERR))
C
	END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	SUBROUTINE DUMP_BASIC(WHICH,RECN,BASICREC)
	IMPLICIT NONE
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
