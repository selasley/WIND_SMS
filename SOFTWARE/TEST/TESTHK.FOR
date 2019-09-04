	PROGRAM TESTHK
        INCLUDE 'INC_DIR:EDB.INC/LIST'
        INCLUDE	'INC_DIR:SW_CODES.INC/LIST'
        INCLUDE	'INC_DIR:WIND_Lun.INC/LIST'
        INCLUDE 'INC_DIR:L1READ.INC/LIST'
        INTEGER*4 SCI_REC_NUMN,SCI_REC_NUMO,STATUS,hkcountN,hkcountO,Record_Size,L1_LUNN/14/,
     &            L1_LUNO/15/
        CHARACTER CTYPE*3/'OLD'/,FIL1NAMEN*80/'L1FNEW'/,FIL1NAMEO*80/'L1FOLD'/
C
        INCLUDE	'($SSDEF)/LIST'

        INTEGER FORTERR,RMSSTC,RMSSTV,LUN,VAXERR

      STRUCTURE	/HKeepO/
          UNION
              MAP
                  CHARACTER*4	CCHK/'HK  '/
                  RECORD /HKP/ HKSET(5)
                  RECORD /HOUSE_KEEP/ HK(75)	! HK VALUE + QUAL
              END MAP

              MAP
                  CHARACTER*914	CHK
              END MAP
          END UNION
      END STRUCTURE

      RECORD /HKeepO/ HKeepO	! old structure

      RECORD /HKeep/ HKeepN	! new structure

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

        DO 100 I=1,1000
  	  CALL WIND_L1_READ_SR(L1_LUNN,SCI_REC_NUMN,STATUS)
	  if (status .ne. 1) stop 1
1001	FORMAT(2X,6I6,'  EDB')
	  CALL WIND_L1_GET_HK(L1_LUNN,HKeepN,hkcountN,STATUS)
C
	  CALL WIND_L1_READ_SR(L1_LUNO,SCI_REC_NUMO,STATUS)
	  if (status .ne. 1) stop 2
	  CALL WIND_L1_GET_HK(L1_LUNO,HKeepO,hkcountO,STATUS)
	TYPE *,'HKCOUNT=',hkcountN,hkcountO
	hkcountO=5
C
          WRITE(11,*)'NEW ',I
	  WRITE(11,1001) L1_SCI(L1_LUNN).EDB(0).W_TIME.YEAR,L1_SCI(L1_LUNN).EDB(0).W_TIME.DOY,
     &	  L1_SCI(L1_LUNN).EDB(0).W_TIME.HR,L1_SCI(L1_LUNN).EDB(0).W_TIME.MIN,
     &	  L1_SCI(L1_LUNN).EDB(0).W_TIME.SEC,L1_SCI(L1_LUNN).EDB(0).W_TIME.CSEC
          DO 50 J=1,MIN(hkcountN+1,5)
            WRITE(11,*)HKeepN.HKSET(J).W_TIME.YEAR,HKeepN.HKSET(J).W_TIME.DOY,
     &      HKeepN.HKSET(J).W_TIME.HR,HKeepN.HKSET(J).W_TIME.MIN,
     &      HKeepN.HKSET(J).W_TIME.SEC,HKeepN.HKSET(J).W_TIME.CSEC
            WRITE(11,*)HKeepN.HKSET(J).SC_TIME.YEAR,HKeepN.HKSET(J).SC_TIME.DOY,
     &      HKeepN.HKSET(J).SC_TIME.HR,HKeepN.HKSET(J).SC_TIME.MIN,
     &      HKeepN.HKSET(J).SC_TIME.SEC,HKeepN.HKSET(J).SC_TIME.CSEC
	    WRITE(11,*)HKeepN.HKSET(J).EPOCH,HKeepN.HKSET(J).SC_EPOCH
!            WRITE(11,20)(HKeepN.HKSET(J).HK(K).BVAL,HKeepN.HKSET(J).HK(K).QUAL,K=1,75)
  20        FORMAT(10(X,I4,X,I1))
  50      CONTINUE
          WRITE(11,*)'OLD ',I
	  WRITE(11,1001) L1_SCI(L1_LUNO).EDB(0).W_TIME.YEAR,L1_SCI(L1_LUNO).EDB(0).W_TIME.DOY,
     &	  L1_SCI(L1_LUNO).EDB(0).W_TIME.HR,L1_SCI(L1_LUNO).EDB(0).W_TIME.MIN,
     &	  L1_SCI(L1_LUNO).EDB(0).W_TIME.SEC,L1_SCI(L1_LUNO).EDB(0).W_TIME.CSEC
          DO 60 J=1,hkcountN
            WRITE(11,*)HKeepO.HKSET(J).W_TIME.YEAR,HKeepO.HKSET(J).W_TIME.DOY,
     &      HKeepO.HKSET(J).W_TIME.HR,HKeepO.HKSET(J).W_TIME.MIN,
     &      HKeepO.HKSET(J).W_TIME.SEC,HKeepO.HKSET(J).W_TIME.CSEC
            WRITE(11,*)HKeepO.HKSET(J).SC_TIME.YEAR,HKeepO.HKSET(J).SC_TIME.DOY,
     &      HKeepO.HKSET(J).SC_TIME.HR,HKeepO.HKSET(J).SC_TIME.MIN,
     &      HKeepO.HKSET(J).SC_TIME.SEC,HKeepO.HKSET(J).SC_TIME.CSEC
	    WRITE(11,*)HKeepO.HKSET(J).EPOCH,HKeepO.HKSET(J).SC_EPOCH
!            WRITE(11,20)(HKeepO.HKSET(J).HK(K).BVAL,HKeepO.HKSET(J).HK(K).QUAL,K=1,75)
  60      CONTINUE
C
          IF(HKeepO.CHK(5:hkcountN*182+4).NE.HKeepN.CHK(9:hkcountN*182+8))THEN
  	    TYPE*, '********** Record ',SCI_REC_NUMN,' not equal**********'
          ENDIF
  100   CONTINUE       
        STOP
2000    TYPE 2001, STATUS
2001    FORMAT(' ***ERROR in WIND_OPEN_L1, STATUS =', I6, ' ***')
        CALL ERRSNS(FORTERR,RMSSTC,RMSSTV,LUN,VAXERR)
        CALL LIB$SIGNAL(%VAL(RMSSTC))
        CALL LIB$SIGNAL(%VAL(VAXERR))
C
	END
