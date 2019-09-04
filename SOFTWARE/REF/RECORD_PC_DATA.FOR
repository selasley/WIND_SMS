      SUBROUTINE RECORD_PC_DATA( TIME, STATUS )
C
C	RECORD_PC_DATA 	records all sciences dat in the same fashion
C	as the GSE records the data.  This allows on one side to check
C	the data as produced on the VAX with data produced with the GSE
C	and allows to use the data produced with either source to
C	establish level 1 data files.
C
C	This version:   1.10   23-FEB-1995
C
C	USAGE:
C		CALL RECORD_PC_DATA( TIME, STATUS )
C
C	PARAMETERS:
C		TIME	R*8	Input
C		STATUS	I*4	Input	0-> max Spin	EDB's
C					100		Hk data
C					200		HDB data
C				Output	001		Normal return
C		
C	CREATION :
C
C		H.BROER	11-MAR-1994
C
C	REVISION :
C	 22-APR-1994	H.BROER
C	 26-MAY-1994	H.BROER		new HK.INC
C	  9-JUN-1994	H.BROER		combined EDB.inc and HK.inc
C	 30-JUN-1994	H.BROER		SWICS,MASS,+STICS converted to structure
C	  9-SEP-1994	J.PAQUETTE	changed location of SHIFT and STORE;
C					altered HK structure; changed
C					references to HKset and Hkeep to array
C					references using EDBset as subscript
C        27-OCT-1994    L. Bleau        made WRITE statements to KB conditional,
C                                       added common block DIAG
C         4-NOV-1994    L. Bleau        changed KB to TTOUT in WRITE statements
C         7-DEC-1994    L. Bleau        changed filename of unit 21 output file
C                                       to GSEFORMAT.DAT in DATA_DIR:
C	 13-DEC-1994	L. Bleau	removed duplicate IDENT line
C   1.8  20-DEC-1994    L. Bleau        changed RECFILE to LOGICAL*4
C	  9-FEB-1995	L. Bleau	changed '!' to 'C' in program comments
C	  9-FEB-1995	L. Bleau	change include stmt to use EDBDEF.INC
C					and EDBVARS.INC instead of EDB.INC only
C        15-FEB-1995    J.PAQUETTE      removed refrences to HDBset since HDB
C                       L. Bleau        now indexed by EDBset
C	 23-FEB-1995	L. Bleau	removed DIAG def, add INCLUDE DIAG.INC
C  1.10	 23-FEB-1995	L. Bleau	if in PRINTLZ mode don't do anything
C
CDEC$ IDENT   '1.10'

      IMPLICIT NONE

      INCLUDE	'INC_DIR:EDBDEF.INC/NOLIST'
      INCLUDE	'INC_DIR:EDBVARS.INC/NOLIST'
      INCLUDE	'INC_DIR:L0_PC_RECORD.INC/NOLIST'
      INCLUDE	'INC_DIR:WIND_L0_RECORD.INC/NOLIST'
      INCLUDE	'INC_DIR:WIND_Lun.INC/NOLIST'
      INCLUDE   'INC_DIR:DIAG.INC/NOLIST'
      INCLUDE	'($SSDEF)/NOLIST'

      REAL*8		TIME,
     :			TIME70

      INTEGER*4		II,
     :			MaxSpin/59/,
     :			ONEXT,
     :			STATUS,
     :			TASK

      BYTE		bEDB(0:777),
     :			bEBYTE(512)/512*0/,
     :			IIByte(4)

      CHARACTER		CEDB*778,
     :			BBYTE*2048,
     :			EBYTE*512,
     :			NAM19*19,
     :			NAM26*26

      LOGICAL*4		RECFILE

      EQUIVALENCE	( bEDB, CEDB ),
     :			( EBYTE,bEBYTE ),
     :			( II,	IIByte(1) )

      TASK	= STATUS
      STATUS	= SS$_NORMAL
C
C If the program is in PRINTLZ mode (ie, dumping the LZ file) this routine
C serves no purpose, so just return without doing anything.
C
      IF (PRINTLZ_MODE) RETURN
C
      INQUIRE( UNIT=21, OPENED=RECFILE )
      IF( .NOT.RECFILE )	THEN
          OPEN(	UNIT = 21,
     :		FILE = 'GSEFORMAT',
     :		DEFAULTFILE = 'DATA_DIR:.DAT',
     :		FORM = 'FORMATTED',
     :		TYPE = 'NEW',
     :		CARRIAGECONTROL = 'NONE',
     :		ACCESS = 'SEQUENTIAL',
     :		RECL = 512,
     :		ERR  = 9990,
     :		IOSTAT = STATUS,
     :		RECORDTYPE = 'FIXED' )
          HLABEL.LABEL.byID(0)	= 'EB'X
          HLABEL.LABEL.byID(1)	= '90'X
          HLABEL.LABEL.byID(2)	= 'G'
          HLABEL.LABEL.byID(3)	= 'S'
          HLABEL.LABEL.byID(4)	= 'E'
          HLABEL.LABEL.wVersion	= '101'X
          NAM19			= 'SMS'
          DO II = 4, 19
              NAM19(II:II)	= CHAR(0)
          END DO
          HLABEL.LABEL.ProjectName	= NAM19
          HLABEL.LABEL.wNULL	= 0

          REC_PRE.LABEL.bID(0)	= 'D'
          REC_PRE.LABEL.bID(1)	= 'E'
          REC_DAT.LABEL.bID(0)	= 'T'
          REC_DAT.LABEL.bID(1)	= 'A'

          CALL WIND_GREG_TO_EPOCH( 1970, 1, 0, 0, TIME70, STATUS )
          TIME70	= TIME70 - 432.D5
          HLABEL.LABEL.dwTime	= INT( (TIME - TIME70) / 1D3 )
          IF(DIAG .LE. 3) WRITE( TTOUT, 100 )	HLABEL.LABEL.dwTime, HLABEL.LABEL.dwTime
  100 FORMAT( 1H , T10, I10, ' SECONDS ELAPSED SINCE 1970, in hex =', Z8.8 )

          DO II = 0, 31
              bEDB( II )	= HLABEL.BBYTE( II )
          END DO
          ONEXT	= 1
          BBYTE(513:)	= EBYTE
          BBYTE(1025:)	= EBYTE

          CALL OUTRECORD( BBYTE, ONEXT, CEDB( 1: 32 ))

          RecordId	= 0	! Next RECORD type to be assigned
          HDBRecId	= -1	! HDB assigned Record ID
          HkRecId	= -1	! Hk assigned Record ID
          CoreId	= -1	! Core data assigned Record ID
          WCoreId	= -1	! SWICS Core assigned Record ID
          SCoreId	= -1	! STICS Core assigned Record ID
          MCoreId	= -1	! MASS  Core assigned Record ID
          WPhaId	= -1	! SWICS PHA assigned Record ID
          SPhaId	= -1	! STICS PHA assigned Record ID
          MPhaId	= -1	! MASS  PHA assigned Record ID
          HDBRecNum	= -1	! HDB record running count
          HkRecNum	= -1	! Hk record running count
          CoreNum	= -1	! Core record running count
          WCoreNum	= -1	! SWICS Core record running count
          SCoreNum	= -1	! STICS Core record running count
          MCoreNum	= -1	! MASS  Core record running count
          WPhaNum	= -1	! SWICS PHA record running count
          SPhaNum	= -1	! STICS PHA record running count
          MPhaNum	= -1	! MASS  PHA record running count
      END IF !	( .NOT.RECFILE )

      IF( TASK .LE. MaxSpin )	THEN
          IF( CoreId .LT. 0 )	THEN
              REC_PRE.LABEL.wBlockID	= RecordId
              NAM26			= 'ExpHeader'		! Core

              DO II = 10, 26
                  NAM26( II:II )	= CHAR( 0 )
              END DO

              REC_PRE.LABEL.szName	= NAM26
              REC_PRE.LABEL.wLen	= 32

              DO II = 0, 31
                  bEDB( II )	= REC_PRE.BBYTE( II )
              END DO

              CALL OUTRECORD( BBYTE, ONEXT, CEDB( 1: 32 ))

              CoreId	= RecordId
              RecordId	= RecordId + 1
          END IF

          CoreNum	= CoreNum + 1
          REC_DAT.LABEL.wBlockID	= CoreId
          REC_DAT.LABEL.wCountStamp	= CoreNum
          REC_DAT.LABEL.wSize		= EDB_Core_Size + 1
          REC_DAT.LABEL.dwTime		= INT( (TIME - TIME70) * 1.D-3 )

          DO II = 0, 11
              bEDB( II )	= REC_DAT.BBYTE( II )
          END DO

          DO II = 0, EDB_Core_Size
              bEDB( 12 + II )	= CORE_data( II, TASK, EDBset )
          END DO

          II	= REC_DAT.LABEL.wSize + 14
          bEDB( REC_DAT.LABEL.wSize + 12 )	= IIByte(1)
          bEDB( REC_DAT.LABEL.wSize + 13 )	= IIByte(2)

          CALL OUTRECORD( BBYTE, ONEXT, CEDB( 1: II ))

          IF( SCI( EDBset ).EDB( TASK ).SWICSPower )	THEN
              IF( WCoreId .LT. 0 )	THEN
                  REC_PRE.LABEL.wBlockID	= RecordId
                  NAM26			= 'ExpWRate'		! SWICS Rate

                  DO II = 9, 26
                      NAM26( II:II )	= CHAR( 0 )
                  END DO

                  REC_PRE.LABEL.szName	= NAM26
                  REC_PRE.LABEL.wLen	= 32

                  DO II = 0, 31
                      bEDB( II )	= REC_PRE.BBYTE( II )
                  END DO

                  CALL OUTRECORD( BBYTE, ONEXT, CEDB( 1: 32 ))

                  WCoreId	= RecordId
                  RecordId	= RecordId + 1
              END IF

              WCoreNum	= WCoreNum + 1
              REC_DAT.LABEL.wBlockID	= WCoreId
              REC_DAT.LABEL.wCountStamp	= WCoreNum
              IF( BITRATE )	THEN
                  REC_DAT.LABEL.wSize	= SWICS_H_RATE_Size+1
              ELSE
                  REC_DAT.LABEL.wSize	= SWICS_N_RATE_Size+1
              END IF
              REC_DAT.LABEL.dwTime		= INT( (TIME - TIME70) * 1.D-3 )

              DO II = 0, 11
                  bEDB( II )	= REC_DAT.BBYTE( II )
              END DO

              DO II = 0, REC_DAT.LABEL.wSize-1
                  bEDB( 12 + II ) = SWICS_RATE( EDBset ).EDB( TASK ).RATE( II )
              END DO

              II	= REC_DAT.LABEL.wSize + 14
              bEDB( REC_DAT.LABEL.wSize + 12 )	= IIByte(1)
              bEDB( REC_DAT.LABEL.wSize + 13 )	= IIByte(2)

              CALL OUTRECORD( BBYTE, ONEXT, CEDB( 1: II ))

              IF( SCI( EDBset ).EDB( TASK ).SWICS_Len .GT. 0 )	THEN
                  IF( WPHAId .LT. 0 )	THEN
                      REC_PRE.LABEL.wBlockID	= RecordId
                      NAM26			= 'ExpWPHA'	! SWICS PHA

                      DO II = 8, 26
                          NAM26( II:II )	= CHAR( 0 )
                      END DO

                      REC_PRE.LABEL.szName	= NAM26
                      REC_PRE.LABEL.wLen	= 32

                      DO II = 0, 31
                          bEDB( II )	= REC_PRE.BBYTE( II )
                      END DO

                      CALL OUTRECORD( BBYTE, ONEXT, CEDB( 1: 32 ))

                      WPHAId	= RecordId
                      RecordId	= RecordId + 1
                  END IF

                  WPHANum	= WPHANum + 1
                  REC_DAT.LABEL.wBlockID	= WPHAId
                  REC_DAT.LABEL.wCountStamp	= WPHANum
                  REC_DAT.LABEL.wSize		= SCI( EDBset ).EDB( TASK ).SWICS_Len * 4
                  REC_DAT.LABEL.dwTime		= INT( (TIME - TIME70) * 1.D-3 )

                  DO II = 0, 11
                      bEDB( II )	= REC_DAT.BBYTE( II )
                  END DO

                  DO II = 0, REC_DAT.LABEL.wSize-1
                      bEDB( 12 + II ) = SWICS_PHA( EDBset ).EDB( TASK ).PHA( II )
                  END DO

                  II	= REC_DAT.LABEL.wSize + 14
                  bEDB( REC_DAT.LABEL.wSize + 12 )	= IIByte(1)
                  bEDB( REC_DAT.LABEL.wSize + 13 )	= IIByte(2)

                  CALL OUTRECORD( BBYTE, ONEXT, CEDB( 1: II ))
              END IF !	( SCI( EDBset ).EDB( TASK ).SWICS_Len .GT. 0 )
          END IF !	( SCI( EDBset ).EDB( TASK ).SWICSPower )

          IF( SCI( EDBset ).EDB( TASK ).STICSPower )	THEN
              IF( SCoreId .LT. 0 )	THEN
                  REC_PRE.LABEL.wBlockID	= RecordId
                  NAM26			= 'ExpSRate'		! STICS Rate

                  DO II = 9, 26
                      NAM26( II:II )	= CHAR( 0 )
                  END DO

                  REC_PRE.LABEL.szName	= NAM26
                  REC_PRE.LABEL.wLen	= 32

                  DO II = 0, 31
                      bEDB( II )	= REC_PRE.BBYTE( II )
                  END DO

                  CALL OUTRECORD( BBYTE, ONEXT, CEDB( 1: 32 ))

                  SCoreId	= RecordId
                  RecordId	= RecordId + 1
              END IF

              SCoreNum	= SCoreNum + 1
              REC_DAT.LABEL.wBlockID	= SCoreId
              REC_DAT.LABEL.wCountStamp	= SCoreNum
              IF( BITRATE )	THEN
                  REC_DAT.LABEL.wSize	= STICS_H_RATE_Size+1
              ELSE
                  REC_DAT.LABEL.wSize	= STICS_N_RATE_Size+1
              END IF
              REC_DAT.LABEL.dwTime		= INT( (TIME - TIME70) * 1.D-3 )

              DO II = 0, 11
                  bEDB( II )	= REC_DAT.BBYTE( II )
              END DO

              DO II = 0, REC_DAT.LABEL.wSize-1
                  bEDB( 12 + II ) = STICS_RATE( EDBset ).EDB( TASK ).RATE( II )
              END DO

              II	= REC_DAT.LABEL.wSize + 14
              bEDB( REC_DAT.LABEL.wSize + 12 )	= IIByte(1)
              bEDB( REC_DAT.LABEL.wSize + 13 )	= IIByte(2)

              CALL OUTRECORD( BBYTE, ONEXT, CEDB( 1: II ))

              IF( SCI( EDBset ).EDB( TASK ).STICS_Len .GT. 0 )	THEN
                  IF( SPHAId .LT. 0 )	THEN
                      REC_PRE.LABEL.wBlockID	= RecordId
                      NAM26			= 'ExpSPHA'	! STICS PHA

                      DO II = 8, 26
                          NAM26( II:II )	= CHAR( 0 )
                      END DO

                      REC_PRE.LABEL.szName	= NAM26
                      REC_PRE.LABEL.wLen	= 32

                      DO II = 0, 31
                          bEDB( II )	= REC_PRE.BBYTE( II )
                      END DO

                      CALL OUTRECORD( BBYTE, ONEXT, CEDB( 1: 32 ))

                      SPHAId	= RecordId
                      RecordId	= RecordId + 1
                  END IF

                  SPHANum	= SPHANum + 1
                  REC_DAT.LABEL.wBlockID	= SPHAId
                  REC_DAT.LABEL.wCountStamp	= SPHANum
                  REC_DAT.LABEL.wSize		= SCI( EDBset ).EDB( TASK ).STICS_Len * 4
                  REC_DAT.LABEL.dwTime		= INT( (TIME - TIME70) * 1.D-3 )

                  DO II = 0, 11
                      bEDB( II )	= REC_DAT.BBYTE( II )
                  END DO

                  DO II = 0, REC_DAT.LABEL.wSize-1
                      bEDB( 12 + II ) = STICS_PHA( EDBset ).EDB( TASK ).PHA( II )
                  END DO

                  II	= REC_DAT.LABEL.wSize + 14
                  bEDB( REC_DAT.LABEL.wSize + 12 )	= IIByte(1)
                  bEDB( REC_DAT.LABEL.wSize + 13 )	= IIByte(2)

                  CALL OUTRECORD( BBYTE, ONEXT, CEDB( 1: II ))
              END IF !	( SCI( EDBset ).EDB( TASK ).STICS_Len .GT. 0 )
          END IF !	( SCI( EDBset ).EDB( TASK ).STICSPower )

          IF( SCI( EDBset ).EDB( TASK ).MASSPower )	THEN
              IF( MCoreId .LT. 0 )	THEN
                  REC_PRE.LABEL.wBlockID	= RecordId
                  NAM26			= 'ExpMRate'		! MASS Rate

                  DO II = 9, 26
                      NAM26( II:II )	= CHAR( 0 )
                  END DO

                  REC_PRE.LABEL.szName	= NAM26
                  REC_PRE.LABEL.wLen	= 32

                  DO II = 0, 31
                      bEDB( II )	= REC_PRE.BBYTE( II )
                  END DO

                  CALL OUTRECORD( BBYTE, ONEXT, CEDB( 1: 32 ))

                  MCoreId	= RecordId
                  RecordId	= RecordId + 1
              END IF

              MCoreNum	= MCoreNum + 1
              REC_DAT.LABEL.wBlockID	= MCoreId
              REC_DAT.LABEL.wCountStamp	= MCoreNum
              REC_DAT.LABEL.wSize	= MASS_RATE_Size+1
              REC_DAT.LABEL.dwTime	= INT( (TIME - TIME70) * 1.D-3 )

              DO II = 0, 11
                  bEDB( II )	= REC_DAT.BBYTE( II )
              END DO

              DO II = 0, REC_DAT.LABEL.wSize-1
                  bEDB( 12 + II ) = MASS_RATE( EDBset ).EDB( TASK ).RATE( II )
              END DO

              II	= REC_DAT.LABEL.wSize + 14
              bEDB( REC_DAT.LABEL.wSize + 12 )	= IIByte(1)
              bEDB( REC_DAT.LABEL.wSize + 13 )	= IIByte(2)

              CALL OUTRECORD( BBYTE, ONEXT, CEDB( 1: II ))

              IF( SCI( EDBset ).EDB( TASK ).MASS_Len .GT. 0 )	THEN
                  IF( MPHAId .LT. 0 )	THEN
                      REC_PRE.LABEL.wBlockID	= RecordId
                      NAM26			= 'ExpMPHA'	! MASS PHA

                      DO II = 8, 26
                          NAM26( II:II )	= CHAR( 0 )
                      END DO

                      REC_PRE.LABEL.szName	= NAM26
                      REC_PRE.LABEL.wLen	= 32

                      DO II = 0, 31
                          bEDB( II )	= REC_PRE.BBYTE( II )
                      END DO

                      CALL OUTRECORD( BBYTE, ONEXT, CEDB( 1: 32 ))

                      MPHAId	= RecordId
                      RecordId	= RecordId + 1
                  END IF

                  MPHANum	= MPHANum + 1
                  REC_DAT.LABEL.wBlockID	= MPHAId
                  REC_DAT.LABEL.wCountStamp	= MPHANum
                  REC_DAT.LABEL.wSize		= SCI( EDBset ).EDB( TASK ).MASS_Len * 2
                  REC_DAT.LABEL.dwTime		= INT( (TIME - TIME70) * 1.D-3 )

                  DO II = 0, 11
                      bEDB( II )	= REC_DAT.BBYTE( II )
                  END DO

                  DO II = 0, REC_DAT.LABEL.wSize-1
                      bEDB( 12 + II ) = MASS_PHA( EDBset ).EDB( TASK ).PHA( II )
                  END DO

                  II	= REC_DAT.LABEL.wSize + 14
                  bEDB( REC_DAT.LABEL.wSize + 12 )	= IIByte(1)
                  bEDB( REC_DAT.LABEL.wSize + 13 )	= IIByte(2)

                  CALL OUTRECORD( BBYTE, ONEXT, CEDB( 1: II ))
              END IF !	( SCI( EDBset ).EDB( TASK ).MASS_Len .GT. 0 )
          END IF !	( SCI( EDBset ).EDB( TASK ).MASSPower )

      ELSE IF( TASK .EQ. 100 )	THEN
          IF( HkRecId .LT. 0 )	THEN
              REC_PRE.LABEL.wBlockID	= RecordId
              NAM26			= 'ExpHk'		! Hk DATA

              DO II = 6, 26
                  NAM26( II:II )	= CHAR( 0 )
              END DO

              REC_PRE.LABEL.szName	= NAM26
              REC_PRE.LABEL.wLen	= 32

              DO II = 0, 31
                  bEDB( II )	= REC_PRE.BBYTE( II )
              END DO

              CALL OUTRECORD( BBYTE, ONEXT, CEDB( 1: 32 ))

              HkRecId	= RecordId
              RecordId	= RecordId + 1
          END IF

          HkRecNum	= HkRecNum + 1
          REC_DAT.LABEL.wBlockID	= HkRecId
          REC_DAT.LABEL.wCountStamp	= HkRecNum
          REC_DAT.LABEL.wSize	= 70
          REC_DAT.LABEL.dwTime	= INT( (TIME - TIME70) * 1.D-3 )

          DO II = 0, 11
              bEDB( II )	= REC_DAT.BBYTE( II )
          END DO

          DO II = 1, REC_DAT.LABEL.wSize
              bEDB( 11 + II )	= HKeep( EDBset ).HKSET( HKset(EDBset) ).HK( II+5 ).BVAL
          END DO

          II	= REC_DAT.LABEL.wSize + 14
          bEDB( REC_DAT.LABEL.wSize + 12 )	= IIByte(1)
          bEDB( REC_DAT.LABEL.wSize + 13 )	= IIByte(2)

          CALL OUTRECORD( BBYTE, ONEXT, CEDB( 1: II ))

      ELSE IF( TASK .EQ. 200 )	THEN
          IF( HDBRecId .LT. 0 )	THEN
              REC_PRE.LABEL.wBlockID	= RecordId
              NAM26			= 'ExpHDB'		! HDB DATA

              DO II = 7, 26
                  NAM26( II:II )	= CHAR( 0 )
              END DO

              REC_PRE.LABEL.szName	= NAM26
              REC_PRE.LABEL.wLen	= 32

              DO II = 0, 31
                  bEDB( II )	= REC_PRE.BBYTE( II )
              END DO

              CALL OUTRECORD( BBYTE, ONEXT, CEDB( 1: 32 ))

              HDBRecId	= RecordId
              RecordId	= RecordId + 1
          END IF

          HDBRecNum	= HDBRecNum + 1
          REC_DAT.LABEL.wBlockID	= HDBRecId
          REC_DAT.LABEL.wCountStamp	= HDBRecNum
          REC_DAT.LABEL.wSize	= 280
          REC_DAT.LABEL.dwTime	= INT( (TIME - TIME70) * 1.D-3 )

          DO II = 0, 11
              bEDB( II )	= REC_DAT.BBYTE( II )
          END DO

          DO II = 0, REC_DAT.LABEL.wSize-1
              bEDB( 12 + II )	= HDB( EDBset - 1 ).BDH.DAT( II )
          END DO

          II	= REC_DAT.LABEL.wSize + 14
          bEDB( REC_DAT.LABEL.wSize + 12 )	= IIByte(1)
          bEDB( REC_DAT.LABEL.wSize + 13 )	= IIByte(2)

          CALL OUTRECORD( BBYTE, ONEXT, CEDB( 1: II ))

      ELSE IF( TASK .EQ. 900 )	THEN

          II	= 513 - ONEXT
          CALL OUTRECORD( BBYTE, ONEXT, EBYTE( 1: II ))
          CLOSE( UNIT=21 )

      END IF !	( TASK .LE. MaxSpin )

      RETURN

 9990 CONTINUE
      WRITE( 6, 990 )
  990 FORMAT( ' COULD NOT OPEN UNIT=21 ' )
      STOP 990
      END

      SUBROUTINE OUTRECORD(BBYTE,ONEXT,CONTENTS)

      CHARACTER*(*)	BBYTE,CONTENTS
      INTEGER*4		ONEXT,
     :			CLEN

      CLEN	= LEN(CONTENTS)
      BBYTE( ONEXT:ONEXT+CLEN-1 ) = CONTENTS
          ONEXT	= ONEXT + CLEN

          IF( ONEXT .GT. 512 )	THEN
              WRITE( 21, 123 )	BBYTE
              BBYTE	= BBYTE( 513: )
              ONEXT	= ONEXT - 512
          END IF !	( ONEXT .GT. 512 )

  123 FORMAT( A512 )
      RETURN

      END
