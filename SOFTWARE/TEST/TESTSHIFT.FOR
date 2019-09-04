C
	PROGRAM PRINTL1
C
C This program reads the L1 data files on disk and prints out the times, rates and PHA words.
C It used the routine WIND_STORE_L1 as a model
C
!	This version:20-OCT-1994
!
!     Creation:
!	H.BROER	8-SEP-1994
!
!     Revisions:

!     9/29/94 	Reduced maximum index used to write out PHA byte arrays by 1. If its the right
!		number for a (1 origin) substring spec its 1 too big for a (0 origin) byte index
!     9/30/94 	The index for accessing the byte arrays was reduced by 4 more, because of the
!		form of the structure
!    10/20/94   Zero out SWICS  rate array between reads; use L1FOLD (not L1F) as input file 
      IMPLICIT NONE

      INCLUDE	'INC_DIR:EDB.INC;3/NOLIST'
      INCLUDE	'INC_DIR:WIND_L0_RECORD.INC/NOLIST'
      INCLUDE	'INC_DIR:L0_DATA_RECORD.INC/NOLIST'
      INCLUDE   'INC_DIR:WIND_Lun.INC/NOLIST'
      INCLUDE	'($SSDEF)/NOLIST'

      INTEGER*4	II,JJ,STATUS,DUMMY,KK,KK2,CNTR/1/,IOUT
      CHARACTER CTYPE*3/'OLD'/,FIL1NAME*80/'L1FOLD'/
C
      OPEN(16,FILE='NLA0:',STATUS='OLD')
      IOUT=16
      CALL WIND_OPEN_L1( FIL1NAME, cTYPE, STATUS )
!
!	Read one (1) science record 
!
  10  IF(CNTR.GE.1) IOUT=17
      READ( L1_Lun, 100, IOSTAT = STATUS, ERR = 9001 ) SCI( 1 ).CCORE			! BASIC
 100  FORMAT( A )

      HDBset = 1
      READ( L1_Lun, 200, IOSTAT = STATUS, ERR = 9001 ) HKeep.CHK, HDB(HDBset).CHDB	! 4 HK + 1 HDB
 200  FORMAT( A, A )
      IF( STATUS .EQ. 0 )	STATUS = SS$_NORMAL
C
      WRITE(IOUT,300) CNTR
 300  FORMAT(' RECORD ',I4)
C
C Write out SWICS rates and PHA by voltage step for all 60 voltage steps
C
      DO II = 0, 59
C
C Zero SWICS Rate array
C
        SWICS_RATE(3).C_SW_RATE(II)(5:) = C_BIG_NULL_BUFFER(1:LEN(SWICS_RATE(3).C_SW_RATE(II))-4)
C
C Read in
C
        READ( L1_LUN, 100 )   SWICS_RATE( 1 ).C_SW_RATE( II )	! SWICS_RATE
        JJ	= SCI( 1 ).EDB( II ).SWICS_Len * 4 + 4
        READ( L1_LUN, 100 )   SWICS_PHA( 1 ).C_SW_PHA(II)( 1:JJ )	! SWICS_PHA
      END DO
      DO KK=0,59
  	WRITE(IOUT,1002) KK
	WRITE(IOUT,1001)  (SWICS_RATE(1).EDB(KK).RATE(KK2),KK2=0,115)
1001	FORMAT(1X,20Z3.2)
1002	FORMAT(1X,/' DUMP OF SWICS RATES IN SPIN ',I2)
      END DO
      DO KK=0,59
        JJ	= SCI( 1 ).EDB( KK ).SWICS_Len * 4 + 4
	WRITE(IOUT,1003) KK
	WRITE(IOUT,1001)  (SWICS_PHA(1).EDB(KK).PHA(KK2),KK2=0,JJ-5)
1003	FORMAT(1X,/' DUMP OF SWICS PHA IN SPIN ',I2)
      END DO
C
C Write out MASS rates and PHA by voltage step for all 60 voltage steps
C
      DO II = 0, 59
          READ( L1_LUN, 100 )   MASS_RATE( 1 ).C_MA_RATE( II )		! MASS_RATE
          JJ	= SCI( 1 ).EDB( II ).MASS_Len * 2 + 4
          READ( L1_LUN, 100 )   MASS_PHA( 1 ).C_MA_PHA(II)( 1:JJ )	! MASS_PHA
      END DO
      DO KK=0,59
	WRITE(IOUT,1004) KK
	WRITE(IOUT,1001)  (MASS_RATE(1).EDB(KK).RATE(KK2),KK2=0,26)
1004	FORMAT(1X,/' DUMP OF MASS RATES IN SPIN ',I2)
      END DO
      DO KK=0,59
        JJ	= SCI( 1 ).EDB( KK ).MASS_Len * 2 + 4
	WRITE(IOUT,1005) KK
	WRITE(IOUT,1001)  (MASS_PHA(1).EDB(KK).PHA(KK2),KK2=0,JJ-5)
1005	FORMAT(1X,/' DUMP OF MASS PHA IN SPIN ',I2)
      END DO
C
C Write out STICS rates and PHA by voltage step for all 60 voltage steps.
C
      DO II = 0, 59
          READ( L1_LUN, 100 )   STICS_RATE( 1 ).C_ST_RATE( II )	! STICS_RATE
          JJ	= SCI( 1 ).EDB( II ).STICS_Len * 4 + 4
          READ( L1_LUN, 100 )   STICS_PHA( 1 ).C_ST_PHA(II)( 1:JJ )	! STICS_PHA
      END DO
      DO KK=0,59
	WRITE(IOUT,1006) KK
	WRITE(IOUT,1001)  (STICS_RATE(1).EDB(KK).RATE(KK2),KK2=0,179)
1006	FORMAT(1X,/'  DUMP OF STICS RATES IN SPIN ',I2)
      END DO
      DO KK=0,59
        JJ	= SCI( 1 ).EDB( KK ).STICS_Len * 4 + 4
	WRITE(IOUT,1007) KK
	WRITE(IOUT,1001)  (STICS_PHA(1).EDB(KK).PHA(KK2),KK2=0,JJ-5)
1007	FORMAT(1X,/' DUMP OF STICS PHA IN SPIN ',I2)
      END DO

      CNTR=CNTR+1
      IF(CNTR.GE.20)THEN
        STOP
      ELSE
        GOTO 10
      ENDIF

9001  CONTINUE
      WRITE( KB, 901 )  '  ***** ERROR READING L1-DATA *****', STATUS
 901  FORMAT( A, '  STATUS =', I5 )
      STOP  1101

      END
