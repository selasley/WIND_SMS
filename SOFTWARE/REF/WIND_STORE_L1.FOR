      SUBROUTINE WIND_STORE_L1( STATUS )
C
C	Stores L1 values on disk in file with LZ replaced L1.
C
C	This version:   2.6   9-FEB-1995 
C
C     Creation:
C	H.BROER		 8-APR-1994
C
C     Revisions:
C	 9-JUN-1994	H.BROER		update include files
C	29-JUN-1994	H.BROER		next update
C       15-JUL-1994	JOHN PAQUETTE	corrected order in which data output
C   	18-JUL-1994	JOHN PAQUETTE	changed indices JJ to KK, to II and JJ
C	 9-SEP-1994	L. Bleau	remove code that copies HKs, trim down
C					routine to do nothing except write
C					out a Science Record to the L1 file
C	21-SEP-1994	L. Bleau	change code to write out HDB(1) instead
C					of HDB(HDBset)
C	14-OCT-1994	L. Bleau	add call to write out sci rec header
C	20-OCT-1994	JOHN PAQUETTE   write out only as many rates as are
C					present 
C	25-OCT-1994	L. Bleau	add logic to determine SWICS rates
C					record length
C        4-NOV-1994     L. Bleau        changed KB to TTOUT in WRITE statements
C	13-DEC-1994	L. Bleau	added call to LIB$SIGNAL to display
C					error message if WRITE gets an error
C	 9-FEB-1995	L. Bleau	change comment delimiters
C	 9-FEB-1995	L. Bleau	change include stmt to use EDBDEF.INC
C					and EDBVARS.INC instead of EDB.INC only
C
CDEC$ IDENT   '2.6'

      IMPLICIT NONE

      INCLUDE	'INC_DIR:EDBDEF.INC/NOLIST'
      INCLUDE	'INC_DIR:EDBVARS.INC/NOLIST'
      INCLUDE	'INC_DIR:WIND_L0_RECORD.INC/NOLIST'
      INCLUDE   'INC_DIR:WIND_Lun.INC/NOLIST'
      INCLUDE	'($SSDEF)/NOLIST'

      INTEGER*4	II,
     :		JJ,
     :          KK,
     :		STATUS
C
C	STORE ONE (1) SCIENCE RECORD DATA
C
      CALL WIND_WRITE_L1_HEADER( L1_Lun, 2 )
      WRITE( L1_Lun, 100, IOSTAT = STATUS, ERR = 9001 ) SCI( 1 ).CCORE			! BASIC
 100  FORMAT( A )

      HKeep( 1 ).HKcount = HKset( 1 )
      WRITE( L1_Lun, 200, IOSTAT = STATUS, ERR = 9001 ) HKeep( 1 ).CHK, HDB( 1 ).CHDB	! HKs + HDB
 200  FORMAT( A, A )
      IF( STATUS .EQ. 0 )	STATUS = SS$_NORMAL
C
C The code which copied the last valied HK to the first HK position has been
C moved to WIND_RESET_HK
C

C
C Write out SWICS rates and PHA by voltage step for all 60 voltage steps
C
      DO II = 0, 59
C
C There is a variable number of SWICS Rates (LBR 62 bytes, HBR:116 bytes);
C write out only as many Rates as we have. We must allow for the 4-byte
C identifier ('SWRA'), and the fact that the variable SWICS_RATE_SIZE assumes a
C zero origin.
C
        IF(SCI(1).EDB(II).SWICS_RATE_SIZE.EQ.0)THEN
          KK = 4
        ELSE
          KK = SCI(1).EDB(II).SWICS_RATE_SIZE + 5
        ENDIF
        WRITE( L1_LUN, 100 )  SWICS_RATE( 1 ).C_SW_RATE(II)( 1:KK )	
C
        JJ	= SCI( 1 ).EDB( II ).SWICS_Len * 4 + 4
        WRITE( L1_LUN, 100 )  SWICS_PHA( 1 ).C_SW_PHA(II)( 1:JJ )	! SWICS_PHA
      END DO
C
C Write out MASS rates and PHA by voltage step for all 60 voltage steps
C
      DO II = 0, 59
          WRITE( L1_LUN, 100 )   MASS_RATE( 1 ).C_MA_RATE( II )		! MASS_RATE
          JJ	= SCI( 1 ).EDB( II ).MASS_Len * 2 + 4
          WRITE( L1_LUN, 100 )   MASS_PHA( 1 ).C_MA_PHA(II)( 1:JJ )	! MASS_PHA
      END DO
C
C Write out STICS rates and PHA by voltage step for all 60 voltage steps.
C
      DO II = 0, 59
C
C There is a variable number of STICS Rates at LBR (180 bytes or 0 bytes);
C write out only as many Rates as we have. If there are 180 rates, we must
C allow for the 4-byte identifier ('STRA'), and the fact that the variable
C SWICS_RATE_SIZE assumes a zero origin. If there are no rates, we still have
C to print out the identifier.
C
          IF(SCI(1).EDB(II).STICS_RATE_SIZE.EQ.0)THEN
            KK = 4
          ELSE
            KK = SCI(1).EDB(II).STICS_RATE_SIZE + 5
          ENDIF
          WRITE( L1_LUN, 100 )  STICS_RATE( 1 ).C_ST_RATE(II)( 1:KK )
C
          JJ	= SCI( 1 ).EDB( II ).STICS_Len * 4 + 4
          WRITE( L1_LUN, 100 )  STICS_PHA( 1 ).C_ST_PHA(II)( 1:JJ )	! STICS_PHA
      END DO

      RETURN

9001  CONTINUE
      WRITE( TTOUT, 901 )  '  ***** ERROR STORING L1-DATA *****', STATUS
 901  FORMAT( A, '  STATUS =', I5 )
      CALL LIB$SIGNAL(%VAL(STATUS))
      STOP  1101

      END
