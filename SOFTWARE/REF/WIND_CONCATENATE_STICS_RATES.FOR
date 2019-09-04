      SUBROUTINE WIND_CONCATENATE_STICS_RATES(SCI_REC_NO)
C
C	Concatenates STICS rates. In low(=normal) bit rate, a voltage step for
C	STICS lasts for 2 spins. The resultant data is split up and part of it
C	is output over a pair of spins. This routine puts the two parts of the
C	data back together in the even-numbered spin of the pair. Only a place-
C	holder is left in the odd-numbered spin of the pair.
C
C	This version:   1.2   16-FEB-1995
C
C     Creation:
C	JOHN PAQUETTE	18-JUL-1994
C
C     Revisions:
C	29-JUL-1994	J. PAQUETTE	Resolved bit rate confusion
C       30-SEP-1994	J. PAQUETTE     Changed this routine to modify the 
C                                       SCI_REC_NO'th science rec, instead of
C                                       the first
C       24-OCT-1994     J. PAQUETTE     Set STICS length variable in Core data
C  1.1  16-DEC-1994     L. Bleau        removed unneeded diagnostic
C       19-JAN-1995     J.PAQUETTE      this routine now concatenates Rate data
C					only 
C        6-FEB-1995     J.PAQUETTE      added check to handle loss of bitrate
C			L.BLEAU         info at data gap
C	 9-FEB-1995	L. Bleau	change comment delimiters
C	 9-FEB-1995	L. Bleau	change include stmt to use EDBDEF.INC
C					and EDBVARS.INC instead of EDB.INC only
C  1.2  16-FEB-1995	J.PAQUETTE	correct error in quality flag setting
C
CDEC$ IDENT	'1.2'

      IMPLICIT NONE

      INCLUDE	'INC_DIR:EDBDEF.INC/NOLIST'
      INCLUDE	'INC_DIR:EDBVARS.INC/NOLIST'
      INCLUDE	'INC_DIR:WIND_L0_RECORD.INC/NOLIST'
      INCLUDE   'INC_DIR:WIND_Lun.INC/NOLIST'
      INCLUDE	'($SSDEF)/NOLIST'

      INTEGER*4	II,JJ,KK,LL,SCI_REC_NO
C
C Loop over the 60 spins in a science record
C
      DO 400 II = 0, 58,2
C
C Check to see if either of the spins that we may be about to concatenate  are
C bad (i.e. have a quality flag which is nonzero). If both are bad, don't
C concatenate (since we don't really know the bitrate of either). If one of the
C two is bad, use the other's bitrate for both.
C
        IF ((SCI(SCI_REC_NO).EDB(II).Qual.NE.0).AND.(SCI(SCI_REC_NO).EDB(II+1).Qual.NE.0))GO TO 400
        IF(SCI(SCI_REC_NO).EDB(II).Qual.NE.0)THEN
          SCI(SCI_REC_NO).EDB(II).BitRate = SCI(SCI_REC_NO).EDB(II+1).BitRate
        ELSEIF(SCI(SCI_REC_NO).EDB(II+1).Qual.NE.0)THEN
          SCI(SCI_REC_NO).EDB(II+1).BitRate = SCI(SCI_REC_NO).EDB(II).BitRate
        ENDIF
C
C Concatenate only if low bitrate
C
        IF(.NOT.SCI(SCI_REC_NO).EDB(II).BitRate)THEN
          DO 200 JJ = 0,STICS_N_RATE_Size
C
C Copy rate data from spin II+1 to spin II, adding it to what's already there.
C
            STICS_RATE( SCI_REC_NO ).EDB( II ).RATE( JJ + STICS_N_RATE_Size + 1 ) =
     &	    STICS_RATE( SCI_REC_NO ).EDB( II + 1 ).RATE( JJ )
C
C Adjust quality flags. Note that this depends on LOGICAL*1 being equivalent to BYTE
C
	    SCI(SCI_REC_NO).EDB(II).ST_HMR_Qual = 
     &       SCI(SCI_REC_NO).EDB(II).ST_HMR_Qual.OR.SCI(SCI_REC_NO).EDB(II+1).ST_HMR_Qual
            SCI(SCI_REC_NO).EDB(II).ST_SMR_Qual =
     &       SCI(SCI_REC_NO).EDB(II).ST_SMR_Qual.OR.SCI(SCI_REC_NO).EDB(II+1).ST_SMR_Qual
            SCI(SCI_REC_NO).EDB(II).ST_BR_Qual =
     &       SCI(SCI_REC_NO).EDB(II).ST_BR_Qual.OR.SCI(SCI_REC_NO).EDB(II+1).ST_BR_Qual
            SCI(SCI_REC_NO).EDB(II).ST_OMR_Qual =
     &       SCI(SCI_REC_NO).EDB(II).ST_OMR_Qual.OR.SCI(SCI_REC_NO).EDB(II+1).ST_OMR_Qual
            SCI(SCI_REC_NO).EDB(II).ST_ER_Qual =
     &       SCI(SCI_REC_NO).EDB(II).ST_ER_Qual.OR.SCI(SCI_REC_NO).EDB(II+1).ST_ER_Qual
C
C Remove the data from the odd-numbered spin.
C
            STICS_RATE( SCI_REC_NO ).EDB( II + 1 ).RATE( JJ ) = 0
 200      CONTINUE
C
C Adjust quality flags. Since the odd spins no longer contain data, set them to
C 8 ( which denotes 'missing EDB'; see EDB.INC).
C
          SCI(SCI_REC_NO).EDB(II+1).ST_HMR_Qual = 8
          SCI(SCI_REC_NO).EDB(II+1).ST_SMR_Qual = 8
          SCI(SCI_REC_NO).EDB(II+1).ST_BR_Qual  = 8
          SCI(SCI_REC_NO).EDB(II+1).ST_OMR_Qual = 8
          SCI(SCI_REC_NO).EDB(II+1).ST_ER_Qual  = 8
C
C Adjust the number of rates per spin in the Core to reflect the  fact that
C some spins now contain as much rate data as a spin at high bit rate, and some
C contain no data.
C
          SCI(SCI_REC_NO).EDB(II).STICS_RATE_Size   = STICS_H_RATE_Size
          SCI(SCI_REC_NO).EDB(II+1).STICS_RATE_Size = 0
        ENDIF
 400  CONTINUE
C
      RETURN
C
      END
