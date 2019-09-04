      SUBROUTINE WIND_CONCATENATE_STICS_PHA(SCI_REC_NO)
C
C	Concatenates STICS PHA. In low(=normal) bit rate, a voltage step for
C	STICS lasts for 2 spins. The resultant data is split up and part of
C	it is output over a pair of spins. This routine puts the two parts of 
C	the data back together in the even-numbered spin of the pair. Only a 
C	placeholder is left in the odd-numbered spin of the pair.
C
C	This version:   1.4    1-MAR-1995
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
C       19-JAN-1995     J.PAQUETTE      this version concatenates only the PHA
C  1.2   6-FEB-1995     J.PAQUETTE      added check to handle loss of bitrate
C                       L.BLEAU         info at data gap
C	 9-FEB-1995	L. Bleau	change comment delimiters
C	 9-FEB-1995	L. Bleau	change include stmt to use EDBDEF.INC
C					and EDBVARS.INC instead of EDB.INC only
C  1.3  16-FEB-1995	J.PAQUETTE	correct error in quality flag setting
C  1.4   1-MAR-1995     J.PAQUETTE      if one spin contains good PHA data,
C                                       and the other's data is marked bad,set
C                                       bit 4 in the PHA quality flag 
C
CDEC$ IDENT	'1.4'

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
C
C Find number of STICS PHA bytes output in IIth spin. Since STICS PHA words are
C 4 bytes long, this is 4 times the number of PHA words. The additional + 4 is
C due to the presence of a 4-character identifier at the start of each PHA 
C record.
C
          JJ = SCI( SCI_REC_NO ).EDB( II ).STICS_Len * 4 + 4		

C
C Find number of STICS PHA bytes output in spin II+1. The logic is the same as
C above.
C 
          KK = SCI( SCI_REC_NO ).EDB( II + 1 ).STICS_Len * 4 + 4
C
C Transfer PHA data from spin II+1 to spin II, adding it to what's already 
C there. Since the first 4 characters (characters 0 to 3) are an identifier 
C ("STPH" in this case) we don't want to transfer them; thus the initial index
C for the source record is 4, not 0.
C
          IF(KK.NE.4)THEN
            STICS_PHA( SCI_REC_NO ).C_ST_PHA( II )( JJ + 1 : JJ + KK - 4 ) = 
     &      STICS_PHA( SCI_REC_NO ).C_ST_PHA( II + 1 )( 5 : KK ) 	
C
C Adjust the # of PHA words for this spin to reflect the change
C
	    SCI( SCI_REC_NO ).EDB( II ).STICS_Len = SCI( SCI_REC_NO ).EDB( II ).STICS_Len +
     &      SCI( SCI_REC_NO ).EDB( II + 1 ).STICS_Len
C
C Adjust quality flags. Note that this depends on LOGICAL*1 being equivalent to
C BYTE. If we have one good and one bad quality flag, set bit 4
C
            IF((SCI(SCI_REC_NO).EDB(II).ST_P_Qual.EQ.0).AND.(SCI(SCI_REC_NO).EDB(II+1).ST_P_Qual.NE.0).OR.
     &        (SCI(SCI_REC_NO).EDB(II).ST_P_Qual.NE.0).AND.(SCI(SCI_REC_NO).EDB(II+1).ST_P_Qual.EQ.0))THEN
              SCI(SCI_REC_NO).EDB(II).ST_P_Qual = IBSET( SCI(SCI_REC_NO).EDB(II).ST_P_Qual, 4 )
            ENDIF
	    SCI(SCI_REC_NO).EDB(II).ST_P_Qual =
     &       SCI(SCI_REC_NO).EDB(II).ST_P_Qual.OR.SCI(SCI_REC_NO).EDB(II+1).ST_P_Qual
C
C Remove data from odd-numbered spin
C
            DO 300 LL=0,(SCI( SCI_REC_NO ).EDB( II + 1 ).STICS_Len-1)*4
              STICS_PHA( SCI_REC_NO ).EDB( II + 1 ).PHA(LL) = 0
 300        CONTINUE
C
C Adjust the # of PHA words for this spin to reflect the change
C
            SCI( SCI_REC_NO ).EDB( II + 1 ).STICS_Len = 0
C
C Adjust quality flags. Since the odd spins no longer contain data, set them to
C 8 ( which denotes 'missing EDB'; see EDB.INC).
C
	    SCI(SCI_REC_NO).EDB(II + 1).ST_P_Qual = 8
          ENDIF
C
        ENDIF
 400  CONTINUE
C
      RETURN
C
      END
