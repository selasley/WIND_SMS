      SUBROUTINE WIND_DISSECT_MASS_PHA( PHA,SECTOR,RANGE,ANODE,TIME_OF_FLIGHT )
C+
C 
C FUNCTIONAL DESCRIPTION:	
C 
C    This subroutine extracts the sector, range, anode, and time of flight
C    fields from a WIND MASS PHA word and returns each component to the caller
C    as an INTEGER*4 quantity.
C
C    NOTE: This routine only works for MASS PHA words.
C 
C FORMAL PARAMETERS:
C  
C     PHA:
C	  A BYTE array of length 2 containing the PHA word to be dissected.
C      
C     SECTOR:
C	  An INTEGER*4 variable which will receive the extracted sector field
C         of the PHA word.
C      
C     RANGE:
C	  An INTEGER*4 variable which will receive the extracted range field
C         of the PHA word.
C      
C     ANODE:
C	  An INTEGER*4 variable which will receive the extracted anode field
C         of the PHA word.
C      
C     TIME_OF_FLIGHT:
C	  An INTEGER*4 variable which will receive the extracted time of flight
C         field of the PHA word.
C      
C DESIGN:
C  
C     Use MVBITS to extract a bit field and store it into an INTEGER*4 variable.
C  
C A MASS PHA word is 16 bits long and is coded as follows:
C
C   MSB                                                         LSB
C    15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0
C     S   R   Anode  <-----          Time of Flight          ----->
C
C Where:
C  S = Sector  R = Range
C                                         
C This version:   17-JAN-1995
C 
C Created: 
C 
C	 6-JAN-1995    J.PAQUETTE
C 
C Revised:
C 
C  17-JAN-1995   L. Bleau   changed name from DISSECT_PHA to
C                           WIND_DISSECT_MASS_PHA since is specific to MASS
C                           sensor
C 
C-
      INTEGER*4 SECTOR,RANGE,ANODE,TIME_OF_FLIGHT
      INTEGER*4 ITEMP
      BYTE PHA(2)
C
      SECTOR = 0
      ITEMP = PHA(1)
      CALL MVBITS( ITEMP, 7, 1, SECTOR, 0 )
C
      RANGE = 0
      CALL MVBITS( ITEMP, 6, 1, RANGE, 0 )
C
      ANODE = 0
      CALL MVBITS( ITEMP, 4, 2, ANODE, 0 )
C
      TIME_OF_FLIGHT = ISHFT( IAND( ITEMP, 15 ), 8 )
      ITEMP = PHA(2)
      TIME_OF_FLIGHT = TIME_OF_FLIGHT + IAND( ITEMP, 255 )
C
      RETURN
      END
