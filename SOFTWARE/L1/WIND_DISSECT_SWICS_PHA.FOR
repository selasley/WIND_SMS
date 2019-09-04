      SUBROUTINE WIND_DISSECT_SWICS_PHA(PHA,SECTOR,ENERGY,RANGE,SSD_ID,
     *                                  TIME_OF_FLIGHT)
C+
C 
C FUNCTIONAL DESCRIPTION:	
C 
C    This subroutine extracts the sector, range, energy, SSD ID, and time of
C    flight fields from a WIND SWICS PHA word and returns each component to
C    the caller as an INTEGER*4 quantity.
C
C    NOTE: This routine only works for SWICS PHA words.
C 
C FORMAL PARAMETERS:
C  
C     PHA:
C	  A BYTE array of length 4 containing the SWICS PHA word to be
C	  dissected. 
C      
C     SECTOR:
C	  An INTEGER*4 variable which will receive the extracted sector field
C         of the PHA word.
C      
C     ENERGY:
C	  An INTEGER*4 variable which will receive the extracted energy field
C         of the PHA word.
C      
C     RANGE:
C	  An INTEGER*4 variable which will receive the extracted and decoded
C         range field of the PHA word.
C      
C     SSD_ID:
C	  An INTEGER*4 variable which will receive the extracted SSD ID
C         field of the PHA word.
C      
C     TIME_OF_FLIGHT:
C	  An INTEGER*4 variable which will receive the extracted time of flight
C         field of the PHA word.
C      
C DESIGN:
C  
C     Use MVBITS to extract a bit field and store it into an INTEGER*4 variable.
C  
C A SWICS PHA word is 32 bits long and is coded as follows:
C
C   MSB
C    31  30  29  28  27  26  25  24  23  22  21  20  19  18  17  16
C     0   0   0   0  <-- Sector -->  <-----      Energy      ----->
C
C                                                               LSB
C    15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0
C    <--  Range -->  SSD_ID   <----       Time of Flight      ---->
C    
C 
C This version:   17-JAN-1995
C 
C Created: 
C 
C	17-NOV-1994    J.PAQUETTE
C 
C Revised:
C 
C  17-JAN-1995   L. Bleau   copied from SWICS_PHA_TABLE.FOR, made its own
C                           routine, added comments
C 
C-
      INTEGER*4 SECTOR,ENERGY,RANGE,SSD_ID,TIME_OF_FLIGHT
      INTEGER*4 ITEMP
      BYTE PHA(4)
C
      SECTOR = 0
      ITEMP = PHA(1)
      CALL MVBITS( ITEMP, 0, 4, SECTOR, 0 )
C
      ENERGY = PHA(2)
      ENERGY = IAND( ENERGY, 255 )
C
      ITEMP = PHA(3)
      RANGE = 0
      CALL MVBITS( ITEMP, 4, 4, RANGE, 0 )
C
      SSD_ID = 0
      CALL MVBITS( ITEMP, 2, 2, SSD_ID, 0 )
C
      TIME_OF_FLIGHT = ISHFT( IAND( ITEMP, 3 ), 8 )
      ITEMP = PHA(4)
      TIME_OF_FLIGHT = TIME_OF_FLIGHT + IAND( ITEMP, 255 )
C
      RETURN
      END
