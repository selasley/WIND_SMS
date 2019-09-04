      SUBROUTINE WIND_DISSECT_STICS_PHA( PHA,SECTOR,RANGE,ENERGY,TIME_OF_FLIGHT,
     *                                   START_ID,STOP_ID,SSD_ID )
C+
C 
C FUNCTIONAL DESCRIPTION:	
C 
C    This subroutine extracts the sector, range, energy, time of flight,
C    start ID, stop ID, and SSD ID fields from a WIND STICS PHA word and
C    returns each component to the caller as an INTEGER*4 quantity.
C
C    NOTE: This routine only works for STICS PHA words.
C 
C FORMAL PARAMETERS:
C  
C     PHA:
C	  A BYTE array of length 4 containing the STICS PHA word to be
C	  dissected. 
C      
C     SECTOR:
C	  An INTEGER*4 variable which will receive the extracted sector field
C         of the PHA word.
C      
C     RANGE:
C	  An INTEGER*4 variable which will receive the extracted and decoded
C         range field of the PHA word.
C      
C     ENERGY:
C	  An INTEGER*4 variable which will receive the extracted energy field
C         of the PHA word.
C      
C     TIME_OF_FLIGHT:
C	  An INTEGER*4 variable which will receive the extracted time of flight
C         field of the PHA word.
C      
C     START_ID:
C	  An INTEGER*4 variable which will receive the extracted and decoded
C         Start ID field of the PHA word.
C      
C     STOP_ID:
C	  An INTEGER*4 variable which will receive the extracted Stop ID
C         field of the PHA word.
C      
C     SSD_ID:
C	  An INTEGER*4 variable which will receive the extracted SSD ID
C         field of the PHA word.
C      
C DESIGN:
C  
C     Use MVBITS to extract a bit field and store it into an INTEGER*4 variable.
C  
C A STICS PHA word is 32 bits long and is coded as follows:
C
C   MSB
C    31  30  29  28  27  26  25  24  23  22  21  20  19  18  17  16
C   Stop_ID  < Start_ID&Range >  <-----        Energy        ----->
C
C                                                               LSB
C    15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0
C    <-- Sector -->  SSD_ID   <----       Time of Flight      ---->
C
C  The Start_ID&Range field actually has the Start_ID and Range values encoded
C into it using the formula
C
C   Start_ID&Range = 3 * Start_ID + Range
C
C 
C This version:   17-JAN-1995
C 
C Created: 
C 
C	 6-JAN-1995    J.PAQUETTE
C 
C Revised:
C 
C  17-JAN-1995   L. Bleau   added START_ID,STOP_ID,SSD_ID arguments and code,
C                           made specific to STICS sensor, changed name
C 
C-
      INTEGER*4 SECTOR,RANGE,ENERGY,TIME_OF_FLIGHT,START_ID,STOP_ID,SSD_ID
      INTEGER*4 ITEMP,START_ID_RANGE
      BYTE PHA(4)
C
      ITEMP = PHA(1)
      STOP_ID = 0
      CALL MVBITS( ITEMP, 6, 2, STOP_ID, 0 )
C
      START_ID_RANGE = 0
      CALL MVBITS( ITEMP, 1, 5, START_ID_RANGE, 0 )
      START_ID = START_ID_RANGE / 3
      RANGE = MOD( START_ID_RANGE, 3 )
C
      ENERGY = 0
      CALL MVBITS( ITEMP, 0, 1, ENERGY, 8 )
      ITEMP = PHA(2)
      CALL MVBITS( ITEMP, 0, 8, ENERGY, 0 )
C
      ITEMP = PHA(3)
      SECTOR = 0
      CALL MVBITS( ITEMP, 4, 4, SECTOR, 0 )
C
      SSD_ID = 0
      CALL MVBITS( ITEMP, 2, 2, SSD_ID, 0 )
C
      TIME_OF_FLIGHT = 0
      CALL MVBITS( ITEMP, 0, 2, TIME_OF_FLIGHT, 8 )
      ITEMP = PHA(4)
      CALL MVBITS( ITEMP, 0, 8, TIME_OF_FLIGHT, 0 )
C
      RETURN
      END
