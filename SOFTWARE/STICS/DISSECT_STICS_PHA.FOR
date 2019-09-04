C
      SUBROUTINE DISSECT_STICS_PHA(PHA,STOP_ID,START_RANGE,ENERGY,
     .                       SECTOR,SSD_ID,TIME_OF_FLIGHT,PSTART,RANGE)
C
C     This subroutine does bit computations to get Stop ID, Start ID, 
C     Energy, Sector, SSD ID, and Time of flight for STICS PHA words on WIND
C
C      
C DESIGN:
C  
C     Use MVBITS to extract a bit field and store it into an INTEGER*4 variable.
C  
C     A STICS PHA word is 32 bits long and is coded as follows:
C
C   MSB
C    31  30  29  28  27  26  25  24  23  22  21  20  19  18  17  16
C   Stop_ID  < Start_ID&Range >  <-----        Energy        ----->
C
C                                                               LSB
C    15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0
C    <-- Sector -->  SSD_ID   <----       Time of Flight      ---->
C
C     The Start_ID&Range field actually has the Start_ID and Range values 
C     encoded into it using the formula
C
C     Start_ID&Range = 3 * Start_ID + Range

C     MODIFICATION HISTORY
C     created by ABG from an early version called DISSECT_PHA by J Paquette. 
C     and comments from L. Bleau
C     version 1:  6 Jan 95


      IMPLICIT NONE

C     dissected pha word:
      INTEGER*4 ENERGY,		!compressed energy channel (9 bit)
     .          SECTOR,         !azimuthal sector (0-15)
     .          SSD_ID,         !solid state detector id (0-3)
     .          STOP_ID,        !stop mcp id
     .          START_RANGE,PSTART,RANGE,  !combined start anode and range
     .          TIME_OF_FLIGHT  !time of flight channel (10 bit)
              
      INTEGER*4 ITEMP		!temporary variable
      BYTE PHA(4)               !the entire pha word
C
      STOP_ID = 0
      ITEMP = PHA(1)
      CALL MVBITS( ITEMP, 6, 2, STOP_ID, 0 )
C
      START_RANGE = 0
      CALL MVBITS( ITEMP, 1, 5, START_RANGE, 0 )
C
      ENERGY = 0
      ENERGY = ISHFT( IAND( ITEMP, 1 ), 8 )
      ITEMP = PHA(2)
      ENERGY = ENERGY + IAND( ITEMP, 255 )
C
      SECTOR = 0
      ITEMP = PHA(3)
      CALL MVBITS( ITEMP, 4, 4, SECTOR, 0 )
C
      SSD_ID = 0
      CALL MVBITS( ITEMP, 2, 2, SSD_ID, 0 )

C
      TIME_OF_FLIGHT = 0
      TIME_OF_FLIGHT = ISHFT( IAND( ITEMP, 3 ), 8 )
      ITEMP = PHA(4)
      TIME_OF_FLIGHT = TIME_OF_FLIGHT + IAND( ITEMP, 255 )

C     Get START and RANGE from START_RANGE  , where
C	                   START_RANGE = START ANODE*3 + PHA RANGE

      PSTART = 0
      RANGE  = 0
      PSTART = START_RANGE/3
      RANGE  = START_RANGE - 3*PSTART

      RETURN
      END
