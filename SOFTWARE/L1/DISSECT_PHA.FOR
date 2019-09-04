      SUBROUTINE DISSECT_PHA(PHA,SECTOR,RANGE,ANODE,TIME_OF_FLIGHT)
C+
C 
C FUNCTIONAL DESCRIPTION:	
C 
C    This subroutine extracts the sector, range, anode, and time of flight
C    fields from a PHA word and returns each component to the caller as an
C    INTEGER*4 quantity.
C
C  NOTE: This subroutine is obsolete.  The routine WIND_DISSECT_MASS_PHA should
C        be called instead.  To provide backward compatibility DISSECT_PHA
C        calls WIND_DISSECT_MASS_PHA with the proper argument list.
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
C 
C This version:   18-JAN-1995
C 
C Created: 
C 
C	 6-JAN-1995    J.PAQUETTE
C 
C Revised:
C 
C   18-JAN-1995   L. Bleau    changed to call WIND_DISSECT_MASS_PHA
C-
      INTEGER*4 SECTOR,RANGE,ANODE,TIME_OF_FLIGHT
      INTEGER*4 ITEMP
      BYTE PHA(2)
C
      CALL WIND_DISSECT_MASS_PHA(PHA,SECTOR,RANGE,ANODE,TIME_OF_FLIGHT)
      RETURN
      END
