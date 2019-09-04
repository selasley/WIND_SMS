
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE COMP_GETT_TRANS(V_SWX, V_SWY, V_SWZ, V_ION, SECT, TELESCOPE,	!input
     .				 V_ION_SWF, SECT_SWF, FLUX_CG_CORR)	        !output
c
c This subroutine receives the cartesian components of the solar wind speed (V_SWX,Y,Z),
c the velocity of a particular ion (V_ION) and its azimuthal sector (SECT) and polar 
c direction (TELESCOPE). The routine calculates the ion speed in the solar wind frame
c using:
c		V' = V - Vsw
c
c The corresponding shift in the flux is given by:
c
c		dJ'/dE' = (V'/V)**2 * dJ/dE  	    {Gold et al., 14th ICRC, p.1801,1975}
c 
c Therefore, FLUX_CG_CORR = (V_ION_SWF/V_ION)**2
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      IMPLICIT NONE
      REAL*4 PI, SECTOR_SIZE
      PARAMETER (PI = 3.141592654, SECTOR_SIZE = 22.5)
C
      INTEGER*4  SECT,				!Azimuthal Sector
     .		 TELESCOPE			!Telescope 1, 2, 3: polar direction
C
      REAL*4 FLUX_CG_CORR,			!Compton getting correction to flux
     . 	     PHI,				!Azimuthal angle for given sector
     .	     THETA,				!Polar angle for given telescope
     .	     SECT_SWF,				!Azimuthal sector in solar wind frame
     .	     V_ION,				!Velocity of ion in spacecraft frame
     .	     V_ION_SWF,				!Velocity of ion in solar wind frame
     .	     V_SWX, V_SWY, V_SWZ,		!Solar wind speed in cartesian coordinates
     .	     VEL_X, VEL_Y, VEL_Z		!Cartesian components in solar wind frame
      
C
C Get polar angle
C
      IF (TELESCOPE .EQ. 1) THEN
         THETA = 143.0 * PI / 180.0
      ELSEIF (TELESCOPE .EQ. 3) THEN
         THETA = 37.0 * PI / 180.0
      ELSE
         THETA = 90.0 * PI / 180.0
      ENDIF
C
C Get azimuthal angle
C
      IF (SECT .EQ. 0) THEN
         PHI = SECTOR_SIZE * PI / 180.0
      ELSEIF (SECT .EQ. 1) THEN
         PHI = 0
      ELSE
         PHI = (17 - SECT) * SECTOR_SIZE * PI / 180.0
      ENDIF    
    
C
C Calculate ion velocity in solar wind frame
C
      VEL_X = V_ION*SIN(THETA)*COS(PHI) - V_SWX
      VEL_Y = V_ION*SIN(THETA)*SIN(PHI) - V_SWY
      VEL_Z = V_ION*COS(THETA)          - V_SWZ
	   
      V_ION_SWF = SQRT( VEL_X**2 + VEL_Y**2 + VEL_Z**2 ) 
	  
C
C Calculate flux correction
C
      FLUX_CG_CORR = (V_ION_SWF/V_ION)**2
      
C 
C Calculate azimuthal sector in solar wind frame
C
      PHI_SWF = ATAN(VEL_X,VEL_Y)



