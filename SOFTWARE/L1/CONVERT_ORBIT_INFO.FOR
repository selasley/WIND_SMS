	SUBROUTINE convert_orbit_info(units, system, nsize, position, 
	1 earth_dist)
 
C+
C 
C FUNCTIONAL DESCRIPTION:	
C 
C    This subroutine is called if the position information needs to be
C    transformed to some other systems of units, such as Earth radii, or to
C    spherical coordinates.  The input coordinates are assumed to be in units
C    of kilometers in a Cartesian space (x,y,z).
C
C    While this is inteded to be used on positions, it will also work on
C    velocity data since they are stored in the same type of structure
C    (/EPHEMERIS/) as position data.
C
C    In addition the distance from Earth (effectively the value of r in the
C    spherical coordinate system) is always computed and returned in the last
C    argument.
C    
C FORMAL PARAMETERS:
C      
C     UNITS:
C	  INT*4	    An encoded value indicating whether or not the units are to
C		    be changed and to what.  The allowable values are:
C			0	no change in units
C			1	Earth radii
C			other	same as 0
C      
C     SYSTEM:
C	  INT*4	    An encoded value indicating whether or not the coordinate
C		    system is to be changed to spherical.  The allowable values are:
C			0	no change to coordinate system
C			1	change to spherical coordinates (r,theta,phi)
C			other	same as 0
C      
C     NSIZE:
C	  INT*4	    The number of elements in the arrays POSITION and
C		    EARTH_DIST.
C  
C     POSITION:
C	  /EPHEMERIS/	An array of NSIZE elements, each of which is a RECORD of
C		    structure type /EPHEMERIS/.  When called this argument
C		    contains the position (or velocity) data.  Upon return this
C		    argument will have been overwritten with the transformed
C		    position (or velocity) data.
C      
C     EARTH_DIST:
C	  REAL*8    An array of NSIZE elements which will, upon return, contain
C		    the distance of each position from Earth (0,0,0) in the
C		    appropriate units.
C  
C COMMON BLOCKS:
C  
C     None
C  
C DESIGN:
C  
C     set up conversion factors to use, logical switches to control code
C     process data, performing coordinate transformation
C     process data, performing unit conversion
C
C NOTES:
C
C     The value used for the Earth's radius is the mean equatorial radius,
C     6378.077.  This was obtained from the CRC Handbook, 63rd ed., p F-154.
C
C This version:    1-JUN-1995	  1.0
C 
C Created: 
C 
C	 1-JUN-1995    Larry Bleau
C 
C Revised:
C 
C	 Date     | Name  | Description
C ----------------+-------+-----------------------------------------------------
C [change_entry]
C 
C-
CDEC$	IDENT '1.0'
	IMPLICIT NONE
	INCLUDE 'INC_DIR:EPHDEF.INC'
	INTEGER*4 units, system, nsize
	RECORD /EPHEMERIS/ position(nsize)
	REAL*8 earth_dist(nsize)
C
	INTEGER i
	REAL*8 factor, earth_radius, theta, phi
	DATA earth_radius/6378.077/    ! mean equatorial radius
C
	factor = 1.0
	IF (units .eq. 1) factor = 1.0 / earth_radius
C
C Change units and compute distance from Earth
C
	DO i=1,nsize
	    position(i).x = position(i).x * factor
	    position(i).y = position(i).y * factor
	    position(i).z = position(i).z * factor
	    earth_dist(i) = SQRT (position(i).x**2 + position(i).y**2 +
	1			  position(i).z**2)
	END DO
C
C Change coordinate system to spherical if requested
C Part of the job ius already done in the EARTH_DIST array
C Make sure to store into POSITION last as we are overwriting the x,y,z values
C
	IF (system .eq. 1) THEN
	    DO i=1,nsize
		phi = acos( position(i).z / earth_dist(i) )
		theta = dasin( position(i).y / (earth_dist(i) * sin(phi) ) )
		position(i).r = earth_dist(i)
		position(i).theta = theta
		position(i).phi = phi * 360.0D0 / 3.141592653589793D0
	    END DO
	END IF
C
	RETURN
	END
