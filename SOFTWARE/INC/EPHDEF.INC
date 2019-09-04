	STRUCTURE /EPHEMERIS/		! structure to describe spacecraft 
					! position in some coordinate system
	    UNION 
	    MAP
		REAL*8           x
		REAL*8           y
		REAL*8           z
	    END MAP
	    MAP
		REAL*8           r
		REAL*8           theta
		REAL*8           phi
	    END MAP
	    MAP
		REAL*8		 member(3)
	    END MAP
	    END UNION
	END STRUCTURE    !EPHEMERIS
	STRUCTURE /ATTITUDE/		! structure to describe spacecraft 
					! attitude (orientation)
	    UNION 
	    MAP
		REAL*4		 r_asc
		REAL*4		 decl
	    END MAP
	    MAP
		REAL*4		 member(2)
	    END MAP
	    END UNION
	END STRUCTURE    !ATTITUDE
