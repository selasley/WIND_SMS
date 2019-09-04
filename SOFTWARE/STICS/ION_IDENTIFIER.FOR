
	SUBROUTINE ION_IDENTIFIER(CALCM,CALCMQ,		!input mass, mass/charge
     .                                MASS,Q,INDEX)	!output mass,charge,array index
	
C	29-MAR-1996 BY K. CHOTOO using SPECIES_IDENTIFIER as template

	IMPLICIT NONE

	REAL*4 CALCM,CALCMQ 		!INPUT:  values for mass, m/q derived from pha 
C					         and algorithms
	REAL*4 MASS, Q			!RETURN: mass, charge, and array position of assumed species
	INTEGER*4 INDEX

	MASS = -1
	Q    = -1
	INDEX= -1

	IF(CALCM.EQ.0)THEN
	     IF(CALCMQ.GE.0.9 .AND. CALCMQ.LT.1.2588)THEN
					MASS = 1.000		! H+,   1  (DCR, use same index as TCR for now)
					   Q = 1.00
				       INDEX = 1
	     ENDIF
	     IF(CALCMQ.GE.1.6066 .AND. CALCMQ.LE.2.3167) THEN
					MASS = 3.971     	!4He+2, 2  (DCR, use same index as TCR for now)
					   Q = 2.00
				       INDEX = 2
             ENDIF
	     IF(CALCMQ.GE.3.5507 .AND. CALCMQ.LE.4.6723) THEN
					MASS = 3.971    	!4He+1, 3  (DCR, use same index as TCR for now)
					   Q = 1.00
				       INDEX = 3
             ENDIF
	ENDIF

	IF((CALCM.GE.0.5 .AND. CALCM.LE.2.1261).AND.
     .     (CALCMQ.GE.0.90 .AND. CALCMQ.LE.1.2588))THEN	
					MASS = 1.000		! H+,   1  (TCR)
					   Q = 1.00
				       INDEX = 1
	ENDIF
	
        IF(CALCM.GE.1.7742 .AND. CALCM.LE.6.8920)THEN           !New He boxes, Mass different from DPU definition
	     IF(CALCMQ.GE.1.7077 .AND. CALCMQ.LE.2.3167) THEN
					MASS = 3.971     	!4He+2, 2  (TCR)
					   Q = 2.00
				       INDEX = 2
             ENDIF
	     IF(CALCMQ.GE.3.5507 .AND. CALCMQ.LE.4.6723) THEN
					MASS = 3.971    	!4He+1, 3  (TCR)
					   Q = 1.00
				       INDEX = 3
             ENDIF
        ENDIF

	IF(CALCM.GE.9.0409 .AND. CALCM.LE.12.9830)THEN
	     IF(CALCMQ.GE.1.8713 .AND. CALCMQ.LE.2.1796) THEN
					MASS = 11.916     	!C+6,   4
					   Q = 6.00
				       INDEX = 4
             ENDIF
	     IF(CALCMQ.GE.2.2471 .AND. CALCMQ.LE.2.5387) THEN
					MASS = 11.916     	!C+5,   5
					   Q = 5.00
				       INDEX = 5
             ENDIF
	     IF(CALCMQ.GE.2.8681 .AND. CALCMQ.LE.3.2402) THEN
					MASS = 11.916       	!C+4,   6
					   Q = 4.00
				       INDEX = 6
             ENDIF
        ENDIF

	IF( (CALCM.GE.14.2120 .AND. CALCM.LE.20.4090).AND.
     .      (CALCMQ.GE.2.1141 .AND. CALCMQ.LE.2.4624) ) THEN
					MASS = 15.872		!O+7,   7
					   Q = 7.00
				       INDEX = 7       
        ENDIF

	IF( (CALCM.GE.10.834 .AND. CALCM.LE.20.409).AND.
     .      (CALCMQ.GE.2.5387 .AND. CALCMQ.LE.2.8681) ) THEN
					MASS = 15.872		!O+6,   8
					   Q = 6.00
				       INDEX = 8       
        ENDIF

	IF( (CALCM.GE.4.7994 .AND. CALCM.LE.79.277).AND.
     .      (CALCMQ.GE.14.890 .AND. CALCMQ.LE.20.20) ) THEN
					MASS = 15.872		!O+1,   9
					   Q = 1.00
				       INDEX = 9       
	ENDIF

	IF( (CALCM.GE.17.031 .AND. CALCM.LE.22.341).AND.
     .      (CALCMQ.GE.2.3167 .AND. CALCMQ.LE.2.5387) ) THEN
					MASS = 20.019		!Ne+8,  10
					   Q = 8.00
				       INDEX = 10       
       	ENDIF

	IF(CALCM.GE.22.341 .AND. CALCM.LE.26.772) THEN
             IF(CALCMQ.GE.2.2471 .AND. CALCMQ.LE.2.5387) THEN
					MASS = 24.112		!Mg+10, 11
					   Q = 10.00
				       INDEX = 11
	     ENDIF
       	     IF(CALCMQ.GE.2.8681 .AND. CALCMQ.LE.3.2402) THEN
					MASS = 24.112		!Mg+8,  12
					   Q = 8.00
				       INDEX = 12
	     ENDIF
        ENDIF

	IF(CALCM.GE.26.772 .AND. CALCM.LE.38.445) THEN
             IF(CALCMQ.GE.2.1796 .AND. CALCMQ.LE.2.3885) THEN
					MASS = 27.863		!Si+12, 13
					   Q = 12.00
				       INDEX = 13
	     ENDIF
       	     IF(CALCMQ.GE.2.9569 .AND. CALCMQ.LE.3.2402) THEN
					MASS = 27.863		!Si+9,  14
					   Q = 9.00
				       INDEX = 14
	     ENDIF
       	     IF(CALCMQ.GE.3.2402 .AND. CALCMQ.LE.3.6607) THEN
					MASS = 27.863		!Si+8,  15
					   Q = 8.00
				       INDEX = 15
	     ENDIF
        ENDIF

	IF(CALCM.GE.38.445 .AND. CALCM.LE.79.277) THEN
             IF(CALCMQ.GE.3.3406 .AND. CALCMQ.LE.3.5507) THEN
					MASS = 55.404		!Fe+16, 16
					   Q = 16.00
				       INDEX = 16
	     ENDIF
       	     IF(CALCMQ.GE.3.7741 .AND. CALCMQ.LE.4.1357) THEN
					MASS = 55.404		!Fe+14, 17
					   Q = 14.00
				       INDEX = 17
	     ENDIF
       	     IF(CALCMQ.GE.4.3958 .AND. CALCMQ.LE.4.8170) THEN
					MASS = 55.404		!Fe+12, 18
					   Q = 12.00
				       INDEX = 18
	     ENDIF
       	     IF(CALCMQ.GE.4.8170 .AND. CALCMQ.LE.5.2786) THEN
					MASS = 55.404		!Fe+11, 19
					   Q = 11.00
				       INDEX = 19
	     ENDIF
       	     IF(CALCMQ.GE.5.2786 .AND. CALCMQ.LE.5.7843) THEN
					MASS = 55.404		!Fe+10, 20
					   Q = 10.00
				       INDEX = 20
	     ENDIF
       	     IF(CALCMQ.GE.5.7843 .AND. CALCMQ.LE.6.5349) THEN
					MASS = 55.404		!Fe+9,  21
					   Q = 9.00
				       INDEX = 21
	     ENDIF
       	     IF(CALCMQ.GE.6.5349 .AND. CALCMQ.LE.7.3828) THEN
					MASS = 55.404		!Fe+8,  22
					   Q = 8.00
				       INDEX = 22
	     ENDIF

	ENDIF

        RETURN
        END
