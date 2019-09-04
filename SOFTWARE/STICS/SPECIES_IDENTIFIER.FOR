
	SUBROUTINE SPECIES_IDENTIFIER(CALCM,CALCMQ,		!input mass, mass/charge
     .                                MASS,Q)			!output mass,mass/charge
	
C	22-AUG-1995 BY ABGalvin

	IMPLICIT NONE

	REAL*4 CALCM,CALCMQ 		!INPUT:  values for mass, m/q derived from pha 
C					         and algorithms
	REAL*4 MASS, Q			!RETURN: mass and charge of assumed species

	IF ( (CALCM.LE.1.94 .AND. CALCMQ.LE.1.71) .OR.
     .	     (CALCMQ.LE.1.30) ) THEN	
					MASS = 1.000	! H+
					   Q = 1.00
	ELSE IF(CALCM.LE.8.0 )THEN
	      
	     IF(CALCMQ.LE.1.65) THEN
					MASS = 3.00     !3He+2
					   Q = 2.00
	     ELSE IF(CALCMQ.LE.3.0) THEN
					MASS = 3.971	! 4He+2 
					   Q = 2.00
	     ELSE IF (CALCMQ.LE.6.0) THEN
					MASS = 3.971	! 4He+1
					   Q = 1.00	
	     ELSE IF (CALCMQ.LE.10.98) THEN
					MASS = 15.872   ! O+2
					   Q = 2.00
	     ELSE IF (CALCMQ.LE.15.35) THEN
					MASS = 13.896   ! N+1
					   Q = 1.00
	     ELSE
					MASS = 15.872   ! O+1
					   Q = 1.00
	     ENDIF

	ELSE IF(CALCM.LE.12.99) THEN			!C, some N, O

	     IF (CALCMQ .LE. 1.51) THEN
					MASS = 1.000	! H+ pileup
					   Q = 1.00
	     ELSE IF(CALCMQ.LE.2.18) THEN
					MASS = 11.916   ! C+6
					   Q = 6.00
	     ELSE IF(CALCMQ.LE.2.54) THEN
					MASS = 11.916   ! C+5
					   Q = 5.00
	     ELSE IF(CALCMQ.LE.2.78) THEN
					MASS = 15.872   ! partial O+6
					   Q = 6.00
	     ELSE IF(CALCMQ.LE.3.55) THEN
					MASS = 11.916   ! C+4
					   Q = 4.00
	     ELSE IF(CALCMQ.LE.4.40) THEN
					MASS = 11.916  ! C+3
					   Q = 3.00
	     ELSE IF(CALCMQ.LE.4.97) THEN
					MASS = 13.896  ! N+3
					   Q = 3.00
	     ELSE IF(CALCMQ.LE.5.61) THEN
					MASS = 15.872  ! partial O+3	
					   Q = 3.00
	     ELSE IF (CALCMQ.LE.6.53) THEN
					MASS = 11.916  ! C+2
					   Q = 2.00
	     ELSE IF (CALCMQ.LE.7.38) THEN
					MASS = 13.896  ! N+2
					   Q = 2.00
	     ELSE IF (CALCMQ.LE.10.98) THEN
					MASS = 15.872  ! partial O+2
					   Q = 2.00
	     ELSE IF (CALCMQ.LE.15.35) THEN
					MASS = 13.896   ! N+1
					   Q = 1.00
	     ELSE
					MASS = 15.872   ! O+1
	     				   Q = 1.00
	     ENDIF


	ELSE IF(CALCM.LE.22.34) THEN			!Ne, O, some N, C

	     IF (CALCMQ .LE. 1.51) THEN
					MASS = 1.000	! H+ pileup
					   Q = 1.00
	     ELSE IF(CALCMQ.LE.2.11) THEN
					MASS = 15.872   ! O+8
					   Q = 8.00
	     ELSE IF(CALCMQ.LE.2.46) THEN
					MASS = 15.872   ! O+7
					   Q = 7.00
	     ELSE IF (CALCM.GT.17.03 .AND. CALCMQ.LE.2.54) THEN
					MASS = 20.019   !Ne +8
					   Q = 8
	     ELSE IF(CALCMQ.LE.2.87) THEN
					MASS = 15.872   ! O+6
					   Q = 6.00
	     ELSE IF(CALCMQ.LE.3.34) THEN
					MASS = 15.872   ! O+5
					   Q = 5.00
	     ELSE IF(CALCMQ.LE.4.40) THEN
					MASS = 15.872  ! O+4
					   Q = 4.00
	     ELSE IF(CALCMQ.LE.4.97) THEN
					MASS = 13.896  ! N+3
					   Q = 3.00
	     ELSE IF(CALCMQ.LE.5.61) THEN
					MASS = 15.872  ! O+3	
					   Q = 3.00
	     ELSE IF (CALCMQ.LE.7.38) THEN
					MASS = 13.896  ! N+2
					   Q = 2.00
	     ELSE IF (CALCMQ.LE.10.98) THEN
					MASS = 15.872  ! O+2
					   Q = 2.00
	     ELSE IF (CALCMQ.LE.15.35) THEN
					MASS = 13.896   ! N+1
					   Q = 1.00
	     ELSE
					MASS = 15.872   ! O+1
	     				   Q = 1.00
	     ENDIF


	ELSE IF(CALCM.LE.29.31) THEN			!Mg    + some Ne+8 + O + Si

	     IF (CALCMQ .LE. 1.51) THEN
					MASS = 1.000	! H+ pileup
					   Q = 1.00
					   Q = 1.00
	     ELSE IF (CALCMQ .LE. 2.39) THEN
					MASS = 24.112   !Mg+10
				           Q = 10.00
	     ELSE IF (CALCMQ .LE. 2.70) THEN
					MASS = 24.112   !Mg+9
				           Q = 9.00
	     ELSE IF (CALCMQ .LE. 3.10) THEN
					MASS = 24.112   !Mg+8
				           Q = 8.00
	     ELSE IF (CALCMQ .LE. 3.24) THEN
					MASS = 27.863   !Si+9
				           Q = 9.00
	     ELSE IF (CALCMQ .LE. 3.66) THEN
					MASS = 27.863   !Si+8
				           Q = 8.00
	     ELSE IF(CALCMQ.LE.4.40) THEN
					MASS = 27.863   !Si+7
					   Q = 7.00
	     ELSE IF(CALCMQ.LE.4.97) THEN
					MASS = 27.863   !Si+6
					   Q = 6.00
	     ELSE IF(CALCMQ.LE.5.61) THEN
					MASS = 15.872  ! O+3	
					   Q = 3.00
	     ELSE IF (CALCMQ.LE.7.38) THEN
					MASS = 13.896  ! N+2
					   Q = 2.00
	     ELSE IF (CALCMQ.LE.10.98) THEN
					MASS = 15.872  ! O+2
					   Q = 2.00
	     ELSE IF (CALCMQ.LE.15.35) THEN
					MASS = 13.896   ! N+1
					   Q = 1.00
	     ELSE
					MASS = 15.872   ! O+1
	     				   Q = 1.00
	     ENDIF


	ELSE IF(CALCM.LE.38.45) THEN			!Si    + some Mg + S + Fe

	     IF (CALCMQ .LE. 1.51) THEN
					MASS = 1.000	! H+ pileup
					   Q = 1.00
					   Q = 1.00
	     ELSE IF (CALCMQ .LE. 2.39) THEN
					MASS = 27.863   !Si+12
				           Q = 12.00
             ELSE IF (CALCMQ .LE. 2.70) THEN
					MASS = 27.863	!Si+11
					   Q = 11.00
	     ELSE IF (CALCMQ .LE. 2.96) THEN
					MASS = 27.863   !Si+10
					   Q = 10.00
	     ELSE IF (CALCMQ .LE. 3.24) THEN
					MASS = 27.863   !Si+9
				           Q = 9.00
	     ELSE IF (CALCMQ .LE. 3.66) THEN
					MASS = 27.863   !Si+8
				           Q = 8.00
	     ELSE IF(CALCMQ.LE.4.20) THEN
					MASS = 27.863   !Si+7
					   Q = 7.00
	     ELSE IF(CALCMQ.LE.4.40) THEN
					MASS = 55.404   !Fe+13
					   Q = 13.00
	     ELSE IF(CALCMQ.LE.4.82) THEN
					MASS = 55.404   !Fe+12
					   Q = 12.00
	     ELSE IF(CALCMQ.LE.5.28) THEN
					MASS = 55.404   !Fe+11
					   Q = 11.00
	     ELSE IF(CALCMQ.LE.5.78) THEN
					MASS = 55.404   !Fe+10
					   Q = 10.00
	     ELSE IF(CALCMQ.LE.6.53) THEN
					MASS = 55.404   !Fe+9
					   Q = 9.00
	     ELSE IF(CALCMQ.LE.7.38) THEN
					MASS = 55.404   !Fe+8
					   Q = 8.00
	     ELSE IF(CALCMQ.LE.8.4) THEN
					MASS = 55.404   !Fe+7
					   Q = 7.00
	     ELSE IF (CALCMQ.LE.10.) THEN
					MASS = 55.404  !Fe+6
					   Q = 6.00
	     ELSE IF(CALCMQ.LE.11.5) THEN
					MASS = 55.404   !Fe+5
					   Q = 5.00
	     ELSE IF (CALCMQ.LE.15.35) THEN
					MASS = 13.896   ! N+1
					   Q = 1.00
	     ELSE
					MASS = 15.872   ! O+1
	     				   Q = 1.00
	     ENDIF

	ELSE IF (CALCM .LE. 50. .AND. CALCMQ.LE.3.24) THEN			! should do S also, but ...
	     IF (CALCMQ .LE. 2.39) THEN
					MASS = 27.863   !Si+12
				           Q = 12.00
             ELSE IF (CALCMQ .LE. 2.70) THEN
					MASS = 27.863	!Si+11
					   Q = 11.00
	     ELSE IF (CALCMQ .LE. 2.96) THEN
					MASS = 27.863   !Si+10
					   Q = 10.00
	     ELSE IF (CALCMQ .LE. 3.24) THEN
					MASS = 27.863   !Si+9
	     			           Q = 9.00
	     ENDIF

	ELSE 						!Fe

	     IF (CALCMQ .LE. 1.51) THEN
					MASS = 1.000	! H+ pileup
					   Q = 1.00
	     ELSE IF(CALCMQ.LE.3.34) THEN
					MASS = 55.404   !Fe+17
					   Q = 17.00
	     ELSE IF(CALCMQ.LE.3.55) THEN
					MASS = 55.404   !Fe+16
					   Q = 16.00
	     ELSE IF(CALCMQ.LE.3.77) THEN
					MASS = 55.404   !Fe+15
					   Q = 15.00
	     ELSE IF(CALCMQ.LE.4.14) THEN
					MASS = 55.404   !Fe+14
					   Q = 14.00
	     ELSE IF(CALCMQ.LE.4.40) THEN
					MASS = 55.404   !Fe+13
					   Q = 13.00
	     ELSE IF(CALCMQ.LE.4.82) THEN
					MASS = 55.404   !Fe+12
					   Q = 12.00
	     ELSE IF(CALCMQ.LE.5.28) THEN
					MASS = 55.404   !Fe+11
					   Q = 11.00
	     ELSE IF(CALCMQ.LE.5.78) THEN
					MASS = 55.404   !Fe+10
					   Q = 10.00
	     ELSE IF(CALCMQ.LE.6.53) THEN
					MASS = 55.404   !Fe+9
					   Q = 9.00
	     ELSE IF(CALCMQ.LE.7.38) THEN
					MASS = 55.404   !Fe+8
					   Q = 8.00
	     ELSE IF(CALCMQ.LE.8.4) THEN
					MASS = 55.404   !Fe+7
					   Q = 7.00
	     ELSE IF (CALCMQ.LE.10.) THEN
					MASS = 55.404  !Fe+6
					   Q = 6.00
	     ELSE IF(CALCMQ.LE.11.5) THEN
					MASS = 55.404   !Fe+5
					   Q = 5.00
	     ELSE IF (CALCMQ.LE.15.35) THEN
					MASS = 13.896   ! N+1
					   Q = 1.00
	     ELSE
					MASS = 15.872   ! O+1
	     				   Q = 1.00
	     ENDIF



	END IF

        RETURN
        END
