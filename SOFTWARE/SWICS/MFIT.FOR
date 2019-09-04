	SUBROUTINE MFIT(V1,F1,V2,F2,V3,F3,N,V0,SIG)
C
C***************************************************************************
C	given three points this subroutine solves the convective maxwellian,
C
C		f(r,v) = N*(pi*sigma)^(-3/2) * exp(-(v-v0)^2/sigma)
C			where sigma = 2kT/m
C
C	for N (density), v0, and sigma.
C
C	this program is derived from GFIT.
C
C	written by C. Shafer, July 14, 1992
C
C	Dec. 4, 1992 - altered to use 1-D rather than 3-D calculation.
C
C***************************************************************************
C
C				DECLARATIONS
     	IMPLICIT NONE
C
	REAL*4
     .   F1, F2, F3				!counts
C
     	REAL*4
     .   V1, V2, V3,
     .	 V0, N, SIG					!velocities, center
C							density, & sigma
     	REAL*4
     .	 PI/3.14159/,
     .	 LF1F2, LF2F3					!log values
C
C***************************************************************************
C
C				STATEMENTS
C
     	IF (F1/F2.LE.0 .OR. F2/F3.LE.0) THEN
     	  V0 = 0.0
     	  N = 0.0
     	  SIG = 0.0
     	ELSE
C							calculate v0
          LF1F2 = LOG(F1/F2)
     	  LF2F3 = LOG(F2/F3)
C
     	  V0 = ((V3*V3 - V2*V2)*LF1F2 - (V2*V2 - V1*V1)*LF2F3) / 
     .		(2*((V1-V2)*LF2F3 - (V2-V3)*LF1F2))
C
C     							calculate sigma
     	  SIG = (V2*V2 - V1*V1 + 2*V0*(V1-V2)) / LF1F2
C
C							calculate n
     	  IF (SIG.GT.0) THEN
     	    N = SQRT(PI*SIG) * F2 * EXP(((V2-V0)**2)/SIG)
     	    N = N * 1E-15				!unit correction
C					now n[cm^-3], f[sec/km^4], v[km/s]
     	  ELSE
     	    N = 0.0
     	  END IF
C
     	END IF						!f1/f2<0
	END
