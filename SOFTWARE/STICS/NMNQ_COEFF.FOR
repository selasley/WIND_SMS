      SUBROUTINE NMNQ_COEFF(UNQ_MAX,UMQMAX,UMQMIN,    !user input
     .		            UNM_MAX,UMMAX, UMMIN,
     .                      EMIN,EMAX,TMIN,TMAX,      !return values
     .                      NQ_MAX,MQMAX,MQMIN,
     .                      NM_MAX,MMAX ,MMIN,
     .                      KM,KQ,
     .                      A,B,C1,C2A,C2B,D1,D2,E,
     .                      BREAK_C2, BREAK_MQ)
C*******************************************************************
C     This SUBROUTINE calculates coefficients for nm and nq        *
C     parameters         					   *
C     Original version 24 Jan 95 by ABG                            *
C     History:							   *
C                                                                  *
c*******************************************************************
      IMPLICIT NONE

      REAL*4 A(6),	!mass coefficients
     .       B(6),      !NM coefficients
     .       C1,C2A,C2B	!mass/charge coefficients
C			 C1= time of flight **2 of 1 keV proton
C			 C2= carbon foil correction
      INTEGER*4 BREAK_C2  !NQ value of C2 break point (83)
      REAL*4    BREAK_MQ  !M/Q value of C2 break point (~11)

      REAL*4 D1,D2,	!E/Q coefficients, D1= analyzer constant
C			                   D2= step resolution
     .       E(3)

      INTEGER*4 DEFAULT_NQ,DEFAULT_NM   !default dpu algorithm used

      REAL*4 EMIN, EMAX,		!min max energy and time
     .       TMIN, TMAX

      INTEGER*4  NM_MAX,  NQ_MAX, 	   !max bin values 
     .           UNM_MAX, UNQ_MAX, 	   !USER max bin values (-1 = use default)
     .           DNM_MAX/59/, DNQ_MAX/126/ !DPU default max bin values

      REAL*4 MMAX,  MMIN,    !measurable mass range in amu 
     .       MQMAX, MQMIN,   !measurable m/q range in amu/e
     .       UMMAX, UMMIN,   !USER measurable mass range in amu (-1 = use default)
     .       UMQMAX,UMQMIN,  !USER measurable m/q range in amu/e (-1 = use default)
     .       DMMAX  /95./, DMMIN  /0.5/,  !DPU DEFAULT measurable mass range in amu 
     .       DMQMAX /42./, DMQMIN /0.9/   !DPU DEFAULT measurable m/q range in amu/e        

      REAL*4  KM,KQ           !NM, NQ resolution parameters

      INTEGER*4 I,J,K
     
C   ***********************************************************************
C   *  Initialize constants with default dpu values (ref:  pages 345-349  *
C   *  of WIND SMS COMMAND AND DATA FORMATS FOR THE SMS INSTRUMENT R.3.0) *
C   ***********************************************************************	

      A(1) = 2.69575
      A(2) = -0.843766
      A(3) = -2.38009
      A(4) = 0.385641
      A(5) = 0.0513127
      A(6) = 0.0690096


      C1 = 1.9159E-05
      C2A= 1.5
      C2B= 2.5
      BREAK_C2 = 83
C          m/q = MQmin *   kQ                     **(nq-1)
      BREAK_MQ = 0.9   * ( (42.0/0.9)**(1./126.) )**(83.-1.)

      D1 = 6.190722
      D2 = 1.1225857

      EMIN = 21.0
      EMAX = 2675.0
      TMIN = 11.0
      TMAX = 408.0

C   ***********************************************************************
C   *  determine if default (i.e., dpu) binning will be used, in whole or *
C   *  in part, or not at all.  Otherwise user input for min, max, number *
C   *  of bins will be used.                                              *
C   ***********************************************************************

      DEFAULT_NQ = 0		!initialize flag for dpu default
      DEFAULT_NM = 0
      IF(UMMIN.LT.0.  .AND.  UMMAX.LT.0.   .AND.   UNM_MAX.LT.0)
     .   DEFAULT_NM = 1		!if true, dpu bins for nm to be used
      IF(UMQMAX.LT.0. .AND.  UMQMIN.LT.0.  .AND.   UNQ_MAX.LT.0) 
     .   DEFAULT_NQ = 1		!if true, dpu bins for nq to be used

      IF(UMMIN.LT.0.)  THEN
             MMIN = DMMIN
      ELSE
             MMIN = UMMIN
      ENDIF

      IF(UMMAX.LT.0.) THEN
              MMAX = DMMAX
      ELSE
              MMAX = UMMAX
      ENDIF

      IF(UMQMIN.LT.0.) THEN
              MQMIN = DMQMIN
      ELSE
              MQMIN = UMQMIN
      ENDIF

      IF(UMQMAX.LT.0.) THEN
              MQMAX = DMQMAX 
      ELSE
              MQMAX = UMQMAX
      ENDIF

      IF(UNM_MAX.LT.0) THEN
            NM_MAX = DNM_MAX
      ELSE
            NM_MAX = UNM_MAX +1   !(DPU formulas use overflow mass as nmmax)
      ENDIF

      IF(UNQ_MAX.LT.0) THEN
            NQ_MAX = DNQ_MAX
      ELSE
            NQ_MAX = UNQ_MAX
      ENDIF



C  ************************************************************
C  *              calculate binning parameters                *
C  *              M(lower bound of Nm) = Mmin * kM**(Nm-1)    *
C  *              M/Q(lower bound of Nq) = MQmin * kQ**(Nq-1) *
C  ************************************************************

      KM = ( MMAX/MMIN )**(1./(FLOAT(NM_MAX)-1.))   !substract 1 from
C                                                    nm_max (= overflow m)
      KQ = (MQMAX/MQMIN)**(1./FLOAT(NQ_MAX))


      IF (DEFAULT_NQ .EQ. 1) THEN
      	E(1) = -351.6997334
      	E(2) = 32.7867706
      	E(3) = 65.5735412
      ELSE
	E(1) = 1. + (  (ALOG(C1) - ALOG(MQMIN))/ALOG(KQ) )
        E(2) = 1. / ALOG(KQ)
        E(3) = 2. / ALOG(KQ)
        BREAK_C2 = IFIX ( ALOG(BREAK_MQ/MQMIN) / ALOG(KQ) )
      END IF

      IF (DEFAULT_NM .EQ. 1) THEN
        B(1) = 38.4605
        B(2) = -9.32689
        B(3) = -26.3092
        B(4) = 4.26283
        B(5) = 0.567205
        B(6) = 0.762824
      ELSE
        B(1) = 1. + (  (A(1)-ALOG(MMIN)) / ALOG(KM) )
        B(2) = A(2) / ALOG(KM)
        B(3) = A(3) / ALOG(KM)
        B(4) = A(4) / ALOG(KM)
        B(5) = A(5) / ALOG(KM)
        B(6) = A(6) / ALOG(KM)
      ENDIF

      RETURN
      END
