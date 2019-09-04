      SUBROUTINE MMPQ_DPU_WSTICS(DPPSP,DPPSM,DVSTEP,ECHN,TCHN,
     .                           MPQ,M,NM,NQ,DRANGE)
C*******************************************************************
C     This SUBROUTINE calculates the MASS and MASS/CHARGE given    * 
C     the status of the +/- DPPS, DV Step number, the compressed   *
C     energy channel number,and the time-of-flight channel number  *
C     for WIND-STICS. The algorithms are those used by the DPU     *
C     as of July 17, 1992.                                         *
C     Original version 9-Jan-95  K.Chotoo               	   *
C     History:                                                     *
C                13.Jan.95   add nm,nq,drange  (ABG)               *
C		 28.Sep.96   now TOF=0 will give MPQ = 0           *
c*******************************************************************
      IMPLICIT NONE

      REAL*4 A(6),	!mass coefficients
     .       B(6),      !NM coefficients
     .       C1,C2A,C2B	!mass/charge coefficients
C			 C1= time of flight **2 of 1 keV proton
C			 C2= carbon foil correction
      REAL*4 D1,D2,	!E/Q coefficients, D1= analyzer constant
C			                   D2= step resolution
     .       E(3)
      INTEGER*4 DPPSP,  !enable bit for dpps plus plate
     .          DPPSM,  !enable bit for dpps minus plate
     .          DVSTEP, !deflection voltage step number 
     .          ECHN    !compressed energy channel number
      REAL*4 ED,        !decompressed energy channel number            
     .       EMEAS,     !measured energy in keV
     .       EMIN,EMAX, !measurable energy range in keV 
     .       EPQ,       !E/Q
     .       M,         !mass in amu
     .       MPQ	!mass per charge in amu/e
      INTEGER*4 NM,NQ,		     	!mass bin, m/q bin
     .          NM_MAX, NQ_MAX, 	!max bin values
     .          DRANGE			!calculated range
      REAL*4 MMAX,MMIN,   !measurable mass range in amu
     .       MQMAX,MQMIN, !measurable m/q range in amu/e
     .       KM,KQ        !NM, NQ resolution parameters
   
      INTEGER*4 TCHN    !time channel number
      REAL*4 TEMP,         
     .       TMIN,TMAX, !measurable time-of-flight range in ns
     .       TOF,       !measured time-of-flight in ns
     .       XX,YY
     
C     Initialize constants with default dpu values (ref:  pages 345-349
C	of WIND SMS COMMAND AND DATA FORMATS FOR THE SMS INSTRUMENT R.3.0)
C					
      A(1) = 2.69575
      A(2) = -0.843766
      A(3) = -2.38009
      A(4) = 0.385641
      A(5) = 0.0513127
      A(6) = 0.0690096

      B(1) = 38.4605
      B(2) = -9.32689
      B(3) = -26.3092
      B(4) = 4.26283
      B(5) = 0.567205
      B(6) = 0.762824

      C1 = 1.9159E-05
      C2A= 1.5
      C2B= 2.5
      D1 = 6.190722
      D2 = 1.1225857

      E(1) = -351.6997334
      E(2) = 32.7867706
      E(3) = 65.5735412

      EMIN = 21.0
      EMAX = 2675.0
      TMIN = 11.0
      TMAX = 408.0
      MMIN  = 0.5
      MMAX  = 95.0
      MQMIN =  0.9
      MQMAX = 42.0 
      NM_MAX  = 59
      NQ_MAX  = 126

      KM = (MMAX/MMIN)**(1./FLOAT(NM_MAX))
      KQ = (MQMAX/MQMIN)**(1./FLOAT(NQ_MAX))

C     Energy Channel Number (ECHN) must first be decompressed (ED) 
C     from 9-bit to 10-bit
      IF (ECHN .LT. 256) ED = ECHN
      IF ((ECHN .GE. 256).AND.(ECHN .LT. 384))ED = 2.0*ECHN - 256.0 + 0.5
      IF (ECHN .GE. 384) ED = 4.0*ECHN - 1024.0 + 1.5


C     Calculate the e/q value based on the default dpu algorithm and constants.
          IF(IAND(DPPSP, DPPSM).EQ.1)THEN               !both plates on
            EPQ = D1 * (D2**DVSTEP)
          ELSE IF (DPPSP.EQ.1 .OR. DPPSM.EQ.1) THEN     !one plate on  
            EPQ = 0.5 * D1 * (D2**DVSTEP)                
          ELSE                                          !neither plate on
            EPQ = 0
          ENDIF
    
C     Calculate measured energy in keV from decompressed channel number
      EMEAS = (ED + 6)/0.37654782
     
C     Calculate time of flight in ns from time channel number
      TOF = (TCHN - 44)/2.3725306895


      IF (TOF .GT. 0) THEN
         MPQ = C1*(EPQ - C2A)*(TOF**2) 			  !Calculate mass per charge in amu/e
         IF (MPQ .GE. 11.0) MPQ = C1*(EPQ - C2B)*(TOF**2)   
         NQ = E(1) + E(2)*LOG(D1* (D2**DVSTEP) - C2A) +
     .     E(3)*LOG(TOF)
         IF (NQ .GE. 83)       
     .      NQ = E(1) + E(2)*LOG(D1* (D2**DVSTEP) - C2B) +
     .     	 E(3)*LOG(TOF)
         IF (NQ .GE. 126) NQ = 127	!M/Q overflow
         IF (NQ .LT. 0  ) NQ = 0	!M/Q underflow
      ELSE
	 MPQ = 0
         NQ = 0
      ENDIF

C     Calculate mass in amu
      IF ((EMEAS .GT. EMIN) .AND. (EMEAS .LT. EMAX) .AND.                
     .    (TOF   .GT. TMIN) .AND. (TOF   .LT. TMAX) ) THEN

         XX = LOG(EMEAS)
         YY = LOG(TOF)
         TEMP = A(1) + A(2)*XX + A(3)*YY + A(4)*XX*YY
         TEMP = TEMP + A(5)*XX*XX + A(6)*YY*YY*YY
         M = EXP(TEMP)

         NM = B(1) + B(2)*XX + B(3)*YY + B(4)*XX*YY +
     .        B(5)*XX*XX + B(6)*YY*YY*YY

         IF (M .GE. MMAX) THEN
              NM =  59
	 ELSE IF (M .LE. MMIN) THEN
              NM =  60
	 ENDIF
      ELSE IF(EMEAS .LE. EMIN) THEN
	 M   = 0
         NM  = 0
      ELSE IF (EMEAS .GE. EMAX) THEN
	 M   = 0
         NM  = 61
      ELSE IF (TOF .LE. TMIN) THEN
         M   = 0 
	 NM  = 62
      ELSE IF (TOF .GE. TMAX) THEN
	 M   = 0
         NM  = 63
      END IF             


C     Determine expected range value based on Nm and Nq values
      IF (  (NM.GE.33  .AND.  NM.LE.58)    .AND.
     .      (NQ.GE.11  .AND.  NQ.LE.82) )  THEN
	 DRANGE = 0
      ELSE IF ( (NM.GE.0  .AND. NM.LE.58 )  .AND.
     .          (NQ.GE.83 .AND. NQ.LE.126) ) THEN
	 DRANGE = 1
      ELSE IF (( (NM.GE.0  .AND. NM.LE.58)   .AND.
     .           (NQ.GE.1  .AND. NQ.LE.10) ) .OR.
     .         ( (NM.GE.0  .AND. NM.LE.32)   .AND.
     .           (NQ.GE.11 .AND. NQ.LE.82) )) THEN
         DRANGE = 2
      ELSE
	 DRANGE = -1    ! undefined
      ENDIF

      RETURN
      END
