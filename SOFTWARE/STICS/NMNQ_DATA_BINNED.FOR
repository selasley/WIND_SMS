      SUBROUTINE NMNQ_DATA_BINNED(DPPSP,DPPSM,DVSTEP,ECHN,TCHN,
     .              UNQ_MAX,UMQMAX,UMQMIN,NQ,
     .		    UNM_MAX,UMMAX, UMMIN, NM)
C*******************************************************************
C     This SUBROUTINE calculates ln bins for MASS and MASS/CHARGE  *
C     given the number of bins requested by the user, plus         * 
C     the status of the +/- DPPS, DV Step number, the compressed   *
C     energy channel number,and the time-of-flight channel number  *
C     for WIND-STICS. The algorithms for m, m/q are those used by  *
C     the DPU  as of July 17, 1992.                                *
C     Original version 24 Jan 95 by ABG from a routine by SChotoo. *
C     History:
C                
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
      INTEGER*4  NM,NQ   		   !mass bin, m/q bin
      INTEGER*4  NM_MAX,  NQ_MAX, 	   !max bin values 
     .           UNM_MAX, UNQ_MAX 	   !USER max bin values (-1 = use default)

      REAL*4 MMAX,  MMIN,    !measurable mass range in amu 
     .       MQMAX, MQMIN,   !measurable m/q range in amu/e
     .       UMMAX, UMMIN,   !USER measurable mass range in amu (-1 = use default)
     .       UMQMAX,UMQMIN   !USER measurable m/q range in amu/e (-1 = use default)

      REAL*4  KM,KQ           !NM, NQ resolution parameters

   
      INTEGER*4 TCHN    !time channel number
      REAL*4 TEMP,         
     .       TMIN,TMAX, !measurable time-of-flight range in ns
     .       TOF,       !measured time-of-flight in ns
     .       XX,YY

      INTEGER*4 I,J,K
     
C   ***********************************************************************
C   *                    determine Nm, Nq coefficients                    *
C   ***********************************************************************

       CALL NMNQ_COEFF     (UNQ_MAX,UMQMAX,UMQMIN,    !user input
     .                      UNM_MAX,UMMAX, UMMIN,
     .                      EMIN,EMAX,TMIN,TMAX,      !return values
     .                      NQ_MAX,MQMAX,MQMIN,
     .                      NM_MAX,MMAX ,MMIN,
     .                      KM,KQ,
     .                      A,B,C1,C2A,C2B,D1,D2,E,
     .                      BREAK_C2, BREAK_MQ)



C  **************************************************************
C  *			PHA Channel number conversions	 	*
C  **************************************************************

C     Energy Channel Number (ECHN) must first be decompressed (ED) 
C     from 9-bit to 10-bit
      IF (ECHN .LT. 256) ED = ECHN
      IF ((ECHN .GE. 256).AND.(ECHN .LT. 384))ED = 2.0*ECHN - 256.0 + 0.5
      IF (ECHN .GE. 384) ED = 4.0*ECHN - 1024.0 + 1.5

C     Calculate measured energy in keV from decompressed channel number
      EMEAS = (ED + 6)/0.37654782

     
C     Calculate time of flight in ns from time channel number
      TOF = (TCHN - 44)/2.3725306895


C  ******************************************************************
C  *                   E/Q, M/Q, and M algorithms                   *
C  *                   Nm, Nq values                                *
C  ******************************************************************

C     Calculate the e/q value based on the default dpu algorithm and constants.
          IF(IAND(DPPSP, DPPSM).EQ.1)THEN               !both plates on
            EPQ = D1 * (D2**DVSTEP)
          ELSE IF (DPPSP.EQ.1 .OR. DPPSM.EQ.1) THEN     !one plate on  
            EPQ = 0.5 * D1 * (D2**DVSTEP)                
          ELSE                                          !neither plate on
            EPQ = 0
          ENDIF

C     Calculate mass per charge in amu/e
    
      MPQ = C1*(EPQ - C2A)*(TOF**2) 
      IF (MPQ .GE. BREAK_MQ) MPQ = C1*(EPQ - C2B)*(TOF**2)   

      IF (TOF .GT. 0) THEN
           NQ = E(1) + E(2)*ALOG(D1* (D2**DVSTEP) - C2A) +
     .          E(3)*ALOG(TOF)
           IF (NQ .GE. BREAK_C2)       
     .      NQ = E(1) + E(2)*ALOG(D1* (D2**DVSTEP) - C2B) +
     .     	 E(3)*ALOG(TOF)
           IF (NQ .GT. NQ_MAX) NQ = NQ_MAX+1	!M/Q overflow
           IF (NQ .LT. 1  ) NQ = 0	        !M/Q underflow
      ELSE
         NQ = 0
      ENDIF

C     Calculate mass in amu
      IF ((EMEAS .GT. EMIN) .AND. (EMEAS .LT. EMAX) .AND.                
     .    (TOF   .GT. TMIN) .AND. (TOF   .LT. TMAX) ) THEN

         XX = ALOG(EMEAS)
         YY = ALOG(TOF)
         TEMP = A(1) + A(2)*XX + A(3)*YY + A(4)*XX*YY
         TEMP = TEMP + A(5)*XX*XX + A(6)*YY*YY*YY
         M = EXP(TEMP)

         NM = B(1) + B(2)*XX + B(3)*YY + B(4)*XX*YY +
     .        B(5)*XX*XX + B(6)*YY*YY*YY

         IF (NM .GE. NM_MAX) THEN
              NM =  NM_MAX		!overflow mass class  (59)
	 ELSE IF (NM .LT. 1) THEN
              NM =  NM_MAX + 1		!underflow mass class (60)
	 ENDIF
      ELSE IF(EMEAS .LE. EMIN) THEN
	 M   = 0
         NM  = 0                       !mass zero event
      ELSE IF (EMEAS .GE. EMAX) THEN
	 M   = 0
         NM  = NM_MAX + 2	       !energy overflow (61)
      ELSE IF (TOF .LE. TMIN) THEN
         M   = 0 
	 NM  = NM_MAX + 3              !time underflow (62)
      ELSE IF (TOF .GE. TMAX) THEN
	 M   = 0
         NM  = NM_MAX + 4              !time overflow (63)
      END IF             


      RETURN
      END
