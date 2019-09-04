     	SUBROUTINE WSTICSEFF(ENERGY, MASS,			!input 
     .					DCRR, TCRF, DCRF)	!output
C*****************************************************************************************
C
C	This subroutine returns TCR/FSR, DCR/FSR AND DCR/RSR efficiencies 
C	for a given mass.  It uses data from tables that were generated from
C       calibration data.
C	For masses not in data files the program interpolates between masses
C	available.
C
C	this is essentially PHAEFF but using different tables.  
C
C	CMSCEFF.FOR	created		17-NOV-1993
C
C       This version is modified for WIND-STICS:
C		WSTICSEFF.FOR 	created 27-JUL-1995	by	K. CHOTOO
C
C	Revision History:
C	 1-AUG-1995	K. CHOTOO	Added Fe data file from interpolation
C					between Ar and Kr. All data files from
C					C - Kr have the same E/nuc values.
C
C     	25-AUG-1995  	ABG 		No Ne_95.eff file available, so
C					interopolate from Ar and O
C
C	 7-FEB-1995  	K.CHOTOO	Ne_95.eff file now avalible. Same as C,N,O_95.eff
C
C	17-APR-1995	K.CHOTOO	Replaced FRS/Io with DCR/RSR in this routine and
C					in efficiency data files
C
C=========================================================================================
C
C  Input:
C
C	Energy 		Real		the total energy of the particle in 
C					keV upon entering the detector
C
C	Mass		Real		the mass of the particle in amu
C
C  Output:
C
C	DCRR		Real		the dcr/rsr START efficiency for that mass
C					and energy
C
C	TCRF		Real		the tcr/fsr STOP efficiency for that mass
C					and energy
C
C	DCRF		Real		the dcr/fsr STOP efficiency for that mass
C					and energy
C
C***************************************************************************
C
C			DECLARATIONS
C			************
C
     	IMPLICIT NONE
C
C			input
     	REAL
     .	  ENERGY,
     .	  MASS
C
C			data available
      	REAL
     .	  M(9)/1.000,3.971,11.916,13.896,15.872,20.019,39.631,55.404,83.135/,
C				aval data mass -  H,He,C,N,O,Ne,Ar,Kr
C				have Ne data but not used in interpolation
     .	  TCRF,						!tcr/fsr
     .	  DCRF,						!dcr/fsr
     .	  DCRR						!dcr/rsr
C
C
     	CHARACTER*5
     .	  HEADER					!text in data files
C
     	CHARACTER*9
     .	  DATFILE(9)/'H_95.EFF','HE_95.EFF','C_95.EFF','N_95.EFF',
     .		   'O_95.EFF','NE_95.EFF','AR_95.EFF','FE_95.EFF','KR_95.EFF'/
C							!data files
C
     	CHARACTER*28
     .	  DIR/'SMS1:[CHOTOO.WIND_STICS_EFF]'/	!location of files
C
C			possible masses
     	REAL
     .	  REALM,					!real ion mass
     .	  ACTLM(13)/1.000,3.971,11.916,13.896,15.872,20.019,	!possible ions
     .		22.808,24.112,27.863,31.810,39.631,55.404,83.135/,
C			!H,HE,C,N,O,NE,NA,MG,SI,S,AR,FE,KR
     .	  DELTA,					!input mass - real mass
     .	  MIN						!minimum difference
C
C			interpolation
     	LOGICAL
     .	  INTERP					!must interpolate
C
C			interpolation by energy
     	REAL
     .	  EPM,
     .	  DATA(4),					!file data values
     .	  ELO, EHI,					!energy values
     .	  DLO, DHI,					!eff values
     .	  TLO, THI,					!eff values
     .	  FLO, FHI,					!eff values
     .	  DEFF,TEFF,FEFF				!interpolated value
C
C			interpolation by mass
     	REAL
     .	  DATA1(4),					!2nd file data
     .	  E1LO, E1HI,					!energy values
     .	  D1LO, D1HI,					!eff values
     .	  T1LO, T1HI,					!eff values
     .	  F1LO, F1HI,					!eff values
     .	  DEFF1,TEFF1,FEFF1,				!interpolated value
     .	  M1, M2					!interp. masses
C
C
     	INTEGER
     .	  I,J
C
C***************************************************************************
C
C			STATEMENTS
C			**********
C					initialization
C
     	INTERP = .FALSE.
     	MIN = 100
C						find the real mass for this ion
C						by finding the real ion mass
C						it is closest to
     	DO I = 1, 13
     	  DELTA = ABS(MASS - ACTLM(I))
     	  IF (DELTA.LT.MIN) THEN
     	    MIN = DELTA
     	    REALM = ACTLM(I)
     	  END IF
     	END DO
C
     	EPM = ENERGY/REALM
C						find if interpolation is
C						necessary or if we have this
C						ion.  open approp. files
     	DO I = 1, 8
     	  IF ((REALM.LT.M(I+1)) .AND. (REALM.GT.M(I))) THEN
     	    OPEN (UNIT=1, FILE=DIR//DATFILE(I), STATUS='OLD', READONLY)
     	    REWIND 1
     	    M1 = M(I)
     	    OPEN (UNIT=5, FILE=DIR//DATFILE(I+1), STATUS='OLD', READONLY)
     	    REWIND 5
     	    M2 = M(I+1)
     	    INTERP = .TRUE.
     	  ELSE IF (REALM.EQ.M(I)) THEN		!have this ion
     	    OPEN (UNIT=1, FILE=DIR//DATFILE(I), STATUS='OLD',READONLY)
     	    REWIND 1
     	  END IF
     	END DO
     	IF (REALM.EQ.M(9)) THEN
     	  OPEN (UNIT=1, FILE=DIR//DATFILE(8), STATUS='OLD', READONLY)
     	  REWIND 1
     	END IF
     	IF (REALM.GT.M(9)) THEN			!shouldn't happen
     	  DCRR = 1.0
     	  TCRF = 1.0
     	  DCRF = 1.0
     	  RETURN
     	END IF
C
 555   	IF (INTERP) THEN			!interpolation necessary
     	  DO I = 1, 2
     	    READ(1,'(A)') HEADER		!skip headers
      	    READ(5,'(A)') HEADER
     	  END DO
C						first file - low mass
     	  ELO = 10000
     	  DO WHILE (EPM.LT.ELO)		!finding energy rng - E is
     	    EHI = ELO				!decreasing in file
     	    DHI = DLO
     	    THI = TLO
     	    FHI = FLO
     	    READ(1,*,END=5555) (DATA(I), I=1,4)	!low ion data
     	    ELO = DATA(1)
     	    DLO = DATA(2)
     	    TLO = DATA(3)
     	    FLO = DATA(4)
     	  END DO
C						energy interpolation
     	  DEFF = ((DHI-DLO)/(EHI-ELO))*(EPM-ELO) + DLO
     	  TEFF = ((THI-TLO)/(EHI-ELO))*(EPM-ELO) + TLO
     	  FEFF = ((FHI-FLO)/(EHI-ELO))*(EPM-ELO) + FLO


C
C						second file - high mass
 1111	  E1LO = 10000
 	  DO WHILE (EPM.LT.E1LO)		!finding energy rng
     	    E1HI = E1LO
     	    D1HI = D1LO
     	    T1HI = T1LO
     	    F1HI = F1LO
     	    READ(5,*,END=6666) (DATA1(I), I=1,4)	!high ion data
     	    E1LO = DATA1(1)
     	    D1LO = DATA1(2)
     	    T1LO = DATA1(3)
     	    F1LO = DATA1(4)
     	  END DO
C						energy interpolation
     	  DEFF1 = ((D1HI-D1LO)/(E1HI-E1LO))*(EPM-E1LO) + D1LO
     	  TEFF1 = ((T1HI-T1LO)/(E1HI-E1LO))*(EPM-E1LO) + T1LO
     	  FEFF1 = ((F1HI-F1LO)/(E1HI-E1LO))*(EPM-E1LO) + F1LO

C
C						mass interpolation
 2222  	  DCRF = ((DEFF1-DEFF)/(M2-M1)) * (REALM-M1) + DEFF
 	  TCRF = ((TEFF1-TEFF)/(M2-M1)) * (REALM-M1) + TEFF
 	  DCRR = ((FEFF1-FEFF)/(M2-M1)) * (REALM-M1) + FEFF
     	ELSE					!have this ion
     	  DO I = 1, 2
     	    READ(1,'(A)') HEADER		!skip headers
     	  END DO
     	  ELO = 10000
     	  DO WHILE (EPM.LT.ELO)
     	    EHI = ELO
     	    DHI = DLO
     	    THI = TLO
     	    FHI = FLO
     	    READ(1,*,END=8888) (DATA(I), I=1,4)
     	    ELO = DATA(1)
     	    DLO = DATA(2)
     	    TLO = DATA(3)
     	    FLO = DATA(4)
     	  END DO
     	  DCRF = ((DHI-DLO)/(EHI-ELO))*(EPM-ELO) + DLO
     	  TCRF = ((THI-TLO)/(EHI-ELO))*(EPM-ELO) + TLO
     	  DCRR = ((FHI-FLO)/(EHI-ELO))*(EPM-ELO) + FLO
     	END IF     					!interp
C
 3333	REWIND 1
     	IF (INTERP) REWIND 5
C
     	CLOSE(UNIT=1)
     	IF (INTERP) CLOSE(UNIT=5)
C
     	RETURN
C
 5555   DEFF = DLO				!at end of first file
     	TEFF = TLO				!at end of first file
     	FEFF = FLO				!at end of first file
     	GO TO 1111
C 	
 6666   DEFF1 = D1LO				!at end of second file
     	TEFF1 = T1LO				!at end of second file
     	FEFF1 = F1LO				!at end of second file
     	GO TO 2222
 8888   DCRF = DLO				!at end of only file
     	TCRF = TLO				!at end of only file
     	DCRR = FLO				!at end of only file
     	GO TO 3333
     	END
