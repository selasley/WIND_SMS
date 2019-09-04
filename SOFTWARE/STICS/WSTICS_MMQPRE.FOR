      PROGRAM MMQPRE_wstics
C** 
C     	Version 16 March 1992
C     	Author A.B. Galvin (derived from mqpre programs)
C**     PROGRAM TO COMPUTE MEASURED ENERGY AND TOF 
C**     FOR WIND STICS INSTRUMENT
C**     USING ALGORITHMS FOR PREDICTING 
C**     MASS  AND  MASS PER CHARGE
C** 
C       Part 1:  exact values of E,T for "test" ions, in
C                order to check out the DPU classification.
C******************************************************** 
C
       IMPLICIT NONE
C
       REAL A(6)        !mass coefficients
       
       REAL C1,C2A,C2B  !mass/charge coefficients 
C                        C1= time of flight **2 of a 1 KeV proton
C                        C2 = carbon foil correction
       REAL CHG(50)     !charge of output ions

       REAL D1,D2       !E/Q coefficients, D1= analyzer constant, 
C                                          D2=step resolution
       CHARACTER DAY*9, HOUR*8
       INTEGER DFECT_TYPE	!which SSD DEFECT function to use
       REAL DIST	!time of flight distance
       REAL DPU_C1,DPU_C2A,DPU_C2B,      ! analyzer constant used by DPU
     .      DPU_D1,DPU_D2
       REAL DPUDIST     ! nominal time of flight distance used in dpu algorithm
       REAL DPUTHICK	! carbon foil thickness assumed to generate algorithm
       INTEGER DVSTEP

       REAL E
       REAL ECF,ECFN  !energy after carbon foil, keV and keV/nuc
       REAL EI          !incident energy = E/Q*Q
       REAL EMEAS       !measured energy
       REAL EMIN,EMAX   !measurable energy range
       REAL ENUC        !energy per nucleon = E/Q* Q/M
       REAL EPQ,EQ	!energy per charge 
       REAL EPQ_DPU	!DPU predicted e/q based on voltage step

       REAL GENERAL_MASS(20),
     .      GENERAL_CHARGE(20)
       CHARACTER*4 GENERAL_SPECIES(20)

       INTEGER I, J, K	!do indices
       REAL INVEL      !incident velocity
       CHARACTER*2 ILAB
       CHARACTER*3 ILABQ
       CHARACTER*10 INSTRUMENT
       INTEGER IOK
       CHARACTER*4 IONS(50)

       INTEGER LU2	!output unit number

       REAL MASS(50)	!mass of output species
       INTEGER MCOEF,   !whether to input mass coefficients manually
     .         MQCOEF   !                 m/q
       REAL MPRED, MQPRED !predicted mass,m/q based on dpu algorithm
       REAL MPQ         !mass per charge
       REAL MMAX,MQMAX	!maximum allowable Mass, Mass per charge value
       REAL MMIN,MQMIN  !minimum  "

       INTEGER N
       INTEGER NIONS    !number of ions in printout (used in do statement)

       CHARACTER*5 RATE(50)
       REAL RNM		! mass bin
       REAL RNQ         ! mass/charge bin
       REAL RMS			!root mean square of something
       REAL R1,R2       ! ratios of actual vs. predicted mass and m/q

       REAL T
       REAL T1, T2, T3, T4, T5, T6, T7
       REAL TEMP
       REAL THICK	!Actual carbon foil thickness, in ug/cm2
       REAL TMIN, TMAX	!min and max time measureable
       REAL TOFPRE      !predicted time of flight 
       REAL TOFVEL      !velocity in time-of-flight chamber, 
C                        i.e., after carbon foil
C
C
       REAL XX, YY
C
C	functions called
C
       REAL CFOIL	!Energy deposited in foil
       REAL SWEFF	!Nuclear defect in detectors (Swics)
       REAL CHEMDFECT   !	"		     (AMPTE)
       REAL DFECT	!       "		     (General = ISEE)
C
C
C
      DATA GENERAL_MASS / 1.,1.99,2.98,3.97,11.92,13.9,15.87,20.02,
     $  24.12, 27.86, 31.81, 39.63, 55.41, 83.14, 6*0. /
C
      DATA GENERAL_CHARGE / 1.,2.,3.,4.,5., 6.,7.,8.,9.,10.,11.,12.,
     $  13.,14.,15.,16.,17.,18.,19.,20. /
C
      DATA GENERAL_SPECIES/     '  H ','  D ','  He','  He',
     x  '  C ','  N ','  O ','  Ne', 
     x  '  Mg','  Si','  S ','  A ','  Fe','  Kr', 6*'    '/
C 
C	Open output file wstics_MMQPRE.PRT as unit 35
C
      OPEN (UNIT=35, FILE='wstics_MMQPRE.PRT', STATUS='NEW', RECL=132)
      LU2 = 35
C
C     initial values for instrument / dpu parameters
      DPUTHICK = 3.0
      THICK = DPUTHICK
      INSTRUMENT = 'WIND/STICS'
      DPUDIST=10.0 
      DIST = DPUDIST
      DFECT_TYPE = 1
      MCOEF = 1
      MQCOEF = 1
      EMIN = 15.
      EMAX = 3030.
      TMIN = 10.
      TMAX = 405.
      MQMIN = 0.9
      MQMAX = 42.0
      MMAX  = 95.0
      MMIN  = 0.5
      DVSTEP = 20
      EPQ = 63.5
C
C     initialize ion arrays
      DO J=1,50
	IONS(J) = '    '
	CHG(J) = 0.
	MASS(J) = 0.
	RATE(J) = '     '
      END DO

C     matrix rates -  "exact" mass, m/q values

      MASS(1) = GENERAL_MASS(4)			!He+2 -- default for HMR
      CHG (1) = GENERAL_CHARGE(2)
      IONS(1) = GENERAL_SPECIES(4)
      RATE(1) = 'HMR  '

      MASS(2) = GENERAL_MASS(1)			!H+  -- default for SMR
      CHG (2) = GENERAL_CHARGE(1)
      IONS(2) = GENERAL_SPECIES(1)
      RATE(2) = 'SMR  '

      MASS(3) = GENERAL_MASS(4)			!He+  -- default for OMR0
      CHG (3) = GENERAL_CHARGE(1)
      IONS(3) = GENERAL_SPECIES(4)
      RATE(3) = 'OMR0 '

      MASS(4) = GENERAL_MASS(5)			!C+6 -- default for OMR1
      CHG (4) = GENERAL_CHARGE(6)
      IONS(4) = GENERAL_SPECIES(5)
      RATE(4) = 'OMR1 '

      MASS(5) = GENERAL_MASS(5)			!C+5 -- default for OMR2
      CHG (5) = GENERAL_CHARGE(5)
      IONS(5) = GENERAL_SPECIES(5)
      RATE(5) = 'OMR2 '

      MASS(6) = GENERAL_MASS(5)			!C+4 -- default for OMR3
      CHG (6) = GENERAL_CHARGE(4)
      IONS(6) = GENERAL_SPECIES(5)
      RATE(6) = 'OMR3 '

      MASS(7) = GENERAL_MASS(7)			!O+7 -- default for OMR4
      CHG (7) = GENERAL_CHARGE(7)
      IONS(7) = GENERAL_SPECIES(7)
      RATE(7) = 'OMR4 '

      MASS(8) = GENERAL_MASS(7)			!O+6 -- default for OMR5
      CHG (8) = GENERAL_CHARGE(6)
      IONS(8) = GENERAL_SPECIES(7)
      RATE(8) = 'OMR5 '

      MASS(9) = GENERAL_MASS(7)			!O+1 -- default for OMR6
      CHG (9) = GENERAL_CHARGE(1)
      IONS(9) = GENERAL_SPECIES(7)
      RATE(9) = 'OMR6 '

      MASS(10) = GENERAL_MASS(8)		!Ne+8 -- default for OMR7
      CHG (10) = GENERAL_CHARGE(8)
      IONS(10) = GENERAL_SPECIES(8)
      RATE(10) = 'OMR7 '

      MASS(11) = GENERAL_MASS(9)		!Mg+10 -- default for OMR8
      CHG (11) = GENERAL_CHARGE(10)
      IONS(11) = GENERAL_SPECIES(9)
      RATE(11) = 'OMR8 '

      MASS(12) = GENERAL_MASS(9)		!Mg+8 -- default for OMR9
      CHG (12) = GENERAL_CHARGE(8)
      IONS(12) = GENERAL_SPECIES(9)
      RATE(12) = 'OMR9 '

      MASS(13) = GENERAL_MASS(10)		!Si+12 -- default for OMR10
      CHG (13) = GENERAL_CHARGE(12)
      IONS(13) = GENERAL_SPECIES(10)
      RATE(13) = 'OMR10'

      MASS(14) = GENERAL_MASS(10)		!Si+9 -- default for OMR11
      CHG (14) = GENERAL_CHARGE(9)
      IONS(14) = GENERAL_SPECIES(10)
      RATE(14) = 'OMR11'

      MASS(15) = GENERAL_MASS(10)		!Si+8 -- default for OMR12
      CHG (15) = GENERAL_CHARGE(8)
      IONS(15) = GENERAL_SPECIES(10)
      RATE(15) = 'OMR12'

      MASS(16) = GENERAL_MASS(13)		!Fe+16 -- default for OMR13
      CHG (16) = GENERAL_CHARGE(16)
      IONS(16) = GENERAL_SPECIES(13)
      RATE(16) = 'OMR13'

      MASS(17) = GENERAL_MASS(13)		!Fe+14 -- default for OMR14
      CHG (17) = GENERAL_CHARGE(14)
      IONS(17) = GENERAL_SPECIES(13)
      RATE(17) = 'OMR14'

      MASS(18) = GENERAL_MASS(13)		!Fe+12 -- default for OMR15
      CHG (18) = GENERAL_CHARGE(12)
      IONS(18) = GENERAL_SPECIES(13)
      RATE(18) = 'OMR15'

      MASS(19) = GENERAL_MASS(13)		!Fe+11 -- default for OMR16
      CHG (19) = GENERAL_CHARGE(11)
      IONS(19) = GENERAL_SPECIES(13)
      RATE(19) = 'OMR16'

      MASS(20) = GENERAL_MASS(13)		!Fe+10 -- default for OMR17
      CHG (20) = GENERAL_CHARGE(10)
      IONS(20) = GENERAL_SPECIES(13)
      RATE(20) = 'OMR17'

      MASS(21) = GENERAL_MASS(13)		!Fe+9 -- default for OMR18
      CHG (21) = GENERAL_CHARGE(9)
      IONS(21) = GENERAL_SPECIES(13)
      RATE(21) = 'OMR18'

      MASS(22) = GENERAL_MASS(13)		!Fe+8 -- default for OMR19
      CHG (22) = GENERAL_CHARGE(8)
      IONS(22) = GENERAL_SPECIES(13)
      RATE(22) = 'OMR19'

      MASS(23) = 9.04				!Basic rate 0, lower left
      CHG (23) = 9.04 / 1.22
      IONS(23) = '  ll'
      RATE(23) = 'BR0  '

      MASS(24) = 95.0				!Basic rate 0, upper right
      CHG (24) = 95.0 / 10.98
      IONS(24) = '  ur'
      RATE(24) = 'BR0  '


      MASS(25) = 29.305				!Basic rate 0, LOG middle
      CHG (25) = 29.305/ 3.64497
      IONS(25) = '  md'
      RATE(25) = 'BR0  '

      MASS(26) = 0.5				!Basic rate 1, lower left
      CHG (26) = 0.5/10.98
      IONS(26) = '  ll'
      RATE(26) = 'BR1  '

      MASS(27) = 95.0				!Basic rate 1, upper right
      CHG (27) = 95.0/42.0
      IONS(27) = '  ur'
      RATE(27) = 'BR1  '

      MASS(28) = 6.892024			!Basic rate 1, LOG middle
      CHG (28) = 6.892024/21.4746  
      IONS(28) = '  md'
      RATE(28) = 'BR1 '

      MASS(29) = 0.5				!Basic rate 2, lower left
      CHG (29) = 0.5/0.9
      IONS(29) = '  ll'
      RATE(29) = 'BR2  '

      MASS(30) = 9.04				!Basic rate 2, upper right
      CHG (30) = 9.04/10.89
      IONS(30) = '  ur'
      RATE(30) = 'BR2  '

      MASS(31) = 2.126				!Basic rate 2, LOG middle
      CHG (31) = 2.126 / 3.1307
      IONS(31) = '  md'
      RATE(31) = 'BR2  '

      MASS(32) = GENERAL_MASS(5)			!C+1, beam ion
      CHG (32) = GENERAL_CHARGE(1)
      IONS(32) = GENERAL_SPECIES(5)
      RATE(32) = 'beam '

      MASS(33) = GENERAL_MASS(6)			!N+1, beam ion
      CHG (33) = GENERAL_CHARGE(1)
      IONS(33) = GENERAL_SPECIES(6)
      RATE(33) = 'beam '

      MASS(34) = GENERAL_MASS(7)			!O+1, beam ion
      CHG (34) = GENERAL_CHARGE(1)
      IONS(34) = GENERAL_SPECIES(7)
      RATE(34) = 'beam '

      MASS(35) = GENERAL_MASS(8)			!Ne+1, beam ion
      CHG (35) = GENERAL_CHARGE(1)
      IONS(35) = GENERAL_SPECIES(8)
      RATE(35) = 'beam '

      MASS(36) = GENERAL_MASS(11)			!S+1, beam ion
      CHG (36) = GENERAL_CHARGE(1)
      IONS(36) = GENERAL_SPECIES(11)
      RATE(36) = 'beam '


      MASS(37) = GENERAL_MASS(12)			!A+1, beam ion
      CHG (37) = GENERAL_CHARGE(1)
      IONS(37) = GENERAL_SPECIES(12)
      RATE(37) = 'beam '

      MASS(38) = GENERAL_MASS(13)			!Fe+1, beam ion
      CHG (38) = GENERAL_CHARGE(1)
      IONS(38) = GENERAL_SPECIES(13)
      RATE(38) = 'beam '

      MASS(39) = GENERAL_MASS(14)			!Kr+1, beam ion
      CHG (39) = GENERAL_CHARGE(1)
      IONS(39) = GENERAL_SPECIES(14)
      RATE(39) = 'beam '

      NIONS = 39

C     Dpu values for e/q algorithm as of 3/16/92
C** 
C*  kev/nuc = (100d)**2 / (4.38T(ns))**2
C*  M/Q = (E/Q) / (E/M)  =  C1 * (E/Q - C2cfcorrection) * TOF**2 
C*  C1 = (4.3771E-2/DIST)**2
	     DPU_C1 = 1.9159E-05
             DPU_C2A = 1.5
             DPU_C2B = 4.0
             DPU_D1 = 6.2996
	     DPU_D2 = 1.1225
C 
C
4000  WRITE(*,400) DPUTHICK
400   FORMAT (/,' Nominal carbon foil thickness is ', F4.1,' ug/cm2',/
     X        ' Enter Carbon Foil Thickness   ', $) 
      CALL TI REAL (THICK, *4000)
C
4100   WRITE(*,410) DPUDIST
410    FORMAT (/,' Nominal TOF distance is ',F5.1,' cm ',/
     X			  ' Enter Time-of Flight distance   ', $) 
       CALL TI REAL (DIST, *4100)

4110   WRITE(*,411) EMIN
411    FORMAT (/,' Nominal minimum measured Energy is ',F7.2,
     X	         ' keV.  Enter EMIN.  ', $) 
       CALL TI REAL (EMIN, *4110)

4120   WRITE(*,412) EMAX
412    FORMAT (/,' Nominal maximum measured Energy is ',F7.2,
     X	         ' keV.  Enter EMAX.  ', $) 
       CALL TI REAL (EMAX, *4120)

4130   WRITE(*,413) TMIN
413    FORMAT (/,' Nominal minimum measured TOF is ',F7.2,
     X	         ' nsec.  Enter TMIN.  ', $) 
       CALL TI REAL (TMIN, *4130)

4140   WRITE(*,414) TMAX
414    FORMAT (/,' Nominal maximum measured TOF is ',F7.2,
     X	         ' nsec.  Enter TMAX.  ', $) 
       CALL TI REAL (TMAX, *4140)

4160   WRITE(*,416)EPQ
416    FORMAT(/, ' Nominal E/Q = ',F7.2,' Enter E/Q: ',$)
       CALL TI REAL (EPQ,*4160)

4170   WRITE(*,417) DVSTEP
417    FORMAT(/, ' DVSTEP = ',I3,' Enter DV step number: ',$)
       CALL TI INT (DVSTEP,*4170)
C
4200   WRITE (*, 420) DFECT_TYPE
420    FORMAT (/,' Nominal pulse height defect function is type ',I2,/
     x          ,'  Which DFECT would you like to use:',/
     x         '      1 - CHEM    ',/
     x         '      2 - ULS SWICS    ',/
     x         '      3 - ISEE      ',$)
       CALL TI INT (DFECT_TYPE, *4200)

4300   WRITE(*,430)
430    FORMAT(//,'  Mass/Charge coefficients: '/
     x           '  M/Q = C1 * [ E/Q - C2a,b ] * T**2 '/
     x           '  E/Q = D1 * D2**S     '//
     x  ' Use default WIND/STICS mass/charge coefficients (1)',/,
     x         ' Enter your own coefficients (2)  ',$) 
       CALL TI INT(MQCOEF, *4300)
C
      IF (MQCOEF .EQ. 2)  THEN		!enter your own m/q coeffiecients
4400         WRITE(*,440)
440  	     FORMAT(' Enter C1, C2A, C2B, D1, D2  ', $)
             READ(*,*) C1, C2A, C2B, D1, D2
       ELSE IF (MQCOEF .EQ. 1) THEN	!default dpu values 
	     C1  = DPU_C1 
             C2A = DPU_C2A
             C2B = DPU_C2B
             D1  = DPU_D1 
             D2  = DPU_D2 
       ENDIF
C*
C*  Log(M) = A1 + A2*Log(E) + A3*Log(T) + A4*Log(E)*Log(T)
C*              + A5*[Log(E)]**2 + A6*[Log(T)]**3 
C*
C**  Set up the MASS Coefficients here 
C
C 
4600   WRITE(*,460)
460    FORMAT(/,' Mass coefficients: '/
     x         '  Log(M) = A1 + A2*Log(E) + A3*Log(T) '
     x          '+ A4*Log(E)*Log(T)'/
     x          '          + A5*[Log(E)]**2 + A6*[Log(T)]**3 '//
     x         ' Use default WIND/STICS mass coefficients (1)',/,
     x         ' Enter your own coefficients (2)  ',$)
       CALL TI INT(MCOEF, *4600)
C
      IF (MCOEF .EQ. 2)  THEN		!enter your own mass coeffiecients
             WRITE(*,470)
470  	     FORMAT(' Enter A1, A2, A3, A4, A5, A6   ', $)
             READ(*,*) A 
       ELSE IF (MCOEF .EQ. 1) THEN	!default dpu values as of 3/16/92
       		A(1) = 2.69575
       		A(2) = -0.843766
       		A(3) = -2.38009
       		A(4) = 0.385641
       		A(5) = 0.0513127
       		A(6) = 0.0690096
       ENDIF
C 
C 
      WRITE(LU2,479) DIST,THICK,EMIN,TMIN,EMAX,TMAX
479   FORMAT(1H1,////,10X,
     $  'MASS AND MASS/CHARGE PREDICTIONS FOR WIND '
     $  'STICS PULSER INPUTS',
     $      //'          Part 1:  "EXACT" VALUES FOR TEST IONS',
     $  ///,'  INPUT PARAMETERS: ',//
     $ '  TOF Distance =',F5.1,5X,'   CF Thickness =',F6.2,
     $      ' ugm/cm**2',/
     $ '  EMIN = 'F8.2' keV        TMIN = 'F7.2' nsec'/
     $ '  EMAX = 'F8.2' keV        TMAX = 'F7.2' nsec'/)
C
       IF (DFECT_TYPE .EQ. 1) THEN
       		WRITE (LU2, 9390)
 9390           FORMAT (//,'  For PH Defect:  CHEMDFECT used')
       ELSE IF (DFECT_TYPE .EQ. 2) THEN
       		WRITE (LU2, 9391)
 9391		FORMAT (//,'  For PH Defect:  SWEFF used')
       ELSE
       		WRITE (LU2, 9392)
 9392		FORMAT (//,'  For PH Defect:  DFECT used')
       ENDIF
       WRITE(LU2,480)EPQ,D1,D2,DVSTEP,C2A,C2B,C1
480    FORMAT(/,'  ALGORITHMS: '//,
     $ '  DPPS E/Q = ',F7.4,/,
     $ '  DPU  E/Q = ',F7.4,' * (',F8.6,')**'I3,//,
     $ '  DPU  M/Q = C1 * [ (E/Q) - ',F5.2,
     $               ' ]  *  (TOF)**2 , for m/q < 11.0'/, 
     $ '  DPU  M/Q = C1 * [ (E/Q) - ',F5.2,
     $               ' ]  *  (TOF)**2 , for m/q > 11.0'/
     $ '     where   C1 = ',1PE11.4//)

       IF (MQCOEF .EQ. 2) THEN
       		WRITE (LU2, 9295) 
 9295		FORMAT ('  (M/Q coefficients entered manually)')
       ELSE IF (MQCOEF .EQ. 1) THEN
       		WRITE (LU2, 9296)
 9296		FORMAT ('  (Default DPU M/Q coefficients used)')
       ENDIF
C
      WRITE(LU2,481)
481   FORMAT(/'  DPU Log(M) = A1 + A2*Log(E)'
     $          ' + A3*Log(T) + A4*Log(E)*Log(T) + '
     $          'A5*[Log(E)]**2 + A6*[Log(T)]**3 '/25X,
     $          ', where T=measured TOF, E=measured Energy'//) 
C
      WRITE(LU2,482)  (I,A(I), I = 1, 6)
482   FORMAT(5X,'A',I1,' =',G14.7/) 
       IF (MCOEF .EQ. 2) THEN
       		WRITE (LU2, 9292) 
 9292		FORMAT ('  (Mass coefficients entered manually)')
       ELSE IF (MCOEF .EQ. 1) THEN
       		WRITE (LU2, 9294)
 9294		FORMAT ('  (Default DPU mass coefficients used)')
       ENDIF
C** *****************************************************************
C                  START ION CALCULATIONS HERE 
C** *****************************************************************
C
          J = DVSTEP
	  EPQ_DPU = D1 * (D2**J)
       	  WRITE (LU2, 405) J,EPQ_DPU,EPQ
 405      FORMAT(1H1,40X,'WIND STICS PULSER CALIBRATION TEST INPUTS'/
     $      17X,'DPPS VOLTAGE STEP = ',I3,'    DPU algorithm E/Q =',
     $                     F7.3,' keV/e     Actual E/Q =',F7.3,
     $                     ' keV/e '/)
C 
             WRITE(LU2,415)
415          FORMAT(73X,'  Volt',1X,' **  Predicted  **',2X,
     $              ' * RATIO *'/
     $       ' Rate  Ion   Mass   Q    M/Q    E/Q    E/M   Einc ',
     $              '  E(cf)   E(meas)  TOF',3X,'step',2X,'E/Q ',
     $              2X,'Mass    M/Q '2X,'Mass',3X,'M/Q',3X,'  NM  ',
     $              6X,'NQ '/46X'keV    keV      keV    nsec'/) 
C
             IOK=0 
             
C
         DO I=1,NIONS 
	 
		ENUC = EPQ * CHG(I) / MASS(I)
                EI   = EPQ * CHG(I)
                EQ   = EPQ
                MPQ = MASS(I)/CHG(I)
	        INVEL= 437.71* SQRT( ENUC )

C               energy after carbon foil
                ECF=EI-THICK*CFOIL(ENUC,MASS(I))

                IF (ECF .LE. 0.)   ECF = 0.1 
                ECFN=ECF/MASS(I) 
                TOFVEL=4.3771E7   *  SQRT(ECFN) 

                TOFPRE=DIST/TOFVEL*1.E9
       		IF (DFECT_TYPE .EQ. 1) THEN
       		   EMEAS = ECF * CHEMDFECT(ECFN,MASS(I))
       		ELSE IF (DFECT_TYPE .EQ. 2) THEN
                    EMEAS=ECF*SWEFF(ECFN,MASS(I))
                ELSE
       		    EMEAS = ECF * DFECT(ECFN,MASS(I))
       	        ENDIF

                IF (EMEAS .LT. 0.) EMEAS = 0.0
C**** 
       		IF (EMEAS.GT.0. .AND. TOFPRE.GT.0.) THEN 
C						!calculate predicted mass
       		    XX = ALOG(EMEAS)
                    YY = ALOG(TOFPRE)
                    TEMP = A(1)  +  A(2) * XX  +  A(3) * YY 
                    TEMP = TEMP  +  A(4) * XX * YY
                    TEMP = TEMP  +  A(5) * XX *XX 
                    TEMP = TEMP  +  A(6) * YY * YY * YY 
                    MPRED = EXP(TEMP)
		ELSE
		    MPRED = 0.
		ENDIF

C		mass = MMIN * (KM)**(NMlow-1)
	        IF (MPRED .GT. 0.) THEN
		    RNM = ( ALOG(MPRED/0.5) / ALOG(1.09468) ) + 1.0
                ENDIF
		
		IF (MPRED .LE. MMIN) RNM = 60.
		IF (MPRED .GE. MMAX) RNM = 59.
		IF (TOFPRE .LE. TMIN) RNM = 62.
		IF (TOFPRE .GE. TMAX) RNM = 63.
                IF (EMEAS .LE. EMIN) RNM = 0.	!mass zero event
		IF (EMEAS. GE. EMAX) RNM = 61.

C               calculate predicted mass/charge
		    MQPRED = C1 * (EPQ_DPU - C2A) * (TOFPRE**2)
		    IF (MQPRED .LT. 0.) MQPRED = 0.
		    IF (MQPRED .GE. 11.0) MQPRED = C1 * 
     $                         (EPQ_DPU - C2B) * (TOFPRE**2)

C               m/q = MQMIN * KQ**(NQlow-1)
		IF (MQPRED .GT. 0.) THEN
                    RNQ = ( ALOG(MQPRED/0.90) / ALOG(1.03097) ) + 1.0
                ENDIF
                IF (MQPRED .GE. MQMAX) RNQ =  127.
		IF (MQPRED .LE. MQMIN) RNQ =  0.
C**** 
		R1 = MPRED / MASS(I)
	 	R2 = MQPRED / MPQ
                ILAB= 'ok'
                ILABQ='ok '
		IF (RNM .EQ. 0.) ILAB = 'mz'
		IF (RNM .EQ. 59.) ILAB ='mo'
		IF (RNM .EQ. 60.) ILAB ='mu'
		IF (RNM .EQ. 61.) ILAB ='eo'
		IF (RNM .EQ. 62.) ILAB ='tu'
		IF (RNM .EQ. 63.) ILAB ='to'
		IF (RNQ .EQ. 127.) ILABQ = 'mqo'
		IF (RNQ .EQ. 0.)   ILABQ = 'mqu'
C
C	      See if predicted values are in instrument range

       IF (MPRED .GE. 10000) MPRED = 999.
       WRITE(LU2,500) RATE(I),IONS(I),MASS(I),CHG(I),MPQ,EQ,ENUC,
     $                EI,ECF,EMEAS,TOFPRE,J,EPQ_DPU,
     $		      MPRED,MQPRED,R1,R2,RNM,ILAB,RNQ,ILABQ
 500    FORMAT(' ',A5,A4,F7.2,F6.2,F6.2,F6.2,3F7.1,
     $		 F9.1,F8.1,I5,1X,F6.2,
     $           F6.1,F7.2,F7.2,F7.2,1X,F5.1,1X,A2,1X,F6.1,1X,A3)
C
        IF (I .LT. 23) THEN    
		IF( IONS(I+1) .NE. IONS(I) )   WRITE(LU2,530)
	ELSE
		IF ( RATE(I+1) .NE. RATE(I) ) WRITE(LU2,530) 
	END IF
 530    FORMAT(5X)
C
       	     ENDDO   !I, ions
       
C 
             CALL DATE (DAY)
             CALL TIME (HOUR)
             WRITE(LU2,550) DAY, HOUR, INSTRUMENT
 550    FORMAT(//,'  PROGRAM  wstics_MMQPRE    --  ',A9,3X,A8,8X,
     $  '"ok": seen in Matrix (o=over, u=underflow in m,m/q,e,t)', 
     $ 	/59X,' "mz": mass-zero (energy underflow)',
     $  /59X,' "ur": upper right hand corner of respective rate',
     $  /59X,' "ll": lower left  hand corner of respective rate',
     $  /59X,' "md": middle (logarthmic)     of respective rate'/
     $ 	'  Instrument :  ',A10,2X,
     $ 'Binning Example:  Nm=39.0 means predicted',
     $ ' mass at lower boundary of Nm bin #39')
C
C 
C 
C********** 
C 
999   TYPE *, ' Output file is wstics_MMQPRE.PRT'
C
C
      END 
C 
C 
C 
      SUBROUTINE ET(AM,Q,V,THICK,DIST,E,T, DFECT_TYPE) 
C 
C INPUT : MASS , Q , VEL. , FOIL THICKNESS , TOF DISTANCE , DFECT_TYPE
C OUTPUT : E , T
C  AM=MASS
C  V = KM/SEC 
C  E = EMEAS IN KEV 
C  T = TOF IN NS
C
       REAL AM		!mass
       REAL Q		!charge
       REAL V		!velocity
       REAL THICK	!foil thickness
       REAL DIST	!
       REAL VPAC
       REAL E
       REAL T
       INTEGER DFECT_TYPE
C 
      VPAC = 0.   !no paps
      TEMP = V / 437.71 
      EPN1 = TEMP * TEMP
      EINC = Q*VPAC + AM*EPN1 
      EPN2 = EINC / AM
      ECF = EINC - THICK*CFOIL(EPN2,AM) 
      EPN3 = ECF / AM 
      VEL = 4.3771E7 * SQRT(EPN3) 
      T = 1.E9 * DIST / VEL 
C
       IF (DFECT_TYPE .EQ. 1) THEN
       	     E = ECF * CHEMDFECT(EPN3, AM)
       ELSE IF (DFECT_TYPE .EQ. 2) THEN
             E = ECF * SWEFF(EPN3,AM)
       ELSE
             E = ECF * DFECT(EPN3, AM)
       ENDIF
C
      RETURN
      END 
