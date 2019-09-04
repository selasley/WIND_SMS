        SUBROUTINE WSWICS_GET_MMQ(SWICS_PHA,SWICS_RATE,SCI,HDB, EXC_H,EFF,!input
     .                   EPQVAL,ET,MMPQ,EFF_WEIGHT,PHANUM)            !output
C**************************************************************************
C       this subroutine reads Wind SWICS data and extracts the pha data.  these
C       data are then converted into mass and mass/charge assuming all ions
C       travel at the proton velocity.
C       only analyze data where Ech > 0 because otherwise could be null data.
C
C       cmsc 3 Jan 1995
C
C       Altered:                                           Date:
C         added option of excluding data during             6 Jan 1995
C         proton peak                                       cmsc
C
C         added to output an efficiency weighting array     9 Feb 1995
C         which contains a weighting value for each mmpq    cmsc
C         pair.  Currently this only includes DCR/FSR
C         'efficiency'                           
C
C         added efficiency subroutine to calculate mss/fsr  9 Sept 1995
C         and fsr/io efficiencies for he - kr.              cmsc
C
C         altered dcr/fsr efficiency correction so that     14 Sept 1995
C         background values are subtracted first            cmsc
C
C	  Moved code fragments at labels 5555, 6666, and    19 Jan 2000
C	  8888 to avoid jump into IF-THEN-ENDIF block;      lrb
C	  change location of EFF files from SMS1:[COHEN.WIND.SWICS.EFFICIENCY]
C	  to SMS1:[WIND.SOFTWARE.SWICS.EFFICIENCY]
C
C**************************************************************************
C                            *Declarations*
        IMPLICIT NONE
C
        INCLUDE 'INC_DIR:EDBDEF.INC/NOLIST'
        INCLUDE 'INC_DIR:L1FMT.INC/NOLIST'
        INCLUDE '($RMSDEF)'
C
        INTEGER*4
     .    WIND_DECOMPRESS                                  !function
C
        RECORD
     .    /CORE/ SCI,                                      !basic data
     .    /HDBstr/ HDB,                                    !housekeeping
     .    /SW_PHA/ SWICS_PHA,                              !pha data
     .    /SW_RATE/ SWICS_RATE                             !rate data
C         
        LOGICAL
     .    EFF,                                             !include eff
     .    EXC_H,
     .    CODE_C                                           !t=compress C
C
        INTEGER
     .    ITEMP,                                           !temp value
     .    VSTEP,                                           !voltage step
     .    H_VS(2), HM1_VS(2), HM2_VS(2),                   !vs for H+
     .    HMSTEP,
     .    ECH, TCH,                                        !energy/time chnls
     .    RANGE, SECTOR,SSD_ID,                            !pha info
     .    PHANUM,                                          !# of pha events
     .    MEMAX                                            !# of me rates
C
        INTEGER*4
     .    PAPSL                                            !paps level
C
        REAL
     .    VEL(2), T4(2),                                   !H+ & He2+ veloicty
     .    REF_EPQ,                                           !H+/He2+ e/q
     .    EPQ,                                             !e/q
     .    ENERGY, TOF,
     .    DIST/10.5/,                                        !tof distance
     .    PAPS,                                             !paps in kV
     .    EFF_WEIGHT(10080),                               !efficiency weight
     .    ET (2,10080),                                    !e,t pairs
     .    MMPQ(2,10080),               !mass&m/q pairs, max # is 168/spin
     .    EPQVAL(10080),                                   !e/q for each pha
     .    C1, 
     .    A1, A2, A3, A4, A5, A6                            !mass calc vars
C
        INTEGER
     .    FSR, DCR,                                        !for efficiency
     .    FSR_BCKGND, DCR_BCKGND,                      !for backgrnd correction
     .    I,J,K
C
C***************************************************************************
C                         
C                            *Statements*
C
        C1 = 1.73778E-5
        A1 = 6.07459
        A2 = -1.53628
        A3 = -3.06883
        A4 = 0.472764
        A5 = 0.0810787
        A6 = 0.0723728
        K = 1
C
        CALL WSWICS_GET_HVEL(SWICS_RATE, SCI, HDB, VEL, T4)          
        IF (VEL(2).LE.0) THEN
          IF (VEL(1).LE.0) THEN
            PHANUM = 0
            RETURN
          ELSE
            REF_EPQ = (VEL(1)/437.7)**2
          END IF
        ELSE
          REF_EPQ = ((VEL(2)/437.7)**2)
        END IF
C
        IF (EXC_H) THEN                                  !don't want H+data
          CALL WSWICS_GET_HPEAK(SWICS_RATE,SCI,HDB,H_VS, HM1_VS, HM2_VS)
          IF (HM1_VS(1).LT.0 .OR. H_VS(1).LT.0) GOTO 3000
        END IF                                   !exc_h
C                                                **get fsr and dcr background**
        FSR_BCKGND = 0
        DCR_BCKGND = 0
        DO I = 0, 4
          IF (SCI.EDB(I).BITRATE) THEN                 !high bitrate
            MEMAX = 109                                !number of merates
          ELSE
            MEMAX = 55                                 !number of merates
          END IF
          ITEMP = HDB.BDH.DAT(189)
          CODE_C = BTEST(ITEMP,2)
          FSR = WIND_DECOMPRESS(SWICS_RATE.EDB(I).RATE(MEMAX),CODE_C)
          DCR = WIND_DECOMPRESS(SWICS_RATE.EDB(I).RATE(MEMAX+1),CODE_C)
          FSR_BCKGND = FSR_BCKGND + FSR
          DCR_BCKGND = DCR_BCKGND + DCR
        END DO                                             !i = 0, 59
        FSR_BCKGND = MAX(NINT(FSR_BCKGND/5.),1)
        DCR_BCKGND = MAX(NINT(DCR_BCKGND/5.),1)
C
        DO I = 0, 59
C                                                **get bit rate**
          IF (SCI.EDB(I).BITRATE) THEN                 !high bitrate
            MEMAX = 109                                !number of merates
          ELSE
            MEMAX = 55                                 !number of merates
          END IF
C                                                get voltage step
          ITEMP = SWICS_RATE.EDB(I).RATE(MEMAX+6)
          VSTEP = IBITS(ITEMP,0,6)
          IF (EXC_H) THEN
            ITEMP = SWICS_RATE.EDB(HM1_VS(1)).RATE(MEMAX+6)
            HMSTEP = IBITS(ITEMP,0,6)
            IF (VSTEP.LE.HMSTEP) GOTO 2000
          END IF                                           !exc_h
C                                                get fsr and dcr
          ITEMP = HDB.BDH.DAT(189)
          CODE_C = BTEST(ITEMP,2)
          FSR = MAX((WIND_DECOMPRESS(SWICS_RATE.EDB(I).RATE(MEMAX),CODE_C)
     .         - FSR_BCKGND),0)
          DCR = MAX((WIND_DECOMPRESS(SWICS_RATE.EDB(I).RATE(MEMAX+1),CODE_C)
     .         - DCR_BCKGND),0)
C                                                get pha data
          DO J=0, SCI.EDB(I).SWICS_Len*4 - 4, 4
            CALL DISSECT_PHA(SWICS_PHA.EDB(I).PHA(J),
     .            SECTOR,ECH,RANGE,SSD_ID,TCH)
            IF (ECH.GT.0) THEN
              ENERGY = (ECH)/0.5
C                                                calc mass/charge
              EPQ = 0.5*60.0**(VSTEP/59.0)
              EPQVAL(K) = EPQ
              MMPQ(2,K) = EPQ/REF_EPQ     !assume all ions flow at ref velocity
C                                                calc tof
              ITEMP = HDB.BDH.DAT(61)
              CALL MVBITS(ITEMP,0,8,PAPSL,0) 
              PAPS = (190.96 - (117.15)*PAPSL)/(-1000)     !from BL 4/24/95
              IF (EPQ+PAPS-1.5 .LT. 0) GOTO 1000
              TOF = DIST/(437.7*SQRT((EPQ+PAPS-1.5)*(REF_EPQ/EPQ)))
              TOF = TOF *1E4                               !unit conversion
              ET(1,K) = ENERGY
              ET(2,K) = TOF
C                                                calc mass
              MMPQ(1,K) = EXP(A1+A2*LOG(ENERGY)+A3*LOG(TOF)+A4*
     .            LOG(ENERGY)*LOG(TOF)+A5*(LOG(ENERGY))**2+
     .            A6*(LOG(TOF))**3)
C                                                calc efficiency
              IF (EFF) THEN
                ENERGY = (EPQ+PAPS)*MMPQ(1,K)/MMPQ(2,K)    !incident on cf
                CALL WSWICS_EFFIC(ENERGY,MMPQ(1,K),EFF_WEIGHT(K))
                IF (FSR.GT.0) THEN
                  EFF_WEIGHT(K) = EFF_WEIGHT(K)*REAL(DCR)/FSR     !dcr/fsr eff
                ELSE
                  EFF_WEIGHT(K) = 1E12
                END IF
              ELSE
                EFF_WEIGHT(K) = 1.
              END IF
              IF (EFF_WEIGHT(K).EQ.0) EFF_WEIGHT(K) = 1E12 !mss/fsr could be 0
              K = K + 1
            END IF                                         !ech > 0
 1000     END DO                                           !j = 0, sci...
 2000   END DO                                           !i = 0, 59
 3000   PHANUM = K - 1
        RETURN
        END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE DISSECT_PHA(PHA,SECTOR,ENERGY,RANGE,SSD_ID,TIME_OF_FLIGHT)
C
C This subroutine does bit computations to get Stop ID, Start ID, Energy, Sector, SSD ID,
C and Time of flight for SWICS PHA words on WIND
C
      INTEGER*4 SECTOR,ENERGY,RANGE,SSD_ID,TIME_OF_FLIGHT
      INTEGER*4 ITEMP
      BYTE PHA(4)
C
C      WRITE(50,101) PHA
C101   FORMAT(4(1X,Z2.2))
C
      SECTOR = 0
      ITEMP = PHA(1)
      CALL MVBITS( ITEMP, 0, 4, SECTOR, 0 )
C
      ENERGY = PHA(2)
      ENERGY = IAND( ENERGY, 255 )
C
      ITEMP = PHA(3)
      RANGE = 0
      CALL MVBITS( ITEMP, 4, 4, RANGE, 0 )
C
      SSD_ID = 0
      CALL MVBITS( ITEMP, 2, 2, SSD_ID, 0 )
C
      TIME_OF_FLIGHT = ISHFT( IAND( ITEMP, 3 ), 8 )
      ITEMP = PHA(4)
      TIME_OF_FLIGHT = TIME_OF_FLIGHT + IAND( ITEMP, 255 )
C
      RETURN
      END
C***************************************************************************
        SUBROUTINE WSWICS_EFFIC(ENERGY,MASS,EFF)
C
C	this subroutine returns total efficiency
C	for a given mass.  It uses data from tables that were created by 
C	fitting a smooth curve to calibration data for H, He, C, O, Ne, Ar, Kr.
C	For masses not in data files the program interpolates between masses
C	available.
C    units of efficiency are cm^2
C
C	written by cmsc, August 10, 1995
C
C    Alterations                                 Date
C     added si and he files                       12 Sept 1995 cmsc
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
     .	  M(7)/1.008,4.0026,12.0,15.995,28.086,39.98,83.92/,
C				aval data mass -  H,He,C,O,Si,Ar,Kr
C				have Ne data but not used in interpolation
     .	  EFF 						!efficiency
C
C
     	CHARACTER*5
     .	  HEADER					!text in data files
C
     	CHARACTER*9
     .	  DATFILE(7)/'H_95.eff','HE_95.eff','C_95.eff','O_95.eff',
     .		'SI_95.EFF','AR_95.EFF','KR_95.eff'/
C.							!data files
C
     	CHARACTER*37
     .	  DIR/'SMS1:[WIND.SOFTWARE.SWICS.EFFICIENCY]'/	!location of files
C
C			possible masses
     	REAL
     .	  REALM,					!real ion mass
     .	  ACTLM(13)/1.008,4.0026,12.0,14.007,15.995,19.992,	!possible ions
     .		22.990,24.305,28.086,32.064,39.98,55.85,83.92/,
C			!H,HE,C,N,O,NE,NA,MG,SI,S,AR,FE,KR
     .    AR_MASS/39.98/, NE_MASS/19.992/,       !ar and ne masses
     .	  DELTA,					!input mass-real mass
     .	  MIN						!minimum difference
C
C			interpolation
     	LOGICAL
     .	  INTERP					!must interpolate
C
C			interpolation by energy
     	REAL
     .	  EPM,
     .	  DATA(5),					!file data values
     .	  ELO, EHI,					!energy values
     .	  DLO, DHI,					!eff values
     .	  TLO, THI,					!eff values
     .	  MLO, MHI,					!eff values
     .	  FLO, FHI,					!eff values
     .	  MEFF,FEFF				!interpolated value
C
C			interpolation by mass
     	REAL
     .	  DATA1(5),					!2nd file data
     .	  E1LO, E1HI,					!energy values
     .	  D1LO, D1HI,					!eff values
     .	  T1LO, T1HI,					!eff values
     .	  M1LO, M1HI,					!eff values
     .	  F1LO, F1HI,					!eff values
     .	  MEFF1,FEFF1,				!interpolated value
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
        IF (MASS.GT.100) THEN
          EFF = 1
          RETURN
        END IF
C
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
C&
        IF (REALM .LT. 2) THEN
c          WRITE(*,*) ' dont have this ion data'
          EFF = 1
          RETURN
        END IF
C&
     	EPM = ENERGY/REALM
C						find if interpolation is
C						necessary or if we have this
C						ion.  open approp. files
     	IF (REALM.EQ.NE_MASS) THEN		!Ne have data for it
     	  OPEN (UNIT=1, FILE=DIR//'NE_95.EFF', STATUS='OLD', READONLY)
     	  REWIND 1
     	  GOTO 555
     	END IF
C
     	DO I = 1, 6
     	  IF ((REALM.LT.M(I+1)) .AND. (REALM.GT.M(I))) THEN
     	    OPEN (UNIT=1, FILE=DIR//DATFILE(I), STATUS='OLD', READONLY)
     	    REWIND 1
     	    M1 = M(I)
     	    OPEN (UNIT=5, FILE=DIR//DATFILE(I+1), STATUS='OLD', READONLY)
     	    REWIND 5
     	    M2 = M(I+1)
     	    INTERP = .TRUE.
            IF (I.EQ.5) THEN
              OPEN(UNIT=6, FILE=DIR//DATFILE(7), STATUS='OLD', READONLY)
              REWIND 6
            END IF                               ! i = 5
            IF (I.EQ.6) THEN
              OPEN (UNIT=6, FILE=DIR//DATFILE(5), STATUS='OLD', READONLY)
              REWIND 6
            END IF                                         !i=6
     	  ELSE IF (REALM.EQ.M(I)) THEN		!have this ion
     	    OPEN (UNIT=1, FILE=DIR//DATFILE(I), STATUS='OLD',READONLY)
     	    REWIND 1
     	  END IF
     	END DO
     	IF (REALM.EQ.M(7)) THEN
     	  OPEN (UNIT=1, FILE=DIR//DATFILE(7), STATUS='OLD', READONLY)
     	  REWIND 1
     	END IF
     	IF (REALM.GT.M(7)) THEN			!shouldn't happen
     	  EFF = 1.0
     	  RETURN
     	END IF
C
 555   	IF (INTERP) THEN			!interpolation necessary
     	  DO I = 1, 2
     	    READ(1,'(A)') HEADER		!skip headers
      	    READ(5,'(A)') HEADER
      	    IF (M1.EQ.AR_MASS .OR. M2.EQ.AR_MASS) READ(6,'(A)') HEADER
     	  END DO
C						first file - low mass
     	  ELO = 10000
     	  DO WHILE (EPM.LT.ELO)		!finding energy rng - E is
     	    EHI = ELO				!decreasing in file
     	    DHI = DLO
     	    THI = TLO
     	    MHI = MLO
     	    FHI = FLO
     	    READ(1,*,END=5555) (DATA(I), I=1,5)	!low ion data
     	    ELO = DATA(1)
     	    DLO = DATA(2)
     	    TLO = DATA(3)
     	    MLO = DATA(4)
     	    FLO = DATA(5)
            IF (M1.EQ.AR_MASS) THEN
              READ(6,*,END=5555) (DATA(I), I=1,5)
              FLO = DATA(5)
            END IF                               !m1=ar_mass
     	  END DO
C						energy interpolation
     	  MEFF = ((MHI-MLO)/(EHI-ELO))*(EPM-ELO) + MLO
     	  FEFF = ((FHI-FLO)/(EHI-ELO))*(EPM-ELO) + FLO
	  GOTO 1111
C
 5555     MEFF = MLO				!at end of first file
     	  FEFF = FLO				!at end of first file
     	  GO TO 1111
C
C						second file - high mass
 1111	  E1LO = 10000
 	  DO WHILE (EPM.LT.E1LO)		!finding energy rng
     	    E1HI = E1LO
     	    D1HI = D1LO
     	    T1HI = T1LO
     	    M1HI = M1LO
     	    F1HI = F1LO
     	    READ(5,*,END=6666) (DATA1(I), I=1,5)	!high ion data
     	    E1LO = DATA1(1)
     	    D1LO = DATA1(2)
     	    T1LO = DATA1(3)
     	    M1LO = DATA1(4)
     	    F1LO = DATA1(5)
            IF (M2.EQ.AR_MASS) THEN
              READ(6,*,END=6666) (DATA1(I), I=1,5)
              F1LO = DATA1(5)
            END IF                               !m1=ar_mass
     	  END DO
C						energy interpolation
     	  MEFF1 = ((M1HI-M1LO)/(E1HI-E1LO))*(EPM-E1LO) + M1LO
     	  FEFF1 = ((F1HI-F1LO)/(E1HI-E1LO))*(EPM-E1LO) + F1LO
	  GOTO 2222
C
 6666     MEFF1 = M1LO				!at end of second file
     	  FEFF1 = F1LO				!at end of second file
     	  GO TO 2222
C
C						mass interpolation
 2222  	  EFF = ((MEFF1-MEFF)/(M2-M1)) * (REALM-M1) + MEFF
 	     EFF = EFF*(((FEFF1-FEFF)/(M2-M1)) * (REALM-M1) + FEFF)
     	ELSE					!have this ion
     	  DO I = 1, 2
     	    READ(1,'(A)') HEADER		!skip headers
     	  END DO
     	  ELO = 10000
     	  DO WHILE (EPM.LT.ELO)
     	    EHI = ELO
     	    DHI = DLO
     	    THI = TLO
     	    MHI = MLO
     	    FHI = FLO
     	    READ(1,*,END=8888) (DATA(I), I=1,5)
     	    ELO = DATA(1)
     	    DLO = DATA(2)
     	    TLO = DATA(3)
     	    MLO = DATA(4)
     	    FLO = DATA(5)
     	  END DO
     	  EFF = ((MHI-MLO)/(EHI-ELO))*(EPM-ELO) + MLO
     	  EFF = EFF*(((FHI-FLO)/(EHI-ELO))*(EPM-ELO) + FLO)
     	END IF     					!interp
     	GOTO 3333
C
 8888   EFF = MLO				!at end of only file
     	EFF = EFF*FLO				!at end of only file
     	GO TO 3333
C
 3333	REWIND 1
     	IF (INTERP) REWIND 5
        IF (M1.EQ.AR_MASS .OR. M2.EQ.AR_MASS) REWIND 6
C
     	CLOSE(UNIT=1)
     	IF (INTERP) CLOSE(UNIT=5)
        IF (M1.EQ.AR_MASS .OR. M2.EQ.AR_MASS) CLOSE(UNIT=6)
C
     	RETURN
     	END
