      PROGRAM OMR_EFF
c***************************************************************************************************
C This routine calculates the efficiency for the omni-directional matrix rates (OMR) 1-19 at each
C voltage step. These rates are for triple coincidence only, i.e. both mass/charge and non-zero
C mass obtained. Efficiencies for all other rates are set to 1.0. 
c
c Created 27-SEP-1995 	by	K. CHOTOO
c
c Revision History:
C
C***************************************************************************************************
			IMPLICIT NONE
      REAL*4 DCR_FSR,			!DCR/FSR
     .       E,				!Energy
     .       EFF_DATA(0:31,1:178),      !Matrix rate efficiency TCR/Io
     .       EPQ(0:31),            	!Energy/charge for each voltage step
     .       FSR_Io,			!FSR/Io
     .       ION_MASS(1:19),		!Mass of ions
     .       M,				!Mass
     .       MR_ENERGY(0:31,1:19),	!Total energy at each voltage step for rates
     .       TCR_FSR			!TCR/FSR             

      INTEGER I,J			!Counters

      DATA ION_MASS/11.916,11.916,11.916,15.872,15.872,15.872,20.019,24.112,24.112,27.863,
     +              27.863,27.863,55.404,55.404,55.404,55.404,55.404,55.404,55.404/


C		************ STATEMENTS *************

      DO 10  I = 0,31
        EPQ(I) = 6.190722 * (1.1225857**I)
  10  CONTINUE

      DO 15 J=1,178			!Initialize efficiency array to 1.0
        DO 14 I=0,31
          EFF_DATA(I,J) = 1.0
  14    CONTINUE
  15  CONTINUE

      DO 20  I = 0,31
        MR_ENERGY(I,1)  = EPQ(I)*6.0	!C+6
        MR_ENERGY(I,2)  = EPQ(I)*5.0	!C+5
        MR_ENERGY(I,3)  = EPQ(I)*4.0	!C+4
        MR_ENERGY(I,4)  = EPQ(I)*7.0	!O+7
        MR_ENERGY(I,5)  = EPQ(I)*6.0	!O+6
        MR_ENERGY(I,6)  = EPQ(I)	!O+1
        MR_ENERGY(I,7)  = EPQ(I)*8.0	!Ne+8
        MR_ENERGY(I,8)  = EPQ(I)*10.0	!Mg+10
        MR_ENERGY(I,9)  = EPQ(I)*8.0	!Mg+8
        MR_ENERGY(I,10) = EPQ(I)*12.0	!Si+12
        MR_ENERGY(I,11) = EPQ(I)*9.0	!Si+9
        MR_ENERGY(I,12) = EPQ(I)*8.0	!Si+8
        MR_ENERGY(I,13) = EPQ(I)*16.0	!Fe+16
        MR_ENERGY(I,14) = EPQ(I)*14.0	!Fe+14
        MR_ENERGY(I,15) = EPQ(I)*12.0	!Fe+12
        MR_ENERGY(I,16) = EPQ(I)*11.0	!Fe+11
        MR_ENERGY(I,17) = EPQ(I)*10.0	!Fe+10
        MR_ENERGY(I,18) = EPQ(I)*9.0	!Fe+9
        MR_ENERGY(I,19) = EPQ(I)*8.0	!Fe+8
  20  CONTINUE 

      DO 40 J=1,19
        DO 30 I=0,31
          M = ION_MASS(J)
          E = MR_ENERGY(I,J)
          CALL WSTICSEFF(E,M,FSR_Io,TCR_FSR,DCR_FSR)
          EFF_DATA(I,145+J) = TCR_FSR * FSR_Io 		!TCR/Io only for OMR1-19
  30    CONTINUE
  40  CONTINUE

      DO 70 I=0,31					!Print Fe rates to verify routine works well
          WRITE(6,60)I,(EFF_DATA(I,J+157),J=1,7)
  60      FORMAT(1X,I4,7F12.6)
  70  CONTINUE

 
       END

