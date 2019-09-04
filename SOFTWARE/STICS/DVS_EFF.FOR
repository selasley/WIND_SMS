c***************************************************************************************************
      PROGRAM DVS_EFF
c***************************************************************************************************
C This routine calculates the efficiency for different ions at each voltage step. 
c
c Created 30-MAR-1995 	by	K. CHOTOO from the one in WSTICS_RATES as template
c
C Subroutines called:	WSTICSEFF
c Revision History:
C
C***************************************************************************************************
			IMPLICIT NONE
      REAL*4 DCR_FSR,			!DCR/FSR
     .       deltaE,			!Energy bandwidth (=0.019E)
     .       E,				!Energy
     .       EFF_DCR_DATA(0:31,1:30),  	!Matrix rate efficiency TCR/Io
     .       EFF_TCR_DATA(0:31,1:30),  	!Matrix rate efficiency TCR/Io
     .       EPQ(0:31),            	!Energy/charge for each voltage step
     .       DCR_RSR,			!DCR/RSR, Start Efficiency
     .       ION_MASS(1:30),		!Mass of ions
     .       M,				!Mass
     .       MR_ENERGY(0:31,1:30),	!Total energy at each voltage step for rates
     .       TCR_FSR			!TCR/FSR             

      INTEGER I,J			!Counters

      DATA ION_MASS/1.000,3.971,3.971,11.916,11.916,11.916,15.872,15.872,15.872,20.019,24.112,24.112,
     .              27.863,27.863,27.863,55.404,55.404,55.404,55.404,55.404,55.404,55.404,0,0,0,0,0,0,0,0/


C		************ STATEMENTS *************

      DO 10  I = 0,31
        EPQ(I) = 6.190722 * (1.1225857**I)
  10  CONTINUE

      DO 15 J=1,30			!Initialize efficiency array to 1.0
        DO 14 I=0,31
          EFF_DCR_DATA(I,J) = 1.0
	  EFF_TCR_DATA(I,J) = 1.0
  14    CONTINUE
  15  CONTINUE

      DO 20  I = 0,31
        MR_ENERGY(I,1)  = EPQ(I)	!H+
        MR_ENERGY(I,2)  = EPQ(I)*2.0	!He+2
        MR_ENERGY(I,3)  = EPQ(I)	!He+
        MR_ENERGY(I,4)  = EPQ(I)*6.0	!C+6
        MR_ENERGY(I,5)  = EPQ(I)*5.0	!C+5
        MR_ENERGY(I,6)  = EPQ(I)*4.0	!C+4
        MR_ENERGY(I,7)  = EPQ(I)*7.0	!O+7
        MR_ENERGY(I,8)  = EPQ(I)*6.0	!O+6
        MR_ENERGY(I,9)  = EPQ(I)	!O+1
        MR_ENERGY(I,10) = EPQ(I)*8.0	!Ne+8
        MR_ENERGY(I,11) = EPQ(I)*10.0	!Mg+10
        MR_ENERGY(I,12) = EPQ(I)*8.0	!Mg+8
        MR_ENERGY(I,13) = EPQ(I)*12.0	!Si+12
        MR_ENERGY(I,14) = EPQ(I)*9.0	!Si+9
        MR_ENERGY(I,15) = EPQ(I)*8.0	!Si+8
        MR_ENERGY(I,16) = EPQ(I)*16.0	!Fe+16
        MR_ENERGY(I,17) = EPQ(I)*14.0	!Fe+14
        MR_ENERGY(I,18) = EPQ(I)*12.0	!Fe+12
        MR_ENERGY(I,19) = EPQ(I)*11.0	!Fe+11
        MR_ENERGY(I,20) = EPQ(I)*10.0	!Fe+10
        MR_ENERGY(I,21) = EPQ(I)*9.0	!Fe+9
        MR_ENERGY(I,22) = EPQ(I)*8.0	!Fe+8
  20  CONTINUE 

      DO 40 J=1,22
        DO 30 I=0,31
          M = ION_MASS(J)
          E = MR_ENERGY(I,J)
          deltaE = 0.019*E				!Energy bandwidth
          CALL WSTICSEFF(E,M,DCR_RSR,TCR_FSR,DCR_FSR)
          EFF_DCR_DATA(I,J) = DCR_FSR * DCR_RSR * deltaE 
          EFF_TCR_DATA(I,J) = TCR_FSR * DCR_RSR * deltaE 
  30    CONTINUE
  40  CONTINUE

      OPEN(UNIT=50,NAME='MAC_DIR:DVS_EFF_DATA.TXT',TYPE='NEW',RECL=400)
      WRITE(50,60)
  60  FORMAT(//,1X,'  DVS    H+    He+2    He+1   C+6   C+5   C+4   O+7   O+6   O+ '
     .             '  Ne+8   Mg+10   Mg+8   Si+12   Si+9   Si+8   Fe+16   Fe+14 '
     .             '  Fe+12   Fe+11   Fe+10   Fe+9   Fe+8'/)

      DO 70 I = 0,31
        WRITE(50,80)I,(EFF_DCR_DATA(I,J),J=1,22)
  80    FORMAT(1X,I5,22(1X,F10.5))
  70  CONTINUE

      WRITE(50,*)
      WRITE(50,*)
      DO 90 I = 0,31
        WRITE(50,80)I,(EFF_TCR_DATA(I,J),J=1,22)
  90  CONTINUE



      CLOSE(50)

      END

