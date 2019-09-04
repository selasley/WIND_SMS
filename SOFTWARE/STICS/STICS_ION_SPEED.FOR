
c***************************************************************************************************
      PROGRAM STICS_ION_SPEED
c***************************************************************************************************
C This routine calculates the speed for different ions at each voltage step. 
c
c Created 27-SEP-1996 	by	K. CHOTOO from the one in OMR_EFF as template
c
c Revision History:
C
C***************************************************************************************************
			IMPLICIT NONE
      REAL*4 ION_SPEED(0:31,1:30),     !Matrix rate efficiency TCR/Io
     .       EPQ(0:31),            	!Energy/charge for each voltage step
     .       ION_MASS(1:30),		!Mass of ions
     .       MR_ENERGY(0:31,1:30)	!Total energy at each voltage step for each ion

      INTEGER I,J			!Counters

      DATA ION_MASS/1.000,3.971,3.971,11.916,11.916,11.916,15.872,15.872,15.872,20.019,24.112,24.112,
     .              27.863,27.863,27.863,55.404,55.404,55.404,55.404,55.404,55.404,55.404,
     .              0,0,0,0,0,0,0,0/

      CHARACTER*9 LABELS(1:30)		!Ion names
      DATA LABELS/ '       H ',   '     He2 ',  '     He1 ',   '      C6 ',
     +             '      C5 ',   '      C4 ',  '      07 ',   '      O6 ',
     +             '      O1 ',   '     Ne8 ',  '    Mg10 ',   '     Mg8 ',
     +		   '    Si12 ',   '     Si9 ',  '     Si8 ',   '    Fe16 ',
     +		   '    Fe14 ',   '    Fe12 ',  '    Fe11 ',   '    Fe10 ',
     +             '     Fe9 ',   '     Fe8 ',8*'         '/


C		************ STATEMENTS *************

      DO 10  I = 0,31
        EPQ(I) = 6.190722 * (1.1225857**I)
  10  CONTINUE


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
	   ION_SPEED(I,J) = 437.74 * SQRT(MR_ENERGY(I,J)/ION_MASS(J))
  30    CONTINUE
  40  CONTINUE

    
      OPEN (UNIT=10, NAME = 'STICS_ION_SPEED.DAT',STATUS='NEW',RECL=400)

      WRITE(10,*) 'STICS ION VELOCITY AT EACH VOLTAGE STEP'
      WRITE(10,50) LABELS
  50  FORMAT(1X,' DVS',30A9)

      DO I = 0,31
        WRITE(10,60) I,(ION_SPEED(I,J), J=1,22)
  60	FORMAT(1X,I4,22F9.1)
      END DO

      CLOSE(10) 
      

      END
