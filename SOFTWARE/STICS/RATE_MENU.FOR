C*****************************************************************************80
      PROGRAM RATE_MENU
C This subroutine displays a menu of the available rates and prompts the user
C to choose the ones required. Or the user can go with a list of pre-selected
C rates. When TGS is on there are no Silicon 8, Silicon 9 nor any Iron rates.
C Instead there are more Engineering rates, UFSR, URSR, MFSR, MRSR.....
C
C Created  7-MAR-1995	by  K. CHOTOO
C

      IMPLICIT NONE

      INTEGER NUM,				!counts number of selections made
     +        I,				!subscript
     +        SELECTION(12)			!array to store selections
      CHARACTER*1 REPLY          		!answer for another selection
      CHARACTER*3 TGS_MODE      		!TGS on/off
      DATA SELECTION/  97,1,145,121,129,	!selections for default list     
     +                137,165,166,167,  	!with TGS OFF
     +                151,158,162/            

      WRITE(6,*)' ENTER TGS_MODE'
      READ(5,'(A3)') TGS_MODE

      IF(TGS_MODE .EQ. 'OFF') THEN
         TYPE 310
 310     FORMAT(1X,'The following is a list of rates that',
     +     ' will be printed out:'//
     +     5X,' (1) H      ',5X,' (2) He2    ',5X,' (3) He1    '/
     +     5X,' (4) BR0    ',5X,' (5) BR1    ',5X,' (6) BR2    '/
     +     5X,' (7) FSR12  ',5X,' (8) FSR34  ',5X,' (9) FSR56  '/
     +     5X,'(10) O1     ',5X,'(11) Fe14&16',5X,'(12) Fe9&10 '//
     +     1X,'Is this list okay or would you like to make your ',
     +        'own list [Y/N]?  /N/'/)
           READ(5,'(A1)')REPLY
      ELSE
         SELECTION(7)  = 156					
         SELECTION(8)  = 158
         SELECTION(9)  = 160
         SELECTION(11) = 150
         SELECTION(12) = 149

         TYPE 315
 315     FORMAT(1X,'The following is a list of rates that',
     +     ' will be printed out:'//
     +     5X,' (1) H      ',5X,' (2) He2    ',5X,' (3) He1    '/
     +     5X,' (4) BR0    ',5X,' (5) BR1    ',5X,' (6) BR2    '/
     +     5X,' (7) FSR12  ',5X,' (8) FSR34  ',5X,' (9) FSR56  '/
     +     5X,'(10) O1     ',5X,'(11) O6     ',5X,'(12) O7     '//
     +     1X,'Is this list okay or would you like to make your ',
     +        'own list [Y/N]?  /N/'/)
           READ(5,'(A1)')REPLY
      ENDIF

      NUM = 12
      IF((REPLY .EQ. 'Y') .OR. (REPLY .EQ. 'y')) NUM = 0
      DO WHILE (((REPLY .EQ. 'Y') .OR. (REPLY .EQ. 'y'))
     +           .AND. (NUM .LE. 12)) 
      NUM = NUM + 1
C        main menu
 320     TYPE 330
 330     FORMAT(/,32X,'MATRIX RATES'/
     +     9X,'  [1] He2  ',8X,' [97] H    ',8X,
     +        '[121] BR0  ',8X,'[129] BR1  '/
     +     9X,'[137] BR2  ',8X,'[145] He1  ',8X,
     +        '[146] C6	 ',8X,'[147] C5   '/
     +     9X,'[148] C4   ',8X,'[149] O7   ',8X,
     +        '[150] O6   ',8X,'[151] O1   '/
     +     9X,'[152] Ne8  ',8X,'[153] Mg10 ',8X,	
     +        '[154] Mg8  ',8X,'[155] Si12 ')

	 IF(TGS_MODE .EQ. 'OFF') THEN	!Normal mode
          TYPE 340
 340      FORMAT(
     +     9X,'[156] Si9  ',8X,'[157] Si8  ',8X,	
     +        '[158] Fe14&16',6X,'[160] Fe12 '/
     +     9X,'[161] Fe11 ',8X,'[162] Fe9&10',7X,	
     +        '[164] Fe8  '/
     +     32X,'ENGINEERING RATES'/
     +     9X,'[165] FSR12',8X,'[166] FSR34',8X,	
     +        '[167] FSR56',8X,'[168] RSR1 '/
     +     9X,'[169] RSR2 ',8X,'[170] RSR3 ',8X,
     +        '[171] DCR1 ',8X,'[172] DCR2 '/
     +     9X,'[173] DCR3 ',8X,'[174] TCR1 ',8X,	
     +        '[175] TCR2 ',8X,'[176] TCR3 '/
     +     9X,'[177] SSD1 ',8X,'[178] SSD2 ',8X,	
     +        '[179] SSD3 '/)

         ELSE				!TGS Mode
          TYPE 350
 350      FORMAT(/,32X,'ENGINEERING RATES'/
     +     9X,'[156] FSR12',8X,'[158] FSR34',8X,	
     +        '[160] FSR56',8X,'[162] UFSR '/
     +     9X,'[163] URSR ',8X,'[164] MFSR ',8X,	
     +        '[165] MRSR ',8X,'[166] MPF  '/
     +     9X,'[167] MPR  ',8X,'[168] RSR1 ',8X,	
     +        '[169] RSR2 ',8X,'[170] RSR3 '/
     +     9X,'[171] DCR1 ',8X,'[172] DCR2 ',8X,	
     +        '[173] DCR3 ',8X,'[174] TCR1 '/
     +     9X,'[175] TCR2 ',8X,'[176] TCR3 ',8X,
     +        '[177] SSD1 ',8X,'[178] SSD2 '/
     +     9X,'[179] SSD3 ',/) 
	 
         ENDIF

	 WRITE(6,*) 'Enter rate number'
         READ(5,*) SELECTION(NUM)
         IF(TGS_MODE .EQ. 'OFF') THEN
          IF(((SELECTION(NUM).GT.1).AND.(SELECTION(NUM).LT.97)).OR.
     +      ((SELECTION(NUM).GT.97).AND.(SELECTION(NUM).LT.121)).OR.
     +      ((SELECTION(NUM).GT.121).AND.(SELECTION(NUM).LT.129)).OR.
     +      ((SELECTION(NUM).GT.129).AND.(SELECTION(NUM).LT.137)).OR.
     +      ((SELECTION(NUM).GT.137).AND.(SELECTION(NUM).LT.145)).OR.
     +      (SELECTION(NUM).EQ.159).OR. (SELECTION(NUM).EQ.163) .OR.
     +       (SELECTION(NUM).GT.179))THEN
              WRITE(6,*)'Ambiguous selection number, please try again'
              GO TO 320
          ENDIF
         ENDIF

         IF(TGS_MODE .EQ. 'ON ') THEN
          IF(((SELECTION(NUM).GT.1).AND.(SELECTION(NUM).LT.97)).OR.
     +      ((SELECTION(NUM).GT.97).AND.(SELECTION(NUM).LT.121)).OR.
     +      ((SELECTION(NUM).GT.121).AND.(SELECTION(NUM).LT.129)).OR.
     +      ((SELECTION(NUM).GT.129).AND.(SELECTION(NUM).LT.137)).OR.
     +      ((SELECTION(NUM).GT.137).AND.(SELECTION(NUM).LT.145)).OR.
     +      (SELECTION(NUM).EQ.157).OR. (SELECTION(NUM).EQ.159) .OR.
     +      (SELECTION(NUM).EQ.161).OR.(SELECTION(NUM).GT.179))THEN
              WRITE(6,*)'Ambiguous selection number, please try again'
              GO TO 320
          ENDIF
         ENDIF

         WRITE(6,*) 'Selections already made:'
         DO 380 I = 1, NUM
         WRITE(6,*)SELECTION(I)
 380     CONTINUE
         WRITE(6,*) 'You may chose up to ',12 - NUM,' more rates.'
 	 WRITE(6,*) 'Would you like to make another selection [Y/N]?'
         READ(5,'(A1)') REPLY
      END DO

      DO 390 I = 1, NUM
         WRITE(6,*)SELECTION(I)
 390  CONTINUE
 
      END
