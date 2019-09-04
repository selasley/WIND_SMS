C*******************************************************************************
      PROGRAM RATE_HEADER
C This subroutine prints out the header information for the rates that have
C already been selected be the user.
C
C Created 	9-MAR-1995	by 	K. CHOTOO
C

      IMPLICIT NONE
      INTEGER I,
     +        NUM,							!number of rates to be printed
     +        POS, 							!position where rate name is written in LINE
     +        SEL(12)							!array of rate number to be printed
      CHARACTER*7 LABELS(179)						!rate names
      CHARACTER*130 LINE						!stores header labels
      CHARACTER*3 TGS_MODE						!TGS mode on/off    
      DATA LABELS/ '  He2  ',95*'       ',  '   H   ',23*'       ',
     +             '  BR0  ', 7*'       ',  '  BR1  ', 7*'       ',
     +             '  BR2  ', 7*'       ',  '  He1  ',   '   C6  ',
     +             '   C5  ',   '   C4  ',  '   07  ',   '   O6  ',
     +             '   O1  ',   '  Ne8  ',  '  Mg10 ',   '  Mg8  ',
     +		   '  Si12 ',   '  Si9  ',  '  Si8  ',   'Fe14&16',
     +		   '       ',   '  Fe12 ',  '  Fe11 ',   ' Fe9&10',
     +             '       ',   '  Fe8  ',  ' FSR12 ',   ' FSR34 ',
     +             ' FSR56 ',   '  RSR1 ',  '  RSR2 ',   '  RSR3 ',
     +             '  DCR1 ',   '  DCR2 ',  '  DCR3 ',   '  TCR1 ',
     +             '  TCR2 ',   '  TCR3 ',  '  SSD1 ',   '  SSD2 ',
     +             '  SSD3 '/
      DATA SEL/ 97,1,145,121,129,137,165,166,167,151,158,162/
      NUM = 12
      WRITE(6,*)'ENTER TGS_MODE'
      READ(5,'(A3)') TGS_MODE       
      IF(TGS_MODE .EQ. 'ON ') THEN
        LABELS(156) = ' FSR12 '
        LABELS(157) = '       '
        LABELS(158) = ' FSR34 '
        LABELS(160) = ' FSR56 '
        LABELS(161) = '       '
        LABELS(162) = '  UFSR '
        LABELS(163) = '  URSR '
        LABELS(164) = '  MFSR '
        LABELS(165) = '  MRSR '
        LABELS(166) = '  MPF  '
        LABELS(167) = '  MPR '
      ENDIF
      WRITE(LINE(1:21),'(A)')' YEAR DOY HR:MIN:SC  '
      POS = 22
      DO 200 I = 1, NUM
         WRITE(LINE(POS:POS+9),'(A9)') LABELS(SEL(I))
         POS = POS + 10
 200  CONTINUE
      WRITE(6,600)LINE
 600  FORMAT(1X,A)
C      DO 400 I= 1, 179,3
C         WRITE(6,500) I, LABELS(I),I+1,LABELS(I+1),I+2,LABELS(I+2)
C 500     FORMAT(3(1X,I6,A13))
C 400  CONTINUE
      END
