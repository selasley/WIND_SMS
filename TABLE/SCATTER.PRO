;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;This routine reads a table listing of Mass vs Mass/charge and makes a scatter plot
;of the data.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FILENAME = ' '
MPQ =  FLTARR(5000)
MASS = FLTARR(5000)
MPQ_DATA = FLTARR(5000)
MASS_DATA = FLTARR(5000)
PRINT, 'ENTER NAME OF PHA FILE'
READ, FILENAME
I=0
J=0
FIRST = 'FIRST'
OPENR, 2, FILENAME
WHILE NOT EOF(2) DO BEGIN
  ON_IOERROR, GO_ON
  READF,2,TEMP1,TEMP2, FORMAT = '(124X,F11.2,F8.1)'
  MPQ(I)  = TEMP1
  MASS(I) = TEMP2
  I = I + 1
  J = J + 1
  IF I EQ 5000 THEN BEGIN
    IF FIRST EQ 'FIRST' THEN BEGIN
       FIRST = ' '
       MPQ_DATA  = MPQ
       MASS_DATA = MASS
    ENDIF ELSE BEGIN
       MPQ_DATA  = [MPQ_DATA,MPQ]
       MASS_DATA = [MASS_DATA,MASS]
    ENDELSE
    I = 0
  ENDIF
  GO_ON:
ENDWHILE
CLOSE, 2

IF MAX(MASS_DATA) EQ 0 THEN BEGIN
   MPQ_DATA  = MPQ
   MASS_DATA = MASS
ENDIF ELSE BEGIN
   MPQ_DATA  = [MPQ_DATA,MPQ(0:I-1)]
   MASS_DATA = [MASS_DATA,MASS(0:I-1)]
ENDELSE  

FOR K = 0,J-1 DO $
PRINT,K, MPQ_DATA(K),MASS_DATA(K), FORMAT = '(I5,2X,F8.2,F8.1)'

END
