;this idl program reads  Engineering Rate file,
;sums rates for each voltage step and plots rate
;against voltage step.
FILENAME = ' '
BITRATE  = ' '
HEADER = ' ' 
DV_STEP = FINDGEN(32)
DV_EPQ = FLTARR(32)              	;array of energy per charge
DVS_HITS= FLTARR(32)			;stores number of hits for each VS
FOR I = 0, 31 DO DV_EPQ(I) = 6.190722*1.1225857^I
DVS_HITS(*) = 0
N = 0           			
INVALID_VS = 0
PRINT, 'Enter name of input data file'
READ, FILENAME
OPENR, 1, FILENAME

;find out whether TGS is ON or OFF
REPEAT BEGIN		
READF,1, HEADER					;read header line by line until "TGS mode is"
TGS_POS= STRPOS(HEADER, 'TGS mode is ')		;is found
PRINT,TGS_POS
ENDREP UNTIL  TGS_POS NE -1 
TGS_MODE = STRMID(HEADER,TGS_POS+12,3)		;extract "ON/OFF" substring
PRINT, TGS_MODE

;get data
CASE 1 OF 
    TGS_MODE EQ 'ON ' : BEGIN			;TGS ON => 6 FSR's, UFSR, URSR, MFSR, MRSR,
      DATA = FLTARR(32,15)			;          MPF, MPR, 3 RSR's
      TEMP = FLTARR(15)
      NORM_DATA = FLTARR(32,15)      
      DATA(*,*) = 0  				;initialize matrix to 0
      WHILE NOT EOF(1) DO BEGIN
	ON_IOERROR, GO_ON		;skip over header information
	READF,1, SR,YR,DOY,HR,MIN,SEC,BITRATE,V_STEP,EPQ,TEMP, $
	FORMAT = '(1X,I8,1X,I2,1X,I3,1X,I2,1X,I2.2,1X,I2.2,1X,A1,1X,I2,1X,F8.3,15(1X,I7))'
 	IF HR+MIN+SEC EQ 0 THEN GOTO, GO_ON     ;skip data gap
	IF TOTAL(TEMP) EQ 0 THEN GOTO, GO_ON    ;sum of rates should not be 0
        PRINT,SR,YR,DOY,HR,MIN,SEC,BITRATE,V_STEP,EPQ, $
        FORMAT = '(1X,I8,1X,I2,1X,I3,1X,I2,1X,I2.2,1X,I2.2,1X,A1,1X,I2,1X,F8.3)'
	IF (V_STEP GE 0) AND (V_STEP LE 31) THEN BEGIN 
	N = N + 1
	DVS_HITS(V_STEP) = DVS_HITS(V_STEP) +1.0 	;count # of hits for each Voltage Step
	DATA(V_STEP,*) = DATA(V_STEP,*) + TEMP 	
        ENDIF ELSE INVALID_VS = INVALID_VS + 1
	GO_ON:
      ENDWHILE
      PRINT, 'TGS mode is ON'

      DVS_HITS = DVS_HITS > 1			;convert 0 hits to 1 to prevent dividing by 0
         					;in normalizing data
      PRINT, INVALID_VS, FORMAT = '("INVALID_VS= ",F6.2)'	       
      FOR I = 0, 31 DO NORM_DATA(I,*) = DATA(I,*) / DVS_HITS(I)
      NORM_DATA = NORM_DATA > 1E-6 		;to avoid taking the log of 0
      Y_MAX = MAX(NORM_DATA)

      ; make plot
      PLOT_IO, DV_STEP,NORM_DATA(*,6), XRANGE=[0,32], $
         YRANGE = [1,Y_MAX]
      OPLOT, DV_STEP, NORM_DATA(*,7), LINE=1
    END

    TGS_MODE EQ 'OFF' : BEGIN			;TGS OFF =>3 FSR's and 3 RSR's
      DATA = FLTARR(32,6)
      TEMP = FLTARR(6)
      NORM_DATA = FLTARR(32,6)      
      DATA(*,*) = 0  				;initialize matrix to 0
      WHILE NOT EOF(1) DO BEGIN
	ON_IOERROR, SKIP_LINE		;skip over header information
	READF,1, SR,YR,DOY,HR,MIN,SEC,BITRATE,V_STEP,EPQ,TEMP, $
	FORMAT = '(1X,I8,1X,I2,1X,I3,1X,I2,2(1X,I2.2),1X,A1,1X,I2,1X,F8.3,3(1X,I7),72X,3(1X,I7))'
 	IF HR+MIN+SEC EQ 0 THEN GOTO, SKIP_LINE     ;skip data gap
	IF TOTAL(TEMP) EQ 0 THEN GOTO,SKIP_LINE     ;sum of rates should not be 0
        PRINT,SR,YR,DOY,HR,MIN,SEC,BITRATE,V_STEP,EPQ, $
        FORMAT = '(1X,I8,1X,I2,1X,I3,1X,I2,1X,I2.2,1X,I2.2,1X,A1,1X,I2,1X,F8.3)'
	IF (V_STEP GE 0) AND (V_STEP LE 31) THEN BEGIN 
	N = N + 1
	DVS_HITS(V_STEP) = DVS_HITS(V_STEP) +1.0 	;count # of hits for each Voltage Step
	DATA(V_STEP,*) = DATA(V_STEP,*) + TEMP 	
        ENDIF ELSE INVALID_VS = INVALID_VS + 1
	SKIP_LINE:
      ENDWHILE
      PRINT, 'TGS mode is OFF'
      DVS_HITS = DVS_HITS > 1			;convert 0 hits to 1 to prevent dividing by 0
					;in normalizing data
      PRINT, INVALID_VS, FORMAT = '("INVALID_VS= ",F6.2)'	       
      FOR I = 0, 31 DO NORM_DATA(I,*) = DATA(I,*) / DVS_HITS(I)
      NORM_DATA = NORM_DATA > 1E-6 		;to avoid taking the log of 0
      Y_MAX = MAX(NORM_DATA)

      ; make plot
      PLOT_IO, DV_STEP,NORM_DATA(*,1), XRANGE=[0,32], $
         YRANGE = [1,Y_MAX]
      OPLOT, DV_STEP,NORM_DATA(*,4), LINE=1
    END
    ELSE : PRINT, 'TGS MODE IS NOT DEFINED'
ENDCASE
   
CLOSE, 1

END

