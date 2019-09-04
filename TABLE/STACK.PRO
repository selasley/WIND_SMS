;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;PROGRAM STACK.PRO
;this idl program reads  Engineering Rate files (ER1.STICS) for FSRs and RSRs,
;sums rates for each voltage step and plots rate
;against voltage step stacked against time. 
;
;Created   20-FEB-1995		KANCHAM CHOTOO
;
;This version:	1.0	20-FEB-1995
;
;Revision History:
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION NOTICKS, axis, index, value
;    this function return blanks for  tick labels
temp = ' '
RETURN, temp
END
FUNCTION YTEST, AXIS, INDEX, VALUE		;function used to format time labels on y-axis
HOUR = LONG(VALUE)/3600			
MINUTE = LONG(VALUE - 3600*HOUR)/60
SEC = VALUE MOD 60
RETURN, STRING(HOUR, MINUTE, SEC, FORMAT="(I2.2,2(':',I2.2))")
END

;                 **main program starts here**
;******************************************************************************
FILENAME = ' '
BITRATE  = ' '
HEADER = ' ' 
DV_STEP = FINDGEN(32)
DV_EPQ = FLTARR(32)              	;array of energy per charge
DVS_HITS= FLTARR(32)			;stores number of hits for each VS
INTERVAL_END_TIME = FLTARR(480)
FOR I = 0, 31 DO DV_EPQ(I) = 6.190722*1.1225857^I
DVS_HITS(*) = 0
INTERVAL_END_TIME(*) = 0
N = -1   			

PRINT, 'Enter name of input data file'
READ, FILENAME

display_type = '2'
PRINT, 'Do you want plot go to poscript file (1)'
PRINT, 'or displayed on screen (2)? /2/'
READ, display_type
IF display_type EQ '1' THEN SET_PLOT, 'PS'
 
PRINT, 'Enter number of Science Records between data points'
READ, INTERVAL
;INTERVAL = 2

OPENR, 1, FILENAME

;get date
REPEAT BEGIN		
READF,1, HEADER					;read header line by line until "WI_L1_SMS_"
DATE_POS= STRPOS(HEADER, 'WI_L1_SMS_')		;is found
PRINT, DATE_POS
ENDREP UNTIL  DATE_POS NE -1 
DATE_OF_FILE = STRMID(HEADER,DATE_POS+10,8)		;extract DATE substring
PRINT, DATE_OF_FILE

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
      DATA_STACK = FLTARR(32,480,15)		;          MPF, MPR, 3 RSR's
      DATA_STACK(*,*,*) = 0
      DATA = FLTARR(32,15)			
      TEMP = FLTARR(15)
      NORM_DATA = FLTARR(32,15)
           
      WHILE NOT EOF(1) DO BEGIN
	ON_IOERROR, GO_ON		;skip over header information
	READF,1, SR,YR,DOY,HR,MIN,SEC,BITRATE,V_STEP,EPQ,TEMP, $
	FORMAT = '(1X,I8,1X,I2,1X,I3,1X,I2,1X,I2.2,1X,I2.2,1X,A1,1X,I2,1X,F8.3,15(1X,I7))'
        LABEL_ONE:
        DATA(*,*) = 0  				;initialize matrix to 0
	DVS_HITS(*) = 0 
        SR_INTERVAL = SR + INTERVAL

        REPEAT BEGIN
           IF (HR+MIN+SEC NE 0) THEN $
              IF TOTAL(TEMP) NE 0 THEN BEGIN
	        TIME_LAST = HR*3600 + MIN*60 + SEC
                DVS_HITS(V_STEP) = DVS_HITS(V_STEP) +1.0   ;count # of hits for each Voltage Step
                DATA(V_STEP,*) = DATA(V_STEP,*) + TEMP
              ENDIF
           READF,1, SR,YR,DOY,HR,MIN,SEC,BITRATE,V_STEP,EPQ,TEMP, $
           FORMAT='(1X,I8,1X,I2,1X,I3,1X,I2,1X,I2.2,1X,I2.2,1X,A1,1X,I2,1X,F8.3,15(1X,I7))'
  	ENDREP  UNTIL (SR GE SR_INTERVAL) OR EOF(1) 
        N = N + 1
        DVS_HITS = DVS_HITS > 1			;convert 0 hits to 1 to prevent dividing by 0
         					;in normalizing data
        FOR I = 0, 31 DO NORM_DATA(I,*) = DATA(I,*) / DVS_HITS(I)
        INTERVAL_END_TIME(N) = TIME_LAST
	DATA_STACK(*,N,*) = NORM_DATA       
        PRINT, INTERVAL_END_TIME(N), FORMAT = '(I8.5)'
       	IF NOT EOF(1) THEN GOTO, LABEL_ONE
	GO_ON:
      ENDWHILE
      PRINT, 'TGS mode is ON'
      
      Z_MAX = MAX(DATA_STACK)
      Z = DATA_STACK(*,0:N,*)
      T = INTERVAL_END_TIME(0:N)
      Z(WHERE(Z EQ 0)) = 5.0E6			;plotting missing data

 ; make plot
      SURFACE,Z(*,*,6), DV_STEP, T, $
               MAX_VALUE = Z_MAX, $		;values > max_value will not be plotted
              /HORIZONTAL, $ 			;no grid lines
;              AX = 75,	$			;rotate around x-axis 75deg to viewer
               AZ = 330, $ 			;rotation about z-axis
;	      /ZTYPE,   $			;log z-axis
	      XRANGE=[0,31], XSTYLE=1, $
              YRANGE = [0,86400], YTICKFORMAT='NOTICKS', $
	      YMINOR = 6, YTICKS = 12, $
              TITLE  = 'WIND/SMS/STICS  ' + DATE_OF_FILE , $
	      XTITLE = 'VOLTAGE STEP', $
              /SAVE
       AXIS, YAXIS = 1, /T3D ,$
              YRANGE = [0,86400], YSTYLE = 1, YTICKFORMAT='YTEST', $
	      YMINOR = 5, YTICKS = 11,  YTITLE = 'TIME', /NOERASE
       AXIS, XAXIS = 1,/T3D,XRANGE=[0,31],XSTYLE=1,XTICKFORMAT='NOTICKS',$
               /NOERASE
    END

    TGS_MODE EQ 'OFF' : BEGIN			;TGS OFF =>3 FSR's and 3 RSR's
      DATA_STACK = FLTARR(32,480,6)
      DATA_STACK(*,*,*) = 0
      DATA = FLTARR(32,6)
      TEMP = FLTARR(6)
      NORM_DATA = FLTARR(32,6)      
      WHILE NOT EOF(1) DO BEGIN
	ON_IOERROR, SKIP_LINE		;skip over header information
	READF,1, SR,YR,DOY,HR,MIN,SEC,BITRATE,V_STEP,EPQ,TEMP, $
	FORMAT = '(1X,I8,1X,I2,1X,I3,1X,I2,2(1X,I2.2),1X,A1,1X,I2,1X,F8.3,3(1X,I7),72X,3(1X,I7))'
        LABEL_TWO:
        DATA(*,*) = 0  				;initialize matrix to 0
	DVS_HITS(*) = 0 
        SR_INTERVAL = SR + INTERVAL

        REPEAT BEGIN
           IF (HR+MIN+SEC NE 0) THEN $
              IF TOTAL(TEMP) NE 0 THEN BEGIN
	        TIME_LAST = HR*3600 + MIN*60 + SEC
                DVS_HITS(V_STEP) = DVS_HITS(V_STEP) + 1.0  ;count # of hits for each Voltage Step
                DATA(V_STEP,*) = DATA(V_STEP,*) + TEMP
              ENDIF
           READF,1, SR,YR,DOY,HR,MIN,SEC,BITRATE,V_STEP,EPQ,TEMP, $
           FORMAT='(1X,I8,1X,I2,1X,I3,1X,I2,2(1X,I2.2),1X,A1,1X,I2,1X,F8.3,3(1X,I7),72X,3(1X,I7))'
  	ENDREP  UNTIL (SR GE SR_INTERVAL) OR EOF(1) 
        N = N + 1
        DVS_HITS = DVS_HITS > 1			;convert 0 hits to 1 to prevent dividing by 0
         					;in normalizing data
        FOR I = 0, 31 DO NORM_DATA(I,*) = DATA(I,*) / DVS_HITS(I)
        INTERVAL_END_TIME(N) = TIME_LAST
	DATA_STACK(*,N,*) = NORM_DATA       
        PRINT, INTERVAL_END_TIME(N), FORMAT = '(I8.4)'
       	IF NOT EOF(1) THEN GOTO, LABEL_TWO
        SKIP_LINE:
      ENDWHILE
      PRINT, 'TGS mode is OFF'
      Z_MAX = MAX(DATA_STACK)
      Z = DATA_STACK(*,0:N,*)
      T = INTERVAL_END_TIME(0:N)
      Z(WHERE(Z EQ 0)) = 5.0E6			;plotting missing dat

; make plot
      SURFACE,Z(*,*,1),DV_STEP, T, $
              MAX_VALUE = Z_MAX, $
              /HORIZONTAL, $ 			;no grid lines
;              AX = 75,	$			;rotate around x-axis 75deg to viewer
              AZ = 330, $	                ;rotation about z-axis
	      /ZTYPE,   $			;log z-axis
	      XRANGE=[0,31], XSTYLE=1, $
              YRANGE = [0,86400], YSTYLE = 1, YTICKFORMAT='NOTICKS', $
	      YMINOR = 6, YTICKS = 12, $
              TITLE  = 'WIND/SMS/STICS  ' + DATE_OF_FILE , $
	      XTITLE = 'VOLTAGE STEP', $
              /SAVE
       AXIS, YAXIS = 1, /T3D ,$
              YRANGE = [0,86400], YSTYLE = 1, YTICKFORMAT='YTEST', $
	      YMINOR = 5, YTICKS = 11,  YTITLE = 'TIME', /NOERASE
       AXIS, XAXIS = 1,/T3D,XRANGE=[0,31],XSTYLE=1,XTICKFORMAT='NOTICKS',$
               /NOERASE
    END
    ELSE : PRINT, 'TGS MODE IS NOT DEFINED'
ENDCASE
   
CLOSE, 1

END

