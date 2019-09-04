;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PROGRAM  ER1_RATES.PRO
;this idl program reads ER1.STICS files for FSRs,
;averages FSR rate for each voltage step over a given number of Science Records,
;then sums the rate over all voltage steps and plots of SUM OF FSRS vs TIME
;
;Created         19-FEB-1995	KANCHAM CHOTOO
;
;This version   1.05      13-APR-1995
;
;Revision History:
;	26-MAR-1995	K. CHOTOO	Write average rates to output file "STX_FSR_(date).LIS"
;	 5-APR-1995	K. CHOTOO	Add command to make printed plots bigger
;       11-APR-1995     K. CHOTOO       Commented out SUM_FSR and writing to file "STX_FSR..."
;	12-APR-1995     K. CHOTOO       After summing FSRs over all voltage steps, divide by
;					number of voltage steps used.
;       13-APR-1995     K. CHOTOO       Took plotting routines out of CASE statement. So
;					program is now has fewer lines. Increased plot size by
;					changing x-position from 0.2 to 0.1 to match up with
;					SWICS time plots.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;*************************************************************************************************
;                            main program starts here
;*************************************************************************************************
FILENAME = ' '
BITRATE  = ' '
HEADER = ' ' 
FSR12 = FLTARR(480)
FSR34 = FLTARR(480)
FSR56 = FLTARR(480)
;SUM_FSRS = FLTARR(480)
DV_STEP = FINDGEN(32)
DV_EPQ = FLTARR(32)              	;array of energy per charge
DVS_HITS= FLTARR(32)			;stores number of hits for each VS
INTERVAL_END_TIME = FLTARR(480)
FOR I = 0, 31 DO DV_EPQ(I) = 6.190722*1.1225857^I  	;DPU algorithm for e/q
DVS_HITS(*) = 0
INTERVAL_END_TIME(*) = 0
N = -1   			

PRINT, 'Enter name of input data file'
READ, FILENAME

;PRINT, 'Enter number Science Record Interval'
;READ, INTERVAL
INTERVAL = 2
OPENR, 1, FILENAME
;get date
REPEAT BEGIN		
READF,1, HEADER					;read header line by line until "WI_L1_SMS_"
DATE_POS= STRPOS(HEADER, 'WI_L1_SMS_')		;is found
PRINT, DATE_POS
ENDREP UNTIL  DATE_POS NE -1 
DATE_OF_FILE = STRMID(HEADER,DATE_POS+10,8)		;extract DATE substring
PRINT, DATE_OF_FILE					

;output file 
;OUTFILE = 'STX_FSR_' + DATE_OF_FILE + '.LIS'
;OPENW,2, OUTFILE

;get lower and upper limits for y-axis
PRINT, 'Enter lower and upper limits for FSR Counts'
READ, Y_LOW, Y_HIGH
;Y_LOW = 100
;Y_HIGH = 10000

DISPLAY_REPLY = '1'
PRINT, 'Do you want plot to go to postscript file (1)'
PRINT, 'or displayed on screen (2)? /2/'
READ,DISPLAY_REPLY, FORMAT = '(A1)'
IF DISPLAY_REPLY EQ '1' THEN BEGIN
   SET_PLOT, 'PS'       ;make postcript file
;makes plot 7 x 9 inches starting (1in,1in) from bottom left corner
   DEVICE, /PORTRAIT, /INCHES, XSIZE=7.0, YSIZE=9.0, XOFF=1.0, YOFF=1.0
ENDIF


IF LONG(DATE_OF_FILE) LE 19950106 THEN $		;DPPS Stepping Sequence change 
     MAIN_TITLE = 'WIND/SMS/STICS  ' + DATE_OF_FILE + $
     '!CCounts/Spin Averaged Over DVS 2-31' $
ELSE MAIN_TITLE = 'WIND/SMS/STICS  ' + DATE_OF_FILE + $
    '!CCounts/Spin Averaged Over DVS 0-30(except 26)'              

;find out whether TGS is ON or OFF
REPEAT BEGIN		
READF,1, HEADER					;read header line by line until "TGS mode is"
TGS_POS= STRPOS(HEADER, 'TGS mode is ')		;is found
PRINT,TGS_POS
ENDREP UNTIL  TGS_POS NE -1 
TGS_MODE = STRMID(HEADER,TGS_POS+12,3)		;extract "ON/OFF" substring

;get data
CASE 1 OF 
    TGS_MODE EQ 'ON ' : BEGIN			;TGS ON => 6 FSR's, UFSR, URSR, MFSR, MRSR,
      DATA = FLTARR(32,15)			;          MPF,MPR, and 3 RSRs
      TEMP = FLTARR(15)
      NORM_DATA = FLTARR(32,15)
           
      WHILE NOT EOF(1) DO BEGIN
	ON_IOERROR, GO_ON		;skip over header information
	READF,1, SR,YR,DOY,HR,MIN,SEC,BITRATE,V_STEP,EPQ,TEMP, $
	FORMAT = '(1X,I8,1X,I2,1X,I3,1X,I2,1X,I2.2,1X,I2.2,1X,A1,1X,I2,1X,F8.3,15(1X,I7))'
        LABEL_ONE:
        DATA(*,*) = 0  				;initialize matrix to 0
	DVS_HITS(*) = 0 
        VOLTAGE_STEPS = 0
        SR_INTERVAL = SR + INTERVAL

        REPEAT BEGIN
           IF (HR+MIN+SEC NE 0) THEN $
              IF TOTAL(TEMP) NE 0 THEN BEGIN
                HR_LAST = HR 
                MIN_LAST = MIN
                SEC_LAST = SEC
	        TIME_LAST = HR*3600 + MIN*60 + SEC
                DVS_HITS(V_STEP) = DVS_HITS(V_STEP) +1.0   ;count # of hits for each Voltage Step
                DATA(V_STEP,*) = DATA(V_STEP,*) + TEMP     ;sum rates for given voltage step
              ENDIF
           READF,1, SR,YR,DOY,HR,MIN,SEC,BITRATE,V_STEP,EPQ,TEMP, $
           FORMAT='(1X,I8,1X,I2,1X,I3,1X,I2,1X,I2.2,1X,I2.2,1X,A1,1X,I2,1X,F8.3,15(1X,I7))'
  	ENDREP  UNTIL (SR GE SR_INTERVAL) OR EOF(1) 
        N = N + 1
        FOR I = 0,31 DO $
           IF DVS_HITS(I) GT 0 THEN VOLTAGE_STEPS = VOLTAGE_STEPS + 1.0	;count # of voltage steps used
        DVS_HITS = DVS_HITS > 1			;convert 0 hits to 1 to prevent dividing by 0
        FOR I = 0, 31 DO NORM_DATA(I,*) = DATA(I,*) / DVS_HITS(I)
        INTERVAL_END_TIME(N) = TIME_LAST
        IF BITRATE EQ 'L' THEN NORM_DATA = NORM_DATA/2.0	;rates are summed over two spins
       								;in Low Bit Rate so divide by 2 for counts/spin
        FSR12(N) = TOTAL(NORM_DATA(*,0:1))/VOLTAGE_STEPS	;average over voltage steps 
        FSR34(N) = TOTAL(NORM_DATA(*,2:3))/VOLTAGE_STEPS
        FSR56(N) = TOTAL(NORM_DATA(*,4:5))/VOLTAGE_STEPS

;        PRINTF,2, YR,DOY,HR_LAST,MIN_LAST,SEC_LAST,$
;                  FSR12(N),FSR34(N),FSR56(N), FORMAT='(2(I5),I4,":",I2.2,":",I2.2,3I10)'
        IF SR GT SR_INTERVAL THEN BEGIN              		;data gaps
           N = N + 1
           INTERVAL_END_TIME(N) = TIME_LAST + 3
           FSR12(N) = 8.0E6
           FSR34(N) = 8.0E6
           FSR56(N) = 8.0E6
;           PRINTF,2, YR,DOY,HR_LAST,MIN_LAST,SEC_LAST+3,$
;                  FSR12(N),FSR34(N),FSR56(N), FORMAT='(2(I5),I4,":",I2.2,":",I2.2,3I10)'

        ENDIF
        PRINT, INTERVAL_END_TIME(N), VOLTAGE_STEPS, FORMAT = '(I8.5,I10)'
       	IF NOT EOF(1) THEN GOTO, LABEL_ONE
	GO_ON:
      ENDWHILE
      PRINT, 'TGS mode is ON'
;      SUM_FSRS = FSR12 + FSR34 + FSR56

    END

    TGS_MODE EQ 'OFF' : BEGIN			;TGS OFF =>3 FSR's and 3 RSR's
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
        VOLTAGE_STEPS = 0
        SR_INTERVAL = SR + INTERVAL

        REPEAT BEGIN
           IF (HR+MIN+SEC NE 0) THEN $
              IF TOTAL(TEMP) NE 0 THEN BEGIN
                HR_LAST = HR 
                MIN_LAST = MIN
                SEC_LAST = SEC
	        TIME_LAST = HR*3600 + MIN*60 + SEC
                DVS_HITS(V_STEP) = DVS_HITS(V_STEP) + 1.0  ;count # of hits for each Voltage Step
                DATA(V_STEP,*) = DATA(V_STEP,*) + TEMP
              ENDIF
           READF,1, SR,YR,DOY,HR,MIN,SEC,BITRATE,V_STEP,EPQ,TEMP, $
           FORMAT='(1X,I8,1X,I2,1X,I3,1X,I2,2(1X,I2.2),1X,A1,1X,I2,1X,F8.3,3(1X,I7),72X,3(1X,I7))'
  	ENDREP  UNTIL (SR GE SR_INTERVAL) OR EOF(1) 
        N = N + 1
        FOR I = 0,31 DO $
           IF DVS_HITS(I) GT 0 THEN VOLTAGE_STEPS = VOLTAGE_STEPS + 1.0	;count # of voltage steps used
        DVS_HITS = DVS_HITS > 1			;convert 0 hits to 1 to prevent dividing by 0
         					;in normalizing data
        FOR I = 0, 31 DO NORM_DATA(I,*) = DATA(I,*) / DVS_HITS(I)
        INTERVAL_END_TIME(N) = TIME_LAST
        IF BITRATE EQ 'L' THEN NORM_DATA = NORM_DATA/2.0	;rates are summed over two spins
								;in Low Bit Rate so divide by 2 for counts/spin
        FSR12(N) = TOTAL(NORM_DATA(*,0))/VOLTAGE_STEPS		;average over voltage steps
        FSR34(N) = TOTAL(NORM_DATA(*,1))/VOLTAGE_STEPS
        FSR56(N) = TOTAL(NORM_DATA(*,2))/VOLTAGE_STEPS

;        PRINTF,2, YR,DOY,HR_LAST,MIN_LAST,SEC_LAST,$
;                  FSR12(N),FSR34(N),FSR56(N), FORMAT='(2(I5),I4,":",I2.2,":",I2.2,3I10)'

        IF SR GT SR_INTERVAL THEN BEGIN              		;data gaps
           N = N + 1
           INTERVAL_END_TIME(N) = TIME_LAST + 3
           FSR12(N) = 8.0E6
           FSR34(N) = 8.0E6
           FSR56(N) = 8.0E6

;           PRINTF,2, YR,DOY,HR_LAST,MIN_LAST,SEC_LAST+3,$
;                  FSR12(N),FSR34(N),FSR56(N), FORMAT='(2(I5),I4,":",I2.2,":",I2.2,3I10)'
        ENDIF
        PRINT, INTERVAL_END_TIME(N),VOLTAGE_STEPS, FORMAT = '(I8.4,I10)'
       	IF NOT EOF(1) THEN GOTO, LABEL_TWO
        SKIP_LINE:
      ENDWHILE
      PRINT, 'TGS mode is OFF'
;      SUM_FSRS = FSR12 + FSR34 + FSR56
;      SUM_FSRS(WHERE(SUM_FSRS EQ 0)) = 5.0E6 	;set missing data points to 3 million
    END
    ELSE : PRINT, 'TGS MODE IS NOT DEFINED'
ENDCASE

;make plots
;    PLOT_IO,INTERVAL_END_TIME, SUM_FSRS,  $		
;      MAX_VALUE = 3.0E6,              $		;do not plot values greater than 3,000,000
;      XRANGE = [0,86400], XSTYLE = 1, $
;      YRANGE = [Y_LOW,Y_HIGH],YSTYLE = 1, $
;      XTICKS = 6, XMINOR = 8,         $
;      XTICKLEN = 0.08,		      $
;      XTICKFORMAT = 'YTEST'  ,        $
;      XTITLE = 'TIME',		      $
;      YTITLE = 'SUM FSRS',         $
;      POSITION = [0.1,0.1,0.9,0.3]

    PLOT_IO,INTERVAL_END_TIME, FSR56,  $		
      MAX_VALUE = 3.0E6,              $		;do not plot values greater than 3,000,000
      XRANGE = [0,86400], XSTYLE = 1, $
      YRANGE = [Y_LOW,Y_HIGH],YSTYLE = 1, $
      XTICKS = 6, XMINOR = 8,         $
      XTICKLEN = 0.08,		      $
      XTICKFORMAT = 'YTEST'  ,        $
      XTITLE = 'TIME',			$
      YTITLE = 'FSR56',			$
      POSITION = [0.1,0.1,0.9,0.4]

    PLOT_IO,INTERVAL_END_TIME, FSR34,  $		
      MAX_VALUE = 3.0E6,              $		;do not plot values greater than 3,000,000
      XRANGE = [0,86400], XSTYLE = 1, $
      YRANGE = [Y_LOW,Y_HIGH],YSTYLE = 1, $
      XTICKS = 6, XMINOR = 8,         $
      XTICKLEN = 0.08,		      $
      XTICKFORMAT = 'NOTICKS'  ,        $
      YTITLE = 'FSR34',$
      POSITION = [0.1,0.4,0.9,0.7],/NOERASE

    PLOT_IO,INTERVAL_END_TIME, FSR12,  $		
      MAX_VALUE = 3.0E6,              $		;do not plot values greater than 3,000,000
      XRANGE = [0,86400], XSTYLE = 1, $
      YRANGE = [Y_LOW,Y_HIGH],YSTYLE = 1, $
      XTICKS = 6, XMINOR = 8,         $
      XTICKLEN = 0.08,		      $
      XTICKFORMAT = 'NOTICKS',        $
      TITLE = MAIN_TITLE,             $
      YTITLE = 'FSR12',	$
      POSITION = [0.1,0.7,0.9,1.0],/NOERASE

CLOSE, 1
;CLOSE,2
END

