;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;PROGRAM DAILYRATEPLT.PRO
;This program reads an input file, which is generated from WSTICS_RATES, that consists of time,
;solar wind speed and 12 matrix rates (default list). These rates are:
;	0. H		1. He+2		2. He+1
;	3. BR0		4. BR1		5. BR2
;	6. FSR12	7. FSR34	8. FSR56
;	9. O+1		10.O+6		11.O+7
;
;Trend plots are then made with the rate data which can either be displayed on the screen or
;converted to PNG or Postscript files.
;		
;Created   19-OCT-1995		KANCHAM CHOTOO
;
;This version:	1.0	19-OCT-1995
;
;Revision History:
;	 8-FEB-1995	K.CHOTOO  	Plots are now in color and can either be GIF or Postcript
;					files.	
;	20-FEB-1996	K.CHOTOO	TEK Screen can now display color
;	 9-APR-1996	K.CHOTOO	Gif files are now written to UMSMS::SMS1:[CHOTOO.WWW.STICS]
;	15-APR-1996	K.CHOTOO	Input files now has both proton and alpha speeds.
;	 1-SEP-2005	L.Bleau 	Use PNG files instad of GIF file; write to SMS1:[WIND.WWW.STICS]
;	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
COMMON COLORS, R_ORIG, G_ORIG, B_ORIG, R_CURR, G_CURR, B_CURR
FILENAME = ' '
BITRATE  = ' '
HEADER = ' ' 
INTERVAL_END_TIME = FLTARR(480)
INTERVAL_END_TIME(*) = 0
H_SPEED = FLTARR(480)
HE2_SPEED = FLTARR(480)
H_SPEED(*) = 0
HE2_SPEED(*) = 0
RATE_DATA = FLTARR(480,12)	  
RATE_DATA(*,*) = 0
TEMP = FLTARR(12)

N = -1   			

PRINT, 'Enter name of input data file'
READ, FILENAME

display_type = '1'
PRINT, 'Do you want plot go to PNG file (1), or Postscript file (2),'
PRINT, 'or displayed on screen (3)? /1/'
READ, display_type
IF display_type EQ '1' THEN   SET_PLOT, 'Z'
IF display_type EQ '2' THEN BEGIN
  SET_PLOT,'PS'
;
; Makes color plot 7 x 9 inches starting (1in,1in) from bottom left corner
;
   DEVICE, /PORTRAIT, /INCHES, XSIZE=7.0, YSIZE=9.0, XOFF=1.0, YOFF=1.0 ;, /COLOR
ENDIF

 
OPENR, 1, FILENAME

;get date
REPEAT BEGIN		
READF,1, HEADER					;read header line by line until "WI_L1_SMS_"
DATE_POS= STRPOS(HEADER, 'WI_L1_SMS_')		;is found
PRINT, DATE_POS
ENDREP UNTIL  DATE_POS NE -1 
DATE_OF_FILE = STRMID(HEADER,DATE_POS+10,8)		;extract DATE substring
PRINT, DATE_OF_FILE

;DPPS stepping change in E/Q range on 1/6/95
IF LONG(DATE_OF_FILE) LT 19950106 THEN EPQ_RANGE = ' 8-223 ' ELSE EPQ_RANGE=' 6-200 '

MAIN_TITLE = '!17WIND/SMS/STICS  ' + DATE_OF_FILE + $
             '!C!Daverage counts/spin over' + EPQ_RANGE + 'keV/e (raw counts)!N' 


DEFAULT_DIR = 'SMS1:[WIND.WWW.STICS]'
OUT_PNG_FILE = DEFAULT_DIR + 'STICS_' + DATE_OF_FILE + '.PNG'

WHILE NOT EOF(1) DO BEGIN
    ON_IOERROR, GO_ON		;skip over header information
    READF,1, YR,DOY,HR,MIN,SEC,SPEED1,SPEED2,TEMP, $
    FORMAT = '(1X,I4,1X,I3,1X,I2,1X,I2.2,1X,I2.2,2X,F6.1,4X,F6.1,2X,12(2X,E13.7))'
    IF (YR+DOY+HR+MIN+SEC) GT 0 THEN BEGIN
       N=N+1
       INTERVAL_END_TIME(N) = HR*3600.0 + MIN*60.0 + SEC
       IF SPEED1 EQ 0 THEN SPEED1 = 9999				;set 0's to value above max value
       IF SPEED2 EQ 0 THEN SPEED2 = 9999				;set 0's to value above max value
       H_SPEED(N) = SPEED1
       HE2_SPEED(N) = SPEED2
       RATE_DATA(N,*) = TEMP
    ENDIF
    GO_ON:
ENDWHILE
      
H_SPEED = H_SPEED(0:N)			;delete unused array slots
HE2_SPEED = HE2_SPEED(0:N)
RATE_DATA = RATE_DATA(0:N,*)
INTERVAL_END_TIME = INTERVAL_END_TIME(0:N)

INDEX = WHERE(RATE_DATA EQ 0.0,COUNT)			;Determine how many bad values and their subscripts
print,count
IF (COUNT GT 0) THEN RATE_DATA(INDEX)= 2.0E+06       	;Set bad values to greater than maximum plotting value


;set up color table. For "TEK 4100" screens, color(0)=color(1)=white
;        0,  1,  2,  3,  4,  5,  6,  7,  8,  9  => color index
;       -----------------------------------------
RED   = [0,255,255,  0,  0,255,  0,255,  0,255];Specify the red component of each color
GREEN = [0,255,  0,255,  0,255,255,  0,  0,255];  "      "  green  "      "    "   "
BLUE  = [0,255,  0,  0,255,  0,255,255,  0,255];  "      "  blue   "      "    "   "
 
TVLCT,RED,GREEN,BLUE       	;Load the first 10 elements of the color table

; make plot
;         Solar Wind Speed
PLOT,INTERVAL_END_TIME,H_SPEED, $
      BACKGROUND=9,COLOR=8,$
      MAX_VALUE = 9990, $       		;values greater than max_value will not be plotted
      XRANGE = [0,86400], XSTYLE = 1, $
      YRANGE = [200,800],YSTYLE = 1, $
      YTICKS = 3, YMINOR=4,		$
      XTICKS = 6, XMINOR = 8,         $
      XTICKLEN = 0.04,		      $
      XTICKFORMAT = 'NOTICKS'  ,        $
      TITLE = MAIN_TITLE,		$
      YTITLE = 'SPEED (km/s)',		$
      POSITION = [0.15,0.70,0.85,0.85]
  XYOUTS,87000,500,'- SW H!U+!N!CSMS/MASS',COLOR=8

;         FSRs  
PLOT_IO,INTERVAL_END_TIME,RATE_DATA(*,7),  $			;FSR34 (solid line)
      BACKGROUND=9,COLOR=8,$
      MAX_VALUE = 1.0E06,              $	      
      XRANGE = [0,86400], XSTYLE = 1, $
      YRANGE = [10,2000],YSTYLE = 1, $
      XTICKS = 6, XMINOR = 8,         $
      XTICKLEN = 0.04,		      $
      XTICKFORMAT = 'NOTICKS'  ,        $
      YTITLE = 'FSRs',$
      POSITION = [0.15,0.55,0.85,0.70],/NOERASE
  OPLOT,INTERVAL_END_TIME,RATE_DATA(*,6),MAX_VALUE=1.0E06,COLOR=2	;FSR12 (solid line)  
  OPLOT,INTERVAL_END_TIME,RATE_DATA(*,8),MAX_VALUE=1.0E06,COLOR=4	;FSR56 (solid line)  
  XYOUTS,87000,200,'- FSR34',COLOR=8
  XYOUTS,87000,100,'- FSR12',COLOR=2
  XYOUTS,87000,50,'- FSR56',COLOR=4
 
;         BRs
PLOT_IO,INTERVAL_END_TIME, RATE_DATA(*,4),$			;BR1 (-)
      BACKGROUND=9,COLOR=8,$
      MAX_VALUE = 1.0E06,              $		
      XRANGE = [0,86400], XSTYLE = 1, $
      YRANGE = [0.001,10.0],YSTYLE = 1, $
      XTICKS = 6, XMINOR = 8,         $
      XTICKLEN = 0.04,		      $
      XTICKFORMAT = 'NOTICKS'  ,        $
      YTITLE = 'BASIC RATE',$
      POSITION = [0.15,0.40,0.85,0.55],/NOERASE
  OPLOT,INTERVAL_END_TIME,RATE_DATA(*,3),MAX_VALUE=1.0E06,$
            PSYM=7, SYMSIZE=0.4,COLOR=3		;BR0 (x)  
  XYOUTS,87000,0.10,'x BR0',COLOR=3
  XYOUTS,87000,0.01,'- BR1',COLOR=8

;     H, He+2
PLOT_IO,INTERVAL_END_TIME,RATE_DATA(*,0),  $		        ;H+1 (solid line)  
      BACKGROUND=9,COLOR=8,$
      MAX_VALUE = 1.0E06,              $		
      XRANGE = [0,86400], XSTYLE = 1, $
      YRANGE = [0.001,500],YSTYLE = 1, $
      XTICKS = 6, XMINOR = 8,         $
      XTICKLEN = 0.04,		      $
      XTICKFORMAT = 'NOTICKS'  ,        $
      YTITLE = 'H!U+!N, He!U+2!N',$
      POSITION = [0.15,0.25,0.85,0.40],/NOERASE
  OPLOT,INTERVAL_END_TIME,RATE_DATA(*,1),PSYM=2,SYMSIZE=0.4,$
      MAX_VALUE=1.0E06, COLOR=6		;He+2 (asterisks)
  XYOUTS,87000,1.0,'- H!U+!N',COLOR=8
  XYOUTS,87000,0.1,'* He!U+2!N',COLOR=6

;     He+1, O+1
PLOT_IO,INTERVAL_END_TIME,RATE_DATA(*,2),  $		        ;He+1 (solid line)  
      BACKGROUND=9,COLOR=8,$
      MAX_VALUE = 1.0E06,              $		
      XRANGE = [0,86400], XSTYLE = 1, $
      YRANGE = [0.001,10],YSTYLE = 1, $
      XTICKS = 6, XMINOR = 8,         $
      XTICKLEN = -0.04,		      $
      XTITLE = 'TIME',		      $
      XTICKFORMAT = 'YTEST',        $
      YTITLE = 'He!U+1!N, O!U+1!N',	$
      POSITION = [0.15,0.1,0.85,0.25],/NOERASE
  OPLOT,INTERVAL_END_TIME,RATE_DATA(*,9), PSYM=2, $
      MAX_VALUE=1.0E06, COLOR=7				;O+1 (asterisks)
  XYOUTS,87000,1.0,'- He!U+1!N!C (T&DCR)',COLOR=8
  XYOUTS,87000,0.01,'* O!U+1!N!C (TCR)'  ,COLOR=7

CLOSE, 1

R_CURR = [RED, BYTARR(246) + 255] 		;first 10 defined above
G_CURR = [GREEN, BYTARR(246) + 255]        	;remaining 246 colors are all white
B_CURR = [BLUE, BYTARR(246) + 255]

; Create PNG file
IF display_type EQ '1' THEN BEGIN
    IMAGE = TVRD()
    PRINT, 'Writing plot to ',OUT_PNG_FILE
    WRITE_PNG, OUT_PNG_FILE, IMAGE, R_CURR, G_CURR, B_CURR
ENDIF

END
