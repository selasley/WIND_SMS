;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PROGRAM STICS_SCATTER.PRO
;This program reads a STICS matrix file with mass and mass/charge and makes a histogram
;of Counts vs Mass/charge for Mass=0. It also reads a table listing file for mass vs
;mass/charge and creates a scatter plot of Mass against Mass/charge on the same page.
;
;Created 	 6-APR-1995	KANCHAM CHOTOO 
;
;Routine used:	STICS_MTX.PRO	KANCHAM CHOTOO AND CHRISTINA COHEN
;
;This version:  1.05	1-SEP-2005
;
;Revision History:
;	11-APR-1995	K. CHOTOO	Include Basic Rate boxes in plots; Commented
;					out file of Rate listings.
;	13-APR-1995	K. CHOTOO	Increased plot size by changing x-position from
;					0.2 to 0.1.
;	 9-FEB-1996	K. CHOTOO	Added BR1 and BR2 labels to histogram plot.
;     	31-MAY-1996	K. CHOTOO	Start and end times, including dates, obtained
;					from matrix file header.
;     	 1-SEP-2005	L. Bleau 	Add display device option 3 to make a PNG file; change
;                                       GIF to PNG
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
PRO BOXES
;This procedure plots the DPU defined matrix boxes
OPLOT, [0.9,0.9,1.26,1.26,0.9],$	        ;H+1
       [0.5,2.13,2.13,0.5,0.5]		
OPLOT, [1.71,1.71,2.32,2.32,1.71],$		;He+2
       [1.77,6.89,6.89,1.77,1.77]
OPLOT, [3.55,3.55,4.67,4.67,3.55],$		;He+1
       [1.77,6.89,6.89,1.77,1.77]
OPLOT, [1.87,1.87,2.18,2.18,1.87],$		;C+6
       [9.04,12.99,12.99,9.04,9.04]
OPLOT, [2.25,2.25,2.54,2.54,2.25],$		;C+5
       [9.04,12.99,12.99,9.04,9.04]
OPLOT, [2.87,2.87,3.24,3.24,2.87],$		;C+4
       [9.04,12.99,12.99,9.04,9.04]
OPLOT, [2.11,2.11,2.46,2.46,2.11],$		;O+7
       [14.21,20.41,20.41,14.21,14.21]
OPLOT, [2.54,2.54,2.87,2.87,2.54],$		;O+6
       [10.83,20.41,20.41,10.83,10.83]
OPLOT, [14.89,14.89,20.20,20.20,14.89],$	;O+1
       [4.80,79.30,79.30,4.80,4.80]
OPLOT, [2.32,2.32,2.54,2.54,2.32],$		;Ne+8
       [17.03,22.34,22.34,17.03,17.03]
OPLOT, [2.25,2.25,2.54,2.54,2.25],$		;Mg+10
       [22.34,26.77,26.77,22.34,22.34]
OPLOT, [2.87,2.87,3.24,3.24,2.87],$		;Mg+8
       [22.34,26.77,26.77,22.34,22.34]
OPLOT, [2.18,2.18,2.39,2.39,2.18],$		;Si+12
       [26.77,38.44,38.44,26.77,26.77]
OPLOT, [2.96,2.96,3.24,3.24,2.96],$		;Si+9
       [26.77,38.44,38.44,26.77,26.77]
OPLOT, [3.24,3.24,3.66,3.66,3.24],$		;Si+8
       [26.77,38.44,38.44,26.77,26.77]
OPLOT, [3.34,3.34,3.55,3.55,3.34],$		;Fe+16
       [38.45,79.28,79.28,38.45,38.45]
OPLOT, [3.77,3.77,4.14,4.14,3.77],$		;Fe+14
       [38.45,79.28,79.28,38.45,38.45]
OPLOT, [4.40,4.40,4.82,4.82,4.40],$		;Fe+12
       [38.45,79.28,79.28,38.45,38.45]
OPLOT, [4.82,4.82,5.28,5.28,4.82],$		;Fe+11
       [38.45,79.28,79.28,38.45,38.45]
OPLOT, [5.28,5.28,5.78,5.78,5.28],$		;Fe+10
       [38.45,79.28,79.28,38.45,38.45]
OPLOT, [5.78,5.78,6.53,6.53,5.78],$		;Fe+9
       [38.45,79.28,79.28,38.45,38.45]
OPLOT, [6.53,6.53,7.38,7.38,6.53],$		;Fe+8
       [38.45,79.28,79.28,38.45,38.45]
RETURN
END

PRO BASIC_RATE_BOXES
;
;This procedure plots the Basic Rate Boxes on the scatter plot of M vs M/Q
;
OPLOT, [1.22,1.22,10.98,10.98,1.22],$		;BR0
       [9.04,95.0, 95.0, 9.04,9.04],LINESTYLE=2
XYOUTS, 8.0, 10.0, '!17BR0!X'
OPLOT, [10.98,10.98,42.0,42.0,10.98],$		;BR1
       [  0.5, 95.0,95.0, 0.5,0.5],LINESTYLE=2
XYOUTS, 30.0, 0.6, '!17BR1!X'
OPLOT, [0.9, 0.9,10.98,10.98,0.9],$		;BR2
       [0.5,95.0,95.0,0.5,0.5], LINESTYLE=2
XYOUTS, 7.0, 0.6, '!17BR2!X'
RETURN
END

  
FUNCTION NOTICKS, axis, index, value
;    this function return blanks for x tick labels
temp = ' '
RETURN, temp
END

PRO GET_MMQ, FILENAME, MPQ_DATA,MASS_DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This routine reads a table listing of Mass vs Mass/charge (STICS_PHA_(date)_S.TBL)
;and returns an array of mass data and one of mass/charge data to the main program.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MPQ =  FLTARR(5000)				;temporary array for mass/charge
MASS = FLTARR(5000)				;temporary array for mass
MPQ_DATA = FLTARR(5000)				;final mass/charge data
MASS_DATA = FLTARR(5000)			;final mass data
I=0						;array counter
J=0						;counts total number of events
FIRST = 'FIRST'
OPENR, 2, FILENAME
WHILE NOT EOF(2) DO BEGIN
  ON_IOERROR, GO_ON
  READF,2,TEMP1,TEMP2, FORMAT = '(124X,F11.2,F8.1)'
  MPQ(I)  = TEMP1
  MASS(I) = TEMP2
  I = I + 1
  J = J + 1
;
; If temporary arrays are full, dump data into final array for mass and mass/charge
;
  IF I EQ 5000 THEN BEGIN			
    IF FIRST EQ 'FIRST' THEN BEGIN		;if first dump
       FIRST = ' '
       MPQ_DATA  = MPQ
       MASS_DATA = MASS
    ENDIF ELSE BEGIN				;if second or more dump, 
       MPQ_DATA  = [MPQ_DATA,MPQ]		;concatenate arrays
       MASS_DATA = [MASS_DATA,MASS]
    ENDELSE
    I = 0					;set I back to 0
    MPQ(*) = 0					;set mass/charge elements to 0
    MASS(*) = 0					;set mass elements to 0
  ENDIF
  GO_ON:
ENDWHILE
CLOSE, 2

IF I EQ 0 THEN I = I + 1			;to avoid I = -1 in following lines

IF MAX(MPQ_DATA) EQ 0 THEN BEGIN		;if I never reached 5000
   MPQ_DATA  = MPQ(0:I-1)
   MASS_DATA = MASS(0:I-1)
ENDIF ELSE BEGIN				;if more than 5000 events
   MPQ_DATA  = [MPQ_DATA,MPQ(0:I-1)]
   MASS_DATA = [MASS_DATA,MASS(0:I-1)]
ENDELSE  
RETURN
END


;******************************************************************************************
;             **start main program**
;******************************************************************************************
filename1 = ' '
filename2 = ' '
header = ' '
PRINT, ' Enter name of MATRIX data file'
READ, filename1
PRINT, ' Enter name of PHA table data file'
READ, filename2

display_type = '1'
PRINT, 'Do you want plot go to PNG file (1), or Postscript file (2),'
PRINT, 'or displayed on screen (3)? /1/'
READ, display_type, format = '(a1)'
IF display_type EQ ' ' THEN display_type = '1'

;
; If PostScript, make plot 6 x 8 inches starting (1in,1in) from bottom left corner
;
IF display_type EQ '1' THEN BEGIN
   SET_PLOT, 'PS'	;make postcript file
   DEVICE, /PORTRAIT, /INCHES, XSIZE=6.0, YSIZE=8.0, XOFF=1.5, YOFF=2.0
ENDIF
;
; If PNG, use virtual device (Z buffer)
;
IF display_type EQ '3' THEN BEGIN
   SET_PLOT, 'Z'	;make PNG file
ENDIF
box_reply = 'Y'
PRINT, 'Do you want matrix boxes to appear on plot (Y/N) ? /Y/'
READ, box_reply


OPENR, 1, filename1

;************************* GET TIME PERIOD FROM MATRIX HEADER **********************************
;get date from L1 filename 
REPEAT BEGIN		
READF,1, HEADER					;read header line by line until "WI_L1_SMS_"
DATE_POS= STRPOS(HEADER, 'WI_L1_SMS_')		;is found
PRINT, DATE_POS
ENDREP UNTIL  DATE_POS NE -1 
DATE_OF_FILE = STRMID(HEADER,DATE_POS+10,8)	;extract DATE substring

;get Start and End times
REPEAT BEGIN		
READF,1, HEADER					;read header line by line until "Start Time '"
STIME_POS= STRPOS(HEADER, 'Start Time:')	;is found
PRINT, STIME_POS
ENDREP UNTIL  STIME_POS NE -1 
START_DATE = STRMID(HEADER,STIME_POS+13,8)    	;extract Start date substring
START_TIME = STRMID(HEADER,STIME_POS+23,8)	;extract Start time substring
PRINT,START_TIME,'  ',START_DATE

ETIME_POS = STRPOS(HEADER, 'End Time:')		;get End Time pos
END_DATE  = STRMID(HEADER,ETIME_POS+11,8)
END_TIME  = STRMID(HEADER,ETIME_POS+21,8)
PRINT,END_TIME,'  ',END_DATE

IF(LONG(START_DATE) LT 0) THEN TIME_PERIOD = DATE_OF_FILE+'  '+'00:00:00 - 24:00:00' $
ELSE IF(START_DATE EQ END_DATE) THEN TIME_PERIOD = START_DATE+'  '+START_TIME+' - '+END_TIME $
ELSE TIME_PERIOD = START_DATE+'  '+START_TIME+' - '+END_DATE+'  '+END_TIME
	
;**************************************************************************************************
;PRINT, ' Enter number of mass bins'
;READ, y_param
y_param = 59					;Number of DPU mass bins
;PRINT, ' Enter number of m/q bins'
;READ, x_param
x_param = 127					;Number of DPU m/q bins
y_data = FLTARR(y_param)
x_data = FLTARR(x_param)
data = FLTARR(x_param,y_param)
temp3 = FLTARR(y_param)
WHILE 1 DO BEGIN                                 ;read headers
     ON_IOERROR, go_on
     READF, 1, y_data
     GOTO, next_read                             ;got y data move on
     go_on:
ENDWHILE
next_read:
FOR i = 1, x_param DO BEGIN
     READF, 1, temp1, temp2, temp3, FORMAT = '(3x,i3,2x,f5.2,500(1x,i5))'
     x_data(i-1) = temp2
     data(i-1,*) = temp3
ENDFOR
CLOSE, 1


GET_MMQ, FILENAME2, PHA_MPQ, PHA_MASS

MAIN_TITLE ='!17WIND/SMS/STICS  raw counts!C' + TIME_PERIOD + ' UT!X'
           
;
; Create own plotting symbol{circle}
;
A = FINDGEN(16) * (!PI*3/24.0)		;Make a vector of 16 points,
					;A(i) = 3(pi)i/24
USERSYM, COS(A), SIN(A)
;
; Make scatter plot of mass vs mass/charge
;              
PLOT, PHA_MPQ, PHA_MASS,$
     PSYM=8, SYMSIZE=0.2,$	       ;user defined symbols, circles one-fifth size
     /XTYPE,$                          ;log scale
     /YTYPE,$                          ;log scale
     TITLE = MAIN_TITLE,$
     YTITLE='MASS (amu)',$
     XRANGE = [0.5,50], XSTYLE = 1 ,$
     YRANGE = [.1,100],YSTYLE = 1, $
     XTICKFORMAT = 'noticks',$         ;don't label ticks
     POSITION = [.1,.3,.9,.9]

BASIC_RATE_BOXES
;
; Display DPU defined matrix rate boxes if requested by user
;
IF box_reply NE 'N' AND box_reply NE 'n' THEN BOXES 


XYOUTS, 0.75,0.9,'!17H!U+!X'
XYOUTS, 1.2,3,'!17He!U+2!X'
XYOUTS, 5.0,3,'!17He!U+!X'
XYOUTS, 22,16,'!17O!U+!X'
XYOUTS, 1.4,10,'!17C!X'
XYOUTS, 1.4,15,'!17O!X'
XYOUTS, 1.4,22,'!17Mg!X'
XYOUTS, 1.4,29,'!17Si!X'
XYOUTS, 1.4,50,'!17Fe!X'

;
; Make histogram of counts vs mass/charge for mass=0
;
PLOT, x_data, data(*,0), $              
     /XTYPE,$
     /YTYPE,$
     XTITLE='MASS/CHARGE (amu/e)',$
     YTITLE='COUNTS',$
     XRANGE = [0.5,50], XSTYLE = 1,$
     YRANGE = [1,40000], YSTYLE = 1, $
     PSYM=10,$                         ;histogram mode
     XTICKLEN = 0.06,$
     POSITION = [.1,.1,.9,.3], $
     /NOERASE                          ;don't erase scatter plot

OPLOT,[0.9,0.9],[1.0,90000]     	;indicate lower m/q boundary at 0.9
                                        ;and lower M=0 H+ boundary
OPLOT,[1.22,1.22],[1.0,90000]	;indicate upper M=0 H+ boundary at 1.22
OPLOT,[1.61,1.61],[1.0,90000]	;indicate lower M=0 He+2 boundary at 1.61
OPLOT,[2.32,2.32],[1.0,90000]	;indicate upper M=0 He+2 boundary at 2.32
OPLOT,[3.55,3.55],[1.0,90000]	;indicate lower M=0 He+ boundary at 3.55
OPLOT,[4.67,4.67],[1.0,90000]	;indicate upper M=0 He+ boundary at 4.67
OPLOT,[10.98,10.98],$			;indicate BR boundary at m/q=10.98
      [  1.0,90000], LINESTYLE = 2                                            
OPLOT,[42.0,42.0],[1.0,20000],LINESTYLE=2	;upper m/q boundary ends at 42
XYOUTS, 18,4000,'!17"mass zero"!X',CHARSIZE=0.8
XYOUTS, 12,1000,'!17BR1!X'
XYOUTS,  8,1000,'!17BR2!X'
;
; If PNG device we'd ordinarily read the Z buffer and write it to disk.
; Don't do anything here, though, since this procedure is intended to be followed
; immediately by another IDL procedure, which does do a write.
; If a PostScript or X windows device, the plotting ccommands have already been
; written to the file or drawn, so we're okay.
;
END
