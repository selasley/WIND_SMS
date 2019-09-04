;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PROGRAM STICS_SCATTER.PRO
;This program reads a STICS matrix file with mass and mass/charge and makes a histogram
;of Counts vs Mass/charge for Mass=0. It also reads a table listing file for mass vs
;mass/charge and creates a scatter plot of Mass against Mass/charge on the same page.
;The program also creates a file listing the number of counts for each Matrix Rate
;with output file name "MR_PHA_(date)". There is an option for displaying the matrix
;rate boxes on the Mass vs Mass/charge plot.
;
;Created 	 6-APR-1995	KANCHAM CHOTOO 
;
;Routine used:	STICS_MTX.PRO	KANCHAM CHOTOO AND CHRISTINA COHEN
;
;This version:  1.02	13-APR-1995
;
;Revision History:
;	11-APR-1995	K. CHOTOO	Include Basic Rate boxes in plots; Commented
;					out file of Rate listings.
;	13-APR-1995	K. CHOTOO	Increased plot size by changing x-position from
;					0.2 to 0.1.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO DPU_MATRIX, MMQ_MATRIX, FDATE
;this procedure calculates the MATRIX RATES as defined
;by the DPU
OMR  = FLTARR(31)
MR_IONS = STRARR(31)
;MR_FILENAME = 'STICS:[WIND.TABLE]PHA_MR_' + FDATE + '.LIS' ;output filename
;OPENW, 2, MR_FILENAME
MR_IONS = ['He+1','C+6','C+5','C+4',$
           'O+7','O+6','O+1','Ne+8',$
           'Mg+10','Mg+8','Si+12',  $
           'Si+9','Si+8','Fe+16',   $
           'Fe+14','Fe+12','Fe+11', $
           'Fe+10','Fe+9','Fe+8',   $
           '  HMR            He+2', $
           '  SMR             H+1', $ 
           '  BR0  m>9.04, m/q<11', $
           '  BR1          m/q>11', $
           '  BR2  m<9.04, m/q<11', $
           'H+1(m>0)','H+1(m=0)',   $
           'He+1(m>0)','He+1(m=0)', $
           'He+2(m>0)','He+2(m=0)'] 
         
BR0  = TOTAL(MMQ_MATRIX(11:82,33:58))           ;Heavies, m/q<11
BR1  = TOTAL(MMQ_MATRIX(83:126,0:58))           ;Heavies, m/q>11
BR2a = TOTAL(MMQ_MATRIX(1:10,0:58))             ;H,He,and mz heavies,m/q<11
BR2b = TOTAL(MMQ_MATRIX(11:82,0:32))
BR2  = BR2a + BR2b    
He1  = TOTAL(MMQ_MATRIX(46:54,20:28))		;TCRs
He1M0= TOTAL(MMQ_MATRIX(46:54,0))		;DCRs
H1   = TOTAL(MMQ_MATRIX(1:11,1:16))		;TCRs
H1M0 = TOTAL(MMQ_MATRIX(1:11,0))		;DCRs
He2  = TOTAL(MMQ_MATRIX(22:31,20:28))		;TCRs
He2M0= TOTAL(MMQ_MATRIX(20:31,0))		;DCRs
OMR(0)  = He1 + He1M0              		;He1
OMR(1)  = TOTAL(MMQ_MATRIX(25:29,33:36))        ;C+6
OMR(2)  = TOTAL(MMQ_MATRIX(31:34,33:36))        ;C+5
OMR(3)  = TOTAL(MMQ_MATRIX(39:42,33:36))  	;C+4
OMR(4)  = TOTAL(MMQ_MATRIX(29:33,38:41))  	;O+7
OMR(5)  = TOTAL(MMQ_MATRIX(35:38,35:41))	;O+6
OMR(6)  = TOTAL(MMQ_MATRIX(93:102,26:56))	;O+1
OMR(7)  = TOTAL(MMQ_MATRIX(32:34,40:42))	;Ne+8
OMR(8)  = TOTAL(MMQ_MATRIX(31:34,43:44))	;Mg+10
OMR(9)  = TOTAL(MMQ_MATRIX(39:42,43:44))	;Mg+8
OMR(10) = TOTAL(MMQ_MATRIX(30:32,45:48))	;Si+12
OMR(11) = TOTAL(MMQ_MATRIX(40:42,45:48))	;Si+9
OMR(12) = TOTAL(MMQ_MATRIX(43:46,45:48))	;Si+8
OMR(13) = TOTAL(MMQ_MATRIX(44:45,49:56))	;Fe+16
OMR(14) = TOTAL(MMQ_MATRIX(48:50,49:56))	;Fe+14
OMR(15) = TOTAL(MMQ_MATRIX(53:55,49:56))        ;Fe+12
OMR(16) = TOTAL(MMQ_MATRIX(56:58,49:56))	;Fe+11
OMR(17) = TOTAL(MMQ_MATRIX(59:61,49:56))	;Fe+10
OMR(18) = TOTAL(MMQ_MATRIX(62:65,49:56))	;Fe+9
OMR(19) = TOTAL(MMQ_MATRIX(66:69,49:56))	;Fe+8
OMR(20) = He2 + He2M0				;HMR
OMR(21) = H1 + H1M0				;SMR
OMR(22) = BR0
OMR(23) = BR1
OMR(24) = BR2
OMR(25) = H1
OMR(26) = H1M0
OMR(27) = He1
OMR(28) = He1M0
OMR(29) = He2
OMR(30) = He2M0

FOR I = 0, 19 DO PRINT,I, MR_IONS(I), OMR(I), $
                       FORMAT = '(I5,A16,I10)'
PRINT,' '
FOR I = 20, 30 DO PRINT, MR_IONS(I), OMR(I), $
                       FORMAT = '(A21,I10)'

;PRINTF,2, FORMAT = '(/,8X,"WIND/SMS/STICS",5X,A)', FDATE
;PRINTF,2, FORMAT = '(8X,"MATRIX RATES CALCULATED FROM PHA",/)'
;PRINTF,2, FORMAT = '(6X,"===================================")'  
;PRINTF,2, FORMAT = '(6X,"   OMR           ION      COUNTS ")'
;PRINTF,2, FORMAT = '(6X,"===================================")' 
;FOR I = 0, 19 DO PRINTF,2,I, MR_IONS(I), OMR(I), $
;                       FORMAT = '(6X,I5,A16,I10)'
;PRINTF,2, FORMAT = '(6X,"===================================")' 
;FOR I = 20, 24 DO PRINTF,2, MR_IONS(I), OMR(I), $
;                       FORMAT = '(6X,A21,I10)'
;PRINTF,2, FORMAT = '(6X,"===================================")' 
;FOR I = 25, 30 DO PRINTF,2, MR_IONS(I), OMR(I), $
;                       FORMAT = '(6X,A21,I10)'
;PRINTF,2, FORMAT = '(6X,"===================================")' 
;CLOSE, 2
RETURN
END
;
PRO BOXES
;This procedure plots the DPU defined matrix boxes
OPLOT, [0.9,0.9,1.26,1.26],$	        	;H+1
       [0.1,2.13,2.13,0.1]		
OPLOT, [1.71,1.71,2.32,2.32,1.71],$		;He+2
       [2.79,6.30,6.30,2.79,2.79]
OPLOT, [3.55,3.55,4.67,4.67,3.55],$		;He+1
       [2.79,6.30,6.30,2.79,2.79]		
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
XYOUTS, 1.25, 60.0, 'BR0'
OPLOT, [10.98,10.98,42.0,42.0],$		;BR1
       [  0.1, 95.0,95.0, 0.1],LINESTYLE=2
XYOUTS, 30.0, 60.0, 'BR1'
OPLOT, [0.9, 0.9,1.22],$			;BR2
       [0.1,95.0,95.0], LINESTYLE=2
XYOUTS, 7.0, 0.2, 'BR2'
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
PRINT, ' Enter name of data file'
READ, filename1
display_type = '2'
PRINT, 'Do you want plot go to poscript file (1)'
PRINT, 'or displayed on screen (2)? /2/'
READ, display_type
IF display_type EQ '1' THEN BEGIN
   SET_PLOT, 'PS'	;make postcript file
;
; Makes plot 7 x 9 inches starting (1in,1in) from bottom left corner
;
   DEVICE, /PORTRAIT, /INCHES, XSIZE=7.0, YSIZE=9.0, XOFF=1.0, YOFF=1.0
ENDIF
box_reply = 'N'
PRINT, 'Do you want matrix boxes to appear on plot (Y/N) ? /N/'
READ, box_reply

DATE_POS = STRPOS(FILENAME1,'19')               ;finds position of date in filename
DATE = STRMID(FILENAME1,DATE_POS,8)             ;extracts date from filename

FILENAME2 = 'STICS_PHA_' + DATE + '_S.TBL'	;Name of table listing file

OPENR, 1, filename1
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

DPU_MATRIX, data, DATE                  ;calculates and prints matrix rates
         				;to a file "PHA_MR_date.LIS"

GET_MMQ, FILENAME2, PHA_MPQ, PHA_MASS

MAIN_TITLE ='WIND/SMS/STICS   ' + DATE
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
     PSYM=8, SYMSIZE=0.2,$	       ;diamond plotting symbols, half-size
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
IF box_reply EQ 'Y' OR box_reply EQ 'y' THEN BOXES 
;
; Make histogram of counts vs mass/charge for mass=0
;
PLOT, x_data, data(*,0), $              
     /XTYPE,$
     /YTYPE,$
     XTITLE='MASS/CHARGE (amu/e)',$
     YTITLE='COUNTS',$
     XRANGE = [0.5,50], XSTYLE = 1,$
     YRANGE = [1,20000], YSTYLE = 1, $
     PSYM=10,$                         ;histogram mode
     XTICKLEN = 0.06,$
     POSITION = [.1,.1,.9,.3], $
     /NOERASE                          ;don't erase scatter plot

OPLOT,[0.9,0.9],[1.0,20000],LINESTYLE=2	;indicate lower m/q boundary at 0.9
OPLOT,[10.98,10.98],$			;indicate BR boundary at m/q=10.98
      [  1.0,20000], LINESTYLE = 2
OPLOT,[42.0,42.0],[1.0,20000],LINESTYLE=2	;upper m/q boundary ends at 42
XYOUTS, 20,4000,'MASS = 0'
;
END
