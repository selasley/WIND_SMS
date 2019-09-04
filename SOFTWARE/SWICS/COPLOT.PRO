FUNCTION NOTICKS, axis, index, value
;    this function return blanks for x tick labels
temp = ' '
RETURN, temp
END
;
FUNCTION XTEST, axis, index, value
;
yy = FIX(value)/365
doy = FIX(value-(yy*365))
temp = (value-(yy*365)-doy)
hour = FIX(temp*24)
min = FIX((temp-(hour/24.))*60*24)
IF min EQ 59 THEN BEGIN                     ;round to hour
     hour = hour + 1
     min = 0
ENDIF
RETURN, STRING(hour, min, FORMAT='(2i2.2)')
END
;
;start main program
;    this IDL program reads in time ordered data and creates a time trend
;    plot
;    cmsc 10 Jan 1995
;                                                for setting up color plots
COMMON COLORS, R_ORIG, G_ORIG, B_ORIG, R_CURR, G_CURR, B_CURR
;
param_num = INTARR(10,10)
param_plot = INTARR(10)
linestyle = INTARR(10,10)
symbol = INTARR(10,10)
color = INTARR(10,10)
answer = ''
XTICKNAME = ''
plot_title = STRARR(10)
PRINT, '  Do you want output to (1) screen or (2) printer?'
READ, out_type
IF out_type EQ 2 THEN BEGIN
     SET_PLOT, 'z'
ENDIF ELSE BEGIN
     SET_PLOT, 'tek'
     DEVICE, /TEK4100, COLORS=64
ENDELSE
plot_title(3) = 'SWICS/Wind'
frames = 4
parameters = 4
ticknumber = 12                          ;12 major tick marks
mticknumber = 6                          ;6 minor tick marks
high_point = 1e9
min_y = FLTARR(frames)
max_y = FLTARR(frames)
xy = FLTARR(4,frames)
loglin = STRARR(frames)
y_title = STRARR(frames)
x_title = ' Time (hours) '
loglin(3) = 'lin'                        ; lin vel plot
min_y(3) = 200.
max_y(3) = 1000.
y_title(3) = 'He2+ Vel (km/s)'
xy(*,3) = [0.1,0.7,0.9,0.9]
param_plot(3) = 1
param_num(3,0) = 0
linestyle(3,0) = 0
symbol(3,0) = 0
color(3,0) = 2
loglin(2) = 'lin'                        ; c/o plot
min_y(2) = 0
max_y(2) = 1.0
y_title(2) = 'C/O'
xy(*,2) = [0.1,0.5,0.9,0.7]
param_plot(2) = 1
param_num(2,0) = 1
linestyle(2,0) = 0
symbol(2,0) = 2
color(2,0) = 4
loglin(1) = 'lin'                        ; lin C t6 plot
min_y(1) = 0.
max_y(1) = 2.
y_title(1) = 'C Ion. T (MK)'
xy(*,1) = [0.1,0.3,0.9,0.5]
param_plot(1) = 1
param_num(1,0) = 2
linestyle(1,0) = 0
symbol(1,0) = 2
color(1,0) = 3
loglin(0) = 'lin'                        ; lin O t6 plot
min_y(0) = 1
max_y(0) = 3
y_title(0) = 'O Ion. T (MK)'
xy(*,0) = [0.1,0.1,0.9,0.3]
param_plot(0) = 1
param_num(0,0) = 3
linestyle(0,0) = 0
symbol(0,0) = 2
color(0,0) = 3
ypos_limit = min_y(0)-0.08*(max_y(0)-min_y(0))/(xy(3,0)-xy(1,0))
time_start = 0.
time_end = 6000.
temp = FLTARR(1,parameters)
temp1 = dblarr(1)
temp_data = FLTARR(5000,parameters)
temp_time = FLTARR(5000)
data = FLTARR(5000,parameters)
time = FLTARR(5000)
filename = ' '
outfilename = ' '
first = 'first'
PRINT, ' Enter file name '
READ, filename
PRINT, ' Enter output file name '
READ, outfilename
plot_title(3) = 'SWICS/Wind  ' + STRMID(outfilename,22,6)
i = 1
OPENR, 1, filename
WHILE NOT EOF(1) DO BEGIN
     ON_IOERROR, go_on
     READF, 1, yy, doy, hh, mm, temp
     search_time=(yy-90)*365.00 + doy*1.0000 + hh/24.00 + mm/(24.00*60.00)
     IF search_time LT time_start THEN GOTO, go_on         ;read next line
     IF search_time GT time_end THEN GOTO, close_file      ;done,close file
     temp_data(i-1,*) =  temp
     temp_time(i-1) = search_time
     i = i + 1
     IF i EQ 5001 THEN BEGIN
        IF first THEN BEGIN
          first = ''
          data = temp_data
          time = temp_time
        ENDIF ELSE BEGIN
          data = [data,temp_data]
          time = [time,temp_time]
        ENDELSE
        i = 1
     ENDIF
     go_on:
ENDWHILE
close_file:
CLOSE, 1
IF MAX(time) EQ 0 THEN BEGIN                     ;not more than 5000 lines
     IF MAX(temp_time) EQ 0 THEN BEGIN           ; no data in time range
        PRINT, 'No data in selected time period'
        GOTO, exit
     ENDIF
     data = temp_data
     time = temp_time
ENDIF ELSE BEGIN
     data = [data,temp_data]
     time = [time,temp_time]
ENDELSE
;                                      find number of data entries
temp = SIZE(data)
data_size = temp(1)-5002+i
data = data(0:data_size,*)
time = time(0:data_size,*)
temp0 = where(data EQ -1e10)
tempa = SIZE(temp0)
;IF temp0 NE -1 THEN data(temp0) = 1e10
;                                      find time range rounded to hour
temp1 = FIX((time(0)-FIX(time(0)))*24)                    ;get hours
;min_time = FIX(time(0)) + temp1/24.                       ;round to nearest hr
min_time = FIX(time(0))                        ;round to nearest day
temp1 = FIX((MAX(time)-FIX(MAX(time)))*24)                    ;get hours
;max_time = FIX(MAX(time)) + temp1/24.
max_time = FIX(MAX(time)) + 1
IF max_time-min_time GT 365 THEN BEGIN
     IF max_time-min_time GE 368 THEN BEGIN
        IF max_time MOD 1 NE 0 THEN max_time = FIX(max_time) + 1
        IF min_time MOD 1 NE 0 THEN min_time = FIX(min_time)
        ticknumber = FIX(max_time-min_time) - 365
        mticknumber = 6
     ENDIF
ENDIF ELSE BEGIN
     IF max_time-min_time GE 3 THEN BEGIN
        IF max_time MOD 1 NE 0 THEN max_time = FIX(max_time) + 1
        IF min_time MOD 1 NE 0 THEN min_time = FIX(min_time)
        ticknumber = FIX(max_time-min_time)
        mticknumber = 6
     ENDIF
ENDELSE
IF ticknumber GT 12 THEN BEGIN
     FOR i = 12, 2, -1 DO BEGIN
        IF ticknumber MOD i EQ 0 THEN BEGIN
          ticknumber = i
          GOTO, next1
        ENDIF
     ENDFOR
ENDIF
IF ticknumber GT 29 THEN ticknumber = 10
next1:
;                                      set up color table
;        0    1   2    3    4    5    6    7    color number
;      ---------------------------------------
RED   = [0, 255, 255,   0,   0, 255,   0,   0] ;red components
GREEN = [0, 255,   0, 255,   0, 255, 255,   0] ;green components
BLUE  = [0, 255,   0,   0, 255,   0, 255,   0] ;blue components
;                                                make first plot
TVLCT, RED, GREEN, BLUE                 ;load the first 8 elements of table
;
IF loglin(0) EQ 'lin' THEN BEGIN
     PLOT, time, data(*,param_num(0,0)), $
        BACKGROUND = 1, COLOR = 7, $
        XRANGE = [min_time, max_time], $
        YRANGE = [min_y(0), max_y(0)], $
        POSITION = xy(*,0), $                              ;position of plot
        YSTYLE=1,$                                         ;exact y range
        XSTYLE=1,$                                         ;exact x range
        TITLE = plot_title(0),$                               ;plot title
        SUBTITLE=x_title,$                                   ;x title
        XTICKFORMAT='xtest',$                              ;format time axis
        XMINOR=mticknumber, $                            ;number of minor ticks
        XTICKS=ticknumber, $                                 ;number of ticks
        XTICK_GET = doy_val,$
        /NODATA                                  ; just make axes
     OPLOT, time, data(*,param_num(0,0)),$
        MAX_VALUE=high_point,$                        ;ignore data above value
        LINE=linestyle(0,0),$                                ;type of line
        PSYM = symbol(0,0), $
        COLOR = color(0,0)
     XYOUTS, xy(0,0)-.07, (xy(3,0)-xy(1,0))/2.+xy(1,0), y_title(0), /NORM, $
          ORIENT=90, ALIGNMENT=0.5, COLOR = 7
ENDIF ELSE BEGIN
     PLOT_IO, time, data(*,param_num(0,0)), $
        BACKGROUND = 1, COLOR = 7, $
        XRANGE = [min_time, max_time], $
        YRANGE = [min_y(0), max_y(0)], $
        POSITION = xy(*,0), $
        YSTYLE=1,$
        XSTYLE=1,$
        SUBTITLE=x_title,$                                   
        TITLE = plot_title(0),$                               ;plot title
        XTICKFORMAT='xtest',$
        XMINOR=mticknumber, $
        XTICKS=ticknumber, $
        XTICK_GET = doy_val,$
        /NODATA
     OPLOT, time, data(*,param_num(0,0)),$
        MAX_VALUE=high_point,$
        LINE=linestyle(0,0),$                                ;type of line
        PSYM = symbol(0,0),$
        COLOR = color(0,0)
     XYOUTS, xy(0,0)-.07, (xy(3,0)-xy(1,0))/2.+xy(1,0), y_title(0), /NORM,$
          ORIENT=90, ALIGNMENT=0.5, COLOR = 7
ENDELSE
FOR i = 1, param_plot(0)-1 DO BEGIN                        ;plot other params
     OPLOT, time, data(*,param_num(0,i)),$
        COLOR = color(0,i), $
        LINE=linestyle(0,i), $
        PSYM = symbol(0,i),$
        MAX_VALUE=high_point  
ENDFOR
;                                               add doy labels
doy_label = STRARR(13)
doy_xpos = FLTARR(13)
doy_ypos = FLTARR(13)
j = 0
FOR i = 0, ticknumber DO BEGIN
     IF doy_val(i) MOD 1 EQ 0 THEN BEGIN
        IF loglin(0) EQ 'log' THEN BEGIN
          doy_ypos(j) = min_y(0)/100
        ENDIF ELSE doy_ypos(j) = MAX([min_y(0)-0.4*(max_y(0)-min_y(0)),$
                                       ypos_limit])
        doy_xpos(j) = doy_val(i)
        temp1 = FIX(doy_val(i))
        temp1 = temp1-(temp1/365)*365
        doy_label(j) = STRING(temp1, FORMAT='(i3.3)')
        j = j + 1
     ENDIF
ENDFOR
XYOUTS, 0, 0.02, ' DOY', /NORM, COLOR = 7
XYOUTS, doy_xpos, doy_ypos, doy_label, COLOR = 7, ALIGNMENT=0.5 ;centered
;                                                add other plots
FOR i = 1, frames-1 DO BEGIN
     IF loglin(i) EQ 'lin' THEN BEGIN
        PLOT, time, data(*,param_num(i,0)), $
          BACKGROUND = 1, COLOR = 7, $
          XRANGE = [min_time, max_time], $
          YRANGE = [min_y(i), max_y(i)], $
          /NOERASE, $                               ;don't erase previous plot
          POSITION = xy(*,i),$
          XSTYLE=1,$
          XTICKS=ticknumber, $
          XMINOR=mticknumber, $
          XTICKFORMAT='noticks',$
          YSTYLE=1,$
          TITLE = plot_title(i),$                               ;plot title
          /NODATA
        OPLOT, time, data(*,param_num(i,0)), $
          MAX_VALUE=high_point, $
          LINE=linestyle(i,0),$
          PSYM = symbol(i,0), $
          COLOR = color(i,0)
        XYOUTS, xy(0,i)-.07, (xy(3,i)-xy(1,i))/2.+xy(1,i), y_title(i), $
            /NORM, COLOR = 7, $
            ORIENT=90, ALIGNMENT=0.5
     ENDIF ELSE BEGIN
        PLOT_IO, time, data(*,param_num(i,0)), $
          BACKGROUND = 1, COLOR = 7, $
          XRANGE = [min_time, max_time], $
          YRANGE = [min_y(i), max_y(i)], $
          /NOERASE, $
          POSITION = xy(*,i),$
          XTICKFORMAT='noticks',$
          XMINOR=mticknumber, $
          XTICKS=ticknumber, $
          XSTYLE=1,$
          YSTYLE=1,$
          TITLE = plot_title(i),$                               ;plot title
          /NODATA
        OPLOT, time, data(*,param_num(i,0)), $
          MAX_VALUE=high_point, $
          LINE=linestyle(i,0),$
          PSYM = symbol(i,0),$
          COLOR = color(i,0)
        XYOUTS, xy(0,i)-.07, (xy(3,i)-xy(1,i))/2.+xy(1,i), y_title(i), $
            /NORM, COLOR = 7, $
            ORIENT=90, ALIGNMENT=0.5
     ENDELSE
     FOR j = 1, param_plot(i)-1 DO BEGIN                   ; other lines
        OPLOT, time, data(*,param_num(i,j)),$
          COLOR = color(i,j), $
          LINE=linestyle(i,j), $
          MAX_VALUE=high_point                        ;ignore data above value
     ENDFOR
ENDFOR
DONE:
IF out_type EQ 2 THEN BEGIN
;                                      set colors for gif file
R_CURR = [RED, BYTARR(248) + 255]
G_CURR = [GREEN, BYTARR(248) + 255]
B_CURR = [BLUE, BYTARR(248) + 255]
;
     IMAGE = TVRD()
     WRITE_GIF, outfilename, IMAGE
ENDIF 
EXIT:
END
