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
;start main program
;    this IDL program reads in time ordered data and creates a time trend
;    plot
;    cmsc 10 Jan 1995
param_num = INTARR(10,10)
param_plot = INTARR(10)
linestyle = INTARR(10,10)
answer = ''
XTICKNAME = ''
plot_title = STRARR(10)
PRINT, '  Do you want output to (1) screen or (2) printer?'
READ, out_type
IF out_type EQ 2 THEN BEGIN
     SET_PLOT, 'ps'
     DEVICE, /PORTRAIT, /INCHES, XSIZE=7.0, YSIZE=9.0, XOFF=1.0, YOFF=1.0
ENDIF ELSE BEGIN
     SET_PLOT, 'tek'
ENDELSE
PRINT, '  1.  Velocity '
PRINT, ' -1.  Other '
PRINT, ' Enter type of file' 
READ, file_type
CASE 1 OF
     file_type eq 1 : BEGIN
        plot_title(5) = 'SWICS/Wind'
        frames = 6
        parameters = 8
        ticknumber = 12                          ;12 major tick marks
        mticknumber = 6                          ;6 minor tick marks
        high_point = 1e9
        min_y = FLTARR(frames)
        max_y = FLTARR(frames)
        xy = FLTARR(4,frames)
        loglin = STRARR(frames)
        y_title = STRARR(frames)
        x_title = ' Time (hours) '
        loglin(5) = 'lin'                        ; lin vel plot
        min_y(5) = 200.
        max_y(5) = 1000.
        y_title(5) = 'H+ Vel (km/s)'
        xy(*,5) = [0.1,0.775,0.9,0.9]
        param_plot(5) = 1
        param_num(5,0) = 0
        linestyle(5,0) = 0
        loglin(3) = 'log'                        ; log t4 plot
        min_y(3) = 0.1
        max_y(3) = 100
        y_title(3) = 'Kin Temp (E4K)'
        xy(*,3) = [0.1,0.525,0.9,0.65]
        param_plot(3) = 1
        param_num(3,0) = 1
        linestyle(3,0) = 0
        loglin(1) = 'lin'                        ; lin t6 plot
        min_y(1) = 1.
        max_y(1) = 3.
        y_title(1) = 'O Ioniz. T (MK)'
        xy(*,1) = [0.1,0.275,0.9,0.4]
        param_plot(1) = 1
        param_num(1,0) = 4
        linestyle(1,0) = 0
        loglin(4) = 'lin'                        ; lin delta v
        min_y(4) = -50.
        max_y(4) = 50.
        y_title(4) = 'He vel-H vel'
        xy(*,4) = [0.1,0.65,0.9,0.775]
        param_plot(4) = 1
        param_num(4,0) = 6
        linestyle(4,0) = 0
        loglin(2) = 'lin'                        ; lin tk ratio
        min_y(2) = 0.
        max_y(2) = 10.
        y_title(2) = 'He Tk/H Tk'
        xy(*,2) = [0.1,0.4,0.9,0.525]
        param_plot(2) = 1
        param_num(2,0) = 7
        linestyle(2,0) = 0
        loglin(0) = 'lin'                        ; lin Fe/cno
        min_y(0) = 0.
        max_y(0) = 30.
        y_title(0) = 'CNO/Fe'
        xy(*,0) = [0.1,0.15,0.9,0.275]
        param_plot(0) = 1
        param_num(0,0) = 5
        linestyle(0,0) = 0
     END
     ELSE : BEGIN
        inp_string = ' '
        inp_dat = FLTARR(2)
        PRINT, 'Enter number of parameters to read'
        READ, parameters
        ticknumber = 12                          ;12 major tick marks
        mticknumber = 6                          ;6 minor tick marks
        high_point = 1e8
        PRINT, 'Enter number of frames to create'
        READ, frames
        PRINT, 'Enter title of plot'
        READ, inp_string
        plot_title(frames-1) = inp_string
        min_y = FLTARR(frames)
        max_y = FLTARR(frames)
        xy = FLTARR(4,frames)
        loglin = STRARR(frames)
        y_title = STRARR(frames)
        x_title = ' Time (hours) '
        FOR i = 1, frames DO BEGIN
          PRINT, ' '
          PRINT, ' Enter min and max of y axis '
          READ, inp_dat
          min_y(i-1) = inp_dat(0)
          max_y(i-1) = inp_dat(1)
          PRINT, ' Enter y title '
          READ, inp_string
          y_title(i-1) = inp_string
          PRINT, '  Default is linear plot full page with one parameter'
          PRINT, '  Do you wish to change this?'
          READ, answer
          IF answer EQ 'n' OR answer EQ 'N' THEN answer = ''
          IF answer THEN BEGIN
            PRINT, ' '
            PRINT, ' Enter lin or log for plot ', i
            READ, inp_string
            IF inp_string EQ 'log' THEN loglin(i-1) = 'log' $
            ELSE loglin(i-1) = 'lin'
            PRINT, ' Enter x range for axis length'
            READ, inp_dat
            xy(0,i-1) = inp_dat(0)
            xy(2,i-1) = inp_dat(1)
            PRINT, ' Enter y range for axis length'
            READ, inp_dat
            xy(1,i-1) = inp_dat(0)
            xy(3,i-1) = inp_dat(1)
            PRINT, ' Enter number of parameters to plot in this frame'
            READ, param
            param_plot(i-1) = param
          ENDIF ELSE BEGIN
            loglin(i-1) = 'lin'
            xy(*,i-1) = [.1,.15,.9,.8]
            param_plot(i-1) = 1
          ENDELSE
          FOR j = 1, param_plot(i-1) DO BEGIN
            PRINT, ' Enter parameter number to plot'
            READ, param
            param_num(i-1,j-1) = param-1
            PRINT, ' Enter line style to use'
            READ, param
            linestyle(i-1,j-1) = param
          ENDFOR
        ENDFOR
     END
ENDCASE
ypos_limit = min_y(0)-0.08*(max_y(0)-min_y(0))/(xy(3,0)-xy(1,0))
PRINT, ' '
PRINT, ' Do you want a time search?'
READ, answer
IF answer EQ 'n' OR answer EQ 'N' THEN answer = ''
IF answer THEN BEGIN
     PRINT, '  Enter start time [yy doy hh mm]'
     READ, yy, doy, hh, mm
     time_start=(yy-93)*365.00 + doy*1.0000 + hh/24.00 + mm/(24.00*60.00)
     PRINT, '  Enter stop time [yy doy hh mm]'
     READ, yy, doy, hh, mm
     time_end=(yy-93)*365.00 + doy*1.0000 + hh/24.00 + mm/(24.00*60.00)
ENDIF ELSE BEGIN
     time_start = 0.
     time_end = 6000.
ENDELSE
temp = FLTARR(1,parameters)
temp1 = dblarr(1)
temp_data = FLTARR(5000,parameters)
temp_time = FLTARR(5000)
data = FLTARR(5000,parameters)
time = FLTARR(5000)
filename = ' '
first = 'first'
PRINT, ' Enter file name '
READ, filename
i = 1
OPENR, 1, filename
WHILE NOT EOF(1) DO BEGIN
     ON_IOERROR, go_on
     READF, 1, yy, doy, hh, mm, temp
     search_time=(yy-93)*365.00 + doy*1.0000 + hh/24.00 + mm/(24.00*60.00)
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
        GOTO, done
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
;                                      find time range rounded to hour
temp1 = FIX((time(0)-FIX(time(0)))*24)                    ;get hours
min_time = FIX(time(0)) + temp1/24.                       ;round to nearest hr
temp1 = 1+FIX((MAX(time)-FIX(MAX(time)))*24)                    ;get hours
max_time = FIX(MAX(time)) + temp1/24.
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
     FOR i = 12, 1, -1 DO BEGIN
        IF ticknumber MOD i EQ 0 THEN BEGIN
          ticknumber = i
          GOTO, next1
        ENDIF
     ENDFOR
ENDIF
;                                                make first plot
next1:
IF loglin(0) EQ 'lin' THEN BEGIN
     PLOT, time, data(*,param_num(0,0)), $
        XRANGE = [min_time, max_time], $
        YRANGE = [min_y(0), max_y(0)], $
        POSITION = xy(*,0), $                              ;position of plot
        YSTYLE=1,$                                         ;exact y range
        XSTYLE=1,$                                         ;exact x range
        TITLE = plot_title(0),$                               ;plot title
        SUBTITLE=x_title,$                                   ;x title
        MAX_VALUE=high_point,$                        ;ignore data above value
        XTICKFORMAT='xtest',$                              ;format time axis
        XMINOR=mticknumber, $                            ;number of minor ticks
        XTICKS=ticknumber, $                                 ;number of ticks
        XTICK_GET = doy_val,$
        LINE=linestyle(0,0)                                ;type of line
        XYOUTS, xy(0,0)-.07, (xy(3,0)-xy(1,0))/2.+xy(1,0), y_title(0), /NORM, $
          ORIENT=90, ALIGNMENT=0.5
ENDIF ELSE BEGIN
     PLOT_IO, time, data(*,param_num(0,0)), $
        XRANGE = [min_time, max_time], $
        YRANGE = [min_y(0), max_y(0)], $
        POSITION = xy(*,0), $
        YSTYLE=1,$
        XSTYLE=1,$
        SUBTITLE=x_title,$                                   
        TITLE = plot_title(0),$                               ;plot title
        MAX_VALUE=high_point,$
        XTICKFORMAT='xtest',$
        XMINOR=mticknumber, $
        XTICKS=ticknumber, $
        XTICK_GET = doy_val,$
        LINE=linestyle(0,0)                                ;type of line
        XYOUTS, xy(0,0)-.07, (xy(3,0)-xy(1,0))/2.+xy(1,0), y_title(0), /NORM,$
          ORIENT=90, ALIGNMENT=0.5
ENDELSE
FOR i = 1, param_plot(0)-1 DO BEGIN                        ;plot other params
     OPLOT, time, data(*,param_num(0,i)),$
        LINE=linestyle(0,i), $
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
XYOUTS, doy_xpos, doy_ypos, doy_label, ALIGNMENT=0.5 ;centered
;                                                add other plots
FOR i = 1, frames-1 DO BEGIN
     IF loglin(i) EQ 'lin' THEN BEGIN
        PLOT, time, data(*,param_num(i,0)), $
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
          MAX_VALUE=high_point, $
          LINE=linestyle(i,0)
          XYOUTS, xy(0,i)-.07, (xy(3,i)-xy(1,i))/2.+xy(1,i), y_title(i), $
            /NORM, $
            ORIENT=90, ALIGNMENT=0.5
     ENDIF ELSE BEGIN
        PLOT_IO, time, data(*,param_num(i,0)), $
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
          MAX_VALUE=high_point, $
          LINE=linestyle(i,0)
          XYOUTS, xy(0,i)-.07, (xy(3,i)-xy(1,i))/2.+xy(1,i), y_title(i), $
            /NORM, $
            ORIENT=90, ALIGNMENT=0.5
     ENDELSE
     FOR j = 1, param_plot(i)-1 DO BEGIN                   ; other lines
        OPLOT, time, data(*,param_num(i,j)),$
          LINE=linestyle(i,j), $
          MAX_VALUE=high_point                        ;ignore data above value
     ENDFOR
ENDFOR
DONE:
END
