        program vaxNumbers
C program to be compiled with DEC Fortran and run on a Vax or Alpha        
        real*4 junk
        integer*4 junkint
        OPEN(UNIT=15, NAME='vaxNumbers.dat', FORM='unformatted', ACCESS='direct', RECL=1, TYPE='NEW')
        junkint = 6
        write(15, REC=1) junkint
        junk=6.14
        write(15, REC=2) junk
        junkint = 300
        write(15, REC=3) junkint
        junk=3.26
        write(15, REC=4) junk
        junkint = 1020
        write(15, REC=5) junkint
        junk=467.98
        write(15, REC=6) junk
        junk=3.32e4
        write(15, REC=7) junk
        junk=4.76
        write(15, REC=8) junk
        junk=4.334e9
        write(15, REC=9) junk
        write(15, REC=10) 712995
        end program
