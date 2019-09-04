C+
C 
C FUNCTIONAL DESCRIPTION:	
C 
C    This file consists of seven functions: one integer function and six
C    logical functions.  All of them have the same parameter list.  The integer
C    function, COMPARE_TIME, is used by the other six functions.  The six
C    logical functions are named COMPARE_TIME_xx, where xx is a 2-letter
C    abbreviation for the relation to be tested: EQ, LE, LT, GE, GT, or NE.
C 
C FORMAL PARAMETERS:
C  
C     T1:
C	  A time of type RECORD /T/.
C      
C     T2:
C	  A time of type RECORD /T/.
C      
C  
C DESIGN:
C  
C     Use a common compare_time routine to return an integer result indicating
C     which of its time arguments is greater or if they are the same.  Test this
C     result to produce the six relations .lt., .le., .eq., .ge., .gt., and
C     .ne. .  Return a logical value indicating if the given relation is true.
C  
C 
C This version:    6-JAN-1995
C 
C Created: 
C 
C      11-NOV-1994    Larry Bleau
C 
C Revised:
C       6-JAN-1995    L. Bleau     added comments
C 
C-
        logical function compare_time_ge (t1, t2)
C
C Compare the times specified in parameter t1 with that of parameter t2. 
C Return .TRUE. if time t1 is greater than (later) or equal to time t2, .FALSE.
C otherwise.
C
C Both t1 and t2 are RECORDs of structure type /T/, which is defined in the
C file EDFDEF.INC.
C
        include 'inc_dir:edbdef.inc/nolist'
        record /t/ t1, t2
	integer compare_time
C
	compare_time_ge = compare_time(t1,t2) .ge. 0
	return
	end

        logical function compare_time_gt (t1, t2)
C
C Compare the times specified in parameter t1 with that of parameter t2. 
C Return .TRUE. if time t1 is greater than (later) to time t2, .FALSE.
C otherwise.
C
C Both t1 and t2 are RECORDs of structure type /T/, which is defined in the
C file EDFDEF.INC.
C
        include 'inc_dir:edbdef.inc/nolist'
        record /t/ t1, t2
	integer compare_time
C
	compare_time_gt = compare_time(t1,t2) .gt. 0
	return
	end

        logical function compare_time_eq (t1, t2)
C
C Compare the times specified in parameter t1 with that of parameter t2. 
C Return .TRUE. if time t1 is equal to time t2, .FALSE. otherwise.
C
C Both t1 and t2 are RECORDs of structure type /T/, which is defined in the
C file EDFDEF.INC.
C
        include 'inc_dir:edbdef.inc/nolist'
        record /t/ t1, t2
	integer compare_time
C
	compare_time_eq = compare_time(t1,t2) .eq. 0
	return
	end

        logical function compare_time_le (t1, t2)
C
C Compare the times specified in parameter t1 with that of parameter t2. 
C Return .TRUE. if time t1 is less than (before) or equal to time t2, .FALSE.
C otherwise.
C
C Both t1 and t2 are RECORDs of structure type /T/, which is defined in the
C file EDFDEF.INC.
C
        include 'inc_dir:edbdef.inc/nolist'
        record /t/ t1, t2
	integer compare_time
C
	compare_time_le = compare_time(t1,t2) .le. 0
	return
	end

        logical function compare_time_lt (t1, t2)
C
C Compare the times specified in parameter t1 with that of parameter t2. 
C Return .TRUE. if time t1 is less than (before) time t2, .FALSE. otherwise.
C
C Both t1 and t2 are RECORDs of structure type /T/, which is defined in the
C file EDFDEF.INC.
C
        include 'inc_dir:edbdef.inc/nolist'
        record /t/ t1, t2
	integer compare_time
C
	compare_time_lt = compare_time(t1,t2) .le. 0
	return
	end

        logical function compare_time_ne (t1, t2)
C
C Compare the times specified in parameter t1 with that of parameter t2. 
C Return .TRUE. if time t1 is not equal to time t2, .FALSE. otherwise.
C
C Both t1 and t2 are RECORDs of structure type /T/, which is defined in the
C file EDFDEF.INC.
C
        include 'inc_dir:edbdef.inc/nolist'
        record /t/ t1, t2
	integer compare_time
C
	compare_time_ne = compare_time(t1,t2) .ne. 0
	return
	end

        integer function compare_time (t1, t2)
C+
C 
C FUNCTIONAL DESCRIPTION:
C 
C Compare the times specified in parameter t1 with that of parameter t2. 
C
C Both t1 and t2 are RECORDs of structure type /T/, which is defined in the
C file EDFDEF.INC.
C 
C FUNCTION VALUE:
C 
C Returns one of the following integer values:
C	-1	if time t1 is greater (later) than time t2
C	 0	if time t1 is equal to time t2
C	-1	if time t1 is less (before) than time t2
C 
C DESIGN:
C  
C     Too simple to need a description
C 
C Created: 
C 
C	 11-NOV-1994    L. Bleau
C 
C Revised:
C 
C-
        include 'inc_dir:edbdef.inc/nolist'
        record /t/ t1, t2
C
        if (t1.year .lt. t2.year) then
          compare_time = -1
        else if (t1.year .gt. t2.year) then
          compare_time = 1
        else
          if (t1.doy .lt. t2.doy) then
            compare_time = -1
          else if (t1.doy .gt. t2.doy) then
            compare_time = 1
          else
            if (t1.hr .lt. t2.hr) then
              compare_time = -1
            else if (t1.hr .gt. t2.hr) then
              compare_time = 1
            else
              if (t1.min .lt. t2.min) then
                compare_time = -1
              else if (t1.min .gt. t2.min) then
                compare_time = 1
              else
                if (t1.sec .lt. t2.sec) then
                  compare_time = -1
                else if (t1.sec .gt. t2.sec) then
                  compare_time = 1
                else
                  if (t1.csec .lt. t2.csec) then
                    compare_time = -1
                  else if (t1.csec .gt. t2.csec) then
                    compare_time = 1
                  else
                    compare_time = 0
                  endif
                endif
              endif
            endif
          endif
        endif
        return
        end
