	SUBROUTINE READ_LIMITS(ENTITY,MAX_ENTITY,NUM_READ,NUM_ENABLED)
 
C+
C 
C FUNCTIONAL DESCRIPTION:	
C 
C    This subroutine reads the limits file (default filename LIMITS.DAT),
C    parses its contents, and loads up the array ENTITY with information about
C    each of the instrument's parameters.
C
C FORMAL PARAMETERS:
C  
C     ENTITY:
C	  An array of MAX_ENTITY structures of type HOUSEKEEPING_STATS.  Each
C	  element in the array contains a strcture describing a single
C	  instrument parameter.
C      
C     MAX_ENTITY:
C	  The number of elements in the array ENTITY.
C      
C     NUM_READ:
C	  An integer variable which upon return will contain a count of the
C	  number of instrument parameters that were read into the array ENTITY.
C      
C     NUM_ENABLED:
C	  An integer variable which upon return will contain a count of the
C	  number of instrument parameters whose limits will be checked.
C  
C COMMON BLOCKS:
C  
C     None
C  
C DESIGN:
C  
C     Initialize ENTITY array to have large MIN_VALUE, small MAX_VALUE
C     Open LIMITS file and read it; for each line
C       Decode line
C       If reporting to be enabled save lower, upper limits
C     Close file
C  
C     Location of limits file can be redirected at runtime by defining the
C       logical name LIMITS.  If not defined filename defaults to LIMITS.DAT.
C  
C 
C This version:   1.2   21-NOV-1995
C 
C Created: 
C 
C	 14-NOV-1995    Larry Bleau
C 
C Revised:
C 
C	 Date     | Name  | Description
C ----------------+-------+-----------------------------------------------------
C 17-NOV-1995 1.1 |  LRB  | Add loop to initialize ENTITY array
C 21-NOV-1995 1.2 |  LRB  | fixed code to extract units, decode limits
C
C-
CDEC$	IDENT  '1.2'
	IMPLICIT NONE
	INCLUDE 'INC_DIR:EDBDEF.INC/NOLIST'
	INCLUDE 'INC_DIR:LIMITDEF.INC/NOLIST'
	INTEGER MAX_ENTITY,NUM_READ,NUM_ENABLED
	RECORD /HOUSEKEEPING_STATS/ ENTITY(MAX_ENTITY)
C
	INTEGER LIM_LUN,LIB$GET_LUN
	INTEGER STATUS,FORTERR,RMSSTS,RMSSTV,LUNIT,CONDVAL
	INTEGER LINENO,NCH,IDX
	CHARACTER LINE*132,NAME*40
	LOGICAL ENABLED
C
C Initialize ENTITY array, setting the REPORT field to .FALSE., MIN_VALUE to a
C large number, and MAX_VALUE to a small number
C
	DO IDX=1,MAX_ENTITY
	  ENTITY(IDX).REPORT = .FALSE.
	  ENTITY(IDX).MIN_VALUE = 99999.
	  ENTITY(IDX).MAX_VALUE = -99999.
	END DO
C
C Get a unit number and open the limits file
C
	STATUS = LIB$GET_LUN (LIM_LUN)
	IF (STATUS .NE. 1) GOTO 9010
C
	OPEN (
	1 UNIT=LIM_LUN,
	1 FILE = 'LIMITS',
	1 STATUS = 'OLD',
	1 CARRIAGECONTROL = 'LIST',
	1 DEFAULTFILE = '.DAT',
	1 READONLY,
	1 ERR = 9000,
	1 IOSTAT = STATUS)
	LINENO = 0
	NUM_READ = 0
	NUM_ENABLED = 0
C
C Read each line of the file and process it.
C If it starts with a "!" or is empty, ignore the line and read the next one.
C Otherwise decode the line into the proper data structures.
C The format of the input line is
C
C                Column   Size
C Field name   Position  (Chars)  Format  Meaning                           
C ===========  ========  =======  ======  =============================     
C Index (#)         1      3       I3     a unique identifying number for this
C                                         parameter; DO NOT CHANGE THIS!!!
C Parameter         5     40       A40    the descriptive name of the parameter
C Lower            46      9       F9.2   lower limit; lowest "good" value
C Upper            57      9       F9.2   upper limit; highest "good" value
C Units            68      4       A4     the units of this parameter
C Check?           77      1       A1     whether of not to check this
C                                         parameter against its upper and lower
C                                         limits; must be Y or N
C 
100	READ(LIM_LUN,101,END=500,ERR=500) NCH,LINE
101	FORMAT(Q,A)
	LINENO = LINENO + 1
	IF (NCH .EQ. 0) GOTO 100
D	type 1004,lineno,line(1:nch)
D1004	format(' Line ',i3,': "',a,'"')
	IF (LINE(1:1) .EQ. '!') GOTO 100
	ENABLED = LINE(77:77) .EQ. 'Y' .OR. LINE(77:77) .EQ. 'y' 
D	enabled = .true.
	READ(LINE,102,ERR=400) IDX,NAME
102	FORMAT(I3,T5,A40)
	IF (IDX .GT. MAX_ENTITY) GOTO 100
	NUM_READ = NUM_READ + 1
	ENTITY(IDX).IDENT = NAME
	ENTITY(IDX).COUNT = 0
	ENTITY(IDX).NZERO = 0
	ENTITY(IDX).UNITS = LINE(68:71)
	IF (.NOT. ENABLED) GOTO 100
C
	IF (LINE(46:54) .EQ. ' ') GOTO 100
	READ(LINE(46:54),*,ERR=400) ENTITY(IDX).LO_LIMIT
C103	FORMAT(T46,F9.2)
	IF (LINE(57:65) .EQ. ' ') GOTO 100
	READ(LINE(57:65),*,ERR=400) ENTITY(IDX).HI_LIMIT
C104	FORMAT(T57,F9.2)
	ENTITY(IDX).REPORT = .TRUE.
	ENTITY(IDX).LO_COUNT = 0
	ENTITY(IDX).HI_COUNT = 0
D	type 105,idx,entity(idx).ident,entity(idx).lo_limit,entity(idx).hi_limit,
D	1 entity(idx).units
D105	format(1x,i3,1x,a40,'|',2f10.2,2x,a)
	NUM_ENABLED = NUM_ENABLED + 1
	GOTO 100
C
400	TYPE *,'FORMAT ERROR READING LIMITS FILE; LINE # ',LINENO,', LINE='
	TYPE *,LINE(1:NCH)
	TYPE *,'LINE IGNORED'
	GOTO 100
C
C Done; close file, release unit number, and return
C
500	CLOSE(LIM_LUN)
	CALL LIB$FREE_LUN (LIM_LUN)
	type *,'NUM_READ    =',NUM_READ
	type *,'NUM_ENABLED =',NUM_ENABLED
	RETURN
C
C Come here for errors
C
9000	TYPE *,'ERROR OPENING LIMITS FILE, STATUS=',STATUS
	CALL ERRSNS (FORTERR, RMSSTS, RMSSTV, LUNIT, CONDVAL)
	CALL EXIT(RMSSTS)
C
9010	TYPE *,'ERROR ALLOCATING UNIT NUMBER'
	CALL EXIT(STATUS)
	END
