	SUBROUTINE select_file(pattern,default_spec,confirm,result,reslen,status)
C+
C 
C FUNCTIONAL DESCRIPTION:	
C 
C    This subroutine searches a directory for a list of files which fit a
C    certain pattern.  It prompts the user interactively, allowing him to select
C    a file.  The selected filename is returned to the caller.
C 
C FORMAL PARAMETERS:
C  
C     pattern:
C	  A CHARACTER variable or constant containing the search pattern to be
C	  used to produce the list of files.  This pattern must contain at
C	  least one '*' or '%' wildcard character.
C      
C     default_spec:
C	  A CHARACTER variable or constant specifying the default file
C	  specification to use in the file search.  If left blank (' ') no
C	  default specification is used.
C      
C     confirm:
C	  An INTEGER*4 variable or constant which, if non-zero, causes
C	  SELECT_FILE to prompt the user for confirmation of the selected
C	  file.
C      
C     result:
C	  A CHARACTER variable which will receive the selected file.  This
C	  should be long enough to contain the entire filename including the
C	  directory name.  If too short the filename will be truncated.  If no
C	  file was selected result will be set to blanks.
C      
C     reslen:
C	  An INTEGER*4 variable which will receive the length (number of
C	  characters) in the selected filename which is stored in `result'.  If
C	  reslen is equal to LEN(result) the filename may have been truncated.
C	  If no file was selected reslen will be set to zero.
C      
C     status:
C	  An INTEGER*4 variable which will receive the status code for the
C	  select operation.  A 1 indicates success; any other value indicates a
C	  problem of some type.
C  
C COMMON BLOCKS:
C  
C     None
C  
C DESIGN:
C  
C     Using pattern call the library routine LIB$FIND_FILE to produce a list of
C     filenames.  Hold the list internally and display them to the terminal
C     with index numbers.  Prompt the user for an index number and return that
C     filename as the result.  Allow user to not select any files or to display
C     more selections if they won't all fit on a screen.
C
C 
C This version:   5-NOV-1994
C 
C Created: 
C 
C	 5-NOV-1994    L. Bleau  University of Maryland
C				 Physics Dept. / Space Physics Group
C				 College Park, MD 20742
C 
C Revised:
C 
C Ver	 Date     | Name  | Description
C ----------------+-------+-----------------------------------------------------
C 1.1  7-NOV-1994 |  LRB  | add comments, include $RMSDEF, add logic to allow
C                           for filename using entire variable's length
C 1.2  9-NOV-1994 |  LRB  | ignore trailing blanks in pattern
C 1.3 10-NOV-1994 |  LRB  | added argument and code to optionally confirm file
C                           selection
C 1.4  9-JAN-1995 |  LRB  | if no more filenames to list use different prompt
C 1.5  5-MAY-1995 |  LRB  | increase max number of files in menu from 200 to 400
C 1.6 16-MAY-1995 |  LRB  | increase max number of files in menu from 400 to 800
C 1.7  1-MAR-2007 |  JAP  | increase max number of files in menu from 800 to 4400
C-
CDEC$   IDENT   '1.4'

	IMPLICIT NONE
	CHARACTER*(*) pattern,result,default_spec
	INTEGER*4 confirm,reslen,status
	INCLUDE '($RMSDEF)/NOLIST'
C
	INTEGER maxsave,nlines
	PARAMETER (maxsave = 4400)
	PARAMETER (nlines = 20)
C
	CHARACTER*132 possibles(maxsave)
	CHARACTER*132 filename,line
	INTEGER dirend(maxsave),namelen(maxsave)
	INTEGER*4 lib$find_file,lib$find_file_end,lib$put_output,lib$get_input
	INTEGER*4 nch,context
	INTEGER nfound,i,first,last,patlen
	INTEGER*2 msglen
	BYTE resarr(4)
	LOGICAL more
C
C Make sure pattern has some wildcards
C If it doesn't, just return pattern as result
C
        patlen = INDEX(pattern,' ') - 1
        IF (patlen .eq. -1) patlen = LEN(pattern)
	IF (INDEX(pattern(1:patlen),'*') .eq. 0 .and. INDEX(pattern(1:patlen),'%') .eq. 0) THEN
	    result = pattern
	    reslen = INDEX(result,' ') - 1
	    IF (reslen .eq. -1) reslen = LEN(result)
	    status = 1
	    RETURN
	END IF
C
C Fill up possibles array with candidate filenames
C
	nfound = 0
	DO i=1,maxsave
	    IF (default_spec .eq. ' ') THEN
		status = lib$find_file (pattern(1:patlen), filename, context, , , ,)
	    ELSE
		status = lib$find_file (pattern(1:patlen), filename, context, 
     1					default_spec, , , )
	    END IF
	    IF (status .eq. RMS$_FNF) GOTO 900		! can't find file/directory
	    IF (status .eq. RMS$_NMF) GOTO 100		! no more files
	    IF (status .ne. RMS$_NORMAL) GOTO 900	! other error, quit
C
C We have a file; store it into table of possible selections
C
	    nfound = nfound + 1
	    dirend(i) = INDEX (filename,']')
	    namelen(i) = INDEX (filename,' ') - 1
	    IF (namelen(i) .eq. 0) namelen(i) = LEN(filename)
	    possibles(i) = filename
	END DO
C
C Display array on screen with index numbers
C
100	first = 1
200	last = MIN(first+nlines-1, nfound)
	more = last .ne. nfound
	call lib$put_output (' ') 
	DO i=first,last
	    WRITE(line,1001) i,possibles(i)(1:namelen(i))
1001	FORMAT(i3,2x,a)
	    call lib$put_output (line(1:namelen(i)+5)) 
	END DO
	call lib$put_output (' ') 
300	IF (more) THEN
	  status = lib$get_input (
     1		     line, 
     1		     'Enter selection # (or q=quit, r=redisplay, <return>=show more): ', 
     1		     nch) 
	ELSE
	  status = lib$get_input (
     1		     line, 
     1		     'Enter selection # (or q=quit, r=redisplay): ', 
     1		     nch) 
	ENDIF
	IF (status .ne. 1) GOTO 800
C
C Check if user wants more filenames displayed (empty line)
C
	IF (nch .eq. 0) THEN
C
C User wants more filenames
C If there are more left adjust starting point and go back to display code
C
	    IF (last .lt. nfound) THEN
		first = last + 1
		GOTO 200
	    END IF
C
C User wants more filenames but there aren't any left
C Give user choice of quitting (no selection made) or redisplaying the entire
C list
C
400	    status = lib$get_input (
     1			 line, 
     1			 'No more filenames; q=quit, r=redisplay entire list: ', 
     1			 nch)
	    IF (status .ne. 1) GOTO 800
	    IF (nch .eq. 0) GOTO 400
	    IF (line(1:1) .eq. 'q' .or. line(1:1) .eq. 'Q') GOTO 800
	    IF (line(1:1) .eq. 'r' .or. line(1:1) .eq. 'R') GOTO 100
	    GOTO 400
	END IF
C
C User entered something; decode it
C First check for 'q' or 'Q', then check if an integer
C
	IF (line(1:1) .eq. 'q' .or. line(1:1) .eq. 'Q') GOTO 800
	IF (line(1:1) .eq. 'r' .or. line(1:1) .eq. 'R') GOTO 200
	READ(line(1:nch),1002,ERR=700) i
1002	FORMAT(I<nch>)
C
C User gave a syntactically valid integer, check if it is in range
C If so and confirm is non-zero display selected name and confirm choice
C
	IF (i .lt. 1 .or. i .gt. nfound) GOTO 700
	IF (confirm .ne. 0) THEN
	  call lib$put_output ('You selected '//possibles(i)(1:namelen(i)))
	  status = lib$get_input (
     1		     line, 
     1		     'Is this correct? (<return>=Y) ', 
     1		     nch) 
	  IF (status .ne. 1) line = 'n'
	  IF (nch .eq. 0) line = 'y'
	  IF (line(1:1) .ne. 'y' .and. line(1:1) .ne. 'Y') GOTO 300
	END IF
	GOTO 850
C
C User gave non-integer, reprompt
C
700	call lib$put_output('Invalid response')
	GOTO 300
C
C User wants to quit without any selection
C Return status of 0 to indicate this to caller
C Also set result to blanks, reslen to 0 to caller doesn't get misleading info
C
800	status = lib$find_file_end (context)
	status = 0
	result = ' '
	reslen = 0
	RETURN
C
C User made his selection; return value to caller
C
850	status = lib$find_file_end (context)
	status = 1
	result = possibles(i)
	reslen = namelen(i)
	RETURN
C
C Error of some type
C If no files in list just return
C If some files in list allow user to select or quit
C
900	IF (nfound .eq. 0) THEN
	    call lib$put_output('Error during file search, no list generated')
	ELSE
	    call lib$put_output('Error during file search, only have partial list')
	ENDIF
C	call lib$signal(%val(status))
	call lib$sys_getmsg (status, msglen, line, , resarr)
	call lib$put_output('*** '//line(1:msglen))
	IF (nfound .ne. 0) GOTO 100
	result = ' '
	reslen = 0
	RETURN
	END
