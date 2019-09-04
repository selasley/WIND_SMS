c||=====================================| TI.for
c|| Terminal Input package

c||-------------------------------------| TI BYTE
c|| accept character

	Subroutine TI BYTE (BYTES, NBYTES, *)

	Byte		BYTES (NBYTES)

	Include		'TI.inc'


  100	ticENT	=' '
	Do 101 j =1, NBYTES
		ticENT (j: j)	=Char (BYTES (j))
  101	Continue
	Call ch ccsp (ticENT)		! convert control chars to spaces

	Type 102, ticENT (1: ch last (ticENT))
  102	Format ('+/',a,'/ ',$)

	jent	=TIX ENT ()		! accept and classify line
	If (jent .eq. ticEBL) Then	! entered blank line
					! leave argument unchanged
	Else If (jent .eq. ticERP) Then	! entered "return previous" command
		Return 1
	Else If (jent .eq. ticETI) Then	! entered other TI command
		go to 100		! reprompt
	Else If (jent .eq. ticENR) Then	! not recognized by TIX ENT
		If (ch last (ticENT) .gt. NBYTES) Then
			Type 120, NBYTES
  120			Format (/,' >>>truncating entry to ',i2.2,' bytes')
		Endif
		Do 122 j =1, NBYTES
			BYTES (j) =Ichar (ticENT (j: j))
  122		Continue
	Endif
c**	call TIX RHK			! remove helpfile key
	End

c||-------------------------------------| TI CHAR
c|| accept character

	Subroutine TI CHAR (CHAR, *)

	Character*(*)	CHAR

	Include		'TI.inc'


  100	Type 101, CHAR (1: ch last (CHAR))
  101	Format ('+/',a,'/ ',$)

	jent	=TIX ENT ()		! accept and classify line
	If (jent .eq. ticEBL) Then	! entered blank line
					! leave argument unchanged
	Else If (jent .eq. ticERP) Then	! entered "return previous" command
		Return 1
	Else If (jent .eq. ticETI) Then	! entered other TI command
		go to 100		! reprompt
	Else If (jent .eq. ticENR) Then	! not recognized by TIX ENT
		If (ch last (ticENT) .gt. Len (CHAR)) Then
			Type 120, Len (CHAR)
  120			Format (/,' >>>truncating entry to ',i2.2,' bytes')
		Endif
		CHAR	=ticENT		! must have been new string
	Endif
c**	call TIX RHK			! remove helpfile key
	End

c||-------------------------------------| TI INT
c|| accept integer

	Subroutine TI INT (INT, *)

	Integer		INT

	Include		'TI.inc'


	Character*5	ifrmt		! to encode E format

  100	Write (ticENT, '(i12)') INT

	Do 101 j =1, 11
		If (ticENT (j:j) .ne. ' ') Then
			go to 102
		Endif
  101	Continue

  102	Type 103, ticENT (j: 12)
  103	Format ('+/',a,'/ ',$)

	jent	=TIX ENT ()		! accept and classify line
	If (jent .eq. ticEBL) Then	! entered blank line
					! leave argument unchanged
	Else If (jent .eq. ticERP) Then	! entered "return previous" command
		Return 1
	Else If (jent .eq. ticETI) Then	! entered other TI command
		go to 100		! reprompt
	Else If (jent .eq. ticENR) Then	! not recognized by TIX ENT
		Write (ifrmt, 117) ch last (ticENT)
  117		Format ('(i',i2.2,')')
		Read (ticENT, ifrmt, Err =120) itemp
		INT	=itemp
		Return

c|	invalid integer
c	------- -------
  120		Type '(/,1x,a,//)', '>>>expect integer or "/"'
		go to 100
	Endif
c**	call TIX RHK			! remove helpfile key
	End
c||-------------------------------------| TI LOG
c|| accept logical

	Subroutine TI LOG (LOG, *)

	Logical		LOG

	Include		'TI.inc'


  100	If (LOG) Then
		ticENT	='Yes'
	Else
		ticENT	='No'
	Endif

  102	Type 103, ticENT (1: ch last (ticENT))
  103	Format ('+/',a,'/ ',$)

	jent	=TIX ENT ()		! accept and classify line
	If (jent .eq. ticEBL) Then	! entered blank line
					! leave argument unchanged
	Else If (jent .eq. ticERP) Then	! entered "return previous" command
		Return 1
	Else If (jent .eq. ticETI) Then	! entered other TI command
		go to 100		! reprompt
	Else If (jent .eq. ticENR) Then	! not recognized by TIX ENT
		Call ch lcuc (ticENT)
		If (ch prfx (ticENT, 'Y') .ne. .0) Then
			LOG	=.true.
			Return
		Else If (ch prfx (ticENT, 'N') .ne. .0) Then
			LOG	=.false.
			Return
		Else
			Type '(/,1x,a,//)',
     x			'>>>first character must be "y" or "n"'
			go to 100
		Endif
	Endif
c**	call TIX RHK			! remove helpfile key
	End
c||-------------------------------------| TI REAL
c|| accept real

	Subroutine TI REAL (REAL, *)

	Real		REAL

	Include		'TI.inc'


	Character*7	efrmt		! to encode E format

  100	Write (ticENT, '(1pe9.2)') REAL

	If (ticENT .eq. ' ') Then	! find first nonblank
		jfrst	=2
	Else
		jrst	=1
	Endif

	Type 101, ticENT (jfrst: 5), ticENT (7: 9)
  101	Format ('+/',a,a,'/ ',$)


	jent	=TIX ENT ()		! accept and classify line
	If (jent .eq. ticEBL) Then	! entered blank line
					! leave argument unchanged
	Else If (jent .eq. ticERP) Then	! entered "return previous" command
		Return 1
	Else If (jent .eq. ticETI) Then	! entered other TI command
		go to 100		! reprompt
	Else If (jent .eq. ticENR) Then	! not recognized by TIX ENT
		Write (efrmt, 117) ch last (ticENT)
  117		Format ('(e',i2.2,'.0)')
		Read (ticENT, efrmt, Err =120) temp
		REAL	=temp
		Return

c|	invalid real
c	------- ----
  120		Type '(/,1x,a,//)', '>>>expect real number or "/"'
		go to 100
	Endif
c**	call TIX RHK			! remove helpfile key
	End
c**c||-------------------------------------| TI OHF
c**c|| open helpfile
c**
c**	Subroutine TI OHF (UNIT, FSPEC)
c**
c**	Integer		UNIT
c**	Character*(*)	FSPEC
c**	Character*(*)	KEY
c**
c**	Include 'TI.inc'
c**
c**	Open (
c**     x		Unit		=UNIT,
c**     x		File		=FSPEC,
c**     x		Status		='OLD',
c**     x		Err		=1)
c**	go to 2
c**
c**    1		Type '(/,1x,a,a)',
c**     x			'>>> can''t open helpfile ',
c**     x			FPSEC (1: ch last (FSPEC))
c**		Call Exit
c**
c**    2	ticHFO	=.true.			! helpfile open
c**	ticHFU	=UNIT
c**	ticHFN	=FSPEC
c**	Return
c||-------------------------------------| TI USER
c|| special user routine

	Function TI USER (IFCN)

	Include 'TI.inc'

	If (IFCN .eq. 0) Then
		TI USER =.false.	! not user written
		Return
	Endif
	End
c||-------------------------------------| TIX ENT
c|| classify user's entry

	Function TIX ENT ()

	Include 	'TI.inc'

	Character*2	cmd
	Equivalence	(cmd, ticENT (2: 3))

	Character*80	temp

c**	If (.not. ticHFT) Then		! must test helpfile
c**		ticHFT	=.true.
c**		ticHFO	=TI HELP (' ')	! test helpfile open
c**	Endif

c| fetch input line
c  ----- ----- ----
  100	If (ticLMS .eq. 2) Then		! reading learn file
		Read (ticLMU, '(a)', End =101) ticENT
		Type '(a,a)',	'+', ticENT (1: ch last (tic ENT))
		go to 102

  101			Close (Unit =ticLMU)
			ticLMS	=0	! learn mode inactive
			go to 100

  102		Continue

	Else
		Accept '(a)', ticENT	! get string entry
		Call TIX EIL
	Endif

	Call ch ccsp (ticENT)		! ctl chars to spaces

c| blank line
c  ----- ----
	If (ticENT .eq. ' ') Then
		TIX ENT	=ticEBL

c| comment
c  -------
	Else If (ch prfx (ticENT, '!') .ne. 0) Then
		TIX ENT	=ticETI		! ask another
			Type *, ' '

c| display command menu
c  ------- ------- ----
	Else If (ch prfx (ticENT, '/') .ne. 0) Then
		jslant	=ch prfx (ticENT, '/')
		temp	=ticENT (jslant: )
		ticENT	=temp
		Call ch lcuc (cmd)
		If (ticENT (2: ) .eq. ' ') Then
			Type 1, tiqPID
    1			Format (
     x
     x			/,t3, a, ' commands are:',
     x			/,t5, '/P',
     x			/,t10,'returns to previous question',
     x			/,t5, '/V COMMAND',
     x			/,t10,'calls VMS to execute COMMAND and waits',
     x			/,t10,'for completion of COMMAND',
     x			/,t5, '/VP COMMAND',
     x			/,t10,'like /V COMMAND, but program execution',
     x			/,t10,'continues in parallel with COMMAND',
     x			/,t5, '/Q',
     x			/,t10,'quits (stops program)')

c|	learn options
c	----- -------
			If (ticLMU .ne. 0) Then
					! learn mode ok
				If (ticLMS .eq. 0) Then
					Type 1001
 1001					Format (t5,'/L',
     x					/,t10,'start learning')
				Else If (ticLMS .eq. 1) Then
					Type 1002
 1002					Format (t5,'/L',
     x					/,t10,'stop learning')
				Endif
				If (ticLMS .eq. 0) Then
					Type 1003
 1003					Format (t5,'/@',
     x					/,t10,'fetch commands from file')
				Endif
			Endif

c|	user routine
c	---- -------
			If (TI USER (0)) Then
				Type 2
    2				Format (t5, '/U',
     x					/,t10,'calls special ',
     x					'user routine')
			Endif
c|	help for this node
c	---- --- ---- ----
c**			If (ticHFE) Then! this node has entry
c**				Type 3
c**    3				Format (t5, '/H',
c**     x					/,t10,'display help for this',
c**     x					' question')
c**			Endif

c|	other help
c	----- ----
c**			If (ticHFO) Then! helpfile open
c**				Type 4
c**    4				Format (t5, '/H KEY',
c**     x					/,t10,'display help for ',
c**     x					'paragraph "KEY"')
c**				Type 5
c**    5				Format (t5, '/HI',
c**     x					/,t10,'display helpfile ',
c**     x					'index')
c**			Endif

			Type '(/)'
			TIX ENT	=ticETI
c| entered command
c  ------- -------
c|	return predecessor
c	------ -----------
		Else If (cmd .eq. 'P ') Then
			TIX ENT	=ticERP

c|	execute VMS command and wait
c	------- --- ------- --- ----
		Else If (cmd .eq. 'V ') Then
			Type *, ' '
			Call VX DCL (ticENT (4: ), .true.)
			Type '(/)'
			TIX ENT	=ticETI

c|	execute VMS command and continue
c	------- --- ------- --- --------
		Else If (cmd .eq. 'VP') Then
			Type *, ' '
			Call VX DCL (ticENT (4: ), .false.)
			Type '(/)'
			TIX ENT	=ticETI

c|	call user routine
c	---- ---- -------
		Else If (cmd .eq. 'U ') Then
			If (TI USER (0)) Then	! user-written user routine
				Call TI USER (1)
			Else
				Type '(/,1x,a,a,//)',
     x					'>>>this program has no',
     x					' user routine'
			Endif
			TIX ENT	=ticETI

c|	learn file
c	----- ----
		Else If (
     x			(ticENT (2: 2) .eq. 'L') .or.
     x			(ticENT (2: 2) .eq. '@')
     x		) Then
			Call TIX LRN	! handle learn file
			TIX ENT	=ticETI
			If (ticLMS .eq. 1) Then
				Return	! don't record /L
				Endif

c|	echo
c	----
		Else If (ticENT (2: 2) .eq. 'E') Then
			ticLME	=.not. ticLME
			TIX ENT	=ticETI
			ticHPL	=ticENT (3: 4) .eq. 'HP'
					! if HP LaserJet
			ticHU1		=' &dD'
			ticHU1 (1:1)	=Char ('1B'x)
			ticHU2		=' &d@'
			ticHU2 (1:1)	=Char ('1B'x)
			Call TIX EIL	! echo it
			Type *, ' '

c| helpfile
c  --------
c**		Else If (ticENT (2: 2) .eq. 'H') Then
c**			Call TIX HFP ()	! helpfile processor

c| exit
c  ----
		Else If (ticENT (2: 2) .eq. 'Q') Then
			Call Exit

c| unrecognizable
c  --------------
		Else
			Call TIX DRC	! don't recognize
		Endif
	Else
		TIX ENT	=ticENR		! entry not recognized
	Endif

c| copy to learn file
c  ---- -- ----- ----
	If (ticLMS .eq. 1) Then		! writing learn file
		Write (ticLMU, '(a)') ticENT (1: ch last (ticENT))
	Endif
	End
c||-------------------------------------| TIX EIL
c|| echo input line

	Subroutine TIX EIL

	Include 'TI.inc'

	If (ticLME) Then	! echo reassigned sys$input
		If (ticHPL) Then! HP LaserJet
			Type '(a,a,a,a)', '+', ticHU1,
     x				ticENT (1: ch last (ticENT)),
     x				ticHU2
		Else
			Type '(a,a)',	'+',
     x				ticENT (1: ch last (tic ENT))
		Endif
	Endif
	End
c||-------------------------------------| TI LRN
c|| enable learn mode

	Subroutine TI LRN (LUNIT, PID)

	Character*(*)	PID		! caller's id string

	Include 'TI.inc'

	ticLMU	=LUNIT			! remember caller's
	ticPID	=PID			!  unit, id

	ticLMF	=' '
	End
c||-------------------------------------| TIX DRC
c|| don't recognize command

	Subroutine TIX DRC ()

	Include 'TI.inc'

	Type '(/,1x,a,//)',
     x		'>>>don''t recognize "/" command'
	TIX ENT	=ticETI
	End
c||-------------------------------------| TIX LRN
c|| handle learn files

	Subroutine TIX LRN

	Include 'TI.inc'

	Character*1	cmd
	Equivalence	(cmd, ticENT (2:2))

	Character*3	status
	Integer		state

	If (ticLMU .le. 0) Then
		Type '(/,1x,a,//)',	'>>> learn files illegal'
		Return
	Endif

	If (cmd .eq. 'L') Then

		If (ticLMS .eq. 0) Then
			state	=1
			status	='NEW'
			go to 1000

		Else
			Type 101, ticLMF (1: ch last (ticLMF))
  101			Format (/, '   >Close "learn" file ',a,//)
			Close (Unit =ticLMU)
			ticLMS	=0
		Endif

	Else If (cmd .eq. '@') Then

		If (ticLMS .eq. 0) Then
			state	=2
			status	='OLD'
			go to 1000

		Else
			Type '(/,1x,a,//)',
     x			'>>> @ illegal while learning'

		Endif
	Endif
	Return

c| open a learn file
c  ---- - ----- ----
 1000	If (ticENT (3: ) .ne. ' ') Then
		ticLMF	=ticENT (3: )
	Endif
	If (ticLMF .eq. ' ') Then
		Type '(/,1x,a,$)',	'?filespec: '
 1001		Accept '(a)', ticLMF
		If (ticLMF .eq. ' ') Then
			Type '(//)'
			Return		! changed his mind
		Endif
	Endif

 1002	If (status .eq. 'OLD') Then

		Open (
     x			Unit	=ticLMU,
     x			File	=ticLMF,
     x			Status	=status,
     x			Err	=1003,
     x			READONLY)
	Else
		Open (
     x			Unit	=ticLMU,
     x			File	=ticLMF,
     x			Status	=status,
     x			Err	=1003,
     x
     x			CARRIAGECONTROL	='LIST')
	Endif
	ticLMS	=state
	If (ticLMS .eq. 1) Then
		Write (ticLMU, 1005) tiqPID, ticPID (1: ch last (ticPID))
 1005		Format ('! ',a,' "learn" file for program ', a)
	Endif
	Return

 1003		Type 1004, ticLMF (1: ch last (ticLMF))
 1004		Format (/,' Unable to open "',a,'" ',$)

		go to 1001		! try opening
	End

