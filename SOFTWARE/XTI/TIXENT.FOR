	Function TIX ENT ()
c||-------------------------------------| TIX ENT
c|| classify user's entry

	Include 	'TI.inc'

	Integer*4	jslant

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
C*			If (TI USER (0)) Then
C*				Type 2
C*    2				Format (t5, '/U',
C*     x					/,t10,'calls special ',
C*     x					'user routine')
C*			Endif
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
c*		Else If (cmd .eq. 'U ') Then
C*			If (TI USER (0)) Then	! user-written user routine
C*				Call TI USER (1)
C*			Else
C*				Type '(/,1x,a,a,//)',
C*     x					'>>>this program has no',
C*     x					' user routine'
C*			Endif
C*			TIX ENT	=ticETI
C*
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
