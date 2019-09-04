	Subroutine TI LOG (LOG, *)
c||-------------------------------------| TI LOG
c|| accept logical

	Include		'TI.inc'

	Logical		LOG

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
