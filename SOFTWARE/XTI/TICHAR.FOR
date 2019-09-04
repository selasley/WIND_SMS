	Subroutine TI CHAR (CHAR, *)
c||-------------------------------------| TI CHAR
c|| accept character

	Include		'TI.inc'

	Character*(*)	CHAR

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
