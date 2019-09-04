	Subroutine TI BYTE (BYTES, NBYTES, *)
c||-------------------------------------| TI BYTE
c|| accept character

	Include		'TI.inc'

	Integer*4	JJ,	NBYTES

	Byte		BYTES (NBYTES)

  100	ticENT	=' '
	Do 101 JJ = 1, NBYTES
		ticENT (JJ: JJ)	=Char (BYTES (JJ))
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
		Do 122 JJ = 1, NBYTES
			BYTES (JJ) =Ichar (ticENT (JJ:JJ))
  122		Continue
	Endif
c**	call TIX RHK			! remove helpfile key
	End
