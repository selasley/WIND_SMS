	Subroutine TI REAL (REAL, *)
c||-------------------------------------| TI REAL
c|| accept real

	Include		'TI.inc'

	Real		REAL,		temp

	Integer		jfrst

	Character*7	efrmt		! to encode E format

  100	Write (ticENT, '(1pe9.2)') REAL

	If (ticENT .eq. ' ') Then	! find first nonblank
		jfrst	=2
	Else
		jfrst	=1
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
