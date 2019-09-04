	Subroutine TI INT4 (INT, *)
c||-------------------------------------| TI INT4
c|| accept integer * 4

	Include		'TI.inc'

	Integer*4	INT,	itemp,	JJ

	Character*5	ifrmt		! to encode E format

  100	Write (ticENT, '(i6)') INT

	Do 101 JJ = 1, 5
		If (ticENT (JJ:JJ) .ne. ' ') Then
			go to 102
		Endif
  101	Continue

  102	Type 103, ticENT (JJ: 6)
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
c     	If (	(itempC.GT. 32767) .OR. 
c    1		(itempC.LT. -32766)) GOTO 120
		INT	=itemp
		Return

c|	invalid integer
c	------- -------
  120		Type '(/,1x,a,//)', '>>>expect integer*4 or "/"'
		go to 100
	Endif
c**	call TIX RHK			! remove helpfile key
	End
