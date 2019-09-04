	Subroutine TIX EIL
c||-------------------------------------| TIX EIL
c|| echo input line

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

