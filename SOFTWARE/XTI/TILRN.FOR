	Subroutine TI LRN (LUNIT, PID)
c||-------------------------------------| TI LRN
c|| enable learn mode

	Include 'TI.inc'

	Integer*4	LUNIT

	Character*(*)	PID		! caller's id string

	ticLMU	=LUNIT			! remember caller's
	ticPID	=PID			!  unit, id

	ticLMF	=' '
	End
