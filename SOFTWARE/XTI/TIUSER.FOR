	Function TI USER (IFCN)
c||-------------------------------------| TI USER
c|| special user routine

	Include 'TI.inc'

	Integer*4	IFCN

	If (IFCN .eq. 0) Then
		TI USER =.false.	! not user written
		Return
	Endif
	End
