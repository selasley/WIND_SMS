	Subroutine TIX LRN
c||-------------------------------------| TIX LRN
c|| handle learn files

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

