c||=====================================| TI.inc
c|| terminal input common
	Character*(*)	tiqPID
	Parameter	(tiqPID	='TI/1.2')
c| functions
c  ---------
	Integer		TIX ENT		! classify user's entry fcn
	Logical		TI USER		! user fcn
	Integer		ch last		! last nonspace fcn
	Integer		ch prfx		! test prefix fcn
c| TIX ENT outcomes
c  --- --- --------
	Integer		ticENR,	ticEBL,	ticERP,	ticETI, jent
	Parameter	(ticENR	=0)	! entry not recognized
	Parameter	(ticEBL	=1)	! entry is blank line
	Parameter	(ticERP	=2)	! entry is return to previous question
	Parameter	(ticETI	=3)	! entry is TI command
	Character*80	ticENT		! line entered by user
	Common		/ticomc/	ticENT
c| helpfile
c  --------
	Logical		ticHFO		! helpfile currently open
	Logical		ticHFT		! if helpfile has been tested
	Common		/ticomn/	ticHFO,	ticHFT
c| learn mode
c  ----- ----
	Integer		ticLMU		! file unit
	Integer		ticLMS		! state
					!  0 => no file open
					!  1 => writing
					!  2 => reading
	Logical		ticLME		! if TTY input to be echoed
	Logical		ticHPL		! if printer is HP LaserJet
	Common		/ticomn/	ticLMU,	ticLMS,	ticLME,	ticHPL
	Character*80	ticLMF		! filespec
	Character*20	ticPID		! calling pgm's id string
	Character*4	ticHU1		! HP begin underline seqence
	Character*4	ticHU2		! HP end   "         "
	common		/ticomc/	ticLMF, ticPID,	ticHU1,	ticHU2
