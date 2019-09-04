	SUBROUTINE TCONA ( TIME, ASCT )
	ENTRY      TCONC ( TIME, ASCT )
!
!	This routine converts a REAL*8 TIME value into an ASCII string
!	of the form: "yyyy / ddd / hrmn:ss.s" (22 characters)
!
!	using GEOTAIL time standard
!
!	 1-JUL-1993	H.Broer	replacement for HOVEY routine
!				will be placed in XTI_LIB.olb
!
	IMPLICIT	NONE

	REAL*8		TIME

	REAL*4		Sec

	INTEGER*4	yr,	dy,	Msec,	Micsec,	HrMin,	status

	CHARACTER*22	ASCT

	Call	EPIC_EPOCH_TO_JULIAN( yr, dy, Msec, Micsec, TIME, status )
	Call	EPIC_Msec_TO_HrMinSec( Msec, HrMin, Sec, status )

	ASCT	= '1993 / 173 / 1257:59.0'

	ENCODE ( 4, 1001, ASCT( 1: 4) ) yr
	ENCODE ( 3, 1002, ASCT( 8:10) ) dy
	ENCODE ( 4, 1001, ASCT(14:17) ) HrMin
	ENCODE ( 4, 1003, ASCT(19:22) ) Sec

	RETURN

1001	FORMAT ( I4.4 )
1002	FORMAT ( I3.3 )
1003	FORMAT ( F4.1 )

	END
