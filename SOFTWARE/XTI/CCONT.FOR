	SUBROUTINE CCONT ( ASCT, TIME )
!
!	This routine converts an ASCII string of the form:
!
!	yyyy / ddd / hrmn:ss.sss             (24 characters)
!
!	into a REAL*8 TIME value.  Using GEOTAIL time standard.
!
!	25-JUN-1993	H.Broer	replacement for Ulysses routine
!				will be placed in XTI_LIB.olb
!
	IMPLICIT	NONE

	REAL*8		TIME

	REAL*4		Sec

	INTEGER*4	yr,	dy,	Msec,	Micsec,	HrMin,	status

	CHARACTER	ASCT*80

	DATA		Micsec/0./

C	ASCT	= '1993 / 173 / 1257:59.000'

	DECODE ( 4, 1001, ASCT( 1: 4) ) yr
	DECODE ( 3, 1002, ASCT( 8:10) ) dy
	DECODE ( 4, 1001, ASCT(14:17) ) HrMin
	DECODE ( 6, 1003, ASCT(19:24) ) Sec

	Call	EPIC_HrMinSec_TO_Msec( Msec, HrMin, Sec, status )
	Call	EPIC_JULIAN_TO_EPOCH( yr, dy, Msec, Micsec, TIME, status )

1001	FORMAT ( I4 )
1002	FORMAT ( I3 )
1003	FORMAT ( F6.3 )

	END
