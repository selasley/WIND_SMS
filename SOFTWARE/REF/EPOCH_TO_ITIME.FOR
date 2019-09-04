	SUBROUTINE EPOCH_TO_ITIME( RTIME, Time, STATUS )
C
C	Convert the time given as a REAL*8 in milliseconds since 3-JAN-0000 B.C.
C	into a structure of the following format:
C
C		      STRUCTURE /T/
C		          INTEGER*2	YEAR,	DOY
C		          BYTE		HR,	MIN,	SEC,	CSEC
C		      END STRUCTURE ! /T/
C
C	This version:	9-FEB-1995
C
C  CREATION:
C	15-JUN-1994	H.BROER	University of Maryland, Physics Department,
C				Space Physics Group
C	28-JUN-1994		regroup INPUT and OUTPUT
C	 9-FEB-1995	L. Bleau	changed '!' to 'C' in program comments
C	 9-FEB-1995	L. Bleau	change include stmt to use EDBDEF.INC
C					and EDBVARS.INC instead of EDB.INC only
C
C------------------------------------------------------------------------------
CDEC$	IDENT	'1.01'

      IMPLICIT NONE

      INCLUDE	'INC_DIR:EDBDEF.INC/NOLIST'
      INCLUDE	'INC_DIR:EDBVARS.INC/NOLIST'

      REAL*8	RTIME

      INTEGER*4	YEAR,
     +		DOY,
     +		HOUR,
     +		MINUTE,
     +		SECOND,
     +		CSEC,
     +		MILLIS,
     +		MICROS,
     +		STATUS

      RECORD /T/ Time


      CALL WIND_EPOCH_TO_GREG( YEAR, DOY, MILLIS, MICROS, RTIME, STATUS )

      CSEC	= MOD( MILLIS/10, 100 )
      SECOND	= MOD( MILLIS/1000, 60 )
      MINUTE	= MOD( MILLIS/60000, 60 )
      HOUR	= MILLIS / 3600000

      Time.YEAR	= YEAR
      Time.DOY	= DOY
      Time.HR	= HOUR
      Time.MIN	= MINUTE
      Time.SEC	= SECOND
      Time.CSEC	= CSEC

      RETURN

      END
