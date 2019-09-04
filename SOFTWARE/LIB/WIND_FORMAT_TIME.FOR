	CHARACTER*30 FUNCTION WIND_FORMAT_TIME(TIMESTRC)
C
C This procedure takes as its input a RECORD of structure type T, which
C contains times.  It formats the fields of the structure using an internal
C WRITE statement, storing the result into a CHARACTER variable of length 30.
C This formatted string is returned as the value of the function.
C
	IMPLICIT NONE
        INCLUDE 'INC_DIR:EDB.INC/NOLIST'
	RECORD /T/ TIMESTRC
	CHARACTER*30 TEMP
C
	WRITE(TEMP,1001) TIMESTRC.YEAR,TIMESTRC.DOY,TIMESTRC.HR,TIMESTRC.MIN,
     &			 TIMESTRC.SEC,TIMESTRC.CSEC
1001	FORMAT(6I5)
	WIND_FORMAT_TIME = TEMP
	RETURN
	END

	CHARACTER*30 FUNCTION WIND_FORMAT_TIME2(TIMESTRC, FMT_SPEC)
C
C This procedure takes as its input a RECORD of structure type T, which
C contains times.  It formats the fields of the structure using an internal
C WRITE statement, storing the result into a CHARACTER variable of length 30.
C The format specification used with the internal WRITE is given by the
C CHARACTER variable FMT_SPEC.  
C
C FMT_SPEC must have six integer field specifications (I format) and must not
C exceed 30 characters in total length.
C
C The formatted string is returned as the value of the function.
C
	IMPLICIT NONE
        INCLUDE 'INC_DIR:EDB.INC/NOLIST'
	RECORD /T/ TIMESTRC
	CHARACTER*(*) FMT_SPEC
	CHARACTER*30 TEMP
C
	WRITE(TEMP,FMT_SPEC) TIMESTRC.YEAR,TIMESTRC.DOY,TIMESTRC.HR,TIMESTRC.MIN,
     &			 TIMESTRC.SEC,TIMESTRC.CSEC
	WIND_FORMAT_TIME2 = TEMP
	RETURN
	END
