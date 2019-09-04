CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INTEGER FUNCTION WIND_DECOMPRESS(BYTE_VAR,CODE_C)
!
! Data from WIND SMS is compressed (logarithmically encoded so that large numbers will 
! fit into a byte). This routine decompresses a byte into its integer equivalent, so
! arithmetic operations may be performed upon it. BYTE_VAR is the byte variable to be
! decompressed, CODE_C is a logical variable that is true if Compression Code C is to
! be used; if its false, Compression Code A is used instead. An I*4 is returned.
!
!	This version:    9-DEC-1994
!
!     Creation:
!	JOHN PAQUETTE	12-OCT-1994
!
!     Revisions:
!	 9-DEC-1994	J.PAQUETTE	corrected error in A compression code:
!					exponent now allowed to exceed 11
!

      IMPLICIT NONE
C
      BYTE BYTE_VAR
      INTEGER*4	INTEGER_VAR,EXPONENT,MANTISSA
      LOGICAL CODE_C
C
C Convert byte to integer, so that we can use IBITS on it
C
      INTEGER_VAR = BYTE_VAR
      IF(CODE_C)THEN                            ! Compression Code C
        IF(IBITS(INTEGER_VAR,6,2).NE.3)THEN     ! Test if either bits 6 or 7 of argument are clear
C
C Get mantissa and exponent. Since either bit 6 or bit 7 (or both) of INTEGER_VAR were
C zero, we have a 4 bit mantissa and a 4 bit exponent.
C
          MANTISSA = IBITS(INTEGER_VAR,0,4)
          EXPONENT = IBITS(INTEGER_VAR,4,4)
C
C Decompress. For EXPONENT=11, MANTSSA=15, we get 31,744 (maximum possible value).
C
          IF(EXPONENT.EQ.0)THEN
            WIND_DECOMPRESS = MANTISSA
          ELSEIF((EXPONENT.GE.1).AND.(EXPONENT.LE.11))THEN
            WIND_DECOMPRESS = (16 + MANTISSA)*2**(EXPONENT-1)
          ENDIF
C
        ELSE                                    ! Test if BOTH bits 6 and 7 of argument are set
C
C Get mantissa and exponent. If bits 6 and 7 of INTEGER_VAR are both set, we have a
C 3 bit mantissa and a 5 bit exponent.
C
          MANTISSA = IBITS(INTEGER_VAR,0,3)
          EXPONENT = IBITS(INTEGER_VAR,3,5)
C
C Decompress. For EXPONENT+31, and M=7 we get 7,864,320 (maximum possible value).
C
          WIND_DECOMPRESS = (8 + MANTISSA)*2**(EXPONENT-12)
C
        ENDIF
      ELSE                                      ! Compression Code A
C
C Get mantissa and exponent. Since we are using Code A,  we always have a 4 bit mantissa 
C and a 4 bit exponent.
C
        MANTISSA = IBITS(INTEGER_VAR,0,4)
        EXPONENT = IBITS(INTEGER_VAR,4,4)
C
C Decompress. For EXPONENT=MANTSSA=15, we get 507,904  (maximum possible value).
C
        IF(EXPONENT.EQ.0)THEN
          WIND_DECOMPRESS = MANTISSA
        ELSEIF((EXPONENT.GE.1).AND.(EXPONENT.LE.15))THEN
          WIND_DECOMPRESS = (16 + MANTISSA)*2**(EXPONENT-1)
        ENDIF
C
      ENDIF
C
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      BYTE FUNCTION WIND_COMPRESS(INTEGER_VAR,CODE_C)
!
! Data from WIND SMS is compressed (logarithmically encoded so that large numbers will 
! fit into a byte). This routine compresses an integer into a byte using one of two
! coding schemes. INTEGER_VAR is the variable to be decompressed, CODE_C is a logical 
! variable that is true if Compression Code C is to be used; if its false, Compression 
! Code A is used instead. An I*4 is returned.
!
!	This version:	13-OCT-1994
!
!     Creation:
!	JOHN PAQUETTE,	13-OCT-1994
!       L. BLEAU
!
!     Revisions:
!
!

      IMPLICIT NONE
C
      BYTE BYTE_VAR
      INTEGER*4	INTEGER_VAR,EXPONENT,MANTISSA,RESULT
      LOGICAL CODE_C,EXP_4_BITS
      REAL TEMP,LOG
C
      IF(CODE_C)THEN                            ! Compression Code C
        EXP_4_BITS = .TRUE.
        IF(INTEGER_VAR .LE. 15)THEN             ! Take care of simple case first
          EXPONENT = 0
          MANTISSA = INTEGER_VAR
        ELSEIF(INTEGER_VAR .GE. 7864320)THEN    ! Take care of upper limit
          EXPONENT = 31
          MANTISSA = 7
          EXP_4_BITS = .FALSE.
        ELSEIF(INTEGER_VAR .LE. 32767)THEN      ! Use 4-bit exponent and mantissa
C
C Compute lowest power of two of which is greater than INTEGER_VAR
C If you want to round instead of truncate, uncomment the following lines and comment out
C the 3 lines that follow.
C          TEMP = LOG(INTEGER_VAR/15.5)/LOG(2.0)
C          EXPONENT = INT(TEMP)
C          IF(FLOAT(EXPONENT) .NE. TEMP) EXPONENT = EXPONENT + 1
          TEMP = LOG(FLOAT(INTEGER_VAR))/LOG(2.0)
          EXPONENT = INT(TEMP) - 3
          MANTISSA = (INTEGER_VAR / (2 ** (EXPONENT-1) )) - 16
C
C Due to the limited pecision of a computer, its possible that borderline cases may have
C been assigned the wrong exponent in the logarithm above. If the calculated exponent is
C too small, then the calculated mantissa will be larger than 7. If the calculated exponent 
C is too large, then the calculated mantissa will be less than 0.
C
          IF(MANTISSA.LT.0)THEN
            EXPONENT=EXPONENT-1
            MANTISSA = (INTEGER_VAR / (2 ** (EXPONENT-1) )) - 16
          ELSEIF(MANTISSA.GT.16)THEN
            EXPONENT=EXPONENT+1
            MANTISSA = (INTEGER_VAR / (2 ** (EXPONENT-1) )) - 16
          ENDIF
C
        ELSE                                    ! Use 5-bit exponent, 3-bit mantissa
          EXP_4_BITS = .FALSE.
C
C Compute lowest power of two of which is greater than INTEGER_VAR
C If you want to round instead of truncate, uncomment the following lines and comment out
C the 3 lines that follow.
C          TEMP = LOG(INTEGER_VAR/15.5)/LOG(2.0)
C          EXPONENT = INT(TEMP)
C          IF(FLOAT(EXPONENT) .NE. TEMP) EXPONENT = EXPONENT + 1
          TEMP = LOG(FLOAT(INTEGER_VAR))/LOG(2.0)
          EXPONENT = INT(TEMP) +  9
          MANTISSA = (INTEGER_VAR / (2 ** (EXPONENT-12) )) - 8
C
C Due to the limited pecision of a computer, its possible that borderline cases may have
C been assigned the wrong exponent in the logarithm above. If the calculated exponent is
C too small, then the calculated mantissa will be larger than 7. If the calculated exponent 
C is too large, then the calculated mantissa will be less than 0.
C
          IF(MANTISSA.LT.0)THEN
            EXPONENT=EXPONENT-1
            MANTISSA = (INTEGER_VAR / (2 ** (EXPONENT-12) )) - 8
          ELSEIF(MANTISSA.GT.7)THEN
            EXPONENT=EXPONENT+1
            MANTISSA = (INTEGER_VAR / (2 ** (EXPONENT-12) )) - 8
          ENDIF
        ENDIF
C
C Convert exponent/mantissa into byte
C
        IF(EXP_4_BITS)THEN
          RESULT = EXPONENT*16 + MANTISSA
        ELSE
          RESULT = EXPONENT*8 + MANTISSA
        ENDIF
      ELSE                                      ! Compression Code A
        IF(INTEGER_VAR .LE. 15)THEN             ! Take care of simple case first
          EXPONENT = 0
          MANTISSA = INTEGER_VAR
        ELSEIF(INTEGER_VAR .GE. 507904)THEN     ! Take care of upper limit
          EXPONENT = 15
          MANTISSA = 15
        ELSE
C
C Compute lowest power of two of which is greater than INTEGER_VAR
C If you want to round instead of truncate, uncomment the following lines and comment out
C the 3 lines that follow.
C          TEMP = LOG(INTEGER_VAR/15.5)/LOG(2.0)
C          EXPONENT = INT(TEMP)
C          IF(FLOAT(EXPONENT) .NE. TEMP) EXPONENT = EXPONENT + 1
          TEMP = LOG(FLOAT(INTEGER_VAR))/LOG(2.0)
          EXPONENT = INT(TEMP) - 3
          MANTISSA = (INTEGER_VAR / (2 ** (EXPONENT-1) )) - 16
C
C Due to the limited pecision of a computer, its possible that borderline cases may have
C been assigned the wrong exponent in the logarithm above. If the calculated exponent is
C too small, then the calculated mantissa will be larger than 7. If the calculated exponent 
C is too large, then the calculated mantissa will be less than 0.
C
          IF(MANTISSA.LT.0)THEN
            EXPONENT=EXPONENT-1
            MANTISSA = (INTEGER_VAR / (2 ** (EXPONENT-1) )) - 16
          ELSEIF(MANTISSA.GT.16)THEN
            EXPONENT=EXPONENT+1
            MANTISSA = (INTEGER_VAR / (2 ** (EXPONENT-1) )) - 16
          ENDIF
C
        ENDIF
C
C Convert exponent/mantissa into byte
C
        RESULT = EXPONENT*16 + MANTISSA
C
      ENDIF
C
C Since Fortran treats BYTE quantities as signed numbers, we encoded the result
C into an INTEGER*4 variable first, then subtract 256 if >128 to keep it in the
C range [-128,127]
C
      IF(RESULT .GE. 128) RESULT = RESULT - 256
      WIND_COMPRESS = RESULT
C
      RETURN
      END
