       INTEGER*4 FUNCTION DECOMP( IVAL, CCODE )
C
C	function to decompress rate values from Ulysses/SWICS DPU to 
C       integer*4. Algorithm see DPU manual, p. 25f.
C	Written by r.v.steiger, Nov. 19, 1990
C       !1 Oct 91:  error corrected in which from A to C code
C
       IMPLICIT NONE
C
       INTEGER*4
     .   IVAL, CCODE, 
     .   MANT, EXPO
C
       IF ( CCODE.EQ.0 .OR. 
     .      ( CCODE.EQ.1 .AND. ISHFT(IVAL,-6).NE.3 ) ) THEN   !1
          MANT = IAND( IVAL, 15 )
          EXPO = ISHFT( IAND( IVAL, 240 ), -4 )
          IF ( EXPO.EQ.0 ) THEN
       	     DECOMP = MANT
       	  ELSE
             DECOMP = ISHFT( MANT+16, EXPO-1 )
          END IF
       ELSE
          MANT = IAND( IVAL, 7 )
          EXPO = ISHFT( IAND( IVAL, 248 ), -3 )
       	  DECOMP = ISHFT( MANT+8, EXPO-12 )
       END IF

       RETURN
       END
