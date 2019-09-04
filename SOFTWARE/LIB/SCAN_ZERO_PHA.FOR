      SUBROUTINE SCAN_ZERO_PHA( PHA, NBYTES, PHASIZE, ZERO_COUNT, WHERE )
C
C This subroutine scans a set of PHA words, counting the number of zero PHA
C words which are non-zero.  This count (the number of zero PHA words) is
C returned to the caller.  Since one of the arguments is the size (in bytes) of
C a PHA word this routine can be used to examine PHA words from any of the
C sensors. 
C
C Parameters:
C
C   PHA:
C          A BYTE array containing the PHA words for a given sensor.
C
C   NBYTES:
C          An INTEGER giving the length (in bytes) of the array PHA.  This is
C          not the same as the number of PHA words, since a PHA word occupies
C          either 2 or 4 bytes, depending on the sensor.
C
C   PHASIZE:
C          An INTEGER specifying the number of bytes contained in a single PHA
C          word.
C
C   ZERO_COUNT:
C          An INTEGER which will contain, upon return, the number of zero
C          PHA words.
C
C   WHERE:
C          An INTEGER array of length 100.  Each element of WHERE gives the
C          PHA word number (1 origin) of a PHA word which is zero.  The number
C          of valid entries in WHERE is given by the argument ZERO_COUNT.
C
C
C Created:  30-JAN-1995   Larry Bleau
C
C This version:  1-FEB-1995
C
C Revisions:
C  1-FEB-1995     L. Bleau   added comments, changed NZERO to ZERO_COUNT
C
      IMPLICIT NONE
      INTEGER NBYTES,ZERO_COUNT,PHASIZE
      INTEGER WHERE(100)
      BYTE PHA(NBYTES)
C
      INTEGER NWORDS,I,J
      LOGICAL GOOD
C
      ZERO_COUNT = 0
      NWORDS = NBYTES / PHASIZE
      IF (NWORDS .EQ. 0) RETURN
C
      DO J=0,NWORDS-1
        GOOD = .FALSE.
C
C If any of the bytes which comprise the PHA word is non-zero count this PHA
C word as good
C
        DO I=1,PHASIZE
          IF (PHA(J*PHASIZE+I) .NE. 0) GOOD = .TRUE.
        ENDDO
C Found one not good, same its index
        IF (.NOT. GOOD) THEN
          ZERO_COUNT = ZERO_COUNT + 1
          WHERE(ZERO_COUNT) = J
        ENDIF
      ENDDO
      RETURN
      END
