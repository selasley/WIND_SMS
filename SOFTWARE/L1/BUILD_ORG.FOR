      SUBROUTINE BUILD_ORG( BUILT )                          ! 16-NOV-94
C
C  Builds the original array of PHA words as reported by the DPU
C
C
C       16-NOV-1994     J.PAQUETTE      altered to use 60 spins instead of 32
C
      IMPLICIT NONE
C
      INCLUDE 'INC_DIR:STICS.INC'
C
      COMMON /ORGPHA/   ORG_PHA,SINDEX, MAXINDEX
C
C                      index, rng, sect, nedb          rng, sect, nedb
      INTEGER*4 ORG_PHA(0:15, 0:2, 0:15, 0:59), SINDEX(0:2, 0:15, 0:59),
     :          MAXINDEX,       EVNT,   INDEX,
     :          NEDB,   RNG,    SECT,   STRT
C
      LOGICAL*1 BUILT
C
      DO  NEDB = 0, 59
          DO  EVNT = 0, S_PHA_NM1( NEDB )
              call mvbits (PHA( EVNT, NEDB), 12, 4, SECT, 0)	! sector
              call mvbits (PHA( EVNT, NEDB), 25, 5, STRT, 0)	! start and range
              RNG  = mod(STRT, 3)                               ! range
              STRT = STRT / 3                                   ! start
              INDEX = SINDEX( RNG, SECT, NEDB )                 ! CURRENT INDEX
              IF( INDEX .LT. 15 )   THEN
                  ORG_PHA( INDEX, RNG, SECT, NEDB ) = PHA( EVNT, NEDB )

                  SINDEX( RNG, SECT, NEDB) = INDEX + 1              ! INDEX

                  IF( MAXINDEX .LT. INDEX )  MAXINDEX = MAXINDEX + 1
              END IF ! INDEX
          END DO ! EVNT
      END DO ! NEDB
      BUILT = .TRUE.
      RETURN

      END
