      SUBROUTINE NMNQ_BINS(UNQ_MAX,UMQMAX,UMQMIN,NQ_MPQ,
     .		           UNM_MAX,UMMAX, UMMIN, NM_M,
     .      		   ELOW,EHIGH,TLOW,THIGH,
     .                     AM,BM,C1_EQ,C2A_EQ,C2B_EQ,D1_EQ,D2_EQ,
     .                     EMQ, BK_C2, BK_MQ, KM, KQ)

C*******************************************************************
C     This SUBROUTINE calculates ln bin boundaris for Nm, and Nq   *
C     given the number of bins requested by the user               * 
C     Original version 24 Jan 95 by ABG                            *
C     History:
C                
c*******************************************************************
      IMPLICIT NONE

      INTEGER*4 UNM_MAX, UNQ_MAX,   	!max bin values (-1 = use default)
     .           NM_MAX,  NQ_MAX

      REAL*4 UMMAX,UMMIN,   !measurable mass range in amu (-1 = use default)
     .       UMQMAX,UMQMIN, !measurable m/q range in amu/e (-1 = use default)
     .       MMAX,MMIN,   !measurable mass range in amu 
     .       MQMAX,MQMIN, !measurable m/q range in amu/e 
     .       KM,KQ        !NM, NQ resolution parameters
   
      REAL*4 NQ_MPQ(0:1000),    ! bin boundary values (m/q and m)
     .       NM_M  (0:500 )

      INTEGER*4 I,J,K

      REAL*4 ELOW,EHIGH,TLOW,THIGH,
     .       AM(6),BM(6),C1_EQ,C2A_EQ,C2B_EQ,D1_EQ,D2_EQ,
     .       EMQ(6), BK_MQ
      INTEGER*4  BK_C2
          
C     determine if default (i.e., dpu) binning will be used, in whole or
C     in part, or not at all

      CALL NMNQ_COEFF      (UNQ_MAX,UMQMAX,UMQMIN,    !user input
     .                      UNM_MAX,UMMAX, UMMIN,
     .                      ELOW,EHIGH,TLOW,THIGH,      !return values
     .                      NQ_MAX,MQMAX,MQMIN,
     .                      NM_MAX,MMAX ,MMIN,
     .                      KM,KQ,
     .                      AM,BM,C1_EQ,C2A_EQ,C2B_EQ,
     .                      D1_EQ,D2_EQ,EMQ,
     .                      BK_C2, BK_MQ)


C  ************************************************************
C  *              calculate binning parameters                *
C  *              M(lower bound of Nm) = Mmin * kM**(Nm-1)    *
C  *              M/Q(lower bound of Nq) = MQmin * kQ**(Nq-1) *
C  ************************************************************


      DO I = 0,500
         NM_M(I) = 0.
         IF(I.GT.0.  .AND.  I.LE.NM_MAX) 
     .        NM_M(I) = MMIN * KM**(FLOAT(I)-1.)
      END DO

      DO I = 0, 1000
         NQ_MPQ(I) = 0.
         IF(I.GT.0  .AND.  I.LE.NQ_MAX+1) 
     .        NQ_MPQ(I) = MQMIN * KQ**(FLOAT(I)-1.)
      END DO

      RETURN
      END
