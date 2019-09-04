C     WIND_Lun.INC
C
C     Contains all the assigned logical units for all files
C
C    This version:    9-FEB-1995
C
C    CREATION:
C        4-Feb-1994     H. Broer, UMD
C
C    Revision:
C       22-Jun_1994     H. Broer, UMD   added L1_Lun
C        4-NOV-1994     L. Bleau        added TTOUT for output stream
C	 9-FEB-1995	l. Bleau	added LOGF_LUN as unit for log file
C
C                                       DESIGNATION
C                                       -----------
      INTEGER*4 KB,                     ! keyboard
     +          TTOUT,                  ! terminal output
     +          LOGF_Lun,               ! log file
     +          L0_Lun,                 ! L0 File
     +          L1_Lun,                 ! L1 File
     +          SFDU_Lun                ! SFDU data file
      PARAMETER ( KB = 5 ,
     +            TTOUT = 6 ,
     +            LOGF_Lun = 7,         ! log file
     +            L0_Lun = 11 ,
     +            L1_Lun = 13 ,
     +            SFDU_Lun = 12 )
