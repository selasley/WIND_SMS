!     WIND_Lun.INC
!
!     Contains all the assigned logical units for all files
!
!    This version	22-Jun-1994
!
!    CREATION:
!	 4-Feb-1994	H. Broer, UMD
!
!    Revision:
!	22-Jun_1994	H. Broer, UMD	added L1_Lun
!
!
!					DESIGNATION
!					-----------
      INTEGER*4	KB,			! KEYBOARD
     +		L0_Lun,			! L0 File
     +		L1_Lun,			! L1 File
     +		SFDU_Lun		! SFDU data file
      PARAMETER	( KB = 5 ,
     +		  L0_Lun = 11 ,
     +		  L1_Lun = 13 ,
     +		  SFDU_Lun = 12 )
