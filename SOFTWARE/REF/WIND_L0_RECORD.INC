!	WIND_L0_RECORD.INC		8-APR-1994
!
!	COMMON ARRAY for creation of data file based on EDBs rather than
!	Minor Frames.
!
!	CREATION:
!	H.BROER	 8-APR-1994
!
!	REVISION:
!
!
      COMMON/L0_RECORD/	RecordId,	! Last Record Type assignment
     :			HDBRecId,	! HDB assigned Record ID
     :			HkRecId,	! Hk assigned Record ID
     :			CoreId,		! Core data assigned Record ID
     :			WCoreId,	! SWICS Core assigned Record ID
     :			SCoreId,	! STICS Core assigned Record ID
     :			MCoreId,	! MASS  Core assigned Record ID
     :			WPhaId,		! SWICS PHA assigned Record ID
     :			SPhaId,		! STICS PHA assigned Record ID
     :			MPhaId,		! MASS  PHA assigned Record ID
     :			HDBRecNum,	! HDB record running count
     :			HkRecNum,	! Hk record running count
     :			CoreNum,	! Core record running count
     :			WCoreNum,	! SWICS Core record running count
     :			SCoreNum,	! STICS Core record running count
     :			MCoreNum,	! MASS  Core record running count
     :			WPhaNum,	! SWICS PHA record running count
     :			SPhaNum,	! STICS PHA record running count
     :			MPhaNum		! MASS  PHA record running count
      INTEGER*2		RecordId,	! Last Record Type assigned
     :			HDBRecId,	! HDB assigned Record ID
     :			HkRecId,	! Hk assigned Record ID
     :			CoreId,		! Core data assigned Record ID
     :			WCoreId,	! SWICS Core assigned Record ID
     :			SCoreId,	! STICS Core assigned Record ID
     :			MCoreId,	! MASS  Core assigned Record ID
     :			WPhaId,		! SWICS PHA assigned Record ID
     :			SPhaId,		! STICS PHA assigned Record ID
     :			MPhaId,		! MASS  PHA assigned Record ID
     :			HDBRecNum,	! HDB record running count
     :			HkRecNum,	! Hk record running count
     :			CoreNum,	! Core record running count
     :			WCoreNum,	! SWICS Core record running count
     :			SCoreNum,	! STICS Core record running count
     :			MCoreNum,	! MASS  Core record running count
     :			WPhaNum,	! SWICS PHA record running count
     :			SPhaNum,	! STICS PHA record running count
     :			MPhaNum		! MASS  PHA record running count
