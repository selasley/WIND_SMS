ç$ set def SMSDISK4:[WIND.L2DATA.STICS]
ç$!                  PHA
ç$ RUN SMS1:[WIND.SOFTWARE.STICS]STICS_MMQ_TABLE
çDATA_DIR:WI_L1_SMS_19990701_V01.DAT
çy
ç1
çy
ç2
ç-1
ç0
çn
ç-1
çn
çPHA_TABLE.OUT
ç-1/
ç$!                    MATRIX
ç$ RUN SMS1:[WIND.SOFTWARE.STICS]STICS_MQET_MATRIX
çDATA_DIR:WI_L1_SMS_19990701_V01.DAT
çy
çn
çn
çn
çMATRIX.OUT
ç-1/
ç$!                 DEFAULT RATE LIST
ç$ RUN SMS1:[WIND.SOFTWARE.STICS]WSTICS_RATES
çDATA_DIR:WI_L1_SMS_19990701_V01.DAT
çy
ç-1/
ç1
ç2
çWSTXRATE.OUT
çn
ç$!                         IDL
ç$ IDL
ç 
ç.run  sms1:[wind.software.stics]auto_stics_scatter
çMATRIX.OUT
çPHA_TABLE.OUT
ç1
çy
ç.run sms1:[wind.software.stics]dailyrateplt
çWSTXRATE.OUT
ç1
çexit
ç$!
ç$!                  PHA
ç$ RUN SMS1:[WIND.SOFTWARE.STICS]STICS_MMQ_TABLE
çDATA_DIR:WI_L1_SMS_20000101_V01.DAT
çy
ç1
çy
ç2
ç-1
ç0
çn
ç-1
çn
çPHA_TABLE.OUT
ç-1/
ç$!                    MATRIX
ç$ RUN SMS1:[WIND.SOFTWARE.STICS]STICS_MQET_MATRIX
çDATA_DIR:WI_L1_SMS_20000101_V01.DAT
çy
çn
çn
çn
çMATRIX.OUT
ç-1/
ç$!                 DEFAULT RATE LIST
ç$ RUN SMS1:[WIND.SOFTWARE.STICS]WSTICS_RATES
çDATA_DIR:WI_L1_SMS_20000101_V01.DAT
çy
ç-1/
ç1
ç2
çWSTXRATE.OUT
çn
ç$!                         IDL
ç$ IDL
ç 
ç.run  sms1:[wind.software.stics]auto_stics_scatter
çMATRIX.OUT
çPHA_TABLE.OUT
ç1
çy
ç.run sms1:[wind.software.stics]dailyrateplt
çWSTXRATE.OUT
ç1
çexit
ç$!
