ç$set default sms1:[wind.software.swics.scratch]
ç$run sms1:[cohen.wind.swics]wvelocity
çsmsdisk4:[wind.l2data.swics.routine]WI_L1_SMS_19980204_V01.outv
çwind_l1_data:WI_L1_SMS_19980204_V01.DAT;1
çy
çn
çn
ç$!                         idl
ç$idl
ç.run sms1:[wind.software.swics]tplot
ç2
ç1
çn
çsmsdisk4:[wind.l2data.swics.routine]WI_L1_SMS_19980204_V01.outv
çexit
ç$psprint idl.ps
ç$!                         hk
ç$run sms1:[cohen.wind.swics]hk_swics
çwind_l1_data:WI_L1_SMS_19980204_V01.DAT;1
çy
çn
çsmsdisk4:[wind.l2data.swics.routine]WI_L1_SMS_19980204_V01.outhk
çn
ç$print smsdisk4:[wind.l2data.swics.routine]WI_L1_SMS_19980204_V01.outhk
ç$!
ç$run sms1:[cohen.wind.swics]wvelocity
çsmsdisk4:[wind.l2data.swics.routine]WI_L1_SMS_19980205_V01.outv
çwind_l1_data:WI_L1_SMS_19980205_V01.DAT;1
çy
çn
çn
ç$!                         idl
ç$idl
ç.run sms1:[wind.software.swics]tplot
ç2
ç1
çn
çsmsdisk4:[wind.l2data.swics.routine]WI_L1_SMS_19980205_V01.outv
çexit
ç$psprint idl.ps
ç$!                         hk
ç$run sms1:[cohen.wind.swics]hk_swics
çwind_l1_data:WI_L1_SMS_19980205_V01.DAT;1
çy
çn
çsmsdisk4:[wind.l2data.swics.routine]WI_L1_SMS_19980205_V01.outhk
çn
ç$print smsdisk4:[wind.l2data.swics.routine]WI_L1_SMS_19980205_V01.outhk
ç$!
