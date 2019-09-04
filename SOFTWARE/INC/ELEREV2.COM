$ CMS
RESERVE EDB.INC ""
REPLACE EDB.INC /INPUT=EDB.INC;3  /KEEP -
"Updated on 4-AUG-1994: changed dimension of SCI to 3, same with HDB and xxx_RATE and xxx_PHA for each instrument"

RESERVE EDB.INC ""
REPLACE EDB.INC /INPUT=EDB.INC;5  /KEEP -
"Updated on 13-SEP-1994: changed HKset from scalar to array of 3, made HKeep record an array of 3; added count field to HKeep structure def"

RESERVE EDB.INC ""
REPLACE EDB.INC /INPUT=EDB.INC;8  /KEEP -
"Updated on 21-SEP-1994: added revision history comments"

RESERVE EDB.INC ""
REPLACE EDB.INC /INPUT=EDB.INC;9  /KEEP -
"Updated on 26-SEP-1994: added a quality flag for each type of rates; neccesary because of shifting"

RESERVE EDB.INC ""
REPLACE EDB.INC /INPUT=EDB.INC;10  /KEEP -
"Updated on 27-SEP-1994: added structure OLD_CORE for compatibility"

RESERVE EDB.INC ""
REPLACE EDB.INC /INPUT=EDB.INC;11  /KEEP -
"Updated on 4-OCT-1994: added L1_sci_rec_cntr as output L1 science record counter in new COMMON block L1INFO"

RESERVE EDB.INC ""
REPLACE EDB.INC /INPUT=EDB.INC;12  /KEEP -
"Updated on 14-OCT-1994: fixed spelling error"

RESERVE EDB.INC ""
REPLACE EDB.INC /INPUT=EDB.INC;13  /KEEP -
"Updated on 19-OCT-1994: corrected error in initial value of SWICS_N_RATE_Size"

RESERVE EDB.INC ""
REPLACE EDB.INC /INPUT=EDB.INC;14  /KEEP -
"Updated on 24-OCT-1994: file was split into two files: EDBDEF.INC and EDBVARS.INC"

RESERVE EDBVARS.INC ""
REPLACE EDBVARS.INC /INPUT=EDBVARS.INC;7  /KEEP -
"Updated on 27-OCT-1994: changed name L1_sci_rec_cntr to sci_rec_cntr, added L1_recs_written"

RESERVE L0_DATA_RECORD.INC ""
REPLACE L0_DATA_RECORD.INC /INPUT=L0_DATA_RECORD.INC;5  /KEEP -
"Updated on 23-SEP-1994: added C_BIG_NULL_Buffer"

RESERVE L1FMT.INC ""
REPLACE L1FMT.INC /INPUT=L1FMT.INC;8  /KEEP -
"Updated on 13-OCT-1994: added fields to file header"

RESERVE L1FMT.INC ""
REPLACE L1FMT.INC /INPUT=L1FMT.INC;11  /KEEP -
"Updated on 14-OCT-1994: added C_ALL field to allow headers to be accessed as a single CHARACTER variable"

RESERVE L1FMT.INC ""
REPLACE L1FMT.INC /INPUT=L1FMT.INC;12  /KEEP -
"Updated on 18-OCT-1994: changed field program_vers to date_of_EXE"

RESERVE L1FMT.INC ""
REPLACE L1FMT.INC /INPUT=L1FMT.INC;15  /KEEP -
"Updated on 20-OCT-1994: changed name of sci rec header structure from L1_SR_HEADER to ST_L1_SR_HEADER"

RESERVE L1FMT.INC ""
REPLACE L1FMT.INC /INPUT=L1FMT.INC;16  /KEEP -
"Updated on 2-NOV-1994: added sr_count to ST_L1_SR_HEADER structure def"

RESERVE L1FMT.INC ""
REPLACE L1FMT.INC /INPUT=L1FMT.INC;17  /KEEP -
"Updated on 8-NOV-1994: corrected length of C_ALL field"

RESERVE L1READ.INC ""
REPLACE L1READ.INC /INPUT=L1READ.INC;5  /KEEP -
"Updated on 13-OCT-1994: added array of L1 file headers"

RESERVE L1READ.INC ""
REPLACE L1READ.INC /INPUT=L1READ.INC;10  /KEEP -
"Updated on 24-OCT-1994: added array of science record headers, added null buffer"

RESERVE WIND_LUN.INC ""
REPLACE WIND_LUN.INC /INPUT=WIND_LUN.INC;2  /KEEP -
"Updated on 4-NOV-1994: added unit TTOUT for output stream, made it same value as KB"

EXIT
