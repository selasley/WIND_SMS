$ CMS
CREATE ELEMENT EDB.INC /INPUT=-
EDB.INC;1           /KEEP -
"Defines common block and structures to represent the elements and the order of elements of the data for the WIND Swics, Mass, Stics experiments"

CREATE ELEMENT L0_DATA_RECORD.INC /INPUT=-
L0_DATA_RECORD.INC;1 /KEEP -
"Include file to define the elements and the order of elements of the WIND Level 0 Data Record for one Major Frame"

CREATE ELEMENT L0_LABL_RECORD.INC /INPUT=-
L0_LABL_RECORD.INC;1 /KEEP -
"Include file to define the elements and the order of elements of the WIND Level 0 Label Record for one Major Frame"

CREATE ELEMENT L0_PC_RECORD.INC /INPUT=-
L0_PC_RECORD.INC;1  /KEEP -
"Define structures for data-recording as done on PC"

CREATE ELEMENT L1FMT.INC /INPUT=-
L1FMT.INC;3         /KEEP -
"Include file containing definitions for the Level 1 file and Science Record headers"

CREATE ELEMENT L1READ.INC /INPUT=-
L1READ.INC;4        /KEEP -
"Include file to define data structures used in reading WIND Level 1 files"

CREATE ELEMENT SW_CODES.INC /INPUT=-
SW_CODES.INC;13     /KEEP -
"Include file to define message codes, other variables"

CREATE ELEMENT WIND_L0_RECORD.INC /INPUT=-
WIND_L0_RECORD.INC;1 /KEEP -
"Include file to define COMMON array for creation of data file based on EDBs rather than Minor Frames"

CREATE ELEMENT WIND_LUN.INC /INPUT=-
WIND_LUN.INC;1      /KEEP -
"Include file for WIND containing all the logical units for all files"

CREATE ELEMENT EDBDEF.INC /INPUT=-
EDBDEF.INC;-0 /KEEP -
"Include file to define WIND data structures"

CREATE ELEMENT EDBVARS.INC /INPUT=-
EDBVARS.INC;-0 /KEEP -
"Include file to define WIND common blocks, the order of elements for the Swics, Mass, and Stics experiments, and other commonly used variables"

!CREATE ELEMENT G_S_CONST.INC /INPUT=-
!G_S_CONST.INC;1     /KEEP -
!"Include file to define constants for DPU"

!CREATE ELEMENT HSKP.INC /INPUT=-
!HSKP.INC;15         /KEEP -
!"Include file to define common block contains with arrays and variables necessary for assembling and decommutating the HKSP data"

!CREATE ELEMENT ICSS_MESSAGES.INC /INPUT=-
!ICSS_MESSAGES.INC;7 /KEEP -
!"Include file to define ICSS message values"

!CREATE ELEMENT MAINSEC_L0.INC /INPUT=-
!MAINSEC_L0.INC;42   /KEEP -
!"Include file defining common block with the data arrays and variables necessary to for assemble and decommutate the Level 0 data stream into 32 EDBs"

!CREATE ELEMENT MAINSEC_L1.INC /INPUT=-
!MAINSEC_L1.INC;1003 /KEEP -
!"Include file defining common block, arrays and variables necessary to assemble and use the Level 1 data"

!CREATE ELEMENT MAINSEC_L2.INC /INPUT=-
!MAINSEC_L2.INC;10   /KEEP -
!"Include file defining common block and L2 parameters that are unique to the CDHF KPGS"

!CREATE ELEMENT STICS.INC /INPUT=-
!STICS.INC;7         /KEEP -
!"Include file with the COMMON area required to communicate STICS EDB data"

EXIT
