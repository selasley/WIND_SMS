                       WIND Project Directory Structure
[WIND.SOFTWARE.LZ]         source code and make files for WIND_CD_READ and
                           related programs which operate on LZ files; see also
                           [WIND.SOFTWARE.LIB]
[WIND.SOFTWARE.L1]         source code and make files for WIND_L1 reading
                           routines which comprise L1LIB.OLB, and for various
                           routines (such as MASS_RATE_TABLE) which red L1
                           files
[WIND.SOFTWARE.INC]        include files used by any WIND routines; pointed to
                           by logical name INC_DIR; since this may change use
                           the logical name 
[WIND.SOFTWARE.LIB]        source code for modules in the WIND_LIB.OLB library;
                           this contains code which is/can be used in more than
                           one WIND-related program
[WIND.SOFTWARE.XTI]        source code for modules in the XTI.OLB (extended
                           terminal input) library
[WIND.SOFTWARE.EXE]        executable WIND programs which have been debugged;
                           pointed to by logical name WIND_EXE; since this may
                           change use the logical name
[WIND.SOURCE]              original (now very old) copy of WIND_CD_READ
                           software; not used for development, is archive only
[WIND.LIB]                 object libraries used by various WIND programs;
                           pointed to by logical name LIB_DIR; since this may
                           change use the logical name 
[WIND.DATA]                directory containing LZ and L1 data files; may be on
                           a different disk, pointed to by logical name
                           DATA_DIR; since this may change use the logical name
[WIND.TABLE]               directory containing output of various TABLE
                           programs; may be on a different disk; pointed to by
                           logical name TABLE_DIR; the TABLE programs assume
                           the directory and logical name exist; since this may
                           change use the logical name
                          WIND Project Logical Names
SRC_DIR                    location of source code for WIND_CD_READ
INC_DIR                    location of include files for all WIND programs
LIB_DIR                    location of object libraries for all WIND programs
WIND_EXE                   location of executable code which is debugged
DATA_DIR                   location of LZ and L1 data files
TABLE_DIR                  location of output from TABLE programs
