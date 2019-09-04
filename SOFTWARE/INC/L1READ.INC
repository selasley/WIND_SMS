	INTEGER maxunits
	PARAMETER (maxunits=20)
	INTEGER*4 L1_units(maxunits)
	RECORD /CORE/ L1_SCI(maxunits)
	RECORD /SW_RATE/ L1_SWICS_RATE(maxunits)
	RECORD /SW_PHA/ L1_SWICS_PHA(maxunits)
	RECORD /MA_RATE/ L1_MASS_RATE(maxunits)
	RECORD /MA_PHA/ L1_MASS_PHA(maxunits)
	RECORD /ST_RATE/ L1_STICS_RATE(maxunits)
	RECORD /ST_PHA/ L1_STICS_PHA(maxunits)
	RECORD /HDBstr/	L1_HDB(maxunits)
	RECORD /HKeep/ L1_HKeep(maxunits)
	COMMON /L1INFO/ 
     +		L1_SCI,			! Basic Core Data
     +		L1_SWICS_RATE,		! SWICS RATE Data, avail. when SWICS on
     +		L1_SWICS_PHA,		! SWICS PHA words, as indicated in CORE
     +		L1_STICS_RATE,		! STICS RATE Data, avail. when STICS on
     +		L1_STICS_PHA,		! STICS PHA words, as indicated in CORE
     +		L1_MASS_RATE,		! MASS  RATE Data, avail. when MASS  on
     +		L1_MASS_PHA,		! MASS  PHA words, as indicated in CORE
     +		L1_HDB,			! HDBs
     +		L1_HKeep 		! HKs
	INTEGER*4 scirecnum(maxunits)
        CHARACTER*2 max_version
	RECORD /ST_L1_FILE_HEADER/ L1_header(maxunits)
	RECORD /ST_L1_SR_HEADER/ L1_sci_rec_header(maxunits)
	COMMON /L1INFO2/ 
     +		L1_units,		! Fortran unit numbers
     +		scirecnum,		! Current science Record number
     +          max_version,            ! Largest L1 version we can handle
     +		L1_header,		! file headers
     +		L1_sci_rec_header	! Science Record headers
	DATA scirecnum /maxunits * 0/
	DATA L1_units /maxunits * 0/
	DATA max_version /'06'/		! Number of highest version of L1 file
					! we can process
	CHARACTER buffer*65534
	COMMON /L1BUF/ buffer
	CHARACTER null_buffer*65534
	BYTE null_byte_array(65534)
	INTEGER*4 null_i4_array(16384)
	EQUIVALENCE (null_buffer,null_byte_array,null_i4_array)
	COMMON /NULLBUF/ null_buffer
