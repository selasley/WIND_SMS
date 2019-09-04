C+
C 
C FUNCTIONAL DESCRIPTION:       
C 
C    This include file contains definitions for the Level 1 file format.
C    Specifiically, it contains the definitions for the file and Science Record
C    headers.
C
C COMMON BLOCKS:
C  
C     L1_HEADERS      contains one copy each of the file header and the Science
C                     Record header
C  
C 
C AUTHOR(S): 
C 
C        L. Bleau
C 
C Created: 
C 
C        12-OCT-1994    L. Bleau
C 
C This version:    6-APR-1995
C 
C Revised:
C 
C        Date     | Name  | Description
C ----------------+-------+-----------------------------------------------------
C 13-OCT-1994     |  LRB  | added fields to file header
C 20-OCT-1994     |  LRB  | changed name of sci rec header structure from
C                           L1_SR_HEADER to ST_L1_SR_HEADER
C  2-NOV-1994     |  LRB  | added sr_count to ST_L1_SR_HEADER structure def
C 15-DEC-1994     |  LRB  | added L1_records, EDB_count, EDB_lost, and EDB_skip
C                           fields to L1 file header 
C  8-MAR-1995     |  LRB  | added program_vers field to file header
C  6-APR-1995     |  LRB  | added gsm, gse, and attitude fields to science
C                           record header; added structure EPHEMERIS
C
C-
      STRUCTURE /ST_L1_FILE_HEADER/
          UNION
          MAP
              CHARACTER*4     ident/'HDRF'/
              CHARACTER*2     header_vers       ! header version
              CHARACTER*72    input_file        ! name of input file
              CHARACTER*2     medium            ! input medium code:
                                                ! 'DK' = disk
                                                ! 'CD' = cd-rom
              CHARACTER*3     credate_doy       ! input file creation date, day
                                                ! of year 
              CHARACTER*4     credate_year      ! input file creation date, year
              CHARACTER*11    credate           ! input file creation date
                                                ! 'DD-MMM-YYYY'
              CHARACTER*23    date_of_EXE       ! version of program which wrote
                                                ! the L1 file
              CHARACTER*1     hw_type           ! system hardware type which ran
                                                ! program: 'V' = VAX, 'A' = AXP
              CHARACTER*11    run_date          ! program run date 'DD-MMM-YYYY'
              CHARACTER*20    start_time        ! user-specified start time
                                                ! 'DD-MMM-YYYY HH:MM:SS'
              CHARACTER*20    stop_time         ! user-specified stop time
                                                ! 'DD-MMM-YYYY HH:MM:SS'
              CHARACTER*3     comment_len       ! length of user comment
              CHARACTER*80    comment           ! user-supplied comment
              CHARACTER*4     L1_records        ! number of L1 records in file
              CHARACTER*6     EDB_count         ! number of good EDBs (spins) stored
              CHARACTER*6     EDB_lost          ! number of EDBs (spins) lost
              CHARACTER*6     EDB_skip          ! number of EDBs (spins) skipped
              CHARACTER*5     program_vers      ! version of WIND_CD_READ program
              CHARACTER*1     eph_avail		! 'T' = orbit and attitude info
						! in sci rec hdr; 'F' = not there
              CHARACTER*116   spare             ! makes total size 400 bytes
          END MAP
          MAP
              CHARACTER*400   c_all             ! references entire structure
          END MAP
          END UNION
      END STRUCTURE    !ST_L1_FILE_HDR
      STRUCTURE /EPHEMERIS/		! structure to describe spacecraft 
					! position in some coordinate system
          REAL*8           x
          REAL*8           y
          REAL*8           z
      END STRUCTURE    !EPHEMERIS
      STRUCTURE /ATTITUDE/		! structure to describe spacecraft 
					! attitude (orientation)
	  REAL*4	   r_asc
	  REAL*4	   decl
      END STRUCTURE    !ATTITUDE
      STRUCTURE /ST_L1_SR_HEADER/
          UNION
          MAP
              CHARACTER*4     ident/'HDRR'/
              INTEGER*4       nvalid            ! number of valid spins
              INTEGER*4       present(2)        ! spin presence flags (bit coded)
              INTEGER*4       bitrate_code      ! bit rate code:
                                                ! 0 = no valid spins
                                                ! 1 = all valid spins low bit rate
                                                ! 2 = all valid spins high bit rate
                                                ! 3 = mixed low and high bit rate
              INTEGER*4       sr_count          ! Sci Rec counter extracted from
                                                ! HDB (24 bits)
              INTEGER*4       zero_swics_pha(2) ! zero SWICS PHA word flags (bit coded)
              INTEGER*4       zero_mass_pha(2)  ! zero MASS PHA word flags (bit coded)
              INTEGER*4       zero_stics_pha(2) ! zero STICS PHA word flags (bit coded)
              RECORD /EPHEMERIS/ gci_pos	! GCI position coordinates
              RECORD /EPHEMERIS/ gci_vel	! velocity in GCI coords
              RECORD /EPHEMERIS/ gse_pos	! GSE position coordinates
              RECORD /EPHEMERIS/ gse_vel	! velocity in GSE coords
              RECORD /EPHEMERIS/ gsm_pos	! GSM position coordinates
              RECORD /EPHEMERIS/ gsm_vel	! velocity in GSM coords
              RECORD /EPHEMERIS/ gci_sun_vector	! sunvector in GCI coords
              RECORD /ATTITUDE/ gci_att		! GCI attitude of spacecraft in RA, DECL
              RECORD /ATTITUDE/ gse_att		! GSE attitude of spacecraft in RA, DECL
              RECORD /ATTITUDE/ gsm_att		! GSM attitude of spacecraft in RA, DECL
	      REAL*4          spin_rate		! body spin rate
          END MAP
          MAP
              CHARACTER*244   c_all             ! references entire structure
          END MAP
          END UNION
      END STRUCTURE    !ST_L1_SR_HEADER
      COMMON /L1_HEADERS/ l1_file_header, l1_sr_header
      RECORD /ST_L1_FILE_HEADER/ l1_file_header
      RECORD /ST_L1_SR_HEADER/ l1_sr_header
