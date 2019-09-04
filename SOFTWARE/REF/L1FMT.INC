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
C This version:    15-DEC-1994
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
              CHARACTER*122   spare             ! makes total size 400 bytes
          END MAP
          MAP
              CHARACTER*400   c_all             ! references entire structure
          END MAP
          END UNION
      END STRUCTURE    !ST_L1_FILE_HDR
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
              CHARACTER*152   spare             ! makes total size 200 bytes
          END MAP
          MAP
              CHARACTER*200   c_all             ! references entire structure
          END MAP
          END UNION
      END STRUCTURE    !ST_L1_SR_HEADER
      COMMON /L1_HEADERS/ l1_file_header, l1_sr_header
      RECORD /ST_L1_FILE_HEADER/ l1_file_header
      RECORD /ST_L1_SR_HEADER/ l1_sr_header
