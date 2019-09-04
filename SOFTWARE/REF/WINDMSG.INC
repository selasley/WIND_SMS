C
C WIND include file to define Error Status Codes
C
        INTEGER WIND$_SUCCESS
        PARAMETER (WIND$_SUCCESS = '08178001'X)     ! normal successful completion
        INTEGER WIND$_HDRFMT
        PARAMETER (WIND$_HDRFMT = '08178053'X)      ! header version mismatch, not all header info available
        INTEGER WIND$_FILEFMT
        PARAMETER (WIND$_FILEFMT = '081780F0'X)     ! L1 file format mismatch, some data can be lost
        INTEGER WIND$_BADRECLEN
        PARAMETER (WIND$_BADRECLEN = '081780F8'X)   ! record length incorrect, possible bad file
        INTEGER WIND$_EMPTY
        PARAMETER (WIND$_EMPTY = '08178100'X)       ! could not read header, L1 file is empty
        INTEGER WIND$_TOOMANY
        PARAMETER (WIND$_TOOMANY = '08178192'X)     ! too many L1 files open
        INTEGER WIND$_INUSE
        PARAMETER (WIND$_INUSE = '0817819A'X)       ! this unit/data structure already in use
        INTEGER WIND$_OUTRANGE
        PARAMETER (WIND$_OUTRANGE = '081781A2'X)    ! L1 unit is out of range
        INTEGER WIND$_INVHDRVER
        PARAMETER (WIND$_INVHDRVER = '081781AA'X)   ! invalid header version
        INTEGER WIND$_FILNOTOPN
        PARAMETER (WIND$_FILNOTOPN = '081781B2'X)   ! L1 file not open
        INTEGER WIND$_FILNAMFMT
        PARAMETER (WIND$_FILNAMFMT = '081781BA'X)   ! L1 file name syntax error, cannot open next file
        INTEGER WIND$_BADFILEHDR
        PARAMETER (WIND$_BADFILEHDR = '081781C2'X)  ! unknown file header level, cannot interpret file
        INTEGER WIND$_NONEXTFILE
        PARAMETER (WIND$_NONEXTFILE = '081781CA'X)  ! cannot find next day's data set
