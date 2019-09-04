	SUBROUTINE CONSTRUCT_CDF_NAME(lzname,dtype,cdfdir,cdfname,cdfnamelen)
C
C This subroutine accepts in the argument LZNAME a Level Zero filename and
C constructs the name of the CDF file which contains the corresponding orbit
C or attitude data.  This constructed filename is returned in the argument
C CDFNAME.  The number of characters in CDFNAME is returned in the argument
C CDFNAMELEN. 
C
C If the argument CDFDIR is not blank its contents will be used for the entire
C device and directory parts of the constructed CDF filename.
C
C If the argument CDFDIR is empty (all blanks) test the flag FROM_CD, which was
C set to indicate if the LZ file's device is a CDROM or not.  If it is set, use
C the same device name and a hard-coded directory name of [DATA.WI.DEF.OR] or
C [DATA.WI.DEF.AT], depending on the value of the argument DTYPE, which can be
C 'OR' or 'AT'..  If FROM_CD is not set (ie, the LZ file is on a normal disk),
C use the logical name WIND_OR_DATA (or WIND_AT_DATA) as the CDF's device and
C directory name.
C
C Note: The file type of .CDF is not added to the constructed filename, since
C the CDF_OPEN routine requires it not be present.  If the caller wishes to do
C an INQUIRE or OPEN the string '.CDF' should first be stored in CDFNAME at
C offset CDFNAMELEN+1 through CDFNAMELEN+4.
C
C The possible input strings for LZNAME are:
C   1)  device:[directory.subdirectory]WI_LZ_SMS_yyyymmdd_Vvv.DAT
C   2)  device:WI_LZ_SMS_yyyymmdd_Vvv.DAT
C   3)  [directory.subdirectory]WI_LZ_SMS_yyyymmdd_Vvv.DAT
C   4)  WI_LZ_SMS_yyyymmdd_Vvv.DAT
C   5)  device:[directory.subdirectory]yymmddvv.DAT
C   6)  device:yymmddvv.DAT
C   7)  [directory.subdirectory]yymmddvv.DAT
C   8)  yymmddvv.DAT
C
C In addition, the LZ file can be either on hard disk or on CDROM.  If on disk
C it can take on any of the above forms; if on CDROM, however, it will always
C take on form 5.
C
C If CDFDIR is blank the constructed filename for each case would be:
C   1)  device:[directory.subdirectory]WI_xx_DEF_yyyymmdd_Vvv
C   2)  device:WI_xx_DEF_yyyymmdd_Vvv
C   3)  [directory.subdirectory]WI_xx_DEF_yyyymmdd_Vvv
C   4)  WI_xx_DEF_yyyymmdd_Vvv
C   5)  device:[directory.subdirectory]yymmddvv
C   6)  device:yymmddvv
C   7)  [directory.subdirectory]yymmddvv
C   8)  yymmddvv
C In each of these cases the device and directory of the LZ file are retained.
C The xx is the data type given in the argument DTYPE.  xx can be either OR or
C AT, for orbit or attitude data.
C
C If CDFDIR is not blank the constructed filename would be:
C   1)  device:[directory.subdirectory]WI_xx_DEF_yyyymmdd_Vvv
C   2)  device:[directory.subdirectory]yymmddvv
C Here the string   "device:[directory.subdirectory]"  represents the contents
C of CDFDIR.  xx once again is the argument DTYPE (either OR or AT).
C
C  This version:   1.1  12-APR-1995
C
C  Created:
C       Larry Bleau    6-APR-1995
C
C Modification history:
C 1.1	12-APR-1995	LRB	added dtype argument, changed to generate
C				filenames on CD if LZ file is on CD
C
CDEC$	IDENT '1.1'
	IMPLICIT NONE
	INCLUDE 'INC_DIR:L1INFO.INC/NOLIST'
	CHARACTER*(*) lzname,cdfdir,cdfname
	CHARACTER*2 dtype
	INTEGER cdfnamelen
C
        INCLUDE 'INC_DIR:L1FMT.INC'
        INCLUDE 'INC_DIR:L1_HDR_INFO.INC'
	CHARACTER devdir*120,cdftemp*40
	INTEGER cdfdirlen,pos,pos2,devdirlen,namelen,nch
C
C Find end of directory or device name; the filename starts immediately after
C this.  Save position for later.
C
	pos = INDEX(lzname, ']')
	pos2 = INDEX(lzname, ':')
	IF (pos .eq. 0) pos = pos2
C
C Check if lzname starts with 'WI_LZ'.  If it does we have a long format
C filename; copy the filename into cdftemp and change the LZ to an OR or AT,
C depending on the contents of dtype.  Also change the descriptor part of the
C filename from 'SMS' to 'DEF'.  If it does not start with 'WI_LZ' assume
C we have a short filename and just copy the name.  If it's neither of these 
C formats all bets are off.
C
	IF (lzname(pos+1:pos+5) .eq. 'WI_LZ' .or.
     *	    lzname(pos+1:pos+5) .eq. 'wi_lz')     THEN
	  cdftemp = lzname(pos+1:pos+22)
	  cdftemp(4:5) = dtype
	  cdftemp(7:9) = 'DEF'
	  namelen = 22
	ELSE
	  cdftemp = lzname(pos+1:pos+8)
	  namelen = 8
	ENDIF
C
C Check if cdfdir is all blank.  If so extract device name and determine if it
C is a CDROM or not.  If it is from a CDROM, use the same device name and a
C hard-coded directory name.  If it is not from a CDROM (ie, it is from a
C normal disk), translate the logical name WIND_OR_DATA (or WIND_AT_DATA) and
C use it as the device and directory name.  
C
	IF (cdfdir .eq. ' ') THEN
	  IF (from_cd) THEN
	    devdir = lzname(1:pos2) // '[DATA.WI.DEF.' // dtype // ']'
	    devdirlen = pos2 + 16
	  ELSE
	    devdir = 'WIND_' // dtype // '_DATA:'
	    devdirlen = 13
	  ENDIF
C
C cdfdir was not blank, so use its contents as the CDF file's device and
C directory name
C
	ELSE
	  devdirlen = INDEX(cdfdir, ' ') - 1
	  IF (devdirlen .eq. -1) devdirlen = LEN(cdfdir)
	  devdir = cdfdir(1:devdirlen)
	ENDIF
C
C devdir now contains the correct device and directory to use for the CDF
C filename, and cdftemp contains the filename proper.  The lengths of these
C strings are devdirlen and namelen, respectively.  If devdirlen is 0 that
C means don't use any device or directory name.
C
C Construct the final CDF filename and compute its length.
C
	cdfnamelen = devdirlen + namelen
	cdfname = ' '
	IF (cdfnamelen .gt. LEN(cdfname)) THEN
	  TYPE *,'CDFNAME ARGUMENT TO CONSTRUCT_CDF_NAME IS TOO SHORT!!'
	  RETURN
	ENDIF
C
	IF (devdirlen .ne. 0) THEN
	  cdfname = devdir(1:devdirlen) // cdftemp(1:namelen)
	ELSE
	  cdfname = cdftemp(1:namelen)
	ENDIF
C
	RETURN
	END
