!	L0_PC_RECORD.INC
!
!	STRUCTURES FOR DATA-RECORDING AS DONE ON PC
!
!	This version :	 7-APR-1994
!
!	CREATION :
!		H.BROER	 7-APR-1994
!
!     --------------------------------------------------------
      STRUCTURE /FILEHEADER/
          BYTE		byID(0:4)	! EB 90 'G' 'S' 'E'
          INTEGER*2	wVersion	! 101
          CHARACTER*19	ProjectName	! "SMS"
          INTEGER*4	dwTime		! SECONDS SINCE 1-JAN-1970
          INTEGER*2	wNULL
      END STRUCTURE
      STRUCTURE /HEADER_LABEL/
          UNION
             MAP
                 RECORD /FILEHEADER/	LABEL
             END MAP
             MAP
                 CHARACTER	CBYTE*32
             END MAP
             MAP
                 BYTE		BBYTE(0:31)
             END MAP
         END UNION
      END STRUCTURE
      RECORD /HEADER_LABEL/	HLABEL

      STRUCTURE /BLOCKHEADER/
          BYTE		bID(0:1)	! 'D' 'E'
          INTEGER*2	wBlockID	! Block ID
          CHARACTER*26	szName		! Block NAME
          INTEGER*2	wLen
      END STRUCTURE
      STRUCTURE /BLOCK_LABEL/
          UNION
             MAP
                 RECORD /BLOCKHEADER/	LABEL
             END MAP
             MAP
                 CHARACTER	CBYTE*32
             END MAP
             MAP
                 BYTE		BBYTE(0:31)
             END MAP
         END UNION
      END STRUCTURE
      RECORD /BLOCK_LABEL/	REC_PRE
      STRUCTURE /BLOCKBODY/
          BYTE		bID(0:1)	! 'T' 'A'
          INTEGER*2	wBlockID	! Block ID as in BLOCKHEADER
          INTEGER*2	wCountStamp	! continuous counter
          INTEGER*2	wSize
          INTEGER*4	dwTime		! SECONDS SINCE 1-JAN-1970
          BYTE		abyData(0:719)
          INTEGER*2	wLen
      END STRUCTURE
      STRUCTURE /BLOCK_DATA/
          UNION
             MAP
                 RECORD /BLOCKBODY/	LABEL
             END MAP
             MAP
                 CHARACTER	CBYTE*734
             END MAP
             MAP
                 BYTE		BBYTE(0:733)
             END MAP
         END UNION
      END STRUCTURE
      RECORD /BLOCK_DATA/	REC_DAT
