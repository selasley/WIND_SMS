       SUBROUTINE TELESCOPE_ID (TIME_OF_FLIGHT,ENERGY,
     .                PSTART,STOP_ID,SSD_ID, TELE_ID, CHECK_TELE)
C
C      this subroutine determines which telescope (1, 2, 3) the pha word comes 
C		from based on PSTART, or if the information from all the detectors 
C		is inconsistent, the check_tele flag is set
C      HISTORY:  derived from TELESCOPE_CHECK, 18Aug95 - ABG

       INTEGER*4 ENERGY,		!energy pulse height
     .           TIME_OF_FLIGHT,	!time pulse height
     .           PSTART,		!FSR id
     .           STOP_ID,		!RSR id
     .           SSD_ID,		!Solid state detector id
     .           TELE_ID,		!based on PSTART if TOF>0, or SSD if only E>0
     .           CHECK_TELE		!flag = 0 if all detectors match, 1 if not


                 CHECK_TELE = 0		!initialize
		 TELE_ID    = 0         

C                ssd checks
	  	 IF (SSD_ID.EQ.0 .AND. ENERGY.NE.0) CHECK_TELE = 1
	         IF (SSD_ID.EQ.1 
     .               .AND.
     .                    (PSTART.NE.1 .AND. PSTART.NE.2 .AND.
     .                     PSTART.NE.7 .AND. PSTART.NE.0) ) CHECK_TELE = 1
	         IF (SSD_ID.EQ.2 
     .               .AND.
     .                (PSTART.NE.3 .AND. PSTART.NE.4 .AND.
     .                 PSTART.NE.8 .AND. PSTART.NE.0) ) CHECK_TELE = 1
	         IF (SSD_ID.EQ.3 
     .                 .AND.
     .                (PSTART.NE.5 .AND. PSTART.NE.6 .AND.
     .                 PSTART.NE.9 .AND. PSTART.NE.0) ) CHECK_TELE = 1
	         IF (SSD_ID.EQ.1 
     .               .AND.
     .                (STOP_ID.NE.1 .AND. STOP_ID.NE.0) ) CHECK_TELE = 1
	         IF (SSD_ID.EQ.2 
     .               .AND.
     .                (STOP_ID.NE.2 .AND. STOP_ID.NE.0) ) CHECK_TELE = 1
	         IF (SSD_ID.EQ.3 
     .               .AND.
     .                (STOP_ID.NE.3 .AND. STOP_ID.NE.0) ) CHECK_TELE = 1

C                stop checks
	  	 IF (STOP_ID.EQ.0 .AND. TIME_OF_FLIGHT.NE.0) CHECK_TELE = 1
	         IF (STOP_ID.EQ.1 
     .               .AND.
     .                    (PSTART.NE.1 .AND. PSTART.NE.2 .AND.
     .                     PSTART.NE.7 .AND. PSTART.NE.0) ) CHECK_TELE = 1
	         IF (STOP_ID.EQ.2 
     .               .AND.
     .                (PSTART.NE.3 .AND. PSTART.NE.4 .AND.
     .                 PSTART.NE.8 .AND. PSTART.NE.0) ) CHECK_TELE = 1
	         IF (STOP_ID.EQ.3 
     .                 .AND.
     .                (PSTART.NE.5 .AND. PSTART.NE.6 .AND.
     .                 PSTART.NE.9 .AND. PSTART.NE.0) ) CHECK_TELE = 1
	         IF (STOP_ID.EQ.1 
     .               .AND.
     .                (SSD_ID.NE.1 .AND. SSD_ID.NE.0) ) CHECK_TELE = 1
	         IF (STOP_ID.EQ.2 
     .               .AND.
     .                (SSD_ID.NE.2 .AND. SSD_ID.NE.0) ) CHECK_TELE = 1
	         IF (STOP_ID.EQ.3 
     .               .AND.
     .                (SSD_ID.NE.3 .AND. SSD_ID.NE.0) ) CHECK_TELE = 1


C                start checks
C			start id only if TOF>0
	  	 IF (PSTART.EQ.0 .AND. TIME_OF_FLIGHT.NE.0) CHECK_TELE = 1

C			start id consistent with stop id
  	         IF ( (PSTART.EQ.1 .OR. PSTART.EQ.2  .OR. PSTART.EQ.7)
     .               .AND.
     .                (STOP_ID.NE.1 .AND. STOP_ID.NE.0) ) CHECK_TELE = 1
	         IF ( (PSTART.EQ.3 .OR. PSTART.EQ.4  .OR. PSTART.EQ.8)
     .               .AND.
     .                (STOP_ID.NE.2 .AND. STOP_ID.NE.0) ) CHECK_TELE = 1
	         IF ( (PSTART.EQ.5 .OR. PSTART.EQ.6  .OR. PSTART.EQ.9)
     .               .AND.
     .                (STOP_ID.NE.3 .AND. STOP_ID.NE.0) ) CHECK_TELE = 1

C			start id consistent with solid state detector id
	         IF ( (PSTART.EQ.1 .OR. PSTART.EQ.2  .OR. PSTART.EQ.7)
     .               .AND.
     .                (SSD_ID.NE.1 .AND. SSD_ID.NE.0) ) CHECK_TELE = 1
	         IF ( (PSTART.EQ.3 .OR. PSTART.EQ.4  .OR. PSTART.EQ.8)
     .               .AND.
     .                (SSD_ID.NE.2 .AND. SSD_ID.NE.0) ) CHECK_TELE = 1
	         IF ( (PSTART.EQ.5 .OR. PSTART.EQ.6  .OR. PSTART.EQ.9)
     .               .AND.
     .                (SSD_ID.NE.3 .AND. SSD_ID.NE.0) ) CHECK_TELE = 1

	
C  	find telescope ID if check_tele ok:
        IF (CHECK_TELE.EQ.0) THEN
	  IF (PSTART.NE.0) THEN
  	    IF (PSTART.EQ.1 .OR. PSTART.EQ.2  .OR. PSTART.EQ.7) TELE_ID = 1
	    IF (PSTART.EQ.3 .OR. PSTART.EQ.4  .OR. PSTART.EQ.8) TELE_ID = 2
	    IF (PSTART.EQ.5 .OR. PSTART.EQ.6  .OR. PSTART.EQ.9) TELE_ID = 3
	  ELSE IF (SSD_ID.NE.0) THEN
	    IF (SSD_ID.EQ.1) TELE_ID = 1
	    IF (SSD_ID.EQ.2) TELE_ID = 2
	    IF (SSD_ID.EQ.3) TELE_ID = 3
	  ENDIF
        ENDIF

       RETURN
       END
