C
       SUBROUTINE TELESCOPE_CHECK (TIME_OF_FLIGHT,ENERGY,
     .                PSTART,STOP_ID,SSD_ID, CHECK_TELE)


       INTEGER*4 ENERGY,
     .           TIME_OF_FLIGHT,
     .           PSTART,
     .           STOP_ID,
     .           SSD_ID,
     .           CHECK_TELE


                 CHECK_TELE = 0		!initialize

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
	  	 IF (PSTART.EQ.0 .AND. TIME_OF_FLIGHT.NE.0) CHECK_TELE = 1
	         IF ( (PSTART.EQ.1 .OR. PSTART.EQ.2  .OR. PSTART.EQ.7)
     .               .AND.
     .                (STOP_ID.NE.1 .AND. STOP_ID.NE.0) ) CHECK_TELE = 1
	         IF ( (PSTART.EQ.3 .OR. PSTART.EQ.4  .OR. PSTART.EQ.8)
     .               .AND.
     .                (STOP_ID.NE.2 .AND. STOP_ID.NE.0) ) CHECK_TELE = 1
	         IF ( (PSTART.EQ.5 .OR. PSTART.EQ.6  .OR. PSTART.EQ.9)
     .               .AND.
     .                (STOP_ID.NE.3 .AND. STOP_ID.NE.0) ) CHECK_TELE = 1

	         IF ( (PSTART.EQ.1 .OR. PSTART.EQ.2  .OR. PSTART.EQ.7)
     .               .AND.
     .                (SSD_ID.NE.1 .AND. SSD_ID.NE.0) ) CHECK_TELE = 1
	         IF ( (PSTART.EQ.3 .OR. PSTART.EQ.4  .OR. PSTART.EQ.8)
     .               .AND.
     .                (SSD_ID.NE.2 .AND. SSD_ID.NE.0) ) CHECK_TELE = 1
	         IF ( (PSTART.EQ.5 .OR. PSTART.EQ.6  .OR. PSTART.EQ.9)
     .               .AND.
     .                (SSD_ID.NE.3 .AND. SSD_ID.NE.0) ) CHECK_TELE = 1


       RETURN
       END
