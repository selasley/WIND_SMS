#include "makecdf.h"

/*********************************************************************/
/*               STRIP 3CHARACTER SUFFIX FROM FILENAME               */
/*********************************************************************/
void StripName (oldname,newname)
char *oldname, *newname;
{
  long length,i,j;
  char suffix[5] = "";
  length=(long)strlen(oldname);
  if (length > 4) {
    for (i=0,j=length-4;i<4;i++,j++) suffix[i]=toupper(oldname[j]);
    suffix[4]='\0';
    if ((strcmp(suffix,".CDF")==0)||(strcmp(suffix,".SKT")==0)) {
      for (i=0;i<(length-4);i++) newname[i] = oldname[i];
      newname[i]='\0'; }
    else strcpy(newname,oldname);
  }
  else strcpy(newname,oldname);
}

/*********************************************************************/
/*                           Message Output                          */
/*********************************************************************/
void MSG_control(mtype,instring,status)
long mtype;
char *instring;
CDFstatus status;
{
  long nofeed=FALSE;
  long printit=FALSE;
  char cdfmessage[CDF_ERRTEXT_LEN+1];

  if ((mtype >= 0)&&(RPARMS.debug == TRUE)) printit=TRUE;
  else if ((mtype >= 1)&&(RPARMS.progress == TRUE)) printit=TRUE;
  else if ( mtype >= 2) printit=TRUE;

  if (printit == TRUE) { /* message passes output level test */
    if (status != 0) CDFerror (status,instring);
    if (instring[strlen(instring)-1] != 10) nofeed=TRUE;

    if (RPARMS.sendtoterm == TRUE) {
      if (nofeed==TRUE) printf("%s\n",instring);
      else printf("%s",instring);
    }

    if ((RPARMS.sendtolog == TRUE)&&(LOGFILE.fptr == NULL)) {
      strcpy(LOGFILE.fname,RPARMS.outcdfname);
      StripName (LOGFILE.fname,LOGFILE.fname);
      strcat(LOGFILE.fname,".log");
      LOGFILE.fptr = fopen(LOGFILE.fname,"w");
      if (LOGFILE.fptr == NULL) {
          RPARMS.sendtolog = FALSE;
          printf("ERROR: Unable to open log file for output. \n");
          printf("       makeCDF execution continuing... \n");
      }
      else LOGFILE.nrecs = 0;
    }

    if ((RPARMS.sendtolog == TRUE)&&(LOGFILE.fptr != NULL)) {
      if (nofeed==TRUE) fprintf(LOGFILE.fptr,"%s\n",instring);
      else fprintf(LOGFILE.fptr,"%s",instring);
      LOGFILE.nrecs++;
      if (LOGFILE.nrecs > LOGFILE.maxrecs) {
        RPARMS.sendtolog = FALSE;
        fclose(LOGFILE.fptr);
  } } }
}


/**********************************************************************/
/*                  VALIDATE INPUT STRING AS ON OR OFF                */
/**********************************************************************/
long Validate_BOOLEAN(instring)
char *instring;
{
  long i;
  long ival = -1;
  for (i=0;i<(long)strlen(instring);i++) instring[i]=toupper(instring[i]);
  if (strcmp(instring,"ON")==0) ival=1;
  else if (strcmp(instring,"TRUE")==0) ival=1;
  else if (strcmp(instring,"YES")==0) ival=1;
  else if (strcmp(instring,"T")==0) ival=1;
  else if (strcmp(instring,"Y")==0) ival=1;
  else if (strcmp(instring,"1")==0) ival=1;
  else if (strcmp(instring,"OFF")==0) ival=0;
  else if (strcmp(instring,"FALSE")==0) ival=0;
  else if (strcmp(instring,"NO")==0) ival=0;
  else if (strcmp(instring,"F")==0) ival=0;
  else if (strcmp(instring,"N")==0) ival=0;
  else if (strcmp(instring,"0")==0) ival=0;
  return(ival);
}

/**********************************************************************/
/*             VALIDATE INPUT STRING AS POSITIVE INTEGER              */
/**********************************************************************/
long Validate_POSINT(instring)
char *instring;
{
  long i,ival;
  long ierr = -1;
  if (sscanf(instring,"%ld",&ival) == 0) return(ierr);
  else if (ival < 0) return(ierr);
  return(ival);
}

/**********************************************************************/
/*             VALIDATE THAT FILLVAL MATCHES CDF TYPE                 */
/**********************************************************************/
double Validate_FILLVAL(instring, cdfvartype)
char *instring;
long cdfvartype;
{
  double dval;
  long ierr = -1;
  if (sscanf(instring,"%lf",&dval) == 0) return(ierr);
  return(dval);
}

/**********************************************************************/
/*             VALIDATE INPUT VARIABLE FORMAT STRING                  */
/**********************************************************************/
long Validate_FORMAT(instring, outstring, length, cdfvartype)
char *instring;
char *outstring;
long *length;
long *cdfvartype;
{
  char ctype;
  char cwidth[10] = "";
  long i,icount,iwidth,ival;
  long ierr = -1;

  ctype = *instring++;
  icount = sscanf(instring,"%ld",&iwidth);
  if (icount == 1) sprintf(cwidth,"%d",iwidth);
  if      ((ctype=='i')||(ctype=='I')) {
    ctype='d'; *cdfvartype=CDF_INT4;   }
  else if ((ctype=='r')||(ctype=='R')) {
    ctype='f'; *cdfvartype=CDF_REAL4;  }
  else if ((ctype=='f')||(ctype=='F')) {
    ctype='f'; *cdfvartype=CDF_REAL4;  }
  else if ((ctype=='c')||(ctype=='C')) {
    ctype='c'; *cdfvartype=CDF_CHAR;   }
  else if ((ctype=='s')||(ctype=='S')) {
    ctype='s'; *cdfvartype=CDF_CHAR;   }
  else if (ctype =='0')                {
    ctype='0'; *cdfvartype=CDF_CHAR;   }
  if (INFILE.format == 0) { /* freeform */
    *outstring++ = ' ';
    *outstring++ = '%';
    if (ctype=='c') {
      for (i=0;i<strlen(cwidth);i++) *outstring++ = cwidth[i];
      *length = iwidth;
    }
    else {
      if (ctype=='s') *length = 40;
      else if (ctype=='0') *length = 0;
      else *length = 20;
    }
    *outstring++ = ctype;
  }
  else {
    *outstring++ = '%';
    for (i=0;i<strlen(cwidth);i++) *outstring++ = cwidth[i];
    *outstring++ = ctype;
    *length = iwidth;
  }
  *outstring++ = '\0';
  ival = 1;
  return(ival);
}

/**********************************************************************/
/*             SEPARATE VARIABLE NAME(S) FROM ELEMENT NUMBER          */
/**********************************************************************/
long Separate_Varnames(instring,vname1,vname2,velem1,velem2)
char *instring;
char *vname1;
char *vname2;
long *velem1;
long *velem2;
{
  char *cptr;
  long ierr =  0;
  long elem1 = -1;
  long elem2 = -1;

  *vname2 = '\0';                                 /* initialize          */
  cptr = memchr(instring,',',strlen(instring));   /* search for comma    */
  if (cptr != NULL) {                             /* comma found         */
    *cptr = '\0'; cptr++;                         /* terminate instring  */
    ierr=sscanf(cptr,"%ld",&elem2);               /* extract element #   */
    if (ierr != 1) {                              /* error reading elem# */
      strcpy(LOGFILE.message,"ERROR: Reading element2# for the variable:");
      strcat(LOGFILE.message,instring);   MSG_control(2,LOGFILE.message,0);
      ierr = 1; }                                 /* set error flag      */
    else { ierr=0; elem2 = elem2 - 1; }           /* base 0 not base1    */
  }
  cptr = memchr(instring,'(',strlen(instring));   /* search for L paren  */
  if (cptr != NULL) {                             /* L parenthesis found */
    *cptr = '\0'; cptr++;                         /* terminate instring  */
    ierr=sscanf(cptr,"%ld",&elem1);               /* extract element #   */
    if (ierr != 1) {                              /* error reading elem# */
      strcpy(LOGFILE.message,"ERROR: Reading element1# for the variable:");
      strcat(LOGFILE.message,instring);   MSG_control(2,LOGFILE.message,0);
      ierr = 1; }                                 /* set error flag      */
    else { ierr=0; elem1 = elem1 - 1; }           /* base 0 not base1    */
  }
  cptr = memchr(instring,'=',strlen(instring));   /* search for equals   */
  if (cptr != NULL) {                             /* varname equivalence */
    *cptr = '\0';                                 /* terminate instring  */
    cptr++; strcpy(vname2,cptr);                  /* copy CDF varname    */
  }
  strcpy(vname1,instring);                        /* copy input varname  */
  *velem1 = elem1;                                /* pass elem1# out     */
  *velem2 = elem2;                                /* pass elem2# out     */
  return(ierr);
}

/**********************************************************************/
/*                     IMPORT THE RUNTIME PARAMETERS                  */
/**********************************************************************/
long Import_RPARMS(file_ptr)
FILE *file_ptr;
{
  char buffer[81];
  char operand[81];
  char sval[20];
  long i,done,found,count,ival;
  long ierr=0;

  MSG_control(1,"Reading runtime parameters.",0);

  /* Locate runtime parameters portion of ffd file */
  found = FALSE; done = FALSE; /* initialize loop flags   */
  do {
    if (fgets(buffer,80,file_ptr) != NULL) {
      if (RPARMS.debug==TRUE) MSG_control(0,buffer,0);
      if (memcmp(buffer,";",1) != 0) {
        count = sscanf(buffer,"%s",operand);
        if (count == 1) {
          for (i=0;i<strlen(operand);i++) operand[i]=toupper(operand[i]);
          if (strcmp(operand,"RUNTIME_PARAMETERS") == 0) found = TRUE;
    } } }
    else done=TRUE;
  } while ((found==FALSE)&&(done==FALSE));
  if (found==FALSE) {
    MSG_control(1,"WARNING: RUNTIME_PARAMETERS not found...Using defaults.",0);
    return(ierr); }
  else done=FALSE;

  /* Read the run time parameters from the ffd file */
  do {
  if (fgets(buffer,80,file_ptr) != NULL) {
    if (RPARMS.debug==TRUE) MSG_control(0,buffer,0);
    if (memcmp(buffer,";",1) != 0) {
      count = sscanf(buffer,"%s",operand);
      if (count==1) {

        for (i=0;i<strlen(operand);i++) operand[i]=toupper(operand[i]);
        if (strcmp(operand,"PROGRESS_OUTPUT")==0) {
          count=sscanf(buffer,"%s %s",operand,sval);
          if (count==2) {
            ival=Validate_BOOLEAN(sval);
            if (ival != -1) RPARMS.progress=ival;
            else MSG_control(1,"WARNING: Unknown value for progress_output",0);}
          else MSG_control(1,"WARNING: Missing value for operand.",0);
        }
        else if (strcmp(operand,"DEBUG_OUTPUT")==0) {
          count=sscanf(buffer,"%s %s",operand,sval);
          if (count==2) {
            ival=Validate_BOOLEAN(sval);
            if (ival != -1) RPARMS.debug=ival;
            else MSG_control(1,"WARNING: Unknown value for debug_output.",0);}
          else MSG_control(1,"WARNING: Missing value for operand.",0);
        }
        else if (strcmp(operand,"LOG_TO_SCREEN")==0) {
          count=sscanf(buffer,"%s %s",operand,sval);
          if (count==2) {
            ival=Validate_BOOLEAN(sval);
            if (ival != -1) RPARMS.sendtoterm=ival;
            else MSG_control(1,"WARNING: Unknown value for log_to_term.",0);}
          else MSG_control(1,"WARNING: Missing value for operand.",0);
        }
        else if (strcmp(operand,"LOG_TO_FILE")==0) {
          count=sscanf(buffer,"%s %s",operand,sval);
          if (count==2) {
            ival=Validate_BOOLEAN(sval);
            if (ival != -1) RPARMS.sendtolog=ival;
            else MSG_control(1,"WARNING: Unknown value for log_to_file.",0);}
          else MSG_control(1,"WARNING: Missing value for operand.",0);
        }
        else if (strcmp(operand,"LOGFILE_MAXRECS")==0) {
          count=sscanf(buffer,"%s %s",operand,sval);
          if (count==2) {
            ival=Validate_POSINT(sval);
            if (ival != -1) {RPARMS.maxlogrecs=ival; LOGFILE.maxrecs=ival;}
            else MSG_control(1,"WARNING:logfile_maxrecs must be +Integer.",0);}
          else MSG_control(1,"WARNING: Missing value for operand.",0);
        }
        else if (strcmp(operand,"LOWERCASE_AUTONAMING")==0) {
          count=sscanf(buffer,"%s %s",operand,sval);
          if (count==2) {
            ival=Validate_BOOLEAN(sval);
            if (ival != -1) RPARMS.autocase=ival;
            else MSG_control(1,"WARNING:autonaming case must be +Integer.",0);}
          else MSG_control(1,"WARNING: Missing value for operand.",0);
        }
        else if (strcmp(operand,"END_RUNTIME_PARAMETERS")==0) done=TRUE;
        else MSG_control(1,"WARNING: Unknown runtime parameter encountered.",0);
  } } }
  else done=TRUE;
  } while (done == FALSE);
  return (ierr);
}

/**********************************************************************/
/*                  IMPORT THE INPUT FILE DESCRIPTION                 */
/**********************************************************************/
long Import_INFILE(file_ptr)
FILE *file_ptr;
{
  char buffer[81];
  char operand[81];
  char sval[20];
  long i,done,found,count,ival;
  long ierr=0;

  MSG_control(1,"Reading infile description...",0);

  /* Locate input file description portion of ffd file */
  found = FALSE; done = FALSE; /* initialize loop flags   */
  do {
    if (fgets(buffer,80,file_ptr) != NULL) {
      if (RPARMS.debug==TRUE) MSG_control(0,buffer,0);
      if (memcmp(buffer,";",1) != 0) {
        count = sscanf(buffer,"%s",operand);
        if (count==1) {
          for (i=0;i<strlen(operand);i++) operand[i]=toupper(operand[i]);
          if (strcmp(operand,"INFILE_DESCRIPTION") == 0) found = TRUE;
    } } }
    else done=TRUE;
  } while ((found==FALSE)&&(done==FALSE));
  if (found==FALSE) {
    MSG_control(1,"WARNING: INFILE_DESCRIPTION  not found...Using defaults.",0);
    return(ierr); }
  else done=FALSE;

  /* Read the input file description from the ffd file */
  do {
  if (fgets(buffer,80,file_ptr) != NULL) {
    if (RPARMS.debug==TRUE) MSG_control(0,buffer,0);
    if (memcmp(buffer,";",1) != 0) {
      count = sscanf(buffer,"%s",operand);
      if (count==1) {
        for (i=0;i<strlen(operand);i++) operand[i]=toupper(operand[i]);
        if (strcmp(operand,"FORMAT")==0) {
          count=sscanf(buffer,"%s %s",operand,sval);
          if (count==2) {
            for (i=0;i<strlen(sval);i++) sval[i]=toupper(sval[i]);
            if (strcmp(sval,"FREEFORM")==0) INFILE.format=0;
            else if (strcmp(sval,"FORMATTED")==0) INFILE.format=1;
            else MSG_control(1,"WARNING: Unknown value for format operand",0);}
          else MSG_control(1,"WARNING: Missing value for operand.",0);
        }
        else if (strcmp(operand,"DATA_TYPE")==0) {
          count=sscanf(buffer,"%s %s",operand,sval);
          if (count==2) {
            for (i=0;i<strlen(sval);i++) sval[i]=toupper(sval[i]);
            if (strcmp(sval,"TEXT")==0) INFILE.dtype=0;
            else if (strcmp(sval,"BINARY")==0) {
              if (INFILE.format==0) 
                MSG_control(2,"ERROR: Freeform datasets MUST be TEXT",0);
            else INFILE.dtype=1; }
            else MSG_control(1,"WARNING: Unknown value for data_type.",0);}
          else MSG_control(1,"WARNING: Missing value for operand.",0);
        }
        else if (strcmp(operand,"TEXT_TYPE")==0) {
          count=sscanf(buffer,"%s %s",operand,sval);
          if (count==2) {
            for (i=0;i<strlen(sval);i++) sval[i]=toupper(sval[i]);
            if (strcmp(sval,"ASCII")==0) INFILE.ttype=0;
            else if (strcmp(sval,"EBCDIC")==0) INFILE.ttype=1;
            else MSG_control(1,"WARNING: Unknown value for text_type.",0);}
          else MSG_control(1,"WARNING: Missing value for operand.",0);
        }
        else if (strcmp(operand,"BINARY_TYPE")==0) {
          count=sscanf(buffer,"%s %s",operand,sval);
          if (count==2) {
          for (i=0;i<strlen(sval);i++) sval[i]=toupper(sval[i]);
          if (strcmp(sval,"VAX")==0) INFILE.btype=VAX_ENCODING;
          else if (strcmp(sval,"ALPHAVMSD")==0) INFILE.btype=ALPHAVMSd_ENCODING;
          else if (strcmp(sval,"ALPHAVMSG")==0) INFILE.btype=ALPHAVMSg_ENCODING;
          else if (strcmp(sval,"SUN")==0) INFILE.btype=SUN_ENCODING;
          else if (strcmp(sval,"SGI")==0) INFILE.btype=SGi_ENCODING;
          else if (strcmp(sval,"DECSTATION")==0) 
                                INFILE.btype=DECSTATION_ENCODING;
          else if (strcmp(sval,"ALPHAOSF1")==0) INFILE.btype=ALPHAOSF1_ENCODING;
          else if (strcmp(sval,"IBMRS")==0) INFILE.btype=IBMRS_ENCODING;
          else if (strcmp(sval,"IBMPC")==0) INFILE.btype=IBMPC_ENCODING;
          else if (strcmp(sval,"HP")==0) INFILE.btype=HP_ENCODING;
          else if (strcmp(sval,"NEXT")==0) INFILE.btype=NeXT_ENCODING;
          else if (strcmp(sval,"MAC")==0) INFILE.btype=MAC_ENCODING;
          else MSG_control(1,"WARNING: Unknown value for binary_type.",0);}
          else MSG_control(1,"WARNING: Missing value for operand.",0);
        }
        else if (strcmp(operand,"DELIMITER")==0) {
          count=sscanf(buffer,"%s %s",operand,sval);
          if (count==2) {
            if (strcmp(sval,"blank")==0) INFILE.delimiter=' ';
            else if (strcmp(sval,"BLANK")==0) INFILE.delimiter=' ';
            else INFILE.delimiter=sval[0]; }
          else MSG_control(1,"WARNING: Missing value for operand.",0);
        }
        else if (strcmp(operand,"END_INFILE_DESCRIPTION")==0) done=TRUE;
        else MSG_control(1,"WARNING: Unknown infile parameter encountered.",0);
  } } }
  else done=TRUE;
  } while (done == FALSE);
  return (ierr);
}

/**********************************************************************/
/*                     IMPORT THE EPOCH PARAMETERS                    */
/**********************************************************************/
long Import_EpALG(file_ptr)
FILE *file_ptr;
{
  char buffer[81];
  char operand[81];
  char sval[9][20];
  long i,done,found,count,ival;
  long ierr=0;

  MSG_control(1,"Reading Epoch description...",0);

  /* Locate epoch parameters portion of ffd file */
  found = FALSE; done = FALSE; /* initialize loop flags   */
  do {
    if (fgets(buffer,80,file_ptr) != NULL) {
      if (RPARMS.debug==TRUE) MSG_control(0,buffer,0);
      if (memcmp(buffer,";",1) != 0) {
        count = sscanf(buffer,"%s",operand);
        if (count==1) {
          for (i=0;i<strlen(operand);i++) operand[i]=toupper(operand[i]);
          if (strcmp(operand,"EPOCH_DESCRIPTION") == 0) found = TRUE;
    } } }
    else done=TRUE;
  } while ((found==FALSE)&&(done==FALSE));
  if (found==FALSE) {
    MSG_control(2,"ERROR: EPOCH_DESCRIPTION not found...NO defaults.",0);
    ierr = -1; return(ierr); }
  else done=FALSE;

  /* Read the epoch description parameters from the ffd file */
  do {
  if (fgets(buffer,80,file_ptr) != NULL) {
    if (RPARMS.debug==TRUE) MSG_control(0,buffer,0);
    if (memcmp(buffer,";",1) != 0) {
      count = sscanf(buffer,"%s",operand);
      if (count==1) {
        for (i=0;i<strlen(operand);i++) operand[i]=toupper(operand[i]);
        if (strcmp(operand,"ALGORITHM")==0) {
          count=sscanf(buffer,"%s %s",operand,sval[0]);
          if (count==2) {
            ival=Validate_POSINT(sval[0]);
            if (ival != -1) EpALG.algorithm=ival;
            else {
              MSG_control(2,"ERROR: Unknown value for Epoch algorithm.",0);
              done=TRUE; ierr = -1;
          } }
          else MSG_control(2,"ERROR: Missing value for Epoch algorithm.",0);
        }
        else if (strcmp(operand,"OPERANDS")==0) {
          count=sscanf(buffer,"%s %s %s %s %s %s %s %s %s %s %s",operand,
                       sval[0],sval[1],sval[2],sval[3],sval[4],
                       sval[5],sval[6],sval[7],sval[8],sval[9]);
          if (count==11) {
            for (i=0;i<10;i++) {
              ival=Validate_POSINT(sval[i]);
              if (ival != -1) EpALG.operand[i]=ival;
              else {
                MSG_control(2,"ERROR: Invalid value found in Epoch operands",0);
                done=TRUE; ierr = -1;
          } } }
          else MSG_control(2,"ERROR: Incorrect number of Epoch operands.",0);
        }
        else if (strcmp(operand,"END_EPOCH_DESCRIPTION")==0) done=TRUE;
        else MSG_control(2,"ERROR: Unknown EPOCH parameter encountered.",0);
  } } }
  else done=TRUE;
  } while (done == FALSE);

  /* Because variable numbers in the ffd file file base 1 but C is */
  /* base 0, subtract one from any operand which represends a var# */
  switch (EpALG.algorithm) {
  case 0 : for(i=0;i<10;i++) EpALG.operand[i]=EpALG.operand[i]-1; break;
  case 1 : EpALG.operand[0] = EpALG.operand[0] - 1; break;
  case 2 : EpALG.operand[0] = EpALG.operand[0] - 1; break;
  case 3 : EpALG.operand[0] = EpALG.operand[0] - 1; break;
  case 4 : EpALG.operand[0] = EpALG.operand[0] - 1; break;
  case 5 : EpALG.operand[7] = EpALG.operand[7] - 1; break;
  case 6 : for(i=0;i<8;i++) EpALG.operand[i]=EpALG.operand[i]-1; break;
  case 7 : EpALG.operand[7] = EpALG.operand[7]-1;
  }
  return (ierr);
}

/**********************************************************************/
/*                     IMPORT THE VARIABLES                           */
/**********************************************************************/
long Import_Variables(file_ptr)
FILE *file_ptr;
{
  char buffer[133];
  char operand[81];
  char sval[16][20];
  char instring[80];
  char vname1[40];
  char vname2[40];
  char cform[10];
  double dfill;
  long i,done,found,count,ielem1,ielem2,ival,ilength,icdftype;
  long isize,ieperr,imaj,icon,ieol,ihead,ifill,ibsub,iesub,inrep,idelta;
  long iavx,iasx,iafx,iafil;
  long ierr=0;

  MSG_control(1,"Reading Variable descriptions...",0);

  /* Locate epoch parameters portion of ffd file */
  found = FALSE; done = FALSE; /* initialize loop flags   */
  do {
    if (fgets(buffer,132,file_ptr) != NULL) {
      if (RPARMS.debug==TRUE) MSG_control(0,buffer,0);
      if (memcmp(buffer,";",1) != 0) {
        count = sscanf(buffer,"%s",operand);
        if (count==1) {
          for (i=0;i<strlen(operand);i++) operand[i]=toupper(operand[i]);
          if (strcmp(operand,"VARIABLE_DESCRIPTIONS") == 0) found = TRUE;
    } } }
    else done=TRUE;
  } while ((found==FALSE)&&(done==FALSE));
  if (found==FALSE) {
    MSG_control(2,"ERROR: VARIABLE_DESCRIPTIONS not found...NO defaults.",0);
    ierr = -1; return(ierr); }
  else done=FALSE;

  /* Read the epoch description parameters from the ffd file */
  do {
  if (fgets(buffer,132,file_ptr) != NULL) {
    if (RPARMS.debug==TRUE) MSG_control(0,buffer,0);
    if (memcmp(buffer,";",1) != 0) {
      count = sscanf(buffer,"%s",operand);
      if (count==1) {
       for (i=0;i<strlen(operand);i++) operand[i]=toupper(operand[i]);
       if (strcmp(operand,"END_VARIABLE_DESCRIPTIONS")==0) done=TRUE;
       else {
       count=sscanf(buffer,"%s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s",
                    instring,sval[0],sval[1],sval[2],sval[3],sval[4],sval[5],
                    sval[6],sval[7],sval[8],sval[9],sval[10],sval[11],sval[12],
                    sval[13],sval[14],sval[15]);
          if (count==17) {
            isize =Validate_POSINT(sval[0]); if (isize==0)  isize=1;
            ival  =Validate_FORMAT(sval[1],cform,&ilength,&icdftype);
            ihead =Validate_BOOLEAN(sval[2]);
            ieol  =Validate_BOOLEAN(sval[3]);
            ibsub =Validate_POSINT(sval[4]);
            iesub =Validate_POSINT(sval[5]);
            inrep =Validate_POSINT(sval[6]);
            idelta=Validate_POSINT(sval[7]);
            ieperr=Validate_POSINT(sval[8]);
            if (ieperr == 0)  ieperr = 1;     /* correct to default of 1 */
            if (ieperr == -1) ieperr = isize; /* illegal same as flat */
            imaj  =Validate_POSINT(sval[9]);
            icon  =Validate_BOOLEAN(sval[10]);
            iavx  =Validate_BOOLEAN(sval[11]);
            iasx  =Validate_BOOLEAN(sval[12]);
            iafx  =Validate_BOOLEAN(sval[13]);
            iafil =Validate_BOOLEAN(sval[14]);
            dfill =Validate_FILLVAL(sval[15],icdftype);
            ierr  =Separate_Varnames(instring,vname1,vname2,&ielem1,&ielem2);
            if (ierr != 0) done=TRUE;
            /* allocate another VLIST element */
            VLISTne++;
            VLIST=realloc(VLIST,(sizeof(VLISTz)*VLISTne));
/* make vname and cdfvname fixed length and try again
            VLIST[VLISTne-1].vname = malloc(sizeof(strlen(vname1)+1));
            VLIST[VLISTne-1].cdfvname = malloc(sizeof(strlen(vname2)+1));
*/
            strcpy(VLIST[VLISTne-1].vname,vname1);
            strcpy(VLIST[VLISTne-1].cdfvname,vname2);
            strcpy(VLIST[VLISTne-1].format,cform);
            if (isize <= 1) {
                   VLIST[VLISTne-1].ndims=0;
                   VLIST[VLISTne-1].nelems=1;
            }
            else { VLIST[VLISTne-1].ndims=1;
                   VLIST[VLISTne-1].dims[0]= isize;
                   VLIST[VLISTne-1].nelems = isize;
            }
            VLIST[VLISTne-1].Elem[0]    = ielem1;
            VLIST[VLISTne-1].Elem[1]    = ielem2;
            VLIST[VLISTne-1].length     = ilength;
            VLIST[VLISTne-1].cdfvartype = icdftype;
            VLIST[VLISTne-1].eperr      = ieperr;
            VLIST[VLISTne-1].majority   = imaj;
            VLIST[VLISTne-1].constant   = icon;
            VLIST[VLISTne-1].header     = ihead;
            VLIST[VLISTne-1].eol        = ieol;
            VLIST[VLISTne-1].bsubr      = ibsub;
            VLIST[VLISTne-1].esubr      = iesub;
            VLIST[VLISTne-1].autovalids = iavx;
            VLIST[VLISTne-1].autoscales = iasx;
            VLIST[VLISTne-1].autoformat = iafx;
            VLIST[VLISTne-1].autofill   = iafil;
            VLIST[VLISTne-1].fillval    = dfill;
            /* add new subrecord if needed */
            if ((long)ibsub >= SRECSne+1) {
              MSG_control(0,"Adding new subrecord",0);
              SRECSne++;
              SRECS=realloc(SRECS,(sizeof(SRECSz)*SRECSne));
              SRECS[SRECSne-1].nreps  = inrep;
              SRECS[SRECSne-1].delta  = (double)idelta;
              SRECS[SRECSne-1].countr = 0;
  } } } } } }
  else done=TRUE;
  } while (done == FALSE);
  VLIST[VLISTne-1].eol=1; /* last variable always has EOL flag */
  return (ierr);
}

/**********************************************************************/
/*                      IMPORT THE DESCRIPTION FILE                   */
/**********************************************************************/
long Import_FFD()
{
  FILE *file_ptr;
  long ierr=0;

  /* open file which contains the description of input data file */
  file_ptr = fopen(RPARMS.ffdfname,"r");
  if (file_ptr == NULL) {
    MSG_control(2,"ERROR: Unable to open input description file",0);
    MSG_control(2,"       Translation Aborted.",0);
    ierr=1; return(ierr);
  }

  ierr = Import_RPARMS(file_ptr);
  if (ierr == 0) {
    ierr = Import_INFILE(file_ptr);
    if (ierr == 0) {
      ierr = Import_EpALG(file_ptr);
      if (ierr == 0) {
        ierr = Import_Variables(file_ptr);
  } } }

  MSG_control(1,"Translation file input completed.",0); MSG_control(1," ",0);
  fclose(file_ptr);
  return (ierr);
}

/**********************************************************************/
/*                  DETERMINE SUBRECORD BOUNDARIES                    */
/**********************************************************************/
long SREC_Bounds(subrecno, firstvar, lastvar)
long subrecno;
long *firstvar;
long *lastvar;
{
  long i,j;
  long ierr=0;

  j= subrecno + 1;
  for (i=0;i<VLISTne;i++) if (VLIST[i].bsubr == j) *firstvar = i;
  for (i=0;i<VLISTne;i++) if (VLIST[i].esubr == j) *lastvar  = i;

/*
  if (subrecno == 0) {
    *firstvar = 0;
    *lastvar = VLISTne-1; }
  else {
    for (i=0;i<VLISTne;i++) if (VLIST[i].bsubr == subrecno) *firstvar = i;
    for (i=0;i<VLISTne;i++) if (VLIST[i].esubr == subrecno) *lastvar  = i;
  }
*/

  return(ierr);
}

/**********************************************************************/
/*             BUILD THE ORDERED LIST OF VARIABLES TO READ            */
/**********************************************************************/
long Build_OLIST()
{
  long g,h,i,j,k,l,m,n,iflag,rflag,pflag,past_headers,done,addeol;
  long ierr=0;
  long *ecount, *rcount;

  MSG_control(1,"Contructing the Ordered List for data stream input...",0);
  past_headers = FALSE;
  for (i=0;i<SRECSne;i++) {
    for (j=0;j<SRECS[i].nreps;j++) {
      iflag=SREC_Bounds(i,&n,&m); /* find first and last var in subrec i */
      ecount = calloc((size_t)m+1,sizeof(long));
      rcount = calloc((size_t)m+1,sizeof(long));
      for (h=n;h<=m;h++) {
        ecount[h]=VLIST[h].nelems;
        rcount[h]=ecount[h];
      }

      done = FALSE;
      while (done == FALSE) {
        done = TRUE;
        for (k=n;k<=m;k++) { /* process each variable in subrecord */
          for (l=0;l<VLIST[k].eperr;l++) { /* process each element */
            rflag = TRUE;                  /* initialize read flag */
            if (VLIST[k].header == FALSE) past_headers = TRUE;
            if ((VLIST[k].header == TRUE)&&(past_headers == TRUE)) rflag=FALSE;
            if (ecount[k] > 0) {           /* elements still to be processed */
              pflag = TRUE;                /* process the element */
              ecount[k] = ecount[k]-1;     /* adjust the element counter */
            }
            else {                         /* element count reached */
              pflag = FALSE;               /* no new element to process */
              if (VLIST[k].eol == TRUE) rflag=FALSE; /* no element to read */
            }
            if (VLIST[k].length == 0) rflag=FALSE;   /* no element to read */

            /* add new element to the ordered list */
            OLISTne++;
            OLIST=realloc(OLIST,(sizeof(OLISTz)*OLISTne));
            OLIST[OLISTne-1].vnum  = k;
            OLIST[OLISTne-1].velem = rcount[k]-ecount[k]-1;
            OLIST[OLISTne-1].rflag = rflag;
            OLIST[OLISTne-1].pflag = pflag;
          }
          /* determine if eol should be added to olist */
          addeol = FALSE;
          if (VLIST[k].eol == TRUE) addeol = TRUE; /* check rflag? */
          if ((k == (VLISTne-1))&&(ecount[k] == 0)) addeol = TRUE;
          if (addeol == TRUE) { /* add eol read to olist */
            OLISTne++;
            OLIST=realloc(OLIST,(sizeof(OLISTz)*OLISTne));
            OLIST[OLISTne-1].vnum  = -1;
            OLIST[OLISTne-1].velem = 0;
            OLIST[OLISTne-1].rflag = TRUE;
            OLIST[OLISTne-1].pflag = FALSE;
          }
          if (ecount[k] > 0) done = FALSE;
        }
      }
      free(ecount); free(rcount);
    }
  }
  /* ordered list construction is complete.*/
  if (RPARMS.progress == TRUE) {
    MSG_control(1,"Ordered List construction completed.",0);
  }
  if (RPARMS.debug == TRUE) {
    MSG_control(0,"------ ORDERED LIST -----",0);
    MSG_control(0,"   v#    e# r p ",0);
    g=OLISTne-1;
    if (g > 200) { 
      MSG_control(0,"WARNING: Only showing first 200 OLIST elements",0);
      g = 200;
    }
    for (i=0;i<=g;i++) {
      sprintf(LOGFILE.message,"%5d %5d %d %d \n",
              OLIST[i].vnum,OLIST[i].velem,OLIST[i].rflag,OLIST[i].pflag);
      MSG_control(0,LOGFILE.message,0);
    }
  }
  return(ierr);
}

/**********************************************************************/
/*      DETERMINE IF INPUT CDF IS IN FORM OF CDF OR SKELETON TABLE    */
/**********************************************************************/
long inCDFform (filename)
char *filename;
{
  char suffix[5] = "";
  long length,i,j;
  long iform = 0;

  length=(long)strlen(filename);
  if (length >= 4) {
    for (i=0,j=length-4;i<4;i++,j++) suffix[i]=toupper(filename[j]);
    suffix[4]='\0';
    if (strcmp(suffix,".CDF")==0) iform=1;
    else if (strcmp(suffix,".SKT")==0) iform=2;
  }
  if (iform==0) {
    MSG_control(2,"ERROR: Cannot determine if the input CDF is in the form",0);
    MSG_control(2,"       of a CDF or a skeleton table because of a missing",0);
    MSG_control(2,"       or unrecognized file extension.  Recognized file ",0);
    MSG_control(2,"       extensions are .cdf and .skt.",0);
  }
  return(iform);
}

/**********************************************************************/
/*      GENERATE CDF SKELETON TO BE POPULATED WITH INPUT DATA         */
/**********************************************************************/
long Generate_CDFskeleton()
{
   long iform,eNum,numrecs,allswell;
   long ierr=0;
   char tempname[80];
   char command[80];
   CDFid id;
   CDFstatus status;

   iform = inCDFform(RPARMS.incdfname);
   if (iform == 2) { /* input cdf description is a skeleton table */
     StripName(RPARMS.incdfname,tempname);
     StripName(RPARMS.outcdfname,RPARMS.outcdfname);
     strcpy(command,"skeletoncdf /CDF=");
#if defined UNIX_OS
     strcpy(command,"$CDF_BIN/skt2cdf -cdf ");
#endif
     strcat(command,RPARMS.outcdfname); strcat(command," ");
     strcat(command,tempname); strcat(command,"");
     MSG_control(1,"Creating empty CDF from skeleton table...",0);
     MSG_control(0,command,0);
     allswell=system(command);
     if (allswell != SYS_OK) {
       MSG_control(2,"ERROR: During creation of cdf from skeleton table",0);
       ierr=1;
   } }
   else if (iform == 1) { /* input cdf description is a CDF */
     /* Determine the number of records in the CDF */
     MSG_control(1,"Opening input CDF to determine empty or full...",0);
     allswell=FALSE;
     StripName(RPARMS.incdfname,tempname);
     StripName(RPARMS.outcdfname,RPARMS.outcdfname);
     status = CDFlib(OPEN_,CDF_,tempname,&id,NULL_);
     if (status <= CDF_WARN) MSG_control(2,"CDFERROR:",status);
     else {
       MSG_control(0,"Getting the variable number of Epoch...",0);
       status = CDFlib(SELECT_, CDF_, id,
                       GET_,zVAR_NUMBER_,"Epoch",&eNum,NULL_);
       if (status != CDF_OK) MSG_control(2,"CDFERROR:",status);
       else {
         MSG_control(0,"Getting the number of records for Epoch...",0);
         status = CDFlib(SELECT_, CDF_, id,
                                  zVAR_, eNum,
                         GET_, zVARs_MAXREC_, &numrecs, NULL_);
         if (status != CDF_OK) MSG_control(2,"CDFERROR:",status);
         else { 
           MSG_control(0,"Closing the input CDF...",0);
           status = CDFlib(CLOSE_, CDF_, NULL_);
           if (status != CDF_OK) MSG_control(2,"CDFERROR:",status);
           else allswell=TRUE;
     } } }

     if (allswell==TRUE) {
       if (numrecs == -1) { /* cdf is empty */
         strcpy(command,"copy ");
#if defined UNIX_OS
         strcpy(command,"cp ");
#endif
         strcat(command,tempname) ; strcat(command," ");
         strcat(command,RPARMS.outcdfname); strcat(command,"");
         MSG_control(1,"Creating copy of empty cdf...",0);
         MSG_control(0,command,0);
         allswell=system(command);
         if (allswell != SYS_OK) {
           MSG_control(2,"ERROR: During creation of copy of empty cdf.",0);
           ierr=1;
       } }
       else { /* cdf is not empty */
         strcpy(command,"skeletontable ");
#if defined UNIX_OS
         strcpy(command,"$CDF_BIN/cdf2skt ");
#endif
         strcat(command,tempname); strcat(command," ");
         MSG_control(1,"Creating skeleton table from non-empty CDF...",0);
         MSG_control(0,command,0);
         allswell=system(command);
         if (allswell != SYS_OK) {
           MSG_control(2,"ERROR: During creation of skeleton table.",0);
           ierr=1; }
         else {
           strcpy(command,"skeletoncdf /CDF=");
#if defined UNIX_OS
           strcpy(command,"$CDF_BIN/skt2cdf ");
#endif
           strcat(command,RPARMS.outcdfname); strcat(command," ");
           strcat(command,tempname);  strcat(command,"");
           MSG_control(1,"Creating cdf from skeleton table",0);
           MSG_control(0,command,0);
           allswell=system(command);
           if (allswell != SYS_OK) {
             MSG_control(2,"ERROR: During creation of cdf.",0);
             ierr=1;
     } } } }
     else ierr=1;
   }
   else {
     MSG_control(2,RPARMS.incdfname,0);
     MSG_control(2,"ERROR: Missing or Unknown file extension.",0);
     MSG_control(2,"       Cannot determine form of input cdf description.",0);
     ierr=1;
   }
   return(ierr);
}

/**********************************************************************/
/*            Determine CDF var#'s for input variables                */
/**********************************************************************/
long Match_Variables()
{
  CDFstatus status;
  char vname[40];
  long i,itype,isize;
  long ierr=0;

  MSG_control(1,"Matching input variables to output CDF vars...",0);
  status = CDFlib(OPEN_,CDF_,RPARMS.outcdfname,&OUTCDF.outCDFid,
                  SELECT_, CDF_zMODE_, zMODEon2, NULL_);
  if (status <= CDF_WARN) {
    MSG_control(2,"CDFERROR:",status);
    ierr=1; return(ierr);
  }

  for (i=0;i<VLISTne;i++) {
    strcpy(vname,VLIST[i].vname);
    if (strlen(VLIST[i].cdfvname) != 0) strcpy(vname,VLIST[i].cdfvname);
    status = CDFlib(GET_,zVAR_NUMBER_,vname,&VLIST[i].cdfvarnum,NULL_);
    if (status != CDF_OK) {
      VLIST[i].cdfvarnum  = -1;
      strcpy(LOGFILE.message,"WARNING: the input variable '");
      strcat(LOGFILE.message,VLIST[i].vname);
      strcat(LOGFILE.message,"' does not exist in input CDF.");
      MSG_control(1,LOGFILE.message,0); }
    else { /* variable is found, get varnum and vartype */
      status = CDFlib(SELECT_,zVAR_,VLIST[i].cdfvarnum,
                      GET_,zVAR_DATATYPE_,&VLIST[i].cdfvartype,NULL_);
      if (status != CDF_OK) MSG_control(1,"CDFERROR:",status);
      else {
        /* determine type & byte-size of the data space to be allocated */
        itype = VLIST[i].cdfvartype;
        if ((itype==CDF_INT1)||(itype==CDF_UINT1)||(itype==CDF_BYTE)) isize=1;
        else if ((itype==CDF_CHAR) ||(itype==CDF_UCHAR))  isize=1;
        else if ((itype==CDF_INT2) ||(itype==CDF_UINT2))  isize=2;
        else if ((itype==CDF_INT4) ||(itype==CDF_UINT4))  isize=4;
        else if ((itype==CDF_REAL4)||(itype==CDF_FLOAT))  isize=4;
        else if ((itype==CDF_REAL8)||(itype==CDF_DOUBLE)) isize=8;
        else if (itype==CDF_EPOCH) isize=8;
        else { 
          MSG_control(2,"ERROR: Unknown CDF datatype encountered during ",0);
          MSG_control(2,"       Buffer and space allocation ... ",0);
          ierr=1; return(ierr);
        }
        /* allocate another CLIST element */
        CLISTne++;
        CLIST=realloc(CLIST,(sizeof(CLISTz)*CLISTne));
        CLIST[CLISTne-1].flagone = 0L;             /* initialize flag   */
        CLIST[CLISTne-1].ccount  = 0L;             /* initialize countr */
        CLIST[CLISTne-1].vmin = calloc(1L,isize);  /* space for min     */
        CLIST[CLISTne-1].vmax = calloc(1L,isize);  /* space for max     */
        VLIST[i].clistnum = CLISTne-1;             /* link to VLIST     */
      }
    }
  }

  status = CDFlib(GET_,zVAR_NUMBER_,"Epoch",&EpALG.Epvarnum,NULL_);
  if (status != CDF_OK) {
    MSG_control(2,"ERROR: CDF does not contain variable named:Epoch",status);
    ierr=1;
  }

  status = CDFlib(CLOSE_,CDF_);
  return(ierr);
}

/**********************************************************************/
/*               Insert 'l' into format because var is DOUBLE         */
/**********************************************************************/
long Fix_Format(format, newform)
char *format;
char *newform;
{
  long ierr=0;
  while (*format != 'f') *newform++ = *format++;
  *newform++ = 'l';
  while (*format != '\0') *newform++ = *format++;
  *newform = '\0';
  return (ierr);
}

/**********************************************************************/
/*               Allocate Buffer Space for Variables                  */
/**********************************************************************/
long Allocate_Buffers()
{
  long i,j,ecount,itype,isize,Last_True_Var;
  char newform[15]="";
  long ierr=0;

  for (i=0;i<VLISTne;i++) {
    /* allocate character buffer space */
    VLIST[i].buffer = malloc(VLIST[i].length+1);
    if (VLIST[i].buffer == NULL) {
      strcpy(LOGFILE.message,"ERROR: Unable to allocate data space for '");
      strcat(LOGFILE.message,VLIST[i].vname);
      strcat(LOGFILE.message,"' variable.  Unable to continue.");
      MSG_control(2,LOGFILE.message,0);
      ierr=1; return(ierr);
    }

    /* allocate data buffer space */
    ecount=VLIST[i].nelems;
    itype = VLIST[i].cdfvartype;
    if ((itype==CDF_INT1)||(itype==CDF_UINT1)||(itype==CDF_BYTE)) isize=1;
    else if ((itype==CDF_CHAR) ||(itype==CDF_UCHAR))  isize=1;
    else if ((itype==CDF_INT2) ||(itype==CDF_UINT2))  isize=2;
    else if ((itype==CDF_INT4) ||(itype==CDF_UINT4))  isize=4;
    else if ((itype==CDF_REAL4)||(itype==CDF_FLOAT))  isize=4;
    else if ((itype==CDF_REAL8)||(itype==CDF_DOUBLE)) isize=8;
    else if (itype==CDF_EPOCH) isize=8;
    else { 
      MSG_control(2,"ERROR: Unknown CDF datatype encountered during ",0);
      MSG_control(2,"       Buffer and space allocation ... ",0);
      ierr=1; return(ierr);
    }
    /* allocate the data space */
    if (VLIST[i].length != 0) { /* normal allocation */
      VLIST[i].dataptr = calloc((ecount+1), isize);
      Last_True_Var = i;
    }
    else { /* special case: input variable is in > 1 cdf variable */
      VLIST[i].dataptr = VLIST[Last_True_Var].dataptr;
    }
    /* test allocation results */
    if (VLIST[i].dataptr == NULL) {
      strcpy(LOGFILE.message,"ERROR: Unable to allocate data space for '");
      strcat(LOGFILE.message,VLIST[i].vname);
      strcat(LOGFILE.message,"' variable.  Unable to continue.");
      MSG_control(2,LOGFILE.message,0);
      ierr=1; return(ierr);
    }
    if (isize == 8) {
      ierr=Fix_Format(VLIST[i].format,newform);
      strcpy(VLIST[i].format,newform);
    }
  }
  return(ierr);
}

/**********************************************************************/
/*        Determine Size of Largest Input record in bytes             */
/**********************************************************************/
long Determine_Bufsize()
{
  long ibufsize=0;
  long imaxsize=0;
  long i,ilength,ipad;

  /* Determine the size of the input buffer by summing the lengths of */
  /* all elements of the OLIST, which are bounded by EOL markers.     */
  for (i=0;i<OLISTne-1;i++) {
    if (OLIST[i].vnum == -1) { /* EOL marker found */
      if (ibufsize > imaxsize) {imaxsize = ibufsize; ibufsize = 0;}
    }
    else {
      if (OLIST[i].rflag == TRUE) { /* only count bytes for readable data */
        ilength = VLIST[(OLIST[i].vnum)].length;
        ipad=0; if (INFILE.format == 0) ipad=1; /* pad if freeform */
        ibufsize= ibufsize + ilength + ipad;
      }
    }
  }
  if (INFILE.dtype == 0) ibufsize=ibufsize+2; /* bump up for text EOL */
  if (ibufsize > imaxsize) imaxsize = ibufsize;
  return(imaxsize);
}

/**********************************************************************/
/*        Read the next record from the input data file               */
/**********************************************************************/
void Read_Infile()
{
  if (INFILE.dtype == 0) { /* input data file is text */
    INFILE.bptr = INFILE.buffer;
    fgets(INFILE.buffer,INFILE.buflen,INFILE.fptr);
    INFILE.recnum = INFILE.recnum + 1;
    MSG_control(0,INFILE.buffer,0);
  }
  else {
    INFILE.buflen = Determine_Bufsize();
    INFILE.bptr = INFILE.buffer;
    fread(INFILE.buffer,INFILE.buflen,1,INFILE.fptr);
    INFILE.recnum = INFILE.recnum + 1;
  }
}

/**********************************************************************/
/*        Open the input file and allocate input buffer space         */
/**********************************************************************/
long Open_Infile(ifile)
long ifile;
{
  long ierr=0;
  long ilength=0;
  long i;

  INFILE.buflen = Determine_Bufsize();
  INFILE.buffer = malloc(INFILE.buflen);
  INFILE.bptr   = INFILE.buffer;
  if (INFILE.buffer == NULL) {
    MSG_control(2,"ERROR: Unable to allocate input buffer for reading file!",0);
    MSG_control(2,"       Unable to proceed with translation...",0);
    ierr=1; return(ierr);
  }

  /* construct name of next input data file from FLIST */
  strcpy(RPARMS.infname,FLIST[ifile].Pathname);
  strcat(RPARMS.infname,FLIST[ifile].Filename);

  /* output a progress message */
  strcpy(LOGFILE.message,"Opening Input File: "); 
  strcat(LOGFILE.message,RPARMS.infname);
  MSG_control(1,LOGFILE.message,0);

  /* open the file */
  if (INFILE.dtype == 0) INFILE.fptr = fopen(RPARMS.infname,"r");
  else INFILE.fptr = fopen(RPARMS.infname,"rb");
  if (INFILE.fptr == NULL) {
    MSG_control(2,"ERROR: Unable to open input data file.",0);
    MSG_control(2,"       Unable to proceed with translation...",0);
    ierr=1; return(ierr);
  }
  else INFILE.recnum = 0;

  MSG_control(1,"Reading the input file...",0);
  Read_Infile();
  if ( (long)*INFILE.buffer == 10) {
    MSG_control(1,"WARNING: Line Feed encountered at beginning of first",0);
    MSG_control(1,"         record of input file...Reading next record...",0);
    Read_Infile();
  }

  return(ierr);
}

/**********************************************************************/
/*     Freeform Scan the given variable from the input buffer         */
/**********************************************************************/
long Parser1(vnum,velem)
long vnum;
long velem;
{
  int  icnt,ccnt,dcnt,icountr;
  short int *sptr;
  long *lptr;
  float *fptr;
  double *dptr;
  char *cptr,*bptr;
  char form[20],sval[20];
  long idone=0;
  long icount=0;

  /* Count the number of delimiting characters at current point of inbuffer */
  for (bptr=INFILE.bptr,dcnt=0;*bptr==INFILE.delimiter;dcnt++,bptr++);
  INFILE.bptr = bptr; /* point to first character past delimiters */

  /* set the format argument for the sscanf function */
  strcpy(form,VLIST[vnum].format); strcat(form,"%n");

  /* execute the sscanf function based on data type and update buffer ptr */
  switch (VLIST[vnum].cdfvartype) {
    case CDF_REAL4 : fptr=(float *)VLIST[vnum].dataptr;
                     fptr=fptr+velem;
                     icnt=sscanf(INFILE.bptr,form,fptr,&ccnt);
                     INFILE.bptr=INFILE.bptr+ccnt;
                     break;
    case CDF_FLOAT : fptr=(float *)VLIST[vnum].dataptr;
                     fptr=fptr+velem;
                     icnt=sscanf(INFILE.bptr,form,fptr,&ccnt);
                     INFILE.bptr=INFILE.bptr+ccnt;
                     break;
    case CDF_REAL8 : dptr=(double *)VLIST[vnum].dataptr;
                     dptr=dptr+velem;
                     icnt=sscanf(INFILE.bptr,form,dptr,&ccnt);
                     INFILE.bptr=INFILE.bptr+ccnt;
                     break;
    case CDF_DOUBLE: dptr=(double *)VLIST[vnum].dataptr;
                     dptr=dptr+velem;
                     icnt=sscanf(INFILE.bptr,form,dptr,&ccnt);
                     INFILE.bptr=INFILE.bptr+ccnt;
                     break;
    case CDF_EPOCH : dptr=(double *)VLIST[vnum].dataptr;
                     dptr=dptr+velem;
                     icnt=sscanf(INFILE.bptr,form,dptr,&ccnt);
                     INFILE.bptr=INFILE.bptr+ccnt;
                     break;
    case CDF_INT4  : lptr=(long *)VLIST[vnum].dataptr;
                     lptr=lptr+velem;
                     icnt=sscanf(INFILE.bptr,form,lptr,&ccnt);
                     INFILE.bptr=INFILE.bptr+ccnt;
                     break;
    case CDF_UINT4 : lptr=(long *)VLIST[vnum].dataptr;
                     lptr=lptr+velem;
                     icnt=sscanf(INFILE.bptr,form,lptr,&ccnt);
                     INFILE.bptr=INFILE.bptr+ccnt;
                     break;
    case CDF_INT2  : sptr=(short int *)VLIST[vnum].dataptr;
                     sptr=sptr+velem;
                     icnt=sscanf(INFILE.bptr,form,sptr,&ccnt);
                     INFILE.bptr=INFILE.bptr+ccnt;
                     break;
    case CDF_UINT2 : sptr=(short int *)VLIST[vnum].dataptr;
                     sptr=sptr+velem;
                     icnt=sscanf(INFILE.bptr,form,sptr,&ccnt);
                     INFILE.bptr=INFILE.bptr+ccnt;
                     break;
    case CDF_CHAR  : if (VLIST[vnum].eol == FALSE) {
                       icnt=sscanf(INFILE.bptr,form,VLIST[vnum].buffer,&ccnt);
                       INFILE.bptr=INFILE.bptr+ccnt; }
                     else { /* cannot just read using sscanf */
                       while (*INFILE.bptr == INFILE.delimiter) INFILE.bptr++;
                       cptr=(char *)VLIST[vnum].buffer;
                       while (idone == 0) {
                         *cptr++ = *INFILE.bptr++;
                         icount++; if (icount == VLIST[vnum].length) idone=1;
                         if (*INFILE.bptr == '\n') idone=1;
                       }
                       strcat(VLIST[vnum].buffer,"\n"); icnt=1; /* success */
                     }
                     break;
    case CDF_UCHAR : if (VLIST[vnum].eol == FALSE) {
                       icnt=sscanf(INFILE.bptr,form,VLIST[vnum].buffer,&ccnt);
                       INFILE.bptr=INFILE.bptr+ccnt; }
                     else { /* cannot just read using sscanf */
                       while (*INFILE.bptr == INFILE.delimiter) INFILE.bptr++;
                       cptr=(char *)VLIST[vnum].buffer;
                       while (idone == 0) {
                         *cptr++ = *INFILE.bptr++;
                         icount++; if (icount == VLIST[vnum].length) idone=1;
                         if (*INFILE.bptr == '\n') idone=1;
                       }
                       strcat(VLIST[vnum].buffer,"\n"); icnt=1; /* succcess */
                     }
                     break;
    default        : cptr=(char *)VLIST[vnum].dataptr;
                     cptr=cptr+velem;
                     icnt=sscanf(INFILE.bptr,form,cptr,&ccnt);
                     INFILE.bptr=INFILE.bptr+ccnt;
                     break;
  }
  if (icnt != 1) { icountr=0;
    strcpy(LOGFILE.message,"WARNING: Read error has occured while processing");
    sprintf(sval,"%ld",(INFILE.recnum+1));
    strcat(LOGFILE.message," record#"); strcat(LOGFILE.message,sval);
    MSG_control(1,LOGFILE.message,0);
    strcpy(LOGFILE.message,"         Variable: "); 
    strcat(LOGFILE.message,VLIST[vnum].vname);
    strcat(LOGFILE.message,"  Element#");
    sprintf(sval,"%ld",(velem+1)); strcat(LOGFILE.message,sval);
    MSG_control(1,LOGFILE.message,0);
  }
  else {
    icountr = (long)ccnt;
    if (VLIST[vnum].ccount < icountr) VLIST[vnum].ccount=icountr;
  }
  return(icountr);
}

/**********************************************************************/
/*   Fixedform ASCII Scan the given variable from the input buffer    */
/**********************************************************************/
long Parser2(vnum,velem)
long vnum;
long velem;
{
  int   icnt,ccnt;
  short int *sptr;
  long *lptr;
  float *fptr;
  double *dptr;
  char *cptr;
  char form[20];
  char sval[20];
  char slen[10];
  long icountr=0;

  /* construct format statement for move from infile buffer to var buffer */
  icnt=sprintf(slen,"%ld",VLIST[vnum].length);
  strcpy(form,"%"); strcat(form,slen); strcat(form,"c%n");

  /* move n-characters from input buffer into the variables buffer */
  icnt=sscanf(INFILE.bptr,form,VLIST[vnum].buffer,&ccnt);
  INFILE.bptr=INFILE.bptr+ccnt;

  /* set the format argument for the sscanf function */
  strcpy(form,VLIST[vnum].format);

  /* convert character data in the variable buffer */
  switch (VLIST[vnum].cdfvartype) {
    case CDF_REAL4 : fptr=(float *)VLIST[vnum].dataptr;
                     fptr=fptr+velem;
                     icnt=sscanf(VLIST[vnum].buffer,form,fptr);
                     break;
    case CDF_FLOAT : fptr=(float *)VLIST[vnum].dataptr;
                     fptr=fptr+velem;
                     icnt=sscanf(VLIST[vnum].buffer,form,fptr);
                     break;
    case CDF_REAL8 : dptr=(double *)VLIST[vnum].dataptr;
                     dptr=dptr+velem;
                     icnt=sscanf(VLIST[vnum].buffer,form,dptr);
                     break;
    case CDF_DOUBLE: dptr=(double *)VLIST[vnum].dataptr;
                     dptr=dptr+velem;
                     icnt=sscanf(VLIST[vnum].buffer,form,dptr);
                     break;
    case CDF_EPOCH : dptr=(double *)VLIST[vnum].dataptr;
                     dptr=dptr+velem;
                     icnt=sscanf(VLIST[vnum].buffer,form,dptr);
                     break;
    case CDF_INT4  : lptr=(long *)VLIST[vnum].dataptr;
                     lptr=lptr+velem;
                     icnt=sscanf(VLIST[vnum].buffer,form,lptr);
                     break;
    case CDF_UINT4 : lptr=(long *)VLIST[vnum].dataptr;
                     lptr=lptr+velem;
                     icnt=sscanf(VLIST[vnum].buffer,form,lptr);
                     break;
    case CDF_INT2  : sptr=(short int *)VLIST[vnum].dataptr;
                     sptr=sptr+velem;
                     icnt=sscanf(VLIST[vnum].buffer,form,sptr);
                     break;
    case CDF_UINT2 : sptr=(short int *)VLIST[vnum].dataptr;
                     sptr=sptr+velem;
                     icnt=sscanf(VLIST[vnum].buffer,form,sptr);
                     break;
    case CDF_CHAR  : break; /* already moved */
    case CDF_UCHAR : break; /* already moved */
    default        : cptr=(char *)VLIST[vnum].dataptr;
                     cptr=cptr+velem;
                     icnt=sscanf(VLIST[vnum].buffer,form,cptr);
                     break;
  }
  if (icnt != 1) { icountr=0;
    strcpy(LOGFILE.message,"WARNING: Read error has occured while processing");
    sprintf(sval,"%ld",(INFILE.recnum+1));
    strcat(LOGFILE.message," record#"); strcat(LOGFILE.message,sval);
    MSG_control(1,LOGFILE.message,0);
    strcpy(LOGFILE.message,"         Variable: "); 
    strcat(LOGFILE.message,VLIST[vnum].vname);
    strcat(LOGFILE.message,"  Element#");
    sprintf(sval,"%ld",(velem+1)); strcat(LOGFILE.message,sval);
    MSG_control(1,LOGFILE.message,0);
  }
  else if (VLIST[vnum].ccount < (long)ccnt) VLIST[vnum].ccount = (long) ccnt;
  return(icountr);
}

/**********************************************************************/
/*   Fixedform BINARY Scan the given variable from the input buffer   */
/**********************************************************************/
long Parser3(vnum,velem)
long vnum;
long velem;
{
  CDFstatus status;
  long dpointer,offset;
  long One=1;
  long icnt=0;

  /* determine address for output of data conversion */
  dpointer = (long)VLIST[vnum].dataptr;
  offset   = VLIST[vnum].length * velem;
  dpointer = dpointer + offset;

  /* convert binary value in buffer to host format */
  status = ConvertBuffer(INFILE.btype,HOST_ENCODING,NEGtoPOSfp0on,
                         VLIST[vnum].cdfvartype,One,
                         (void *)INFILE.bptr,(void *)dpointer);
  if (status != CDF_OK) {
    MSG_control(2,"ERROR:During binary data conversion to HOST=",status);
  } else icnt = VLIST[vnum].length;

  /* advance buffer pointer */
  INFILE.bptr = INFILE.bptr + VLIST[vnum].length;

  return(icnt);
}

/************************************************************************/
/*   Compare data values to existing min and max for the given variable */
/************************************************************************/
long AutoMinMax(vnum)
long vnum;
{
  short int *sptr,*sptr2,*sptr3;
  long *lptr,*lptr2,*lptr3;
  float *fptr,*fptr2,*fptr3;
  double *dptr,*dptr2,*dptr3;
  char *cptr;
  long velem,celem;
  long status=0;
  long ierr=0;

  /* Keep track of data min and max values */
  switch (VLIST[vnum].cdfvartype) {
    case CDF_REAL4 : for (velem=0;velem < VLIST[vnum].nelems;velem++) {
                       fptr=(float *)VLIST[vnum].dataptr;
                       fptr=fptr+velem; celem=VLIST[vnum].clistnum;
                       fptr2=(float *)CLIST[celem].vmin;
                       fptr3=(float *)CLIST[celem].vmax;
                       if ((CLIST[celem].flagone == 0)||(*fptr < *fptr2)) {
                         *fptr2 = *fptr; CLIST[celem].flagone=1L;
                       }
                       if ((CLIST[celem].flagone == 0)||(*fptr > *fptr3)) {
                         *fptr3 = *fptr; CLIST[celem].flagone=1L;
                       }
                     }
                     break;
    case CDF_FLOAT : for (velem=0;velem < VLIST[vnum].nelems;velem++) {
                       fptr=(float *)VLIST[vnum].dataptr;
                       fptr=fptr+velem; celem=VLIST[vnum].clistnum;
                       fptr2=(float *)CLIST[celem].vmin;
                       fptr3=(float *)CLIST[celem].vmax;
                       if ((CLIST[celem].flagone == 0)||(*fptr < *fptr2)) {
                         *fptr2 = *fptr; CLIST[celem].flagone=1L;
                       }
                       if ((CLIST[celem].flagone == 0)||(*fptr > *fptr3)) {
                         *fptr3 = *fptr; CLIST[celem].flagone=1L;
                       }
                     }
                     break;
    case CDF_REAL8 : for (velem=0;velem < VLIST[vnum].nelems;velem++) {
                       dptr=(double *)VLIST[vnum].dataptr;
                       dptr=dptr+velem; celem=VLIST[vnum].clistnum;
                       dptr2=(double *)CLIST[celem].vmin;
                       dptr3=(double *)CLIST[celem].vmax;
                       if ((CLIST[celem].flagone == 0)||(*dptr < *dptr2)) {
                         *dptr2 = *dptr; CLIST[celem].flagone=1L;
                       }
                       if ((CLIST[celem].flagone == 0)||(*dptr > *dptr3)) {
                         *dptr3 = *dptr; CLIST[celem].flagone=1L;
                       }
                     }
                     break;
    case CDF_DOUBLE: for (velem=0;velem < VLIST[vnum].nelems;velem++) {
                       dptr=(double *)VLIST[vnum].dataptr;
                       dptr=dptr+velem; celem=VLIST[vnum].clistnum;
                       dptr2=(double *)CLIST[celem].vmin;
                       dptr3=(double *)CLIST[celem].vmax;
                       if ((CLIST[celem].flagone == 0)||(*dptr < *dptr2)) {
                         *dptr2 = *dptr; CLIST[celem].flagone=1L;
                       }
                       if ((CLIST[celem].flagone == 0)||(*dptr > *dptr3)) {
                         *dptr3 = *dptr; CLIST[celem].flagone=1L;
                       }
                     }
                     break;
    case CDF_EPOCH : for (velem=0;velem < VLIST[vnum].nelems;velem++) {
                       dptr=(double *)VLIST[vnum].dataptr;
                       dptr=dptr+velem; celem=VLIST[vnum].clistnum;
                       dptr2=(double *)CLIST[celem].vmin;
                       dptr3=(double *)CLIST[celem].vmax;
                       if ((CLIST[celem].flagone == 0)||(*dptr < *dptr2)) {
                         *dptr2 = *dptr; CLIST[celem].flagone=1L;
                       }
                       if ((CLIST[celem].flagone == 0)||(*dptr > *dptr3)) {
                         *dptr3 = *dptr; CLIST[celem].flagone=1L;
                       }
                     }
                     break;
    case CDF_INT4  : for (velem=0;velem < VLIST[vnum].nelems;velem++) {
                       lptr=(long *)VLIST[vnum].dataptr;
                       lptr=lptr+velem; celem=VLIST[vnum].clistnum;
                       lptr2=(long *)CLIST[celem].vmin;
                       lptr3=(long *)CLIST[celem].vmax;
                       if ((CLIST[celem].flagone == 0)||(*lptr < *lptr2)) {
                         *lptr2 = *lptr; CLIST[celem].flagone=1L;
                       }
                       if ((CLIST[celem].flagone == 0)||(*lptr > *lptr3)) {
                         *lptr3 = *lptr; CLIST[celem].flagone=1L;
                       }
                     }
                     break;
    case CDF_UINT4 : for (velem=0;velem < VLIST[vnum].nelems;velem++) {
                       lptr=(long *)VLIST[vnum].dataptr;
                       lptr=lptr+velem; celem=VLIST[vnum].clistnum;
                       lptr2=(long *)CLIST[celem].vmin;
                       lptr3=(long *)CLIST[celem].vmax;
                       if ((CLIST[celem].flagone == 0)||(*lptr < *lptr2)) {
                         *lptr2 = *lptr; CLIST[celem].flagone=1L;
                       }
                       if ((CLIST[celem].flagone == 0)||(*lptr > *lptr3)) {
                         *lptr3 = *lptr; CLIST[celem].flagone=1L;
                       }
                     }
                     break;
    case CDF_INT2  : for (velem=0;velem < VLIST[vnum].nelems;velem++) {
                       sptr=(short int *)VLIST[vnum].dataptr;
                       sptr=sptr+velem; celem=VLIST[vnum].clistnum;
                       sptr2=(short int *)CLIST[celem].vmin;
                       sptr3=(short int *)CLIST[celem].vmax;
                       if ((CLIST[celem].flagone == 0)||(*sptr < *sptr2)) {
                         *sptr2 = *sptr; CLIST[celem].flagone=1L;
                       }
                       if ((CLIST[celem].flagone == 0)||(*sptr > *sptr3)) {
                         *sptr3 = *sptr; CLIST[celem].flagone=1L;
                       }
                     }
                     break;
    case CDF_UINT2 : for (velem=0;velem < VLIST[vnum].nelems;velem++) {
                       sptr=(short int *)VLIST[vnum].dataptr;
                       sptr=sptr+velem; celem=VLIST[vnum].clistnum;
                       sptr2=(short int *)CLIST[celem].vmin;
                       sptr3=(short int *)CLIST[celem].vmax;
                       if ((CLIST[celem].flagone == 0)||(*sptr < *sptr2)) {
                         *sptr2 = *sptr; CLIST[celem].flagone=1L;
                       }
                       if ((CLIST[celem].flagone == 0)||(*sptr > *sptr3)) {
                         *sptr3 = *sptr; CLIST[celem].flagone=1L;
                       }
                     }
                     break;
    case CDF_CHAR  : MSG_control(1,"WARNING: char minmax checking TBD",status);
                     break;
    case CDF_UCHAR : MSG_control(1,"WARNING: char minmax checking TBD",status);
                     break;
    default        : MSG_control(1,"WARNING: unknown minmax vartype",status);
  }

  /* Keep track of the #characters in the longest valid string from INFILE */
  celem = VLIST[vnum].clistnum;
  if (VLIST[vnum].ccount > CLIST[celem].ccount) {
    CLIST[celem].ccount=VLIST[vnum].ccount;
  }
  VLIST[vnum].ccount = 0; /* reset */

  return(ierr);
}

/************************************************************************/
/*   Replace all fill values identified by user with ISTP standard fill */
/************************************************************************/
long AutoFiller(vnum)
long vnum;
{
  short int *sptr;
  long *lptr;
  float *fptr;
  double *dptr;
  char *cptr;
  long velem;
  long status=0;
  long iflag=0;

  switch (VLIST[vnum].cdfvartype) {
    case CDF_REAL4 : for (velem=0;velem < VLIST[vnum].nelems;velem++) {
                       fptr=(float *)VLIST[vnum].dataptr;
                       fptr=fptr+velem;
                       if (*fptr==(float)VLIST[vnum].fillval) {
                         *fptr = -1.0e31; iflag=1L;
                       }
                     }
                     break;
    case CDF_FLOAT : for (velem=0;velem < VLIST[vnum].nelems;velem++) {
                       fptr=(float *)VLIST[vnum].dataptr;
                       fptr=fptr+velem;
                       if (*fptr==(float)VLIST[vnum].fillval) {
                         *fptr = -1.0e31; iflag=1L;
                       }
                     }
                     break;
    case CDF_REAL8 : for (velem=0;velem < VLIST[vnum].nelems;velem++) {
                       dptr=(double *)VLIST[vnum].dataptr;
                       dptr=dptr+velem;
                       if (*dptr==VLIST[vnum].fillval) {
                         *dptr = -1.0e31; iflag=1L;
                       }
                     }
                     break;
    case CDF_DOUBLE: for (velem=0;velem < VLIST[vnum].nelems;velem++) {
                       dptr=(double *)VLIST[vnum].dataptr;
                       dptr=dptr+velem;
                       if (*dptr==VLIST[vnum].fillval) {
                         *dptr = -1.0e31; iflag=1L;
                       }
                     }
                     break;
    case CDF_EPOCH : for (velem=0;velem < VLIST[vnum].nelems;velem++) {
                       dptr=(double *)VLIST[vnum].dataptr;
                       dptr=dptr+velem;
                       if (*dptr==VLIST[vnum].fillval) {
                         *dptr = -1.0e31; iflag=1L;
                       }
                     }
                     break;
    case CDF_INT4  : for (velem=0;velem < VLIST[vnum].nelems;velem++) {
                       lptr=(long *)VLIST[vnum].dataptr;
                       lptr=lptr+velem;
                       if (*lptr==(long)VLIST[vnum].fillval) {
                         *lptr = -2147483648; iflag=1L;
                       }
                     }
                     break;
    case CDF_UINT4 : for (velem=0;velem < VLIST[vnum].nelems;velem++) {
                       lptr=(long *)VLIST[vnum].dataptr;
                       lptr=lptr+velem;
                       if (*lptr==(long)VLIST[vnum].fillval) {
                         *lptr = -2147483648; iflag=1L;
                       }
                     }
                     break;
    case CDF_INT2  : for (velem=0;velem < VLIST[vnum].nelems;velem++) {
                       sptr=(short int *)VLIST[vnum].dataptr;
                       sptr=sptr+velem;
                       if (*sptr==(short int)VLIST[vnum].fillval) {
                         *sptr = -32768; iflag=1L;
                       }
                     }
                     break;
    case CDF_UINT2 : for (velem=0;velem < VLIST[vnum].nelems;velem++) {
                       sptr=(short int *)VLIST[vnum].dataptr;
                       sptr=sptr+velem;
                       if (*sptr==(short int)VLIST[vnum].fillval) {
                         *sptr = -32768; iflag=1L;
                       }
                     }
                     break;
    case CDF_CHAR  : MSG_control(1,"WARNING: char fillval checking TBD",status);
                     break;
    case CDF_UCHAR : MSG_control(1,"WARNING: char fillval checking TBD",status);
                     break;
    default        : MSG_control(1,"WARNING: unknown fillval vartype",status);
  }

  /* If any data is filler, then reset the character counter, so that */
  /* the max field size is not set by any fill data, only non-fill.   */
  if (iflag == 1L) VLIST[vnum].ccount = 0L;
  return(iflag);
}

/************************************************************************/
/*            Determine the FORMAT of the identified variable           */
/************************************************************************/
long AutoFormat(vnum,form)
long vnum;
char *form;
{
  double dnum1,dnum2;
  double *dptr1,*dptr2;
  float fnum1,fnum2;
  float *fptr1,*fptr2;
  long cnum,d,s;
  long ierr=0;
  char slen[10]="";
  char flen[10]="";

  cnum = VLIST[vnum].clistnum;
  switch (VLIST[vnum].cdfvartype) {
    case CDF_REAL4 : fptr1 = (float *)CLIST[cnum].vmax; fnum1 = *fptr1;
                     fptr2 = (float *)CLIST[cnum].vmin; fnum2 = *fptr2;
                     dnum1 = fabs((double)fnum1); dnum2 = fabs((double)fnum2);
                     if (dnum2 > dnum1) {
                       if (fnum2 < 0.0) s=1L; else s=0L; dnum1 = dnum2;
                     } else if (fnum1 < 0.0) s=1L; else s=0L;
                     for (d=0;dnum1 >= 1.0;d++) dnum1 = dnum1 / 10.0;
                     sprintf(slen,"%ld",CLIST[cnum].ccount);
                     sprintf(flen,"%ld",CLIST[cnum].ccount - 1 - d - s);
                     strcpy(form,"F"); strcat(form,slen); 
                     strcat(form,"."); strcat(form,flen);
                     break;
    case CDF_FLOAT : fptr1 = (float *)CLIST[cnum].vmax; fnum1 = *fptr1;
                     fptr2 = (float *)CLIST[cnum].vmin; fnum2 = *fptr2;
                     dnum1 = fabs((double)fnum1); dnum2 = fabs((double)fnum2);
                     if (dnum2 > dnum1) {
                       if (fnum2 < 0.0) s=1L; else s=0L; dnum1 = dnum2;
                     } else if (fnum1 < 0.0) s=1L; else s=0L;
                     for (d=0;dnum1 >= 1.0;d++) dnum1 = dnum1 / 10.0;
                     sprintf(slen,"%ld",CLIST[cnum].ccount);
                     sprintf(flen,"%ld",CLIST[cnum].ccount - 1 - d);
                     strcpy(form,"F"); strcat(form,slen); 
                     strcat(form,"."); strcat(form,flen);
                     break;
    case CDF_REAL8 : dptr1 = (double *)CLIST[cnum].vmax; dnum1 = *dptr1;
                     dptr2 = (double *)CLIST[cnum].vmin; dnum2 = *dptr2;
                     dnum1 = fabs(dnum1); dnum2 = fabs(dnum2);
                     if (dnum2 > dnum1) {
                       if (fnum2 < 0.0) s=1L; else s=0L; dnum1 = dnum2;
                     } else if (fnum1 < 0.0) s=1L; else s=0L;
                     for (d=0;dnum1 >= 1.0;d++) dnum1 = dnum1 / 10.0;
                     sprintf(slen,"%ld",CLIST[cnum].ccount);
                     sprintf(flen,"%ld",CLIST[cnum].ccount - 1 - d);
                     strcpy(form,"F"); strcat(form,slen); 
                     strcat(form,"."); strcat(form,flen);
                     break;
    case CDF_DOUBLE: dptr1 = (double *)CLIST[cnum].vmax; dnum1 = *dptr1;
                     dptr2 = (double *)CLIST[cnum].vmin; dnum2 = *dptr2;
                     dnum1 = fabs(dnum1); dnum2 = fabs(dnum2);
                     if (dnum2 > dnum1) {
                       if (fnum2 < 0.0) s=1L; else s=0L; dnum1 = dnum2;
                     } else if (fnum1 < 0.0) s=1L; else s=0L;
                     for (d=0;dnum1 >= 1.0;d++) dnum1 = dnum1 / 10.0;
                     sprintf(slen,"%ld",CLIST[cnum].ccount);
                     sprintf(flen,"%ld",CLIST[cnum].ccount - 1 - d);
                     strcpy(form,"F"); strcat(form,slen); 
                     strcat(form,"."); strcat(form,flen);
                     break;
    case CDF_EPOCH : dptr1 = (double *)CLIST[cnum].vmax; dnum1 = *dptr1;
                     dptr2 = (double *)CLIST[cnum].vmin; dnum2 = *dptr2;
                     dnum1 = fabs(dnum1); dnum2 = fabs(dnum2);
                     if (dnum2 > dnum1) {
                       if (fnum2 < 0.0) s=1L; else s=0L; dnum1 = dnum2;
                     } else if (fnum1 < 0.0) s=1L; else s=0L;
                     for (d=0;dnum1 >= 1.0;d++) dnum1 = dnum1 / 10.0;
                     sprintf(slen,"%ld",CLIST[cnum].ccount);
                     sprintf(flen,"%ld",CLIST[cnum].ccount - 1 - d);
                     strcpy(form,"F"); strcat(form,slen); 
                     strcat(form,"."); strcat(form,flen);
                     break;
    case CDF_INT4 :  sprintf(slen,"%ld",CLIST[cnum].ccount);
                     strcpy(form,"I"); strcat(form,slen); 
                     break;
    case CDF_UINT4 : sprintf(slen,"%ld",CLIST[cnum].ccount);
                     strcpy(form,"I"); strcat(form,slen); 
                     break;
    case CDF_INT2 :  sprintf(slen,"%ld",CLIST[cnum].ccount);
                     strcpy(form,"I"); strcat(form,slen); 
                     break;
    case CDF_UINT2 : sprintf(slen,"%ld",CLIST[cnum].ccount);
                     strcpy(form,"I"); strcat(form,slen); 
                     break;
    case CDF_BYTE  : sprintf(slen,"%ld",CLIST[cnum].ccount);
                     strcpy(form,"I"); strcat(form,slen); 
                     break;
    case CDF_CHAR  : printf("WARNING: No auto formatting of CHAR data\n");
                     break;
    case CDF_UCHAR : printf("WARNING: No auto formatting of CHAR data\n");
                     break;
    default        : printf("WARNING: Unknown data type in autoformatter\n");
  }
  return(ierr);
}

/**********************************************************************/
/*      Close all open files and free all allocated buffer space      */
/**********************************************************************/
long Cleanup_makeCDF(ifile)
long ifile;
{
  CDFid id;
  CDFstatus status;
  char scnt[20]="";
  long i;
  long ierr=0;

  /* free data space used for the ordered variable list */
  free(OLIST); OLISTne=0L; OLIST=NULL;

  /* close output cdf file */
  if (OUTCDF.outCDFid != 0) {
    sprintf(scnt,"%ld",(OUTCDF.CDFrec+1));
    strcpy(LOGFILE.message,"Number of records written to CDF=");
    strcat(LOGFILE.message,scnt); strcat(LOGFILE.message,"");
    MSG_control(1,LOGFILE.message,0);
    MSG_control(1,"Closing output CDF...",0);
    status=CDFlib(CLOSE_,CDF_,OUTCDF.outCDFid,NULL_);
    OUTCDF.outCDFid=0L;
  }

  /* reset first epoch flag and Base time flag*/
  EpALG.FirstEp = 0.0; EpALG.BaseEp = 0.0;

  if (ifile == FLISTne-1) { /* total termination */

    /* close input file and free allocated buffer space */
    free(INFILE.buffer);
    if (INFILE.fptr != NULL) fclose(INFILE.fptr);

    /* free data space allocated for variables */
    for (i=0;i<VLISTne;i++) {
      if (VLIST[i].buffer  != NULL) free(VLIST[i].buffer);
      if (VLIST[i].dataptr != NULL) free(VLIST[i].dataptr);
      free(VLIST[i].vname); free(VLIST[i].cdfvname);
    }

    /* free data space allocated for min/max tracking */
    for (i=0;i<CLISTne;i++) {
      if (CLIST[i].vmin !=NULL) free(CLIST[i].vmin);
      if (CLIST[i].vmax !=NULL) free(CLIST[i].vmax);
    }

    free(VLIST); VLISTne=0L;
    free(CLIST); CLISTne=0L;
    free(SRECS); SRECSne=0L;
  }
  return(ierr);
}

/**********************************************************************/
/*                        Open the output CDF                         */
/**********************************************************************/
long Open_OutCDF()
{
  CDFstatus status;
  CDFid     id;
  long ierr=0;

  strcpy(LOGFILE.message,"Opening the output CDF named: ");
  strcat(LOGFILE.message,RPARMS.outcdfname);
  MSG_control(1,LOGFILE.message,0);
  status = CDFlib(OPEN_,CDF_,RPARMS.outcdfname,&id,
                  SELECT_, CDF_zMODE_, zMODEon2, NULL_);
  if (status >= CDF_WARN) {
    OUTCDF.outCDFid = id;
    OUTCDF.CDFrec   = -1L; }
  else {
    MSG_control(2,"CDFERROR:",status);
    ierr = 1;
  }

  status = CDFlib(GET_,CDF_ENCODING_,&OUTCDF.encoding,NULL_);
  if (status != CDF_OK) {
    MSG_control(2,"ERROR: Unable to determine CDF encoding",status);
    ierr=1;
  }
  else { /* confirm NETWORK encoding format as ISTP guideline */
    if (OUTCDF.encoding != NETWORK_ENCODING) {
      MSG_control(1,"WARNING:Output CDF encoding is not NETWORK      ",0);
      MSG_control(1,"        ISTP guidelines require NETWORK encoding",0);
      MSG_control(1,"        Processing continuing ...",0);
  } }

  return(ierr);
}

/**********************************************************************/
/*                Write Epoch value to output CDF                     */
/**********************************************************************/
long WriteEpochToCDF (depoch, CDFrec)
double depoch;
long CDFrec;
{
  CDFstatus status;
  long ierr=0;
  status = CDFlib(SELECT_,zVAR_,EpALG.Epvarnum,
                  SELECT_,zVAR_RECNUMBER_,CDFrec,
                  PUT_,zVAR_DATA_,&depoch,NULL_);
  if (status != CDF_OK) {
    MSG_control(2,"ERROR: while writing epoch value to cdf.",status);
    ierr=1;
  }
  return(ierr);
}

/**********************************************************************/
/*                     Write data to output CDF                       */
/**********************************************************************/
long WriteToCDF (vnum, CDFrec)
long vnum;
long CDFrec;
{
  CDFstatus status;
  char vstring[20];
  long ierr=0;
  if (VLIST[vnum].cdfvartype != CDF_CHAR) {
    if (VLIST[vnum].Elem[0] == -1L) {
      status = CDFlib(SELECT_,zVARs_RECNUMBER_,CDFrec,
                      PUT_,zVARs_RECDATA_,1,&VLIST[vnum].cdfvarnum,
                                            VLIST[vnum].dataptr,NULL_);
    }
    else {
      if (VLIST[vnum].Elem[1] == -1L) { /* only one index */
        status = CDFlib(SELECT_,zVARs_RECNUMBER_,CDFrec,
                                zVAR_,VLIST[vnum].cdfvarnum,
                                zVAR_DIMINDICES_,&VLIST[vnum].Elem[0],
                        PUT_,   zVAR_DATA_,VLIST[vnum].dataptr,NULL_); }
      else { /* both element indices being used */
        status = CDFlib(SELECT_,zVARs_RECNUMBER_,CDFrec,
                                zVAR_,VLIST[vnum].cdfvarnum,
                                zVAR_DIMINDICES_,&VLIST[vnum].Elem,
                        PUT_,   zVAR_DATA_,VLIST[vnum].dataptr,NULL_);
      }
    }
  }
  else { /* character data */
    if (VLIST[vnum].Elem[0] == -1L) {
      status = CDFlib(SELECT_,zVARs_RECNUMBER_,CDFrec,
                      PUT_,zVARs_RECDATA_,1,&VLIST[vnum].cdfvarnum,
                                             VLIST[vnum].buffer,NULL_);
    }
    else {
      status = CDFlib(SELECT_,zVARs_RECNUMBER_,CDFrec,
                              zVAR_,VLIST[vnum].cdfvarnum,
                              zVAR_DIMINDICES_,&VLIST[vnum].Elem,
                      PUT_,   zVAR_DATA_,VLIST[vnum].buffer,NULL_);
    }
  }
  if (status != CDF_OK) {
    strcpy(LOGFILE.message,"ERROR: while writing variable #");
    sprintf(vstring,"%d",vnum); strcat(vstring," to the cdf");
    strcat(LOGFILE.message,vstring);
    MSG_control(2,LOGFILE.message,status);
    ierr=1;
  }
  return(ierr);
}

/***********************************************************************
*                     GENERATE THE LFILEID VALUE                       *
***********************************************************************/
long Generate_LFILEID(lfileid)
char *lfileid;
{
  long i,scope,ne,lyear,lmon,lday,lhour,lmin,lsec,lmsec;
  CDFstatus status;
  time_t current_time; struct tm *ptrtime;
  double depoch;
  char source[80]=""; char sabbr[10]=""; char type[80]="";
  char desc[80]=""; char version[10]=""; char datestr[10]="";
  long ierr=0;

  /* Get values of Source_name, Data_type Descriptor and Version gattrs */
  status=CDFlib(SELECT_,ATTR_NAME_,"Source_name", SELECT_,gENTRY_,0,
                GET_,ATTR_SCOPE_,&scope, GET_,gENTRY_NUMELEMS_,&ne, 
                GET_,gENTRY_DATA_,source,NULL_);
  if ((status != CDF_OK)||(scope != GLOBAL_SCOPE)) {
    MSG_control(1,"WARNING: gATTR 'Source_name' not found.",status); ierr=1;}
  else {
    source[ne] = '\0'; /* insert string terminator */
    ierr=AbbrSCname(source,sabbr); /* abbreviate source name */
    status=CDFlib(SELECT_,ATTR_NAME_,"Data_type", SELECT_,gENTRY_,0,
                  GET_,ATTR_SCOPE_,&scope, GET_,gENTRY_NUMELEMS_,&ne,
                  GET_,gENTRY_DATA_,type,NULL_);
    if ((status != CDF_OK)||(scope != GLOBAL_SCOPE)) {
      MSG_control(1,"WARNING: gATTR 'Data_type' not found.",status); ierr=1;}
    else {
      type[ne] = '\0'; /* insert string terminator */
      status=CDFlib(SELECT_,ATTR_NAME_,"Descriptor", SELECT_,gENTRY_,0,
                    GET_,ATTR_SCOPE_,&scope, GET_,gENTRY_NUMELEMS_,&ne,
                    GET_,gENTRY_DATA_,desc,NULL_);
      if ((status != CDF_OK)||(scope != GLOBAL_SCOPE)) {
        MSG_control(1,"WARNING: gATTR 'Descriptor' not found.",status); ierr=1;}
      else {
        desc[ne] = '\0'; /* insert string terminator */
        status=CDFlib(SELECT_,ATTR_NAME_,"Data_version", SELECT_,gENTRY_,0,
                      GET_,ATTR_SCOPE_,&scope, GET_,gENTRY_NUMELEMS_,&ne,
                      GET_,gENTRY_DATA_,version,NULL_);
        if ((status != CDF_OK)||(scope != GLOBAL_SCOPE)) {
          MSG_control(1,"WARNING: gATTR 'Version' not found.",status); ierr=1;}
        else {
          version[ne] = '\0'; /* insert string terminator */
          if (EpALG.FirstEp != 0) {
            depoch = EpALG.FirstEp;
            EPOCHbreakdown(depoch,&lyear,&lmon,&lday,&lhour,&lmin,&lsec,&lmsec);
            sprintf(datestr,"%4ld%02ld%02ld",lyear,lmon,lday);
            strcat(datestr,"");}
          else { /* get time from system */
            time(&current_time);
            ptrtime = localtime(&current_time);
            strftime(datestr,9,"%Y%m%d",ptrtime);
          }
          /* All information required to make lfileid has been gathered */
          for (i=0;i<2;i++) *lfileid++ = sabbr[i]; *lfileid++ = '_';
          for (i=0;i<2;i++) *lfileid++ = type[i];  *lfileid++ = '_';
          for (i=0;i<3;i++) *lfileid++ = desc[i];  *lfileid++ = '_';
          for (i=0;i<strlen(datestr);i++) *lfileid++ = datestr[i];
          *lfileid++ = '_'; *lfileid++ = 'V';
          if (strlen(version) == 1) *lfileid++ = '0';
          for (i=0;i<strlen(version);i++) *lfileid++ = version[i];
          *lfileid = '\0';
  } } } }
  if (ierr != 0) {
    MSG_control(1,"       UNABLE TO GENERATE LOGICAL FILE ID.",0);
    strcpy(lfileid,"");
  }
  return(ierr);
}

/***********************************************************************
*                        SET THE LFILEID VATTR                         *
***********************************************************************/
long Set_LFILEID(lfileid)
char *lfileid;
{
  CDFstatus status;
  long ierr=0;

  /* Put the logical file id value into the CDF */
  status=CDFlib(SELECT_,ATTR_NAME_,"Logical_file_id", SELECT_,gENTRY_,0,
                PUT_,gENTRY_DATA_,CDF_CHAR,strlen(lfileid),lfileid,NULL_);
  if (status != CDF_OK) {
    MSG_control(1,"WARNING: Unable to write logical file id to CDF.",status);
    ierr=1;
  }
  return(ierr);
}

/***********************************************************************
*                     SET THE MIN AND MAX VATTRS                       *
***********************************************************************/
long Set_AutoVattrs()
{
  char form[10];
  CDFstatus status;
  long vnum,cnum;
  long ierr=0;

  for (vnum=0;vnum<VLISTne;vnum++) { /* check each variable */
    cnum = VLIST[vnum].clistnum;
    /* Determine if validmin and validmax vattrs should be set */
    if ((VLIST[vnum].autovalids > 0)&&
        (VLIST[vnum].cdfvarnum >= 0)&&
        (CLIST[cnum].flagone > 0   )) {
      /* Put the validmin and validmax values into the vattr */
      status=CDFlib(SELECT_,ATTR_NAME_,"VALIDMIN", 
                            zENTRY_,VLIST[vnum].cdfvarnum,
                    PUT_,   zENTRY_DATA_,VLIST[vnum].cdfvartype,1,
                            CLIST[cnum].vmin,NULL_);
      if (status != CDF_OK) {
        MSG_control(1,"WARNING: Unable to write VALIDMIN to CDF.",status);
        ierr=1;
      }
      status=CDFlib(SELECT_,ATTR_NAME_,"VALIDMAX",
                            zENTRY_,VLIST[vnum].cdfvarnum,
                    PUT_,   zENTRY_DATA_,VLIST[vnum].cdfvartype,1,
                            CLIST[cnum].vmax,NULL_);
      if (status != CDF_OK) {
        MSG_control(1,"WARNING: Unable to write VALIDMAX to CDF.",status);
        ierr=1;
      }
    }
    /* Determine if scalemin and scalemax vattrs should be set */
    if ((VLIST[vnum].autoscales > 0)&&
        (VLIST[vnum].cdfvarnum >= 0)&&
        (CLIST[cnum].flagone > 0   )) {
      /* Put the scalemin and scalemax values into the vattr */
      status=CDFlib(SELECT_,ATTR_NAME_,"SCALEMIN",
                            zENTRY_,VLIST[vnum].cdfvarnum,
                    PUT_,   zENTRY_DATA_,VLIST[vnum].cdfvartype,1,
                            CLIST[cnum].vmin,NULL_);
      if (status != CDF_OK) {
        MSG_control(1,"WARNING: Unable to write SCALEMIN to CDF.",status);
        ierr=1;
      }
      status=CDFlib(SELECT_,ATTR_NAME_,"SCALEMAX",
                            zENTRY_,VLIST[vnum].cdfvarnum,
                    PUT_,   zENTRY_DATA_,VLIST[vnum].cdfvartype,1,
                            CLIST[cnum].vmax,NULL_);
      if (status != CDF_OK) {
        MSG_control(1,"WARNING: Unable to write SCALEMAX to CDF.",status);
        ierr=1;
      }
    }
    /* Determine if FORMAT vattrs should be set */
    if ((VLIST[vnum].autoformat > 0)&&
        (VLIST[vnum].cdfvarnum >= 0)&&
        (CLIST[cnum].ccount > 0   )) {
      strcpy(form,""); /* initialize the format */
      ierr = AutoFormat(vnum,form); /* determine format */
      if (ierr == 0) {
        status=CDFlib(SELECT_,ATTR_NAME_,"FORMAT",
                              zENTRY_,VLIST[vnum].cdfvarnum,
                      PUT_,   zENTRY_DATA_,CDF_CHAR,strlen(form),form,NULL_);
        if (status != CDF_OK) {
          MSG_control(1,"WARNING: Unable to write SCALEMIN to CDF.",status);
          ierr=1;
    } } }

  } /* for each variable */
  return(ierr);
}

/***********************************************************************
*                    ABBREVIATE THE SPACECRAFT NAME                    *
***********************************************************************/
long AbbrSCname (source, abbr)
char *source, *abbr;
{
  long ierr=0;
  memcpy(abbr,source,2);
  strcat(abbr,"");
  if (strcmp(source,"SAMPEX>Solar Anomalous Magnetic Particle Explorer")==0)
    strcpy(abbr,"SX>");
  if (strcmp(source,"GEOTAIL>Geomagnetic Tail")==0)
    strcpy(abbr,"GE>");
  if (strcmp(source,"WIND>Wind Interplanetary Plasma Laboratory")==0)
    strcpy(abbr,"WI>");
  if (strcmp(source,"POLAR>Polar Plasma Laboratory")==0)
    strcpy(abbr,"PO>");
  if (strcmp(source,"SOHO>Solar Heliospheric Observatory")==0)
    strcpy(abbr,"SO>");
  if (strcmp(source,"DARN>Dual Auroral Radar Network")==0)
    strcpy(abbr,"DN>");
  if (strcmp(source,
     "SESAME>Satellite Exploration Simultaneous with Antarctic Measurements")
     ==0) strcpy(abbr,"SE>");
  if (strcmp(source,"SONDRESTROM>Sondrestrom Incoherent-Scatter Radar")==0)
    strcpy(abbr,"SN>");
  if (strcmp(source,
     "CANOPUS>Canadian Auroral Network Open Program Unified Study")==0)
    strcpy(abbr,"CN>");
  if (strcmp(source,"IMP8>Interplanetary Monitoring Platform")==0)
    strcpy(abbr,"I8>");
  if (strcmp(source,"LANL1989>")==0) strcpy(abbr,"L9>");
  if (strcmp(source,"LANL1990>")==0) strcpy(abbr,"L0>");
  if (strcmp(source,"LANL1991>")==0) strcpy(abbr,"L1>");
  if (strcmp(source,"GOES6>Geostationary Operational Environment Satellite")==0)
     strcpy(abbr,"G6>");
  if (strcmp(source,"GOES7>Geostationary Operational Environment Satellite")==0)
     strcpy(abbr,"G7>");
  if (strcmp(source,"GOES8>Geostationary Operational Environment Satellite")==0)
     strcpy(abbr,"G8>");
  if (strcmp(source,"GOES9>Geostationary Operational Environment Satellite")==0)
     strcpy(abbr,"G9>");
  if (strcmp(source,
     "STELAB>Solar-Terrestrial Environment Laboratory, Nagoya U.")==0)
     strcpy(abbr,"SL>");
  return(ierr);
}

/**************************************************************************
*                RENAME THE CDF USING ISTP CONVENTIONS                    *
**************************************************************************/
long AutoNamer (oldname, newname)
char *oldname;
char *newname;
{
  int  i,j;
  long ierr=0;
  long olength;
  char c;
  char *ptr=oldname;
  char direc[80+1]="";
  char filen[80+1]="";
  char otemp[80+1]="";
  char ntemp[80+1]="";

  /* make the newname uppercase or lowercase depending on runtime param */
  strcpy(ntemp,newname); olength=(long)strlen(newname);
  if (RPARMS.autocase == 1) { /* force to lowercase */
    for (i=0;i<olength;i++) ntemp[i]=tolower(ntemp[i]); ntemp[i]='\0';
  } else if (RPARMS.autocase == 0) { /* force to uppercase */
    for (i=0;i<olength;i++) ntemp[i]=toupper(ntemp[i]); ntemp[i]='\0';
  }
  strcpy(newname,ntemp);

  strcpy(otemp,oldname);  olength=(long)strlen(otemp);
  /* separate the filename from directory information. Find last ]or\.   */
  for (i=0,j= -1;i<olength;i++) { c= *ptr++; if((c==']')||(c=='\'')) j=i; }
  if (j!= -1) { /* directory mark found, separate directory and filename */
    for (i=0;i<=j;i++) direc[i]=otemp[i]; direc[i]='\0'; j++;
    for (i=0;j<olength;i++,j++) filen[i]=toupper(otemp[j]); filen[i]='\0';
    if (strcmp(filen,"AUTO")==0) { /* only rename if oldname=AUTO */
      strcat(direc,newname); strcat(direc,".cdf"); strcat(oldname,".cdf");
      /* output progress message */
      strcpy(LOGFILE.message,"Renaming the file from ");
      strcat(LOGFILE.message,oldname); strcat(LOGFILE.message," to ");
      strcat(LOGFILE.message,newname); strcat(LOGFILE.message,"\n");
      MSG_control(1,LOGFILE.message,0);
      if (rename(oldname,direc)!=0) {
        MSG_control(2,"ERROR: During attempt to rename the output CDF",0);
        ierr=1;
  } } }
  else { /* no directory mark found. convert to uppercase and compare */
    for (i=0;i<olength;i++) otemp[i]=toupper(otemp[i]); strcat(otemp,"");
    if (strcmp(otemp,"AUTO")==0) { /* only rename if oldname=AUTO */
      strcat(newname,".cdf"); strcat(oldname,".cdf");
      /* output progress message */
      strcpy(LOGFILE.message,"Renaming the file from ");
      strcat(LOGFILE.message,oldname); strcat(LOGFILE.message," to ");
      strcat(LOGFILE.message,newname); strcat(LOGFILE.message,"\n");
      MSG_control(1,LOGFILE.message,0);
      if (rename(oldname,newname)!=0) {
        MSG_control(2,"ERROR: During attempt to rename the output CDF",0);
        ierr=1;
  } } }
  return (ierr);
}

/**************************************************************************
*                RENAME THE CDF USING ISTP CONVENTIONS                    *
**************************************************************************/
long ShortName (Lfileid)
char *Lfileid;
{
  int i,j;
  char *ptr = Lfileid;
  char tempID[80]="";
  long ierr=0;
  for (i=0;i<12;i++) ptr++; /* skip spacecraft,datatype,descriptor,century */
  for (i=0;i<6;i++) tempID[i] = *ptr++; /* copy year,month,and day */
  for (j=0;j<2;j++) ptr++; /* skip underscore and 'v' */
  for (j=0;j<2;j++,i++) tempID[i] = *ptr++; /* copy verion number */
  tempID[i]='\0'; /* add terminator */
  strcpy(Lfileid,tempID);
  return(ierr);
}

/**************************************************************************
*                CHECK FOR WILDCARD USAGE IN INPUT FILE                   *
**************************************************************************/
long Build_FLIST()
{
  int  icnt;
  int  pcount=1;
  long i,j;
  char *cptr;
  char **inDirs;
  char **inFiles;
  char *inPattern[1];
  char inPath[80]="";
  char inFile[80]="";
  char newname[80]="";
  long ierr=0;

  ParsePath(RPARMS.infname,inPath,inFile);  /* separate file and path names */
  cptr = memchr(inFile,'*',strlen(inFile)); /* search for wildcard */

  if (cptr == NULL) { /* no wildcard character found */
    FLISTne++;
    FLIST=realloc(FLIST,(sizeof(FLISTz)*FLISTne));
    strcpy(FLIST[FLISTne-1].Pathname,inPath);
    strcpy(FLIST[FLISTne-1].Filename,inFile);
    return(ierr);
  }

  /* wildcard character must have been found - get list of filenames */
  inPattern[0] = inFile;
  icnt = DirList(inPath,pcount,&inPattern,&inDirs,&inFiles);

  /* Validate DirList results */
  if (icnt <= 0) {
    MSG_control(2,"ERROR: No files matched wildcard pattern...",0);
    MSG_control(2,"ERROR: Translation Aborted.",0);
    ierr=1; return(ierr); }
  else if (icnt > 1) { /* verify auto naming option is on */
    strcpy(newname,RPARMS.outcdfname);
    for (i=0,j=strlen(newname);i<j;i++) newname[i]=toupper(newname[i]);
    newname[i]='\0';
    if (strcmp(newname,"AUTO")!=0) {
      MSG_control(2,"ERROR: Wildcard pattern requires auto-naming option.",0);
      MSG_control(2,"ERROR: Translation Aborted.",0);
      ierr=1; return(ierr);
  } }

  /* Copy DirList results into FLIST data structure */
  for (i=0;i<icnt;i++) {
    FLISTne++;
    FLIST=realloc(FLIST,(sizeof(FLISTz)*FLISTne));
    strcpy(FLIST[FLISTne-1].Pathname,inDirs[i]);
    strcpy(FLIST[FLISTne-1].Filename,inFiles[i]);
  }
  return(ierr);
}

