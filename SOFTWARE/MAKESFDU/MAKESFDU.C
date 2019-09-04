/* Sabrina Sowers */
/* Started:  June 17, 1993 */
/* Generate a minimal, detached SFDU file for any ISTP formatted CDF file */
/* Modifications:                                                         */
/* K. Horrocks  5/25/95  ; Enhance to be able to read both R and Z vars   */
/* R. Burley    5/31/95  ; Get Generation date from sys clock not data    */
/*                       ; REFERENCE portion bounded by () not ''         */
/* R. Burley    6/28/95  ; Each line should be followed by both a line-   */
/*                       ; feed AND a carriage return.  The byte lengths  */
/*                       ; of the R-label and CIO-label should not include*/
/*                       ; their own lengths but SHOULD include the LF+CR */
/*                       ; following it.  The CIO stop time has been fixed*/
/*                       ; from 'END' time to 'STOP' time                 */
/* R. Burley    8/8/95   ; Support Cluster Project by making the follow-  */
/*                       ; ing enhancements: 1) Do not utilize the var-   */
/*                       ; iable 'Epoch' for determining start/stop times,*/
/*                       ; because Cluster's CSDS format does not require */
/*                       ; it as ISTP's format does.  Instead, find the   */
/*                       ; time variable by getting value of a DEPEND_0   */
/*                       ; vattr which both formats share.  2) Search for */
/*                       ; a gattr called 'Generation_date' to use to fill*/
/*                       ; in that portion of the SFDU, if this gattr does*/
/*                       ; not exist them use the computer clock time,    */
/*                       ; 3) Use the value of the ADID_ref gattr for the */
/*                       ; SFDU reference label, and if a value does not  */
/*                       ; exist then resort to the adi.txt file.         */
/* R. Burley   8/22/95   ; Bug in Determine_TimeVar.  Residual characters */
/*                       ; could follow time variable name.  Before GET_  */
/*                       ; from the CDF, overwrite aname with blanks.     */
/*                       ; Increase size of datestr array in get_gen_date3*/
/*                       ; to 32 characters.  Close adi file within loop  */
/* R. Burley   4/15/96   ; Add .cdf to REFERENCE object                   */
/* L. Bleau    6/25/96   ; in get_ref_label: add code to null-terminate	  */
/*			 ; ADIDbuffer; change ConAuth and ACAI from *char */
/*			 ; to char [] so initial value won't be in write- */
/*			 ; protected memory, change later assignment from */
/*			 ; pointer assignment to strcpy call; add extern  */
/*			 ; declaration for function CDFdirList		  */
/* L. Bleau    7/8/96    ; in get_ref_label: fix bug in directory name	  */
/*                       ; syntax conversion code			  */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <cdf.h>

extern int CDFdirList();

/****************************************************************************
************************ ERROR STATUS HANDLER *******************************
****************************************************************************/

/* This function handles the CDF status codes */

void error (CDFstatus status)
{
  char message [CDF_STATUSTEXT_LEN+1];

  if (status < CDF_WARN) {
    CDFerror(status, message);	         /* A halting error has occurred */
    printf("ATTENTION> %s\n", message);
    exit(status);
  }
  else
    if (status < CDF_OK) {
      CDFerror(status, message);        /* Function may not have completed
					         correctly */
      printf("ATTENTION> %s\n", message);
      exit(1);
    }
    else
      if (status > CDF_OK) {
        CDFerror(status, message);      /* Function completed, but something
					       incorrect has occured */
        printf("ATTENTION> %s\n", message);
        exit(1);
      }
return;
}


/****************************************************************************
************************ GENERATION DATE FUNCTION ***************************
****************************************************************************/

/* This function extracts the generation date from the global scope
       attribute "Logical_file_id". NOTE: The date extracted from the id
	    is an approximation and may not be the exact generation date */

int get_gen_date (char *string, FILE *fp, int p_to_file, char *okay)
{
  char str[11];			/* String to hold the formatted date */
  char hold_date[9];		/* String to hold the unformatted date */
  char *no_date="0000-00-00";	/* Used if an ivalid Logical_file_id */
  char *k;		/* Pointer to the string with the generation date */
  char *flag="F";	/* Flag */
  int i=0, j=0, size=0;	/* Loop control variables/indices and gen_date size */

  k = string;
   /* Increment address in k until at date or null encountered */
  for (i=0; string[i] != '\0'; i++) { 
    if (string[i] == '_') j++;
    k++;
    if (j==3) break;
  }

    /* If the generation date does not exist, print default date to file
	    or the size of the date -- Sabrina Sowers */
  if (string[i] == '\0') {
    *okay = *flag;	    /* Set okay flag to false */
    if (p_to_file) fprintf (fp, "Generation_Date = %s;\r\n", no_date);
    else { size = strlen(no_date) + 21; return(size); }
  }
  else {
    for (i=0; k[i] != '_'; i++) hold_date[i] = k[i];    /* Get the date */
    hold_date[i] = '\0';	    /* Insert null character */
    /* Put date in generation date format: yyyy-mm-dd */
    for (i=0; i<4; i++) str[i] = hold_date[i];
    str[i] = '-';
    for ( ; i<6; i++) str[i+1] = hold_date[i];
    str[i+1] = '-';
    for ( ; hold_date[i] != '\0'; i++) str[i+2] = hold_date[i];
    str[i+2] = '\0';	/* Insert null character */
    if (p_to_file) fprintf (fp, "Generation_Date = %s;\r\n", str);
    else { size = strlen(str) + 21; return(size); }
  }
  return(size);
}

/**********************************************************************/
/* Generate a YYYY-mm-dd date string based on system clock time       */
/**********************************************************************/
int get_gen_date2 (char *string, FILE *fp, int p_to_file, char *okay)
{
  time_t current_time;
  struct tm *ptrtime;
  char datestr[12]="";
  int size=0;

  time(&current_time);                     /* get time from sys clock */
  ptrtime = localtime(&current_time);      /* breakdown to y m d      */
  strftime(datestr,11,"%Y-%m-%d",ptrtime); /* format for output       */
  if (p_to_file) fprintf(fp,"Generation_Date = %s;\r\n",datestr);
  else size=strlen(datestr)+21;
  return(size);
}


/**********************************************************************/
/* Generate a YYYY-mm-dd date string based on Generation Date gattr   */
/**********************************************************************/
int get_gen_date3 (CDFid id,char *string,FILE *fp,int p_to_file,char *okay)
{
  long entryZero=0;
  CDFstatus status;
  char datestr[32]="";
  int size=0;
  status = CDFlib(SELECT_,CDF_,id,
                          ATTR_NAME_,"Generation_date",
                          gENTRY_,entryZero,
                  GET_,gENTRY_DATA_,datestr,NULL_);
  if (status != CDF_OK) { /* use current clock time */
    size = get_gen_date2(string,fp,p_to_file,okay);
  }
  else {
    if (p_to_file) fprintf(fp,"Generation_Date = %s;\r\n",datestr);
    else size=strlen(datestr)+21;
  }
  return(size);
}


/**********************************************************************/
/*          Determine which variable is the time variable             */
/**********************************************************************/
int Determine_TimeVar (CDFid id, char *varName)
{
  CDFstatus status;
  long nz,found,entryN;
  char aname[31] = "";
  char ablank[31]= "                              ";

  /* Get the number of variables in the open CDF */
  status = CDFlib(SELECT_,CDF_,id,
                  GET_,CDF_NUMzVARS_,&nz,NULL_);
  if (status != CDF_OK) {
    printf("ERROR>While determining the time variable ...\n");
    printf("      Unable to get number of variables in CDF.\n");
    error(status);
  }
  else { /* search all entrys for a value for depend_0 attribute */
    found = 0; entryN = 0;
    while ((found == 0)&&(entryN < nz)) {
      strcpy(aname,ablank); /* clear if from previous get_ */      
      status = CDFlib(SELECT_,CDF_,id,
                              ATTR_NAME_,"DEPEND_0",
                              zENTRY_,entryN,
                      GET_,zENTRY_DATA_,aname,NULL_);
      if (status != CDF_OK) entryN = entryN + 1;
      else { 
        strcat(aname,"");
        strcpy(varName,aname); found = 1;
      }
    }
    /* determine if the time variable was found or not */
    if (found == 0) {
      printf("ERROR>Unable to get value of a DEPEND_0 vattr.\n");
      printf("      Unable to determine which variable represents time.\n");
    }
  }
  return(found);
}

/****************************************************************************
************************ GLOBAL ATTRIBUTE FUNCTION **************************
****************************************************************************/

/* This function gets the necessary global scope attributes and reads and 
     writes their values to file */

int get_glob_attr (CDFid id, char *attrName, FILE *fp, int p_to_file,
			char *okay)
{
  CDFstatus status;	    /* Returned status code */
  long entryN;		    /* Entry number */
  long numelems;	    /* Number of elements */
  int glob_size=0;	    /* Global attribute size in bytes */
  int g_date_size=0;	    /* Generation date size */
  char *buffer;		    /* Buffer space */

  entryN = 0;
  status = CDFlib(SELECT_, CDF_, id,
                           ATTR_NAME_, attrName,
		           gENTRY_, entryN,
		  GET_, gENTRY_NUMELEMS_, &numelems,
		  NULL_);

  if (status < CDF_OK) {
    if (status != NO_SUCH_ENTRY) error(status);
  }
  else {
    buffer = (char *) malloc (numelems + 1);	/* Allocate buffer space */
    if (buffer == NULL) printf ("Buffer for information is not allocated.\n");

    /* Get attribute value */
    status = CDFlib(SELECT_, CDF_, id,
                             ATTR_NAME_, attrName,
		             gENTRY_, entryN,
		    GET_, gENTRY_DATA_, buffer,
		    NULL_);
    if (status != CDF_OK) error(status);
    else {
      buffer[numelems] = '\0';	/* Insert null character into buffer */

      /* If p_to_file is true, compare attribute name to comparison string.
	    If no match occurs, print to file. Otherwise, print the generation
		date to file -- Sabrina Sowers */
      if (p_to_file) {
        if ((strcmp(attrName, "Logical_file_id"))) {

          if (!(strcmp(attrName, "Data_version")))
            fprintf (fp, "%s = %s;\r\n", attrName, buffer);
          else fprintf (fp, "%s = \"%s\";\r\n", attrName, buffer);
        }
	else g_date_size = get_gen_date3(id, buffer, fp, p_to_file, okay);
	free(buffer);
      }
	    /* If p_to_file is false, compare attribute name to comparison
		  string.  If match occurs, get the generation date size and
		    return the global attribute size */
      else {
        if (!(strcmp(attrName, "Logical_file_id"))) {
	  g_date_size = get_gen_date3(id, buffer, fp, p_to_file, okay);
          free(buffer);
          return(g_date_size);
        }
        else {
          if (!(strcmp(attrName, "Data_version")))
            glob_size = strlen(attrName) + strlen(buffer) + 6;
          else glob_size = strlen(attrName) + strlen(buffer) + 8;
          free(buffer);
          return(glob_size);
        }
      } 
    }
  }
  return(glob_size);
}

/****************************************************************************
************************ EPOCH VARIABLE FUNCTION ****************************
****************************************************************************/

/* This function will read the Epoch rVariable from the current CDF file
     and decipher the value */

int get_epoch_var (CDFid id, char *varName, FILE *fp,
			int p_to_file)
{
   CDFstatus status;	    /* Returned status code */
   long maxRec;		    /* Maximum record for rVariable */
   long varNum;		    /* Variable number */
   long recNum;		    /* Record number */
   long indices[2];	    /* Dimension indices */
   long year, month, day, hour;	    /* For dates */
   long minute, second, msec;
   double value;		/* rVariable value */
   int date_size=0;		/* Size in bytes dates take up */

   indices[0]=0; indices[1]=0;
/*  If p_to_file is true, get the start and end dates. Otherwise return the
	date_size -- Sabrina Sowers */

   if (p_to_file) {
     status = CDFlib(SELECT_, CDF_, id,
                              zVAR_NAME_, varName,
		     GET_, zVAR_NUMBER_, varName, &varNum,
		           zVAR_MAXREC_, &maxRec,
		     NULL_);

     if (status != CDF_OK) error(status);
     else {
        /* Check if CDF skeleton exists. If so, set dates to zero. Otherwise
	        get the start and end date */

       if (maxRec == -1) {
         fprintf (fp, "Start_date = %04d-%02d-%02dT%02d:%02d:%02d.%03dZ;\r\n",
		    0,0,0,0,0,0,0);
         fprintf (fp, "Stop_date = %04d-%02d-%02dT%02d:%02d:%02d.%03dZ;\r\n",
		    0,0,0,0,0,0,0);
       }
       else {
		    /* Get the value of the first record */
         recNum = 0;
	 status = CDFlib(SELECT_, CDF_, id,
                                  zVAR_, varNum,
			          zVAR_RECNUMBER_, recNum,
			          zVAR_DIMINDICES_, indices,
			 GET_, zVAR_DATA_, &value,
			 NULL_);
         if (status != CDF_OK) error(status);
           else {	    /* Decipher the variable value into date */
             EPOCHbreakdown(value, &year, &month, &day, &hour, &minute,
			     &second, &msec);
             fprintf (fp, "Start_date = %d-%02d-%02dT%02d:%02d:%02d.%03dZ;\r\n",
		    year, month, day, hour, minute, second, msec);
           }

         /* Get the value of the last record */

         recNum = maxRec;
         status = CDFlib(SELECT_, CDF_, id,
                                  zVAR_, varNum,
                                  zVAR_RECNUMBER_, recNum,
                                  zVAR_DIMINDICES_, indices,
                         GET_, zVAR_DATA_, &value,
                         NULL_);
         if (status != CDF_OK) error(status);
         else {	    /* Decipher the variable value into date */
           EPOCHbreakdown(value, &year, &month, &day, &hour, &minute,
			     &second, &msec);
           fprintf (fp, "Stop_date = %d-%02d-%02dT%02d:%02d:%02d.%03dZ;\r\n",
		     year, month, day, hour, minute, second, msec);
         }
       }
     }
   }
   else {
     date_size = 79;
     return(date_size);
   }
   return(date_size);
}

/****************************************************************************
************************ CONTENTS IDENTIFIER OBJECT *************************
****************************************************************************/

int get_CIO (CDFid id, FILE *fp, int p_to_file, char *okay)
{
  CDFstatus status;	    /* Returned status code */
  int CIOsize=0;			/* Contents Identifier Object size */
  char attrName[CDF_ATTR_NAME_LEN+1];	/* Attribute name */
  char varName[CDF_VAR_NAME_LEN+1];	/* Variable Name */
  long attrNum;                         /* Attribute Number*/
  long varNum;                          /* Variable Name */
  long numelems;		/* Number of elements */

  /* Acquire the necessary global scope attributes -- Sabrina Sowers */

    status = CDFlib(SELECT_, CDF_, id,
		             ATTR_NAME_, "Project",
                    GET_, ATTR_NAME_, attrName,
		    NULL_);
    if (status != CDF_OK) {
      printf ("\nERROR> Unable to find global attribute \"Project\".\n");
      printf ("CDF file is not ISTP formatted.  Please modify.\n");
      error(status);
    }
    CIOsize = get_glob_attr(id, attrName, fp, p_to_file, okay);

    status = CDFlib(SELECT_, CDF_, id,
                             ATTR_NAME_, "Discipline",
                    GET_, ATTR_NAME_, attrName,
                    NULL_);
    if (status != CDF_OK) {
      printf ("\nERROR> Unable to find global attribute \"Discipline\".\n");
      printf ("CDF file is not ISTP formatted.  Please modify.\n");
      error(status);
    }
    CIOsize += get_glob_attr(id, attrName, fp, p_to_file, okay);

    status = CDFlib(SELECT_, CDF_, id,
                             ATTR_NAME_, "Source_name",
                    GET_, ATTR_NAME_, attrName,
                    NULL_);
    if (status != CDF_OK) {
      printf ("\nERROR> Unable to find global attribute \"Source_name\".\n");
      printf ("CDF file is not ISTP formatted.  Please modify.\n");
      error(status);
    }
    CIOsize += get_glob_attr(id, attrName, fp, p_to_file, okay);

    status = CDFlib(SELECT_, CDF_, id,
                             ATTR_NAME_, "Data_type",
                    GET_, ATTR_NAME_, attrName,
                    NULL_);
    if (status != CDF_OK) {
      printf ("\nERROR> Unable to find global attribute \"Data_type\".\n");
      printf ("CDF file is not ISTP formatted.  Please modify.\n");
      error(status);
    }
    CIOsize += get_glob_attr(id, attrName, fp, p_to_file, okay);

    status = CDFlib(SELECT_, CDF_, id,
                             ATTR_NAME_, "Descriptor",
                    GET_, ATTR_NAME_, attrName,
                    NULL_);
    if (status != CDF_OK) {
      printf ("\nERROR> Unable to find global attribute \"Descriptor\".\n");
      printf ("CDF file is not ISTP formatted.  Please modify.\n");
      error(status);
    }
    CIOsize += get_glob_attr(id, attrName, fp, p_to_file, okay);

    /* Determine the name of the variable which contains the Epoch */
    status = Determine_TimeVar(id, varName);

    CIOsize += get_epoch_var (id, varName, fp, p_to_file);

    /* Acquire the data version */
    status = CDFlib(SELECT_, CDF_, id,
                             ATTR_NAME_, "Data_version",
                    GET_, ATTR_NAME_, attrName,
                    NULL_);
    if (status != CDF_OK) {
      printf ("\nERROR> Unable to find global attribute \"Data_version\".\n");
      printf ("CDF file is not ISTP formatted.  Please modify.\n");
      error(status);
    }
    CIOsize += get_glob_attr(id, attrName, fp, p_to_file, okay);

    status = CDFlib(SELECT_, CDF_, id,
                             ATTR_NAME_, "Logical_file_id",
                    GET_, ATTR_NAME_, attrName,
                    NULL_);
    if (status != CDF_OK) {
      printf ("\nERROR> Unable to find global attribute \"Logical_file_id\".\n");
      printf ("CDF file is not ISTP formatted.  Please modify.\n");
      error(status);
    }
    CIOsize += get_glob_attr(id, attrName, fp, p_to_file, okay);

  return(CIOsize);
}

/****************************************************************************
************************ REFERENCE LABEL OBJECT *****************************
****************************************************************************/

int get_ref_label (CDFid id, char *CDFname, FILE *fp, FILE *adi,
		     int p_to_file, char *okay)
{
  CDFstatus status;		/* Returned status code */
  char *str, *p;                /* Pointer to adjusted CDFfilename */
  char ConAuth[]="NSSD";	/* Control authority (default) */
  char ACAI[]="0000";		/* Assigned Control Authority id (default) */
  char *mission;		/* Mission identifier */
  char *datatype;		/* Data type */
  char *descr;			/* Descriptor */
  char *buffer;			/* Buffer space */
  char *MI, *DS, *DT, *CA, *AI;	    /* Buffers for file search */
  char *T1, *T2, *T3;              /* Buffers for file search */
  char ADIDbuffer[9];           /* buffer for ADID_ref */
  int i, j, k;		/* Loop control variables */
  int refsize;		/* Reference label size */
  int success=0;	/* Success flag */
  long numelems;	/* Number of elements */
  long entryno=0;       /* entry number */

  str = CDFname;    /* Str points to the CDFfilename supplied by user */

  i = 0;
  /* Check for device or node names in the CDFfilename.  If there, remove it. */
  p = strstr(str, "::");	    /* search for nodename */
  if (p != NULL) str = p+2;	    /* if found advance beyond "::" */
  p = strchr(str, ':');		    /* search for device name */
  if (p != NULL) str = p+1;	    /* if found advance beyond ':' */
  p = str;
/* Note: The loop below needs the preceeding if or it will convert the '.'  */
/* which preceeds the file type to a '/' */
  if (*p == '[')		    /* VMS directory name? */
    while (*p != '\0')		    /* replace VMS directory punctuation */
    {				    /* with Unix-style '/' */
      if (*p == '[' || *p == '.' || *p == ']') *p = '/';
      if (*p == ']') break;
    }

  if ((p_to_file) && (strcmp(okay, "F"))) {
    /* Attempt to get the Control Authority info from ADID_ref gattr */
    status = CDFlib(SELECT_, CDF_, id,
                             ATTR_NAME_, "ADID_ref",
                             gENTRY_, entryno,
                    GET_, gENTRY_DATA_, ADIDbuffer, NULL_);
    ADIDbuffer[8] = '\0';		/* CDFlib call doesn't end string
					   w/null; do it ourselves */
    if ((status == CDF_OK)&&(strlen(ADIDbuffer) == 8)) {
      for (i=0;i<4;i++) ConAuth[i]=ADIDbuffer[i]; 
      for (j=0;j<4;j++,i++) ACAI[j]=ADIDbuffer[i]; 
      /* Write the information to file */  
      fprintf (fp, "%s\r\n", "REFERENCETYPE = ($CCSDS2);");
      fprintf (fp, "%s%s%s%s%s%s\r\n", "LABEL = ", ConAuth, "3IF0", ACAI,
  	           "00000001", ";");
      fprintf (fp, "%s%s%s\r\n", "REFERENCE = (", str, ");");
    }
    else {
      /* Attempt to get the Control Authority info from Logical_File_ID */
      status = CDFlib(SELECT_, CDF_, id,
                               ATTR_NAME_, "Logical_file_id",
  		             gENTRY_, entryno,
  		      GET_, gENTRY_NUMELEMS_, &numelems, NULL_);
      if (status < CDF_OK) {
        if (status != NO_SUCH_ENTRY) error(status);
      }
      else {
        buffer = (char *) malloc (numelems + 1); /* Allocate buffer space */
        if (buffer == NULL) printf ("Buffer for info not allocated.\n");
        /* Get attribute value */
        status = CDFlib(SELECT_, CDF_, id,
                                 ATTR_NAME_, "Logical_file_id",
  		                 gENTRY_, entryno,
  		        GET_, gENTRY_DATA_, buffer, NULL_);
        if (status != CDF_OK) error(status);
        else {
          buffer[numelems] = '\0';	/* Insert null character into buffer */

          mission = malloc (6);	/* Allocate memory space */
          datatype = malloc (6);
          descr = malloc (6);
          /* Get the mission, datatype , descriptor from Logical_file_id */
          for (i=0; buffer[i] != '_'; i++) mission[i] = buffer[i];
          mission[i] = '\0';
          for (j=0; buffer[i+j+1] != '_'; j++) datatype[j] = buffer[i+j+1];
          datatype[j] = '\0';
  	  /* Descriptor is currently a maximum of 4 letters. That is a
	     restriction adhered to in its extraction */
          for (k=0; buffer[i+j+k+2] != '_'; k++) descr[k] = buffer[i+j+k+2];
          if (k==3) {
            descr[k] = ' ';
            descr[k+1] = '\0';
          }
          else descr[k] = '\0';
          /* Allocate memory for the file search. This is restricted to the
	    current format of the ADI.TXT file */
          MI = malloc (4); DS = malloc (6); DT = malloc (4);
          T1 = malloc (17); T2 = malloc (20); T3 = malloc (25);
          AI = malloc (6); CA = malloc (6);
          i = 0; j = 0; k = 0;  /* Reinitialize the loop control variables */
          /* Search for the existence of mission, datatype and descriptor while
	     not end of file adi.txt and success is false. If they exist,
             get the Control Authority and Assigned Control Authority ID */
          do {
            fgets(MI, 3, adi);		    /* Get mission from file */
            if (!(strcmp(MI, mission))) {	    /* If comparison, get descriptor */
  	      if (fgetc(adi) ==EOF) printf("ADI FILE FORMAT ERROR\n");
	      fgets(DS, 5, adi);              /* Get descriptor from file */
              if (!(strcmp(DS, descr))) {	    /* If comparison, get datatype */
	        if (fgetc(adi) ==EOF) printf("ADI FILE FORMAT ERROR\n");
                fgets(DT, 3, adi);	    /* Get datatype from file */
                if (!(strcmp(DT, datatype))) {  /* If comparison, get ACAI, CA */
		  if (fgetc(adi) ==EOF) printf("ADI FILE FORMAT ERROR\n");
                  fgets(CA, 5, adi);	/* Get Control Authority */
		    strcpy (ConAuth, CA);
                  fgets(AI, 5, adi);	/* Get the Control Authority Id */
		    strcpy (ACAI, AI);
		  success = 1;            /* Set success flag to true */
                }
                else fgets(T1, 16, adi);  /* Read to end of line */
              }
              else fgets(T2, 19, adi);	/* Read to end of line */
            }
            else fgets(T3, 24, adi);	/* Read to end of line */
         } while ((!(feof(adi))) && (!(success==1)));

          /* Write the information to file */  
          fprintf (fp, "%s\r\n", "REFERENCETYPE = ($CCSDS2);");
          fprintf (fp, "%s%s%s%s%s%s\r\n", "LABEL = ", ConAuth, "3IF0", ACAI,
  			 "00000001", ";");
          fprintf (fp, "%s%s%s\r\n", "REFERENCE = (", str, ");");
       }
      }
      /* Free allocated memory space */
      free(DT); free(T1); free(T2);  free(T3);  free(AI); free(MI);
      free(DS); free(mission);  free(datatype);  free(descr); free(buffer);
    }

  }

  /*  If p_to_file is true and invalid Logical_file_id, print the
		default information to file */

  if ((p_to_file) && (strcmp(okay, "T"))) {
    fprintf (fp, "%s%c\r\n", "REFERENCETYPE = ($CCSDS2);");
    fprintf (fp, "%s%s%s%s%s%s\r\n", "LABEL = ", ConAuth, "3IF0", ACAI,
	"00000001", ";");
    fprintf (fp, "%s%s%s\r\n", "REFERENCE = (", str, ");");
  }
  else {
    refsize = 26 + 8 + strlen(ConAuth) + 4 + strlen(ACAI) + 9 +
		 15 + strlen(str) + 6;
    return(refsize);
  }
  return(refsize);
}

/****************************************************************************
******************************** MAIN PROGRAM *******************************
****************************************************************************/

main (int argc, char *argv[])
{
  FILE *fp, *adi;	        /* file pointers */
  CDFid id;		        /* CDF identifier */
  CDFstatus status;	        /* Status code */
  int CIOsize=0;		/* Contents Identifier Object size */
  int REFsize=0;		/* Reference Label size */
  int p_to_file=0;		/* Print to file flag */
  int Z_label=0, R_label=0, K_label=0;    /* SFDU label sizes */
  char *okay="T";		/* Flag */
  int nCDFs,i,wild;
  char **dirS;
  char **CDFs;
  char cname[80] = "";
  char CDFname[80] = "";
  char SFDUname[80] = "";


  /* Check if correct number of arguments on command line */
  if (argc != 4) {
    printf ("\nIncorrect number of parameters entered on command line.\n");
    printf ("Need to enter:\n");
    printf ("makesfdu istp::sys$public:[sfdu_tools.bld_sfdu]adi.txt ");
    printf ("CDFfilename SFDUfilename\n");
    exit(1);
  }

  strcpy(CDFname,argv[2]); /* copy CDF file name */
  strcpy(SFDUname,argv[3]); /* copy SFDU file name */

  /* Check for single SFDU construction or for wildcard */
  if (strcmp(CDFname,"*")==0)  {
    nCDFs = CDFdirList ("*", &dirS, &CDFs); wild=1;
  }
  else { nCDFs = 1;  wild=0; }

  /* For all SFDU's to be created */
  for (i=0; i<nCDFs; i++) {

    /* Open input CDF file */
    if (wild==1) { strcpy(CDFname,*CDFs++); strcpy(argv[2],CDFname); }
    status = CDFlib(OPEN_, CDF_, CDFname, &id,
		    NULL_);
    if (status < CDF_WARN) {
      printf ("\nProblem opening CDF file %s\n", argv[2]);
      error(status); }
    else {
      status = CDFlib(SELECT_, CDF_, id,
                               CDF_zMODE_, zMODEon2,
		      NULL_);
      if (status != CDF_OK) {
        printf ("\nProblem opening CDF file %s in z mode \n", argv[2]);
        error(status);
      }

      /* Open the ADI.TXT file */
      if ((adi = fopen(argv[1], "r"))==NULL) {
        printf ("\nError opening representative ADI.TXT file: '%s'\n", argv[1]);
        printf ("Try:  istp::sys$public:[sfdu_tools.bld_sfdu]adi.txt\n");
        exit(1);
      }

      /* Open the SFDU file */
      if (wild == 1) { strcpy(SFDUname,CDFname); strcat(SFDUname,".sfd");}
      if ((fp = fopen(SFDUname, "w"))==NULL) {
        printf ("\nError opening SFDU file '%s'\n", SFDUname);
        exit(1);
      }

      p_to_file = 0;
      strcpy(cname,CDFname); strcat(cname,".cdf");
      CIOsize = get_CIO(id, fp, p_to_file, okay);
      REFsize = get_ref_label(id, cname, fp, adi, p_to_file, okay);
      p_to_file = 1;
      R_label = REFsize + 2;
      K_label = CIOsize + 2;
      Z_label = 20 + 20 + K_label + R_label;
      fprintf (fp, "%s%08d", "CCSD1Z000001", Z_label);
      fprintf (fp, "%s%08d\r\n", "NSSD1K000060", K_label);
      CIOsize = get_CIO(id, fp, p_to_file, okay);
      fprintf (fp, "%s%08d\r\n", "CCSD1R000003", R_label);
      REFsize = get_ref_label(id, cname, fp, adi, p_to_file, okay);
    }

    /* close the CDF file */
    status = CDFlib(SELECT_, CDF_, id,
                    CLOSE_, CDF_, NULL_);
    if (status < CDF_OK) error(status);
    fclose(fp); /* close the SFDU file */
    fclose(adi); /* Close the represntative ADI.TXT file */

  } /* for all SFDUs to be generated */
}
