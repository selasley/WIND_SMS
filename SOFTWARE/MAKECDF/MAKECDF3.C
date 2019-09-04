#include "makecdf.h"

/**********************************************************************/
/*                     GENERATE A NEW EPOCH VALUE                     */
/**********************************************************************/
long Generate_Epoch(EpVAL)
double *EpVAL;
{
  long lyear,lmonth,lday,lsince;
  double dhour,dmin,dsec,dmsec,depoch,dsince;
  char epstring[EPOCH_STRING_LEN+1];
  long ierr=0;
  switch (EpALG.algorithm) {
    case 0 :ierr = Compute_Epoch(&depoch);
            if (ierr == 0) *EpVAL = depoch;
            break;
    case 1 :ierr=epochParse2(VLIST[EpALG.operand[0]].buffer,&depoch);
            if (ierr == 0) *EpVAL = depoch;            
            break;
    case 2 :ierr=epochParse3(VLIST[EpALG.operand[0]].buffer,&depoch);
            if (ierr == 0) *EpVAL = depoch;            
            break;
    case 3 :ierr=epochParse4(VLIST[EpALG.operand[0]].buffer,&depoch);
            if (ierr == 0) *EpVAL = depoch;            
            break;
    case 4: ierr=Get_Epoch(EpALG.operand[0],&depoch);
            *EpVAL = depoch;
            break;
    case 5: if (EpALG.BaseEp == 0.0) { /* Derive Base Epoch from operands */
              ierr=Get_BaseEpoch(&depoch);
              if (ierr == 0) EpALG.BaseEp = depoch;
            }
            switch (EpALG.operand[8]) { /* units of offset variable */
            case 0 : MSG_control(2,"ERROR: Years is not legal offset unit!",0);
                     ierr=1; break;
            case 1 : MSG_control(2,"ERROR: Months is not legal offset unit!",0);
                     ierr=1; break;
            case 2 : ierr=Get_Day(EpALG.operand[7],&lsince);
                     depoch = EpALG.BaseEp + (lsince * 86400000);
                     break;
            case 3 : ierr=Get_Hour(EpALG.operand[7],&dsince);
                     depoch = EpALG.BaseEp + (dsince * 3600000.0);
                     break;
            case 4 : ierr=Get_Minute(EpALG.operand[7],&dsince);
                     depoch = EpALG.BaseEp + (dsince * 60000.0);
                     break;
            case 5 : ierr=Get_Second(EpALG.operand[7],&dsince);
                     depoch = EpALG.BaseEp + (dsince * 1000.0);
                     break;
            case 6 : ierr=Get_MSecs(EpALG.operand[7],&dsince);
                     depoch = EpALG.BaseEp + dsince;
                     break;
            default: MSG_control(2,"ERROR: Offset variable units unknown.",0);
                     ierr=1; break;
            }
            if (ierr == 0) *EpVAL = depoch;
            break;
    case 6: if (EpALG.BaseEp == 0.0) { /* Derive Base Epoch from operands */
              ierr=Compute_Epoch(&depoch);
              if (ierr == 0) EpALG.BaseEp = depoch;
            }
            switch (EpALG.operand[8]) { /* units of offset variable */
            case 0 : MSG_control(2,"ERROR: Years is not legal offset unit!",0);
                     ierr=1; break;
            case 1 : MSG_control(2,"ERROR: Months is not legal offset unit!",0);
                     ierr=1; break;
            case 2 : ierr=Get_Day(EpALG.operand[7],&lsince);
                     depoch = EpALG.BaseEp + (lsince * 86400000);
                     break;
            case 3 : ierr=Get_Hour(EpALG.operand[7],&dsince);
                     depoch = EpALG.BaseEp + (dsince * 3600000.0);
                     break;
            case 4 : ierr=Get_Minute(EpALG.operand[7],&dsince);
                     depoch = EpALG.BaseEp + (dsince * 60000.0);
                     break;
            case 5 : ierr=Get_Second(EpALG.operand[7],&dsince);
                     depoch = EpALG.BaseEp + (dsince * 1000.0);
                     break;
            case 6 : ierr=Get_MSecs(EpALG.operand[7],&dsince);
                     depoch = EpALG.BaseEp + dsince;
                     break;
            default: MSG_control(2,"ERROR: Offset variable units unknown.",0);
                     ierr=1; break;
            }
            if (ierr == 0) *EpVAL = depoch;
            break;
    case 7: if (EpALG.BaseEp == 0.0) { /* Derive Base Epoch from filename */
              ierr=Parse_Filename(RPARMS.infname);
              if (ierr != 0) return(ierr);
            }
            switch (EpALG.operand[8]) { /* units of offset variable */
            case 0 : MSG_control(2,"ERROR: Years is not legal offset unit!",0);
                     ierr=1; break;
            case 1 : MSG_control(2,"ERROR: Months is not legal offset unit!",0);
                     ierr=1; break;
            case 2 : ierr=Get_Day(EpALG.operand[7],&lsince);
                     depoch = EpALG.BaseEp + (lsince * 86400000);
                     break;
            case 3 : ierr=Get_Hour(EpALG.operand[7],&dsince);
                     depoch = EpALG.BaseEp + (dsince * 3600000.0);
                     break;
            case 4 : ierr=Get_Minute(EpALG.operand[7],&dsince);
                     depoch = EpALG.BaseEp + (dsince * 60000.0);
                     break;
            case 5 : ierr=Get_Second(EpALG.operand[7],&dsince);
                     depoch = EpALG.BaseEp + (dsince * 1000.0);
                     break;
            case 6 : ierr=Get_MSecs(EpALG.operand[7],&dsince);
                     depoch = EpALG.BaseEp + dsince;
                     break;
            default: MSG_control(2,"ERROR: Offset variable units unknown.",0);
                     ierr=1; break;
            }
            if (ierr == 0) *EpVAL = depoch;
            break;
    default:MSG_control(2,"ERROR: Unknown epoch algorithm",0);
            ierr=1;
  }

  if (ierr == 0) {
    if (EpALG.FirstEp == 0.0) EpALG.FirstEp = depoch;
    if (RPARMS.debug == TRUE) {
      encodeEPOCH(depoch,epstring);
      strcpy(LOGFILE.message,"New Epoch Generated="); 
      strcat(LOGFILE.message,epstring);
      MSG_control(0,LOGFILE.message,0);
    }
  }
  else MSG_control(1,"WARNING: Invalid Epoch generated.",0);
  return(ierr);
}

/**********************************************************************/
/*                 DETERMINE EPOCH GENERATOR VARIABLE                 */
/**********************************************************************/
long Determine_EpGenerator()
{
  long i;
  long ierr=0;

  switch (EpALG.algorithm) {
    case 0 : for (i=0;i<=9;i++) {
               if (EpALG.operand[i] != -1) EpALG.EpGenerator=EpALG.operand[i];
             }
             break;
    case 1 : EpALG.EpGenerator = EpALG.operand[0]; break;
    case 2 : EpALG.EpGenerator = EpALG.operand[0]; break;
    case 3 : EpALG.EpGenerator = EpALG.operand[0]; break;
    case 4 : EpALG.EpGenerator = EpALG.operand[7]; break;
    case 5 : EpALG.EpGenerator = EpALG.operand[7]; break;
    case 6 : EpALG.EpGenerator = EpALG.operand[7]; break;
    case 7 : EpALG.EpGenerator = EpALG.operand[7]; break;
    default: MSG_control(1,"ERROR: Unknown Epoch algorithm!",0);
             MSG_control(1,"       Unable to continue.",0);
             ierr=1;
  }
  return(ierr);
}

/**********************************************************************/
/*                            MonthToken.                             */
/**********************************************************************/
char *MonthToken (month)
long month;
{
  switch (month) {
    case 1:  return "Jan";
    case 2:  return "Feb";
    case 3:  return "Mar";
    case 4:  return "Apr";
    case 5:  return "May";
    case 6:  return "Jun";
    case 7:  return "Jul";
    case 8:  return "Aug";
    case 9:  return "Sep";
    case 10: return "Oct";
    case 11: return "Nov";
    case 12: return "Dec";
  }
  return "???";
}

/******************************************************************************
* epochParse2.  This function parses an input date/time string (inString) to
*               set the integer members of the EPOCH structure.
*
*               This function performs the same function as the routine
*               epochParse, but requires a less rigid string format.  It
*               allows the following:
*                    a) The year field may be 2 or 4 digits.  If only 2
*                       digits, the launch date of SPUTNIK ('57) will be
*                       used to determine the century.
*                    b) Any punctuation marks may exist between the fields.
*                       You are not restricted to hyphens and colons.
*                    c) Only the day,month and year fields are required.
*                       Unless present the other fields are assumed to be 0.
*
*                    "dd-MON-yyyy hh:mm:ss.mmm"
*                    1-Apr-1990 03:05:02.002
*
******************************************************************************/
long epochParse2 (inString, Etime)
char *inString;
double *Etime;
{
  char *Sptr;
  char moString[4];
  char p;
  long j,year,month,day,hour,minute,second,msec;

  Sptr = inString; /* point to the input string */
  if (sscanf(Sptr,"%ld%c",&day,&p)!=2) return (1); /* parse day out */
  while (*Sptr != p) Sptr++; Sptr++; /* advance pointer to next field */
  if (sscanf(Sptr,"%3s%c",moString,&p)!=2) return (1); /* parse month out */
  while (*Sptr != p) Sptr++; Sptr++; /* advance pointer to next field */
  if (sscanf(Sptr,"%ld",&year)!=1) return (1); /* parse year out */
  while (isdigit(*Sptr)!= 0) Sptr++; /* advance pointer to next field */
  /* correct the year if it is only 2 digits, assuming pre-post SPUTNIK */
  if ((year >= 57)&&(year < 100)) year = year + 1900;
  else if ((year < 57)&&(year < 100)) year = year + 2000;
  /* day,month,year parsed. Initialize remaining fields */
  hour = 0; minute = 0; second = 0; msec = 0;
  if (sscanf(Sptr,"%c",&p)==1) { Sptr++; /* intermediate punctuation found */
    if (sscanf(Sptr,"%ld",&hour)==1) { Sptr++; Sptr++; /* hour found */
      if (sscanf(Sptr,"%c",&p)==1) { Sptr++; /* punctuation */
	if (sscanf(Sptr,"%ld",&minute)==1) { Sptr++; Sptr++; /* minute */
	  if (sscanf(Sptr,"%c",&p)==1) { Sptr++; /* punctuation */
	    if (sscanf(Sptr,"%ld",&second)==1) { Sptr++; Sptr++; /* sec */
	      if (sscanf(Sptr,"%c",&p)==1) { Sptr++; /* punctuation */
		sscanf(Sptr,"%ld",&msec); /* parse milliseconds out */
  } } } } } } }

  if ((day < 1 || day > 31) ||
      (year < 0 || year > 9999) ||
      (hour < 0 || hour > 23) ||
      (minute < 0 || minute > 59) ||
      (second < 0 || second > 59) ||
      (msec < 0 || msec > 999)) {
    return (1);
  }

  for (j = 0; j < strlen(moString); j++)
     moString[j] = (islower(moString[j]) ? toupper(moString[j]) : moString[j]);

  if (islower(moString[0])) moString[0] = toupper(moString[0]);   /* J */
  if (isupper(moString[1])) moString[1] = tolower(moString[1]);   /* a */
  if (isupper(moString[2])) moString[2] = tolower(moString[2]);   /* n */

  month = 0;
  for (j = 1; j <= 12; j++)
    if (strcmp(moString, MonthToken(j)) == 0) {
      month = j;
      break;
    }
  if (month == 0) return (1);
  *Etime = computeEPOCH(year,month,day,hour,minute,second,msec);
  return (0);
}

/******************************************************************************
* epochParse3.  This function parses an input date/time string (inString) to
*               set the integer members of the EPOCH structure.
*               This function performs the same function as the routine
*               epochParse2, but parses an ISO time string format.  It
*               allows the following:
*               a) The year field may be 2 or 4 digits.  If only 2
*                  digits, the launch date of SPUTNIK ('57) will be
*                  used to determine the century.
*               b) Any punctuation marks may exist between the fields.
*                  You are not restricted to hyphens and colons.
*               c) Only the day,month and year fields are required.
*                  Unless present the other fields are assumed to be 0.
*
*                    "yyyy-mm-ddThh:mm:ss.mmmZ
*                     1994-04-01T13:01:02.000Z
*
******************************************************************************/

long epochParse3 (inString, Etime)
char *inString;
double *Etime;
{
  char *Sptr;
  char p;
  long year,month,day,hour,minute,second,msec;

  Sptr = inString; /* point to the input string */
  if (sscanf(Sptr,"%ld%c",&year,&p) < 2) return (1); /* parse year out */
  while (*Sptr != p) Sptr++; Sptr++; /* advance pointer to next field */
  if (sscanf(Sptr,"%ld%c",&month,&p)!=2) return (1); /* parse month out */
  while (*Sptr != p) Sptr++; Sptr++; /* advance pointer to next field */
  if (sscanf(Sptr,"%ld%c",&day,&p)!=2) return (1); /* parse day out */
  while (*Sptr != p) Sptr++; Sptr++; /* advance pointer to next field */
  hour = 0; minute = 0; second = 0; msec = 0; /* Initialize remaining fields */
  if (sscanf(Sptr,"%ld",&hour)==1) { Sptr++; Sptr++; /* hour found */
    if (sscanf(Sptr,"%c",&p)==1) { Sptr++; /* punctuation */
      if (sscanf(Sptr,"%ld",&minute)==1) { Sptr++; Sptr++; /* minute */
        if (sscanf(Sptr,"%c",&p)==1) { Sptr++; /* punctuation */
          if (sscanf(Sptr,"%ld",&second)==1) { Sptr++; Sptr++; /* sec */
            if (sscanf(Sptr,"%c",&p)==1) { Sptr++; /* punctuation */
              sscanf(Sptr,"%ld",&msec); /* parse milliseconds out */
  } } } } } }

  /* correct the year if it is only 2 digits, assuming pre-post SPUTNIK */
  if ((year >= 57)&&(year < 100)) year = year + 1900;
  else if ((year < 57)&&(year < 100)) year = year + 2000;
  if ((day    < 1 || day    > 31)   ||
      (year   < 0 || year   > 9999) ||
      (month  < 1 || month  > 12)   ||
      (hour   < 0 || hour   > 23)   ||
      (minute < 0 || minute > 59)   ||
      (second < 0 || second > 59)   ||
      (msec   < 0 || msec   > 999))  return (1);
  *Etime = computeEPOCH(year,month,day,hour,minute,second,msec);
  return (0);
}

/******************************************************************************
* epochParse4.  This function parses an input date/time string (inString) to
*               set the integer members of the EPOCH structure.
*               This function performs the same function as the routine
*               epochParse2&3, but parses a different time string format.  It
*               allows the following:
*               a) The year field may be 2 or 4 digits.  If only 2
*                  digits, the launch date of SPUTNIK ('57) will be
*                  used to determine the century.
*               b) Day is Day of Year, no Month is present
*               b) Any punctuation marks may exist between the fields.
*                  You are not restricted to hyphens and colons.
*               c) Only the year and day fields are required.
*                  Unless present the other fields are assumed to be 0.
*
*                    "yyyy-dd hh:mm:ss.mmmZ
*                     1994-01 13:01:02.000Z
*
******************************************************************************/

long epochParse4 (inString, Etime)
char *inString;
double *Etime;
{
  char *Sptr;
  char p;
  long year,month,day,hour,minute,second,msec,i;

  Sptr = inString; /* point to the input string */
  if (sscanf(Sptr,"%ld%c",&year,&p) < 2) return (1); /* parse year out */
  while (*Sptr != p) Sptr++; Sptr++; /* advance pointer to next field */
  if (sscanf(Sptr,"%ld%c",&day,&p)!=2) return (1); /* parse day of year out */
  while (*Sptr != p) Sptr++; Sptr++; /* advance pointer to next field */
  hour = 0; minute = 0; second = 0; msec = 0; /* Initialize remaining fields */
  if (sscanf(Sptr,"%ld",&hour)==1) { Sptr++; Sptr++; /* hour found */
    if (sscanf(Sptr,"%c",&p)==1) { Sptr++; /* punctuation */
      if (sscanf(Sptr,"%ld",&minute)==1) { Sptr++; Sptr++; /* minute */
        if (sscanf(Sptr,"%c",&p)==1) { Sptr++; /* punctuation */
          if (sscanf(Sptr,"%ld",&second)==1) { Sptr++; Sptr++; /* sec */
            if (sscanf(Sptr,"%c",&p)==1) { Sptr++; /* punctuation */
              sscanf(Sptr,"%ld",&msec); /* parse milliseconds out */
  } } } } } }

  /* correct the year if it is only 2 digits, assuming pre-post SPUTNIK */
  if ((year >= 57)&&(year < 100)) year = year + 1900;
  else if ((year < 57)&&(year < 100)) year = year + 2000;
  /* determine month given the day of year */
  Convert_DOY(year,&month,&day);

  /* validate before computation */
  if ((day    < 1 || day    > 31)   ||
      (year   < 0 || year   > 9999) ||
      (month  < 1 || month  > 12)   ||
      (hour   < 0 || hour   > 23)   ||
      (minute < 0 || minute > 59)   ||
      (second < 0 || second > 59)   ||
      (msec   < 0 || msec   > 999))  return (1);
  *Etime = computeEPOCH(year,month,day,hour,minute,second,msec);
  return (0);
}

/**************************************************************************
*                         COMPUTE JULIAN DATE                             *
**************************************************************************/
/* The julian_date function is included directly here because its needed */
/* by the makeEpoch function.  It computes the julian date given y,m,d   */
long julian_date(y,m,d)
long y,m,d;
{
long jd;
jd = (long) (367*y-7*(y+(m+9)/12)/4-3*((y+(m-9)/7)/100+1)/4+275*m/9+d+1721029);
return (jd);
}

/**************************************************************************
*                             GET_YEAR                                    *
**************************************************************************/
long Get_Year(vnum, lval)
long vnum;
long *lval;
{
  int  *iptr;
  long *lptr;
  long year;
  long ierr=0;

  if (vnum == -1) { ierr=1; return(ierr);} /* cannot compute without year */
  switch (VLIST[vnum].cdfvartype) {
    case CDF_REAL4 : ierr=1; break;
    case CDF_FLOAT : ierr=1; break;
    case CDF_REAL8 : ierr=1; break;
    case CDF_DOUBLE: ierr=1; break;
    case CDF_EPOCH : ierr=1; break;
    case CDF_INT4  : lptr =(long *)VLIST[vnum].dataptr;
                     year = *lptr;
                     break;
    case CDF_UINT4 : lptr =(long *)VLIST[vnum].dataptr;
                     year = *lptr;
                     break;
    case CDF_INT2  : iptr =(int *)VLIST[vnum].dataptr;
                     year =(long)*iptr;
                     break;
    case CDF_UINT2 : iptr =(int *)VLIST[vnum].dataptr;
                     year =(long)*iptr;
                     break;
    case CDF_BYTE  : ierr=1; break;
    case CDF_CHAR  : ierr=1; break;
    case CDF_UCHAR : ierr=1; break;
    default        : ierr=1; break;
  }
  if (ierr == 0) {
    /* validate the year */
    if ((year >= 57)&&(year < 100)) year = year + 1900;
    else if ((year < 57)&&(year < 100)) year = year + 2000;
    *lval=year;
  }
  return(ierr);
}

/**************************************************************************
*                             GET_MONTH                                   *
**************************************************************************/
long Get_Month(vnum, lval)
long vnum;
long *lval;
{
  int  *iptr;
  long *lptr;
  long month,i;
  char mstr[20];

  static char cjan[] = "JANUARY";
  static char cfeb[] = "FEBRUARY";
  static char cmar[] = "MARCH";
  static char capr[] = "APRIL";
  static char cmay[] = "MAY";
  static char cjun[] = "JUNE";
  static char cjul[] = "JULY";
  static char caug[] = "AUGUST";
  static char csep[] = "SEPTEMBER";
  static char coct[] = "OCTOBER";
  static char cnov[] = "NOVEMBER";
  static char cdec[] = "DECEMBER";
  long ierr=0;

  if (vnum == -1) { *lval=0L; return(ierr); }
  switch (VLIST[vnum].cdfvartype) {
    case CDF_REAL4 : ierr=1; break;
    case CDF_FLOAT : ierr=1; break;
    case CDF_REAL8 : ierr=1; break;
    case CDF_DOUBLE: ierr=1; break;
    case CDF_EPOCH : ierr=1; break;
    case CDF_INT4  : lptr  =(long *)VLIST[vnum].dataptr;
                     month = *lptr;
                     break;
    case CDF_UINT4 : lptr  =(long *)VLIST[vnum].dataptr;
                     month = *lptr;
                     break;
    case CDF_INT2  : iptr  =(int *)VLIST[vnum].dataptr;
                     month =(long)*iptr;
                     break;
    case CDF_UINT2 : iptr  =(int *)VLIST[vnum].dataptr;
                     month =(long)*iptr;
                     break;
    case CDF_BYTE  : ierr=1; break;
    case CDF_CHAR  : strcpy(mstr,VLIST[vnum].buffer);
                     for (i=0;i<strlen(mstr);i++) toupper(mstr[i]);
                     if (memcmp(mstr,cjan,strlen(mstr))==0) month=1;
                     if (memcmp(mstr,cfeb,strlen(mstr))==0) month=2;
                     if (memcmp(mstr,cmar,strlen(mstr))==0) month=3;
                     if (memcmp(mstr,capr,strlen(mstr))==0) month=4;
                     if (memcmp(mstr,cmay,strlen(mstr))==0) month=5;
                     if (memcmp(mstr,cjun,strlen(mstr))==0) month=6;
                     if (memcmp(mstr,cjul,strlen(mstr))==0) month=7;
                     if (memcmp(mstr,caug,strlen(mstr))==0) month=8;
                     if (memcmp(mstr,csep,strlen(mstr))==0) month=9;
                     if (memcmp(mstr,coct,strlen(mstr))==0) month=10;
                     if (memcmp(mstr,cnov,strlen(mstr))==0) month=11;
                     if (memcmp(mstr,cdec,strlen(mstr))==0) month=12;
                     break;
    case CDF_UCHAR : ierr=1; break;
    default        : ierr=1; break;
  }
  if (ierr == 0) *lval = month; /* validate the month value */
  return(ierr);
}

/**************************************************************************
*                             GET_DAY                                     *
**************************************************************************/
long Get_Day(vnum, lval)
long vnum;
long *lval;
{
  int  *iptr;
  long *lptr;
  long day;
  long ierr=0;

  if (vnum == -1) { *lval=0L; return(ierr); }
  switch (VLIST[vnum].cdfvartype) {
    case CDF_REAL4 : ierr=1; break;
    case CDF_FLOAT : ierr=1; break;
    case CDF_REAL8 : ierr=1; break;
    case CDF_DOUBLE: ierr=1; break;
    case CDF_EPOCH : ierr=1; break;
    case CDF_INT4  : lptr=(long *)VLIST[vnum].dataptr;
                     day = *lptr;
                     break;
    case CDF_UINT4 : lptr=(long *)VLIST[vnum].dataptr;
                     day = *lptr;
                     break;
    case CDF_INT2  : iptr=(int *)VLIST[vnum].dataptr;
                     day =(long)*iptr;
                     break;
    case CDF_UINT2 : iptr=(int *)VLIST[vnum].dataptr;
                     day =(long)*iptr;
                     break;
    case CDF_BYTE  : ierr=1; break;
    case CDF_CHAR  : ierr=1; break;
    case CDF_UCHAR : ierr=1; break;
    default        : ierr=1; break;
  }
  if (ierr == 0) *lval=day; /* validate the day value */
  return(ierr);
}

/************************************************************************/
/*             CONVERT DAYS OF YEAR TO DAYS OF MONTH                    */
/************************************************************************/
void Convert_DOY(lyear,lmonth,lday)
long lyear;
long *lmonth;
long *lday;
{
  long day,month;
  long daysinmonth[12] = {31,28,31,30,31,30,31,31,30,31,30,31};

  month = *lmonth; day = *lday;
  if (fmod((double)lyear,4.0)==0.0) daysinmonth[1]=29; /*leap-year*/
  for (month=0;day>=daysinmonth[month];month++) {
    day = day - daysinmonth[month];
  }
  month++; /* Julian January is 1 not 0 */
  *lmonth = month; *lday = day;
}

/**************************************************************************
*                             GET_HOUR                                    *
**************************************************************************/
long Get_Hour(vnum, dval)
long vnum;
double *dval;
{
  int  *iptr;
  long *lptr;
  float *fptr;
  double *dptr;
  double hour;
  long ierr=0;

  if (vnum == -1) { *dval=0.0; return(ierr); }
  switch (VLIST[vnum].cdfvartype) {
    case CDF_REAL4 : fptr =(float *)VLIST[vnum].dataptr;
                     hour =(double)*fptr;
                     break;
    case CDF_FLOAT : fptr =(float *)VLIST[vnum].dataptr;
                     hour =(double)*fptr;
                     break;
    case CDF_REAL8 : dptr =(double *)VLIST[vnum].dataptr;
                     hour = *dptr;
                     break;
    case CDF_DOUBLE: dptr =(double *)VLIST[vnum].dataptr;
                     hour = *dptr;
                     break;
    case CDF_EPOCH : ierr=1; break;
    case CDF_INT4  : lptr =(long *)VLIST[vnum].dataptr;
                     hour =(double)*lptr;
                     break;
    case CDF_UINT4 : lptr =(long *)VLIST[vnum].dataptr;
                     hour =(double)*lptr;
                     break;
    case CDF_INT2  : iptr =(int *)VLIST[vnum].dataptr;
                     hour =(double)*iptr;
                     break;
    case CDF_UINT2 : iptr =(int *)VLIST[vnum].dataptr;
                     hour =(double)*iptr;
                     break;
    case CDF_BYTE  : ierr=1; break;
    case CDF_CHAR  : ierr=1; break;
    case CDF_UCHAR : ierr=1; break;
    default        : ierr=1; break;
  }
  if (ierr == 0) *dval=hour; /* validate the hour value */
  return(ierr);
}

/**************************************************************************
*                             GET_MINUTE                                  *
**************************************************************************/
long Get_Minute(vnum, dval)
long vnum;
double *dval;
{
  int  *iptr;
  long *lptr;
  float *fptr;
  double *dptr;
  double min;
  long ierr=0;

  if (vnum == -1) { *dval=0.0; return(ierr); }
  switch (VLIST[vnum].cdfvartype) {
    case CDF_REAL4 : fptr=(float *)VLIST[vnum].dataptr;
                     min =(double)*fptr;
                     break;
    case CDF_FLOAT : fptr=(float *)VLIST[vnum].dataptr;
                     min =(double)*fptr;
                     break;
    case CDF_REAL8 : dptr=(double *)VLIST[vnum].dataptr;
                     min = *dptr;
                     break;
    case CDF_DOUBLE: dptr=(double *)VLIST[vnum].dataptr;
                     min = *dptr;
                     break;
    case CDF_EPOCH : ierr=1; break;
    case CDF_INT4  : lptr=(long *)VLIST[vnum].dataptr;
                     min =(double)*lptr;
                     break;
    case CDF_UINT4 : lptr=(long *)VLIST[vnum].dataptr;
                     min =(double)*lptr;
                     break;
    case CDF_INT2  : iptr=(int *)VLIST[vnum].dataptr;
                     min =(double)*iptr;
                     break;
    case CDF_UINT2 : iptr=(int *)VLIST[vnum].dataptr;
                     min=(double)*iptr;
                     break;
    case CDF_BYTE  : ierr=1; break;
    case CDF_CHAR  : ierr=1; break;
    case CDF_UCHAR : ierr=1; break;
    default        : ierr=1; break;
  }
  if (ierr == 0) *dval=min; /* validate the min value */
  return(ierr);
}

/**************************************************************************
*                             GET_SECOND                                  *
**************************************************************************/
long Get_Second(vnum, dval)
long vnum;
double *dval;
{
  int  *iptr;
  long *lptr;
  float *fptr;
  double *dptr;
  double secs;
  long ierr=0;

  if (vnum == -1) { *dval=0.0; return(ierr); }
  switch (VLIST[vnum].cdfvartype) {
    case CDF_REAL4 : fptr=(float *)VLIST[vnum].dataptr;
                     secs=(double)*fptr;
                     break;
    case CDF_FLOAT : fptr=(float *)VLIST[vnum].dataptr;
                     secs=(double)*fptr;
                     break;
    case CDF_REAL8 : dptr=(double *)VLIST[vnum].dataptr;
                     secs= *dptr;
                     break;
    case CDF_DOUBLE: dptr=(double *)VLIST[vnum].dataptr;
                     secs= *dptr;
                     break;
    case CDF_EPOCH : ierr=1; break;
    case CDF_INT4  : lptr=(long *)VLIST[vnum].dataptr;
                     secs=(double)*lptr;
                     break;
    case CDF_UINT4 : lptr=(long *)VLIST[vnum].dataptr;
                     secs=(double)*lptr;
                     break;
    case CDF_INT2  : iptr=(int *)VLIST[vnum].dataptr;
                     secs=(double)*iptr;
                     break;
    case CDF_UINT2 : iptr=(int *)VLIST[vnum].dataptr;
                     secs=(double)*iptr;
                     break;
    case CDF_BYTE  : ierr=1; break;
    case CDF_CHAR  : ierr=1; break;
    case CDF_UCHAR : ierr=1; break;
    default        : ierr=1; break;
  }
  if (ierr == 0) *dval=secs; /* validate the secs value */
  return(ierr);
}

/**************************************************************************
*                             GET_MSecs                                   *
**************************************************************************/
long Get_MSecs(vnum, dval)
long vnum;
double *dval;
{
  int  *iptr;
  long *lptr;
  float *fptr;
  double *dptr;
  double msec;
  long ierr=0;

  if (vnum == -1) { *dval=0.0; return(ierr); }
  switch (VLIST[vnum].cdfvartype) {
    case CDF_REAL4 : fptr=(float *)VLIST[vnum].dataptr;
                     msec=(double)*fptr;
                     break;
    case CDF_FLOAT : fptr=(float *)VLIST[vnum].dataptr;
                     msec=(double)*fptr;
                     break;
    case CDF_REAL8 : dptr=(double *)VLIST[vnum].dataptr;
                     msec= *dptr;
                     break;
    case CDF_DOUBLE: dptr=(double *)VLIST[vnum].dataptr;
                     msec= *dptr;
                     break;
    case CDF_EPOCH : ierr=1; break;
    case CDF_INT4  : lptr=(long *)VLIST[vnum].dataptr;
                     msec=(double)*lptr;
                     break;
    case CDF_UINT4 : lptr=(long *)VLIST[vnum].dataptr;
                     msec=(double)*lptr;
                     break;
    case CDF_INT2  : iptr=(int *)VLIST[vnum].dataptr;
                     msec=(double)*iptr;
                     break;
    case CDF_UINT2 : iptr=(int *)VLIST[vnum].dataptr;
                     msec=(double)*iptr;
                     break;
    case CDF_BYTE  : ierr=1; break;
    case CDF_CHAR  : ierr=1; break;
    case CDF_UCHAR : ierr=1; break;
    default        : ierr=1; break;
  }
  if (ierr == 0) *dval=msec; /* validate the msec value */
  return(ierr);
}

/**************************************************************************
*                             GET_Epoch                                   *
**************************************************************************/
long Get_Epoch(vnum, dval)
long vnum;
double *dval;
{
  double *dptr;
  double msec;
  long ierr=0;

  if (vnum == -1) { *dval=0.0; return(ierr); }
  switch (VLIST[vnum].cdfvartype) {
    case CDF_REAL4 : ierr=1; break;
    case CDF_FLOAT : ierr=1; break;
    case CDF_REAL8 : dptr=(double *)VLIST[vnum].dataptr;
                     msec= *dptr;
                     break;
    case CDF_DOUBLE: dptr=(double *)VLIST[vnum].dataptr;
                     msec= *dptr;
                     break;
    case CDF_EPOCH : dptr=(double *)VLIST[vnum].dataptr;
                     msec= *dptr;
                     break;
    case CDF_INT4  : ierr=1; break;
    case CDF_UINT4 : ierr=1; break;
    case CDF_INT2  : ierr=1; break;
    case CDF_UINT2 : ierr=1; break;
    case CDF_BYTE  : ierr=1; break;
    case CDF_CHAR  : ierr=1; break;
    case CDF_UCHAR : ierr=1; break;
    default        : ierr=1; break;
  }
  if (ierr == 0) *dval=msec; /* validate the msec value */
  return(ierr);
}

/**************************************************************************
*                            Get_BaseEpoch                                *
**************************************************************************/
long Get_BaseEpoch(dval)
double *dval;
{
  double dhour,dmin,dsec,dmsec,depoch;
  long lyear,lmonth,lday;
  long ierr = 0;

  /* Compute an Epoch time based of values contained in Epoch operands */
  lyear  = EpALG.operand[0];
  lmonth = EpALG.operand[1];
  lday   = EpALG.operand[2];
  dhour  = (double)EpALG.operand[3];
  dmin   = (double)EpALG.operand[4];
  dsec   = (double)EpALG.operand[5];
  dmsec  = (double)EpALG.operand[6];
  depoch = (double)julian_date(lyear,lmonth,lday) - 1721060;
  depoch = (depoch *   24.0) + dhour;
  depoch = (depoch *   60.0) + dmin;
  depoch = (depoch *   60.0) + dsec;
  depoch = (depoch * 1000.0) + dmsec;
  *dval  = depoch;
  return(ierr);
}
/**************************************************************************
*                         Compute_BaseEpoch                               *
**************************************************************************/
long Compute_Epoch(dval)
double *dval;
{
  double dhour,dmin,dsec,dmsec,depoch;
  long lyear,lmonth,lday;
  long ierr = 0;

  /* Compute an Epoch time based on variables contained in Epoch operands */
  *dval = 0.0; /* initialize */
  ierr = Get_Year(EpALG.operand[0],&lyear);
  if (ierr==0) ierr=Get_Month(EpALG.operand[1],&lmonth);
  if (ierr==0) ierr=Get_Day(EpALG.operand[2],&lday);
  if (ierr==0) { /* got y/m/d that work with juliandate */
    if (lmonth == 0) Convert_DOY(lyear,&lmonth,&lday);
    ierr=Get_Hour(EpALG.operand[3],&dhour);
    if (ierr==0) ierr=Get_Minute(EpALG.operand[4],&dmin);
    if (ierr==0) ierr=Get_Second(EpALG.operand[5],&dsec);
    if (ierr==0) ierr=Get_MSecs(EpALG.operand[6],&dmsec);
    if (ierr==0) { /* got h/m/s/ms sans error */
      depoch = (double)julian_date(lyear,lmonth,lday) - 1721060;
      depoch = (depoch *   24.0) + dhour;
      depoch = (depoch *   60.0) + dmin;
      depoch = (depoch *   60.0) + dsec;
      depoch = (depoch * 1000.0) + dmsec;
      *dval  = depoch; /* set output value */
  } }
  return(ierr);
}

/**************************************************************************
*                           Parse_Filename                                *
**************************************************************************/
long Parse_Filename(fname)
char *fname;
{
  char ys[6],ms[6],ds[6],hs[6],mns[6],ss[6],mss[6];
  long i,lyear,lmonth,lday,lhour,lmin,lsec,lmsec,form[7];
  double depoch;
  long ierr = 0;

  /* initialize character substrings */
  strcpy(ys,"");  strcpy(ms,""); strcpy(ds,"");  strcpy(hs,"");
  strcpy(mns,""); strcpy(ss,""); strcpy(mss,"");

  /* set format array based on format supplied by user */
  switch (EpALG.operand[9]) {
  case 0 : form[0]=2; form[1]=2; form[2]=2; form[3]=0;
           form[4]=0; form[5]=0; form[6]=0; break;
  case 1 : form[0]=2; form[1]=2; form[2]=2; form[3]=2;
           form[4]=0; form[5]=0; form[6]=0; break;
  case 2 : form[0]=2; form[1]=2; form[2]=2; form[3]=2;
           form[4]=2; form[5]=0; form[6]=0; break;
  case 3 : form[0]=2; form[1]=2; form[2]=2; form[3]=0;
           form[4]=2; form[5]=2; form[6]=0; break;
  case 4 : form[0]=2; form[1]=0; form[2]=3; form[3]=0;
           form[4]=0; form[5]=0; form[6]=0; break;
  case 5 : form[0]=2; form[1]=0; form[2]=3; form[3]=2;
           form[4]=0; form[5]=0; form[6]=0; break;
  case 6 : form[0]=2; form[1]=0; form[2]=3; form[3]=2;
           form[4]=2; form[5]=0; form[6]=0; break;
  case 7 : form[0]=2; form[1]=0; form[2]=3; form[3]=2;
           form[4]=2; form[5]=2; form[6]=0; break;
  case 8 : form[0]=2; form[1]=0; form[2]=3; form[3]=0;
           form[4]=0; form[5]=0; form[6]=4; break;
  default: MSG_control(2,"ERROR: Unknown Filename Time format",0);
           MSG_control(2,"ERROR: Translation Aborted...",0); ierr=1;
           return(ierr);
  }

  /* Extract the substrings from the filename */
  for (i=0;i<EpALG.operand[6]-1;i++) *fname++;
  strncpy(ys ,fname,form[0]); for (i=0;i<form[0];i++) *fname++;
  strncpy(ms ,fname,form[1]); for (i=0;i<form[1];i++) *fname++;
  strncpy(ds ,fname,form[2]); for (i=0;i<form[2];i++) *fname++;
  strncpy(hs ,fname,form[3]); for (i=0;i<form[3];i++) *fname++;
  strncpy(mns,fname,form[4]); for (i=0;i<form[4];i++) *fname++;
  strncpy(ss ,fname,form[5]); for (i=0;i<form[5];i++) *fname++;
  strncpy(mss,fname,form[6]);

  /* Convert substrings into long integers */
  lyear=0;  if (form[0] != 0) i = sscanf(ys ,"%ld",&lyear);  if (i==0) ierr=1;
  lmonth=0; if (form[1] != 0) i = sscanf(ms ,"%ld",&lmonth); if (i==0) ierr=1;
  lday=0;   if (form[2] != 0) i = sscanf(ds ,"%ld",&lday);   if (i==0) ierr=1;
  lhour=0;  if (form[3] != 0) i = sscanf(hs ,"%ld",&lhour);  if (i==0) ierr=1;
  lmin=0;   if (form[4] != 0) i = sscanf(mns,"%ld",&lmin);   if (i==0) ierr=1;
  lsec=0;   if (form[5] != 0) i = sscanf(ss ,"%ld",&lsec);   if (i==0) ierr=1;
  lmsec=0;  if (form[6] != 0) i = sscanf(mss,"%ld",&lmsec);  if (i==0) ierr=1;
  if (ierr == 1) {
    MSG_control(2,"ERROR: Error during filename time conversions",0);
    MSG_control(2,"ERROR: Translation Aborted",0); return(ierr);
  }

  /* Ensure that year includes century */
  if (lyear < 100) {
    if (lyear >= 57) lyear = lyear + 1900;
    else lyear = lyear + 2000;
  }

  /* Calculate the epoch time from the time elements */
  depoch = computeEPOCH(lyear,lmonth,lday,lhour,lmin,lsec,lmsec);

  /* Set Base Epoch time */
  EpALG.BaseEp = depoch;
  return(ierr);
}

