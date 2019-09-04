#include "makecdf.h"

main(argc,argv)
int argc;
char *argv[];
{

  /* declare local variables */
  long i,j,ierr,icnt,ifile,iflag,OLidx,vnum,velem,snum,pvnum,CDFrec;
  float *rnum;
  double depoch;
  char pname;
  char lfileid[80]="";
  char newname[80]="";

  if (argc != 5) {
    printf("ERROR: Wrong  number of makeCDF command parameters!\n");
    printf("ERROR: Translation Aborted.\n");
    exit(EXIT_FAILURE);
  }

  /* Initialize Run time parameters */
  strcpy(RPARMS.infname,""); strcpy(RPARMS.ffdfname,"");
  strcpy(RPARMS.incdfname,""); RPARMS.progress=0L; RPARMS.debug=0L;
  RPARMS.sendtolog=0L; RPARMS.maxlogrecs=100L; RPARMS.sendtoterm=1L;
  RPARMS.haltateof=1L; RPARMS.numrecs=0L; RPARMS.autocase=1L;

  /* Initialize output CDF information */
  OUTCDF.outCDFid=0L; OUTCDF.encoding=0L; OUTCDF.CDFrec=0L;

  /* extract command line parameters from calling sequence */
  for (i=0;i<strlen(argv[0]);i++) pname = argv[0][i];
  for (i=0;i<strlen(argv[1]);i++) RPARMS.infname[i]=argv[1][i];
  RPARMS.infname[i]='\0';
  for (i=0;i<strlen(argv[2]);i++) RPARMS.ffdfname[i]=argv[2][i];
  RPARMS.ffdfname[i]='\0';
  for (i=0;i<strlen(argv[3]);i++) RPARMS.incdfname[i]=argv[3][i];
  RPARMS.incdfname[i]='\0';
  for (i=0;i<strlen(argv[4]);i++) RPARMS.outcdfname[i]=argv[4][i];
  RPARMS.outcdfname[i]='\0';

  ierr = Build_FLIST();                /* Construct list of input files    */
  if (ierr != 0) exit(EXIT_FAILURE);

  for (ifile=0;ifile < FLISTne;ifile++) {
    if (ifile == 0) {
      ierr=Import_FFD();               /* Import infile description */
      if (ierr != 0) exit(EXIT_FAILURE);
      ierr = Determine_EpGenerator();  /* Find var which enables Epoch calc*/
      if (ierr != 0) exit(EXIT_FAILURE);
    }
    MSG_control(1," ",0); MSG_control(1,"----Beginning Translation----",0);
    ierr = Generate_CDFskeleton();     /* Generate the CDF skeleton.     */
    if (ierr != 0) exit(EXIT_FAILURE);
    ierr = Build_OLIST();              /* Build the ordered input list.  */
    if (ierr != 0) exit(EXIT_FAILURE);
    if (ifile == 0) { 
      ierr = Match_Variables();        /* Map input vars to CDF variables*/
      if (ierr != 0) exit(EXIT_FAILURE);
      ierr = Allocate_Buffers();       /* Allocate buffer space for vars.*/
      if (ierr != 0) exit(EXIT_FAILURE);
    }
    ierr = Open_Infile(ifile);         /* Open Input File and in-buffer. */
    if (ierr != 0) exit(EXIT_FAILURE);
    ierr = Open_OutCDF();              /* Open the output CDF file       */
    if (ierr != 0) exit(EXIT_FAILURE);

    OLidx=0;
    do {
      ierr=0;  vnum  = OLIST[OLidx].vnum;  velem = OLIST[OLidx].velem;
      if (vnum == -1) { /* time to read a new record from the input file */
        if (OLIST[OLidx].rflag == TRUE) Read_Infile();
        /* set rflag for this eol to false if in header section of file */
        pvnum = OLIST[OLidx-1].vnum;
        if (VLIST[pvnum].header == TRUE) OLIST[OLidx].rflag = FALSE;
      }
      else {
        if (OLIST[OLidx].rflag == TRUE) {
          if (INFILE.format == 0) icnt=Parser1(vnum,velem);  /* freeform  */
          else {                                             /* fixedform */
            if (INFILE.dtype == 0) icnt=Parser2(vnum,velem); /* text      */
            else icnt=Parser3(vnum,velem);                   /* binary    */
          }
        }
        if (icnt == 0) { Read_Infile(); OLidx=OLISTne+1; }
        else {
          if (OLIST[OLidx].pflag == TRUE) { /* process variable */
            if (vnum == EpALG.EpGenerator) {
              ierr = Generate_Epoch(&depoch);
              if (ierr == 0) { /* increment CDF rec# and write to CDF */
                OUTCDF.CDFrec++;
                ierr = WriteEpochToCDF(depoch,OUTCDF.CDFrec);
              }
            }
            else if ((VLIST[vnum].bsubr != 0)&&(velem == 0)) {
              /* first element of first variable in new subrecord */
              snum = VLIST[vnum].bsubr-1; /* get subrecord number */
              if (SRECS[snum].countr != 0) {   /* not first iteration */
                if (SRECS[snum].delta > 0.0) { /* involved in time calcs */
                  depoch=depoch+((double)SRECS[snum].countr*SRECS[snum].delta);
                  OUTCDF.CDFrec++;
                  ierr = WriteEpochToCDF(depoch,OUTCDF.CDFrec);
                }
              }
              SRECS[snum].countr++;
            }
            if ((VLIST[vnum].cdfvarnum>=0)&&(velem==VLIST[vnum].nelems-1)) {
              /* Perform data quality checking */
              if (VLIST[vnum].autofill > 0) iflag = AutoFiller(vnum);
              if (iflag == 0) { /* data is not filler - continue QA checks */
                if ((VLIST[vnum].autovalids > 0)||
                    (VLIST[vnum].autoscales > 0)) ierr = AutoMinMax(vnum);
              }
              /* Write the data out to the CDF file */
              if (vnum < EpALG.EpGenerator) {
                ierr = WriteToCDF(vnum,OUTCDF.CDFrec+1); }
              if (vnum >= EpALG.EpGenerator) {
                ierr = WriteToCDF(vnum,OUTCDF.CDFrec);
              }
            }
          }
          if (VLIST[vnum].constant == TRUE) OLIST[OLidx].pflag = FALSE;
          if (VLIST[vnum].header == TRUE) OLIST[OLidx].rflag   = FALSE;
          if (VLIST[vnum].header == TRUE) OLIST[OLidx].pflag   = FALSE;
      } }

      OLidx = OLidx + 1;
      if (OLidx >= OLISTne) {
        OLidx=0; for (i=0;i<SRECSne;i++) SRECS[i].countr=0;
      }
    } while (!feof(INFILE.fptr));

    /* Set attribute values in CDF which were determined during Xlation   */
    ierr = Generate_LFILEID(lfileid); /* Determine ISTP formatted lfileid */
    if (ierr==0) ierr = Set_LFILEID(lfileid); /* Set ISTP lfileid gATTR   */
    ierr = Set_AutoVattrs(); /* optionally set min and max vATTRS*/

    /* Close the CDF, the input file, and deallocate memory */
    ierr = Cleanup_makeCDF(ifile);
    if (ierr == 0) {
      /* Rename the output CDF if auto-naming option is used */
#if defined SHORTY
      ierr = ShortName(lfileid);
#endif
      if (ierr == 0) ierr = AutoNamer(RPARMS.outcdfname,lfileid);
    }

    /* Close the logfile */
    if (LOGFILE.fptr != NULL) fclose(LOGFILE.fptr);
  }
  exit(EXIT_SUCCESS);
}
