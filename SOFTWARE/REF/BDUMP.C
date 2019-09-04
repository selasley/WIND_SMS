
/*
**++
**  FACILITY:  BDUMP
**
**  MODULE DESCRIPTION:
**
**      This program performs a byte-oriented dump of a disk file.  Unlike the
**	VMS DUMP command, though, it dumps bytes from left to right across the
**	page (screen), not right to left.
**
**	It has command line options available to modify its behaviour.  The
**	options are:
**
**	recl=n		Specifies the length of records to be dumped; defaults
**			to 512 bytes (1 disk block).
**
**	start=n		Controls which record (of length specified by the recl=
**			option) is to be dumped first.  The record number is
**			1 origin.
**
**	count=n		Controls how many records to dump, each of which has a
**			length specified by the recl= option
**
**	out=file	Specifies a disk file to which the dump output will be
**			written.  By default dump output is written to the
**			screen.
**
**  AUTHORS:
**
**      Larry Bleau
**
**  CREATION DATE:  7-SEP-1995
**
**  DESIGN ISSUES:
**
**      {@tbs@}
**
**
**  MODIFICATION HISTORY:
**
**--
*/


/*
**
**  INCLUDE FILES
**
*/

#include <stdio.h>
#include <string.h>


/*
**
**  MACRO DEFINITIONS
**
*/

#define BYTES_PER_LINE	32



/*
**++
**  MODULE NAME:    dump_bytes
**
**  FUNCTIONAL DESCRIPTION:
**
**      This procedure writes a number of bytes, formatted into hex values, to
**	an output file.  A pointer to the first byte, the byte count, the output
**	file pointer, and a heading are passed as parameters.
**
**  FORMAL PARAMETERS:
**
**      buffer:
**          unsigned char *	A char pointer to the first byte to be dumped.
**       
**      nbytes:
**          long int		A count of the number of bytes to be dumped in
**				this call.
**	 
**      dumpf:
**          FILE *		A pointer to the output file to which the
**				formatted bytes are to be written.
**	 
**      heading:
**          char *		A pointer to a character string to be written
**				to dumpf just before the dumped bytes.
**
**  RETURN VALUE:
**
**      None
**
**  SIDE EFFECTS:
**
**      None
**
**  DESIGN:
**
**      The macro BYTES_PRE_LINE is used to determine the number of bytes to be
**	written on a single output line.
**	Each line of output will contain:
**	    the offset of the first byte of the line in decimal
**	    the offset of the first byte of the line in hex
**	    BYTES_PRE_LINE bytes dumped as 2-digit zero-filled hex numbers
**	The hex values will be dumped without interveaning blanks.
**
**--
*/
void dump_bytes (unsigned char *buffer,
		 long int nbytes,
		 FILE *dumpf,
		 char *heading) 
{
    int offset = 0, count, i;
    unsigned char *p = buffer;

    if (nbytes <= 0) return;
    fprintf (dumpf, "\n%s\n\n", heading);

    while (nbytes > 0)
    {
    	fprintf (dumpf, "%4d  %04x(x)  ", offset, offset);
	count = nbytes > BYTES_PER_LINE ? BYTES_PER_LINE : nbytes;
	nbytes -= count;
	offset += count;
	while (count-- > 0)
	{
	    fprintf (dumpf, "%02x", *p++);
	}
	fprintf (dumpf, "\n");
    }
}


/*
**++
**  MODULE NAME:    main
**
**  FUNCTIONAL DESCRIPTION:
**
**      This is the main procedure for the BDUMP program.
**
**  FORMAL PARAMETERS:
**
**      {@subtags@}
**
**  RETURN VALUE:
**
**      A status code of 1 is returned, which indicates normal completion on a
**	VMS system.
**
**  SIDE EFFECTS:
**
**      None
**
**  DESIGN:
**
**	Parameters will be read from the command line and used to control the
**	program.
**      If executed without any parameters the program usage will be printed.
**	Parameter format will be keyword=value, instead of -keyword (Unix) or
**	/keyword (VMS).
**	Dump in arbitrary chunks instead of only 512 bytes at a time, like VMS
**	DUMP does.  Allow user to specify record length (dump chunk size).
**	Use fread to read an arbitrary number of bytes.
**	Build a header line for each record, pass it to dump_bytes to print.
**	The output file is opened with special fopen options to give it carriage
**	return carriage control and variable length record attributes.
**
**
**  CALLING SEQUENCE:
**   
**	bdump filename [out=file] [start=n] [count=n] [recl=n]
**   
**--
*/
int main (int argc, char *argv[])
{
    FILE *ifp, *dump_fp;
    long int reclen = 512, skip_count = 0, dump_count = 1000000, recnum = 1,
	     nbytes, i;
    char heading[100], *infile, outfile[100];
    unsigned char *buffer;

    if (argc < 2)
    {
    	printf ("Usage:\n");
	printf ("  bdump filename [out=file] [start=n] [count=n] [recl=n]\n");
	printf ("where\n");
	printf ("    out=    specifies output file; otherwise terminal will be used\n");
	printf ("    start=  starting record number (1 origin)\n");
	printf ("    count=  number of records to dump; default is 1000000\n");
	printf ("    recl=   record length; default is 512\n");
	printf ("    [] indicates optional argument.\n");
	return 1;
    }

    infile = argv[1];
    ifp = fopen (infile, "r");
    dump_fp = stdout;

    for (i=2;  i<argc;  i++)
    {
	if (strncmp (argv[i], "out=", 4) == 0)
	{
	    strcpy (outfile, argv[i]+4);
	    dump_fp = fopen (outfile, "w", "rfm=var", "rat=cr");
	}
	else if (strncmp (argv[i], "start=", 6) == 0)
	{
	    skip_count = atoi(argv[i]+6) - 1;
	}
	else if (strncmp (argv[i], "count=", 6) == 0)
	{
	    dump_count = atoi(argv[i]+6);
	}
	else if (strncmp (argv[i], "recl=", 5) == 0)
	{
	    reclen = atoi(argv[i]+5);
	}
    }

    buffer = (unsigned char *)malloc (reclen);
    if (buffer == (unsigned char *)NULL)
    {
    	printf("Error: memory allocation, requested %d bytes\n", reclen);
	return 1;
    }

    nbytes = 1;
    for (i=0;  i<skip_count && nbytes > 0;  i++, recnum++)
    {
    	nbytes = fread (buffer, 1, reclen, ifp);
    }

    if (nbytes <= 0)
    {
    	printf("Error: End of file encountered before dump started\n");
	fclose(dump_fp);
	return 1;
    }

    nbytes = fread (buffer, 1, reclen, ifp);
    for (i=0;  i<dump_count && nbytes > 0;  i++, recnum++)
    {
	sprintf (heading, "File: %-60s Record %4d", infile, recnum);
	dump_bytes(buffer, nbytes, dump_fp, heading);
    	nbytes = fread (buffer, 1, reclen, ifp);
    }

    if (nbytes <= 0)
    {
    	printf("** End of file **\n");
    }

    fclose(dump_fp);
    fclose(ifp);
    return 1;
}
