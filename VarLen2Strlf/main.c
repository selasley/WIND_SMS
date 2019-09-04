#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>

int convertToStreamlf(char *filePath, FILE *tmpFile, int isVFC);
FILE *makeTempFile(char *tmpFileName, int *tmpFileDes);
int replaceVMSFile(char *vmsFilePath, FILE *tmpFile, const struct stat vmsFileStat);
int main (int argc, const char * argv[]);

int main (int argc, const char * argv[]) {
    struct stat fileStat;
    int vfc = 0;
    int firstFile = 1;
    long junk;
    char tmpFileName[18];
    int  tmpFileDes;
    FILE *tmpFileS;
    
    if (argc < 2) {
        printf("\nUsage: VarLen2Strlf [-v] path_to_VMS_variable_length_text_file\n");
        printf("Options: -v indicates this is a VFC file with a 2 byte header like a lot of batch job log files.\n");
        printf("Notes: The program attempts to convert a VMS variable length text file to a stream_lf text file useable on non-VMS systems.\n");
        printf("       It will replace the record length characters in the input file with line feeds.  If the -v options is used, an extra 2 bytes are skipped.\n");
        printf("       The original file is overwritten.\n\n");
        exit(0);
    }
    if (strcmp(argv[1], "-v") == 0) {
        vfc = 1;
        firstFile++;
    }
    tmpFileS = makeTempFile(tmpFileName, &tmpFileDes);
    for (int i = firstFile; i < argc; i++) {
        if (lstat(argv[i], &fileStat) == 0) {
            if (S_ISDIR(fileStat.st_mode) == 0) {
                // "reset" the temporary file used for conversion
                rewind(tmpFileS);
                junk = ftruncate(tmpFileDes, 0L);
                // attempt to convert file and replace the original if conversion succeeds
                if (convertToStreamlf((char *)argv[i], tmpFileS, vfc) == 0) {
                    replaceVMSFile((char *)argv[i], tmpFileS, fileStat);
                }
            }
        }
    }
    fclose(tmpFileS);
    unlink(tmpFileName);
    return 0;
}

int convertToStreamlf(char *filePath, FILE *tmpFile, int isVFC) {
/* converts the file at filePath to stream_lf in the file tmpFile */
    char    text[1024];
    long    bytesWritten;
    short   offset, junk;
    size_t  stringsRead;
    FILE    *vmsFile;
    
    printf("Translating %s\n", filePath);
    vmsFile = fopen(filePath, "r");
    stringsRead = fread(&offset, 2, 1, vmsFile);
    if (isVFC) {
        // read in and ignore an additional 2 bytes
        // if the VMS file contains only a line feed the while test below will fail and
        // the file will not be translated, but i don't care :)
        stringsRead = fread(&junk, 2, 1, vmsFile);
        offset -= 2;
    }
    while (stringsRead > 0) {
        if (offset > 1023) {
            printf("Line length of %d encountered.\n%s may not be a variable length VMS file.\nExiting without changing %s.\n", offset, filePath, filePath);
            fclose(vmsFile);
            return 1;
        }
        /* for some reason sometimes get FFFF for the offset followed by 
         0000's to pad to 512 byte boundaries */
        while ((offset <= 0) && (stringsRead == 1)) {
            stringsRead = fread(&offset, 2, 1, vmsFile);
        }
        if (offset > 0) {
            // lines with only a linefeed have a 0 offset
            // reading 0 bytes will cause fread to return 0
            stringsRead = fread(text, offset, 1, vmsFile); 
        }
        if (stringsRead == 1) {
            text[offset] = '\0';
            bytesWritten = fputs(text, tmpFile);
            bytesWritten = fputs("\n", tmpFile);
            //printf("%ld + %d\n", ftell(vmsFile), offset % 2);
            fseek(vmsFile, offset % 2, SEEK_CUR);
            //printf("%ld\n", ftell(vmsFile));
            stringsRead = fread(&offset, 2, 1, vmsFile);
            if (isVFC) {
                stringsRead = fread(&junk, 2, 1, vmsFile);
                offset -= 2;
            }
        }
    }
    fclose(vmsFile);
    return 0;
}

FILE *makeTempFile(char *tmpFileName, int *tmpFileDes) {
    char sfn[18] = "";
    FILE *sfp;
    int fd = -1;
    
    sfp = NULL;
    strlcpy(sfn, "/tmp/strlf.XXXXXX", sizeof sfn);
    if ((fd = mkstemp(sfn)) == -1 ||
        (sfp = fdopen(fd, "w+")) == NULL) {
        if (fd != -1) {
            unlink(sfn);
            close(fd);
        }
        fprintf(stderr, "%s: %s\n", sfn, strerror(errno));
        return (NULL);
    }
    strncpy(tmpFileName, sfn, 18);
    *tmpFileDes = fd;
    return (sfp);
}

int replaceVMSFile(char *vmsFilePath, FILE *tmpFile, const struct stat vmsFileStat) {
    char ch;
    struct timeval accModTimes[2];
    FILE *vmsFile;
    
    if((vmsFile=fopen(vmsFilePath, "wb")) == NULL) {
        printf("Cannot open vmsFileput file.\n");
        exit(1);
    }
    truncate(vmsFilePath, 0L);
    rewind(tmpFile);
    
    while(!feof(tmpFile)) {
        ch = getc(tmpFile);
        if(ferror(tmpFile)) {
            printf("Read Error");
            clearerr(tmpFile);
            break;
        } else {
            if(!feof(tmpFile)) putc(ch, vmsFile);
            if(ferror(vmsFile)) {
                printf("Write Error");
                clearerr(vmsFile);
                break;
            }
        }
    }
    fclose(vmsFile);
    TIMESPEC_TO_TIMEVAL(&accModTimes[0], &vmsFileStat.st_atimespec);
    TIMESPEC_TO_TIMEVAL(&accModTimes[1], &vmsFileStat.st_mtimespec);
    utimes(vmsFilePath, accModTimes);
    return 0;
}
