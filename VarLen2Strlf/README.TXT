Some files transferred from the Vax are in variable length format.  The program 
attempts to convert a VMS variable length text file to a stream_lf text file 
useable on non-VMS systems.  It will replace the record length characters in 
the input file with line feeds.  If the -v options is used, an extra 2 bytes 
are skipped. The original file is overwritten.

Compile with

gcc -o VarLen2Strlf main.c