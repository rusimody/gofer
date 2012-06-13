/*-----------------------------------------------------------------------------
 * gofsplit.c   This program can be used to split the C code output produced
 *              by the Gofer compiler into a number of smaller C files, each
 *              of which can be compiled separately before linking to obtain
 *              the final executable program.
 *
 *              Why bother?  Some C compilers seem to die on really large
 *              input files, even if the function definitions themselves are
 *              reasonably sized.  Perhaps they try to do some fancy global
 *              optimization/data flow analysis...?
 *
 * For example: gofc + project              -- produces project.c as output
 *              gofsplit project.c part .c  -- produces part00.c, part01.c,...
 *              cc -O -c part00.c ...       -- compile individual files
 *              cc -o project part00.o ...  -- link to produce executable
 *                                             program
 *---------------------------------------------------------------------------*/

#include <stdio.h>

#define LONGLINE 1024
char theLine[LONGLINE+1];
int  foundEof=0;

FILE *newPart(prefix,partNo,suffix)
char *prefix;
int  partNo;
char *suffix; {
    static char outputFile[256];
    FILE *tmp;
    sprintf(outputFile,"%s%02d%s",prefix,partNo,suffix);
    tmp = fopen(outputFile,"w");
    if (!tmp) {
	fprintf(stderr,"Can't write %s\n",outputFile);
	exit(1);
    }
    printf("Writing %s\n",outputFile);
    if (partNo>0)
	fprintf(tmp,"#include \"gofc.h\"\n\n");
    return tmp;
}

int readLine(fp)
FILE *fp; {
    int c;
    int i = 0;
    if (!foundEof) {
	while ((c=fgetc(fp))!=EOF && c!='\n')
	    theLine[i++] = c;
	if (c==EOF)
	    foundEof = 1;
	else
	    theLine[i++] = c;
	theLine[i] = '\0';
    }
    return foundEof;
}

main(argc,argv)
int argc;
char *argv[]; {
    if (argc!=4)
	fprintf(stderr,"usage: gofsplit inputfile prefix suffix\n");
    else {
	char *inputFile = argv[1];
	char *prefix    = argv[2];
	char *suffix    = argv[3];
	int  partCount  = 0;
	FILE *ifile;
	FILE *ofile;

	ifile = fopen(inputFile,"r");
	if (!ifile)
	    fprintf(stderr,"Can't read %s\n",inputFile);
	else {
	    do {
		FILE *ofile=newPart(prefix,partCount++,suffix);
		int linecount=0;
		do {
		    if (readLine(ifile))
			break;
		    linecount++;
		    fprintf(ofile,"%s",theLine);
		} while (linecount<3000 || strcmp(theLine,"End\n")!=0);
		fclose(ofile);
	    } while (!foundEof);
	    fclose(ifile);
	}
    }
}

