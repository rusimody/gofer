#include <stdio.h>

/* Crude page formatter:
 *
 *
 * .pa          start new page
 * .co xxx      comment
 * .ti title    set title string
 * .TI title    set and print title string
 * .st title    set section title
 * .ST title    set and print section title
 * .in file     include contents of file
 * .op          page headings off
 * .po          page headings on
 * .pn n        set page number (numbers <=0 don't print)
 * .pl n        set page length
 * .ht n        blank lines above header line
 * .hb n        blank lines below header line
 * .ft n        blank lines above page number line
 * .fb n        blank lines below page number line
 * .ss n        minimum number of lines on page before new section begins
 * .cc n        start new page unless at least n clear lines on page
 * .mi n        only print pages numbered >= n
 * .mx n        print only pages numbered <= n (if max<=min, all pages printed)
 * .off         hide output
 * .on          show output
 * .>stdout	send following output to stdout
 * .>file	send following output to named file
 */

#define MAXLINE   1024
#define MAXINPUT  32

int physicalLines   = 66;
int headerLines1    = 2;
int headerLines2    = 2;
int footerLines1    = 2;
int footerLines2    = 2;
int suppressHeaders = 0;
int contentsReqd    = 0;
int lastSect        = 0;
int sectSkip        = 5;
int minPage         = 0;
int maxPage         = 0;
int outputOn        = 1;

int sofarThisPage;
int linesThisPage;
int pageNumber;

FILE *outputFile = 0;
FILE *inputs[MAXINPUT];
int  inputLevel=0;
#define in inputs[inputLevel]

includeFile(n)
char *n; {
    if (inputLevel<MAXINPUT-1) {
	inputs[++inputLevel] = fopen(n,"r");
	if (!in) {
	    fprintf(stderr,"Cannot open file \"%s\"\n",n);
	    inputLevel--;
	}
    }
    else
	fprintf(stderr,"Too many included files\n");
}

int readLine(s)
char *s; {
    int i = 0;
    int c;

    while (feof(in)) {
	if (inputLevel>0) {
	    fclose(in);
	    inputLevel--;
	}
	else
	    return 0;
    }

    while ((c=getc(in))!=EOF && c!='\n')
	if (c!=26 && c!='\r')
	    if (i<MAXLINE)
		s[i++] = c;
	    else
		fprintf(stderr,"input line too long\n");
    s[i] = '\0';
    return 1;
}

char title[MAXLINE+1];
char sectitle[MAXLINE+1];

setSecTitle(t)
char *t; {

    strcpy(sectitle,t);

    if (contentsReqd) {
        int i;
        int thisSect = atoi(t);
        if ((lastSect>0 && thisSect!=lastSect) || thisSect==0)
	    fprintf(stderr,"\n.cc 2\n");
        fprintf(stderr,"    ");
        for (i=0; *t; ++i)
	    fputc(*t++, stderr);
        for (; i<66; i++)
	    fputc(((i&1) ? '.' : ' '), stderr);
	fprintf(stderr,"%3d\n",pageNumber);
	lastSect = thisSect;
    }

    if (sofarThisPage + sectSkip >= linesThisPage)
	clearPage();
}

outputLine(s)
char *s; {
    if (!contentsReqd && outputOn &&
        (maxPage<=minPage ||
         (pageNumber>=minPage && pageNumber<=maxPage)))
	if (outputFile) {
	    fputs(s,outputFile);
	    fputc('\n',outputFile);
	}
	else
	    puts(s);
    sofarThisPage++;
}

startPage() {
    if (suppressHeaders)
        linesThisPage = physicalLines;
    else {
	static char buffer[MAXLINE+1];
        int i;
        for (i=0; i<headerLines1; i++)
	    outputLine("");

	sprintf(buffer,"%-20s%50s",title,sectitle);
        outputLine(buffer);

        for (i=0; i<headerLines2; i++)
	    outputLine("");
	linesThisPage = physicalLines - headerLines1
                                      - 1
                                      - headerLines2
                                      - footerLines1
				      - 1
                                      - footerLines2;
    }
    sofarThisPage = 0;
}

clearPage() {
    int i;
    while (sofarThisPage<linesThisPage)
	outputLine("");
    if (!suppressHeaders) {
	static char buffer[MAXLINE+1];

        for (i=0; i<footerLines1; i++)
	    outputLine("");

        if (pageNumber>0) {
            sprintf(buffer,"%38s%d","",pageNumber);
	    outputLine(buffer);
	}
        else
	    outputLine("");

        for (i=0; i<footerLines2; i++)
	    outputLine("");
    }
    sofarThisPage = 0;
    pageNumber++;
}

startDocument() {
    inputLevel      = 0;
    in              = stdin;
    pageNumber      = 1;
    sofarThisPage   = 0;
    linesThisPage   = physicalLines;
    suppressHeaders = 0;
    lastSect        = 0;
    strcpy(title,"");
    strcpy(sectitle,"");
}

endDocument() {
    clearPage();
    if (outputFile)
	fclose(outputFile);
    outputFile = 0;
}

int setParam(line,s,v)
char *line;
char *s;
int  *v; {
    while (*s!='\0' && *s==*line)
	s++, line++;
    if (*s)
	return 0;
    *v = atoi(line);
    return 1;
}

int matchesFlag(line,s)  
char *line; 
char *s; {
    while (*s!='\0' && *s==*line) 
        s++, line++;
    return (*s == '\0');
}

main(argc,argv)
int argc;
char *argv[]; {
    char buffer[MAXLINE+1];
    int  condClear;

    for (argc--, argv++; argc>0; argc--, argv++)
	if (strcmp(*argv,"-c")==0)
	    contentsReqd = 1;
	else
	    fprintf(stderr,"unknown command line argument \"%s\"\n",*argv);

    startDocument();

    while (readLine(buffer)) {
ugly:
	if (matchesFlag(buffer,".pa"))
	    clearPage();
        else if (matchesFlag(buffer,".on"))
	    outputOn = 1;
        else if (matchesFlag(buffer,".off"))
	    outputOn = 0;
        else if (matchesFlag(buffer,".co"))
	    continue;
        else if (matchesFlag(buffer,".ti "))
            strcpy(title,buffer+4);
	else if (matchesFlag(buffer,".TI ")) {
	    strcpy(title,buffer+4);
	    strcpy(buffer,title);
	    goto ugly;
	}
	else if (matchesFlag(buffer,".st "))
	    setSecTitle(buffer+4);
	else if (matchesFlag(buffer,".ST ")) {
	    setSecTitle(buffer+4);
	    strcpy(buffer,sectitle);
	    goto ugly;
	}
	else if (matchesFlag(buffer,".>stdout")) {
	    if (outputFile)
		fclose(outputFile);
	    outputFile = 0;
	}
	else if (matchesFlag(buffer,".>")) {
	    if (outputFile)
		fclose(outputFile);
	    outputFile = fopen(buffer+2,"w");
	}
	else if (matchesFlag(buffer,".in "))
	    includeFile(buffer+4);
	else if (matchesFlag(buffer,".po"))
	    suppressHeaders = 0;
	else if (matchesFlag(buffer,".op"))
	    suppressHeaders = 1;
        else if (setParam(buffer,".pn",&pageNumber)    ||
                 setParam(buffer,".pl",&physicalLines) ||
                 setParam(buffer,".ht",&headerLines1)  ||
                 setParam(buffer,".hb",&headerLines2)  ||
                 setParam(buffer,".ft",&footerLines1)  ||
                 setParam(buffer,".fb",&footerLines2)  ||
                 setParam(buffer,".ss",&sectSkip)      ||
                 setParam(buffer,".mi",&minPage)       ||
                 setParam(buffer,".mx",&maxPage))
	    continue;

        else if (setParam(buffer,".cc",&condClear)) {
	    if (sofarThisPage + condClear >= linesThisPage)
		clearPage();
	}

	else if (*buffer ||
                 suppressHeaders ||
                 (sofarThisPage>0 && sofarThisPage<linesThisPage)) {

	    /* ignore blank lines if they occur at the top of the page with
             * headers not suppressed
	     */

	    if (sofarThisPage>=linesThisPage)	/* finish off any completed*/
		clearPage();			/* page			   */

	    if (sofarThisPage==0)		/* maybe start a new page  */
		startPage();
	    outputLine(buffer);		/* then print the line	   */
	}
    }

    endDocument();
    exit(0);
}
