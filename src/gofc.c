/* --------------------------------------------------------------------------
 * gofc.c:      Copyright (c) Mark P Jones 1991-1994.   All rights reserved.
 *              See goferite.h for details and conditions of use etc...
 *		Gofer Compiler version 1.01 February 1992
 *              Gofer version 2.30 March 1994
 *
 * Gofer->C main program
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "command.h"
#include "connect.h"
#include "errors.h"
#include <setjmp.h>
#include <ctype.h>

#define VERSION "1.02 (2.30a)"

Bool dumpScs = FALSE;			/* TRUE => output sc defns	  */

typedef FILE *Fp;
static  Fp   gofcFp  = 0;		/* for output to file		  */

/* --------------------------------------------------------------------------
 * Machine dependent code for Gofer compiler:
 * ------------------------------------------------------------------------*/

#define  MACHDEP_GOFC 1
#include "machdep.c"

/* --------------------------------------------------------------------------
 * Shared parts of user interface:
 * ------------------------------------------------------------------------*/

#include "commonui.c"

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static Void local loadCompile	Args((Void));
static Fp   local initOutput	Args((String));
static Void local initialise	Args((Int,String []));

/* --------------------------------------------------------------------------
 * Gofer entry point:
 * ------------------------------------------------------------------------*/

Main main Args((Int, String []));	/* now every func has a prototype  */

Main main(argc,argv)
int  argc;
char *argv[]; {
    CStackBase = &argc;                 /* Save stack base for use in gc   */

    /* The startup banner now includes my name.  Gofer is provided free of */
    /* charge.  I ask however that you show your appreciation for the many */
    /* hours of work involved by retaining my name in the banner.  Thanks! */

    printf("Gofer->C Version %s  Copyright (c) Mark P Jones 1992-1994\n\n",
	   VERSION);
    fflush(stdout);
    breakOn(TRUE);			/* enable break trapping	   */
    initialise(argc, argv);      	/* initialise compiler		   */

    if (dumpScs) {			/* produce script of sc defns for  */
	gofcFp = initOutput(".gsc");	/* debugging purposes		   */
	printf("[Writing supercombinators to \"%s\"]\n",outputFile);
	loadCompile();
	fprintf(gofcFp,"\n/* end of %s */\n",outputFile);
	fclose(gofcFp);
    }
    else {				/* produce C code as output	   */
	extern Void outputCode Args((FILE *,Name,String));
	Name mn;
        String topLevel = 0;
	loadCompile();
	gofcFp = initOutput(".c");

	mn = findName(findText("main"));/* check for main symbol	   */
	if (isNull(mn)) {
	    ERROR(0) "Program must include definition for \"main\" symbol"
	    EEND;
	}

	if (name(mn).defn==CFUN ||	/* check that definition is ok	   */
	    name(mn).defn==MFUN ||
	    name(mn).primDef    ||
	    isNull(name(mn).type)) {
	    ERROR(0) "Invalid definition for \"main\""
	    EEND;
	}

#if IO_DIALOGUE
	if (typeMatches(name(mn).type,typeDialogue))
	    topLevel = "dialogue";
#endif
#if IO_MONAD
	if (typeMatches(name(mn).type,typeProgIO))
	    topLevel = "iomonad";
#endif
	if (topLevel==0) {
	    ERROR(0) "Illegal typing for \"main\":" ETHEN
	    ERRTEXT  "\n*** inferred type  : "      ETHEN
	    ERRTYPE(name(mn).type);
#if IO_DIALOGUE
#if IO_MONAD
	    ERRTEXT  "\n*** does not match : Dialogue or IO ()\n"
#else
	    ERRTEXT  "\n*** does not match : Dialogue\n"
#endif
#else
#if IO_MONAD
	    ERRTEXT  "\n*** does not match : IO ()\n"
#else
	    ERRTEXT  "\n*** no suitable top-level available\n"
#endif
#endif
	    EEND;
	}

	printf("\nWriting C output file \"%s\":\n",outputFile);
	outputCode(gofcFp,mn,topLevel);
	fclose(gofcFp);
    }

    printf("[Leaving Gofer->C]\n");
    everybody(EXIT);
    exit(0);
    MainDone
}

static Void local loadCompile() {	/* load and compile source modules */
    Module i;
    Time   timeStamp;
    Long   fileSize;

    for (i=0; i<namesUpto; ++i) {	/* load and compile source modules */
	getFileInfo(scriptName[i], &timeStamp, &fileSize);
	if (i>0)
	    startNewModule();
        addScript(scriptName[i], fileSize);
	numScripts++;
    }
}

/* --------------------------------------------------------------------------
 * Determine name of output file:
 * ------------------------------------------------------------------------*/

static Fp local initOutput(suff)	/* find name for output file, open */
String suff; {				/* it and write header ...	   */
    Fp  fp = 0;
    int i;

    if (!outputFile) {			/* user specified name has priority*/
	String s,dot;

	if (projectLoaded && currProject)	/* use project name if poss*/
	    s = currProject;
	else
	    s = scriptName[namesUpto-1];	/* o/w use last script name*/

	outputFile = malloc(strlen(s)+strlen(suff)+1);
	if (!outputFile)
	    fatal("setOutputName");
	strcpy(outputFile,s);

        for (s=outputFile, dot=0; *s; ++s)	/* do something sensible   */
	    if (*s=='.')			/* with file extensions	   */
		dot = s;

#if !RISCOS
	if (dot && (strcmp(dot+1,"gp") == 0 || strcmp(dot+1,"prj") ==0 ||
		    strcmp(dot+1,"hs") == 0 || strcmp(dot+1,"lhs") ==0 ||
		    strcmp(dot+1,"gs") == 0 || strcmp(dot+1,"lgs") ==0 ||
		    strcmp(dot+1,"gof")== 0 || strcmp(dot+1,"has") ==0 ||
		    strcmp(dot+1,"lit")== 0 || strcmp(dot+1,"verb")==0 ||
		    strcmp(dot+1,"prelude")==0))
	    *dot = '\0';

	strcat(outputFile,suff);
#else
        if (dot) {
            char *prev = dot;
            while (prev>outputFile && *--prev!='.')
		;
	    if (*prev == '.')
		++prev;
	    if (namecmp(prev, "gp")      || namecmp(prev, "hs") 
                 || namecmp(prev, "gs")  || namecmp(prev, "gof")
		 || namecmp(prev, "lit") || namecmp(prev, "prj")
		 || namecmp(prev, "lhs") || namecmp(prev, "lgs")
		 || namecmp(prev, "has") || namecmp(prev, "verb")
		 || namecmp(prev, "prelude")) {
		strcpy(prev, suff+1);
		strcat(prev, dot);
	    }
	    else {
		strcat(outputFile,suff);
		outputFile[strlen(outputFile)-strlen(suff)] = '_'; /* No dot */
	    }
	}
        else {
	    strcat(outputFile,suff);
	    outputFile[strlen(outputFile)-strlen(suff)] = '_';	   /* No dot */
        }
#endif
    }

    if (!(fp=fopen(outputFile,"w"))) {		/* now try to open	   */
	ERROR(0) "Unable to open output file \"%s\" for writing",
		 outputFile
	EEND;
    }

    fprintf(fp,"/* %s\t\t\t\t%s *\n",outputFile,timeString());
    fprintf(fp," * This program produced by gofc %s from:\n",VERSION);

    if (projectLoaded && currProject)
        fprintf(fp," * Project file %s comprising:\n",currProject);

    for (i=0; i<namesUpto; i++)
        fprintf(fp," *\t%s\n",scriptName[i]);
    fprintf(fp," */\n\n");

    return fp;
}

/* --------------------------------------------------------------------------
 * Include our own version of output.c with ability to output sc defns
 * (This is a big hack, but it would probably be worth doing a proper
 * overhaul of the overall structure of Gofer before spending too much
 * time here.)
 * ------------------------------------------------------------------------*/

#define GOFC_OUTPUT
#include "output.c"

/* --------------------------------------------------------------------------
 * Initialisation, interpret command line args and read prelude:
 * ------------------------------------------------------------------------*/

struct options toggle[] = {
    {'d', "Show dictionary values in output exprs",&showDicts},
    {'g', "Print no. cells recovered after gc",	   &gcMessages},
    {'c', "Test conformality for pattern bindings",&useConformality},
    {'l', "Treat input files as literate scripts", &literateScripts},
    {'e', "Warn about errors in literate scripts", &literateErrors},
    {'i', "Apply fromInteger to integer literals", &coerceNumLiterals},
    {'o', "Optimise use of (&&) and (||)",	   &andorOptimise},
    {'u', "Catch ambiguously typed top-level vars",&catchAmbigs},
    {'a', "Use any evidence, not nec. best",	   &anyEvidence},
    {'E', "Fail silently if evidence not found",   &silentEvFail},
    {'.', "Print dots to show progress",	   &useDots},
    {'1', "Overload singleton list notation",	   &overSingleton},
    {'D', "Output .gsc file for debugging",	   &dumpScs},
    {0,   0,					   0}
};

static Void local initialise(argc,argv)/* compiler initialisation	   */
Int    argc;
String argv[]; {
    Module i;
    String proj = 0;

    scriptFile	  = 0;
    numScripts	  = 0;
    namesUpto	  = 1;
    scriptName[0] = strCopy(fromEnv("GOFER",STD_PRELUDE));

    for (i=1; i<argc; ++i)		/* process command line arguments  */
	if (strcmp(argv[i],"+")==0 && i+1<argc)
	    if (proj) {
		ERROR(0) "Multiple project filenames on command line"
		EEND;
	    }
	    else
		proj = argv[++i];
	else
	    addScriptName(argv[i]);

    everybody(INSTALL);
    if (proj) {
	if (namesUpto>1)
	    fprintf(stderr,
		    "\nUsing project file, ignoring additional filenames\n");
	loadProject(strCopy(proj));
    }
}

Void errHead(l) 		       /* print start of error message	   */
Int l; {
    failed();			       /* failed to reach target ...	   */
    fprintf(errorStream,"ERROR");

    if (scriptFile) {
	fprintf(errorStream," \"%s\"", scriptFile);
	if (l) fprintf(errorStream," (line %d)",l);
    }
    fprintf(errorStream,": ");
    fflush(errorStream);
}

Void errFail() {		       /* terminate error message	   */
    fprintf(errorStream,"\nAborting compilation\n");
    fflush(errorStream);
    exit(1);
}

Void errAbort() {			/* altern. form of error handling  */
    failed();				/* used when suitable error message*/
    errFail();
}

Void internal(msg)			/* handle internal error	   */
String msg; {
    fatal(msg);				/* treat as fatal condition	   */
}

Void fatal(msg)				/* handle fatal error	 	   */
String msg; {
    fflush(stdout);
    printf("\nINTERNAL ERROR: %s\n",msg);
    everybody(EXIT);
    exit(1);
}

sigHandler(breakHandler) {	       /* respond to break interrupt	   */
    breakOn(TRUE);
    printf("{Interrupted!}\n");
    everybody(BREAK);
    fflush(stdout);
    errAbort();
    sigResume;/*NOTREACHED*/
}

/*-------------------------------------------------------------------------*/
