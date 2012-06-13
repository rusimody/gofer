/* --------------------------------------------------------------------------
 * gofer.c:	Copyright (c) Mark P Jones 1991-1994.   All rights reserved.
 *		See goferite.h for details and conditions of use etc...
 *		Gofer version 2.30 March 1994
 *
 * Command interpreter
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "command.h"
#include "connect.h"
#include "errors.h"
#include <setjmp.h>
#include <ctype.h>

/* --------------------------------------------------------------------------
 * Machine dependent code for Gofer interpreter:
 * ------------------------------------------------------------------------*/

#define  MACHDEP_GOFER 1
#include "machdep.c"

/* --------------------------------------------------------------------------
 * Shared parts of user interface:
 * ------------------------------------------------------------------------*/

#include "commonui.c"

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static Void   local initialise	      Args((Int,String []));
static Void   local interpreter       Args((Int,String []));
static Void   local menu	      Args((Void));
static Void   local guidance	      Args((Void));
static Void   local forHelp	      Args((Void));
static Void   local set		      Args((Void));
static Void   local changeDir	      Args((Void));
static Void   local load	      Args((Void));
static Void   local project           Args((Void));
static Void   local readScripts       Args((Int));
static Void   local whatFiles	      Args((Void));
static Void   local editor	      Args((Void));
static Void   local find	      Args((Void));
static Void   local runEditor         Args((Void));
static Void   local evaluator	      Args((Void));
static Void   local stopAnyPrinting   Args((Void));
static Void   local showtype	      Args((Void));
static Void   local info	      Args((Void));
static Void   local describe	      Args((Text));
static Void   local listNames	      Args((Void));

/* --------------------------------------------------------------------------
 * Local data areas:
 * ------------------------------------------------------------------------*/

static Time lastChange[NUM_MODULES];	/* Time of last change to file	   */
static Bool printing  = FALSE;		/* TRUE => currently printing value*/
static Bool addType;			/* TRUE => print type with value   */
static Bool showStats = TRUE;		/* TRUE => print stats after eval  */
static Bool listFiles = TRUE;		/* TRUE => list files after loading*/

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

    printf("Gofer Version 2.30a  Copyright (c) Mark P Jones 1991-1994\n\n");
    fflush(stdout);
    interpreter(argc,argv);
    printf("[Leaving Gofer]\n");
    everybody(EXIT);
    exit(0);
    MainDone
}

/* --------------------------------------------------------------------------
 * Initialisation, interpret command line args and read prelude:
 * ------------------------------------------------------------------------*/

static Void local initialise(argc,argv)/* interpreter initialisation	   */
Int    argc;
String argv[]; {
    Module i;
    String proj = 0;

    setLastEdit((String)0,0);
    lastEdit	  = 0;
    scriptFile	  = 0;
    numScripts	  = 0;
    namesUpto	  = 1;
    scriptName[0] = strCopy(fromEnv("GOFER",STD_PRELUDE));
    prompt	  = strCopy("?");
    repeatStr	  = strCopy("$$");

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
    readScripts(0);
}

/* --------------------------------------------------------------------------
 * Print Menu of list of commands:
 * ------------------------------------------------------------------------*/

static struct cmd cmds[] = {
 {":?",	   HELP},    {":type",   TYPEOF}, {":load",    LOAD},
 {":also", ALSO},    {":reload", RELOAD}, {":project", PROJECT},
 {":edit", EDIT},    {":find",   FIND},   {":names",   NAMES},
 {":set",  SET},     {":quit",   QUIT},   {":cd",      CHGDIR},
 {":!",    SYSTEM},  {":info",	 INFO},	  {":gc",      COLLECT},
 {"",      EVAL},
 {0,0}
};

static Void local menu() {
    printf("LIST OF COMMANDS:  Any command may be abbreviated to :c where\n");
    printf("c is the first character in the full name.\n\n");
    printf(":load <filenames>   load scripts from specified files\n");
    printf(":load               clear all files except prelude\n");
    printf(":also <filenames>   read additional script files\n");
    printf(":reload             repeat last load command\n");
    printf(":project <filename> use project file\n");
    printf(":edit <filename>    edit file\n");
    printf(":edit               edit last file\n");
    printf("<expr>              evaluate expression\n");
    printf(":type <expr>        print type of expression\n");
    printf(":?                  display this list of commands\n");
    printf(":set <options>      set command line options\n");
    printf(":set                help on command line options\n");
    printf(":names [pat]        list names currently in scope\n");
    printf(":info <names>       describe named objects\n");
    printf(":find <name>        edit file containing definition of name\n");
    printf(":!command           shell escape\n");
    printf(":cd dir             change directory\n");
    printf(":gc                 force garbage collection\n");
    printf(":quit               exit Gofer interpreter\n");
}

static Void local guidance() {
    printf("Command not recognised.  ");
    forHelp();
}

static Void local forHelp() {
    printf("Type :? for help\n");
}

/* --------------------------------------------------------------------------
 * Setting of command line options:
 * ------------------------------------------------------------------------*/

struct options toggle[] = {		/* List of command line toggles	   */
    {'s', "Print no. reductions/cells after eval", &showStats},
    {'t', "Print type after evaluation",	   &addType},
    {'d', "Show dictionary values in output exprs",&showDicts},
    {'f', "Terminate evaluation on first error",   &failOnError},
    {'g', "Print no. cells recovered after gc",	   &gcMessages},
    {'c', "Test conformality for pattern bindings",&useConformality},
    {'l', "Literate scripts as default",	   &literateScripts},
    {'e', "Warn about errors in literate scripts", &literateErrors},
    {'i', "Apply fromInteger to integer literals", &coerceNumLiterals},
    {'o', "Optimise use of (&&) and (||)",	   &andorOptimise},
    {'u', "Catch ambiguously typed top-level vars",&catchAmbigs},
    {'.', "Print dots to show progress",	   &useDots},
    {'w', "Always show which files loaded",	   &listFiles},
    {'1', "Overload singleton list notation",	   &overSingleton},
#ifdef TECH_TOGGLES
    {'a', "Use any evidence, not nec. best",	   &anyEvidence},
    {'E', "Fail silently if evidence not found",   &silentEvFail},
#endif
    {'k', "Show kind errors in full",		   &kindExpert},
    {0,   0,					   0}
};

static Void local set() {		/* change command line options from*/
    String s;				/* Gofer command line		   */

    if (s=readFilename()) {
	do {
	    if (s[0]=='+' || s[0]=='-')
		processOption(s);
	    else {
		ERROR(0) "Option string must begin with `+' or `-'"
		EEND;
	    }
	} while (s=readFilename());
    }
    else
	optionInfo();
}

/* --------------------------------------------------------------------------
 * Change directory command:
 * ------------------------------------------------------------------------*/

static Void local changeDir() {		/* change directory		   */
    extern int chdir Args((String));
    String s = readFilename();
    if (s && chdir(s)) {
	ERROR(0) "Unable to change to directory \"%s\"", s
	EEND;
    }
}

/* --------------------------------------------------------------------------
 * Loading and removal of script files:
 * ------------------------------------------------------------------------*/

static Void local load() {	       /* read filenames from command line */
    String s;			       /* and add to list of files waiting */
				       /* to be read			   */
    while (s=readFilename())
	addScriptName(s);
    readScripts(1);
}

static Void local project() {	       /* read list of file names from     */
    String s;			       /* project file			   */

    if ((s=readFilename()) || currProject) {
	if (!s)
	    s = strCopy(currProject);
	else if (readFilename()) {
	    ERROR(0) "Too many project files"
	    EEND;
	}
	else
	    s = strCopy(s);
    }
    else {
	ERROR(0) "No project filename specified"
	EEND;
    }
    loadProject(s);
    readScripts(1);
}

static Void local readScripts(first)	/* reread current list of scripts, */
Int first; {				/* loading everything after and	   */
    Module i;				/* including the first script which*/
    Time   timeStamp;			/* has been either changed or added*/
    Long   fileSize;

    for (i=first; i<namesUpto; ++i) {
	getFileInfo(scriptName[i], &timeStamp, &fileSize);

	if (i<numScripts && timeChanged(timeStamp,lastChange[i])) {
	    dropModulesFrom(i-1);	/* previously loaded file changed ?*/
	    numScripts = i;
	}

	if (i>=numScripts) {		/* new script file to be read ?	   */
	    timeSet(lastChange[i],timeStamp);
	    if (i>0)			/* no new module for prelude	   */
		startNewModule();
	    addScript(scriptName[i],fileSize);
	    numScripts++;
	}
    }

    if (listFiles)
	whatFiles();
    if (numScripts<=1)
	setLastEdit((String)0, 0);
}

static Void local whatFiles() {		/* list files in current session   */
    int i;
    printf("\nGofer session for:");
    if (projectLoaded)
	printf(" (project: %s)",currProject);
    for (i=0; i<numScripts; ++i)
	printf("\n%s",scriptName[i]);
    putchar('\n');
}

/* --------------------------------------------------------------------------
 * Access to external editor:
 * ------------------------------------------------------------------------*/

static Void local editor() {		/* interpreter-editor interface	   */
    String newFile  = readFilename();
    if (newFile) {
	setLastEdit(newFile,0);
	if (readFilename()) {
	    ERROR(0) "Multiple filenames not permitted"
	    EEND;
	}
    }
    runEditor();
    readScripts(1);			/* try to reload scripts after edit*/
}

static Void local find() {		/* edit file containing definition */
    String nm = readFilename();		/* of specified name		   */
    if (!nm) {
	ERROR(0) "No name specified"
	EEND;
    }
    else if (readFilename()) {
	ERROR(0) "Multiple names not permitted"
	EEND;
    }
    else {
	Name n;
	startNewModule();
	if (isNull(n = findName(findText(nm)))) {
	    ERROR(0) "No current definition for name \"%s\"", nm
	    EEND;
	}
	setLastEdit(scriptName[moduleThisName(n)],name(n).line);
	runEditor();
	readScripts(1);
    }
}

static Void local runEditor() {		/* run editor on file lastEdit at  */
    static char editorCmd[100];		/* line editLine		   */
    String edt;
    Int    l,f;

    if ((edt = fromEnv("EDITLINE",DEF_EDITLINE))
	    && lastEdit && lastLine && (l=substr("%d",edt))>=0
				    && (f=substr("%s",edt))>=0)
        if (l<f)
	    sprintf(editorCmd,edt,lastLine,lastEdit);
	else
	    sprintf(editorCmd,edt,lastEdit,lastLine);
    else
	if ((edt = fromEnv("EDITOR",DEF_EDITOR)))
	    if (lastEdit)
		sprintf(editorCmd,"%s %s",edt,lastEdit);
	    else
		sprintf(editorCmd,"%s",edt);
	else {
	    ERROR(0) "No editor specified in environment variable EDITOR"
	    EEND;
	}

    if (shellEsc(editorCmd)) {
	ERROR(0) "Editor terminated abnormally"
	EEND;
    }
}

/* --------------------------------------------------------------------------
 * Read and evaluate an expression:
 * ------------------------------------------------------------------------*/

static Void local evaluator() {        /* evaluate expr and print value    */
    Type type;

    scriptFile = 0;
    startNewModule();		       /* Enables recovery of storage	   */
				       /* allocated during evaluation	   */
    parseExp();
    checkExp();
    type = typeCheckExp();
    if (whatIs(isPolyType(type) ? monoTypeOf(type) : type)==QUAL) {
	ERROR(0) "Unresolved overloading" ETHEN
	ERRTEXT  "\n*** type        : "   ETHEN ERRTYPE(type);
	ERRTEXT  "\n*** translation : "   ETHEN ERREXPR(inputExpr);
	ERRTEXT  "\n"
	EEND;
    }
    compileExp();
    numCells	  = 0;
    numReductions = 0;
    numberGcs     = 0;
    printing	  = TRUE;
    if (typeMatches(type,typeString))
	outputString(stdout,graphForExp());
#if IO_DIALOGUE
    else if (typeMatches(type,typeDialogue))
	dialogue(graphForExp());
#endif
#if IO_MONAD
    else if (typeMatches(type,typeProgIO))
	ioExecute(graphForExp());
#endif
#ifdef LAMBDAVAR
    else if (typeMatches(type,typeProg))
	lvExecute(graphForExp());
#endif
#ifdef LAMBDANU
    else if (typeInstOf(type,typeLnProg))
	lnExecute(graphForExp());
#endif
    else {
	outputString(stdout,ap(ap(ap(namePrint,
				     mkInt(MIN_PREC)),
				     graphForExp()),
				     nameNil));
	if (addType) {
	    printf(" :: ");
	    printType(stdout,type);
	}
    }
    stopAnyPrinting();
}

static Void local stopAnyPrinting() {  /* terminate printing of expression,*/
    if (printing) {		       /* after successful termination or  */
	printing = FALSE;	       /* runtime error (e.g. interrupt)   */
	putchar('\n');
	if (showStats) {
#define plural(v)   v, (v==1?"":"s")
	    printf("(%lu reduction%s, ",plural(numReductions));
	    printf("%lu cell%s",plural(numCells));
	    if (numberGcs>0)
		printf(", %u garbage collection%s",plural(numberGcs));
	    printf(")\n");
#undef plural
	}
	fflush(stdout);
    }
}

/* --------------------------------------------------------------------------
 * Print type of input expression:
 * ------------------------------------------------------------------------*/

static Void local showtype() {	       /* print type of expression (if any)*/
    Cell type;

    startNewModule();		       /* Enables recovery of storage	   */
				       /* allocated during evaluation	   */
    parseExp();
    checkExp();
    type = typeCheckExp();
    printExp(stdout,inputExpr);
    printf(" :: ");
    printType(stdout,type);
    putchar('\n');
}

/* --------------------------------------------------------------------------
 * Enhanced help system:  print current list of scripts or give information
 * about an object.
 * ------------------------------------------------------------------------*/

static Void local info() {		/* describe objects		   */
    Int    count = 0;			/* or give menu of commands	   */
    String s;

    startNewModule();			/* for recovery of storage	   */
    for (; s=readFilename(); count++)
	describe(findText(s));
    if (count == 0)
	whatFiles();
}

static Void local describe(t)		/* describe an object		   */
Text t; {
    Tycon tc = findTycon(t);
    Class cl = findClass(t);
    Name  nm = findName(t);

    if (nonNull(tc)) {			/* as a type constructor	   */
	Type t = tc;
	Int  i;
	for (i=0; i<tycon(tc).arity; ++i)
	    t = ap(t,mkOffset(i));
	printf("-- type constructor");
	if (kindExpert) {
	    printf(" with kind ");
	    printKind(stdout,tycon(tc).kind);
	}
	putchar('\n');
	switch (tycon(tc).what) {
	    case SYNONYM      : printf("type ");
				printType(stdout,t);
				printf(" = ");
				printType(stdout,tycon(tc).defn);
				break;

	    case DATATYPE     : {   List cs = tycon(tc).defn;
				    printf("data ");
				    printType(stdout,t);
				    if (nonNull(cs))
					printf("\n\n-- constructors:");
				    for (; nonNull(cs); cs=tl(cs)) {
					putchar('\n');
					printExp(stdout,hd(cs));
					printf(" :: ");
					printType(stdout,name(hd(cs)).type);
				    }
				}
				break;

	    case RESTRICTSYN  : printf("type ");
				printType(stdout,t);
				printf(" = <restricted>");
				break;
	}
	printf("\n\n");
    }

    if (nonNull(cl)) {			/* as a class			   */
	List ins = class(cl).instances;
	if (isPair(class(cl).sig) && fst(class(cl).sig)==STAR
				  && isNull(snd(class(cl).sig)))
	    printf("-- type class");
	else {
	    printf("-- constructor class");
	    if (kindExpert) {
		printf(" with arity ");
		printSig(stdout,class(cl).sig);
	    }
	}
	printf("\nclass ");
	if (nonNull(class(cl).supers)) {
	    printContext(stdout,class(cl).supers);
	    printf(" => ");
	}
	printPred(stdout,class(cl).head);
	if (nonNull(class(cl).members)) {
	    List ms = class(cl).members;
	    printf(" where");
	    do {
		printf("\n    ");
		printExp(stdout,hd(ms));
		printf(" :: ");
		printType(stdout,name(hd(ms)).type);
		ms = tl(ms);
	    } while (nonNull(ms));
	}
	putchar('\n');
	if (nonNull(ins))
	    printf("\n-- instances:\n");
	for (; nonNull(ins); ins=tl(ins)) {
	    printf("instance ");
	    if (nonNull(inst(hd(ins)).specifics)) {
		printContext(stdout,inst(hd(ins)).specifics);
		printf(" => ");
	    }
	    printPred(stdout,inst(hd(ins)).head);
	    putchar('\n');
	}
	putchar('\n');
    }

    if (nonNull(nm)) {			/* as a function/name		   */
	printExp(stdout,nm);
	printf(" :: ");
	if (nonNull(name(nm).type))
	    printType(stdout,name(nm).type);
	else
	    printf("<unknown type>");
	switch (name(nm).defn) {
	    case MFUN : printf("   -- class member");
			break;
	    case CFUN : printf("   -- data constructor");
			break;
	}
	if (name(nm).primDef)
	    printf("   -- primitive");
	printf("\n\n");
    }

    if (isNull(tc) && isNull(cl) && isNull(nm)) {
	printf("Unknown reference `%s'\n",textToStr(t));
    }
}

/* --------------------------------------------------------------------------
 * List all names currently in scope:
 * ------------------------------------------------------------------------*/

static Void local listNames() {		/* list names matching optional pat*/
    String pat   = readFilename();
    List   names = NIL;
    Int    width = getTerminalWidth() - 1;
    Int    count = 0;
    Int    termPos;

    if (pat)				/* First gather names to list	   */
	do
	    names = addNamesMatching(pat,names);
	while (pat=readFilename());
    else
	names = addNamesMatching((String)0,names);

    if (isNull(names)) {		/* Then print them out		   */
	ERROR(0) "No names selected"
	EEND;
    }
    for (termPos=0; nonNull(names); names=tl(names)) {
	String s = textToStr(name(hd(names)).text);
	Int    l = strlen(s);
	if (termPos+1+l>width) {
	    putchar('\n');
	    termPos = 0;
	}
	else if (termPos>0) {
	    putchar(' ');
	    termPos++;
	}
	printf("%s",s);
	termPos += l;
	count++;
    }
    printf("\n(%d names listed)\n", count);
}

/* --------------------------------------------------------------------------
 * main read-eval-print loop, with error trapping:
 * ------------------------------------------------------------------------*/

static jmp_buf catch_error;	       /* jump buffer for error trapping   */

#ifdef WANT_TIMER
#include "timer.c"
#endif

static Void local interpreter(argc,argv)/* main interpreter loop	   */
Int    argc;
String argv[]; {
    Int errorNumber = setjmp(catch_error);

    breakOn(TRUE);		       /* enable break trapping 	   */
    if (numScripts==0) {	       /* only succeeds on first time,	   */
	if (errorNumber)	       /* before prelude has been loaded   */
	    fatal("Unable to load prelude");
	initialise(argc,argv);
	forHelp();
    }

    for (;;) {
	everybody(RESET);		/* reset to sensible initial state */
	dropModulesFrom(numScripts-1);	/* remove partially loaded scripts */
					/* not counting prelude as a module*/
	consoleInput(prompt);
#ifdef WANT_TIMER
        updateTimers();
#endif
	switch (readCommand(cmds, (Char)':', (Char)'!')) {
	    case EDIT	: editor();
			  break;
	    case FIND   : find();
			  break;
	    case LOAD	: clearProject();
			  forgetScriptsFrom(1);
			  load();
			  break;
	    case ALSO   : clearProject();
			  forgetScriptsFrom(numScripts);
			  load();
			  break;
	    case RELOAD : readScripts(1);
			  break;
	    case PROJECT: project();
			  break;
	    case EVAL	: evaluator();
			  break;
	    case TYPEOF : showtype();
			  break;
	    case NAMES  : listNames();
			  break;
	    case HELP	: menu();
			  break;
	    case BADCMD : guidance();
			  break;
	    case SET    : set();
			  break;
	    case SYSTEM : shellEsc(readLine());
			  break;
	    case CHGDIR : changeDir();
			  break;
	    case INFO   : info();
			  break;
	    case QUIT	: return;
	    case COLLECT: garbageCollect();
			  printf("Garbage collection recovered %d cells\n",
				 cellsRecovered);
			  break;
	    case NOCMD	: break;
	}
#ifdef WANT_TIMER
        updateTimers();
        printf("Elapsed time (ms): %ld (user), %ld (system)\n",
                 millisecs(userElapsed), millisecs(systElapsed));
#endif
    }
}

Void errHead(l) 		       /* print start of error message	   */
Int l; {
    failed();			       /* failed to reach target ...	   */
    stopAnyPrinting();
    fprintf(errorStream,"ERROR");

    if (scriptFile) {
	fprintf(errorStream," \"%s\"", scriptFile);
	setLastEdit(scriptFile,l);
	if (l) fprintf(errorStream," (line %d)",l);
	scriptFile = 0;
    }
    fprintf(errorStream,": ");
    fflush(errorStream);
}

Void errFail() {			/* terminate error message and	   */
    putc('\n',errorStream);		/* produce exception to return to  */
    fflush(errorStream);		/* main command loop		   */
    longjmp(catch_error,1);
}

Void errAbort() {			/* altern. form of error handling  */
    failed();				/* used when suitable error message*/
    stopAnyPrinting();			/* has already been printed	   */
    errFail();
}

Void internal(msg)			/* handle internal error 	   */
String msg; {
    failed();
    stopAnyPrinting();
    fprintf(errorStream,"INTERNAL ERROR: %s\n",msg);
    fflush(errorStream);
    longjmp(catch_error,1);
}

Void fatal(msg)				/* handle fatal error		   */
String msg; {
    fflush(stdout);
    printf("\nFATAL ERROR: %s\n",msg);
    everybody(EXIT);
    exit(1);
}

sigHandler(breakHandler) {		/* respond to break interrupt	   */
    breakOn(TRUE);
    printf("{Interrupted!}\n");
    everybody(BREAK);
    failed();
    stopAnyPrinting();
    fflush(stdout);
    longjmp(catch_error,1);
    sigResume;/*NOTREACHED*/
}

/*-------------------------------------------------------------------------*/
