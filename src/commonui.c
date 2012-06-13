/* --------------------------------------------------------------------------
 * commonui.c:  Copyright (c) Mark P Jones 1991-1994.   All rights reserved.
 *              See goferite.h for details and conditions of use etc...
 *              Gofer version 2.30 March 1994
 *
 * Parts of user interface common to both compiler and interpreter.
 * ------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static Void   local toggleSet	      Args((Char,Bool));
static Void   local togglesIn	      Args((Bool));
static Void   local optionInfo	      Args((Void));
static Void   local processOption     Args((String));
static Int    local argToInt	      Args((String *));

static Void   local loadProject       Args((String));
static Void   local clearProject      Args((Void));
static Void   local addScriptName     Args((String));
static Void   local addScript	      Args((String,Long));
static Void   local forgetScriptsFrom Args((Module));

static Void   local setLastEdit       Args((String,Int));

static Void   local failed	      Args((Void));

static String local strCopy	      Args((String));
static Int    local substr	      Args((String,String));

/* --------------------------------------------------------------------------
 * Local data areas:
 * ------------------------------------------------------------------------*/

static String scriptName[NUM_MODULES];	/* Script file names		   */
static Int    numScripts;		/* Number of scripts loaded	   */
static Int    namesUpto;		/* Number of script names set	   */

static String currProject = 0;		/* Name of current project file	   */
static Bool   projectLoaded = FALSE;	/* TRUE => project file loaded	   */
static String scriptFile;		/* Name of current script (if any) */

#if RISCOS
static Bool   useDots    = TRUE;	/* TRUE => use dots in progress    */
#else
static Bool   useDots    = FALSE;	/* TRUE => use dots in progress    */
#endif
static String lastEdit	 = 0;		/* Name of file to edit (if any)   */
static Int    lastLine	 = 0;		/* Editor line number (if possible)*/
static String prompt     = 0;		/* Prompt string (gofer only)	   */
static String outputFile = 0;		/* User spec. output file (gofc)   */

/* --------------------------------------------------------------------------
 * Command line options:
 * ------------------------------------------------------------------------*/

struct options {			/* command line option toggles	   */
    char   c;				/* table defined in main app.	   */
    String description;
    Bool   *flag;
};
extern struct options toggle[];

static Void local toggleSet(c,state)	/* Set command line toggle	   */
Char c;
Bool state; {
    Int i;
    for (i=0; toggle[i].c; ++i)
	if (toggle[i].c == c) {
	    *toggle[i].flag = state;
	    return;
	}
    ERROR(0) "Unknown toggle `%c'", c
    EEND;
}

static Void local togglesIn(state)	/* Print current list of toggles in*/
Bool state; {				/* given state			   */
    Int count = 0;
    Int i;
    for (i=0; toggle[i].c; ++i)
	if (*toggle[i].flag == state) {
	    if (count==0)
		putchar(state ? '+' : '-');
	    putchar(toggle[i].c);
	    count++;
	}
    if (count>0)
	putchar(' ');
}

static Void local optionInfo() {	/* Print information about command */
    static String fmts = "%-5s%s\n";	/* line settings		   */
    static String fmtc = "%-5c%s\n";
    Int    i;

    printf("TOGGLES: groups begin with +/- to turn options on/off resp.\n");
    for (i=0; toggle[i].c; ++i)
	printf(fmtc,toggle[i].c,toggle[i].description);

    printf("\nOTHER OPTIONS: (leading + or - makes no difference)\n");
    printf(fmts,"hnum","Set heap size (cannot be changed within Gofer)");
    printf(fmts,"pstr","Set prompt string to str");
    printf(fmts,"rstr","Set repeat last expression string to str");
#ifdef TECH_TOGGLES
    printf(fmts,"xnum","Set maximum depth for evidence search");
#endif

    printf("\nCurrent settings: ");
    togglesIn(TRUE);
    togglesIn(FALSE);
#ifdef TECH_TOGGLES
    printf("-h%d -p%s -x%d -r%s\n",heapSize,prompt,maxEvidLevel,repeatStr);
#else
    printf("-h%d -p%s -r%s\n",heapSize,prompt,repeatStr);
#endif
}

static Void local processOption(s)	/* process option string s	   */
String s; {
    Bool state = (s[0]=='+' ? TRUE : FALSE);

    while (*++s)
	switch (*s) {
	    case 'n' : if (s[1]) {
			   if (outputFile) free(outputFile);
			   outputFile = strCopy(s+1);
		       }
		       return;

	    case 'p' : if (s[1]) {
			   if (prompt) free(prompt);
			   prompt = strCopy(s+1);
		       }
		       return;

	    case 'r' : if (s[1]) {
			   if (repeatStr) free(repeatStr);
			   repeatStr = strCopy(s+1);
		       }
		       return;	

	    case 'h' : if (heapBuilt()) {
			   ERROR(0) "Cannot change heap size"
			   EEND;
		       }
		       heapSize = argToInt(&s);
		       if (heapSize<MINIMUMHEAP)
			   heapSize = MINIMUMHEAP;
		       else if (MAXIMUMHEAP && heapSize>MAXIMUMHEAP)
			   heapSize = MAXIMUMHEAP;
		       break;

#ifdef TECH_TOGGLES
	    case 'x' : maxEvidLevel = argToInt(&s);
		       break;
#endif

	    default  : toggleSet(*s,state);
		       break;
	}
}

static Int local argToInt(sp)		/* read integer from argument str  */
String *sp; {
    Int num = 0;
    while (isascii((*sp)[1]) && isdigit((*sp)[1])) {
	num = 10*num + (*(++*sp) - '0');
    }
    return num;
}

/* --------------------------------------------------------------------------
 * Loading project and script files:
 * ------------------------------------------------------------------------*/

static Void local loadProject(s)	/* Load project file		  */
String s; {
    clearProject();
    currProject = s;
    projInput(currProject);
    scriptFile = currProject;
    forgetScriptsFrom(1);
    while (s=readFilename())
	addScriptName(s);
    if (namesUpto<=1) {
	ERROR(0) "Empty project file"
	EEND;
    }
    scriptFile    = 0;
    projectLoaded = TRUE;
}

static Void local clearProject() {     /* clear name for current project   */
    if (currProject)
	free(currProject);
    currProject   = 0;
    projectLoaded = FALSE;
}

static Void local addScriptName(s)     /* add script name to list of files */
String s; {			       /* to be read in ...		   */
    if (s[0]=='-' || s[0]=='+')
	processOption(s);
    else if (namesUpto>=NUM_MODULES) {
	ERROR(0) "Too many script files (maximum of %d allowed)",
		 NUM_MODULES
	EEND;
    }
    else
	scriptName[namesUpto++] = strCopy(s);
}

static Void local addScript(fname,len) /* read single script file	   */
String fname;			       /* name of script file		   */
Long   len; {			       /* length of script file 	   */
    scriptFile = fname;

    printf("Reading script file \"%s\":\n",fname);
    setLastEdit(fname,0);

    parseScript(fname,len);	       /* process script file		   */
    checkDefns();
    if (numScripts==0)			/* initialisation to be done once  */
	everybody(PRELUDE);		/* prelude Tycons and Classes known*/
    typeCheckDefns();	
    compileDefns();

    scriptFile = 0;
}

static Void local forgetScriptsFrom(scno)/* remove scripts from system	   */
Module scno; {
    Module i;
    for (i=scno; i<namesUpto; ++i)
	if (scriptName[i])
	    free(scriptName[i]);
    dropModulesFrom(scno-1);		 /* don't count prelude as module  */
    namesUpto = scno;
    if (numScripts>0)
	numScripts = scno;
}

static Void local setLastEdit(fname,line)/* keep name of last file to edit */
String fname;
Int    line; {
    if (lastEdit)
	free(lastEdit);
    lastEdit = strCopy(fname);
    lastLine = line;
}

/* --------------------------------------------------------------------------
 * Display progress towards goal:
 * ------------------------------------------------------------------------*/

static Target currTarget;
static Bool   aiming = FALSE;
static Int    currPos;
static Int    maxPos;
static Int    charCount;

Void setGoal(what, t)		       /* Set goal for what to be t	   */
String what;
Target t; {
    currTarget = (t?t:1);
    aiming     = TRUE;
    if (useDots) {
	currPos = strlen(what);
	maxPos  = getTerminalWidth() - 1;
	printf("%s",what);
    }
    else
	for (charCount=0; *what; charCount++)
	    putchar(*what++);
    fflush(stdout);
}

Void soFar(t)			       /* Indicate progress towards goal   */
Target t; {			       /* has now reached t		   */
    if (useDots) {
	Int newPos = (Int)((maxPos * ((long)t))/currTarget);

	if (newPos>maxPos)
	    newPos = maxPos;

	if (newPos>currPos) {
	    do
		putchar('.');
	    while (newPos>++currPos);
	    fflush(stdout);
	}
	fflush(stdout);
    }
}

Void done() {			       /* Goal has now been achieved	   */
    if (useDots) {
	while (maxPos>currPos++)
	    putchar('.');
	putchar('\n');
	aiming = FALSE;
    }
    else
	for (; charCount>0; charCount--) {
	    putchar('\b');
	    putchar(' ');
	    putchar('\b');
	}
    fflush(stdout);
}

static Void local failed() {	       /* Goal cannot be reached due to    */
    if (aiming) {		       /* errors			   */
	aiming = FALSE;
	putchar('\n');
	fflush(stdout);
    }
}

/* --------------------------------------------------------------------------
 * Send message to each component of system:
 * ------------------------------------------------------------------------*/

Void everybody(what)		/* send command `what' to each component of*/
Int what; {			/* system to respond as appropriate ...    */
    machdep(what);		/* The order of calling each component is  */
    storage(what);		/* important for the INSTALL command	   */
    input(what);
    staticAnalysis(what);
    typeChecker(what);
    compiler(what);
    machine(what);
    builtIn(what);
}

/* --------------------------------------------------------------------------
 * Read value from environment variable:
 * ------------------------------------------------------------------------*/

String fromEnv(var,def)		/* return value of:	 		   */
String var;			/*     environment variable named by var   */
String def; {			/* or: default value given by def	   */
    String s = getenv(var);

    return (s ? s : def);
}

/* --------------------------------------------------------------------------
 * String manipulation routines:
 * ------------------------------------------------------------------------*/

static String local strCopy(s)	       /* make malloced copy of a string   */
String s; {
    if (s) {
	char *t,*r;
	if ((t=(char *)malloc(strlen(s)+1))==0) {
	    ERROR(0) "String storage space exhausted"
	    EEND;
	}
	for (r=t; *r++ = *s++; )
	    ;
	return t;
    }
    return s;
}

static Int local substr(s1,s2)		/* find posn of substring s1 in s2 */
String s1, s2; {			/* (naive implementation)	   */
    String t;

    for (t=s2; *t; t++) {
	Int i = 0;
        while (s1[i] && s1[i]==t[i])
	    i++;
	if (s1[i]=='\0')
	    return t-s2;
    }
    return (-1);
}

/*-------------------------------------------------------------------------*/
