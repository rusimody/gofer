/* --------------------------------------------------------------------------
 * runtime.c:   Copyright (c) Mark P Jones 1991-1994.   All rights reserved.
 *              See goferite.h for details and conditions of use etc...
 *		Gofer Compiler version 1.00 January 1992
 *              Gofer version 2.30 March 1994
 *
 * Runtime system for compiled Gofer programs ... uses a considerably
 * simplified runtime system than required in the full interpreter.
 * ------------------------------------------------------------------------*/

#define  NEED_MATH
#include "gofc.h"

#define cfunNil		mkCfun(0)	/* List constructors:		   */
#define cfunCons	mkCfun(1)

#define cfunFalse	mkCfun(0)	/* Bool constructors:		   */
#define cfunTrue	mkCfun(1)

/* --------------------------------------------------------------------------
 * Static data areas:
 * ------------------------------------------------------------------------*/

static  int   keep_argc;		/* keep record of command line	   */
static  char  **keep_argv;		/* arguments			   */

static  Cell  consCharArray[NUM_CHARS];	/* array of ((:) c) for each char c*/

static  Cell  resps = 0;		/* pointer to list of responses	   */

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static Cell   openFile		Args((String));
static Void   closeFile		Args((Int));

#if IO_DIALOGUE
static Void   readFile		Args((Void));
static Void   writeFile		Args((Void));
static Void   appendFile	Args((Void));
static Void   readChan		Args((Void));
static Void   appendChan	Args((Void));
static FILE   *validOutChannel	Args((String));
static Void   echo		Args((Void));
static Void   getArgs		Args((Void));
static Void   getProgName	Args((Void));
static Void   getEnv		Args((Void));
static String evalName		Args((Cell));
#endif
static Void   outputString	Args((FILE *,Cell));

#if HASKELL_ARRAYS
static Void   addAssocs		Args((Cell,Int,Cell));
static Void   foldAssocs	Args((Cell,Int,Cell,Cell));
#endif

static Int    compare		Args((Void));

static Void   primInit		Args((Void));
static Void   primMark		Args((Void));

static sigProto(onBreak);

static Void   abandon		Args((String));
static Void   leave		Args((int));

/* --------------------------------------------------------------------------
 * Machine dependent code for Gofer runtime system:
 * ------------------------------------------------------------------------*/

#define  MACHDEP_RUNTIME 1
#define  internal abandon
#include "machdep.c"
#undef   internal

/* --------------------------------------------------------------------------
 * Heap storage: Provides a garbage collected heap.
 *
 * We currently have a choice of two garbage collectors here.  You may use
 * either one or substitute your own collector if you prefer.
 * ------------------------------------------------------------------------*/

#if GC_MARKSCAN
#include "markscan.c"
#endif
#if GC_TWOSPACE
#include "twospace.c"
#endif

/* --------------------------------------------------------------------------
 * Control stack:
 * ------------------------------------------------------------------------*/

Cell	 cellStack[NUM_STACK];		/* Storage for cells on stack	   */
#ifndef  GLOBALsp
StackPtr sp;				/* stack pointer 		   */
#endif

Void overflow() {			/* Report stack overflow 	   */
    abandon("control stack overflow");
}

Void insufficientArgs() {		/* Report insufficent args on stack*/
    abandon("insufficient arguments on stack");
}

/* --------------------------------------------------------------------------
 * File operations:
 * ------------------------------------------------------------------------*/

static FILE *infiles[NUM_FILES];	/* file pointers for input files   */

static Cell openFile(s)			/* create FILECELL object for named*/
String s; {				/* input file			   */
    Int i;

    for (i=0; i<NUM_FILES && infiles[i]; ++i)	/* look for unused file .. */
	;
    if (i>=NUM_FILES) {				/* if at first we don't    */
	garbageCollect();			/* succeed, garbage collect*/
	for (i=0; i<NUM_FILES && infiles[i]; ++i)
	    ;					/* and try again ...	   */
    }
    if (i>=NUM_FILES) {				/* ... before we give up   */
	abandon("Too many files open");
    }

    if (infiles[i]=fopen(s,"r")) {
	heap(1);
	return pair(FILECELL,i);
    }
    else
	return cfunNil;
}

static Void closeFile(n)			/* close input file n	   */
Int n; {					/* only permitted when the */
    if (0<=n && n<NUM_FILES && infiles[n]) {	/* end of file is read or  */
	fclose(infiles[n]);			/* when discarded during gc*/
	infiles[n] = 0;
    }
}

/* --------------------------------------------------------------------------
 * Evaluator:
 * ------------------------------------------------------------------------*/

Cell  whnf;				/* head of term in whnf		   */
Int   whnfInt;				/* whnf term integer value	   */

Void eval(n)				/* Graph reduction evaluator	   */
register Cell n; {
    StackPtr base = sp;

unw:if (isPair(n)) {
	switch (fst(n)) {
	    case INDIRECT	: n = snd(n);
				  allowBreak();
				  goto unw;

	    case SUPERCOMB	: push(n);
#if ARGCHECK
				  (*superOf(n))(base);
#else
				  (*superOf(n))();
#endif
				  n  = pop();
				  goto unw;

	    case INTCELL	: whnfInt = bigOf(n);
				  break;

#if HASKELL_ARRAYS
	    case ARRAY		:
#endif
#if IO_MONAD
	    case MUTVAR		:
#endif
	    case FLOATCELL	: break;

	    case STRCELL	: {   String s = stringOf(n);
				      Int    c = *s;

				      if (c<0)
					  c += NUM_CHARS;
				      if (c==0)		/* end of string?  */
					  n = cfunNil;
				      else {
					  if (c=='\\')	/* escape sequence */
					      if ((c = *++s) !='\\')
						  c = 0;
					  needStack(2);
					  onto(n);
					  heap(1);
					  pushStr(++s);
					  snd(pushed(1)) = pushed(0);
					  fst(pushed(1)) =
					  pushed(0)      = consCharArray[c];
					  n		 = cfunCons;
				      }
				  }
				  break;

	    case FILECELL	: {   Int c = fgetc(infiles[snd(n)]);
				      if (c==EOF) {
					  closeFile(snd(n));/* delete this */
					  fst(n) = INDIRECT;/* FILECELL	   */
					  n      =
					  snd(n) = cfunNil;
				      }
				      else {
					  Int f = snd(n);
					  if (c<0)
					      c += NUM_CHARS;
					  needStack(2);
					  onto(n);
					  heap(1);
					  pushpair(FILECELL,f);
					  snd(pushed(1)) = pushed(0);
					  fst(pushed(1)) =
					  pushed(0)      = consCharArray[c];
					  n		 = cfunCons;
				      }
				  }
				  break;

	    default		: push(n);
				  n = fst(n);
				  goto unw;
	}
    }
    else
	whnfInt = smallOf(n);

    whnf = n;				/* save head of term		   */

    {   register StackPtr tmp=sp;	/* rearrange components of term on */
	while (tmp<base) {		/* stack, now in whnf ...	   */
	    fst(*tmp) = n;
	    n	      = *tmp;
	    *tmp++    = snd(n);
	}
    }
}

static Void evalString(n)		/* expand STRCELL at node n	   */
Cell n; {
}

Void fail() {				/* failure to apply supercombinator*/
    abandon("no applicable equation");
}

Cell rootFst(r)				/* find root node		   */
register Cell r; {
    for (; fst(r)==INDIRECT; r=snd(r))
	allowBreak();
    for (r=fst(r); isPair(r) && fst(r)==INDIRECT; r=snd(r))
	allowBreak();
    return r;
}

/* --------------------------------------------------------------------------
 * Dialogue based input/output:
 *
 * N.B. take care when modifying this code - it is rather delicate and even
 * the simplest of changes might create a nasty space leak... you have been
 * warned (please let me know if you think there already is a space leak!).
 * ------------------------------------------------------------------------*/

#if IO_DIALOGUE
#define cfunReadFile		mkCfun(0)	/* Request constructors:   */
#define cfunWriteFile		mkCfun(1)
#define cfunAppendFile		mkCfun(2)
#define nameReadChan		mkCfun(3)
#define cfunAppendChan		mkCfun(4)
#define cfunEcho		mkCfun(5)
#define cfunGetArgs		mkCfun(6)
#define cfunGetProgName		mkCfun(7)
#define cfunGetEnv		mkCfun(8)

#define cfunSuccess		mkCfun(0)	/* Response constructors:  */
#define cfunStr			mkCfun(1)
#define cfunFailure		mkCfun(2)	/* N.B. different ordering */
#define cfunStrList		mkCfun(3)	/* to Haskell report	   */

#define cfunWriteError		mkCfun(0)	/* IOError constructors:   */
#define cfunReadError		mkCfun(1)
#define cfunSearchError		mkCfun(2)
#define cfunFormatError		mkCfun(3)
#define	cfunOtherError		mkCfun(4)

static Bool echoChanged;		/* TRUE => echo changed in dialogue*/
static Bool stdinUsed;			/* TRUE => ReadChan stdin has been */
					/*	   seen in dialogue	   */

Void dialogue(prog)			/* carry out dialogue ...	   */
Cell prog; {				/*    :: [Response]->[Request]	   */

    echoChanged = FALSE;		/* set status flags		   */
    stdinUsed   = FALSE;

    clearStack();
    heap(3);
    pushStr("Attempt to read response before request complete");
    resps = pair(primError,pop());	/* set up initial responses	   */

    eval(pair(prog,resps));
    while (whnf==cfunCons) {
	eval(pop());			/* evaluate the request		   */

	if (whnf==cfunReadFile)		/* carry out the request	   */
	    readFile();
	else if (whnf==cfunWriteFile)
	    writeFile();
	else if (whnf==cfunAppendFile)
	    appendFile();
	else if (whnf==nameReadChan)
	    readChan();
	else if (whnf==cfunAppendChan)
	    appendChan();
	else if (whnf==cfunEcho)
	    echo();
	else if (whnf==cfunGetArgs)
	    getArgs();
	else if (whnf==cfunGetProgName)
	    getProgName();
	else if (whnf==cfunGetEnv)
	    getEnv();
	else
	    abandon("type error in request");

	heap(2);
	fst(resps) = pair(cfunCons,pop());	/* save response	   */
        snd(resps) = pair(primError,snd(resps));
        resps      = snd(resps);

	eval(pop());			/* evaluate the rest of the program*/
    }
    if (whnf!=cfunNil)
	abandon("type error in dialogue");
}

/* --------------------------------------------------------------------------
 * File system requests:
 * ------------------------------------------------------------------------*/

static Void readFile() {		/* repond to ReadFile request	   */
    String s = evalName(pushed(0));	/* pushed(0) = file name string	   */
    Cell   f;				/* pushed(1) = rest of program	   */

    if (access(s,0)!=0) {		/* can't find file		   */
	heap(2);
	topfun(cfunSearchError);
        topfun(cfunFailure);
    }
    else if (isPair(f=openFile(s))) {	/* file opened?			   */
	pushed(0) = f;
	heap(1);
        topfun(cfunStr);
    }
    else {				/* can't open file		   */
	heap(2);
	topfun(cfunReadError);
	topfun(cfunFailure);
    }
}

static Void writeFile() {		/* respond to WriteFile request	   */
    String s   = evalName(pushed(0));	/* pushed(0) = file name string	   */
    FILE   *fp;				/* pushed(1) = contents		   */
					/* pushed(2) = rest of program	   */

    if ((fp=fopen(s,FOPEN_WRITE))==0) { /* problem with output file	   */
	heap(2);
	topfun(cfunWriteError);
	topfun(cfunFailure);
	slide(1,top());
    }
    else {
	drop();				/* discard file name		   */
        outputString(fp,pop());		/* output string		   */
	fclose(fp);			/* and then close file		   */
	onto(cfunSuccess);
    }
}

static Void appendFile() {		/* respond to AppendFile request   */
    String s   = evalName(pushed(0));	/* pushed(0) = file name string	   */
    FILE   *fp;				/* pushed(1) = contents		   */
					/* pushed(2) = rest of program	   */

    if (access(s,0)!=0) {		/* can't find file		   */
	heap(2);
	topfun(cfunSearchError);
        topfun(cfunFailure);
	slide(1,top());
    }
    else if ((fp=fopen(s,FOPEN_APPEND))==0) {
	heap(2);
	topfun(cfunWriteError);		/* problem with output file	   */
	topfun(cfunFailure);
	slide(1,top());
    }
    else {
	drop();				/* discard file name		   */
        outputString(fp,pop());		/* output string		   */
	fclose(fp);			/* and then close file		   */
	onto(cfunSuccess);
    }
}

/* --------------------------------------------------------------------------
 * Channel system requests:
 * ------------------------------------------------------------------------*/

static Cell primInput;			/* builtin primitive function	   */

static Void readChan() {                /* respond to ReadChan request	   */
    String s = evalName(pushed(0));	/* pushed(0) = channel name string */
                                        /* pushed(1) = rest of program	   */

    if (strcmp(s,"stdin")!=0) {		/* only valid channel == stdin	   */
	heap(2);
        topfun(cfunSearchError);
	topfun(cfunFailure);
    }
    else if (stdinUsed) {		/* can't reuse stdin channel	   */
	heap(2);
	topfun(cfunReadError);
	topfun(cfunFailure);
    }
    else {				/* otherwise we can read from stdin*/
	stdinUsed = 1;
	pushed(0) = cfunFalse;/*dummy*/
	heap(2);
        topfun(primInput);
	topfun(cfunStr);
    }
}

static comb1(pr_Input)			/* input from stdin primitive	   */
{   Int c = readTerminalChar();
    if (c==EOF || c<0 || c>=NUM_CHARS) {
	clearerr(stdin);
	update(0,cfunNil);
    }
    else {
	needStack(1);
	heap(1);
	pushpair(primInput,cfunNil);
	updap(0,consCharArray[c<0 ? c+NUM_CHARS : c],pop());
    }
    ret();
}
End

static comb3(pr_Fopen)			/* open file for reading as str	   */
{   String s = evalName(offset(3));	/*  :: String->a->(String->a)->a   */

    if (s) {
	Cell file = openFile(s);
	if (file!=cfunNil) {
	    updap(0,offset(1),file);
	    ret();
	}
    }
    update(0,offset(2));
    ret();
}
End

static Void appendChan() {		/* respond to AppendChan request   */
    String s    = evalName(pushed(0));	/* pushed(0) = channel name string */
    FILE   *fp;				/* pushed(1) = contents		   */
					/* pushed(2) = rest of program	   */

    if ((fp=validOutChannel(s))==0) {	/* problem with output channel	   */
	heap(2);
	topfun(cfunSearchError);
	topfun(cfunFailure);
	slide(1,top());
    }
    else {				/* otherwise do output		   */
	drop();
	outputString(fp,pop());
	onto(cfunSuccess);
    }
}

static FILE *validOutChannel(s)		/* return FILE * for valid output  */
String s; {				/* channel name or 0 otherwise...  */
    if (strcmp(s,"stdout")==0)
	return stdout;
    if (strcmp(s,"stderr")==0)
	return stderr;
    if (strcmp(s,"stdecho")==0)		/* in Gofer, stdecho==stdout	   */
	return stdout;
    return 0;
}

/* --------------------------------------------------------------------------
 * Environment requests:
 * ------------------------------------------------------------------------*/

static Void echo() {			/* respond to Echo request	   */
					/* pushed(0) = boolean echo status */
					/* pushed(1) = rest of program	   */

    if (stdinUsed) {			/* stdin already used?		   */
	heap(3);
	top() = mkString("stdin already in use");
	topfun(cfunOtherError);
	topfun(cfunFailure);
    }
    else if (echoChanged) {		/* echo previously changed?	   */
	heap(3);
	top() = mkString("repeated Echo request");
	topfun(cfunOtherError);
	topfun(cfunFailure);
    }
    else {				/* otherwise evaluate and carry	   */
	eval(top());			/* out request			   */
	if (whnf==cfunFalse)
	    noechoTerminal();
	echoChanged = 1;
        top() = cfunSuccess;
    }
}

static Void getArgs() {			/* respond to GetArgs request	   */
    int i = keep_argc;

    push(cfunNil);			/* build list of args in reverse   */
    while (1<i--) {
	heap(3);
	pushStr(keep_argv[i]);
	topfun(cfunCons);
	mkap();
    }
    heap(1);
    topfun(cfunStrList);		/* and add StrList constructor	   */
}

static Void getProgName() {		/* respond to GetProgName request  */
    if (keep_argc>=1 && keep_argv[0]) {	/* normally, just return argv[0]   */
	heap(2);
	pushStr(keep_argv[0]);
	topfun(cfunStr);
    }
    else {
	heap(3);
	push(cfunNil);			/* return Failure (OtherError "")  */
	topfun(cfunOtherError);
	topfun(cfunFailure);
    }
}

static Void getEnv() {			/* repond to GetEnv request	   */
    String s = evalName(pushed(0));	/* pushed(0) = variable name str   */
    String r = getenv(s);		/* pushed(1) = rest of program	   */
    if (r) {
	heap(2);
	top() = mkString(r);
	topfun(cfunStr);
    }
    else {
	heap(2);
	topfun(cfunSearchError);
	topfun(cfunFailure);
    }
}

/* --------------------------------------------------------------------------
 * Evaluate name, obtaining a C string from a Gofer string:
 * ------------------------------------------------------------------------*/

static String evalName(es)		/* evaluate es :: [Char] and save  */
Cell es; {				/* in char array... return ptr to  */
    static char buffer[FILENAME_MAX+1];	/* string or 0, if error occurs	   */
    Int         pos    = 0;

    eval(es);
    while (whnf==cfunCons && pos<FILENAME_MAX) {
	eval(pop());
	buffer[pos++] = charOf(whnf);
	eval(pop());
    }
    if (pos>=FILENAME_MAX)		/* perhaps name was too long?	   */
	abandon("name too long");
    if (whnf!=cfunNil)			/* check for proper end of string  */
	abandon("type error in name");
    buffer[pos] = '\0';
    return buffer;
}
#endif

/* --------------------------------------------------------------------------
 * Top-level printing mechanism:
 * ------------------------------------------------------------------------*/

static Void outputString(fp,cs)		/* Evaluate string cs and print	   */
FILE *fp;				/* on specified output stream fp   */
Cell cs; {
    eval(cs);				/* keep reducing and printing head */
    while (whnf==cfunCons) {
	eval(pop());			/* evaluate character		   */
	fputc(charOf(whnf),fp);
	/*fflush(fp);*/
	eval(pop());			/* evaluate rest of string	   */
    }
    if (whnf!=cfunNil)			/* check for proper end of string  */
	abandon("type error in string");
}

/* --------------------------------------------------------------------------
 * Builtin primitive functions:
 * ------------------------------------------------------------------------*/

static comb2(pr_FATBAR)			/* FAIL `FATBAR` r = r		   */
    eval(offset(2));			/* l    `FATBAR` r = l		   */
    update(0,offset(whnf==FAIL?1:2));
    ret();
End

static comb0(pr_FAIL)			/* Pattern matching/guard failure  */
    update(0,FAIL);
    ret();
End

static comb0(pr_UNDEFMEM)		/* undefined member		   */
    abandon("undefined member function");
    ret();/*not reached*/
End

static comb0(pr_BlackHole)		/* garbage collector black hole	   */
    abandon("{GC black hole detected}");
    ret();/* not reached */
End

static comb3(pr_SEL)			/* component selection		   */
    eval(offset(2));			/* _SEL c e n ==> nth component in */
    if (whnf==offset(3)) {		/* expression e built using cfun c */
	update(0,pushed(intOf(offset(1))-1));
    }
    else
	abandon("pattern matching");
    ret();
End

static comb3(pr_IF)			/* conditional primitive	   */
    eval(offset(3));
    if (whnf==cfunTrue) {
	update(0,offset(2));
    }
    else {
	update(0,offset(1));
    }
    ret();
End

static comb2(pr_STRICT)			/* strict application primitive	   */
    eval(offset(1));
    updap(0,offset(2),offset(1));
    ret();
End

static comb1(pr_Error)			/* error primitive		   */
    fputs("\nprogram error: ",stderr);
    outputString(stderr,pop());
    fputc('\n',stderr);
    leave(1);
End

/* --------------------------------------------------------------------------
 * Array primitives:
 * ------------------------------------------------------------------------*/

#if    HASKELL_ARRAYS
#define getSize(bnds,rng,size)		{   Int lo;			   \
					    heap(2);			   \
					    eval(bnds);			   \
					    eval(pop()); lo=whnfInt;	   \
					    eval(pop());		   \
					    size = whnfInt - lo;	   \
					    size = (size>=0 ? size+1 : 0); \
					}

static Cell primUndefElt;		/* undefined element primitive	   */
static comb0(pr_UndefElt)
    abandon("undefined array element");
    ret();/*not reached*/
End

static Void addAssocs(r,size,arr)	/* add assocs in top() to array arr*/
Cell r;					/* using r for the range	   */
Int  size;				/* and with size elements	   */
Cell arr; {
    eval(pop());
    while (whnf==cfunCons) {
	ArrEltPtr pa;
	eval(pop());			/* evaluate an assoc pair	   */
	heap(1);			/* find integer position	   */
	topfun(r);
	eval(pop());

	if (whnfInt<0 || whnfInt>=size)			/* test range	   */
	    abandon("Array element out of bounds");
        setEltPtr(pa,arr,whnfInt);			/* find elem	   */
	if (arrElt(pa)!=FAIL) {				/* set value	   */
	    arrElt(pa) = primUndefElt;
	    drop();
	}
	else
            arrElt(pa) = pop();

	eval(pop());			/* evaluate rest of list	   */
    }
}

static Void foldAssocs(r,size,f,arr)	/* fold assocs in top() to array   */
Cell r;					/* using r for the range	   */
Int  size;				/* and with size elements	   */
Cell f;					/* and fold function f		   */
Cell arr; {
    eval(pop());
    while (whnf==cfunCons) {
	ArrEltPtr pa;
	eval(pop());			/* evaluate an assoc pair	   */
	heap(1);			/* find integer position	   */
	topfun(r);
	eval(pop());

	if (whnfInt<0 || whnfInt>=size)			/* test range	   */
	    abandon("Array element out of bounds");
        setEltPtr(pa,arr,whnfInt);			/* find elem	   */
	heap(2);					/* apply fold	   */
	arrElt(pa) = pair(f,arrElt(pa));
	arrElt(pa) = pair(arrElt(pa),pop());

	eval(pop());			/* evaluate rest of list	   */
    }
}

static comb3(pr_Array)			/* Array creation		   */
{   Int       size, i;			/*  :: (a -> Int) ->		   */
    ArrEltPtr pa;			/*	(a,a) ->		   */
    needStack(4);			/*	 [Assoc a b] -> Array a b  */
    getSize(offset(2),offset(3),size);
    allocArray(size,offset(2),FAIL);	/* alloc array at offset(4)	   */
    onto(offset(1));			/* load assocs			   */
    offset(1) = FAIL;			/* avoid space leak		   */
    addAssocs(offset(3),size,offset(4));
    setEltPtr(pa,offset(4),0);
    for (i=0; i<size; ++i, nextElt(pa))
	if (arrElt(pa)==FAIL)
	    arrElt(pa) = primUndefElt;
    update(0,offset(4));
    ret();
}
End

static comb3(pr_Update)			/* Array update			   */
{   Int       size, i;			/*  :: (a -> Int) ->		   */
    ArrEltPtr pa, opa;			/*	Array a b ->		   */
    needStack(5);			/*	 [Assoc a b] -> Array a b  */
    eval(offset(2));				/* evaluate array	   */
    onto(whnf);					/* and save at offset(4)   */
    getSize(arrBnds(offset(4)),offset(3),size);
    allocArray(size,arrBnds(offset(4)),FAIL);	/* alloc array at offset(5)*/
    onto(offset(1));				/* load assocs		   */
    offset(1) = FAIL;				/* avoid space leak	   */
    addAssocs(offset(3),size,offset(5));
    setEltPtr(opa,offset(4),0);
    setEltPtr(pa, offset(5),0);
    for (i=0; i<size; ++i, nextElt(pa), nextElt(opa))
	if (arrElt(pa)==FAIL)
	    arrElt(pa) = arrElt(opa);
    update(0,offset(5));
    ret();
}
End

static comb4(pr_Accum)			/* Array accum			   */
{   Int       size;			/*  :: (a -> Int) ->		   */
    needStack(4);			/*	(b -> c -> b) ->	   */
    eval(offset(2));			/*	 Array a b ->		   */
    dupArray(whnf);			/*	  [Assoc a c] -> Array a b */
    getSize(arrBnds(top()),offset(4),size);
    onto(offset(1));			/* load assocs			   */
    offset(1) = FAIL;			/* avoid space leak		   */
    foldAssocs(offset(4),size,offset(3),offset(5));
    update(0,offset(5));
    ret();
}
End

static comb5(pr_AccumArray)		/* array accumArray		   */
{   Int       size;			/*  :: (a -> Int) ->		   */
    needStack(4);			/*	(b -> c -> b) ->	   */
    getSize(offset(2),offset(5),size);	/*	 b ->			   */
    allocArray(size,offset(2),		/*	  (a,a) ->		   */
		    offset(3));		/*	   [Assoc a c] -> Array a b*/
    onto(offset(1));			/* load assocs			   */
    offset(1) = FAIL;			/* avoid space leak		   */
    foldAssocs(offset(5),size,offset(4),offset(6));
    update(0,offset(6));
    ret();
}
End

static comb2(pr_Amap)			/* map function over array	   */
    needStack(3);			/*  :: (b -> c) ->		   */
    eval(offset(1));			/*	Array a b -> Array a c	   */
    dupArray(whnf);
#define applyF(pa) heap(1); arrElt(pa)=pair(offset(2),arrElt(pa))
    arrMap(applyF,top());
#undef  applyF
    update(0,top());
    ret();
End

static comb3(pr_Subscript)		/* array subscript		   */
{   ArrEltPtr pa;			/*  :: (a -> Int) ->		   */
    Int       size, index;		/*	Array a b ->		   */
    Cell      arr;			/*	 a -> b			   */
    needStack(2);
    heap(1);
    pushpair(offset(3),offset(1));
    eval(pop());
    index = whnfInt;
    eval(top());
    arr   = whnf;
    getSize(arrBnds(arr),offset(3),size);
    if (index<0 || index>=size)
	abandon("subscript out of range");
    setEltPtr(pa,arr,index);
    update(0,arrElt(pa));
    ret();
}
End

static comb1(pr_Bounds)			/* bounds primitive		   */
    eval(offset(1));			/*  :: Array a b -> (a,a)	   */
    update(0,arrBnds(whnf));
    ret();
End

static comb1(pr_Elems)			/* elems primitive		   */
{   Cell es;
    needStack(2);
    eval(offset(1));
    push(cfunNil);
#define addElem(pa)  heap(2); pushpair(cfunCons,arrElt(pa)); mkap()
    arrMap(addElem,whnf);
#undef addElem
    for (es=cfunNil; isPair(top()); ) {	/* reverse the list		   */
	Cell tmp   = snd(top());
	snd(top()) = es;
	es         = top();
	top()	   = tmp;
    }
    update(0,top());
    ret();
}
End
#endif

/* --------------------------------------------------------------------------
 * IO monad/lazy state threads implementation:
 * ------------------------------------------------------------------------*/

#if IO_MONAD
static Cell primFst;			/* builtin primitive functions	   */
static Cell primSnd;			/* for fst and snd projections	   */

#define cfunPair   mkCfun(2)
#define cfunUnit   mkCfun(0)

Void iomonad(prog)			/* execute program in IO monad	   */
Cell prog; {
    noechoTerminal();
    heap(1);
    eval(pair(prog,cfunUnit));		/* run program			   */
    drop();				/* discard result (must be ())	   */
    eval(pop());			/* force the state		   */
}

static comb1(pr_STRun)			/* ST monad encapsulate		   */
    heap(1);				/*  :: all s.(ST s a) -> a	   */
    toparg(cfunUnit);
    updap(0,primFst,top());
    ret();
End

static comb1(pr_Fst)			/* fst primitive		   */
    eval(pop());			/*  :: (a,s) -> a		   */
    update(0,offset(2));
    ret();
End

static comb1(pr_Snd)			/* snd primitive		   */
    eval(pop());			/*  :: (a,s) -> s		   */
    update(0,offset(1));
    ret();
End

static comb1(pr_STReturn)		/* ST monad return		   */
    updap(0,cfunPair,offset(1));	/*  :: a -> ST s a		   */
    ret();
End

static comb3(pr_IOBind)			/* IO monad bind		   */
    heap(1);				/* :: ST s a ->			   */
    eval(pair(offset(3),offset(1)));	/*     (a -> ST s b) ->		   */
    heap(1);				/*	ST s b			   */
    topfun(offset(2));
    updap(0,offset(5),offset(4));
    ret();
End

static comb3(pr_STBind)			/* ST monad bind		   */
    needStack(3);			/* :: ST s a ->			   */
    heap(4);				/*     (a -> ST s b) ->		   */
    pushpair(offset(3),offset(1));	/*	ST s b			   */
    pushpair(primSnd,offset(4));
    pushpair(primFst,offset(4));
    topfun(offset(2));
    updap2(0);
    ret();
End

static comb2(pr_STInter)		/* ST monad interleave		   */
    needStack(1);			/*  :: ST s a -> ST s a		   */
    heap(3);
    pushpair(offset(2),offset(1));
    topfun(primFst);
    topfun(mkCfun(2));
    updap(0,pop(),offset(1));
    ret();
End

static comb2(pr_STNew)			/* ST monad variable allocator	   */
    heap(2);				/*  :: a ->			   */
    topfun(MUTVAR);			/* 	ST s (MutVar s a)	   */
    topfun(cfunPair);
    eval(offset(1));			/* force evaluation of state	   */
    updap(0,offset(2),offset(1));
    ret();
End

static comb3(pr_STAssign)		/* ST monad assignment		   */
    eval(offset(1));			/*  :: MutVar s a ->		   */
    eval(offset(3));			/*	a ->			   */
    snd(whnf) = offset(2);		/*	 ST s ()		   */
    heap(1);
    needStack(1);
    pushpair(cfunPair,cfunUnit);
    updap(0,top(),offset(1));
    ret();
End

static comb2(pr_STDeref)		/* ST monad dereference		   */
    eval(offset(1));			/*  :: MutVar s a ->		   */
    eval(offset(2));			/*	ST s a			   */
    heap(1);
    updap(0,pair(cfunPair,snd(whnf)),offset(1));
    ret();
End
 
static comb2(pr_STMutVarEq)		/* ST monad variable equality	   */
{   Cell x;				/*  :: MutVar s a ->		   */
    eval(offset(2));			/*	MutVar s a -> Bool	   */
    x = whnf;
    eval(offset(1));
    update(0,(x==whnf ? cfunTrue : cfunFalse));
    ret();
}
End

static comb1(pr_IOGetch)		/* get character from stdin	   */
    needStack(1);			/*  :: IO Char			   */
    eval(offset(1));
    heap(1);
    updap(0,pair(cfunPair,mkChar(readTerminalChar())),offset(1));
    ret();
End

static comb2(pr_IOPutchar)		/* print character on stdout	   */
    eval(offset(1));			/*  :: Char ->			   */
    eval(offset(2));			/*	IO ()			   */
    putchar(charOf(whnf));
#if DJGPP
    fflush(stdout);
#endif
    heap(1);
    updap(0,pair(cfunPair,cfunUnit),offset(1));
    ret();
End

#if HASKELL_ARRAYS
static comb4(pr_STNewArr)		/* allocate mutable array	   */
{   Cell arr;				/*  :: (a -> Int) ->		   */
    Int  size;				/*	(a,a) ->		   */
    needStack(2);			/*	 b ->			   */
    eval(offset(1));			/*	  ST s (MutArr s a b)	   */
    getSize(offset(3),offset(4),size);
    allocArray(size,offset(3),offset(2));
    heap(1);
    topfun(cfunPair);
    updap(0,top(),offset(1));
    ret();
}
End

static comb4(pr_STReadArr)		/* read element in mutable array   */
{   Cell      arr;			/*  :: ((a,a) -> a -> Int) ->	   */
    ArrEltPtr pa;			/*	MutArr s a b ->		   */
    needStack(2);			/*	 a ->			   */
    eval(offset(1));			/*	  ST s b		   */
    eval(offset(3));
    arr = whnf;
    heap(2);
    pushpair(offset(4),arrBnds(arr));
    toparg(offset(2));
    eval(pop());
    setEltPtr(pa,arr,whnfInt);		/* assumes index checks range	   */
    heap(1);
    pushpair(cfunPair,arrElt(pa));
    updap(0,top(),offset(1));
    ret();
}
End

static comb5(pr_STWriteArr)		/* write element in mutable array  */
{   Cell      arr;			/*  :: ((a,a) -> a -> Int) ->	   */
    ArrEltPtr pa;			/*	MutArr s a b ->		   */
    needStack(1);			/*	 a ->			   */
    eval(offset(1));			/*	  b ->			   */
    eval(offset(4));			/*	   ST s ()		   */
    arr = whnf;
    heap(2);
    pushpair(offset(5),arrBnds(arr));
    toparg(offset(3));
    eval(pop());
    setEltPtr(pa,arr,whnfInt);		/* assumes index checks range	   */
    arrElt(pa) = offset(2);
    heap(1);
    pushpair(cfunPair,cfunUnit);
    updap(0,top(),offset(1));
    ret();
}
End

static comb2(pr_STFreeze)		/* freeze mutable array		   */
    needStack(1);			/*  :: MutArr s a b ->		   */
    eval(offset(1));			/*      ST s (Array a b)	   */
    eval(offset(2));
    dupArray(whnf);
    heap(1);
    topfun(cfunPair);
    updap(0,top(),offset(1));
    ret();
End
#endif
#endif

/* --------------------------------------------------------------------------
 * Integer arithmetic primitives:
 * ------------------------------------------------------------------------*/

static comb2(pr_PlusInt)		/* integer addition primitive	   */
{   Int x;
    eval(offset(2));
    x = whnfInt;
    eval(offset(1));
    heap(1);
    update(0,mkInt(x+whnfInt));
    ret();
}
End

static comb2(pr_MinusInt)		/* integer subtraction primitive   */
{   Int x;
    eval(offset(2));
    x = whnfInt;
    eval(offset(1));
    heap(1);
    update(0,mkInt(x-whnfInt));
    ret();
}
End

static comb2(pr_MulInt)			/* integer multiplication primitive*/
{   Int x;
    eval(offset(2));
    x = whnfInt;
    eval(offset(1));
    heap(1);
    update(0,mkInt(x*whnfInt));
    ret();
}
End

static comb2(pr_DivInt)			/* integer division primitive	   */
{   Int x,y;				/* truncate towards -ve infinity   */
    eval(offset(2));
    x = whnfInt;
    eval(offset(1));
    if (whnfInt==0)
	abandon("division by zero");
    y = x%whnfInt;
    x = x/whnfInt;
    if ((y<0 && whnfInt>0) || (y>0 && whnfInt<0))
	x--;
    heap(1);
    update(0,mkInt(x));
    ret();
}
End

static comb2(pr_QuotInt)		/* integer division primitive	   */
{   Int x;				/* truncated towards zero	   */
    eval(offset(2));
    x = whnfInt;
    eval(offset(1));
    if (whnfInt==0)
	abandon("division by zero");
    heap(1);
    update(0,mkInt(x/whnfInt));
    ret();
}
End

static comb2(pr_ModInt)			/* integer modulo primitive	   */
{   Int x,y;
    eval(offset(2));
    x = whnfInt;
    eval(offset(1));
    if (whnfInt==0)
	abandon("division by zero");
    heap(1);
    y = x%whnfInt;			/* "... the modulo having the sign */
    if ((y<0 && whnfInt>0) ||		/*	       of the divisor ..." */
	(y>0 && whnfInt<0)) {		/* See definition on p.81 of	   */
	update(0,mkInt(y+whnfInt));	/* Haskell report...		   */
    }
    else {
	update(0,mkInt(y));
    }
    ret();
}
End

static comb2(pr_RemInt)			/* integer remainder primitive	   */
{   Int x;
    eval(offset(2));			/* div and rem satisfy:		   */
    x = whnfInt;			/* (x `div` y)*y+(x `rem` y) == x  */
    eval(offset(1));			/* which is exactly the property   */
    if (whnfInt==0)			/* described in K&R 2:		   */
	abandon("division by zero");	/*      (a/b)*b + a%b == a	   */
    heap(1);
    update(0,mkInt(x%whnfInt));
    ret();
}
End

static comb1(pr_NegInt)			/* integer negation primitive	   */
    eval(offset(1));
    heap(1);
    update(0,mkInt(-whnfInt));
    ret();
End

/* --------------------------------------------------------------------------
 * Coercion primitives:
 * ------------------------------------------------------------------------*/

static comb1(pr_CharToInt)		/* character to integer conversion */
    eval(offset(1));
    heap(1);
    update(0,mkInt(charOf(whnf)));
    ret();
End

static comb1(pr_IntToChar)		/* integer to character conversion */
    eval(offset(1));
    if (whnfInt<0 || whnfInt>=NUM_CHARS)
	abandon("character out of range");
    update(0,mkChar(whnfInt));
    ret();
End

static comb1(pr_IntToFloat)		/* integer to float primitive	   */
    eval(offset(1));
    heap(1);
    update(0,mkFloat((Float)(whnfInt)));
    ret();
End

/* --------------------------------------------------------------------------
 * Float arithmetic primitives:
 * ------------------------------------------------------------------------*/

static comb2(pr_PlusFloat)		/* float addition primitive	   */
{   Float x;
    eval(offset(2));
    x = floatOf(whnf);
    eval(offset(1));
    heap(1);
    update(0,mkFloat(x+floatOf(whnf)));
    ret();
}
End

static comb2(pr_MinusFloat)		/* float subtraction primitive	   */
{   Float x;
    eval(offset(2));
    x = floatOf(whnf);
    eval(offset(1));
    heap(1);
    update(0,mkFloat(x-floatOf(whnf)));
    ret();
}
End

static comb2(pr_MulFloat)		/* float multiplication primitive  */
{   Float x;
    eval(offset(2));
    x = floatOf(whnf);
    eval(offset(1));
    heap(1);
    update(0,mkFloat(x*floatOf(whnf)));
    ret();
}
End

static comb2(pr_DivFloat)		/* float division primitive	   */
{   Float x;
    eval(offset(2));
    x = floatOf(whnf);
    eval(offset(1));
    if (floatOf(whnf)==0)
	abandon("float division by zero");
    heap(1);
    update(0,mkFloat(x/floatOf(whnf)));
    ret();
}
End

static comb1(pr_NegFloat)		/* float negation primitive	   */
    eval(offset(1));
    heap(1);
    update(0,mkFloat(-floatOf(whnf)));
    ret();
End

#if HAS_FLOATS
#define FPRIM(n,f)		static comb1(n)				    \
				    eval(offset(1));			    \
				    heap(1);				    \
				    update(0,safeMkFloat(f(floatOf(whnf))));\
				    ret();				    \
				End
FPRIM(pr_SinFloat,sin)			/* floating point math prims	   */
FPRIM(pr_CosFloat,cos)
FPRIM(pr_TanFloat,tan)
FPRIM(pr_AsinFloat,asin)
FPRIM(pr_AcosFloat,acos)
FPRIM(pr_AtanFloat,atan)
FPRIM(pr_LogFloat,log)			/* one day, I should expand these  */
FPRIM(pr_Log10Float,log10)		/* to ensure the argument is > 0   */
FPRIM(pr_ExpFloat,exp)
FPRIM(pr_SqrtFloat,sqrt)
#undef FPRIM

static comb2(pr_Atan2Float)		/* arc tan with quadrant info	   */
{   Float x;
    eval(offset(2));
    x = floatOf(whnf);
    eval(offset(1));
    heap(1);
    update(0,mkFloat(atan2(x,floatOf(whnf))));
    ret();
}
End

static comb1(pr_FloatToInt)		/* convert floating point to int   */
    eval(offset(1));			/* :: Float -> Int		   */
    heap(1);
    update(0,mkInt((Int)(floatOf(whnf))));
    ret();
End
#endif

/* --------------------------------------------------------------------------
 * Comparison primitives:
 * ------------------------------------------------------------------------*/

static comb2(pr_EqInt)			/* integer equality primitive	   */
{   Int x;
    eval(offset(2));
    x = whnfInt;
    eval(offset(1));
    update(0,(x==whnfInt ? cfunTrue : cfunFalse));
    ret();
}
End

static comb2(pr_LeInt)			/* integer <= primitive		   */
{   Int x;
    eval(offset(2));
    x = whnfInt;
    eval(offset(1));
    update(0,(x<=whnfInt ? cfunTrue : cfunFalse));
    ret();
}
End

static comb2(pr_EqChar)			/* character equality primitive	   */
{   Cell x;
    eval(offset(2));
    x = whnf;
    eval(offset(1));
    update(0,(x==whnf ? cfunTrue : cfunFalse));
    ret();
}
End

static comb2(pr_LeChar)			/* character <= primitive	   */
{   Cell x;
    eval(offset(2));
    x = whnf;
    eval(offset(1));
    update(0,(x<=whnf ? cfunTrue : cfunFalse));
    ret();
}
End

static comb2(pr_EqFloat)		/* float equality primitive	   */
{   Float x;
    eval(offset(2));
    x = floatOf(whnf);
    eval(offset(1));
    update(0,(x==floatOf(whnf) ? cfunTrue : cfunFalse));
    ret();
}
End

static comb2(pr_LeFloat)		/* float <= primitive		   */
{   Float x;
    eval(offset(2));
    x = floatOf(whnf);
    eval(offset(1));
    update(0,(x<=floatOf(whnf) ? cfunTrue : cfunFalse));
    ret();
}
End

/* --------------------------------------------------------------------------
 * Generic comparison primitives:
 *
 * The following primitives are provided for the benefit of anyone that
 * wants to use Gofer's generic comparison functions in place of the
 * type class alternative.  Be warned however, that an attempt to compare
 * two function values using these routines will generate a runtime error
 * which will not be trapped unless you compile the runtime system and
 * application with ARGCHECK=1 (in which case, the overall performance
 * will degrade, even if you never actually do compare function values).
 * You see, using type classes really can bring benefits ...       :-)
 *
 * (The hardest thing in the following code is ensuring that all of the
 * appropriate temporary variables stay on the stack to ensure proper
 * operation of the garbage collector.)
 * ------------------------------------------------------------------------*/

#define LT 0
#define EQ 1
#define GT 2

static Int compare() {			/* Shared auxiliary function	   */
    StackPtr args = sp;			/* for generic comparisons	   */
    Int      xy;

    heap(2);
    pushed(1) = pair(pushed(1),cfunNil);/* turn arguments into lists	   */
    pushed(0) = pair(pushed(0),cfunNil);/* simulating depth-first stack	   */

    do {
	Int xdepth, ydepth;

	eval(fst(pushed(0)));		/* evaluate part of `x'		   */
        push(whnf);
	xdepth = pushedSince(args);

	eval(fst(pushed(1+xdepth)));	/* evaluate part of `y'		   */
	push(whnf);
	ydepth = pushedSince(args) - xdepth;

	xy	     = xdepth+ydepth;	/* discard values on top of depth- */
	pushed(xy)   = snd(pushed(xy));	/* first stacks			   */
	pushed(xy+1) = snd(pushed(xy+1));

        /* If the whnf of the part of x is   X x1 ... xn
	 * and the whnf of the part of y is  Y y1 ... ym,
	 * then the top of the stack will look like this:
	 *
	 *	top() =	Y  \
	 *		y1  |
	 *		.   |	ydepth elements
	 *		.   |
	 *		ym /
	 *		X  \
	 *		x1  |
	 *		.   |	xdepth elements
	 *		.   |
	 *		xn /
	 *		xs
	 *		ys
	 */

	if (isPair(top()) || isPair(pushed(ydepth))) {
	    if (isPair(top()) && fst(top())==FLOATCELL) {	/* Floats  */
		Float xf = floatOf(pushed(ydepth));
		Float yf = floatOf(top());
		if (xf<yf) return LT;
		if (xf>yf) return GT;
	    }
	    else {						/* Ints	   */
		Int xi = intOf(pushed(ydepth));
		Int yi = intOf(top());
		if (xi<yi) return LT;
		if (xi>yi) return GT;
	    }
	}
	else {				/* two proper constructor applics  */
	    if (top()>pushed(ydepth))	/* x structure has smaller constr  */
		return LT;
	    if (top()<pushed(ydepth))	/* y structure has smaller constr  */
		return GT;
	    if (xdepth!=ydepth)
		abandon("type error in comparison");
	    else {
		Int i;
		heap(2*ydepth);
		for (i=ydepth-1; i>0; --i) {		/* add new values  */
		    pushed(xy+1) = pair(pushed(i),pushed(xy+1));
		    pushed(xy)   = pair(pushed(i+ydepth),pushed(xy));
		}
	    }
	}
	sp = args;
    } while (isPair(top()));		/* loop if value queue not empty*/

    return EQ;				/* everything matched, so x==y  */
}

#define genericPrim(n,bool)	static comb2(n)				   \
				{   Int result = bool;			   \
				    update(0, result?cfunTrue:cfunFalse);  \
				    ret();				   \
				}					   \
				End
genericPrim(pr_GenericEq, compare()==EQ)
genericPrim(pr_GenericNe, compare()!=EQ)
genericPrim(pr_GenericLt, compare()==LT)
genericPrim(pr_GenericLe, compare()!=GT)
genericPrim(pr_GenericGt, compare()==GT)
genericPrim(pr_GenericGe, compare()!=LT)
#undef genericPrim

/* --------------------------------------------------------------------------
 * Print primitives:
 * ------------------------------------------------------------------------*/

static comb3(pr_ShowsInt)		/* find string rep. for integer	   */
{   Int num;				/*  :: Int -> Int -> ShowS	   */
    drop();				/* throw away first parameter	   */
    eval(pop());
    num = whnfInt;

    if (0<=num && num<10) {				/* single digit	   */
	updap(0,consCharArray['0'+num],top());
    }
    else if (num<0) {					/* negative integer*/
	num = -num;
	do {
	    heap(1);
	    topfun(consCharArray['0'+num%10]);
	} while ((num/=10)>0);
	updap(0,consCharArray['-'],top());
    }
    else {						/* positive integer*/
	do {
	    heap(1);
	    topfun(consCharArray['0'+num%10]);
	} while ((num/=10)>9);
	updap(0,consCharArray['0'+num],top());
    }
    ret();
}
End

static comb3(pr_ShowsFloat)		/* find string rep. for float	   */
{   String s;				/*  :: Int -> Float -> ShowS	   */
    Int    n;
    drop();				/* throw away first parameter	   */
    eval(pop());
    s = floatToString(floatOf(whnf));
    n = strlen(s);
    while (1<n--) {
	heap(1);
	topfun(consCharArray[s[n]]);
    }
    updap(0,consCharArray[*s],top());
    ret();
}
End

/* --------------------------------------------------------------------------
 * Storage, initialisation and marking of primitives:
 * ------------------------------------------------------------------------*/

Cell primFatbar,     primFail;		/* System (internal) primitives	   */
Cell primUndefMem,   primBlackHole;
Cell primSel,	     primIf;
Cell primStrict;

#if  HASKELL_ARRAYS
Cell primArray,      primUpdate;	/* Haskell IO primitives	   */
Cell primAccum,      primAccumArray;
Cell primAmap,       primSubscript;
Cell primBounds,     primElems;
#endif

Cell primPlusInt,    primMinusInt;	/* User (general) primitives	   */
Cell primMulInt,     primDivInt;
Cell primModInt,     primRemInt;
Cell primNegInt,     primQuotInt;
Cell primCharToInt,  primIntToChar;
Cell primIntToFloat;
Cell primPlusFloat,  primMinusFloat;
Cell primMulFloat,   primDivFloat;
Cell primNegFloat;
Cell primEqInt,	     primLeInt;
Cell primEqChar,     primLeChar;
Cell primEqFloat,    primLeFloat;
Cell primGenericEq,  primGenericNe;
Cell primGenericGt,  primGenericGe;
Cell primGenericLt,  primGenericLe;
Cell primShowsInt,   primShowsFloat;
Cell primError;

#if  IO_MONAD
Cell primSTRun,	     primSTReturn;	/* IO and ST monad primitives	   */
Cell primIOBind,     primSTBind;
Cell primSTNew,	     primSTAssign;
Cell primSTDeref,    primSTMutVarEq;
Cell primIOGetch,    primIOPutchar;
Cell primSTInter;
#if  HASKELL_ARRAYS
Cell primSTNewArr,   primSTReadArr;	/* mutable array primitives	   */
Cell primSTWriteArr, primSTFreeze;
#endif
#endif

#if  HAS_FLOATS
Cell primSinFloat,   primAsinFloat;
Cell primCosFloat,   primAcosFloat;
Cell primTanFloat,   primAtanFloat;
Cell primAtan2Float, primExpFloat;
Cell primLogFloat,   primLog10Float;
Cell primSqrtFloat,  primFloatToInt;
#endif

Cell primFopen;				/* read from file primitive	   */

static Void primInit() {		/* initialise primitives	   */
    primFatbar	   = mkSuper(pr_FATBAR);
    primFail	   = mkSuper(pr_FAIL);
    primUndefMem   = mkSuper(pr_UNDEFMEM);
    primBlackHole  = mkSuper(pr_BlackHole);
    primSel	   = mkSuper(pr_SEL);
    primIf	   = mkSuper(pr_IF);
    primStrict	   = mkSuper(pr_STRICT);
#if HASKELL_ARRAYS
    primArray      = mkSuper(pr_Array);
    primUpdate     = mkSuper(pr_Update);
    primAccum      = mkSuper(pr_Accum);
    primAccumArray = mkSuper(pr_AccumArray);
    primAmap       = mkSuper(pr_Amap);
    primSubscript  = mkSuper(pr_Subscript);
    primBounds     = mkSuper(pr_Bounds);
    primElems      = mkSuper(pr_Elems);
    primUndefElt   = mkSuper(pr_UndefElt);
#endif
    primPlusInt	   = mkSuper(pr_PlusInt);
    primMinusInt   = mkSuper(pr_MinusInt);
    primMulInt	   = mkSuper(pr_MulInt);
    primDivInt	   = mkSuper(pr_DivInt);
    primQuotInt	   = mkSuper(pr_QuotInt);
    primModInt	   = mkSuper(pr_ModInt);
    primRemInt	   = mkSuper(pr_RemInt);
    primNegInt	   = mkSuper(pr_NegInt);
    primCharToInt  = mkSuper(pr_CharToInt);
    primIntToChar  = mkSuper(pr_IntToChar);
    primIntToFloat = mkSuper(pr_IntToFloat);
    primPlusFloat  = mkSuper(pr_PlusFloat);
    primMinusFloat = mkSuper(pr_MinusFloat);
    primMulFloat   = mkSuper(pr_MulFloat);
    primDivFloat   = mkSuper(pr_DivFloat);
    primNegFloat   = mkSuper(pr_NegFloat);
    primEqInt	   = mkSuper(pr_EqInt);
    primLeInt	   = mkSuper(pr_LeInt);
    primEqChar	   = mkSuper(pr_EqChar);
    primLeChar	   = mkSuper(pr_LeChar);
    primEqFloat	   = mkSuper(pr_EqFloat);
    primLeFloat	   = mkSuper(pr_LeFloat);
    primGenericEq  = mkSuper(pr_GenericEq);
    primGenericNe  = mkSuper(pr_GenericNe);
    primGenericGt  = mkSuper(pr_GenericGt);
    primGenericGe  = mkSuper(pr_GenericGe);
    primGenericLt  = mkSuper(pr_GenericLt);
    primGenericLe  = mkSuper(pr_GenericLe);
    primShowsInt   = mkSuper(pr_ShowsInt);
    primShowsFloat = mkSuper(pr_ShowsFloat);
    primError      = mkSuper(pr_Error);
#if IO_DIALOGUE
    primInput	   = mkSuper(pr_Input);
    primFopen      = mkSuper(pr_Fopen);
#endif
#if IO_MONAD
    primSTRun	   = mkSuper(pr_STRun);
    primFst	   = mkSuper(pr_Fst);
    primSnd        = mkSuper(pr_Snd);
    primSTReturn   = mkSuper(pr_STReturn);
    primIOBind	   = mkSuper(pr_IOBind);
    primSTBind     = mkSuper(pr_STBind);
    primSTInter    = mkSuper(pr_STInter);
    primSTNew	   = mkSuper(pr_STNew);
    primSTAssign   = mkSuper(pr_STAssign);
    primSTDeref	   = mkSuper(pr_STDeref);
    primSTMutVarEq = mkSuper(pr_STMutVarEq);
    primIOGetch	   = mkSuper(pr_IOGetch);
    primIOPutchar  = mkSuper(pr_IOPutchar);
#if HASKELL_ARRAYS
    primSTNewArr   = mkSuper(pr_STNewArr);
    primSTReadArr  = mkSuper(pr_STReadArr);
    primSTWriteArr = mkSuper(pr_STWriteArr);
    primSTFreeze   = mkSuper(pr_STFreeze);
#endif
#endif
#if HAS_FLOATS
    primSinFloat   = mkSuper(pr_SinFloat);
    primAsinFloat  = mkSuper(pr_AsinFloat);
    primCosFloat   = mkSuper(pr_CosFloat);
    primAcosFloat  = mkSuper(pr_AcosFloat);
    primTanFloat   = mkSuper(pr_TanFloat);
    primAtanFloat  = mkSuper(pr_AtanFloat);
    primAtan2Float = mkSuper(pr_Atan2Float);
    primExpFloat   = mkSuper(pr_ExpFloat);
    primLogFloat   = mkSuper(pr_LogFloat);
    primLog10Float = mkSuper(pr_Log10Float);
    primSqrtFloat  = mkSuper(pr_SqrtFloat);
    primFloatToInt = mkSuper(pr_FloatToInt);
#endif
}

static Void primMark() {		/* mark primitives		   */
    mark(primFatbar);
    mark(primFail);
    mark(primUndefMem);
    mark(primBlackHole);
    mark(primSel);
    mark(primIf);
    mark(primStrict);
#if HASKELL_ARRAYS
    mark(primArray);
    mark(primUpdate);
    mark(primAccum);
    mark(primAccumArray);
    mark(primAmap);
    mark(primSubscript);
    mark(primBounds);
    mark(primElems);
    mark(primUndefElt);
#endif
    mark(primPlusInt);
    mark(primMinusInt);
    mark(primMulInt);
    mark(primDivInt);
    mark(primQuotInt);
    mark(primModInt);
    mark(primRemInt);
    mark(primNegInt);
    mark(primCharToInt);
    mark(primIntToChar);
    mark(primIntToFloat);
    mark(primPlusFloat);
    mark(primMinusFloat);
    mark(primMulFloat);
    mark(primDivFloat);
    mark(primNegFloat);
    mark(primEqInt);
    mark(primLeInt);
    mark(primEqChar);
    mark(primLeChar);
    mark(primEqFloat);
    mark(primLeFloat);
    mark(primGenericEq);
    mark(primGenericNe);
    mark(primGenericGt);
    mark(primGenericGe);
    mark(primGenericLt);
    mark(primGenericLe);
    mark(primShowsInt);
    mark(primShowsFloat);
    mark(primError);
#if IO_DIALOGUE
    mark(primInput);
    mark(primFopen);
#endif
#if IO_MONAD
    mark(primSTRun);
    mark(primFst);
    mark(primSnd);
    mark(primSTReturn);
    mark(primIOBind);
    mark(primSTBind);
    mark(primSTInter);
    mark(primSTNew);
    mark(primSTAssign);
    mark(primSTDeref);
    mark(primSTMutVarEq);
    mark(primIOGetch);
    mark(primIOPutchar);
#if  HASKELL_ARRAYS
    mark(primSTNewArr);
    mark(primSTReadArr);
    mark(primSTWriteArr);
    mark(primSTFreeze);
#endif
#endif
#if HAS_FLOATS
    mark(primSinFloat);
    mark(primAsinFloat);
    mark(primCosFloat);
    mark(primAcosFloat);
    mark(primTanFloat);
    mark(primAtanFloat);
    mark(primAtan2Float);
    mark(primExpFloat);
    mark(primLogFloat);
    mark(primLog10Float);
    mark(primSqrtFloat);
    mark(primFloatToInt);
#endif
}

/* --------------------------------------------------------------------------
 * Main program including startup code and initialisation:
 * ------------------------------------------------------------------------*/

Main main(argc,argv)			/* entry point and initialisation  */
int argc;
char *argv[]; {
    int i;

    if (argcheck!=ARGCHECK)		/* consistency check on compilation*/
	abandon("program linked with wrong runtime support file");

    keep_argc = argc;			/* save command line arguments	   */
    keep_argv = argv;

    for (i=0; i<NUM_FILES; i++)		/* initialise file storage	   */
	infiles[i] = 0;

    clearStack();			/* initialise control stack	   */

    heapInit();				/* initialise heap storage	   */

    for (i=0; i<num_scs; i++)		/* initialise CAF table		   */
	sc[i] = mkSuper(scNames[i]);

    primInit();				/* initialise primitives	   */

    for (i=num_dicts; --i>0; )		/* initialise dictionaries	   */
	if (dictImps[i]>=0)
	    if (dict[i])
		dict[i] = pair(sc[dictImps[i]],dict[i]);
	    else
		dict[i] = primUndefMem;

    for (i=0; i<NUM_CHARS; ++i)		/* initialise character array	   */
	consCharArray[i] = pair(cfunCons,mkChar(i));

    ctrlbrk(onBreak);
    (*topLevel)(sc[num_scs-1]);		/* sc_main is always the last sc   */
    leave(0);
    MainDone
}

static sigHandler(onBreak) {		/* break handler		   */
    abandon("interrupted");
    sigResume;/*NOTREACHED*/
}

static Void abandon(why)		/* abort execution of program	   */
String why; {
    fputs("\nprogram aborting: ",stderr);
    fputs(why,stderr);
    fputc('\n',stderr);
    leave(1);
}

static Void leave(exitcode)		/* tidy up and exit from program   */
int exitcode; {
    normalTerminal();
    exit(exitcode);
}

/*-------------------------------------------------------------------------*/
