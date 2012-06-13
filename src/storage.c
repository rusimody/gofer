/* --------------------------------------------------------------------------
 * storage.c:   Copyright (c) Mark P Jones 1991-1994.   All rights reserved.
 *              See goferite.h for details and conditions of use etc...
 *              Gofer version 2.30 March 1994
 *
 * Primitives for manipulating global data structures
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include <setjmp.h>

static List local insertName		Args((Name,List));
static Void local patternError		Args((String));
static Bool local stringMatch		Args((String,String));

static Int  local hash			Args((String));
static Int  local saveText		Args((Text));
static Cell local markCell		Args((Cell));
static Void local markSnd		Args((Cell));
static Cell local indirectChain		Args((Cell));
static Cell local lowLevelLastIn	Args((Cell));
static Cell local lowLevelLastOut	Args((Cell));
static Void local closeFile		Args((Int));

/* --------------------------------------------------------------------------
 * Text storage:
 *
 * provides storage for the characters making up identifier and symbol
 * names, string literals, character constants etc...
 *
 * All character strings are stored in a large character array, with textHw
 * pointing to the next free position.	Lookup in the array is improved using
 * a hash table.  Internally, text strings are represented by integer offsets
 * from the beginning of the array to the string in question.
 *
 * Where memory permits, the use of multiple hashtables gives a significant
 * increase in performance, particularly when large source files are used.
 *
 * Each string in the array is terminated by a zero byte.  No string is
 * stored more than once, so that it is safe to test equality of strings by
 * comparing the corresponding offsets.
 *
 * Special text values (beyond the range of the text array table) are used
 * to generate unique `new variable names' as required.
 *
 * The same text storage is also used to hold text values stored in a saved
 * expression.  This grows downwards from the top of the text table (and is
 * not included in the hash table).
 * ------------------------------------------------------------------------*/

#define TEXTHSZ 512			/* Size of Text hash table	   */
#define NOTEXT	((Text)(~0))		/* Empty bucket in Text hash table */
static	Text	textHw;			/* Next unused position		   */
static  Text	savedText = NUM_TEXT;	/* Start of saved portion of text  */
static	Text	nextNewText;		/* Next new text value		   */
static  Text    nextNewDText;		/* Next new dict text value	   */
static	char	text[NUM_TEXT];		/* Storage of character strings	   */
static	Text	textHash[TEXTHSZ][NUM_TEXTH]; /* Hash table storage	   */

String textToStr(t)		       /* find string corresp to given Text*/
Text t; {
    static char newVar[16];

    if (0<=t && t<NUM_TEXT)			/* standard char string	   */
	return text + t;
    if (t<0)
	sprintf(newVar,"d%d",-t);		/* dictionary variable	   */
    else
	sprintf(newVar,"v%d",t-NUM_TEXT);	/* normal variable	   */
    return newVar;
}

Text inventText() {			/* return new unused variable name */
    return nextNewText++;
}

Text inventDictText() {			/* return new unused dictvar name  */
    return nextNewDText--;
}

static Int local hash(s)		/* Simple hash function on strings */
String s; {
    int v, j = 3;

    for (v=((int)(*s))*8; *s; s++)
	v += ((int)(*s))*(j++);
    if (v<0)
	v = (-v);
    return(v%TEXTHSZ);
}

Text findText(s)		       /* Locate string in Text array	   */
String s; {
    int    h	   = hash(s);
    int    hashno  = 0;
    Text   textPos = textHash[h][hashno];

#define TryMatch	{   Text   originalTextPos = textPos;		   \
			    String t;					   \
			    for (t=s; *t==text[textPos]; textPos++,t++)	   \
				if (*t=='\0')				   \
				    return originalTextPos;		   \
			}
#define Skip		while (text[textPos++]) ;

    while (textPos!=NOTEXT) {
	TryMatch
	if (++hashno<NUM_TEXTH)		/* look in next hashtable entry	   */
	    textPos = textHash[h][hashno];
	else {
	    Skip
	    while (textPos < textHw) {
		TryMatch
		Skip
	    }
	    break;
	}
    }

#undef TryMatch
#undef Skip

    textPos = textHw;		       /* if not found, save in array	   */
    if (textHw + strlen(s) + 1 > savedText) {
	ERROR(0) "Character string storage space exhausted"
	EEND;
    }
    while (text[textHw++] = *s++)
	;
    if (hashno<NUM_TEXTH) {	       /* updating hash table as necessary */
	textHash[h][hashno] = textPos;
	if (hashno<NUM_TEXTH-1)
	    textHash[h][hashno+1] = NOTEXT;
    }

    return textPos;
}

static Int local saveText(t)		/* Save text value in buffer	   */
Text t; {				/* at top of text table		   */
    String s = textToStr(t);
    Int    l = strlen(s);

    if (textHw + strlen(s) + 1 > savedText) {
	ERROR(0) "Character string storage space exhausted"
	EEND;
    }
    savedText -= l+1;
    strcpy(text+savedText,s);
    return savedText;
}

/* --------------------------------------------------------------------------
 * Syntax storage:
 *
 * Operator declarations are stored in a table which associates Text values
 * with Syntax values.
 * ------------------------------------------------------------------------*/

static Int syntaxHw;		       /* next unused syntax table entry   */
static struct { 		       /* table of Text <-> Syntax values  */
    Text   text;
    Syntax syntax;
} tabSyntax[NUM_SYNTAX];

Syntax syntaxOf(t)		       /* look up syntax of operator symbol*/
Text t; {
    int i;

    for (i=0; i<syntaxHw; ++i)
	if (tabSyntax[i].text==t)
	    return tabSyntax[i].syntax;
    return defaultSyntax(t);
}

Void addSyntax(line,t,sy)	       /* add (t,sy) to syntax table	   */
Int    line;
Text   t;
Syntax sy; {
    int i;

    for (i=0; i<syntaxHw; ++i)
	if (tabSyntax[i].text==t) {
	    ERROR(line) "Attempt to redefine syntax of operator \"%s\"",
			textToStr(t)
	    EEND;
	}

    if (syntaxHw>=NUM_SYNTAX) {
	ERROR(line) "Too many fixity declarations"
	EEND;
    }

    tabSyntax[syntaxHw].text   = t;
    tabSyntax[syntaxHw].syntax = sy;
    syntaxHw++;
}

/* --------------------------------------------------------------------------
 * Addr storage: records `next unused program location'
 * ------------------------------------------------------------------------*/

static Addr addrHw;		       /* next unused program location	   */

Addr getMem(n)			       /* Get some more memory		   */
Int n; {
    Addr newAddr = addrHw;
    addrHw += n;
    if (addrHw>=NUM_ADDRS) {
	ERROR(0) "Program code storage space exhausted"
	EEND;
    }
    return newAddr;
}

/* --------------------------------------------------------------------------
 * Tycon storage:
 *
 * A Tycon represents a user defined type constructor.	Tycons are indexed
 * by Text values ... a very simple hash function is used to improve lookup
 * times.  Tycon entries with the same hash code are chained together, with
 * the most recent entry at the front of the list.
 * ------------------------------------------------------------------------*/

#define TYCONHSZ 256			/* Size of Tycon hash table	   */
#define tHash(x) ((x)%TYCONHSZ)		/* Tycon hash function		   */
static	Tycon	 tyconHw;		/* next unused Tycon		   */
static	Tycon	 tyconHash[TYCONHSZ];	/* Hash table storage		   */

struct	Tycon	 tabTycon[NUM_TYCON];	/* Tycon storage 		   */

Tycon newTycon(t)			/* add new tycon to tycon table	   */
Text t; {
    Int h = tHash(t);

    if (tyconHw-TYCMIN >= NUM_TYCON) {
	ERROR(0) "Type constructor storage space exhausted"
	EEND;
    }
    tycon(tyconHw).text 	 = t;	/* clear new tycon record	   */
    tycon(tyconHw).kind		 = NIL;
    tycon(tyconHw).defn 	 = NIL;
    tycon(tyconHw).what		 = NIL;
    tycon(tyconHw).nextTyconHash = tyconHash[h];
    tyconHash[h]		 = tyconHw;

    return tyconHw++;
}

Tycon findTycon(t)			/* locate Tycon in tycon table	   */
Text t; {
    Tycon tc = tyconHash[tHash(t)];

    while (nonNull(tc) && tycon(tc).text!=t)
	tc = tycon(tc).nextTyconHash;
    return tc;
}

Tycon addPrimTycon(s,kind,ar,what,defn) /* add new primitive type constr   */
String s;
Kind   kind;
Int    ar;
Cell   what;
Cell   defn; {
    Tycon tc	    = newTycon(findText(s));
    tycon(tc).line  = 0;
    tycon(tc).kind  = kind;
    tycon(tc).what  = what;
    tycon(tc).defn  = defn;
    tycon(tc).arity = ar;
    return tc;
}

/* --------------------------------------------------------------------------
 * Name storage:
 *
 * A Name represents a top level binding of a value to an identifier.
 * Such values may be any one of the following:
 *    CFUN   constructor function
 *    MFUN   member function in class
 *    NIL    user defined (or machine generated) compiled function
 *
 * Names are indexed by Text values ... a very simple hash functions speeds
 * access to the table of Names and Name entries with the same hash value
 * are chained together, with the most recent entry at the front of the
 * list.
 * ------------------------------------------------------------------------*/

#define NAMEHSZ  256			/* Size of Name hash table	   */
#define nHash(x) ((x)%NAMEHSZ)		/* Name hash function :: Text->Int */
static	Name	 nameHw;		/* next unused name		   */
static	Name	 nameHash[NAMEHSZ];	/* Hash table storage		   */

struct	Name	 tabName[NUM_NAME];	/* Name table storage		   */

Name newName(t)				/* add new name to name table	   */
Text t; {
    Int h = nHash(t);

    if (nameHw-NAMEMIN >= NUM_NAME) {
	ERROR(0) "Name storage space exhausted"
	EEND;
    }
    name(nameHw).text	      = t;	/* clear new name record 	   */
    name(nameHw).line	      = 0;
    name(nameHw).arity	      = 0;
    name(nameHw).number       = 0;
    name(nameHw).defn	      = NIL;
    name(nameHw).type	      = NIL;
    name(nameHw).primDef      = 0;
    name(nameHw).nextNameHash = nameHash[h];
    nameHash[h] 	      = nameHw;

    return nameHw++;
}

Name findName(t)			/* locate name in name table	   */
Text t; {
    Name n = nameHash[nHash(t)];

    while (nonNull(n) && name(n).text!=t)
	n = name(n).nextNameHash;
    return n;
}

Void addPrim(l,n,s,ty)			/* add primitive function value    */
Int    l;
Name   n;
String s;
Type   ty; {
    Int  i;

    name(n).line = l;
    name(n).defn = NIL;
    name(n).type = ty;

    for (i=0; primitives[i].ref; ++i)
        if (strcmp(s,primitives[i].ref)==0) {
	    name(n).arity   = primitives[i].arity;
	    name(n).number  = i;
	    name(n).primDef = primitives[i].imp;
	    return;
	}
    externalPrim(n,s);
}

Name addPrimCfun(s,arity,no,type)	/* add primitive constructor func. */
String s;
Int    arity;
Int    no;
Cell   type; {
    Name n	    = newName(findText(s));
    name(n).arity   = arity;
    name(n).number  = no;
    name(n).defn    = CFUN;
    name(n).type    = type;
    name(n).primDef = 0;
    return n;
}

static List local insertName(nm,ns)	/* insert name nm into sorted list */
Name nm;				/* ns				   */
List ns; {
    Cell   prev = NIL;
    Cell   curr = ns;
    String s    = textToStr(name(nm).text);

    while (nonNull(curr) && strcmp(s,textToStr(name(hd(curr)).text))>=0) {
	if (hd(curr)==nm)		/* just in case we get duplicates! */
	    return ns;
	prev = curr;
	curr = tl(curr);
    }
    if (nonNull(prev)) {
	tl(prev) = cons(nm,curr);
	return ns;
    }
    else
	return cons(nm,curr);
}

List addNamesMatching(pat,ns)		/* Add names matching pattern pat  */
String pat;				/* to list of names ns		   */
List   ns; {				/* Null pattern matches every name */
    Name nm;
    for (nm=NAMEMIN; nm<nameHw; ++nm)
	if (nonNull(name(nm).type) &&
	    (!pat || stringMatch(pat,textToStr(name(nm).text))))
	    ns = insertName(nm,ns);
    return ns;
}

/* --------------------------------------------------------------------------
 * A simple string matching routine
 *     `*'    matches any sequence of zero or more characters
 *     `?'    matches any single character exactly 
 *     `@str' matches the string str exactly (ignoring any special chars)
 *     `\c'   matches the character c only (ignoring special chars)
 *     c      matches the character c only
 * ------------------------------------------------------------------------*/

static Void local patternError(s)	/* report error in pattern	   */
String s; {
    ERROR(0) "%s in pattern", s
    EEND;
}

static Bool local stringMatch(pat,str)	/* match string against pattern	   */
String pat;
String str; {

    for (;;)
	switch (*pat) {
	    case '\0' : return (*str=='\0');

	    case '*'  : do {
			    if (stringMatch(pat+1,str))
				return TRUE;
			} while (*str++);
			return FALSE;

            case '?'  : if (*str++=='\0')
			    return FALSE;
			pat++;
			break;

            case '['  : {   Bool found = FALSE;
			    while (*++pat!='\0' && *pat!=']')
				if (!found && ( pat[0] == *str  ||
					       (pat[1] == '-'   &&
						pat[2] != ']'   &&
						pat[2] != '\0'  &&
						pat[0] <= *str  &&
						pat[2] >= *str)))
                                               
				    found = TRUE;
			    if (*pat != ']')
				patternError("missing `]'");
			    if (!found)
				return FALSE;
			    pat++;
			    str++;
			}
                        break;

	    case '\\' : if (*++pat == '\0')
			    patternError("extra trailing `\\'");
			/*fallthru!*/
	    default   : if (*pat++ != *str++)
			    return FALSE;
			break;
	}
}

/* --------------------------------------------------------------------------
 * Storage of type classes, instances etc...:
 * ------------------------------------------------------------------------*/

static Class classHw;		       /* next unused class		   */
static Inst  instHw;		       /* next unused instance record	   */
static Idx   idxHw;		       /* next unused index tree record    */
static Dict  dictHw;		       /* next unused dictionary slot	   */

struct Class	tabClass[NUM_CLASSES]; /* table of class records	   */
struct Inst far *tabInst;	       /* (pointer to) table of instances  */
struct Idx  far *tabIndex;	       /* (pointer to) table of indices    */
Cell	    far *tabDict;	       /* (pointer to) table of dict slots */

Class newClass(t)		       /* add new class to class table	   */
Text t; {
    if (classHw-CLASSMIN >= NUM_CLASSES) {
	ERROR(0) "Class storage space exhausted"
	EEND;
    }
    class(classHw).text      = t;
    class(classHw).sig	     = NIL;
    class(classHw).head	     = NIL;
    class(classHw).supers    = NIL;
    class(classHw).members   = NIL;
    class(classHw).defaults  = NIL;
    class(classHw).instances = NIL;
    class(classHw).dictIndex = NOIDX;

    return classHw++;
}

Class findClass(t)		       /* look for named class in table    */
Text t; {
    Class c;

    for (c=CLASSMIN; c<classHw; c++)
	if (class(c).text==t)
	    return c;
    return NIL;
}

Inst newInst() {		       /* add new instance to table	   */
    if (instHw-INSTMIN >= NUM_INSTS) {
	ERROR(0) "Instance storage space exhausted"
	EEND;
    }
    inst(instHw).head 	    = NIL;
    inst(instHw).specifics  = NIL;
    inst(instHw).implements = NIL;

    return instHw++;
}

Idx newIdx(test)		       /* Add node to index tree, with	   */
Cell test; {			       /* specified test value	 	   */
    if (idxHw >= NUM_INDEXES) {
	ERROR(0) "Index storage space exhausted"
	EEND;
    }
    idx(idxHw).test  = test;
    idx(idxHw).fail  = NOIDX;
    idx(idxHw).match = NODICT;

    return idxHw++;
}

Dict newDict(dictSize)		       /* Allocate dictionary of given size*/
Int dictSize; {
    Dict dictStarts = dictHw;

    if ((dictHw+=dictSize) > NUM_DICTS) {
	ERROR(0) "Dictionary storage space exhausted"
	EEND;
    }
    return dictStarts;
}

/* --------------------------------------------------------------------------
 * Control stack:
 *
 * Various parts of the system use a stack of cells.  Most of the stack
 * operations are defined as macros, expanded inline.
 * ------------------------------------------------------------------------*/

Cell	 cellStack[NUM_STACK];	       /* Storage for cells on stack	   */
#ifndef  GLOBALsp
StackPtr sp;			       /* stack pointer 		   */
#endif

Void stackOverflow() {		       /* Report stack overflow 	   */
    ERROR(0) "Control stack overflow"
    EEND;
}

/* --------------------------------------------------------------------------
 * Module storage:
 *
 * script files are read into the system one after another.  The state of
 * the stored data structures (except the garbage-collected heap) is recorded
 * before reading a new script.  In the event of being unable to read the
 * script, or if otherwise requested, the system can be restored to its
 * original state immediately before the file was read.
 * ------------------------------------------------------------------------*/

typedef struct {		       /* record of storage state prior to */
    Text  textHw;		       /* reading script/module 	   */
    Text  nextNewText;
    Text  nextNewDText;
    Int   syntaxHw;
    Addr  addrHw;
    Tycon tyconHw;
    Name  nameHw;
    Class classHw;
    Inst  instHw;
    Idx   idxHw;
    Dict  dictHw;
} module;

static Module moduleHw; 	       /* next unused module number	   */
static module modules[NUM_MODULES];    /* storage for module records	   */

Module startNewModule() {	       /* start new module, keeping record */
    if (moduleHw >= NUM_MODULES) {     /* of status for later restoration  */
	ERROR(0) "Too many script/module files in use"
	EEND;
    }
    modules[moduleHw].textHw	   = textHw;
    modules[moduleHw].nextNewText  = nextNewText;
    modules[moduleHw].nextNewDText = nextNewDText;
    modules[moduleHw].syntaxHw	   = syntaxHw;
    modules[moduleHw].addrHw	   = addrHw;
    modules[moduleHw].tyconHw	   = tyconHw;
    modules[moduleHw].nameHw	   = nameHw;
    modules[moduleHw].classHw	   = classHw;
    modules[moduleHw].instHw	   = instHw;
    modules[moduleHw].idxHw	   = idxHw;
    modules[moduleHw].dictHw	   = dictHw;
    return moduleHw++;
}

Bool nameThisModule(n)			/* Test if given name is defined in*/
Name n; {				/* current module		   */
    return moduleHw<1 || n>=modules[moduleHw-1].nameHw;
}

Module moduleThisName(nm)		/* find module number for name	   */
Name nm; {
    Module m;

    for (m=0; m<moduleHw && nm>=modules[m].nameHw; m++)
	;
    if (m>=moduleHw)
	internal("moduleThisName");
    return m;
}

Void dropModulesFrom(mno)		/* Restore storage to state prior  */
Module mno; {				/* to reading module mno 	   */
    if (mno<moduleHw) {			/* is there anything to restore?   */
	int i;
	textHw	     = modules[mno].textHw;
	nextNewText  = modules[mno].nextNewText;
	nextNewDText = modules[mno].nextNewDText;
	syntaxHw     = modules[mno].syntaxHw;
	addrHw	     = modules[mno].addrHw;
	tyconHw      = modules[mno].tyconHw;
	nameHw	     = modules[mno].nameHw;
	classHw      = modules[mno].classHw;
	instHw	     = modules[mno].instHw;
	idxHw	     = modules[mno].idxHw;
	dictHw	     = modules[mno].dictHw;

	for (i=0; i<TEXTHSZ; ++i) {
	    int j = 0;
	    while (j<NUM_TEXTH && textHash[i][j]!=NOTEXT
			       && textHash[i][j]<textHw)
		++j;
	    if (j<NUM_TEXTH)
		textHash[i][j] = NOTEXT;
	}

	for (i=0; i<TYCONHSZ; ++i) {
	    Tycon tc = tyconHash[i];
	    while (nonNull(tc) && tc>=tyconHw)
		tc = tycon(tc).nextTyconHash;
	    tyconHash[i] = tc;
	}

	for (i=0; i<NAMEHSZ; ++i) {
	    Name n = nameHash[i];
	    while (nonNull(n) && n>=nameHw)
		n = name(n).nextNameHash;
	    nameHash[i] = n;
	}

	for (i=CLASSMIN; i<classHw; i++) {
	    List in = class(i).instances;
	    List is = NIL;

	    if (class(i).dictIndex>=idxHw)
		class(i).dictIndex = NOIDX;

	    while (nonNull(in)) {
		List temp = tl(in);
		if (hd(in)<instHw) {
		    tl(in) = is;
		    is     = in;
		}
		in = temp;
	    }
	    class(i).instances = rev(is);
	}

	for (i=0; i<idxHw; ++i)
	    if (idx(i).fail>=idxHw)
		idx(i).fail = NOIDX;

	moduleHw = mno;
    }
}

/* --------------------------------------------------------------------------
 * Heap storage:
 * Provides a garbage collectable heap for storage of expressions etc.
 * ------------------------------------------------------------------------*/

Int     heapSize = DEFAULTHEAP;		/* number of cells in heap	   */
Heap    heapFst;			/* array of fst component of pairs */
Heap    heapSnd;			/* array of snd component of pairs */
#ifndef GLOBALfst
Heap    heapTopFst;
#endif
#ifndef GLOBALsnd
Heap    heapTopSnd;
#endif
Long    numCells;
Int     numberGcs;			/* number of garbage collections   */
Int	cellsRecovered;			/* number of cells recovered	   */

static  Cell freeList;			/* free list of unused cells	   */

Cell pair(l,r)				/* Allocate pair (l, r) from	   */
Cell l, r; {				/* heap, garbage collecting first  */
    Cell c = freeList;			/* if necessary ...		   */

    if (isNull(c)) {
	garbageCollect();
	c = freeList;
    }
    freeList = snd(freeList);
    fst(c)   = l;
    snd(c)   = r;
    numCells++;
    return c;
}

Void overwrite(dst,src)			/* overwrite dst cell with src cell*/
Cell dst, src; {			/* both *MUST* be pairs            */
    if (isPair(dst) && isPair(src)) {
        fst(dst) = fst(src);
        snd(dst) = snd(src);
    }
    else
        internal("overwrite");
}

static Int *marks;
static Int marksSize;

Cell markExpr(c)			/* External interface to markCell  */
Cell c; {
    return markCell(c);
}

static Cell local markCell(c)		/* Traverse part of graph marking  */
Cell c; {				/* cells reachable from given root */

mc: if (!isPair(c))
	return c;

    if (fst(c)==INDIRECT) {
	c = indirectChain(c);
	goto mc;
    }

    {   register place = placeInSet(c);
	register mask  = maskInSet(c);
	if (marks[place]&mask)
	    return c;
	else
	    marks[place] |= mask;
    }

    if (isPair(fst(c))) {
	fst(c) = markCell(fst(c));
	markSnd(c);
    }
    else if (isNull(fst(c)) || fst(c)>=BCSTAG)
	markSnd(c);

    return c;
}

static Void local markSnd(c)		/* Variant of markCell used to     */
Cell c; {				/* update snd component of cell    */
    Cell t;				/* using tail recursion		   */

ma: t = snd(c);
mb: if (!isPair(t))
	return;

    if (fst(t)==INDIRECT) {
	snd(c) = t = indirectChain(t);
	goto mb;
    }
    c = snd(c) = t;

    {   register place = placeInSet(c);
	register mask  = maskInSet(c);
	if (marks[place]&mask)
	    return;
	else
	    marks[place] |= mask;
    }

    if (isPair(fst(c))) {
	fst(c) = markCell(fst(c));
	goto ma;
    }
    else if (isNull(fst(c)) || fst(c)>=BCSTAG)
	goto ma;
    return;
}

static Cell local indirectChain(c)	/* Scan chain of indirections	   */
Cell c; {				/* Detecting loops of indirections */
    Cell is = c;			/* Uses pointer reversal ...	   */
    c       = snd(is);
    snd(is) = NIL;
    fst(is) = INDIRECT1;

    while (isPair(c) && fst(c)==INDIRECT) {
	register Cell temp = snd(c);
	snd(c)  = is;
	is      = c;
	c       = temp;
	fst(is) = INDIRECT1;
    }

    if (isPair(c) && fst(c)==INDIRECT1)
	c = nameBlackHole;

    do {
	register Cell temp = snd(is);
	fst(is) = INDIRECT;
	snd(is) = c;
	is	= temp;
    } while (nonNull(is));

    return c;
}

Void markWithoutMove(n)			/* Garbage collect cell at n, as if*/
Cell n; {				/* it was a cell ref, but don't    */
					/* move cell (i.e. retain INDIRECT */
					/* at top level) so we don't have  */
					/* to modify the stored value of n */
    if (isGenPair(n)) {
	if (fst(n)==INDIRECT) {		/* special case for indirections   */
	    register place = placeInSet(n);
	    register mask  = maskInSet(n);
	    marks[place]  |= mask;
	    markSnd(n);
	}
	else
	    markCell(n);		/* normal pairs don't move anyway  */
    }
}

Void garbageCollect() {			/* Run garbage collector ...	   */
    Bool breakStat = breakOn(FALSE);	/* disable break checking	   */
    Int i,j;
    register Int mask;
    register Int place;
    Int      recovered;
    jmp_buf  regs;			/* save registers on stack	   */
    setjmp(regs);

    gcStarted();
    for (i=0; i<marksSize; ++i)		/* initialise mark set to empty    */
	marks[i] = 0;
    everybody(MARK);			/* mark all components of system   */

    /* Just in case garbageCollect is triggered when free list is non-empty*/
    /* (called by openFile for example), scan the free list and unmark all */
    /* cells - which otherwise might have been marked from the Cstack      */
    for (; nonNull(freeList); freeList=snd(freeList))
	marks[placeInSet(freeList)] &= ~(maskInSet(freeList));

    gcScanning();			/* scan mark set		   */
    mask      = 1;
    place     = 0;
    recovered = 0;
    j         = 0;
    for (i=1; i<=heapSize; i++) {
	if ((marks[place] & mask) == 0) {
	    if (fst(-i)==FILECELL) {
		closeFile(intValOf(-i));
		fst(-i) = INTCELL;
	    }
	    snd(-i)  = freeList;
	    freeList = -i;
	    recovered++;
	}
	mask <<= 1;
	if (++j == bitsPerWord) {
	    place++;
	    mask = 1;
	    j    = 0;
	}
    }
    gcRecovered(recovered);

    breakOn(breakStat);			/* restore break trapping if nec.  */

    /* can only return if freeList is nonempty on return. */
    if (recovered<minRecovery || isNull(freeList)) {
	ERROR(0) "Garbage collection fails to reclaim sufficient space"
	EEND;
    }
    numberGcs++;
    cellsRecovered = recovered;
}

/* --------------------------------------------------------------------------
 * Code for saving last expression entered:
 *
 * This is a little tricky since some text values (e.g. strings or variable
 * names) may not be defined or have the same value when the expression is
 * recalled.  These text values are therefore saved in the top portion of
 * the text table.
 * ------------------------------------------------------------------------*/

static Cell lastExprSaved;		/* last expression to be saved	   */

Void setLastExpr(e)			/* save expression for later recall*/
Cell e; {
    lastExprSaved = NIL;		/* in case attempt to save fails   */
    savedText	  = NUM_TEXT;
    lastExprSaved = lowLevelLastIn(e);
}

static Cell local lowLevelLastIn(c)	/* Duplicate expression tree (i.e. */
Cell c; {				/* acyclic graph) for later recall */
    if (isPair(c))			/* Duplicating any text strings    */
	if (isBoxTag(fst(c)))		/* in case these are lost at some  */
	    switch (fst(c)) {		/* point before the expr is reused */
		case VARIDCELL :
		case VAROPCELL :
		case DICTVAR   :
		case CONIDCELL :
		case CONOPCELL :
		case STRCELL   : return pair(fst(c),saveText(textOf(c)));
		default	       : return pair(fst(c),snd(c));
	    }
	else
	    return pair(lowLevelLastIn(fst(c)),lowLevelLastIn(snd(c)));
    else
	return c;
}

Cell getLastExpr() {			/* recover previously saved expr   */
    return lowLevelLastOut(lastExprSaved);
}

static Cell local lowLevelLastOut(c)	/* As with lowLevelLastIn() above  */
Cell c; {				/* except that Cells refering to   */
    if (isPair(c))			/* Text values are restored to	   */
	if (isBoxTag(fst(c)))		/* appropriate values		   */
	    switch (fst(c)) {
		case VARIDCELL :
		case VAROPCELL :
		case DICTVAR   :
		case CONIDCELL :
		case CONOPCELL :
		case STRCELL   : return pair(fst(c),
					     findText(text+intValOf(c)));
		default	       : return pair(fst(c),snd(c));
	    }
	else
	    return pair(lowLevelLastOut(fst(c)),lowLevelLastOut(snd(c)));
    else
	return c;
}

/* --------------------------------------------------------------------------
 * Miscellaneous operations on heap cells:
 * ------------------------------------------------------------------------*/

/* profiling suggests that the number of calls to whatIs() is typically    */
/* rather high.  The recoded version below attempts to improve the average */
/* performance for whatIs() using a binary search for part of the analysis */

Cell whatIs(c)			       /* identify type of cell 	   */
register Cell c; {
    if (isPair(c)) {
	register Cell fstc = fst(c);
	return isTag(fstc) ? fstc : AP;
    }
    if (c<TUPMIN)    return c;
    if (c>=INTMIN)   return INTCELL;

    if (c>=SELMIN)  if (c>=CLASSMIN)	if (c>=CHARMIN) return CHARCELL;
					else		return CLASS;
		    else		if (c>=INSTMIN) return INSTANCE;
					else		return SELECT;
    else	    if (c>=TYCMIN)	if (c>=NAMEMIN)	return NAME;
					else		return TYCON;
		    else		if (c>=OFFMIN)	return OFFSET;
					else		return TUPLE;

/*  if (c>=CHARMIN)  return CHARCELL;
    if (c>=CLASSMIN) return CLASS;
    if (c>=INSTMIN)  return INSTANCE;
    if (c>=SELMIN)   return SELECT;
    if (c>=NAMEMIN)  return NAME;
    if (c>=TYCMIN)   return TYCON;
    if (c>=OFFMIN)   return OFFSET;
    if (c>=TUPMIN)   return TUPLE;
    return c;*/
}

Bool isVar(c)				/* is cell a VARIDCELL/VAROPCELL ? */
Cell c; {				/* also recognises DICTVAR cells   */
    return isPair(c) &&
	       (fst(c)==VARIDCELL || fst(c)==VAROPCELL || fst(c)==DICTVAR);
}

Bool isCon(c)			       /* is cell a CONIDCELL/CONOPCELL ?  */
Cell c; {
    return isPair(c) && (fst(c)==CONIDCELL || fst(c)==CONOPCELL);
}

Bool isInt(c)			       /* cell holds integer value?	   */
Cell c; {
    return isSmall(c) || (isPair(c) && fst(c)==INTCELL);
}

Int intOf(c)			       /* find integer value of cell?	   */
Cell c; {
    return isPair(c) ? (Int)(snd(c)) : (Int)(c-INTZERO);
}

Cell mkInt(n)			       /* make cell representing integer   */
Int n; {
    return isSmall(INTZERO+n) ? INTZERO+n : pair(INTCELL,n);
}

/* --------------------------------------------------------------------------
 * List operations:
 * ------------------------------------------------------------------------*/

Int length(xs)			       /* calculate length of list xs	   */
List xs; {
    Int n = 0;
    for (n=0; nonNull(xs); ++n)
	xs = tl(xs);
    return n;
}

List appendOnto(xs,ys)		       /* Destructively prepend xs onto    */
List xs, ys; {			       /* ys by modifying xs ...	   */
    if (isNull(xs))
	return ys;
    else {
	List zs = xs;
	while (nonNull(tl(zs)))
	    zs = tl(zs);
	tl(zs) = ys;
	return xs;
    }
}

List dupList(xs)		       /* Duplicate spine of list xs	   */
List xs; {
    List ys = NIL;
    for (; nonNull(xs); xs=tl(xs))
	ys = cons(hd(xs),ys);
    return rev(ys);
}

List revOnto(xs,ys)		       /* Destructively reverse elements of*/
List xs, ys; {			       /* list xs onto list ys...	   */
    Cell zs;

    while (nonNull(xs)) {
	zs     = tl(xs);
	tl(xs) = ys;
	ys     = xs;
	xs     = zs;
    }
    return ys;
}

Cell varIsMember(t,xs)		       /* Test if variable is a member of  */
Text t; 			       /* given list of variables	   */
List xs; {
    for (; nonNull(xs); xs=tl(xs))
	if (t==textOf(hd(xs)))
	    return hd(xs);
    return NIL;
}

Cell cellIsMember(x,xs) 	       /* Test for membership of specific  */
Cell x; 			       /* cell x in list xs		   */
List xs; {
    for (; nonNull(xs); xs=tl(xs))
	if (x==hd(xs))
	    return hd(xs);
    return NIL;
}

List copy(n,x)			       /* create list of n copies of x	   */
Int n;
Cell x; {
    List xs=NIL;
    while (0<n--)
	xs = cons(x,xs);
    return xs;
}

List diffList(from,take)	       /* list difference: from\take	   */
List from, take; {		       /* result contains all elements of  */
    List result = NIL;		       /* `from' not appearing in `take'   */

    while (nonNull(from)) {
	List next = tl(from);
	if (!cellIsMember(hd(from),take)) {
	    tl(from) = result;
	    result   = from;
	}
	from = next;
    }
    return rev(result);
}

List take(n,xs)				/* destructively trancate list to  */
Int  n;					/* specified length		   */
List xs; {
    List start = xs;

    if (n==0)
	return NIL;
    while (1<n-- && nonNull(xs))
	xs = tl(xs);
    if (nonNull(xs))
	tl(xs) = NIL;
    return start;
}

List removeCell(x,xs)			/* destructively remove cell from  */
Cell x;					/* list				   */
List xs; {
    if (nonNull(xs)) {
	if (hd(xs)==x)
	    return tl(xs);		/* element at front of list	   */
	else {
	    List prev = xs;
	    List curr = tl(xs);
	    for (; nonNull(curr); prev=curr, curr=tl(prev))
		if (hd(curr)==x) {
		    tl(prev) = tl(curr);
		    return xs;		/* element in middle of list	   */
		}
	}
    }
    return xs;				/* here if element not found	   */
}

/* --------------------------------------------------------------------------
 * Operations on applications:
 * ------------------------------------------------------------------------*/

Int argCount;			       /* number of args in application    */

Cell getHead(e) 		       /* get head cell of application	   */
Cell e; {			       /* set number of args in argCount   */
    for (argCount=0; isAp(e); e=fun(e))
	argCount++;
    return e;
}

List getArgs(e) 		       /* get list of arguments in function*/
Cell e; {			       /* application:			   */
    List as;			       /* getArgs(f e1 .. en) = [e1,..,en] */

    for (as=NIL; isAp(e); e=fun(e))
	as = cons(arg(e),as);
    return as;
}

Cell nthArg(n,e)		       /* return nth arg in application    */
Int  n;				       /* of function to m args (m>=n)     */
Cell e; {                              /* nthArg n (f x0 x1 ... xm) = xn   */
    for (n=numArgs(e)-n-1; n>0; n--)
	e = fun(e);
    return arg(e);
}

Int numArgs(e)			       /* find number of arguments to expr */
Cell e; {
    Int n;
    for (n=0; isAp(e); e=fun(e))
	n++;
    return n;
}

Cell applyToArgs(f,args)	       /* destructively apply list of args */
Cell f;				       /* to function f			   */
List args; {
    while (nonNull(args)) {
	Cell temp = tl(args);
	tl(args)  = hd(args);
	hd(args)  = f;
	f	  = args;
	args	  = temp;
    }
    return f;
}

/* --------------------------------------------------------------------------
 * File operations:
 * ------------------------------------------------------------------------*/

static FILE *infiles[NUM_FILES];	/* file pointers for input files   */

Cell openFile(s)			/* create FILECELL object for named*/
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
	ERROR(0) "Too many files open; cannot open %s", s
	EEND;
    }

    if (infiles[i]=fopen(s,"r"))
	return ap(FILECELL,i);
    else
	return NIL;
}

Void evalFile(f)				/* read char from given    */
Cell f; {					/* input file -- ensure	   */
    Int c;					/* only 1 copy of FILECELL */
    if ((c = fgetc(infiles[intValOf(f)]))==EOF) {
	closeFile(intValOf(f));
	fst(f) = INDIRECT;
	snd(f) = nameNil;
    }
    else {
	snd(f) = ap(FILECELL,intValOf(f));
	fst(f) = NIL;	/* avoid having 2 copies of FILECELL, so that file */
			/* is not closed prematurely by garbage collector  */
	fst(f) = consChar(c);
    }
}

static Void local closeFile(n)			/* close input file n	   */
Int n; {					/* only permitted when the */
    if (0<=n && n<NUM_FILES && infiles[n]) {	/* end of file is read or  */
	fclose(infiles[n]);			/* when discarded during gc*/
	infiles[n] = 0;
    }
}

/* --------------------------------------------------------------------------
 * storage control:
 * ------------------------------------------------------------------------*/

Void storage(what)
Int what; {
    Int i;

    switch (what) {
	case RESET   : clearStack();

		       /* the next 2 statements are particularly important
		        * if you are using GLOBALfst or GLOBALsnd since the
			* corresponding registers may be reset to their
			* uninitialised initial values by a longjump.
			*/
		       heapTopFst = heapFst + heapSize;
		       heapTopSnd = heapSnd + heapSize;

		       if (isNull(lastExprSaved))
			   savedText = NUM_TEXT;
		       break;

	case MARK    : for (i=TYCMIN; i<tyconHw; ++i) {
			   mark(tycon(i).defn);
			   mark(tycon(i).kind);
			   mark(tycon(i).what);
		       }

		       for (i=NAMEMIN; i<nameHw; ++i) {
			   mark(name(i).defn);
			   mark(name(i).type);
		       }

		       for (i=CLASSMIN; i<classHw; ++i) {
			   mark(class(i).sig);
			   mark(class(i).head);
			   mark(class(i).supers);
			   mark(class(i).members);
			   mark(class(i).defaults);
                           mark(class(i).instances);
		       }

		       for (i=INSTMIN; i<instHw; ++i) {
			   mark(inst(i).sig);
			   mark(inst(i).head);
			   mark(inst(i).specifics);
			   mark(inst(i).implements);
		       }

		       for (i=0; i<=sp; ++i)
			   mark(stack(i));

                       for (i=0; i<dictHw; ++i)
                           mark(dict(i));

		       mark(lastExprSaved);

                       gcCStack();

		       break;

	case INSTALL : clearStack();

		       for (i=0; i<NUM_FILES; i++)
			   infiles[i] = 0;

		       heapFst = heapAlloc(heapSize);
		       heapSnd = heapAlloc(heapSize);

		       if (heapFst==(Heap)0 || heapSnd==(Heap)0) {
			   ERROR(0) "Cannot allocate heap storage (%d cells)",
				    heapSize
			   EEND;
		       }

		       heapTopFst = heapFst + heapSize;
		       heapTopSnd = heapSnd + heapSize;

		       for (i=1; i<heapSize; ++i)
			   snd(-i) = -(i+1);
		       snd(-heapSize) = NIL;
		       freeList       = -1;
		       numberGcs      = 0;

		       marksSize  = bitArraySize(heapSize);
		       if ((marks=(Int *)calloc(marksSize, sizeof(Int)))==0) {
			   ERROR(0) "Unable to allocate gc markspace"
			   EEND;
		       }

		       textHw	     = 0;
		       nextNewText   = NUM_TEXT;
		       nextNewDText  = (-1);
		       lastExprSaved = NIL;
		       savedText     = NUM_TEXT;
		       for (i=0; i<TEXTHSZ; ++i)
			   textHash[i][0] = NOTEXT;

		       syntaxHw = 0;

		       addrHw	= 0;

		       tyconHw	= TYCMIN;
		       for (i=0; i<TYCONHSZ; ++i)
			   tyconHash[i] = NIL;

		       nameHw = NAMEMIN;
		       for (i=0; i<NAMEHSZ; ++i)
			   nameHash[i] = NIL;

		       classHw	= CLASSMIN;

		       instHw	= INSTMIN;

		       idxHw	= 0;

		       dictHw	= 0;

		       tabInst	= (struct Inst far *)
				    farCalloc(NUM_INSTS,sizeof(struct Inst));
		       tabIndex = (struct Idx far *)
				    farCalloc(NUM_INDEXES,sizeof(struct Idx));
		       tabDict	= (Cell far *)
				    farCalloc(NUM_DICTS,sizeof(Cell));

		       if (tabInst==0 || tabIndex==0 || tabDict==0) {
			   ERROR(0) "Cannot allocate instance tables"
			   EEND;
		       }

		       moduleHw = 0;

		       break;
    }
}

/*-------------------------------------------------------------------------*/
