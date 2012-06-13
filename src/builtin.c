/* --------------------------------------------------------------------------
 * builtin.c:   Copyright (c) Mark P Jones 1991-1994.   All rights reserved.
 *              See goferite.h for details and conditions of use etc...
 *              Gofer version 2.30 March 1994
 *
 * Primitive functions, input output etc...
 * ------------------------------------------------------------------------*/

#define  NEED_MATH
#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"

Name nameFatbar, nameFail;		/* primitives reqd for translation */
Name nameIf,	 nameSel;
Name nameMinus,  nameDivide;

Name nameUndefMem;			/* undefined member primitive	   */
Name nameError;				/* error primitive function	   */
Name nameBlackHole;			/* for GC-detected black hole	   */

Name nameAnd,    nameOr;		/* built-in logical connectives	   */
Name nameOtherwise;

Name namePrint,  nameNPrint;		/* primitives for printing	   */

#if    HASKELL_ARRAYS
static Name nameEltUndef;		/* undefined element in array	   */
static Name nameOutBounds;		/* value of of bounds		   */
#endif
#if    IO_MONAD
Name   nameSTRun;			/* encapsulation operator for IO   */
static Name nameFst;			/* fst primitive		   */
static Name nameSnd;			/* snd primitive		   */
#endif
#ifdef LAMBDAVAR
static Name nameLvUnbound;		/* unbound mutable variable	   */
#endif
#ifdef LAMBDANU
static Name nameLnUnbound;		/* unbound mutable variable	   */
static Name nameLnNocont;		/* unspecified continuation	   */
static Name nameLnFlip;			/* simple flip primitive	   */
static Name nameLnDone;			/* simple finishing continuation   */
#endif

/* --------------------------------------------------------------------------
 * Built-in primitives:
 * ------------------------------------------------------------------------*/

#define PRIMITIVES_CODE 1		/* want to include code for prims  */
#include "prims.c"

/* --------------------------------------------------------------------------
 * Built-in control:
 * ------------------------------------------------------------------------*/

Void builtIn(what)
Int what; {
    Int i;

    switch (what) {
#if IO_DIALOGUE
	case RESET   : if (writingFile) {
			   fclose(writingFile);
			   writingFile = 0;
		       }
		       break;
#endif

	case MARK    : for (i=0; i<NUM_CHARS; ++i)
			   mark(consCharArray[i]);
		       break;

	case INSTALL : for (i=0; i<NUM_CHARS; ++i)
			   consCharArray[i] = ap(nameCons,mkChar(i));

		       consOpen       = consCharArray['('];
		       consSpace      = consCharArray[' '];
		       consComma      = consCharArray[','];
		       consClose      = consCharArray[')'];
		       consObrace     = consCharArray['{'];
		       consCbrace     = consCharArray['}'];
		       consOsq	      = consCharArray['['];
		       consCsq	      = consCharArray[']'];
		       consBack       = consCharArray['`'];
		       consMinus      = consCharArray['-'];
		       consQuote      = consCharArray['\''];
		       consDQuote     = consCharArray['\"'];

#define pFun(n,s,t)    addPrim(0,n=newName(findText(s)),t,NIL)
		       pFun(nameFatbar,	   "_FATBAR", "primFatbar");
		       pFun(nameFail,	   "_FAIL",   "primFail");
		       pFun(nameIf,	   "_IF",     "primIf");
		       pFun(nameSel,	   "_SEL",    "primSel");

		       pFun(nameMinus,     "_minus",  "primMinusInt");
		       pFun(nameDivide,	   "_divide", "primDivInt");

		       pFun(namePrimCmp,   "_compare", "primCompare");
		       pFun(namePrint,	   "_print",   "primPrint");
		       pFun(nameNPrint,	   "_nprint",  "primNprint");
		       pFun(nameLPrint,	   "_lprint",  "primLprint");
		       pFun(nameNLPrint,   "_nlprint", "primNlprint");
		       pFun(nameSPrint,	   "_sprint",  "primSprint");
		       pFun(nameNSPrint,   "_nsprint", "primNsprint");
#if IO_DIALOGUE
		       pFun(nameInput,	   "_input",   "primInput");
#endif
		       pFun(nameUndefMem,  "_undefined_member", "primUndefMem");
		       pFun(nameBlackHole, "Gc Black Hole", "primGCBhole");
#if    HASKELL_ARRAYS
		       pFun(nameEltUndef,  "_undefined_array_element",
							"primEltUndef");
		       pFun(nameOutBounds, "_out_of_bounds","primOutBounds");
#endif
#if    IO_MONAD
		       pFun(nameSTRun,	   "runST",	"primSTRun");
		       pFun(nameFst,	   "_fst",	"primFst");
		       pFun(nameSnd,	   "_snd",	"primSnd");
#endif
#ifdef LAMBDAVAR
		       pFun(nameLvUnbound, "Unbound mutable variable",
							"primLvUnbound");
#endif
#ifdef LAMBDANU
		       pFun(nameLnUnbound, "Unbound mutable variable",
							"primLnUnbound");
		       pFun(nameLnNocont,  "Unspecified continuation",
							"primLnNocont");
		       pFun(nameLnFlip,	   "_LambdaNuFlip", "primLnFlip");
		       pFun(nameLnDone,	   "_LambdaNuDone", "primLnDone");
#endif
#undef pFun
#define predef(nm,str) nm=newName(findText(str)); name(nm).defn=PREDEFINED
		       predef(nameAnd,		"&&");
		       predef(nameOr,		"||");
		       predef(nameOtherwise,	"otherwise");
		       predef(nameError,	"error");
#undef  predef
		       break;
    }
}

/*-------------------------------------------------------------------------*/
