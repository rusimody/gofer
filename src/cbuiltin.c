/* --------------------------------------------------------------------------
 * cbuiltin.c:  Copyright (c) Mark P Jones 1991-1994.   All rights reserved.
 *              See goferite.h for details and conditions of use etc...
 *              Gofer version 2.30 March 1994
 *
 * Stub module for compiler
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

#if    IO_MONAD
Name nameSTRun;				/* encapsulation operator for IO   */
#endif

/* --------------------------------------------------------------------------
 * Built-in primitives:
 * ------------------------------------------------------------------------*/

#define PRIMITIVES_CODE 0		/* don't need prims code	   */
#include "prims.c"

/* --------------------------------------------------------------------------
 * Built-in control:
 * ------------------------------------------------------------------------*/

Void builtIn(what)
Int what; {
    Int i;

    switch (what) {
	case MARK    : for (i=0; i<NUM_CHARS; ++i)
			   mark(consCharArray[i]);
		       break;

	case INSTALL : for (i=0; i<NUM_CHARS; ++i)
			   consCharArray[i] = ap(nameCons,mkChar(i));

#define pFun(n,s,t)    addPrim(0,n=newName(findText(s)),t,NIL)
		       pFun(nameFatbar,	   "_FATBAR",	"primFatbar");
		       pFun(nameFail,	   "_FAIL",	"primFail");
		       pFun(nameIf,	   "_IF",	"primIf");
		       pFun(nameSel,	   "_SEL",	"primSel");
		       pFun(nameMinus,     "_minus",	"primMinusInt");
		       pFun(nameDivide,	   "_divide",	"primDivInt");
		       pFun(nameUndefMem,  "_undefined_member", "primUndefMem");
#if    IO_MONAD
		       pFun(nameSTRun,	   "runST",	"primSTRun");
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
