/* --------------------------------------------------------------------------
 * output.c:    Copyright (c) Mark P Jones 1991-1994.   All rights reserved.
 *              See goferite.h for details and conditions of use etc...
 *              Gofer version 2.30 March 1994
 *
 * Unparse expressions and types - for use in error messages, type checker
 * and for debugging.
 * ------------------------------------------------------------------------*/

#ifndef  GOFC_OUTPUT
#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include <ctype.h>
#endif

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static Void local putChr	 Args((Int));
static Void local putStr	 Args((String));
static Void local putInt	 Args((Int));
static Void local indent	 Args((Int));

static Void local put	         Args((Int,Cell));
static Void local putComp	 Args((Cell,List));
static Void local putQual        Args((Cell));
static Bool local isDictVal	 Args((Cell));
static Cell local maySkipDict	 Args((Cell));
static Void local putAp		 Args((Int,Cell));
static Void local putOverInfix   Args((Int,Text,Syntax,Cell));
static Void local putInfix	 Args((Int,Text,Syntax,Cell,Cell));
static Void local putSimpleAp	 Args((Cell));
static Void local putTuple	 Args((Int,Cell));
static Int  local unusedTups	 Args((Int,Cell));
static Void local unlexVar	 Args((Text));
static Void local unlexOp	 Args((Text));
static Void local unlexCharConst Args((Cell));
static Void local unlexStrConst	 Args((Text));

#ifdef GOFC_OUTPUT
static Void local pPut		 Args((Int,Cell,Int));
static Void local pPutAp	 Args((Int,Cell,Int));
static Void local pPutSimpleAp	 Args((Cell,Int));
static Void local pPutTuple	 Args((Int,Cell,Int));
static Int  local punusedTups	 Args((Int,Cell,Int));
static Void local pPutOffset	 Args((Int));
static Int  local pPutLocals	 Args((List,Int));
static Void local pLiftedStart	 Args((Cell,Int,String));
static Void local pLifted	 Args((Cell,Int,String));
static Int  local pDiscr	 Args((Cell,Int));
#endif

static Void local putSigType	 Args((Cell));
static Void local putContext	 Args((List));
static Void local putPred	 Args((Cell));
static Void local putType	 Args((Cell,Int));
static Void local putTyVar	 Args((Int));
static Bool local putTupleType   Args((Cell));
static Void local putApType	 Args((Type));

static Void local putKind	 Args((Kind));
static Void local putSig	 Args((Cell));

/* --------------------------------------------------------------------------
 * Basic output routines:
 * ------------------------------------------------------------------------*/

static FILE *outputStream;		/* current output stream	   */
static Int  outColumn = 0;		/* current output column number	   */
Bool   showDicts = FALSE;		/* TRUE => include dictionary vars */
					/*	   in output expressions   */

#define OPEN(b)    if (b) putChr('(');
#define CLOSE(b)   if (b) putChr(')');

static Void local putChr(c)             /* print single character          */
Int c; {
    putc(c,outputStream);
    outColumn++;
}

static Void local putStr(s)             /* print string                    */
String s; {
    for (; *s; s++) {
        putc(*s,outputStream);
        outColumn++;
    }
}

static Void local putInt(n)             /* print integer                   */
Int n; {
    static char intBuf[16];
    sprintf(intBuf,"%d",n);
    putStr(intBuf);
}

static Void local indent(n)             /* indent to particular position   */
Int n; {
    outColumn = n;
    while (0<n--) {
        putc(' ',outputStream);
    }
}

/* --------------------------------------------------------------------------
 * Precedence values (See Haskell report p.10):
 * ------------------------------------------------------------------------*/

#define ALWAYS	    FUN_PREC	       /* Always use parens (unless atomic)*/
				       /* User defined operators have prec */
				       /* in the range MIN_PREC..MAX_PREC  */
#define ARROW_PREC  MAX_PREC           /* for printing -> in type exprs    */
#define COCO_PREC   (MIN_PREC-1)       /* :: is left assoc, low precedence */
#define COND_PREC   (MIN_PREC-2)       /* conditional expressions	   */
#define WHERE_PREC  (MIN_PREC-3)       /* where expressions		   */
#define LAM_PREC    (MIN_PREC-4)       /* lambda abstraction		   */
#define NEVER	    LAM_PREC	       /* Never use parentheses 	   */


/* --------------------------------------------------------------------------
 * Print an expression (used to display context of type errors):
 * ------------------------------------------------------------------------*/

static Int putDepth = 0;	       /* limits depth of printing DBG	   */

static Void local put(d,e)	       /* print expression e in context of */
Int  d; 			       /* operator of precedence d	   */
Cell e; {
    List xs;

    if (putDepth>10) {
	putStr("...");
	return;
    }
    else
	putDepth++;

    switch (whatIs(e)) {
	case FINLIST	: putChr('[');
			  xs = snd(e);
			  if (nonNull(xs)) {
			      put(NEVER,hd(xs));
			      while (nonNull(xs=tl(xs))) {
				  putChr(',');
				  put(NEVER,hd(xs));
			      }
			  }
			  putChr(']');
			  break;

	case AP 	: putAp(d,e);
			  break;

	case NAME	: unlexVar(name(e).text);
			  break;

	case VARIDCELL	:
	case VAROPCELL	:
	case DICTVAR    :
	case CONIDCELL	:
	case CONOPCELL	: unlexVar(textOf(e));
			  break;

	case DICTCELL   : putStr("{dict");
			  putInt(dictOf(e));
			  putChr('}');
			  break;

	case SELECT	: putStr("#");
			  putInt(selectOf(e));
			  break;

	case UNIT	: putStr("()");
			  break;

	case TUPLE	: putTuple(tupleOf(e),e);
			  break;

	case WILDCARD	: putChr('_');
			  break;

	case ASPAT	: put(NEVER,fst(snd(e)));
			  putChr('@');
			  put(ALWAYS,snd(snd(e)));
			  break;

	case LAZYPAT	: putChr('~');
			  put(ALWAYS,snd(e));
			  break;

	case MONADCOMP	: putComp(fst(snd(snd(e))),snd(snd(snd(e))));
			  break;

#if DO_COMPS
	case DOCOMP	: putStr("do {...}");
			  break;
#endif

	case COMP	:
	case LISTCOMP   : putComp(fst(snd(e)),snd(snd(e)));
			  break;

	case CHARCELL	: unlexCharConst(charOf(e));
			  break;

	case INTCELL	: putInt(intOf(e));
			  break;

        case FLOATCELL  : putStr(floatToString(floatOf(e)));
			  break;

	case STRCELL	: unlexStrConst(textOf(e));
			  break;

	case LETREC	: OPEN(d>WHERE_PREC);
#ifdef DEBUG_CODE
			  putStr("let {");
			  put(NEVER,fst(snd(e)));
			  putStr("} in ");
#else
                          putStr("let {...} in ");
#endif
                          put(WHERE_PREC+1,snd(snd(e)));
			  CLOSE(d>WHERE_PREC);
			  break;

	case COND	: OPEN(d>COND_PREC);
			  putStr("if ");
			  put(COND_PREC+1,fst3(snd(e)));
			  putStr(" then ");
			  put(COND_PREC+1,snd3(snd(e)));
			  putStr(" else ");
			  put(COND_PREC+1,thd3(snd(e)));
			  CLOSE(d>COND_PREC);
			  break;

#if IO_MONAD
	case RUNST	: OPEN(d>=FUN_PREC);
			  putStr("runST ");
			  put(ALWAYS,snd(e));
			  CLOSE(d>=FUN_PREC);
			  break;
#endif

	case LAMBDA	: xs = fst(snd(e));
			  if (!showDicts) {
			      while (nonNull(xs) && isDictVal(hd(xs)))
				  xs = tl(xs);
			      if (isNull(xs)) {
				  put(d,snd(snd(snd(e))));
				  break;
			      }
			  }
			  OPEN(d>LAM_PREC);
			  putChr('\\');
			  if (nonNull(xs)) {
			      put(ALWAYS,hd(xs));
			      while (nonNull(xs=tl(xs))) {
				  putChr(' ');
				  put(ALWAYS,hd(xs));
			      }
			  }
			  putStr(" -> ");
			  put(LAM_PREC,snd(snd(snd(e))));
			  CLOSE(d>LAM_PREC);
			  break;

	case ESIGN	: OPEN(d>COCO_PREC);
			  put(COCO_PREC,fst(snd(e)));
			  putStr(" :: ");
			  putSigType(snd(snd(e)));
			  CLOSE(d>COCO_PREC);
			  break;

	case CASE	: putStr("case ");
			  put(NEVER,fst(snd(e)));
#ifdef DEBUG_CODE
			  putStr(" of {");
			  put(NEVER,snd(snd(e)));
			  putChr('}');
#else
			  putStr(" of {...}");
#endif
			  break;

	case INDIRECT	: putChr('^');
			  put(ALWAYS,snd(e));
			  break;

	default 	: /*internal("put");*/
			  putChr('$');
			  putInt(e);
			  break;
    }
    putDepth--;
}

static Void local putComp(e,qs)		/* print comprehension		   */
Cell e;
List qs; {
    putStr("[ ");
    put(NEVER,e);
    if (nonNull(qs)) {
	putStr(" | ");
	putQual(hd(qs));
	while (nonNull(qs=tl(qs))) {
	    putStr(", ");
	    putQual(hd(qs));
	}
    }
    putStr(" ]");
}

static Void local putQual(q)		/* print list comp qualifier	   */
Cell q; {
    switch (whatIs(q)) {
	case BOOLQUAL : put(NEVER,snd(q));
			return;

	case QWHERE   : putStr("let {...}");
			return;

	case FROMQUAL : put(ALWAYS,fst(snd(q)));
			putStr("<-");
			put(NEVER,snd(snd(q)));
			return;
    }
}

static Bool local isDictVal(e)		/* Look for dictionary value	   */
Cell e; {
    switch (whatIs(e)) {
	case AP	      : return isSelect(fun(e));
	case DICTCELL :
	case DICTVAR  : return TRUE;
    }
    return FALSE;
}

static Cell local maySkipDict(e)	/* descend function application	   */
Cell e; {				/* possibly ignoring dict aps	   */
    if (!showDicts)
	while (isAp(e) && isDictVal(arg(e)))
	    e = fun(e);
    return e;
}

static Void local putAp(d,e)		/* print application (args>=1)	   */
Int  d;
Cell e; {
    Bool   anyDictArgs = FALSE;
    Cell   h;
    Text   t;
    Syntax sy;
    Int    args = 0;

    for (h=e; isAp(h); h=fun(h))	/* find head of expression, looking*/
	if (isDictVal(arg(h))) {	/* for dictionary arguments	   */
	    anyDictArgs = TRUE;
	    if (showDicts)
		args++;
	}
	else
	    args++;

    if (args==0) {			/* Special case when *all* args	   */
	put(d,h);			/* are dictionary values	   */
	return;
    }

    switch (whatIs(h)) {
#if NPLUSK
	case ADDPAT	: if (args==1)
			      putInfix(d,textPlus,syntaxOf(textPlus),
					 arg(e),mkInt(intValOf(fun(e))));
			  else
			      putStr("ADDPAT");
			  return;

	case MULPAT	: if (args==1)
			      putInfix(d,textMult,syntaxOf(textMult),
					 mkInt(intValOf(fun(e))),arg(e));
			  else
			      putStr("MULPAT");
			  return;
#endif

	case TUPLE	: OPEN(args>tupleOf(h) && d>=FUN_PREC);
			  putTuple(tupleOf(h),e);
			  CLOSE(args>tupleOf(h) && d>=FUN_PREC);
			  return;

	case NAME	: sy = syntaxOf(t = name(h).text);
			  break;
	case VARIDCELL	:
	case VAROPCELL	:
	case DICTVAR    :
	case CONIDCELL	:
	case CONOPCELL	: sy = syntaxOf(t = textOf(h));
			  break;

	default 	: sy = APPLIC;
			  break;
    }

    e = maySkipDict(e);
    if (showDicts && anyDictArgs)	/* expressions involving dicts	   */
	sy = APPLIC;			/* are printed applicatively	   */

    if (sy==APPLIC) {		       	/* print simple application	   */
	OPEN(d>=FUN_PREC);
	putSimpleAp(e);
	CLOSE(d>=FUN_PREC);
	return;
    }
    else if (args==1) {		        /* print section of the form (e+)  */
	putChr('(');
	put(FUN_PREC-1,arg(e));
	putChr(' ');
	unlexOp(t);
	putChr(')');
    }
    else if (args==2)		       /* infix expr of the form e1 + e2   */
	putInfix(d,t,sy,arg(maySkipDict(fun(e))),arg(e));
    else {			       /* o/w (e1 + e2) e3 ... en   (n>=3) */
	OPEN(d>=FUN_PREC);
	putOverInfix(args,t,sy,e);
	CLOSE(d>=FUN_PREC);
    }
}

static Void local putOverInfix(args,t,sy,e)
Int    args;			       /* infix applied to >= 3 arguments  */
Text   t;
Syntax sy;
Cell   e; {
    if (args>2) {
	putOverInfix(args-1,t,sy,maySkipDict(fun(e)));
	putChr(' ');
	put(FUN_PREC,arg(e));
    }
    else
	putInfix(ALWAYS,t,sy,arg(maySkipDict(fun(e))),arg(e));
}

static Void local putInfix(d,t,sy,e,f)  /* print infix expression	   */
Int    d;
Text   t;				/* Infix operator symbol  	   */
Syntax sy;				/* with name t, syntax s 	   */
Cell   e, f; {				/* Left and right operands	   */
    Syntax a = assocOf(sy);
    Int    p = precOf(sy);

    OPEN(d>p);
    put((a==LEFT_ASS ? p : 1+p), e);
    putChr(' ');
    unlexOp(t);
    putChr(' ');
    put((a==RIGHT_ASS ? p : 1+p), f);
    CLOSE(d>p);
}

static Void local putSimpleAp(e)       /* print application e0 e1 ... en   */
Cell e; {
    if (isAp(e)) {
	putSimpleAp(maySkipDict(fun(e)));
	putChr(' ');
	put(FUN_PREC,arg(e));
    }
    else
	put(FUN_PREC,e);
}

static Void local putTuple(ts,e)	/* Print tuple expression, allowing*/
Int  ts;				/* for possibility of either too   */
Cell e; {				/* few or too many args to constr  */
    Int i;
    putChr('(');
    if ((i=unusedTups(ts,e))>0) {
	while (--i>0)
	    putChr(',');
        putChr(')');
    }
}

static Int local unusedTups(ts,e)	/* print first part of tuple expr  */
Int  ts;				/* returning number of constructor */
Cell e; {				/* args not yet printed ...	   */
    if (isAp(e)) {
	if ((ts=unusedTups(ts,fun(e))-1)>=0) {
	    put(NEVER,arg(e));
	    putChr(ts>0?',':')');
	}
	else {
	    putChr(' ');
	    put(FUN_PREC,arg(e));
	}
    }
    return ts;
}

static Void local unlexVar(t)	       /* print text as a variable name    */
Text t; {			       /* operator symbols must be enclosed*/
    String s = textToStr(t);	       /* in parentheses... except [] ...  */

    if ((isascii(s[0]) && isalpha(s[0])) || s[0]=='_' || strcmp(s,"[]")==0)
	putStr(s);
    else {
	putChr('(');
	putStr(s);
	putChr(')');
    }
}

static Void local unlexOp(t)	       /* print text as operator name	   */
Text t; {			       /* alpha numeric symbols must be    */
    String s = textToStr(t);	       /* enclosed by backquotes	   */

    if (isascii(s[0]) && isalpha(s[0])) {
	putChr('`');
	putStr(s);
	putChr('`');
    }
    else
	putStr(s);
}

static Void local unlexCharConst(c)
Cell c; {
    putChr('\'');
    putStr(unlexChar(c,'\''));
    putChr('\'');
}

static Void local unlexStrConst(t)
Text t; {
    String s            = textToStr(t);
    static Char SO      = 14;		/* ASCII code for '\SO'		   */
    Bool   lastWasSO    = FALSE;
    Bool   lastWasDigit = FALSE;
    Bool   lastWasEsc   = FALSE;

    putChr('\"');
    for (; *s; s++) {
        String ch = unlexChar(*s,'\"');
	Char   c  = ' ';

	if ((lastWasSO && *ch=='H') ||
		(lastWasEsc && lastWasDigit && isascii(*ch) && isdigit(*ch)))
	    putStr("\\&");

        lastWasEsc   = (*ch=='\\');
        lastWasSO    = (*s==SO);
        for (; *ch; c = *ch++)
	    putChr(*ch);
        lastWasDigit = (isascii(c) && isdigit(c));
    }
    putChr('\"');
}

/* --------------------------------------------------------------------------
 * Pretty printer for supercombinator definitions:
 * i.e. for lambda-lifter output, immediately prior to code generation.
 * ------------------------------------------------------------------------*/

#ifdef GOFC_OUTPUT
static Void local pPut(d,e,co)	       /* pretty print expr in context of  */
Int  d; 			       /* operator of precedence d	   */
Cell e;				       /* with current offset co	   */
Int  co; {
    switch (whatIs(e)) {
	case AP 	: if (fun(e)==mkSelect(0))
			      pPut(d,arg(e),co);
			  else
			      pPutAp(d,e,co);
			  break;

	case OFFSET	: pPutOffset(offsetOf(e));
			  break;

	case NAME	: unlexVar(name(e).text);
			  break;

	case DICTCELL   : putStr("{dict");
			  putInt(dictOf(e));
			  putChr('}');
			  break;

	case SELECT	: putStr("#");
			  putInt(selectOf(e));
			  break;

	case UNIT	: putStr("()");
			  break;

	case TUPLE	: pPutTuple(tupleOf(e),e,co);
			  break;

	case CHARCELL	: unlexCharConst(charOf(e));
			  break;

	case INTCELL	: putInt(intOf(e));
			  break;

        case FLOATCELL  : putStr(floatToString(floatOf(e)));
			  break;

	case STRCELL	: unlexStrConst(textOf(e));
			  break;

	case LETREC	: OPEN(d>WHERE_PREC);
			  co += pPutLocals(fst(snd(e)),co);
			  pPut(WHERE_PREC+1, snd(snd(e)), co);
			  CLOSE(d>WHERE_PREC);
			  break;

	case COND	: OPEN(d>COND_PREC);
			  putStr("if ");
			  pPut(COND_PREC+1,fst3(snd(e)),co);
			  putStr(" then ");
			  pPut(COND_PREC+1,snd3(snd(e)),co);
			  putStr(" else ");
			  pPut(COND_PREC+1,thd3(snd(e)),co);
			  CLOSE(d>COND_PREC);
			  break;

	default 	: internal("pPut");
    }
}

static Void local pPutAp(d,e,co)	/* print application (args>=1)	   */
Int  d;
Cell e;
Int  co; {
    Bool   anyDictArgs = FALSE;
    Cell   h;
    Text   t;
    Syntax sy;
    Int    args = 0;

    for (h=e; isAp(h); h=fun(h)) {	/* find head of expression, looking*/
	if (isDictVal(arg(h)))		/* for dictionary arguments	   */
	    anyDictArgs = TRUE;
	args++;
    }

    switch (whatIs(h)) {
	case TUPLE	: OPEN(args>tupleOf(h) && d>=FUN_PREC);
			  pPutTuple(tupleOf(h),e,co);
			  CLOSE(args>tupleOf(h) && d>=FUN_PREC);
			  return;

	case NAME	: sy = syntaxOf(t = name(h).text);
			  break;

	default 	: sy = APPLIC;
			  break;
    }

    if (anyDictArgs || args>2)		/* print some exprs applicatively  */
	sy = APPLIC;

    if (sy==APPLIC) {		       	/* print simple application	   */
	OPEN(d>=FUN_PREC);
	pPutSimpleAp(e,co);
	CLOSE(d>=FUN_PREC);
	return;
    }
    else if (args==1) {		        /* print section of the form (e+)  */
	putChr('(');
	pPut(FUN_PREC-1,arg(e),co);
	putChr(' ');
	unlexOp(t);
	putChr(')');
    }
    else {				/* infix expr of the form e1 + e2  */
	Syntax a = assocOf(sy);
	Int    p = precOf(sy);
	OPEN(d>p);
	pPut((a==LEFT_ASS ? p : 1+p), arg(fun(e)), co);
	putChr(' ');
	unlexOp(t);
	putChr(' ');
	pPut((a==RIGHT_ASS ? p : 1+p), arg(e), co);
	CLOSE(d>p);
    }

}

static Void local pPutSimpleAp(e,co)    /* print application e0 e1 ... en  */
Cell e;
Int  co; {
    if (isAp(e)) {
	pPutSimpleAp(fun(e),co);
	putChr(' ');
	pPut(FUN_PREC,arg(e),co);
    }
    else
	pPut(FUN_PREC,e,co);
}

static Void local pPutTuple(ts,e,co)	/* Print tuple expression, allowing*/
Int  ts;				/* for possibility of either too   */
Cell e;					/* few or too many args to constr  */
Int  co; {
    Int i;
    putChr('(');
    if ((i=punusedTups(ts,e,co))>0) {
	while (--i>0)
	    putChr(',');
        putChr(')');
    }
}

static Int local punusedTups(ts,e,co)	/* print first part of tuple expr  */
Int  ts;				/* returning number of constructor */
Cell e;					/* args not yet printed ...	   */
Int  co; {
    if (isAp(e)) {
	if ((ts=punusedTups(ts,fun(e),co)-1)>=0) {
	    pPut(NEVER,arg(e),co);
	    putChr(ts>0?',':')');
	}
	else {
	    putChr(' ');
	    pPut(FUN_PREC,arg(e),co);
	}
    }
    return ts;
}

static Void local pPutOffset(n)		/* pretty print offset number	   */
Int n; {
    putChr('o');
    putInt(n);
}

static Int local pPutLocals(vs,co)	/* pretty print locals		   */
List vs;
Int  co; {
    Int left = outColumn;
    Int n    = length(vs);
    Int i;

    putStr("let { ");
    for (i=0; i<n; i++) {
	pPutOffset(co+i+1);
	putChr(' ');
	pLiftedStart(hd(vs),co+n,"=");
	vs = tl(vs);
	if (nonNull(vs))
	    indent(left+6);
    }
    indent(left);
    putStr("} in  ");
    return n;
}

static Void local pLiftedStart(e,co,eq)	/* print start of definition	   */
Cell   e;
Int    co;
String eq; {
    if (whatIs(e)!=GUARDED) {
	putStr(eq);
	putChr(' ');
    }
    pLifted(e,co,eq);
}

static Void local pLifted(e,co,eq)	/* print lifted definition	   */
Cell   e;
Int    co;
String eq; {
    switch (whatIs(e)) {
	case GUARDED : {   Int  left = outColumn;
			   List gs   = snd(e);
			   if (isNull(gs))
			       internal("pLifted");
			   for (;;) {
			       putStr("| ");
			       pPut(NEVER,fst(hd(gs)),co);
			       putChr(' ');
			       putStr(eq);
			       putChr(' ');
			       pPut(NEVER,snd(hd(gs)),co);
			       putStr(";\n");
			       gs = tl(gs);
			       if (nonNull(gs))
				   indent(left);
			       else
				   break;
			   }
		       }
		       break;

	case LETREC  : co += pPutLocals(fst(snd(e)),co);
		       pLifted(snd(snd(e)), co, eq);
		       break;

        case FATBAR  : {   Int left = outColumn;
			   pLifted(fst(snd(e)),co,eq);
			   indent(left);
			   putStr("FATBAR\n");
			   indent(left);
			   pLifted(snd(snd(e)),co,eq);
		       }
		       break;

	case CASE    : {   Int  left = outColumn;
			   List cs   = snd(snd(e));
			   putStr("case ");
			   pPut(NEVER,fst(snd(e)),co);
			   putStr(" of {\n");
			   for (; nonNull(cs); cs=tl(cs)) {
			       Int arity;
			       indent(left+2);
			       arity = pDiscr(fst(hd(cs)),co);
			       putChr(' ');
			       pLiftedStart(snd(hd(cs)),co+arity,"->");
			   }
			   indent(left);
			   putStr("}\n");
		       }
		       break;

	default	     : pPut(NEVER,e,co);
		       putStr(";\n");
		       break;
    }
}

static Int local pDiscr(d,co)		/* pretty print discriminator	   */
Cell d;
Int  co; {
    Int arity = 0;

    switch (whatIs(d)) {
	case INTCELL  : putInt(intOf(d));
			break;

	case CHARCELL : unlexCharConst(charOf(d));
			break;

	case UNIT     : putStr("()");
			break;

#if NPLUSK
	case ADDPAT   : pPutOffset(co+1);
			putChr('+');
			putInt(intValOf(d));
			arity = 1;
			break;

	case MULPAT   : putInt(intValOf(d));
			putChr('*');
			pPutOffset(co+1);
			arity = 1;
			break;
#endif

	case NAME     : {   Int i = 0;
			    arity = name(d).arity;
			    unlexVar(name(d).text);
			    for (; i<arity; ++i) {
				putChr(' ');
				pPutOffset(co+arity-i);
			    }
			}
			break;

	case TUPLE    : {   Int i = 0;
			    arity = tupleOf(d);
			    putChr('(');
			    pPutOffset(co+arity);
			    while (++i<arity) {
				putChr(',');
				pPutOffset(co+arity-i);
			    }
			    putChr(')');
			}
			break;

	default	      : internal("pDiscr");
    }

    return arity;
}

Void pScDef(t,arity,e)			/* pretty print sc defn on gofcFp  */
Text t;
Int  arity;
Cell e; {
    Int i;
    outputStream = gofcFp;
    putChr('\n');
    outColumn = 0;
    unlexVar(t);
    for (i=0; i<arity; i++) {
	putChr(' ');
	pPutOffset(arity-i);
    }
    putChr(' ');
    pLiftedStart(e,arity,"=");
}
#endif

/* --------------------------------------------------------------------------
 * Print type expression:
 * ------------------------------------------------------------------------*/

static Void local putSigType(t)		/* print (possibly) generic type   */
Cell t; {
    if (isPolyType(t))			/* skip (forall _) part (if any)   */
        t = monoTypeOf(t);

    if (whatIs(t)==QUAL) {		/* Handle qualified types          */
        putContext(fst(snd(t)));
        putStr(" => ");
        t = snd(snd(t));
    }

    putType(t,NEVER);			/* Finally, print rest of type ... */
}

static Void local putContext(qs)	/* print context list		   */
List qs; {
    if (isNull(qs))
	putStr("()");
    else {
	Int nq = length(qs);

	if (nq!=1) putChr('(');
	putPred(hd(qs));
	while (nonNull(qs=tl(qs))) {
	    putStr(", ");
	    putPred(hd(qs));
	}
	if (nq!=1) putChr(')');
    }
}

static Void local putPred(pi)		/* Output predicate		   */
Cell pi; {
    if (isAp(pi)) {
	putPred(fun(pi));
	putChr(' ');
	putType(arg(pi),ALWAYS);
    }
    else if (isClass(pi))
	putStr(textToStr(class(pi).text));
    else if (isCon(pi))
	putStr(textToStr(textOf(pi)));
    else
	putStr("<unknownPredicate>");
}

static Void local putType(t,prec)	/* print nongeneric type expression*/
Cell t;
Int  prec; {
    switch(whatIs(t)) {
	case UNIT    : putStr("()");
		       break;

	case LIST    : putStr("[]");
		       break;

	case ARROW   : putStr("(->)");
		       break;

	case TYCON   : putStr(textToStr(tycon(t).text));
		       break;

	case TUPLE   : {   Int n = tupleOf(t);
			   putChr('(');
			   while (--n > 0)
			       putChr(',');
			   putChr(')');
		       }
		       break;

	case OFFSET  : putTyVar(offsetOf(t));
		       break;

	case INTCELL : putChr('_');
		       putInt(intOf(t));
		       break;

	case AP	     : {   Cell typeHead = getHead(t);
			   Bool brackets = (argCount!=0 && prec>=ALWAYS);

			   switch (whatIs(typeHead)) {
			       case LIST  : if (argCount==1) {
						putChr('[');
						putType(arg(t),NEVER);
						putChr(']');
						return;
					    }
					    break;

			       case ARROW : if (argCount==2) {
						OPEN(prec>=ARROW_PREC);
						putType(arg(fun(t)),ARROW_PREC);
						putStr(" -> ");
						putType(arg(t),NEVER);
						CLOSE(prec>=ARROW_PREC);
						return;
					    }
					    else if (argCount==1) {
						putChr('(');
						putType(arg(t),ARROW_PREC);
						putStr("->)");
						return;
					    }
					    break;

			       case TUPLE : if (argCount==tupleOf(typeHead)) {
						putChr('(');
						putTupleType(t);
						putChr(')');
						return;
					    }
					    break;

			       case TYCON :
#if IO_MONAD
					    if (typeHead==typeST &&
					        argCount==1      &&
						snd(t)==typeWorld)
						brackets = FALSE;
#endif
					    break;
			   }
			   OPEN(brackets);
			   putApType(t);
			   CLOSE(brackets);
		       }
		       break;

	default      : putStr("(bad type)");
    }
}

static Void local putTyVar(n)		/* print type variable		   */
Int n; {
    static String alphabet		/* for the benefit of EBCDIC :-)   */
		="abcdefghijklmnopqrstuvwxyz";
    putChr(alphabet[n%26]);
    if (n /= 26)			/* just in case there are > 26 vars*/
	putInt(n);
}

static Bool local putTupleType(e)	/* print tuple of types, returning */
Cell e; {				/* TRUE if something was printed,  */
    if (isAp(e)) {			/* FALSE otherwise; used to control*/
	if (putTupleType(fun(e)))	/* printing of intermed. commas	   */
	    putChr(',');
	putType(arg(e),NEVER);
	return TRUE;
    }
    return FALSE;
}

static Void local putApType(t)		/* print type application	   */
Cell t; {
    if (isAp(t)) {
#if IO_MONAD
	if (fun(t)==typeST && arg(t)==typeWorld)
	    putType(typeIO,ALWAYS);
	else
#endif
	{
	    putApType(fun(t));
	    putChr(' ');
	    putType(arg(t),ALWAYS);
	}
    }
    else
	putType(t,ALWAYS);
}

/* --------------------------------------------------------------------------
 * Print kind expression:
 * ------------------------------------------------------------------------*/

static Void local putKind(k)		/* print kind expression	   */
Kind k; {
    switch (whatIs(k)) {
	case AP	     : if (isAp(fst(k))) {
			   putChr('(');
			   putKind(fst(k));
			   putChr(')');
		       }
		       else
			   putKind(fst(k));
		       putStr(" -> ");
		       putKind(snd(k));
		       break;

	case STAR    : putChr('*');
		       break;

	case OFFSET  : putTyVar(offsetOf(k));
		       break;

	case INTCELL : putChr('_');
		       putInt(intOf(k));
		       break;

	default      : putStr("(bad kind)");
    }
}

static Void local putSig(sig)		/* print class kind signature	   */
Cell sig; {
    putChr('(');
    putKind(hd(sig));
    for (sig=tl(sig); nonNull(sig); sig=tl(sig)) {
	putStr(", ");
	putKind(hd(sig));
    }
    putChr(')');
}

/* --------------------------------------------------------------------------
 * Main drivers:
 * ------------------------------------------------------------------------*/

Void printExp(fp,e)			/* print expr on specified stream  */
FILE *fp;
Cell e; {
    outputStream = fp;
    putDepth     = 0;
    put(NEVER,e);
}

Void printType(fp,t)			/* print type on specified stream  */
FILE *fp;
Cell t; {
    outputStream = fp;
    putSigType(t);
}

Void printContext(fp,qs)		/* print context on spec. stream   */
FILE *fp;
List qs; {
    outputStream = fp;
    putContext(qs);
}

Void printPred(fp,pi)			/* print predicate pi on stream    */
FILE *fp;
Cell pi; {
    outputStream = fp;
    putPred(pi);
}

Void printKind(fp,k)			/* print kind k on stream	   */
FILE *fp;
Kind k; {
    outputStream = fp;
    putKind(k);
}

Void printSig(fp,sig)			/* print class kind signature	   */
FILE *fp;
Cell sig; {
    outputStream = fp;
    putSig(sig);
}

/*-------------------------------------------------------------------------*/
