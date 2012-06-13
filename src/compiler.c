/* --------------------------------------------------------------------------
 * compiler.c:  Copyright (c) Mark P Jones 1991-1994.   All rights reserved.
 *              See goferite.h for details and conditions of use etc...
 *              Gofer version 2.30 March 1994
 *
 * This is the Gofer compiler, handling translation of typechecked code to
 * `kernel' language, elimination of pattern matching and translation to
 * super combinators (lambda lifting).
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"

Bool useConformality = TRUE;		/* TRUE => check pat-bind conform'y*/
Addr inputCode;				/* Addr of compiled code for expr  */

Name nameResult, nameBind;		/* for translating monad comps	   */
Name nameZero;				/* for monads with a zero	   */

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static Cell local translate		Args((Cell));
static Void local transPair		Args((Pair));
static Void local transTriple		Args((Triple));
static Void local transAlt		Args((Cell));
static Void local transCase		Args((Cell));
static List local transBinds		Args((List));
static Cell local transRhs		Args((Cell));
static Cell local mkConsList		Args((List));
static Cell local expandLetrec		Args((Cell));

static Cell local transComp		Args((Cell,List,Cell));
static Cell local transMComp		Args((Cell,Cell,Cell,List));

static Cell local refutePat		Args((Cell));
static List local remPat		Args((Cell,Cell,List));
static List local remPat1		Args((Cell,Cell,List));

static Cell local pmcTerm		Args((Int,List,Cell));
static Cell local pmcPair		Args((Int,List,Pair));
static Cell local pmcTriple		Args((Int,List,Triple));
static Cell local pmcVar		Args((List,Text));
static Void local pmcLetrec		Args((Int,List,Pair));
static Cell local pmcVarDef		Args((Int,List,List));
static Void local pmcFunDef		Args((Int,List,Triple));

static Cell local match 		Args((Int,List,List));
static Void local tidyHdPat		Args((Offset,Cell));
static Cell local hdDiscr		Args((List));
static Int  local discrKind		Args((Cell));

static Cell local matchVar		Args((Int,List,List,Cell));

static Cell local matchCon		Args((Int,List,List,Cell));
static List local addConTable		Args((Cell,Cell,List));
static Cell local makeCases		Args((Int,List,List));

static Cell local matchInt		Args((Int,List,List,Cell));

static List local addOffsets		Args((Int,Int,List));
static Cell local mkSwitch		Args((List,Pair));
static Cell local joinSw		Args((Int,List));
static Bool local canFail		Args((Cell));

static Cell local lift			Args((Int,List,Cell));
static Void local liftPair		Args((Int,List,Pair));
static Void local liftTriple		Args((Int,List,Triple));
static Void local liftAlt		Args((Int,List,Cell));
static Cell local liftVar		Args((List,Cell));
static Cell local liftLetrec		Args((Int,List,Cell));
static Void local liftFundef		Args((Int,List,Triple));
static Void local solve 		Args((List));

static Cell local preComp		Args((Cell));
static Cell local preCompPair		Args((Pair));
static Cell local preCompTriple 	Args((Triple));
static Void local preCompCase		Args((Pair));
static Cell local preCompOffset 	Args((Int));

static Void local compileGlobalFunction Args((Pair));
static Void local compileMemberFunction Args((Name));
static Void local newGlobalFunction	Args((Name,Int,List,Int,Cell));

/* --------------------------------------------------------------------------
 * Transformation: Convert input expressions into a less complex language
 *		   of terms using only LETREC, AP, constants and vars.
 *		   Also remove pattern definitions on lhs of eqns.
 * ------------------------------------------------------------------------*/

static Cell local translate(e)	       /* Translate expression: 	   */
Cell e; {
    switch (whatIs(e)) {
	case LETREC	: snd(snd(e)) = translate(snd(snd(e)));
			  return expandLetrec(e);

	case COND	: transTriple(snd(e));
			  break;

	case AP 	: transPair(e);
			  break;

	case UNIT	:
	case TUPLE	:
	case NAME	:
	case SELECT	:
	case VAROPCELL	:
	case VARIDCELL	:
	case DICTVAR	:
	case DICTCELL	:
	case INTCELL	:
	case FLOATCELL  :
	case STRCELL	:
	case CHARCELL	: break;

	case FINLIST	: mapOver(translate,snd(e));
			  return mkConsList(snd(e));

	case LISTCOMP	: return transComp(translate(fst(snd(e))),
					   snd(snd(e)),
					   nameNil);

	case MONADCOMP  : if (dictOf(fst(fst(snd(e)))) == listMonadDict())
			      return transComp(translate(fst(snd(snd(e)))),
					       snd(snd(snd(e))),
					       nameNil);
			  /*intentional fall-thru*/
	case DOCOMP	: {   Cell m  = fst(fst(snd(e)));
			      Cell m0 = snd(fst(snd(e)));
			      Cell r  = translate(fst(snd(snd(e))));
			      if (fst(e)!=DOCOMP)
				  r = ap(ap(nameResult,m),r);
			      return transMComp(m,m0,r,snd(snd(snd(e))));
			  }

#if IO_MONAD
	case RUNST	: fst(e) = nameSTRun;
			  snd(e) = translate(snd(e));
			  break;
#endif

	case CASE	: {   Cell nv = inventVar();
			      mapProc(transCase,snd(snd(e)));
			      return ap(LETREC,
					pair(singleton(pair(nv,snd(snd(e)))),
					     ap(nv,translate(fst(snd(e))))));
			  }

	case LAMBDA	: {   Cell nv = inventVar();
			      transAlt(snd(e));
			      return ap(LETREC,
					pair(singleton(pair(
							nv,
							singleton(snd(e)))),
					     nv));
			  }

	default 	: internal("translate");
    }
    return e;
}

static Void local transPair(pr)        /* Translate each component in a    */
Pair pr; {			       /* pair of expressions.		   */
    fst(pr) = translate(fst(pr));
    snd(pr) = translate(snd(pr));
}

static Void local transTriple(tr)      /* Translate each component in a    */
Triple tr; {			       /* triple of expressions.	   */
    fst3(tr) = translate(fst3(tr));
    snd3(tr) = translate(snd3(tr));
    thd3(tr) = translate(thd3(tr));
}

static Void local transAlt(e)	       /* Translate alt:		   */
Cell e; {			       /* ([Pat], Rhs) ==> ([Pat], Rhs')   */
    snd(e) = transRhs(snd(e));
}

static Void local transCase(c)	       /* Translate case:		   */
Cell c; {			       /* (Pat, Rhs) ==> ([Pat], Rhs')	   */
    fst(c) = singleton(fst(c));
    snd(c) = transRhs(snd(c));
}

static List local transBinds(bs)       /* Translate list of bindings:	   */
List bs; {			       /* eliminating pattern matching on  */
    List newBinds;		       /* lhs of bindings.		   */

    for (newBinds=NIL; nonNull(bs); bs=tl(bs)) {
	if (isVar(fst(hd(bs)))) {
	    mapProc(transAlt,snd(hd(bs)));
	    newBinds = cons(hd(bs),newBinds);
	}
	else
	    newBinds = remPat(fst(snd(hd(bs))),
			      snd(snd(hd(bs)))=transRhs(snd(snd(hd(bs)))),
			      newBinds);
    }

    return newBinds;
}

static Cell local transRhs(rhs)        /* Translate rhs: removing line nos */
Cell rhs; {
    switch (whatIs(rhs)) {
	case LETREC  : snd(snd(rhs)) = transRhs(snd(snd(rhs)));
		       return expandLetrec(rhs);

	case GUARDED : mapOver(snd,snd(rhs));	    /* discard line number */
		       mapProc(transPair,snd(rhs));
		       return rhs;

	default      : return translate(snd(rhs));  /* discard line number */
    }
}

static Cell local mkConsList(es)       /* Construct expression for list es */
List es; {			       /* using nameNil and nameCons	   */
    if (isNull(es))
	return nameNil;
    else
	return ap(ap(nameCons,hd(es)),mkConsList(tl(es)));
}

static Cell local expandLetrec(root)   /* translate LETREC with list of    */
Cell root; {			       /* groups of bindings (from depend. */
    Cell e   = snd(snd(root));	       /* analysis) to use nested LETRECs  */
    List bss = fst(snd(root));
    Cell temp;

    if (isNull(bss))		       /* should never happen, but just in */
	return e;		       /* case:  LETREC [] IN e  ==>  e    */

    mapOver(transBinds,bss);	       /* translate each group of bindings */

    for (temp=root; nonNull(tl(bss)); bss=tl(bss)) {
	fst(snd(temp)) = hd(bss);
	snd(snd(temp)) = ap(LETREC,pair(NIL,e));
	temp	       = snd(snd(temp));
    }
    fst(snd(temp)) = hd(bss);

    return root;
}

/* --------------------------------------------------------------------------
 * Transformation of list comprehensions is based on the description in
 * `The Implementation of Functional Programming Languages':
 *
 * [ e | qs ] ++ L	      => transComp e qs []
 * transComp e []	    l => e : l
 * transComp e ((p<-xs):qs) l => LETREC _h []	   = l
 *					_h (p:_xs) = transComp e qs (_h _xs)
 *					_h (_:_xs) = _h _xs --if p refutable.
 *				 IN _h xs
 * transComp e (b:qs)	    l => if b then transComp e qs l else l
 * transComp e (decls:qs)   l => LETREC decls IN transComp e qs l
 * ------------------------------------------------------------------------*/

static Cell local transComp(e,qs,l)    /* Translate [e | qs] ++ l	   */
Cell e;
List qs;
Cell l; {
    if (nonNull(qs)) {
	Cell q	 = hd(qs);
	Cell qs1 = tl(qs);

	switch (fst(q)) {
	    case FROMQUAL : {	Cell ld    = NIL;
				Cell hVar  = inventVar();
				Cell xsVar = inventVar();

				if (refutable(fst(snd(q))))
				    ld = cons(pair(singleton(
						    ap(ap(nameCons,
							  WILDCARD),
							  xsVar)),
						   ap(hVar,xsVar)),
					      ld);

				ld = cons(pair(singleton(
						ap(ap(nameCons,
						      fst(snd(q))),
						      xsVar)),
					       transComp(e,
							 qs1,
							 ap(hVar,xsVar))),
					  ld);
				ld = cons(pair(singleton(nameNil),
					       l),
					  ld);

				return ap(LETREC,
					  pair(singleton(pair(hVar,
							      ld)),
					       ap(hVar,
						  translate(snd(snd(q))))));
			    }

	    case QWHERE   : return
				expandLetrec(ap(LETREC,
						pair(snd(q),
						     transComp(e,qs1,l))));

	    case BOOLQUAL : return ap(COND,
				      triple(translate(snd(q)),
					     transComp(e,qs1,l),
					     l));
	}
    }

    return ap(ap(nameCons,e),l);
}

/* --------------------------------------------------------------------------
 * Transformation of monad comprehensions is based on the description in
 * Comprehending monads / The essence of functional programming:
 *
 * [ e | ]                =>  return m e   (return is applied in translate())
 * [ e | p <- exp, qs ]   =>  LETREC _h p = [ e | qs]
 *				     _h _ = zero m0    -- if monad with 0
 *			      IN bind m exp _h
 * [ e | LET decls, qs ]  =>  LETREC decls IN [ e | qs ]
 * [ e | guard, qs ]      =>  if guard then [ e | qs ] else zero m0
 * [ e | DO expr, qs ]    =>  LETREC _h _ = [ e | qs ] in bind m exp _h
 *
 * where  m :: Monad f,  m0 :: Monad0 f
 * ------------------------------------------------------------------------*/

static Cell local transMComp(m,m0,e,qs)	/* Translate [e | qs]		   */
Cell m;
Cell m0;
Cell e;
List qs; {
    if (nonNull(qs)) {
	Cell q	 = hd(qs);
	Cell qs1 = tl(qs);

	switch (fst(q)) {
	    case FROMQUAL : {	Cell ld   = NIL;
				Cell hVar = inventVar();

				if (refutable(fst(snd(q))) && nonNull(m0))
				    ld = cons(pair(singleton(WILDCARD),
						   ap(nameZero,m0)),ld);

				ld = cons(pair(singleton(fst(snd(q))),
					       transMComp(m,m0,e,qs1)),
					  ld);

				return ap(LETREC,
					  pair(singleton(pair(hVar,ld)),
					       ap(ap(ap(nameBind,
							m),
						     translate(snd(snd(q)))),
						  hVar)));
			    }

#if DO_COMPS
	    case DOQUAL :   {	Cell hVar = inventVar();
				Cell ld   = cons(pair(singleton(WILDCARD),
						      transMComp(m,m0,e,qs1)),
						 NIL);
				return ap(LETREC,
					  pair(singleton(pair(hVar,ld)),
					       ap(ap(ap(nameBind,
							m),
						     translate(snd(q))),
						  hVar)));
			    }
#endif

	    case QWHERE	  : return
				expandLetrec(ap(LETREC,
						pair(snd(q),
						     transMComp(m,m0,e,qs1))));

	    case BOOLQUAL : return ap(COND,
				      triple(translate(snd(q)),
					     transMComp(m,m0,e,qs1),
					     ap(nameZero,m0)));
	}
    }

    return e; /* If necessary, a monad unit/return will have already been
	         applied to the expression e during translate() ... */
}

/* --------------------------------------------------------------------------
 * Elimination of pattern bindings:
 *
 * The following code adopts the definition of irrefutable patterns as given
 * in the Haskell report in which only variables, wildcards and ~pat patterns
 * are irrefutable.  As a special case (and contrary to definition of Haskell),
 * we also treat tuples as irrefutable, so long as all their components are
 * irrefutable.  Note that the definition in Peyton Jones takes this even
 * further and allows arbitrary product constructor functions as irrefutable
 * patterns.
 * ------------------------------------------------------------------------*/

Bool refutable(pat)		  /* is pattern refutable (do we need to   */
Cell pat; {			  /* to use a conformality check?)	   */
    Cell c = getHead(pat);

    switch (whatIs(c)) {
	case ASPAT     : return refutable(snd(snd(pat)));

	case TUPLE     : for (; isAp(pat); pat=fun(pat))
			     if (refutable(arg(pat)))
				return TRUE;
			 /*intentional fall-thru*/
	case LAZYPAT   :
	case VAROPCELL :
	case VARIDCELL :
	case DICTVAR   :
	case WILDCARD  : return FALSE;

	default        : return TRUE;
    }
}

static Cell local refutePat(pat)  /* find pattern to refute in conformality*/
Cell pat; {			  /* test with pat.			   */
				  /* e.g. refPat  (x:y) == (_:_)	   */
				  /*	  refPat ~(x:y) == _	  etc..    */

    switch (whatIs(pat)) {
	case ASPAT     : return refutePat(snd(snd(pat)));

	case FINLIST   : {   Cell ys = snd(pat);
			     Cell xs = NIL;
			     for (; nonNull(ys); ys=tl(ys))
				 xs = ap(ap(nameCons,refutePat(hd(ys))),xs);
			     return revOnto(xs,nameNil);
			 }

	case VAROPCELL :
	case VARIDCELL :
	case DICTVAR   :
	case WILDCARD  :
	case LAZYPAT   : return WILDCARD;

	case INTCELL   :
        case FLOATCELL :
	case STRCELL   :
	case CHARCELL  :
#if NPLUSK
	case ADDPAT    :
	case MULPAT    :
#endif
	case UNIT      :
	case TUPLE     :
	case NAME      : return pat;

	case AP        : return ap(refutePat(fun(pat)),refutePat(arg(pat)));

	default        : internal("refutePat");
			 return NIL; /*NOTREACHED*/
    }
}

#define addEqn(v,val,lds)  cons(pair(v,singleton(pair(NIL,val))),lds)

static List local remPat(pat,expr,lds)
Cell pat;			  /* Produce list of definitions for eqn   */
Cell expr;			  /* pat = expr, including a conformality  */
List lds; {			  /* check if required. 		   */

    /* Conformality test (if required):
     *	 pat = expr  ==>    nv = LETREC confCheck nv@pat = nv
     *				 IN confCheck expr
     *			    remPat1(pat,nv,.....);
     */

    if (useConformality && refutable(pat)) {
	Cell confVar = inventVar();
	Cell nv      = inventVar();
	Cell locfun  = pair(confVar,	     /* confVar [([nv@refPat],nv)] */
			    singleton(pair(singleton(ap(ASPAT,
							pair(nv,
							     refutePat(pat)))),
					   nv)));

	if (whatIs(expr)==GUARDED) {	     /* A spanner ... special case */
	    lds  = addEqn(nv,expr,lds);	     /* for guarded pattern binding*/
	    expr = nv;
	    nv   = inventVar();
	}

	if (whatIs(pat)==ASPAT) {	     /* avoid using new variable if*/
	    nv   = fst(snd(pat));	     /* a variable is already given*/
	    pat  = snd(snd(pat));	     /* by an as-pattern	   */
	}

	lds = addEqn(nv,				/* nv = 	   */
		     ap(LETREC,pair(singleton(locfun),	/* LETREC [locfun] */
				    ap(confVar,expr))), /* IN confVar expr */
		     lds);

	return remPat1(pat,nv,lds);
    }

    return remPat1(pat,expr,lds);
}

static List local remPat1(pat,expr,lds)
Cell pat;			  /* Add definitions for: pat = expr to    */
Cell expr;			  /* list of local definitions in lds.	   */
List lds; {
    Cell c;

    switch (whatIs(c=getHead(pat))) {
	case WILDCARD  :
	case UNIT      :
	case INTCELL   :
        case FLOATCELL :
	case STRCELL   :
	case CHARCELL  : break;

	case ASPAT     : return remPat1(snd(snd(pat)),	   /* v@pat = expr */
					fst(snd(pat)),
					addEqn(fst(snd(pat)),expr,lds));

	case LAZYPAT   : {   Cell nv;

			     if (isVar(expr) || isName(expr))
				 nv  = expr;
			     else {
				 nv  = inventVar();
				 lds = addEqn(nv,expr,lds);
			     }

			     return remPat(snd(pat),nv,lds);
			 }

#if NPLUSK
	case ADDPAT    : return addEqn(snd(pat),	   /* n + k = expr */
				       ap(ap(nameMinus,expr),
					  mkInt(intValOf(fst(pat)))),
				       lds);

	case MULPAT    : return addEqn(snd(pat),	   /* c * n = expr */
				       ap(ap(nameDivide,expr),
					  mkInt(intValOf(fst(pat)))),
				       lds);
#endif

	case FINLIST   : return remPat1(mkConsList(snd(pat)),expr,lds);

	case DICTVAR   : /* shouldn't really occur */
	case VARIDCELL :
	case VAROPCELL : return addEqn(pat,expr,lds);

	case TUPLE     :
	case NAME      : {   List ps = getArgs(pat);

			     if (nonNull(ps)) {
				 Cell nv, sel;
				 Int  i;

				 if (isVar(expr) || isName(expr))
				     nv  = expr;
				 else {
				     nv  = inventVar();
				     lds = addEqn(nv,expr,lds);
				 }

				 sel = ap(ap(nameSel,c),nv);
				 for (i=1; nonNull(ps); ++i, ps=tl(ps))
				      lds = remPat1(hd(ps),
						    ap(sel,mkInt(i)),
						    lds);
			     }
			 }
			 break;

	default        : internal("remPat1");
			 break;
    }
    return lds;
}

/* --------------------------------------------------------------------------
 * Eliminate pattern matching in function definitions -- pattern matching
 * compiler:
 *
 * Based on Wadler's algorithms described in `Implementation of functional
 * programming languages'.
 *
 * During the translation, in preparation for later stages of compilation,
 * all local and bound variables are replaced by suitable offsets, and
 * locally defined function symbols are given new names (which will
 * eventually be their names when lifted to make top level definitions).
 * ------------------------------------------------------------------------*/

static Offset freeBegin; /* only variables with offset <= freeBegin are of */
static List   freeVars;  /* interest as `free' variables		   */
static List   freeFuns;  /* List of `free' local functions		   */

static Cell local pmcTerm(co,sc,e)     /* apply pattern matching compiler  */
Int  co;			       /* co = current offset		   */
List sc;			       /* sc = scope			   */
Cell e;  {			       /* e  = expr to transform	   */
    switch (whatIs(e)) {
	case GUARDED  : map2Over(pmcPair,co,sc,snd(e));
			break;

	case LETREC   : pmcLetrec(co,sc,snd(e));
			break;

	case VARIDCELL:
	case VAROPCELL:
	case DICTVAR  : return pmcVar(sc,textOf(e));

	case COND     : return ap(COND,pmcTriple(co,sc,snd(e)));

	case AP       : return pmcPair(co,sc,e);

	case UNIT     :
	case TUPLE    :
	case NAME     :
	case SELECT   :
	case DICTCELL :
	case CHARCELL :
	case INTCELL  :
        case FLOATCELL:
	case STRCELL  : break;

	default       : internal("pmcTerm");
			break;
    }
    return e;
}

static Cell local pmcPair(co,sc,pr)    /* apply pattern matching compiler  */
Int  co;			       /* to a pair of exprs		   */
List sc;
Pair pr; {
    return pair(pmcTerm(co,sc,fst(pr)),
		pmcTerm(co,sc,snd(pr)));
}

static Cell local pmcTriple(co,sc,tr)  /* apply pattern matching compiler  */
Int    co;			       /* to a triple of exprs		   */
List   sc;
Triple tr; {
    return triple(pmcTerm(co,sc,fst3(tr)),
		  pmcTerm(co,sc,snd3(tr)),
		  pmcTerm(co,sc,thd3(tr)));
}

static Cell local pmcVar(sc,t)	       /* find translation of variable	   */
List sc;			       /* in current scope		   */
Text t; {
    List xs;
    Name n;

    for (xs=sc; nonNull(xs); xs=tl(xs)) {
	Cell x = hd(xs);
	if (t==textOf(fst(x)))
	    if (isOffset(snd(x))) {		     /* local variable ... */
		if (snd(x)<=freeBegin && !cellIsMember(snd(x),freeVars))
		    freeVars = cons(snd(x),freeVars);
		return snd(x);
	    }
	    else {				     /* local function ... */
		if (!cellIsMember(snd(x),freeFuns))
		    freeFuns = cons(snd(x),freeFuns);
		return fst3(snd(x));
	    }
    }

    if (isNull(n=findName(t)))	       /* Lookup global name - the only way*/
	n = newName(t); 	       /* this (should be able to happen)  */
				       /* is with new global var introduced*/
				       /* after type check; e.g. remPat1   */
    return n;
}

static Void local pmcLetrec(co,sc,e)   /* apply pattern matching compiler  */
Int  co;			       /* to LETREC, splitting decls into  */
List sc;			       /* two sections			   */
Pair e; {
    List fs = NIL;		       /* local function definitions	   */
    List vs = NIL;		       /* local variable definitions	   */
    List ds;

    for (ds=fst(e); nonNull(ds); ds=tl(ds)) {	    /* Split decls into two */
	Cell v	   = fst(hd(ds));
	Int  arity = length(fst(hd(snd(hd(ds)))));

	if (arity==0) { 			   /* Variable declaration */
	    vs = cons(snd(hd(ds)),vs);
	    sc = cons(pair(v,mkOffset(++co)),sc);
	}
	else {					   /* Function declaration */
	    fs = cons(triple(inventVar(),mkInt(arity),snd(hd(ds))),fs);
	    sc = cons(pair(v,hd(fs)),sc);
	}
    }
    vs	     = rev(vs); 	       /* Put declaration lists back in    */
    fs	     = rev(fs); 	       /* original order		   */
    fst(e)   = pair(vs,fs);	       /* Store declaration lists	   */
    map2Over(pmcVarDef,co,sc,vs);      /* Translate variable definitions   */
    map2Proc(pmcFunDef,co,sc,fs);      /* Translate function definitions   */
    snd(e)   = pmcTerm(co,sc,snd(e));  /* Translate LETREC body 	   */
    freeFuns = diffList(freeFuns,fs);  /* Delete any `freeFuns' bound in fs*/
}

static Cell local pmcVarDef(co,sc,vd)  /* apply pattern matching compiler  */
Int  co;			       /* to variable definition	   */
List sc;
List vd; {			       /* vd :: [ ([], rhs) ]		   */
    Cell d = snd(hd(vd));
    if (nonNull(tl(vd)) && canFail(d))
	return ap(FATBAR,pair(pmcTerm(co,sc,d),
			      pmcVarDef(co,sc,tl(vd))));
    return pmcTerm(co,sc,d);
}

static Void local pmcFunDef(co,sc,fd)  /* apply pattern matching compiler  */
Int    co;			       /* to function definition	   */
List   sc;
Triple fd; {			       /* fd :: (Var, Arity, [Alt])	   */
    Offset saveFreeBegin = freeBegin;
    List   saveFreeVars  = freeVars;
    List   saveFreeFuns  = freeFuns;
    Int    arity	 = intOf(snd3(fd));
    Cell   temp 	 = thd3(fd);
    Cell   xs;

    map1Over(mkSwitch,sc,temp);

    freeBegin = mkOffset(co);
    freeVars  = NIL;
    freeFuns  = NIL;
    temp      = match(co+arity,temp,addOffsets(co+arity,co+1,NIL));
    thd3(fd)  = triple(freeVars,freeFuns,temp);

    for (xs=freeVars; nonNull(xs); xs=tl(xs))
	if (hd(xs)<=saveFreeBegin && !cellIsMember(hd(xs),saveFreeVars))
	    saveFreeVars = cons(hd(xs),saveFreeVars);

    for (xs=freeFuns; nonNull(xs); xs=tl(xs))
	if (!cellIsMember(hd(xs),saveFreeFuns))
	    saveFreeFuns = cons(hd(xs),saveFreeFuns);

    freeBegin = saveFreeBegin;
    freeVars  = saveFreeVars;
    freeFuns  = saveFreeFuns;
}

/* --------------------------------------------------------------------------
 * Main part of pattern matching compiler: convert lists of Alt to case
 * construct:
 *
 * At each stage, each branch is represented by an element of type:
 *	Switch ::= ([Pat],Scope,Rhs)
 * which indicates that, if we can succeed in matching the given list of
 * patterns, then the result will be the indicated Rhs.  The Scope component
 * has type:
 *	Scope  ::= [(Var,Expr)]
 * and provides a mapping from variable names to offsets used in the matching
 * process.
 *
 * ------------------------------------------------------------------------*/

#define switchPats(s)	      fst3(s)
#define switchSyms(s)	      snd3(s)
#define switchRhs(s)	      thd3(s)
#define addSym(v,o,s)	      switchSyms(s) = cons(pair(v,o),switchSyms(s))
#define matchMore(sw,c,co,us) nonNull(sw)?ap(FATBAR,pair(c,match(co,sw,us))):c

				       /* There are three kinds of case:   */
#define CONDISCR	      0        /* Constructor			   */
#define INTDISCR	      1        /* Integer (integer const/n+k)	   */
#define VARDISCR	      2        /* variable (or wildcard)	   */

#define isConPat(discr)       (discrKind(discr)==CONDISCR)
#define isVarPat(discr)       (discrKind(discr)==VARDISCR)
#define isIntPat(discr)       (discrKind(discr)==INTDISCR)

static Cell local match(co,sws,us)     /* produce case statement to select */
Int  co;			       /* between switches in sw, matching */
List sws;			       /* pats against values at offsets   */
List us; {			       /* given by us.	co is the current  */
    if (nonNull(us)) {		       /* offset at which new values are   */
	Cell discr;		       /* saved 			   */

	map1Proc(tidyHdPat,hd(us),sws);
	switch (discrKind(discr=hdDiscr(sws))) {
	    case CONDISCR : return matchCon(co,sws,us,discr);
	    case INTDISCR : return matchInt(co,sws,us,discr);
	    case VARDISCR : return matchVar(co,sws,us,discr);
	}
    }
    return joinSw(co,sws);
}

static Void local tidyHdPat(u,s)       /* tidy head of pat list in a switch*/
Offset u;			       /* (Principally eliminating @ pats) */
Cell   s; {
    Cell p = hd(switchPats(s));

thp:switch (whatIs(p)) {
	case ASPAT   : addSym(fst(snd(p)),u,s);
		       p = snd(snd(p));
		       goto thp;

	case LAZYPAT : {   Cell nv	= inventVar();
			   switchRhs(s) = ap(LETREC,
					     pair(remPat(snd(p),nv,NIL),
						  switchRhs(s)));
			   p		= nv;
		       }
		       break;

	case FINLIST : p = mkConsList(snd(p));
		       break;

	case STRCELL : {   Text t = textOf(p);
			   Int  c;
			   p = NIL;
			   while ((c=textToStr(t++)[0])!='\0') {
			       if (c=='\\' && (c=textToStr(t++)[0])!='\\')
				   c = 0;
			       p = ap(consChar(c),p);
			   }
			   p = revOnto(p,nameNil);
		       }
		       break;

    }
    hd(switchPats(s)) = p;
}

static Cell local hdDiscr(sws)	       /* get discriminant of head pattern */
List sws; {			       /* in first branch of a [Switch].   */
    return getHead(hd(fst3(hd(sws))));
}

static Int local discrKind(e)	       /* find kind of discriminant	   */
Cell e; {
    switch (whatIs(e)) {
	case NAME      :
	case TUPLE     :
	case UNIT      :
	case STRCELL   : /* shouldn't be here? */
	case CHARCELL  : return CONDISCR;

#if NPLUSK
	case ADDPAT    :
	case MULPAT    :
#endif
	case INTCELL   : return INTDISCR;

	case VARIDCELL :
	case VAROPCELL :
	case DICTVAR   :
	case WILDCARD  : return VARDISCR;
    }
    internal("discrKind");
    return 0;/*NOTREACHED*/
}

Int discrArity(e)		       /* find arity of discriminant	   */
Cell e; {
    switch (whatIs(e)) {
	case NAME      : return name(e).arity;

	case TUPLE     : return tupleOf(e);

	case UNIT      :
	case STRCELL   : /* shouldn't be here? */
        case FLOATCELL :
	case CHARCELL  :
	case INTCELL   : return 0;

#if NPLUSK
	case ADDPAT    :
	case MULPAT    :
#endif
	case VARIDCELL :
	case VAROPCELL :
	case DICTVAR   :
	case WILDCARD  : return 1;
    }
    internal("discrArity");
    return 0;/*NOTREACHED*/
}

/* --------------------------------------------------------------------------
 * Match on variables:
 * ------------------------------------------------------------------------*/

static Cell local matchVar(co,sws,us,discr)/* matching against a variable  */
Int  co;			       /* does not trigger any evaluation, */
List sws;			       /* but can extend the scope with a  */
List us;			       /* new binding			   */
Cell discr; {
    List varsw = NIL;
    Cell s;

    do {
	s = hd(sws);
	if (discr!=WILDCARD)
	    addSym(discr,hd(us),s);
	switchPats(s) = tl(switchPats(s));
	varsw	      = cons(s,varsw);
	sws	      = tl(sws);
    } while (nonNull(sws) && isVarPat(discr=hdDiscr(sws)));

    s = match(co,rev(varsw),tl(us));
    return matchMore(sws,s,co,us);
}

/* --------------------------------------------------------------------------
 * Match on constructors:
 * ------------------------------------------------------------------------*/

static Cell local matchCon(co,sws,us,discr) /* matching against constructor*/
Int  co;
List sws;
List us;
Cell discr; {
    List tab = NIL;		       /* build table of (discr, [Switch]) */
    Cell s;
    List ps;

    do {
	s	      = hd(sws);
	ps	      = switchPats(s);
	ps	      = appendOnto(getArgs(hd(ps)),tl(ps));
	switchPats(s) = ps;
	tab	      = addConTable(discr,s,tab);
	sws	      = tl(sws);
     } while (nonNull(sws) && isConPat(discr=hdDiscr(sws)));

     s = ap(CASE,pair(hd(us),makeCases(co,rev(tab),tl(us))));
     return matchMore(sws,s,co,us);
}

/* type Table a b = [(a, [b])]
 *
 * addTable		    :: a -> b -> Table a b -> Table a b
 * addTable x y []	     = [(x,[y])]
 * addTable x y (z@(n,sws):zs)
 *		| n == x     = (n,sws++[y]):zs
 *		| otherwise  = (n,sws):addTable x y zs
 */

static List local addConTable(x,y,tab) /* add element (x,y) to table	   */
Cell x, y;
List tab; {
    if (isNull(tab))
	return singleton(pair(x,singleton(y)));
    else if (fst(hd(tab))==x)
	snd(hd(tab)) = appendOnto(snd(hd(tab)),singleton(y));
    else
	tl(tab) = addConTable(x,y,tl(tab));

    return tab;
}

static Cell local makeCases(co,tab,us) /* build CASE construct for constr  */
Int  co;			       /* match 			   */
List tab;
List us; {
     List cases;

     for (cases=NIL; nonNull(tab); tab=tl(tab)) {
	 Cell n   = fst(hd(tab));
	 Int  co1 = co+discrArity(n);
	 cases	  = cons(pair(n,
			      match(co1,
				    snd(hd(tab)),
				    addOffsets(co1,co+1,us))),
			 cases);
     }
     return cases;
}

/* --------------------------------------------------------------------------
 * Match on integers:
 * ------------------------------------------------------------------------*/

static Cell local matchInt(co,sws,us,discr)/* match against integer values */
Int  co;
List sws;
List us;
Cell discr; {
    List tab	= NIL;	 	       /* table of (discr, [Switch]) pairs */
    Cell s	= hd(sws);
#if NPLUSK
    Cell cnkPat = NIL;			/* Current MULPAT or ADDPAT	   */
    Cell intPat = NIL;			/* Current INTCELL pattern	   */
#endif
    List ps;

    do {
#if NPLUSK
        if (whatIs(discr)==INTCELL) {
	    if (nonNull(cnkPat))
		break;
	    if (isNull(intPat))
		intPat = discr;
        }
	else {
	    if (nonNull(intPat))
		break;
	    if (isNull(cnkPat))
		cnkPat = discr;
	    else
		if (fst(cnkPat)!=fst(discr)||intValOf(cnkPat)!=intValOf(discr))
		    break;
		else
		    discr = cnkPat;
	}
#endif

	s	      = hd(sws);
	ps	      = switchPats(s);
	ps	      = appendOnto(getArgs(hd(ps)),tl(ps));
	switchPats(s) = ps;
	tab	      = addConTable(discr,s,tab);
	sws	      = tl(sws);
     } while (nonNull(sws) && isIntPat(discr=hdDiscr(sws)));

     s = ap(CASE,pair(hd(us),makeCases(co,rev(tab),tl(us))));
     return matchMore(sws,s,co,us);
}

/* --------------------------------------------------------------------------
 * Miscellaneous:
 * ------------------------------------------------------------------------*/

static List local addOffsets(m,n,us)   /* addOffsets m n us		   */
Int  m, n;			       /*  = map mkOffset [m,m-1..n] ++ us */
List us; {
    for (; m>=n; n++)
	us = cons(mkOffset(n),us);
    return us;
}

static Cell local mkSwitch(sc,alt)     /* convert Alt into Switch:	   */
List sc;			       /* mkSwitch sc (ps,r) = (ps,sc,r)   */
Pair alt; {
    return triple(fst(alt),sc,snd(alt));
}

static Cell local joinSw(co,sws)       /* Combine list of Switches into rhs*/
Int  co;			       /* using FATBARs as necessary	   */
List sws; {			       /* :: [ ([], Scope, Rhs) ]	   */
    Cell s = hd(sws);

    if (nonNull(tl(sws)) && canFail(thd3(s)))
	return ap(FATBAR,
		  pair(pmcTerm(co,snd3(s),thd3(s)),
		       joinSw(co,tl(sws))));
    return pmcTerm(co,snd3(s),thd3(s));
}

static Bool local canFail(rhs)	       /* Determine if expression (as rhs) */
Cell rhs; {			       /* might ever be able to fail	   */
    switch (whatIs(rhs)) {
	case LETREC  : return canFail(snd(snd(rhs)));
	case GUARDED : return TRUE;    /* could get more sophisticated ..? */
	default      : return FALSE;
    }
}

/* --------------------------------------------------------------------------
 * Lambda Lifter:    replace local function definitions with new global
 *		     functions.  Based on Johnsson's algorithm.
 * ------------------------------------------------------------------------*/

static Cell local lift(co,tr,e)        /* lambda lift term		   */
Int  co;
List tr;
Cell e; {
    switch (whatIs(e)) {
	case GUARDED   : map2Proc(liftPair,co,tr,snd(e));
			 break;

	case FATBAR    : liftPair(co,tr,snd(e));
			 break;

	case CASE      : map2Proc(liftAlt,co,tr,snd(snd(e)));
			 break;

	case COND      : liftTriple(co,tr,snd(e));
			 break;

	case AP        : liftPair(co,tr,e);
			 break;

	case VAROPCELL :
	case VARIDCELL :
	case DICTVAR   : return liftVar(tr,e);

	case LETREC    : return liftLetrec(co,tr,e);

	case UNIT      :
	case TUPLE     :
	case NAME      :
	case SELECT    :
	case DICTCELL  :
	case INTCELL   :
	case FLOATCELL :
	case STRCELL   :
	case OFFSET    :
	case CHARCELL  : break;

	default        : internal("lift");
			 break;
    }
    return e;
}

static Void local liftPair(co,tr,pr)   /* lift pair of terms		   */
Int  co;
List tr;
Pair pr; {
    fst(pr) = lift(co,tr,fst(pr));
    snd(pr) = lift(co,tr,snd(pr));
}

static Void local liftTriple(co,tr,e)  /* lift triple of terms		   */
Int    co;
List   tr;
Triple e; {
    fst3(e) = lift(co,tr,fst3(e));
    snd3(e) = lift(co,tr,snd3(e));
    thd3(e) = lift(co,tr,thd3(e));
}

static Void local liftAlt(co,tr,pr)    /* lift (discr,case) pair	   */
Int  co;
List tr;
Cell pr; {			       /* pr :: (discr,case)		   */
    snd(pr) = lift(co+discrArity(fst(pr)), tr, snd(pr));
}

static Cell local liftVar(tr,e)        /* lift variable 		   */
List tr;
Cell e; {
    Text t = textOf(e);

    while (nonNull(tr) && textOf(fst(hd(tr)))!=t)
	tr = tl(tr);
    if (isNull(tr))
	internal("liftVar");
    return snd(hd(tr));
}

static Cell local liftLetrec(co,tr,e)  /* lift letrec term		   */
Int  co;
List tr;
Cell e; {
    List vs = fst(fst(snd(e)));
    List fs = snd(fst(snd(e)));
    List fds;

    co += length(vs);
    solve(fs);

    for (fds=fs; nonNull(fds); fds=tl(fds)) {
	Triple fundef = hd(fds);
	List   fvs    = fst3(thd3(fundef));
	Cell   n      = newName(textOf(fst3(fundef)));
	Cell   e0;

	for (e0=n; nonNull(fvs); fvs=tl(fvs))
	    e0 = ap(e0,hd(fvs));

	tr	     = cons(pair(fst3(fundef),e0),tr);
	fst3(fundef) = n;
    }

    map2Proc(liftFundef,co,tr,fs);
    if (isNull(vs))
	return lift(co,tr,snd(snd(e)));
    map2Over(lift,co,tr,vs);
    fst(snd(e)) = vs;
    snd(snd(e)) = lift(co,tr,snd(snd(e)));
    return e;
}

static Void local liftFundef(co,tr,fd) /* lift function definition	   */
Int    co;
List   tr;
Triple fd; {
    Int arity = intOf(snd3(fd));
    newGlobalFunction(fst3(fd), 			 /* name	   */
		      arity,				 /* arity	   */
		      fst3(thd3(fd)),			 /* free variables */
		      co+arity, 			 /* current offset */
		      lift(co+arity,tr,thd3(thd3(fd)))); /* lifted case    */
}

/* Each element in a list of fundefs has the form: (v,a,(fvs,ffs,rhs))
 * where fvs is a list of free variables which must be added as extra
 *	     parameters to the lifted version of function v,
 *	 ffs is a list of fundefs defined either in the group of definitions
 *	     including v, or in some outer LETREC binding.
 *
 * In order to determine the correct value for fvs, we must include:
 * - all variables explicitly appearing in the body rhs (this much is
 *   achieved in pmcVar).
 * - all variables required for lifting those functions appearing in ffs.
 *   - If f is a fundef in an enclosing group of definitions then the
 *     correct list of variables to include with each occurrence of f will
 *     have already been calculated and stored in the fundef f.  We simply
 *     take the union of this list with fvs.
 *   - If f is a fundef in the same group of bindings as v, then we iterate
 *     to find the required solution.
 */

#ifdef DEBUG_CODE
static Void dumpFundefs(fs)
List fs; {
    printf("Dumping Fundefs:\n");
    for (; nonNull(fs); fs=tl(fs)) {
        Cell t   = hd(fs);
	List fvs = fst3(thd3(t));
	List ffs = snd3(thd3(t));
	printf("Var \"%s\", arity %d:\n",textToStr(textOf(fst3(t))),
                                         intOf(snd3(t)));
	printf("Free variables: ");
        printExp(stdout,fvs);
	putchar('\n');
	printf("Local functions: ");
        for (; nonNull(ffs); ffs=tl(ffs)) {
	    printExp(stdout,fst3(hd(ffs)));
	    printf("  ");
	}
	putchar('\n');
    }
    printf("----------------\n");
}
#endif

static Void local solve(fs)	       /* Solve eqns for lambda-lifting    */
List fs; {			       /* of local function definitions    */
    Bool hasChanged;
    List fs0, fs1;

    /* initial pass distinguishes between those functions defined in fs and
     * those defined in enclosing LETREC clauses ...
     */

    for (fs0=fs; nonNull(fs0); fs0=tl(fs0)) {
	List fvs = fst3(thd3(hd(fs0)));
	List ffs = NIL;

	for (fs1=snd3(thd3(hd(fs0))); nonNull(fs1); fs1=tl(fs1)) {
	    if (cellIsMember(hd(fs1),fs))	 /* function in same LETREC*/
		ffs = cons(hd(fs1),ffs);
	    else {				 /* enclosing letrec	   */
		List fvs1 = fst3(thd3(hd(fs1)));
		for (; nonNull(fvs1); fvs1=tl(fvs1))
		    if (!cellIsMember(hd(fvs1),fvs))
			fvs = cons(hd(fvs1),fvs);
	    }
	}
	fst3(thd3(hd(fs0))) = fvs;
	snd3(thd3(hd(fs0))) = ffs;
    }

    /* now that the ffs component of each fundef in fs has been restricted
     * to a list of fundefs in fs, we iterate to add any extra free variables
     * that are needed (in effect, calculating the reflexive transitive
     * closure of the local call graph of fs).
     */

    do {
	hasChanged = FALSE;
	for (fs0=fs; nonNull(fs0); fs0=tl(fs0)) {
	    List fvs0 = fst3(thd3(hd(fs0)));
	    for (fs1=snd3(thd3(hd(fs0))); nonNull(fs1); fs1=tl(fs1))
		 if (hd(fs1)!=hd(fs0)) {
		     List fvs1 = fst3(thd3(hd(fs1)));
		     for (; nonNull(fvs1); fvs1=tl(fvs1))
			 if (!cellIsMember(hd(fvs1),fvs0)) {
			     hasChanged = TRUE;
			     fvs0	= cons(hd(fvs1),fvs0);
			 }
		}
	    if (hasChanged) fst3(thd3(hd(fs0))) = fvs0;
	}
    } while (hasChanged);
}

/* --------------------------------------------------------------------------
 * Pre-compiler: Uses output from lambda lifter to produce terms suitable
 *		 for input to code generator.
 * ------------------------------------------------------------------------*/

static List extraVars;	   /* List of additional vars to add to function   */
static Int  numExtraVars;  /* Length of extraVars			   */
static Int  localOffset;   /* offset value used in original definition	   */
static Int  localArity;    /* arity of function being compiled w/o extras  */

/* --------------------------------------------------------------------------
 * Arrangement of arguments on stack prior to call of
 *		   n x_1 ... x_e y_1 ... y_a
 * where
 *	e = numExtraVars,      x_1,...,x_e are the extra params to n
 *	a = localArity of n,   y_1,...,y_a are the original params
 *
 *    offset 1	   :  y_a  }			       STACKPART1
 *	..		   }
 *    offset a	   :  y_1  }
 *
 *    offset 1+a   :  x_e  }			       STACKPART2
 *	..		   }
 *    offset e+a   :  x_1  }
 *
 *    offset e+a+1 :  used for temporary results ...   STACKPART3
 *	..
 *	..
 *
 * In the original defn for n, the offsets in STACKPART1 and STACKPART3
 * are contiguous.  To add the extra parameters we need to insert the
 * offsets in STACKPART2, adjusting offset values as necessary.
 * ------------------------------------------------------------------------*/

static Cell local preComp(e)	       /* Adjust output from compiler to   */
Cell e; {			       /* include extra parameters	   */
    switch (whatIs(e)) {
	case GUARDED   : mapOver(preCompPair,snd(e));
		         break;

	case LETREC    : mapOver(preComp,fst(snd(e)));
		         snd(snd(e)) = preComp(snd(snd(e)));
		         break;

	case COND      : return ap(COND,preCompTriple(snd(e)));

	case FATBAR    : return ap(FATBAR,preCompPair(snd(e)));

	case AP        : return preCompPair(e);

	case CASE      : fst(snd(e)) = preComp(fst(snd(e)));
		         mapProc(preCompCase,snd(snd(e)));
		         break;

	case OFFSET    : return preCompOffset(offsetOf(e));

	case UNIT      :
	case TUPLE     :
	case NAME      :
	case SELECT    :
	case DICTCELL  :
	case INTCELL   :
	case FLOATCELL :
	case STRCELL   :
	case CHARCELL  : break;

	default        : internal("preComp");
    }
    return e;
}

static Cell local preCompPair(e)       /* Apply preComp to pair of Exprs   */
Pair e; {
    return pair(preComp(fst(e)),
		preComp(snd(e)));
}

static Cell local preCompTriple(e)     /* Apply preComp to triple of Exprs */
Triple e; {
    return triple(preComp(fst3(e)),
		  preComp(snd3(e)),
		  preComp(thd3(e)));
}

static Void local preCompCase(e)       /* Apply preComp to (Discr,Expr)    */
Pair e; {
    snd(e) = preComp(snd(e));
}

static Cell local preCompOffset(n)     /* Determine correct offset value   */
Int n; {			       /* for local variable/function arg. */
    if (n>localOffset-localArity)
	if (n>localOffset)				     /* STACKPART3 */
	    return mkOffset(n-localOffset+localArity+numExtraVars);
	else						     /* STACKPART1 */
	    return mkOffset(n-localOffset+localArity);
    else {						     /* STACKPART2 */
	List fvs = extraVars;
	Int  i	 = localArity+numExtraVars;

	for (; nonNull(fvs) && offsetOf(hd(fvs))!=n; --i)
	    fvs=tl(fvs);
	return mkOffset(i);
    }
}

/* --------------------------------------------------------------------------
 * Main entry points to compiler:
 * ------------------------------------------------------------------------*/

Void compileExp() {		       /* compile input expression	   */
    compiler(RESET);

    inputExpr	 = lift(0,NIL,pmcTerm(0,NIL,translate(inputExpr)));
    extraVars	 = NIL;
    numExtraVars = 0;
    localOffset  = 0;
    localArity	 = 0;
    inputCode	 = codeGen(NIL,0,preComp(inputExpr));
    inputExpr	 = NIL;
}

Void compileDefns() {		       /* compile script definitions	   */
    Target t = length(valDefns) + length(overDefns);
    Target i = 0;

    setGoal("Compiling",t);
    for (; nonNull(valDefns); valDefns=tl(valDefns)) {
	mapProc(compileGlobalFunction,transBinds(hd(valDefns)));
	soFar(i++);
    }
    for (; nonNull(overDefns); overDefns=tl(overDefns)) {
        compileMemberFunction(hd(overDefns));
	soFar(i++);
    }
    done();
}

static Void local compileGlobalFunction(bind)
Pair bind; {
    Name n     = findName(textOf(fst(bind)));
    List defs  = snd(bind);
    Int  arity = length(fst(hd(defs)));

    if (isNull(n))
	internal("compileGlobalFunction");
    compiler(RESET);
    map1Over(mkSwitch,NIL,defs);
    newGlobalFunction(n,
		      arity,
		      NIL,
		      arity,
		      lift(arity,
			   NIL,
			   match(arity,
				 defs,
				 addOffsets(arity,1,NIL))));
}

static Void local compileMemberFunction(n)
Name n; {
    List defs  = name(n).defn;
    Int  arity = length(fst(hd(defs)));

    compiler(RESET);
    mapProc(transAlt,defs);
    map1Over(mkSwitch,NIL,defs);
    newGlobalFunction(n,
		      arity,
		      NIL,
		      arity,
		      lift(arity,
			   NIL,
			   match(arity,
				 defs,
				 addOffsets(arity,1,NIL))));
}

static Void local newGlobalFunction(n,arity,fvs,co,e)
Name n;
Int  arity;
List fvs;
Int  co;
Cell e; {
    extraVars	  = fvs;
    numExtraVars  = length(extraVars);
    localOffset   = co;
    localArity	  = arity;
    name(n).arity = arity+numExtraVars;
    name(n).code  = codeGen(n,name(n).arity,preComp(e));
}

/* --------------------------------------------------------------------------
 * Compiler control:
 * ------------------------------------------------------------------------*/

Void compiler(what)
Int what; {
    switch (what) {
	case INSTALL :
	case RESET   : freeVars      = NIL;
		       freeFuns      = NIL;
		       freeBegin     = mkOffset(0);
		       extraVars     = NIL;
		       numExtraVars  = 0;
		       localOffset   = 0;
		       localArity    = 0;
		       break;

	case MARK    : mark(freeVars);
		       mark(freeFuns);
		       mark(extraVars);
		       break;
    }
}

/*-------------------------------------------------------------------------*/
