/* --------------------------------------------------------------------------
 * preds.c:     Copyright (c) Mark P Jones 1991-1994.   All rights reserved.
 *              See goferite.h for details and conditions of use etc...
 *              Gofer version 2.30 March 1994
 *
 * Part of type checker dealing with predicates and entailment.
 * ------------------------------------------------------------------------*/

Bool anyEvidence  = TRUE;		/* no need to search for `best'    */
					/* evidence - any will do.	   */
Int  maxEvidLevel = 8;			/* maximum no. of dict selects     */
Bool silentEvFail = TRUE;		/* TRUE => fail silently if	   */
					/*         maxEvidLevel exceeded   */

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static Cell   local assumeEvid        Args((Cell,Int));
static List   local makeEvidArgs      Args((List,Int));
static Void   local markPred          Args((Cell));
static List   local copyPreds         Args((List));
static Cell   local copyPred	      Args((Cell,Int));
static Void   local qualify           Args((List,Cell));
static Void   local qualifyBinding    Args((List,Cell));

static Cell   local instsOverlap      Args((Inst,Inst));
static Bool   local instsCompare      Args((Inst,Inst));

static Bool   local oneWayMatches     Args((Cell,Int,Cell,Int));
static Bool   local oneWayTypeMatches Args((Type,Int,Type,Int));

static Cell   local proveFrom         Args((List,Cell,Int));
static List   local evidFrom          Args((Cell,Int));
static Void   local explicitProve     Args((Int,String,Cell,List,List));
static Cell   local addEvidArgs	      Args((Int,String,Cell,List,List,Cell));
static Void   local cantProve	      Args((Int,String,List,Cell,Cell));
static List   local simplify          Args((List));
static Void   local overEvid          Args((Cell,Cell));

static List   local elimConstPreds    Args((Int,String,Cell,List));
static Bool   local scanPred	      Args((Cell,Int));
static Bool   local scanTyvar         Args((Int));
static Bool   local scanType          Args((Type,Int));

static Cell   local makeInst	      Args((Int,String,Cell,Cell,Int));
static Cell   local makeDict          Args((Cell,Int));

static Void   local indexPred	      Args((Class,Cell,Int));
static Void   local indexType	      Args((Type,Int));
static Void   local indexLeaf	      Args((Cell));

/* --------------------------------------------------------------------------
 * Predicate sets:
 *
 * A predicate set is represented by a list of triples (pi, o, used)
 * where o is the offset for types in pi, with evidence required at the
 * node pointed to by used (which is taken as a dictionary parameter if
 * no other evidence is available).  Note that the used node will be
 * overwritten at a later stage if evidence for that predicate is found
 * subsequently.
 * ------------------------------------------------------------------------*/

static List preds;		       /* current predicate list	   */

static Cell local assumeEvid(pi,o)     /* add predicate pi (offset o) to   */
Cell pi;			       /* preds with new dictionary var and*/
Int  o; {			       /* return that dictionary variable  */
    Cell nd = inventDictVar();
    preds   = cons(triple(pi,mkInt(o),nd),preds);
    return nd;
}

static List local makeEvidArgs(qs,o)   /* make list of predicate assumps.  */
List qs;			       /* from qs (offset o), with new dict*/
Int  o; {			       /* vars for each predicate	   */
    List result;
    for (result=NIL; nonNull(qs); qs=tl(qs))
	result = cons(triple(hd(qs),mkInt(o),inventDictVar()),result);
    return rev(result);
}

static Void local markPred(pi)	       /* marked fixed type variables in pi*/
Cell pi; {
    Cell cl = fst3(pi);
    Int  o  = intOf(snd3(pi));

    for (; isAp(cl); cl=fun(cl))
	markType(arg(cl),o);
}

static List local copyPreds(qs)        /* copy list of predicates          */
List qs; {
    List result;
    for (result=NIL; nonNull(qs); qs=tl(qs)) {
	Cell pi = hd(qs);
	result  = cons(copyPred(fst3(pi),intOf(snd3(pi))),result);
    }
    return rev(result);
}

static Cell local copyPred(pi,o)	/* copy single predicate (or part  */
Cell pi;				/* thereof) ...			   */
Int  o; {
    if (isAp(pi)) {
	Cell temp = copyPred(fun(pi),o);/* to ensure correct order of eval.*/
	return ap(temp,copyType(arg(pi),o));
    }
    else
	return pi;
}

static Void local qualify(qs,alt)	/* Add extra dictionary args to	   */
List qs;				/* qualify alt by predicates in qs */
Cell alt; {				/* :: ([Pat],Rhs)		   */
    List ds;
    for (ds=NIL; nonNull(qs); qs=tl(qs))
	ds = cons(thd3(hd(qs)),ds);
    fst(alt) = revOnto(ds,fst(alt));
}

static Void local qualifyBinding(qs,b)	/* Add extra dict args to each	   */
List qs;				/* alternative in function binding */
Cell b ; {
    if (!isVar(fst(b)))			/* check for function binding	   */
	internal("qualifyBinding");
    map1Proc(qualify,qs,snd(snd(b)));
}

/* --------------------------------------------------------------------------
 * Check for overlapping instances of class:
 * ------------------------------------------------------------------------*/

static Cell local instsOverlap(ia,ib)	/* see if heads of instances can be*/
Inst ia, ib; {				/* unified			   */
    Int  alpha, beta;
    Cell pa, pb;

    emptySubstitution();
    matchMode = FALSE;
    alpha     = newKindedVars(inst(ia).sig);
    pa	      = inst(ia).head;
    beta      = newKindedVars(inst(ib).sig);
    pb	      = inst(ib).head;
    while (isAp(pa) && isAp(pb)) {
	if (!unify(arg(pa),alpha,arg(pb),beta))
	    return NIL;
	pa = fun(pa);
	pb = fun(pb);
    }
    return copyPred(inst(ia).head,alpha);
}

static Bool local instsCompare(ia,ib)	/* see if ib is an instance of ia  */
Inst ia, ib; {
    Int  alpha, beta;
    Cell pa, pb;

    emptySubstitution();
    alpha = newKindedVars(inst(ia).sig);
    pa	  = inst(ia).head;
    beta  = newKindedVars(inst(ib).sig);
    pb	  = inst(ib).head;
    return oneWayMatches(pa,alpha,pb,beta);
}

Void insertInst(line,cl,in)		/* insert instance into class	   */
Int   line;
Class cl;
Inst  in; {
    List done = NIL;
    List ins  = class(cl).instances;

    while (nonNull(ins)) {
	Cell tmp = tl(ins);
	Cell pi  = instsOverlap(in,hd(ins));
	if (nonNull(pi)) {
	    Bool bef = instsCompare(hd(ins),in);
	    Bool aft = instsCompare(in,hd(ins));
	    if (bef==aft) {
		class(cl).instances = revOnto(done,ins);
		ERROR(line) "Overlapping instances for class \"%s\"",
			    textToStr(class(inst(in).cl).text)
		ETHEN
		ERRTEXT "\n*** This instance   : " ETHEN ERRPRED(inst(in).head);
		ERRTEXT "\n*** Overlaps with   : " ETHEN
						   ERRPRED(inst(hd(ins)).head);
		ERRTEXT "\n*** Common instance : " ETHEN
					           ERRPRED(pi);
		ERRTEXT "\n"
		EEND;
	    }
	    if (bef)
		break;
	}
	tl(ins) = done;
	done    = ins;
	ins     = tmp;
    }
    class(cl).instances = revOnto(done,cons(in,ins));
}

/* --------------------------------------------------------------------------
 * One way matching of instance headers with predicates:
 * ------------------------------------------------------------------------*/

static Bool local oneWayMatches(p1,o1,p2,o2)
Cell p1, p2;				/* determine if S(p1,o1) = (p2,o2) */
Int  o1, o2; {				/* for some substitution S	   */
    while (isAp(p1) && isAp(p2)) {
	if (!oneWayTypeMatches(arg(p1),o1,arg(p2),o2))
	    return FALSE;
	p1 = fun(p1);
	p2 = fun(p2);
    }
    return TRUE;
}

static Bool local oneWayTypeMatches(t1,o1,t2,o2)
Type t1, t2;				/* determine if S(t1,o1) = (t2,o2) */
Int  o1, o2; {				/* for some substitution S	   */
    Tyvar *tyv;
    Cell  h1,h2;			/* heads of (t1,o1) and (t2,o2)	   */
    Int   a1,a2;			/* #args of (t1,o1) and (t2,o2)	   */

    while (h1=getDerefHead(t1,o1),	/* eliminate synonym at hd (t1,o1) */
	   a1=argCount,
	   (isSynonym(h1) && tycon(h1).arity<=a1)) {
	expandSyn(h1,a1,&t1,&o1);
	if (isOffset(t1)) {
	    tyv = tyvar (o1 + offsetOf(t1));
	    t1  = tyv -> bound;
	    o1  = tyv -> offs;
	}
    }

    deRef(tyv,t2,o2);			/* eliminate synonym at hd (t2,o2) */
    while (h2=getDerefHead(t2,o2),
	   a2=argCount,
	   (isSynonym(h2) && tycon(h2).arity<=a2)) {
	expandSyn(h2,a2,&t2,&o2);
	deRef(tyv,t2,o2);
    }

    /* there are certain conditions under which there is no way to match   */
    /* the type (t1,o1) with (t2,o2):					   */
    /* - if (t1,o1) has more arguments than (t2,o2)			   */
    /* - if (t1,o1) has fewer arguments than (t2,o2) and h1 not a variable */
    /* - if h1 not a variable and h2!=h1				   */

    if (a1>a2 || (!isOffset(h1) && (a1<a2 || h1!=h2)))
	return FALSE;

    while (isAp(t1)) {			/* loop through arguments	   */
	if (!oneWayTypeMatches(arg(t1),o1,arg(t2),o2))
	    return FALSE;
	t1 = fun(t1);
	t2 = fun(t2);
	deRef(tyv,t2,o2);
    }

    if (isOffset(t1)) {			/* (t1,o1) is a variable	   */
	tyv = tyvar(o1 + offsetOf(t1));
	if (tyv->bound)
	    return sameType(tyv->bound,tyv->offs,t2,o2);
        if (!eqKind(tyv->kind,getKind(t2,o2)))
	    return FALSE;
	tyv->bound = t2;
	tyv->offs  = o2;
    }
    return TRUE;
}

Bool typeInstOf(type,pt)		/* test if type is instance of poly*/
Type type, pt; {			/* type pt (not-overloaded)	   */
    Bool result;
    Int  alpha = 0, beta = 0;
    typeChecker(RESET);

    instantiate(pt);			/* instantiate given polytype	   */
    alpha = typeOff;
    pt    = typeIs;
    if (predsAre)
	internal("typeInstOf"); 

    instantiate(type);			/* and type against which it will  */
    beta = typeOff;			/* be compared			   */
    type = typeIs;
    if (predsAre)
	internal("typeInstOf"); 

    result = oneWayTypeMatches(pt,alpha,type,beta);
    typeChecker(RESET);
    return result;
}

/* --------------------------------------------------------------------------
 * Predicate entailment:
 * ------------------------------------------------------------------------*/

static Cell  classProve;
static Cell  predProve;
static Int   offsetProve;
static Int   evDepth;
static Int   evidLevel;

static Cell local proveFrom(qs,pi,o)	/* Construct evidence for predicate*/
List  qs;				/* pi, offset o from predicates qs,*/
Cell  pi;                               /* returning NIL if qs ||- (pi,o)  */
Int   o; {                              /* does not hold.                  */
    List  bestEvid  = NIL;
    Int   bestDepth = (-1);

    classProve  = getHead(pi);
    predProve   = pi;
    offsetProve = o;
    evidLevel   = 0;

    for (; nonNull(qs); qs=tl(qs)) {
	Cell qpi   = hd(qs);
	List dSels = evidFrom(fst3(qpi),intOf(snd3(qpi)));

	if (evDepth>=0 && (isNull(bestEvid) || evDepth<bestDepth)) {
	    bestEvid  = revOnto(dSels,thd3(qpi));
	    bestDepth = evDepth;
	    if (anyEvidence)
		return bestEvid;
	}
    }
    return bestEvid;
}

static List local evidFrom(pi,o)	/* recursive part of proveFrom	   */
Cell pi;				/* return list of dict selectors   */
Int  o; {				/* for optimal (shortest) evidence */
    Class cpi	    = getHead(pi);	/* returns evDepth for number of   */
    List  bestYet   = NIL;		/* selectors used, or (-1) if no   */
    Int   bestDepth = (-1);		/* solution possible.		   */
    Int   doffs;
    Int   beta;
    List  cs, is;

    if (evidLevel++ >= maxEvidLevel) {	/* crude attempt to catch loops	   */
	if (silentEvFail)
	    goto end;

	ERROR(0) "Possible loop for instance " ETHEN
		 ERRPRED(copyPred(predProve,offsetProve));
	ERRTEXT  "\n"
	EEND;
    }

    if (classProve==cpi) {				/* preds match?	   */
	Cell pi1 = pi;
	Cell pi2 = predProve;
	do {
	    if (!sameType(arg(pi1),o,arg(pi2),offsetProve))
		break;
	    pi1 = fun(pi1);
	    pi2 = fun(pi2);
	} while (isAp(pi1) && isAp(pi2));

	if (!isAp(pi1) && !isAp(pi2)) {
	    evDepth = 0;
	    return NIL;
	}
    }

    doffs = 1 + class(cpi).numMembers;			/* 1st superclass  */

    beta  = newKindedVars(class(cpi).sig);		/* match predicate */
    if (!oneWayMatches(class(cpi).head,beta,pi,o))	/* against class   */
	internal("evidFrom");				/* header	   */

    for (cs=class(cpi).supers; nonNull(cs); cs=tl(cs)) {/* scan supers...  */
	List dSels = evidFrom(hd(cs),beta);
	if (evDepth>=0 && (isNull(bestYet) || evDepth+1<bestDepth)) {
	    bestYet   = cons(mkSelect(doffs),dSels);
	    bestDepth = evDepth+1;
	    if (anyEvidence)
		goto end;
	}
	doffs++;
    }

    for (is=class(cpi).instances; nonNull(is); is=tl(is)) {
	Inst in = hd(is);				/* look through	   */
	beta    = newKindedVars(inst(in).sig);		/* instances	   */
	if (oneWayMatches(inst(in).head,beta,pi,o)) {
	    for (cs=inst(in).specifics; nonNull(cs); cs=tl(cs)) {
		List dSels = evidFrom(hd(cs),beta);
		if (evDepth>=0 && (isNull(bestYet) || evDepth+1<bestDepth)) {
		    bestYet   = cons(mkSelect(doffs),dSels);
		    bestDepth = evDepth+1;
		    if (anyEvidence)
			goto end;
		}
		doffs++;
	    }
	    break; /* at most one instance matches... */
	}
        else
	    freeTypeVars(beta);
    }

end:evidLevel--;
    evDepth = bestDepth;
    return bestYet;
}

static Void local explicitProve(l,wh,e,given,reqd)
Int    l;				/* construct evidence for reqd	   */
String wh;				/* predicates from given preds	   */
Cell   e;
List   given, reqd; {
    for (; nonNull(reqd); reqd=tl(reqd)) {
	Cell pi = hd(reqd);
	Cell ev = proveFrom(given,fst3(pi),intOf(snd3(pi)));
	if (isNull(ev))
	    cantProve(l,wh,copyPreds(given),e,
		      copyPred(fst3(pi),intOf(snd3(pi))));
	overEvid(thd3(pi),ev);
    }
}

static Cell local addEvidArgs(l,wh,e,given,reqd,f)
Int    l;
String wh;
Cell   e;
List   given, reqd;
Cell   f; {
    for (; nonNull(reqd); reqd=tl(reqd)) {
	Cell pi = hd(reqd);
	Cell ev = proveFrom(given,fst3(pi),intOf(snd3(pi)));
	if (isNull(ev))
	    cantProve(l,wh,copyPreds(given),e,
		      copyPred(fst3(pi),intOf(snd3(pi))));
	f = ap(f,ev);
    }
    return f;
}

static Void local cantProve(l,wh,context,e,pi)
Int    l;				/* produce error message when an   */
String wh;				/* instance of a class cannot be   */
List   context;				/* constructed			   */
Cell   e;
Cell   pi; {
    ERROR(l) "Cannot derive instance in %s", wh ETHEN
    ERRTEXT  "\n*** Expression        : " ETHEN ERREXPR(e);
    ERRTEXT  "\n*** Context           : " ETHEN ERRCONTEXT(context);
    ERRTEXT  "\n*** Required instance : " ETHEN ERRPRED(pi);
    ERRTEXT  "\n"
    EEND;
}

/* --------------------------------------------------------------------------
 * Predicate set Simplification:
 *
 * This function calculates a minimal equivalent subset of a given set of
 * predicates.  I believe this algorithm will work for any entailment
 * relation, although I have only checked this for the particular relation
 * coded in the above.
 * ------------------------------------------------------------------------*/

static List local simplify(qs)		/* Simplify predicates in qs,      */
List qs; {				/* returning equiv minmal subset   */
    List result = qs;
    Int  n      = length(qs);

    while (0<n--) {
	Cell pi = hd(result);
	Cell ev = proveFrom(tl(result),fst3(pi),intOf(snd3(pi)));
	if (nonNull(ev)) {
	    overEvid(thd3(pi),ev);
	    result = tl(result);
	}
	else {
	    Cell temp  = tl(result);
	    tl(result) = NIL;
	    result     = appendOnto(temp,result);
	}
    }
    return result;
}

static Void local overEvid(c,ev)	/* overwrite evidence (possibly	   */
Cell c;					/* including indirection; select0) */
Cell ev; {
    if (isPair(ev) && isSelect(fst(ev)))
	overwrite(c,ev);		/* overwrite with dict selection   */
    else {
	fst(c) = mkSelect(0);		/* indirect to dict variable	   */
	snd(c) = ev;
    }
}

/* --------------------------------------------------------------------------
 * Deal with constant and locally constant predicates:
 * ------------------------------------------------------------------------*/

static Int numFixedVars;		/* number of fixed vars found	   */

static List local elimConstPreds(l,wh,e,ps)
Int    l;
String wh;
Cell   e;
List   ps; {
    List qs = NIL;

    while (nonNull(preds)) {
	Cell pi = hd(preds);
	Cell nx = tl(preds);

	numFixedVars = 0;
	if (scanPred(fst3(pi),intOf(snd3(pi)))) {	/* contains generic*/
	    tl(preds) = qs;
	    qs	      = preds;
	}
	else if (numFixedVars>0) {			/* only fixed vars */
	    tl(preds) = ps;
	    ps	      = preds;
	}
	else						/* constant types  */
	    overwrite(thd3(pi),makeInst(l,wh,e,fst3(pi),intOf(snd3(pi))));

	preds = nx;
    }
    preds = qs;
    return ps;
}

static Bool local scanPred(pi,o)	/* scan pred (pi,o) to determine if*/
Cell pi;				/* it is constant or locally-const */
Int  o; {				/* by counting fixed & generic vars*/
    for (; isAp(pi); pi=fun(pi))
	if (scanType(arg(pi),o))
	    return TRUE;
    return FALSE;
}

static Bool local scanTyvar(vn)		/* return TRUE if type var contains*/
Int vn; {				/* a generic variable, counting the*/
    Tyvar *tyv = tyvar(vn);		/* number of fixed variables	   */

    if (tyv->bound)
	return scanType(tyv->bound, tyv->offs);
    else if (tyv->offs == FIXED_TYVAR) {
	numFixedVars++;
	return FALSE;
    }
    return TRUE;
}

static Bool local scanType(t,o)		/* Return TRUE if (t,o) contains   */
Type t; 				/* a generic variable		   */
Int  o; {
    switch (whatIs(t)) {
	case AP      : return scanType(fst(t),o) || scanType(snd(t),o);
	case OFFSET  : return scanTyvar(o+offsetOf(t));
	case INTCELL : return scanTyvar(intOf(t));
    }
    return FALSE;
}

/* -----------------------------------------------------------------------
 * Dictionary construction:
 *
 * 0 | class(c).numMembers | class(c).numSupers | inst(in).numSpecifics |
 * ----------------------------------------------------------------------- */

static Cell   instPred;
static Int    instOffs;
static Int    instDepth;
static Cell   instExpr;
static String instWhere;
static Int    instLine;

static Cell local makeInst(l,wh,e,pi,o)	/* Build instance, keeping track of*/
Int    l;				/* top-level required instance for */
String wh;				/* benefit of error reporting...   */
Cell   e;
Cell   pi;
Int    o; {
    Cell result;

    instPred  = pi;
    instOffs  = o;
    instDepth = 0;
    instExpr  = e;
    instWhere = wh;
    instLine  = l;
    result    = makeDict(pi,o);
    instPred  = NIL;
    instExpr  = NIL;
    return result;
}

static Idx  lastIdx, currIdx;		/* used to describe position in idx*/

static Cell local makeDict(pi,o)	/* Build dictionary for predicate  */
Cell   pi;
Int    o; {
    Class c = getHead(pi);
    List  xs, is, ds;
    Int   alpha, beta, doffs;
    Dict  dc;
    Inst  in;

    indexPred(c,pi,o);                          	/* dict has already*/
    if (currIdx!=NODICT)				/* been built?     */
	return dict(currIdx);

    for (xs=class(c).instances; nonNull(xs); xs=tl(xs)){/* No; then try and*/
	in   = hd(xs);					/* find a matching */
	beta = newKindedVars(inst(in).sig);		/* instance to use */
        if (oneWayMatches(inst(in).head,beta,pi,o))	/* to construct the*/
	    break;					/* required dict   */
	else
	    freeTypeVars(beta);
    }

    if (isNull(xs)) {					/* No suitable inst*/
	clearMarks();
	ERROR(instLine) "Cannot derive instance in %s", instWhere ETHEN
	ERRTEXT		"\n*** Expression        : " ETHEN
        ERREXPR(instExpr);
	ERRTEXT		"\n*** Required instance : " ETHEN
	ERRPRED(copyPred(instPred,instOffs));
	if (instDepth>0) {
	    ERRTEXT     "\n*** No subdictionary  : " ETHEN
	    ERRPRED(copyPred(pi,o));
	}
	ERRTEXT  "\n"
	EEND;
    }

    alpha = newKindedVars(class(c).sig);		/* match against   */
    if (!oneWayMatches(class(c).head,alpha,pi,o))	/* class header	   */
	internal("makeDict");

    instDepth++;

    dc       = idx(lastIdx).match			/* alloc new dict  */
	     = newDict(1 + class(c).numMembers		/* and add to index*/
			 + class(c).numSupers
			 + inst(in).numSpecifics);
    dict(dc) = mkDict(dc);				/* self reference  */
    doffs    = 1 + class(c).numMembers;
    for (xs=class(c).supers; nonNull(xs); xs=tl(xs))	/* super classes   */
	dict(dc+doffs++) = makeDict(hd(xs),alpha);
    for (xs=inst(in).specifics; nonNull(xs); xs=tl(xs))	/* specifics	   */
	dict(dc+doffs++) = makeDict(hd(xs),beta);

    xs = class(c).members;				/* member function */
    ds = class(c).defaults;				/* implementations */
    is = inst(in).implements;
    for (doffs=1; nonNull(xs); xs=tl(xs)) {
	if (nonNull(is) && nonNull(hd(is)))
	    dict(dc+doffs++) = ap(hd(is),dict(dc));
	else if (nonNull(ds) && nonNull(hd(ds)))
            dict(dc+doffs++) = ap(hd(ds),dict(dc));
	else
	    dict(dc+doffs++) = ap(nameUndefMem,hd(xs));

	if (nonNull(is)) is=tl(is);
	if (nonNull(ds)) ds=tl(ds);
    }

#ifdef DEBUG_CODE
printf("Just made dictionary {dict%d}@%d for ",dc,dict(dc));
printPred(stdout,copyPred(pi,o));
putchar('\n');
printf("breakdown = 1+%d+%d+%d\n",class(c).numMembers,
				  class(c).numSupers,
				  inst(in).numSpecifics);
{
    int i;
    int size = 1+class(c).numMembers+class(c).numSupers+inst(in).numSpecifics;
    for (i=0; i<size; i++) {
         printf("dict(%d) = ",dc+i);
         printExp(stdout,dict(dc+i));
         putchar('\n');
    }
    printf("--------------------\n");
}
#endif
    instDepth--;
    return dict(dc);
}

/* --------------------------------------------------------------------------
 * Locate entry in an index corresponding to a given (constant) predicate:
 * ------------------------------------------------------------------------*/

static Idx firstIdx;

static Void local indexPred(c,pi,o)	/* scan over a monopredicate (i.e a*/
Class c;				/* predicate with monotype args),  */
Cell  pi;				/* producing an indexing string of */
Int   o; {				/* type constrs, and using them to */
    firstIdx =				/* move through a particular index */
    lastIdx  =
    currIdx  = class(c).dictIndex;
    for (; isAp(pi); pi=fun(pi))
	 indexType(arg(pi),o);
    class(c).dictIndex = firstIdx;
}

static Void local indexType(t,o)	/* scan a monotype as part of the  */
Type t;					/* indexPred process.		   */
Int  o; {
    Cell  temp;
    Tyvar *tyv;

    for (;;) {				/* dereference bound vars/synonyms */
	deRef(tyv,t,o);
	if (tyv) internal("indexType");	/* monotypes cannot contain tyvars */

	temp = getDerefHead(t,o);	/* check for type synonym...	   */
	if (isSynonym(temp) && argCount>=tycon(temp).arity)
	    expandSyn(temp,argCount,&t,&o);
	else
	    break;
    }

    /* now we've `evaluated (t,o) to whnf': Con t1 t2 ... tn, we output the*/
    /* constructor Con as a leaf and then go thru' tn, ..., t2, t1 in turn.*/
    /* Admittedly, this gives a less than intuitive mapping of monopreds to*/
    /* strings of type constructors, but it is sufficient for the moment.  */

    indexLeaf(temp);
    while (isAp(t)) {
	indexType(arg(t),o);
	t = fun(t);
	deRef(tyv,t,o);
    }
}

static Void local indexLeaf(lf)		/* adjust pointers into current idx*/
Cell lf; {				/* having detected type constructor*/
    if (currIdx==NOIDX) {		/* lf whilst indexing over a type  */
	if (lastIdx==NOIDX)
	    lastIdx = firstIdx = newIdx(lf);
	else
	    lastIdx = idx(lastIdx).match = newIdx(lf);
	currIdx = NOIDX;
    }
    else {
	while (idx(currIdx).test!=lf) {
	    if (idx(currIdx).fail==NOIDX) {
		lastIdx = idx(currIdx).fail = newIdx(lf);
		currIdx = NOIDX;
		return;
	    }
	    else
		currIdx = idx(currIdx).fail;
	}
	lastIdx = currIdx;
	currIdx = idx(currIdx).match;
    }
}

Dict listMonadDict() {			/* look for a dict for Monad [ ]   */
    if (nonNull(classMonad)) {
	currIdx = class(classMonad).dictIndex;
	while (currIdx!=NOIDX && idx(currIdx).test!=LIST)
	    currIdx = idx(currIdx).fail;
	if (currIdx!=NOIDX)
	    return idx(currIdx).match;
    }
    return NODICT;
}

/*-------------------------------------------------------------------------*/
