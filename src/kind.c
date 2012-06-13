/* --------------------------------------------------------------------------
 * kind.c:      Copyright (c) Mark P Jones 1991-1994.   All rights reserved.
 *              See goferite.h for details and conditions of use etc...
 *              Gofer version 2.30 March 1994
 *
 * Part of type checker dealing with kind inference
 * ------------------------------------------------------------------------*/

#define newKindvars(n)	newTyvars(n)	/* to highlight uses of type vars  */
					/* as kind variables		   */

Bool kindExpert = FALSE;		/* TRUE => display kind errors in  */
					/*	   full detail		   */

/* --------------------------------------------------------------------------
 * Kind checking code:
 * ------------------------------------------------------------------------*/

static Void local kindError(l,c,in,wh,k,o)
Int    l;				/* line number near constuctor exp */
Constr c;				/* constructor			   */
Constr in;				/* context (if any)		   */
String wh;				/* place in which error occurs	   */
Kind   k;				/* expected kind (k,o)		   */
Int    o; {				/* inferred kind (typeIs,typeOff)  */
    clearMarks();

    if (!kindExpert) {			/* for those with a fear of kinds  */
	ERROR(l) "Illegal type" ETHEN
	if (nonNull(in)) {
	    ERRTEXT " \"" ETHEN ERRTYPE(in);
	    ERRTEXT "\""  ETHEN
	}
	ERRTEXT " in %s\n", wh
	EEND;
    }

    ERROR(l) "Kind error in %s", wh ETHEN
    if (nonNull(in)) {
	ERRTEXT "\n*** expression     : " ETHEN ERRTYPE(in);
    }
    ERRTEXT "\n*** constructor    : " ETHEN ERRTYPE(c);
    ERRTEXT "\n*** kind           : " ETHEN ERRKIND(copyType(typeIs,typeOff));
    ERRTEXT "\n*** does not match : " ETHEN ERRKIND(copyType(k,o));
    if (unifyFails) {
	ERRTEXT "\n*** because        : %s", unifyFails ETHEN
    }
    ERRTEXT "\n"
    EEND;
}

#define shouldKind(l,c,in,wh,k,o) if (!kunify(typeIs,typeOff,k,o)) \
				      kindError(l,c,in,wh,k,o)
#define checkKind(l,c,in,wh,k,o)  kindConstr(l,c); shouldKind(l,c,in,wh,k,o)
#define inferKind(k,o)		  typeIs=k; typeOff=o

static Int  locCVars;			/* offset to local variable kinds  */
static List unkindTypes;		/* types in need of kind annotation*/

static Void local kindConstr(l,c)	/* Determine kind of constructor   */
Int  l;
Cell c; {
    Cell h = getHead(c);
    Int  n = argCount;

    if (isSynonym(h) && n<tycon(h).arity) {
	ERROR(l) "Not enough arguments for type synonym \"%s\"",
		 textToStr(tycon(h).text)
	EEND;
    }

    if (n==0)				/* trivial case, no arguments	   */
	typeIs = kindAtom(c);
    else {				/* non-trivial application	   */
	static String app = "constructor application";
	Cell   a = c;
	Int    i;
	Kind   k;
	Int    beta;

	varKind(n);
	beta   = typeOff;
	k      = typeIs;

	typeIs = kindAtom(h);		/* h  :: v1 => ... => vn => w	   */
        shouldKind(l,h,c,app,k,beta);

	for (i=n; i>0; --i) {		/* ci :: vi for each 1 <- 1..n	   */
	    checkKind(l,arg(a),c,app,var,beta+i-1);
	    a = fun(a);
	}
	tyvarType(beta+n);		/* inferred kind is w		   */
    }
}

static Kind local kindAtom(c)		/* Find kind of atomic constructor */
Cell c; {
    switch (whatIs(c)) {
	case LIST   : return simpleKind(1);		/*[_]::* -> *	   */
	case UNIT   : return STAR;			/*() ::*	   */
	case TUPLE  : return simpleKind(tupleOf(c));	/*(,)::* -> * -> * */
	case ARROW  : return simpleKind(2);		/* ->::* -> * -> * */
	case OFFSET : return mkInt(locCVars+offsetOf(c));
	case TYCON  : return tycon(c).kind;
    }
    internal("kindAtom");
    return STAR;/* not reached */
}

static Void local kindPred(line,pred)	/* Check kinds of arguments in pred*/
Int  line;
Cell pred; {
    static String predicate = "class constraint";
    Class c   = getHead(pred);		/* get class name		   */
    List  as  = getArgs(pred);		/* get arguments		   */
    Cell  sig = class(c).sig;		/* get kind signature to match	   */

    while (nonNull(sig)) {
	checkKind(line,hd(as),NIL,predicate,hd(sig),0);
	sig = tl(sig);
	as  = tl(as);
    }
}

static Void local kindType(line,wh,type)/* check that (poss qualified) type*/
Int    line;				/* is well-kinded		   */
String wh;
Type   type; {
    locCVars = 0;
    if (isPolyType(type)) {		/* local constructor vars reqd?	   */
	locCVars    = newKindvars(selectOf(polySigOf(type)));
	unkindTypes = cons(pair(mkInt(locCVars),snd(type)),unkindTypes);
	type	    = monoTypeOf(type);
    }
    if (whatIs(type)==QUAL) {		/* examine context (if any)	   */
	map1Proc(kindPred,line,fst(snd(type)));
	type = snd(snd(type));
    }
    checkKind(line,type,NIL,wh,STAR,0);	/* finally, check type part	   */
}

static Void local fixKinds() {		/* add kind annotations to types   */
    for (; nonNull(unkindTypes); unkindTypes=tl(unkindTypes)) {
	Pair pr   = hd(unkindTypes);
	if (isSelect(fst(snd(pr)))) {	/* just in case two refs point to a*/
	    Int  beta = intOf(fst(pr));	/* single type			   */
	    Int  n    = selectOf(fst(snd(pr)));
	    Kind k    = STAR;
	    while (n-- > 0)
		k = ap(copyKindvar(beta+n),k);
	    fst(snd(pr)) = k;
#ifdef DEBUG_KINDS
	    printf("Type expression: ");
	    printType(stdout,snd(snd(pr)));
	    printf(" ::: ");
	    printKind(stdout,k);
	    printf("\n");
#endif
	}
    }
}

/* --------------------------------------------------------------------------
 * Kind checking of groups of type constructors and classes:
 * ------------------------------------------------------------------------*/

Void kindTCGroup(tcs)			/* find kinds for mutually rec. gp */
List tcs; {				/* of tycons and classes	   */
    typeChecker(RESET);
    mapProc(initTCKind,tcs);
    mapProc(kindTC,tcs);
    mapProc(genTC,tcs);
    fixKinds();
    typeChecker(RESET);
}
    
static Void local initTCKind(c)		/* build initial kind/arity for	c  */
Cell c; {
    if (isTycon(c)) {			/* Initial kind of tycon is:	   */
	Int beta = newKindvars(1);	/*	 v1 => ... => vn => vn	   */
	varKind(tycon(c).arity);	/* where n is the arity of c.	   */
	bindTv(beta,typeIs,typeOff);	/* For data definitions, vn == *   */
	if (tycon(c).what == DATATYPE)
	    bindTv(typeOff+tycon(c).arity,STAR,0);
	tycon(c).kind = mkInt(beta);
    }
    else {
	Int n	 = class(c).arity;
	Int beta = newKindvars(n);
	class(c).sig = NIL;
	do {
	    n--;
	    class(c).sig = pair(mkInt(beta+n),class(c).sig);
	} while (n>0);
    }
}

static Void local kindTC(c)		/* check each part of a tycon/class*/
Cell c; {				/* is well-kinded		   */
    if (isTycon(c)) {
	static String data = "datatype definition";
	static String tsyn = "synonym definition";
	Int line = tycon(c).line;

	locCVars = tyvar(intOf(tycon(c).kind))->offs;
	if (tycon(c).what == DATATYPE) {/* check conponents of constr fns  */
	    List cs = tycon(c).defn;
	    if (whatIs(cs)==QUAL) {
		map1Proc(kindPred,line,fst(snd(cs)));
		cs = snd(snd(cs));
	    }
	    for (; nonNull(cs); cs=tl(cs)) {
		Constr cn = hd(cs);
		for (; isAp(cn); cn=fun(cn)) {
		    checkKind(line,arg(cn),NIL,data,STAR,0);
		}
	    }
	}
	else {				/* check synonym expansion	   */
	    checkKind(line,tycon(c).defn,NIL,tsyn,var,locCVars+tycon(c).arity);
	}
    }
    else {				/* scan type exprs in class defn to*/
	List ms = class(c).members;	/* determine the class signature   */

	locCVars = newKindvars(class(c).arity);
	kindPred(class(c).line,class(c).head);
	map1Proc(kindPred,class(c).line,class(c).supers);
	for (; nonNull(ms); ms=tl(ms)) {
	    Int  line = intOf(fst3(hd(ms)));
	    Type type = thd3(hd(ms));
	    kindType(line,"member function type signature",type);
	}
    }
}

static Void local genTC(c)		/* generalise kind inferred for	   */
Cell c; {				/* given tycon/class		   */
    if (isTycon(c)) {
	tycon(c).kind = copyKindvar(intOf(tycon(c).kind));
#ifdef DEBUG_KINDS
	printf("%s :: ",textToStr(tycon(c).text));
	printKind(stdout,tycon(c).kind);
	putchar('\n');
#endif
    }
    else {
	Cell sig = class(c).sig;
	for (; nonNull(sig); sig=tl(sig))
	    hd(sig) = copyKindvar(intOf(hd(sig)));
#ifdef DEBUG_KINDS
	printf("%s :: ",textToStr(class(c).text));
	printSig(stdout,class(c).sig);
	putchar('\n');
#endif
    }
}

static Kind local copyKindvar(vn)      	/* build kind attatched to variable*/
Int vn; {
    Tyvar *tyv = tyvar(vn);
    if (tyv->bound)
	return copyKind(tyv->bound,tyv->offs);
    return STAR;			/* any unbound variable defaults to*/
}					/* the kind of all types	   */

static Kind local copyKind(k,o)        	/* build kind expression from	   */
Kind k; 			       	/* given skeleton		   */
Int  o; {
    switch (whatIs(k)) {
	case AP      : {   Kind l = copyKind(fst(k),o);  /* ensure correct */
			   Kind r = copyKind(snd(k),o);  /* eval. order    */
			   return ap(l,r);
		       }
	case OFFSET  : return copyKindvar(o+offsetOf(k));
	case INTCELL : return copyKindvar(intOf(k));
    }
    return k;
}

/* --------------------------------------------------------------------------
 * Kind checking of instance declaration headers:
 * ------------------------------------------------------------------------*/

Void kindInst(in,freedom)		/* check predicates in instance    */
Inst in;
Int  freedom; {
    typeChecker(RESET);
    locCVars = newKindvars(freedom);
    kindPred(inst(in).line,inst(in).head);
    map1Proc(kindPred,inst(in).line,inst(in).specifics);
    inst(in).sig = NIL;
    while (0<freedom--)
        inst(in).sig = ap(copyKindvar(locCVars+freedom),inst(in).sig);
#ifdef DEBUG_KINDS
    printf("instance ");
    printPred(stdout,inst(in).head);
    printf(" :: ");
    printSig(stdout,inst(in).sig);
    putchar('\n');
#endif
    typeChecker(RESET);
}

/* --------------------------------------------------------------------------
 * Kind checking of individual type signatures:
 * ------------------------------------------------------------------------*/

Void kindSigType(line,type)		/* check that type is well-kinded  */
Int  line;
Type type; {
    typeChecker(RESET);
    kindType(line,"type expression",type);
    fixKinds();
    typeChecker(RESET);
}

/* --------------------------------------------------------------------------
 * Support for `kind preserving substitutions' from unification:
 * ------------------------------------------------------------------------*/

static Bool local eqKind(k1,k2)		/* check that two (mono)kinds are  */
Kind k1, k2; {				/* equal			   */
    return k1==k2
	   || (isPair(k1) && isPair(k2)
	      && eqKind(fst(k1),fst(k2))
	      && eqKind(snd(k1),snd(k2)));
}

static Kind local getKind(c,o)		/* Find kind of constr during type */
Cell c;					/* checking process		   */
Int  o; {
    if (isAp(c))					/* application	   */
	return snd(getKind(fst(c),o));
    switch (whatIs(c)) {
	case LIST   : return simpleKind(1);		/*[_]::* -> *	   */
	case UNIT   : return STAR;			/*() ::*	   */
	case TUPLE  : return simpleKind(tupleOf(c));	/*(,)::* -> * -> * */
	case ARROW  : return simpleKind(2);		/*-> ::* -> * -> * */
	case OFFSET : return tyvar(o+offsetOf(c))->kind;
	case INTCELL: return tyvar(intOf(c))->kind;
	case TYCON  : return tycon(c).kind;
    }
#ifdef DEBUG_KINDS
    printf("getKind c = %d, whatIs=%d\n",c,whatIs(c));
#endif
    internal("getKind");
    return STAR;/* not reached */
}

/* --------------------------------------------------------------------------
 * Two forms of kind expression are used quite frequently:
 *	*  => *  => ... => *  => *	for kinds of ->, [], ->, (,) etc...
 *	v1 => v2 => ... => vn => vn+1	skeletons for constructor kinds
 * Expressions of these forms are produced by the following functions which
 * use a cache to avoid repeated construction of commonly used values.
 * A similar approach is used to store the types of tuple constructors in the
 * main type checker.
 * ------------------------------------------------------------------------*/

#define MAXKINDFUN 10
static  Kind simpleKindCache[MAXKINDFUN];
static  Kind varKindCache[MAXKINDFUN];

static Kind local makeSimpleKind(n)	/* construct * => ... => * (n args)*/
Int n; {
    Kind k = STAR;
    while (n-- > 0)
	k = ap(STAR,k);
    return k;
}

static Kind local simpleKind(n)		/* return (possibly cached) simple */
Int n; {				/* function kind		   */
    if (n>=MAXKINDFUN)
	return makeSimpleKind(n);
    else if (nonNull(simpleKindCache[n]))
	return simpleKindCache[n];
    else if (n==0)
	return simpleKindCache[0] = STAR;
    else
	return simpleKindCache[n] = ap(STAR,simpleKind(n-1));
}

static Kind local makeVarKind(n)	/* construct v0 => .. => vn	   */
Int n; {
    Kind k = mkOffset(n);
    while (n-- > 0)
	k = ap(mkOffset(n),k);
    return k;
}

static Void local varKind(n)		/* return (possibly cached) var	   */
Int n; {				/* function kind		   */
    typeOff = newKindvars(n+1);
    if (n>=MAXKINDFUN)
	typeIs = makeVarKind(n);
    else if (nonNull(varKindCache[n]))
	typeIs = varKindCache[n];
    else
	typeIs = varKindCache[n] = makeVarKind(n);
}

/*-------------------------------------------------------------------------*/
