/* --------------------------------------------------------------------------
 * subst.c:     Copyright (c) Mark P Jones 1991-1994.   All rights reserved.
 *              See goferite.h for details and conditions of use etc...
 *              Gofer version 2.30 March 1994
 *
 * Part of type checker dealing with operations on current substitution.
 * ------------------------------------------------------------------------*/

static Void local emptySubstitution() {	/* clear current substitution	   */
    numTyvars   = 0;
#if !FIXED_SUBST
    if (maxTyvars!=NUM_TYVARS) {
	maxTyvars = 0;
	if (tyvars) {
	    free(tyvars);
	    tyvars = 0;
	}
    }
#endif
    nextGeneric = 0;
    genericVars = NIL;
    typeIs      = NIL;
    predsAre    = NIL;
}

static Void local expandSubst(n)	/* add further n type variables to */
Int n; {				/* current substituion		   */
#if FIXED_SUBST
    if (numTyvars+n>NUM_TYVARS) {
	ERROR(0) "Too many type variables in type checker"
	EEND;
    }
#else
    if (numTyvars+n>maxTyvars) {	/* need to expand substitution	   */
        Int   newMax = maxTyvars+NUM_TYVARS;
	Tyvar *newTvs;
	Int   i;

	if (numTyvars+n>newMax) {	/* safety precaution		   */
	    ERROR(0) "Substitution expanding too quickly"
	    EEND;
	}

	/* It would be better to realloc() here, but that isn't portable
	 * enough for calloc()ed arrays.  The following code could cause
	 * a space leak if an interrupt occurs while we're copying the
	 * array ... we won't worry about this for the time being because
	 * we don't expect to have to go through this process much (if at
	 * all) in normal use of the type checker.
	 */

	newTvs = (Tyvar *)calloc(newMax,sizeof(Tyvar));
	if (!newTvs) {
	    ERROR(0) "Too many variables (%d) in type checker", newMax
	    EEND;
	}
	for (i=0; i<numTyvars;++i) {		/* copy substitution	   */
	    newTvs[i].offs  = tyvars[i].offs;
	    newTvs[i].bound = tyvars[i].bound;
	    newTvs[i].kind  = tyvars[i].kind;
	}
	maxTyvars = 0;				/* protection from SIGINT? */
	if (tyvars) free(tyvars);
	tyvars    = newTvs;
	maxTyvars = newMax;
    }
#endif
}

static Int local newTyvars(n)	        /* allocate new type variables	   */
Int n; {				/* all of kind STAR		   */
    Int beta = numTyvars;

    expandSubst(n);
    for (numTyvars+=n; n>0; n--) {
	tyvars[numTyvars-n].offs  = UNUSED_GENERIC;
	tyvars[numTyvars-n].bound = NIL;
	tyvars[numTyvars-n].kind  = STAR;
#ifdef DEBUG_TYPES
	printf("new type variable: _%d ::: ",numTyvars-n);
	printKind(stdout,tyvars[numTyvars-n].kind);
	putchar('\n');
#endif
    }
    return beta;
}

static Int local newKindedVars(k)	/* allocate new variables with	   */
Kind k; {				/* specified kinds		   */
    Int beta = numTyvars;		/* if k = k0 -> k1 -> ... -> kn	   */
    for (; isAp(k); k=snd(k)) {		/* then allocate n vars with kinds */
	expandSubst(1);			/* k0, k1, ..., k(n-1)		   */
	tyvars[numTyvars].offs  = UNUSED_GENERIC;
	tyvars[numTyvars].bound = NIL;
	tyvars[numTyvars].kind  = fst(k);
#ifdef DEBUG_TYPES
	printf("new type variable: _%d ::: ",numTyvars);
	printKind(stdout,tyvars[numTyvars].kind);
	putchar('\n');
#endif
	numTyvars++;
    }
    return beta;
}

#define freeTypeVars(beta) numTyvars=beta

#define deRef(tyv,t,o)  while ((tyv=getTypeVar(t,o)) && tyv->bound) { \
                            t = tyv->bound;                           \
                            o = tyv->offs;                            \
                        }

static Tyvar *local getTypeVar(t,o)	/* get number of type variable	   */
Type t; 				/* represented by (t,o) [if any].  */
Int  o; {
    switch (whatIs(t)) {
	case INTCELL : return tyvar(intOf(t));
	case OFFSET  : return tyvar(o+offsetOf(t));
    }
    return ((Tyvar *)0);
}

static Void local tyvarType(vn)       	/* load type held in type variable */
Int vn; {			       	/* vn into (typeIs,typeOff)	   */
    Tyvar *tyv;

    while ((tyv=tyvar(vn))->bound)
	switch(whatIs(tyv->bound)) {
	    case INTCELL : vn = intOf(tyv->bound);
			   break;

	    case OFFSET  : vn = offsetOf(tyv->bound)+(tyv->offs);
			   break;

	    default	 : typeIs  = tyv->bound;
			   typeOff = tyv->offs;
			   return;
	}
    typeIs  = var;
    typeOff = vn;
}

static Void local bindTv(vn,t,o)       	/* set type variable vn to (t,o)   */
Int  vn;
Type t;
Int  o; {
    Tyvar *tyv = tyvar(vn);
    tyv->bound = t;
    tyv->offs  = o;
#ifdef DEBUG_TYPES
    printf("binding type variable: _%d to ",vn);
    printType(stdout,debugType(t,o));
    putchar('\n');
#endif
}

static Void local expandSyn(h,ar,at,ao)	/* Expand type synonym with:	   */
Tycon h;				/* head h			   */
Int   ar;				/* ar args (NB. ar>=tycon(h).arity)*/
Type  *at;				/* original expression (*at,*ao)   */
Int   *ao; {				/* expansion returned in (*at,*ao) */
    ar -= tycon(h).arity;		/* calculate surplus arguments	   */
    if (ar==0)
	expandSyn1(h,at,ao);
    else {				/* if there are more args than the */
	Type t    = *at;		/* arity, we have to do a little   */
	Int  o    = *ao;		/* bit of work to isolate args that*/
	Type args = NIL;		/* will not be changed by expansion*/
	Int  i    = tycon(h).arity;
	Kind k    = tycon(h).kind;
	while (i-- > 0)			/* find kind of expanded part	   */
	    k = snd(k);
	while (ar-- > 0) {		/* find part to expand, and the	   */
	    Tyvar *tyv;			/* unused arguments		   */
	    args = cons(arg(t),args);
	    t    = fun(t);
	    deRef(tyv,t,o);
	}
	expandSyn1(h,&t,&o);		/* do the expansion		   */
	bindTv((i=newTyvars(1)),t,o);	/* and embed the results back in   */
	tyvar(i)->kind = getKind(t,o);	/* (*at, *ao) as required	   */
	*at = applyToArgs(mkInt(i),args);
    }
}

static Void local expandSyn1(h,at,ao)	/* Expand type synonym with:	   */
Tycon h;				/* head h, tycon(h).arity args,	   */
Type  *at;				/* original expression (*at,*ao)   */
Int   *ao; {				/* expansion returned in (*at,*ao) */
    Int   n = tycon(h).arity;
    Type  t = *at;
    Int   o = *ao;
    Tyvar *tyv;

    *at = tycon(h).defn;
    *ao = newKindedVars(tycon(h).kind);
    for (; 0<n--; t=fun(t)) {
	deRef(tyv,t,o);
	if (tyv || !isAp(t))
	    internal("expandSyn1");
	bindTv(*ao+n,arg(t),o);
    }
}

static Cell local getDerefHead(t,o)	/* get value at head of type exp.  */
Type t;
Int  o; {
    Tyvar *tyv;
    argCount = 0;
    for (;;) {
	while (isAp(t)) {
	    argCount++;
	    t = fun(t);
	}
	tyv = getTypeVar(t,o);
	if (tyv && tyv->bound) {
	    t = tyv->bound;
	    o = tyv->offs;
	}
	else
	    break;
    }
    return t;
}

/* --------------------------------------------------------------------------
 * Mark type expression, so that all variables are marked as unused generics
 * ------------------------------------------------------------------------*/

static Void local clearMarks() {       	/* set all unbound type vars to	   */
    Int i;			       	/* unused generic variables	   */
    for (i=0; i<numTyvars; ++i)
	if (isNull(tyvars[i].bound))
	    tyvars[i].offs = UNUSED_GENERIC;
    nextGeneric = 0;
    genericVars = NIL;
}

static Void local resetGenericsFrom(n)	/* reset all generic vars to unused*/
Int n; {				/* for generics >= n		   */
    Int i;

    if (n==0)				/* reset generic variables list	   */
	genericVars = NIL;		/* most common case: reset to zero */
    else
	for (i=length(genericVars); i>n; i--)
	    genericVars = tl(genericVars);

    for (i=0; i<numTyvars; ++i)
	if (isNull(tyvars[i].bound) && tyvars[i].offs>=GENERIC+n)
	    tyvars[i].offs = UNUSED_GENERIC;
    nextGeneric = n;
}

static Void local markTyvar(vn)        	/* mark fixed vars in type bound to*/
Int vn; {			       	/* given type variable		   */
    Tyvar *tyv = tyvar(vn);

    if (tyv->bound)
	markType(tyv->bound, tyv->offs);
    else
	(tyv->offs) = FIXED_TYVAR;
}

static Void local markType(t,o)        	/* mark fixed vars in type (t,o)   */
Type t;
Int  o; {
    switch (whatIs(t)) {
	case TYCON   :
	case TUPLE   :
	case UNIT    :
	case ARROW   :
	case LIST    : return;

	case AP      : markType(fst(t),o);
		       markType(snd(t),o);
		       return;

	case OFFSET  : markTyvar(o+offsetOf(t));
		       return;

	case INTCELL : markTyvar(intOf(t));
		       return;

	default      : internal("markType");
    }
}

/* --------------------------------------------------------------------------
 * Copy type expression from substitution to make a single type expression:
 * ------------------------------------------------------------------------*/

static Type local copyTyvar(vn)        	/* calculate most general form of  */
Int vn; {			       	/* type bound to given type var	   */
    Tyvar *tyv = tyvar(vn);

    if (tyv->bound)
	return copyType(tyv->bound,tyv->offs);

    switch (tyv->offs) {
	case FIXED_TYVAR    : return mkInt(vn);

	case UNUSED_GENERIC : (tyv->offs) = GENERIC + nextGeneric++;
			      if (nextGeneric>=NUM_OFFSETS) {
				  ERROR(0)
				      "Too many polymorphic type variables"
				  EEND;
			      }
			      genericVars = cons(mkInt(vn),genericVars);

	default 	    : return mkOffset(tyv->offs - GENERIC);
    }
}

static Type local copyType(t,o)        	/* calculate most general form of  */
Type t; 			       	/* type expression (t,o) 	   */
Int  o; {
    switch (whatIs(t)) {
	case AP      : {   Type l = copyType(fst(t),o);  /* ensure correct */
			   Type r = copyType(snd(t),o);  /* eval. order    */
			   return ap(l,r);
		       }
	case OFFSET  : return copyTyvar(o+offsetOf(t));
	case INTCELL : return copyTyvar(intOf(t));
    }

    return t;
}

#ifdef DEBUG_TYPES
static Type local debugTyvar(vn)	/* expand type structure in full   */
Int vn; {				/* detail			   */
    Tyvar *tyv = tyvar(vn);

    if (tyv->bound)
	return debugType(tyv->bound,tyv->offs);
    return mkInt(vn);
}

static Type local debugType(t,o)
Type t;
Int  o; {
    switch (whatIs(t)) {
	case AP      : {   Type l = debugType(fst(t),o);
			   Type r = debugType(snd(t),o);
			   return ap(l,r);
		       }
	case OFFSET  : return debugTyvar(o+offsetOf(t));
	case INTCELL : return debugTyvar(intOf(t));
    }

    return t;
}
#endif /*DEBUG_TYPES*/

/* --------------------------------------------------------------------------
 * Occurs check:
 * ------------------------------------------------------------------------*/

static Tyvar *lookingFor;	       	/* var to look for in occurs check */

static Bool local doesntOccurIn(t,o)   	/* Return TRUE if var lookingFor   */
Type t; 			       	/* isn't referenced in (t,o)	   */
Int o; {
    Tyvar *tyv;

    for (;;) {
	deRef(tyv,t,o);
	if (tyv)			/* type variable		   */
	    return tyv!=lookingFor;
        else if (isAp(t)) {		/* application			   */
	    if (doesntOccurIn(snd(t),o))
		t = fst(t);
	    else
		return FALSE;
	}
	else                            /* no variable found		   */
	    break;
    }
    return TRUE;
}

/* --------------------------------------------------------------------------
 * Unification algorithm:
 * ------------------------------------------------------------------------*/

static char *unifyFails = 0;		/* unification error message	   */
static Bool matchMode	= FALSE;	/* set to TRUE to prevent binding  */
				       	/* during matching process	   */

static Bool local varToVarBind(tyv1,tyv2)/* Make binding tyv1 := tyv2	   */
Tyvar *tyv1, *tyv2; {
    if (tyv1!=tyv2)
	if (matchMode)
	    return FALSE;
	else {
	    if (!eqKind(tyv1->kind,tyv2->kind)) {
		unifyFails = "constructor variable kinds do not match";
		return FALSE;
	    }
	    tyv1->bound = var;
	    tyv1->offs	= tyvNum(tyv2);
#ifdef DEBUG_TYPES
	    printf("vv binding tyvar: _%d to _%d\n",tyvNum(tyv1),tyvNum(tyv2));
#endif
	}
    return TRUE;
}

static Bool local varToTypeBind(tyv,t,o)/* Make binding tyv := (t,o)	   */
Tyvar *tyv;
Type  t;				/* guaranteed not to be a v'ble or */
Int   o; {				/* have synonym as outermost constr*/
    if (!matchMode) {
	lookingFor = tyv;
	if (doesntOccurIn(t,o)) {
	    if (!eqKind(tyv->kind,getKind(t,o))) {
		unifyFails = "constructor variable kinds do not match";
		return FALSE;
	    }
	    tyv->bound = t;
	    tyv->offs  = o;
#ifdef DEBUG_TYPES
	    printf("vt binding type variable: _%d to ",tyvNum(tyv));
	    printType(stdout,debugType(t,o));
	    putchar('\n');
#endif
	    return TRUE;
	}
    }
    unifyFails = "unification would give infinite type";
    return FALSE;	/* INFINITE TYPE (or failed match in matchMode)    */
}

static Bool local kvarToVarBind(tyv1,tyv2)/* Make binding tyv1 := tyv2	   */
Tyvar *tyv1, *tyv2; {			  /* for kind variable bindings	   */
    if (tyv1!=tyv2) {
	tyv1->bound = var;
	tyv1->offs  = tyvNum(tyv2);
    }
    return TRUE;
}

static Bool local kvarToTypeBind(tyv,t,o)/* Make binding tyv := (t,o)	   */
Tyvar *tyv;				/* for kind variable bindings	   */
Type  t;				/* guaranteed not to be a v'ble or */
Int   o; {				/* have synonym as outermost constr*/
    lookingFor = tyv;
    if (doesntOccurIn(t,o)) {
	tyv->bound = t;
	tyv->offs  = o;
	return TRUE;
    }
    unifyFails = "unification would give infinite kind";
    return FALSE;
}

static Bool local unify(t1,o1,t2,o2)	/* Main unification routine	   */
Type t1,t2;				/* unify (t1,o1) with (t2,o2)	   */
Int  o1,o2; {
    Tyvar *tyv1, *tyv2;

    deRef(tyv1,t1,o1);
    deRef(tyv2,t2,o2);

un: if (tyv1)
        if (tyv2)
	    return varToVarBind(tyv1,tyv2);	    /* t1, t2 variables    */
        else {
	    Cell h2 = getDerefHead(t2,o2);	    /* t1 variable, t2 not */
	    if (isSynonym(h2) && argCount>=tycon(h2).arity) {
		expandSyn(h2,argCount,&t2,&o2);
		deRef(tyv2,t2,o2);
		goto un;
	    }
	    return varToTypeBind(tyv1,t2,o2);
        }
    else
        if (tyv2) {
	    Cell h1 = getDerefHead(t1,o1);	    /* t2 variable, t1 not */
	    if (isSynonym(h1) && argCount>=tycon(h1).arity) {
		expandSyn(h1,argCount,&t1,&o1);
		deRef(tyv1,t1,o1);
		goto un;
	    }
	    return varToTypeBind(tyv2,t1,o1);
        }
	else {					    /* t1, t2 not vars	   */
	    Type h1 = getDerefHead(t1,o1);
	    Int  a1 = argCount;
	    Type h2 = getDerefHead(t2,o2);
	    Int  a2 = argCount;

#ifdef DEBUG_TYPES
	    printf("tt unifying types: ");
	    printType(stdout,debugType(t1,o1));
	    printf(" with ");
	    printType(stdout,debugType(t2,o2));
	    putchar('\n');
#endif

	    if (isOffset(h1) || isInt(h1)) h1=NIL;  /* represent var by NIL*/
	    if (isOffset(h2) || isInt(h2)) h2=NIL;

	    if (nonNull(h1) && h1==h2) {/* Assuming well-formed types, both*/
		if (a1!=a2) {		/* t1, t2 must have same no of args*/
		    unifyFails = "incompatible constructors";
		    return FALSE;
		}
		while (isAp(t1)) {
		    if (!unify(arg(t1),o1,arg(t2),o2))
			return FALSE;
		    t1 = fun(t1);
		    deRef(tyv1,t1,o1);
		    t2 = fun(t2);
		    deRef(tyv2,t2,o2);
		}
		unifyFails = 0;
		return TRUE;
	    }

	    /* Types do not match -- look for type synonyms to expand */

	    if (isSynonym(h1) && a1>=tycon(h1).arity) {
		expandSyn(h1,a1,&t1,&o1);
		deRef(tyv1,t1,o1);
		goto un;
	    }
	    if (isSynonym(h2) && a2>=tycon(h2).arity) {
		expandSyn(h2,a2,&t2,&o2);
                deRef(tyv2,t2,o2);
		goto un;
	    }

	    if ((isNull(h1) && a1<=a2) ||       /* last attempt -- maybe   */
		(isNull(h2) && a2<=a1))	{	/* one head is a variable? */
		for (;;) {
		    deRef(tyv1,t1,o1);
		    deRef(tyv2,t2,o2);

		    if (tyv1)				/* unify heads!	   */
			if (tyv2)
			    return varToVarBind(tyv1,tyv2);
			else
			    return varToTypeBind(tyv1,t2,o2);
		    else if (tyv2)
			return varToTypeBind(tyv2,t1,o1);

		    /* at this point, neither t1 nor t2 is a variable. In  */
		    /* addition, they must both be APs unless one of the   */
		    /* head variables has been bound during unification of */
		    /* the arguments.					   */

		    if (!isAp(t1) || !isAp(t2)) {	/* might not be APs*/
			unifyFails = 0;
			return t1==t2;
		    }
		    if (!unify(arg(t1),o1,arg(t2),o2))	/* o/w must be APs */
			return FALSE;
		    t1 = fun(t1);
		    t2 = fun(t2);
		}
	    }
	}
    unifyFails = 0;
    return FALSE;
}

static Bool local sameType(t1,o1,t2,o2)/* Compare types without binding    */
Type t1,t2;
Int  o1,o2; {
    Bool result;
    matchMode = TRUE;
    result    = unify(t1,o1,t2,o2);
    matchMode = FALSE;
    return result;
}

Bool typeMatches(type,mt)		/* test if type matches monotype mt*/
Type type, mt; {
    Bool result;
    if (isPolyType(type) || whatIs(type)==QUAL)
	return FALSE;
    typeChecker(RESET);
    matchMode = TRUE;
    result    = unify(mt,0,type,0);
    matchMode = FALSE;
    typeChecker(RESET);
    return result;
}

/* --------------------------------------------------------------------------
 * Unify kind expressions:
 * ------------------------------------------------------------------------*/

static Bool local kunify(k1,o1,k2,o2)	/* Unify kind expr (k1,o1) with	   */
Kind k1,k2;				/* (k2,o2)			   */
Int  o1,o2; {
    Tyvar *kyv1, *kyv2;

    deRef(kyv1,k1,o1);
    deRef(kyv2,k2,o2);

    if (kyv1)
        if (kyv2)
	    return kvarToVarBind(kyv1,kyv2);	    /* k1, k2 variables    */
        else
	    return kvarToTypeBind(kyv1,k2,o2);	    /* k1 variable, k2 not */
    else
        if (kyv2)
	    return kvarToTypeBind(kyv2,k1,o1);	    /* k2 variable, k1 not */
	else
	    if (k1==STAR && k2==STAR)		    /* k1, k2 not vars	   */
		return TRUE;
	    else if (isAp(k1) && isAp(k2))
		return kunify(fst(k1),o1,fst(k2),o2) &&
		       kunify(snd(k1),o1,snd(k2),o2);
    unifyFails = 0;
    return FALSE;
}

/*-------------------------------------------------------------------------*/
