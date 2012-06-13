/* --------------------------------------------------------------------------
 * prims.c:     Copyright (c) Mark P Jones 1991-1994.   All rights reserved.
 *              See goferite.h for details and conditions of use etc...
 *              Gofer version 2.30 March 1994
 *
 * Primitive functions, input output etc...
 * if PRIMITIVES_CODE == 0 then the code for PRIMITIVES is excluded: only
 * the primitives table and consChar() parts are retained.
 * ------------------------------------------------------------------------*/

#if PRIMITIVES_CODE
#include <ctype.h>
#if (TURBOC | BCC)
#include <io.h>
#endif
#endif

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

#if PRIMITIVES_CODE
#define PROTO_PRIM(name)	static Void name Args((StackPtr))
#define primFun(name)		static Void name(root) StackPtr root;
#define primArg(n)		stack(root+n)

/* IMPORTANT: the second element of an update must be written first.
 * this is to deal with the case where an INDIRECT tag is written into
 * a Cell before the second value has been set.  If a garbage collection
 * occurs before the second element was set then the INDIRECTion will be
 * (wrongly) elided and result in chaos.  I know.  It happened to me.
 */

#define update(l,r)		((snd(stack(root))=r),(fst(stack(root))=l))
#define updateRoot(c)		update(INDIRECT,c)
#define updapRoot(l,r)		update(l,r)
#define cantReduce()		evalFails(root)

PROTO_PRIM(primFatbar);
PROTO_PRIM(primFail);
PROTO_PRIM(primSel);
PROTO_PRIM(primIf);
PROTO_PRIM(primStrict);
PROTO_PRIM(primTrace);

#if    HASKELL_ARRAYS
static Int  local getSize	Args((Cell, Cell));
static List local addAssocs	Args((Cell, Int, Cell, List));
static List local foldAssocs	Args((Cell, Int, Cell, Cell, List));

PROTO_PRIM(primArray);
PROTO_PRIM(primUpdate);
PROTO_PRIM(primAccum);
PROTO_PRIM(primAccumArray);
PROTO_PRIM(primAmap);
PROTO_PRIM(primSubscript);
PROTO_PRIM(primBounds);
PROTO_PRIM(primElems);
#endif

PROTO_PRIM(primPlusInt);
PROTO_PRIM(primMinusInt);
PROTO_PRIM(primMulInt);
PROTO_PRIM(primDivInt);
PROTO_PRIM(primQuotInt);
PROTO_PRIM(primModInt);
PROTO_PRIM(primRemInt);
PROTO_PRIM(primNegInt);

PROTO_PRIM(primCharToInt);
PROTO_PRIM(primIntToChar);
PROTO_PRIM(primIntToFloat);

PROTO_PRIM(primPlusFloat);
PROTO_PRIM(primMinusFloat);
PROTO_PRIM(primMulFloat);
PROTO_PRIM(primDivFloat);
PROTO_PRIM(primNegFloat);

#if HAS_FLOATS
PROTO_PRIM(primSinFloat);
PROTO_PRIM(primCosFloat);
PROTO_PRIM(primTanFloat);
PROTO_PRIM(primAsinFloat);
PROTO_PRIM(primAcosFloat);
PROTO_PRIM(primAtanFloat);
PROTO_PRIM(primAtan2Float);
PROTO_PRIM(primExpFloat);
PROTO_PRIM(primLogFloat);
PROTO_PRIM(primLog10Float);
PROTO_PRIM(primSqrtFloat);
PROTO_PRIM(primFloatToInt);
#endif

PROTO_PRIM(primEqInt);
PROTO_PRIM(primLeInt);

PROTO_PRIM(primEqChar);
PROTO_PRIM(primLeChar);

PROTO_PRIM(primEqFloat);
PROTO_PRIM(primLeFloat);

PROTO_PRIM(primCmp);
PROTO_PRIM(primGenericEq);
PROTO_PRIM(primGenericLe);
PROTO_PRIM(primGenericLt);
PROTO_PRIM(primGenericGe);
PROTO_PRIM(primGenericGt);
PROTO_PRIM(primGenericNe);

PROTO_PRIM(primPrint);
PROTO_PRIM(primNPrint);

static Void   local printer		Args((StackPtr,Name,Int,Cell));
static Void   local startList		Args((StackPtr,Cell));
static Void   local startNList		Args((StackPtr,Cell));

PROTO_PRIM(primLPrint);
PROTO_PRIM(primNLPrint);
PROTO_PRIM(primSPrint);
PROTO_PRIM(primNSPrint);

static Cell   local textAsVar		Args((Text,Cell));
static Cell   local textAsOp		Args((Text,Cell));
static Cell   local stringOutput	Args((String,Cell));
static Cell   local printBadRedex	Args((Cell,Cell));
static Cell   local printDBadRedex	Args((Cell,Cell));

#if (IO_DIALOGUE | LAMBDAVAR | LAMBDANU)
static String local evalName		Args((Cell));
#endif
#if IO_DIALOGUE
static Void   local abandonDialogue	Args((Cell));
static Cell   local readFile		Args((Void));
static Cell   local writeFile		Args((Void));
static Cell   local appendFile		Args((Void));
static Cell   local readChan		Args((Void));
static Cell   local appendChan		Args((Void));
static FILE  *local validOutChannel	Args((String));
static Cell   local echo		Args((Void));
static Cell   local getCLArgs		Args((Void));
static Cell   local getProgName		Args((Void));
static Cell   local getEnv		Args((Void));
static Cell   local outputDString	Args((FILE *));

PROTO_PRIM(primInput);
PROTO_PRIM(primFopen);
#endif

#if IO_MONAD
PROTO_PRIM(primSTRun);
PROTO_PRIM(primFst);
PROTO_PRIM(primSnd);
PROTO_PRIM(primSTReturn);
PROTO_PRIM(primIOBind);
PROTO_PRIM(primSTBind);
PROTO_PRIM(primSTInter);
PROTO_PRIM(primSTNew);
PROTO_PRIM(primSTAssign);
PROTO_PRIM(primSTDeref);
PROTO_PRIM(primSTMutVarEq);
PROTO_PRIM(primIOGetch);
PROTO_PRIM(primIOPutchar);
#if HASKELL_ARRAYS
PROTO_PRIM(primSTNewArr);
PROTO_PRIM(primSTReadArr);
PROTO_PRIM(primSTWriteArr);
PROTO_PRIM(primSTFreeze);
#endif
#endif

#ifdef LAMBDAVAR
PROTO_PRIM(primLvReturn);
PROTO_PRIM(primLvPure);
PROTO_PRIM(primLvRead);
PROTO_PRIM(primLvBind);
PROTO_PRIM(primLvVar);
PROTO_PRIM(primLvNewvar);
PROTO_PRIM(primLvAssign);
PROTO_PRIM(primLvVarEq);
PROTO_PRIM(primLvGetch);
PROTO_PRIM(primLvPutchar);
PROTO_PRIM(primLvSystem);
#endif

#ifdef LAMBDANU
PROTO_PRIM(primLnReturn);
PROTO_PRIM(primLnBind);
PROTO_PRIM(primLnFlip);
PROTO_PRIM(primLnNew);
PROTO_PRIM(primLnAssign);
PROTO_PRIM(primLnRead);
PROTO_PRIM(primLnIo);
PROTO_PRIM(primLnBegin);
PROTO_PRIM(primLnTagEq);
PROTO_PRIM(primLnGetch);
PROTO_PRIM(primLnPutchar);
PROTO_PRIM(primLnSystem);
PROTO_PRIM(primLnDone);
#endif

#endif

/* --------------------------------------------------------------------------
 * Table of primitive/built-in values:
 * ------------------------------------------------------------------------*/

#if PRIMITIVES_CODE
#define GofcPrim(imp)	imp
#define NoGofcPrim(imp)	imp
#else
#define GofcPrim(imp)	PRIM_GOFC
#define NoGofcPrim(imp)	PRIM_NOGOFC
#endif

struct primitive primitives[] = {
  {"primFatbar",	2, GofcPrim(primFatbar)},
  {"primFail",		0, GofcPrim(primFail)},
  {"primUndefMem",	1, GofcPrim(primFail)},
  {"primGCBhole",	0, NoGofcPrim(primFail)},
  {"primError",		1, GofcPrim(primFail)},
  {"primSel",		3, GofcPrim(primSel)},
  {"primIf",		3, GofcPrim(primIf)},
  {"primTrace",		2, NoGofcPrim(primTrace)},

#if    HASKELL_ARRAYS
  {"primArray",		3, GofcPrim(primArray)},
  {"primUpdate",	3, GofcPrim(primUpdate)},
  {"primAccum",		4, GofcPrim(primAccum)},
  {"primAccumArray",	5, GofcPrim(primAccumArray)},
  {"primAmap",		2, GofcPrim(primAmap)},
  {"primSubscript",	3, GofcPrim(primSubscript)},
  {"primBounds",	1, GofcPrim(primBounds)},
  {"primElems",		1, GofcPrim(primElems)},
  {"primEltUndef",	0, NoGofcPrim(primFail)},
  {"primOutBounds",	2, NoGofcPrim(primFail)},
#endif

  {"primCompare",	1, NoGofcPrim(primCmp)},
  {"primPrint",		3, NoGofcPrim(primPrint)},
  {"primNprint",	3, NoGofcPrim(primNPrint)},
  {"primLprint",	2, NoGofcPrim(primLPrint)},
  {"primNlprint",	2, NoGofcPrim(primNLPrint)},
  {"primSprint",	2, NoGofcPrim(primSPrint)},
  {"primNsprint",	2, NoGofcPrim(primNSPrint)},

  {"primPlusInt",	2, GofcPrim(primPlusInt)},
  {"primMinusInt",	2, GofcPrim(primMinusInt)},
  {"primMulInt",	2, GofcPrim(primMulInt)},
  {"primDivInt",	2, GofcPrim(primDivInt)},
  {"primQuotInt",	2, GofcPrim(primQuotInt)},
  {"primModInt",	2, GofcPrim(primModInt)},
  {"primRemInt",	2, GofcPrim(primRemInt)},
  {"primNegInt",	1, GofcPrim(primNegInt)},

  {"primPlusFloat",	2, GofcPrim(primPlusFloat)},
  {"primMinusFloat",	2, GofcPrim(primMinusFloat)},
  {"primMulFloat",	2, GofcPrim(primMulFloat)},
  {"primDivFloat",	2, GofcPrim(primDivFloat)},
  {"primNegFloat",	1, GofcPrim(primNegFloat)},

#if HAS_FLOATS
  {"primSinFloat",	1, GofcPrim(primSinFloat)},
  {"primCosFloat",	1, GofcPrim(primCosFloat)},
  {"primTanFloat",	1, GofcPrim(primTanFloat)},
  {"primAsinFloat",	1, GofcPrim(primAsinFloat)},
  {"primAcosFloat",	1, GofcPrim(primAcosFloat)},
  {"primAtanFloat",	1, GofcPrim(primAtanFloat)},
  {"primAtan2Float",	2, GofcPrim(primAtan2Float)},
  {"primExpFloat",	1, GofcPrim(primExpFloat)},
  {"primLogFloat",	1, GofcPrim(primLogFloat)},
  {"primLog10Float",	1, GofcPrim(primLog10Float)},
  {"primSqrtFloat",	1, GofcPrim(primSqrtFloat)},
  {"primFloatToInt",	1, GofcPrim(primFloatToInt)},
#endif

  {"primIntToChar",	1, GofcPrim(primIntToChar)},
  {"primCharToInt",	1, GofcPrim(primCharToInt)},
  {"primIntToFloat",	1, GofcPrim(primIntToFloat)},

  {"primEqInt",		2, GofcPrim(primEqInt)},
  {"primLeInt",		2, GofcPrim(primLeInt)},
  {"primEqChar",	2, GofcPrim(primEqChar)},
  {"primLeChar",	2, GofcPrim(primLeChar)},
  {"primEqFloat",	2, GofcPrim(primEqFloat)},
  {"primLeFloat",	2, GofcPrim(primLeFloat)},

  {"primGenericEq",	2, GofcPrim(primGenericEq)},
  {"primGenericNe",	2, GofcPrim(primGenericNe)},
  {"primGenericGt",	2, GofcPrim(primGenericGt)},
  {"primGenericLe",	2, GofcPrim(primGenericLe)},
  {"primGenericGe",	2, GofcPrim(primGenericGe)},
  {"primGenericLt",	2, GofcPrim(primGenericLt)},

  {"primShowsInt",	3, GofcPrim(primPrint)},
  {"primShowsFloat",	3, GofcPrim(primPrint)},

  {"primStrict",	2, GofcPrim(primStrict)},

#if IO_DIALOGUE
  {"primInput",		1, NoGofcPrim(primInput)},
  {"primFopen",		3, GofcPrim(primFopen)},
#endif

#if IO_MONAD
  {"primSTRun",		1, GofcPrim(primSTRun)},
  {"primFst",		1, NoGofcPrim(primFst)},
  {"primSnd",		1, NoGofcPrim(primSnd)},
  {"primSTReturn",	1, GofcPrim(primSTReturn)},
  {"primIOBind",	3, GofcPrim(primIOBind)},
  {"primSTBind",	3, GofcPrim(primSTBind)},
  {"primSTInter",	2, GofcPrim(primSTInter)},
  {"primSTNew",		2, GofcPrim(primSTNew)},
  {"primSTAssign",	3, GofcPrim(primSTAssign)},
  {"primSTDeref",	2, GofcPrim(primSTDeref)},
  {"primSTMutVarEq",	2, GofcPrim(primSTMutVarEq)},
  {"primIOGetch",	1, GofcPrim(primIOGetch)},
  {"primIOPutchar",	2, GofcPrim(primIOPutchar)},
#if HASKELL_ARRAYS
  {"primSTNewArr",	4, GofcPrim(primSTNewArr)},
  {"primSTReadArr",	4, GofcPrim(primSTReadArr)},
  {"primSTWriteArr",	5, GofcPrim(primSTWriteArr)},
  {"primSTFreeze",	2, GofcPrim(primSTFreeze)},
#endif
#endif

#ifdef LAMBDAVAR
  {"primLvReturn",	2, NoGofcPrim(primLvReturn)},
  {"primLvPure",	1, NoGofcPrim(primLvPure)},
  {"primLvRead",	3, NoGofcPrim(primLvRead)},
  {"primLvBind",	3, NoGofcPrim(primLvBind)},
  {"primLvVar",		2, NoGofcPrim(primLvVar)},
  {"primLvNewvar",	1, NoGofcPrim(primLvNewvar)},
  {"primLvAssign",	3, NoGofcPrim(primLvAssign)},
  {"primLvVarEq",	2, NoGofcPrim(primLvVarEq)},
  {"primLvUnbound",	0, NoGofcPrim(primFail)},
  {"primLvGetch",	1, NoGofcPrim(primLvGetch)},
  {"primLvPutchar",	2, NoGofcPrim(primLvPutchar)},
  {"primLvSystem",	2, NoGofcPrim(primLvSystem)},
#endif

#ifdef LAMBDANU
  {"primLnReturn",	2, NoGofcPrim(primLnReturn)},
  {"primLnBind",	3, NoGofcPrim(primLnBind)},
  {"primLnFlip",	3, NoGofcPrim(primLnFlip)},
  {"primLnNew",		1, NoGofcPrim(primLnNew)},
  {"primLnAssign",	3, NoGofcPrim(primLnAssign)},
  {"primLnRead",	3, NoGofcPrim(primLnRead)},
  {"primLnIo",		2, NoGofcPrim(primLnIo)},
  {"primLnBegin",	1, NoGofcPrim(primLnBegin)},
  {"primLnTagEq",	2, NoGofcPrim(primLnTagEq)},
  {"primLnGetch",	1, NoGofcPrim(primLnGetch)},
  {"primLnPutchar",	2, NoGofcPrim(primLnPutchar)},
  {"primLnSystem",	2, NoGofcPrim(primLnSystem)},
  {"primLnUnbound",	0, NoGofcPrim(primFail)},
  {"primLnNocont",	0, NoGofcPrim(primFail)},
  {"primLnDone",	1, NoGofcPrim(primLnDone)},
#endif

  {0,			0, 0}
};

/* --------------------------------------------------------------------------
 * Primitive functions:
 * ------------------------------------------------------------------------*/

#if PRIMITIVES_CODE
primFun(primFatbar) {			/* Fatbar primitive		   */
    Cell l    = primArg(2);		/* _FAIL [] r = r		   */
    Cell r    = primArg(1);		/* l     [] r = l  -- otherwise	   */
    Cell temp = evalWithNoError(l);
    if (nonNull(temp))
	if (temp==nameFail)
	    updateRoot(r);
	else {
	    updateRoot(temp);
	    cantReduce();
	}
    else
	updateRoot(l);
}

primFun(primFail) {		       /* Failure primitive		   */
    cantReduce();
}

primFun(primSel) {		       /* Component selection		   */
    Cell c = primArg(3);	       /* _sel c e n   return nth component*/
    Cell e = primArg(2);	       /*	       in expression e	   */
    Cell n = intOf(primArg(1));        /*	       built using cfun c  */

    eval(e);
    if (whnfHead==c &&	((isName(whnfHead) && name(whnfHead).arity==whnfArgs)
		      || (isTuple(whnfHead) && tupleOf(whnfHead)==whnfArgs)))
	updateRoot(pushed(n-1));
    else
	cantReduce();
}

primFun(primIf) {		       /* Conditional primitive 	   */
    eval(primArg(3));
    if (whnfHead==nameTrue)
	updateRoot(primArg(2));
    else
	updateRoot(primArg(1));
}

primFun(primStrict) {		       /* Strict application primitive	   */
    eval(primArg(1));		       /* evaluate 2nd argument 	   */
    updapRoot(primArg(2),primArg(1));  /* and apply 1st argument to result */
}

primFun(primTrace) {			/* an unsound trace primitive for  */
    fflush(stdout);			/* debugging purposes		   */
    eval(pop());			/*  :: String -> a -> a		   */
    while (whnfHead==nameCons) {
	eval(pop());
	putchar(charOf(whnfHead));
	eval(pop());
    }
    updateRoot(pop());
}

/* --------------------------------------------------------------------------
 * Array primitives:
 * ------------------------------------------------------------------------*/

#if    HASKELL_ARRAYS
static Int local getSize(bounds,range)
Cell bounds, range; {
    Int lo;
    eval(bounds);			/* get components of bounds pair   */
    eval(ap(range,pop()));		/* get lower bound as an integer   */
    lo = whnfInt;
    eval(ap(range,pop()));		/* get upper bound as an integer   */
    whnfInt -= lo;
    return (whnfInt<0 ? 0 : whnfInt+1);
}

static List local addAssocs(r,s,as,vs)	/* add assocs in as to array	   */
Cell r;					/* list vs, using r for the range  */
Int  s;					/* and s for array size		   */
Cell as;
List vs; {
    for (;;) {				/* loop through assocs		   */
	eval(as);
	if (whnfHead==nameNil && whnfArgs==0)
	    break;
	else if (whnfHead==nameCons && whnfArgs==2) {
	    eval(pop());
	    /* at this point, the top of the stack looks like:
	     *
	     *      pushed(0) == index  (first component in assoc)
	     *      pushed(1) == value for assoc
	     *	    pushed(2) == rest of assocs
	     */
	    eval(ap(r,top()));
	    if (whnfInt<0 || whnfInt>=s)
		return UNIT;
	    else {
		List us = NIL;
		drop();
		for (us=vs; whnfInt>0; --whnfInt)
		    us = tl(us);
		hd(us) = (isNull(hd(us)) ? top() : nameEltUndef);
		drop();
		as = pop();
	    }
	}
	else
	    internal("runtime type error");
    }
    return vs;
}

static List local foldAssocs(r,s,f,as,vs)
Cell r;					/* fold assocs as into array list  */
Int  s;					/* vs using function f, with r for */
Cell f;					/* range and s for size		   */
Cell as;				/* bounds.			   */
List vs; {
    for (;;) {				/* loop through assocs		   */
	eval(as);
	if (whnfHead==nameNil && whnfArgs==0)
	    break;
	else if (whnfHead==nameCons && whnfArgs==2) {
	    eval(pop());
	    /* at this point, the top of the stack looks like:
	     *
	     *      pushed(0) == index  (first component in assoc)
	     *      pushed(1) == value for assoc
	     *	    pushed(2) == rest of assocs
	     */
	    eval(ap(r,top()));
	    if (whnfInt<0 || whnfInt>s)
		return UNIT;
	    else {
		List us = NIL;
		drop();
		for (us=vs; whnfInt>0; --whnfInt)
		    us = tl(us);
		hd(us) = ap(ap(f,hd(us)),pop());
		as = pop();
	    }
	}
	else
	    internal("runtime type error");
    }
    return vs;
}

primFun(primArray) {			/* Array creation		   */
    Cell range  = primArg(3);		/*  :: (a -> Int) -> 		   */
    Cell bounds = primArg(2);		/*	(a,a) ->		   */
    Cell assocs = primArg(1);		/*	 [Assoc a b] -> Array a b  */
    List vs     = NIL;
    List us	= NIL;
    Int  size;

    size = getSize(bounds,range);			/* check bounds	   */
    vs   = copy(size,NIL);				/* initialize elems*/
    vs   = addAssocs(range,size,assocs,vs);		/* process assocs  */
    if (vs==UNIT) {
	updapRoot(ap(nameOutBounds,bounds),top());
	cantReduce();
    }
    for (us=vs; nonNull(us); us=tl(us))			/* set undef elts  */
	if (isNull(hd(us)))
	    hd(us) = nameEltUndef;

    updapRoot(ARRAY,ap(bounds,vs));
}

primFun(primUpdate) {			/* Array update			   */
    Cell range  = primArg(3);		/*  :: (a -> Int) ->		   */
    Cell oldArr = primArg(2);		/*	Array a b ->		   */
    Cell assocs = primArg(1);		/*	 [Assoc a b] -> Array a b  */
    Cell bounds = NIL;
    Cell elems  = NIL;
    List vs     = NIL;
    List us	= NIL;
    Int  size;

    eval(oldArr);					/* find bounds	   */
    bounds = fst(snd(whnfHead));
    elems  = snd(snd(whnfHead));
    size   = getSize(bounds,range);
    vs     = copy(size,NIL);				/* initialize elems*/
    vs     = addAssocs(range,size,assocs,vs);		/* process assocs  */
    if (vs==UNIT) {
        updapRoot(ap(nameOutBounds,bounds),top());
        cantReduce();
    }
    for (us=vs; nonNull(us) && nonNull(elems); us=tl(us), elems=tl(elems))
	if (isNull(hd(us)))				/* undef values    */
	    hd(us) = hd(elems);				/* replaced by the */
							/* old array vals  */
    updapRoot(ARRAY,ap(bounds,vs));
}

primFun(primAccum) {			/* Array accum			   */
    Cell range  = primArg(4);		/*  :: (a -> Int) ->		   */
    Cell f      = primArg(3);		/*	(b -> c -> b) ->	   */
    Cell orig   = primArg(2);		/*	 Array a b ->		   */
    Cell assocs = primArg(1);		/*	  [Assoc a c] -> Array a b */
    Cell bounds = NIL;
    List vs     = NIL;
    Int  size;

    eval(orig);						/* find bounds	   */
    bounds = fst(snd(whnfHead));
    vs     = dupList(snd(snd(whnfHead)));		/* elements of orig*/
    size   = getSize(bounds,range);
    vs     = foldAssocs(range,size,f,assocs,vs);	/* process assocs  */
    if (vs==UNIT) {
        updapRoot(ap(nameOutBounds,bounds),top());
        cantReduce();
    }
    updapRoot(ARRAY,ap(bounds,vs));
}

primFun(primAccumArray) {		/* Array accumArray		   */
    Cell range  = primArg(5);		/*  :: (a -> Int) ->		   */
    Cell f      = primArg(4);		/*	(b -> c -> b) ->	   */
    Cell z      = primArg(3);		/*	 b ->			   */
    Cell bounds = primArg(2);		/*	  (a,a) ->		   */
    Cell assocs = primArg(1);		/*	   [Assoc a c] -> Array a b*/
    List vs     = NIL;
    Int  size;

    size = getSize(bounds,range);			/* check size	   */
    vs   = copy(size,z);				/* initialize elems*/
    vs   = foldAssocs(range,size,f,assocs,vs);		/* process assocs  */
    if (vs==UNIT) {
        updapRoot(ap(nameOutBounds,bounds),top());
        cantReduce();
    }
    updapRoot(ARRAY,ap(bounds,vs));
}

primFun(primAmap) {			/* map function over array	   */
    Cell f  = primArg(2);		/*  :: (b -> c) ->		   */
    Cell a  = primArg(1);		/*	Array a b -> Array a c	   */
    List us = NIL;
    List vs = NIL;

    eval(a);		
    a = whnfHead;
    for (us=snd(snd(a)); nonNull(us); us=tl(us))
	vs = cons(ap(f,hd(us)),vs);
    updapRoot(ARRAY,ap(fst(snd(a)),rev(vs)));
}

primFun(primSubscript) {		/* Subscript primitive		   */
    Int  index = 0;			/*  :: (a -> Int) ->		   */
    List vs = NIL;			/*	Array a b ->		   */
					/*	 a -> b			   */

    eval(ap(primArg(3),primArg(1)));	/* find required position	   */
    if ((index=whnfInt) < 0)
	cantReduce();
    eval(primArg(2));			/* evaluate array		   */
    if (whatIs(whnfHead)!=ARRAY)
	internal("primBounds");
    for (vs=snd(snd(whnfHead)); nonNull(vs) && index>0; vs=tl(vs))
	--index;
    if (isNull(vs))
	cantReduce();
    updateRoot(hd(vs));
}

primFun(primBounds) {			/* Bounds primitive		   */
    eval(primArg(1));			/*  :: Array a b -> (a,a)	   */
    if (whatIs(whnfHead)!=ARRAY)
	internal("primBounds");
    updateRoot(fst(snd(whnfHead)));
}

primFun(primElems) {			/* elems primitive		   */
    Cell vs = NIL;
    Cell us = NIL;
    eval(primArg(1));			/* evaluate array to whnf	   */
    if (whatIs(whnfHead)!=ARRAY)
	internal("primElems");
    for (us=snd(snd(whnfHead)); nonNull(us); us=tl(us))
	vs = ap(ap(nameCons,hd(us)),vs);
    updateRoot(revOnto(vs,nameNil));
}
#endif

/* --------------------------------------------------------------------------
 * Integer arithmetic primitives:
 * ------------------------------------------------------------------------*/

primFun(primPlusInt) {		       /* Integer addition primitive	   */
    Int x;
    eval(primArg(2));
    x = whnfInt;
    eval(primArg(1));
    updateRoot(mkInt(x+whnfInt));
}

primFun(primMinusInt) { 	       /* Integer subtraction primitive    */
    Int x;
    eval(primArg(2));
    x = whnfInt;
    eval(primArg(1));
    updateRoot(mkInt(x-whnfInt));
}

primFun(primMulInt) {		       /* Integer multiplication primitive */
    Int x;
    eval(primArg(2));
    x = whnfInt;
    eval(primArg(1));
    updateRoot(mkInt(x*whnfInt));
}

primFun(primQuotInt) {			/* Integer division primitive	   */
    Int x;				/* truncated towards zero	   */
    eval(primArg(2));
    x = whnfInt;
    eval(primArg(1));

    if (whnfInt==0)
	cantReduce();

    updateRoot(mkInt(x/whnfInt));
}

primFun(primDivInt) {			/* Integer division primitive	   */
    Int x,r;				/* truncated towards -ve infinity  */
    eval(primArg(2));
    x = whnfInt;
    eval(primArg(1));

    if (whnfInt==0)
	cantReduce();
    r = x%whnfInt;
    x = x/whnfInt;
    if ((whnfInt<0 && r>0) || (whnfInt>0 && r<0))
	x--;
    updateRoot(mkInt(x));
}

primFun(primModInt) {		       /* Integer modulo primitive	   */
    Int x,y;
    eval(primArg(2));
    x = whnfInt;
    eval(primArg(1));
    if (whnfInt==0)
	cantReduce();
    y = x%whnfInt;		       /* "... the modulo having the sign  */
    if ((y<0 && whnfInt>0) ||	       /*	       of the divisor ..." */
	(y>0 && whnfInt<0))	       /* See definition on p.91 of Haskell*/
	updateRoot(mkInt(y+whnfInt));  /* report...			   */
    else
	updateRoot(mkInt(y));
}

primFun(primRemInt) {		       /* Integer remainder primitive	   */
    Int x;
    eval(primArg(2));		       /* div and rem satisfy:		   */
    x = whnfInt;		       /* (x `div` y)*y + (x `rem` y) == x */
    eval(primArg(1));		       /* which is exactly the property    */
    if (whnfInt==0)		       /* described in K&R 2:		   */
	cantReduce();		       /*      (a/b)*b + a%b == a	   */
    updateRoot(mkInt(x%whnfInt));
}

primFun(primNegInt) {		       /* Integer negation primitive	   */
    eval(primArg(1));
    updateRoot(mkInt(-whnfInt));
}

/* --------------------------------------------------------------------------
 * Coercion primitives:
 * ------------------------------------------------------------------------*/

primFun(primCharToInt) {	       /* Character to integer primitive   */
    eval(primArg(1));
    updateRoot(mkInt(charOf(whnfHead)));
}

primFun(primIntToChar) {	       /* Integer to character primitive   */
    eval(primArg(1));
    if (whnfInt<0  || whnfInt>MAXCHARVAL)
	cantReduce();
    updateRoot(mkChar(whnfInt));
}

primFun(primIntToFloat) {		/* Integer to Float primitive	   */
    eval(primArg(1));
    updateRoot(mkFloat((Float)(whnfInt)));
}

/* --------------------------------------------------------------------------
 * Float arithmetic primitives:
 * ------------------------------------------------------------------------*/

primFun(primPlusFloat) {	       /* Float addition primitive	   */
    Float x;
    eval(primArg(2));
    x = whnfFloat;
    eval(primArg(1));
    updateRoot(mkFloat(x+whnfFloat));
}

primFun(primMinusFloat) { 	       /* Float subtraction primitive	   */
    Float x;
    eval(primArg(2));
    x = whnfFloat;
    eval(primArg(1));
    updateRoot(mkFloat(x-whnfFloat));
}

primFun(primMulFloat) {		       /* Float multiplication primitive   */
    Float x;
    eval(primArg(2));
    x = whnfFloat;
    eval(primArg(1));
    updateRoot(mkFloat(x*whnfFloat));
}

primFun(primDivFloat) {		       /* Float division primitive	   */
    Float x;
    eval(primArg(2));
    x = whnfFloat;
    eval(primArg(1));
    if (whnfFloat==0)
	cantReduce();
    updateRoot(mkFloat(x/whnfFloat));
}

primFun(primNegFloat) {		       /* Float negation primitive	   */
    eval(primArg(1));
    updateRoot(mkFloat(-whnfFloat));
}

#if HAS_FLOATS
primFun(primSinFloat) {			/* Float sin (trig) primitive	   */
    eval(primArg(1));
    updateRoot(mkFloat(sin(whnfFloat)));
}

primFun(primCosFloat) {			/* Float cos (trig) primitive	   */
    eval(primArg(1));
    updateRoot(mkFloat(cos(whnfFloat)));
}

primFun(primTanFloat) {			/* Float tan (trig) primitive	   */
    eval(primArg(1));
    updateRoot(mkFloat(tan(whnfFloat)));
}

primFun(primAsinFloat) {		/* Float arc sin (trig) primitive  */
    eval(primArg(1));
    updateRoot(mkFloat(asin(whnfFloat)));
}

primFun(primAcosFloat) {		/* Float arc cos (trig) primitive  */
    eval(primArg(1));
    updateRoot(mkFloat(acos(whnfFloat)));
}

primFun(primAtanFloat) {		/* Float arc tan (trig) primitive  */
    eval(primArg(1));
    updateRoot(mkFloat(atan(whnfFloat)));
}

primFun(primAtan2Float) {		/* Float arc tan with quadrant info*/
    Float t;				/* 		 (trig) primitive  */
    eval(primArg(2));
    t = whnfFloat;
    eval(primArg(1));
    updateRoot(mkFloat(atan2(t,whnfFloat)));
}

primFun(primExpFloat) {			/* Float exponential primitive	   */
    eval(primArg(1));
    updateRoot(mkFloat(exp(whnfFloat)));
}

primFun(primLogFloat) {			/* Float logarithm primitive	   */
    eval(primArg(1));
    if (whnfFloat<=0)
	cantReduce();
    updateRoot(mkFloat(log(whnfFloat)));
}

primFun(primLog10Float) {		/* Float logarithm (base 10) prim  */
    eval(primArg(1));
    if (whnfFloat<=0)
	cantReduce();
    updateRoot(mkFloat(log10(whnfFloat)));
}

primFun(primSqrtFloat) {		/* Float square root primitive	   */
    eval(primArg(1));
    if (whnfFloat<0)
	cantReduce();
    updateRoot(mkFloat(sqrt(whnfFloat)));
}

primFun(primFloatToInt) {		/* Adhoc Float --> Int conversion  */
    eval(primArg(1));
    updateRoot(mkInt((Int)(whnfFloat)));
}
#endif

/* --------------------------------------------------------------------------
 * Comparison primitives:
 * ------------------------------------------------------------------------*/

primFun(primEqInt) {		       /* Integer equality primitive	   */
    Int x;
    eval(primArg(2));
    x = whnfInt;
    eval(primArg(1));
    updateRoot(x==whnfInt ? nameTrue : nameFalse);
}

primFun(primLeInt) {		       /* Integer <= primitive		   */
    Int x;
    eval(primArg(2));
    x = whnfInt;
    eval(primArg(1));
    updateRoot(x<=whnfInt ? nameTrue : nameFalse);
}

primFun(primEqChar) {		       /* Character equality primitive	   */
    Cell x;
    eval(primArg(2));
    x = whnfHead;
    eval(primArg(1));
    updateRoot(x==whnfHead ? nameTrue : nameFalse);
}

primFun(primLeChar) {		       /* Character <= primitive	   */
    Cell x;
    eval(primArg(2));
    x = whnfHead;
    eval(primArg(1));
    updateRoot(x<=whnfHead ? nameTrue : nameFalse);
}

primFun(primEqFloat) {		       /* Float equality primitive	   */
    Float x;
    eval(primArg(2));
    x = whnfFloat;
    eval(primArg(1));
    updateRoot(x==whnfFloat ? nameTrue : nameFalse);
}

primFun(primLeFloat) {		       /* Float <= primitive		   */
    Float x;
    eval(primArg(2));
    x = whnfFloat;
    eval(primArg(1));
    updateRoot(x<=whnfFloat ? nameTrue : nameFalse);
}

/* Generic comparisons implemented using the internal primitive function:
 *
 * primCmp []			= EQ
 *         ((C xs, D ys):rs)
 *	   | C < D		= LT
 *	   | C == D		= primCmp (zip xs ys ++ rs)
 *	   | C > D		= GT
 *	   ((Int n, Int m):rs)
 *	   | n < m		= LT
 *	   | n == m		= primCmp rs
 *	   | n > m		= GT
 *	   etc ... similar for comparison of characters:
 *
 * The list argument to primCmp is represented as an `internal list';
 * i.e. no (:)/[] constructors - use internal cons and NIL instead!
 *
 * To compare two values x and y, evaluate primCmp [(x,y)] and use result.
 */

#define LT            1
#define EQ            2
#define GT            3
#define compResult(x) updateRoot(mkInt(x))

static Name namePrimCmp;

primFun(primCmp) {			/* generic comparison function	   */
    Cell rs = primArg(1);

    if (isNull(rs)) {
	compResult(EQ);
	return;
    }
    else {
	Cell x = fst(hd(rs));
	Cell y = snd(hd(rs));
	Int  whnfArgs1;
	Cell whnfHead1;

	rs = tl(rs);
	eval(x);
	whnfArgs1 = whnfArgs;
	whnfHead1 = whnfHead;

	switch (whatIs(whnfHead1)) {
	    case INTCELL  : if (whnfArgs==0) {		/* compare ints    */
				eval(y);
				if (!isInt(whnfHead) || whnfArgs!=0)
				    break;
				if (intOf(whnfHead1) > whnfInt)
				    compResult(GT);
				else if (intOf(whnfHead1) < whnfInt)
				    compResult(LT);
				else
				    updapRoot(namePrimCmp,rs);
				return;
			    }
			    break;

	    case FLOATCELL: if (whnfArgs==0) {		/* compare floats  */
				eval(y);
				if (!isFloat(whnfHead) || whnfArgs!=0)
				    break;
				if (floatOf(whnfHead1) > whnfFloat)
				    compResult(GT);
				else if (floatOf(whnfHead1) < whnfFloat)
				    compResult(LT);
				else
				    updapRoot(namePrimCmp,rs);
				return;
			    }
			    break;

	    case CHARCELL : if (whnfArgs==0) {		/* compare chars   */
				eval(y);
				if (!isChar(whnfHead) || whnfArgs!=0)
				    break;
				if (charOf(whnfHead1) > charOf(whnfHead))
				    compResult(GT);
				else if (charOf(whnfHead1) < charOf(whnfHead))
				    compResult(LT);
				else
				    updapRoot(namePrimCmp,rs);
				return;
			    }
			    break;

#if HASKELL_ARRAYS
	    case ARRAY    : break;
#endif
#if IO_MONAD
	    case MUTVAR   : break;
#endif

	    default	  : eval(y);			/* compare structs */
			    if (whnfHead1==whnfHead &&
				whnfArgs1==whnfArgs &&
				(whnfHead==UNIT    ||
				 isTuple(whnfHead) ||
				 (isName(whnfHead) &&
				  name(whnfHead).defn==CFUN))) {
				while (whnfArgs1-- >0)
				    rs = cons(pair(pushed(whnfArgs+whnfArgs1),
						   pushed(whnfArgs1)),rs);
				updapRoot(namePrimCmp,rs);
				return;
			    }
			    if (isName(whnfHead1)	    &&
				 name(whnfHead1).defn==CFUN &&
				 isName(whnfHead)	    &&
				 name(whnfHead).defn==CFUN) {
				if (name(whnfHead1).number
						> name(whnfHead).number)
				    compResult(GT);
				else if (name(whnfHead1).number
						< name(whnfHead).number)
				    compResult(LT);
				else
				    break;
				return;
			    }
                            break;
	}
        /* we're going to fail because we can't compare x and y; modify    */
	/* the root expression so that it looks reasonable before failing  */
	/* i.e. output produced will be:  {_compare x y}		   */
	updapRoot(ap(namePrimCmp,x),y);
    }
    cantReduce();
}

primFun(primGenericEq) {		/* Generic equality test	   */
    Cell c = ap(namePrimCmp,singleton(pair(primArg(2),primArg(1))));
    eval(c);
    updateRoot(whnfInt==EQ ? nameTrue : nameFalse);
}

primFun(primGenericLe) {		/* Generic <= test		   */
    Cell c = ap(namePrimCmp,singleton(pair(primArg(2),primArg(1))));
    eval(c);
    updateRoot(whnfInt<=EQ ? nameTrue : nameFalse);
}

primFun(primGenericLt) {		/* Generic < test		   */
    Cell c = ap(namePrimCmp,singleton(pair(primArg(2),primArg(1))));
    eval(c);
    updateRoot(whnfInt<EQ ? nameTrue : nameFalse);
}

primFun(primGenericGe) {		/* Generic >= test		   */
    Cell c = ap(namePrimCmp,singleton(pair(primArg(2),primArg(1))));
    eval(c);
    updateRoot(whnfInt>=EQ ? nameTrue : nameFalse);
}

primFun(primGenericGt) {		/* Generic > test		   */
    Cell c = ap(namePrimCmp,singleton(pair(primArg(2),primArg(1))));
    eval(c);
    updateRoot(whnfInt>EQ ? nameTrue : nameFalse);
}

primFun(primGenericNe) {		/* Generic /= test		   */
    Cell c = ap(namePrimCmp,singleton(pair(primArg(2),primArg(1))));
    eval(c);
    updateRoot(whnfInt!=EQ ? nameTrue : nameFalse);
}

/* --------------------------------------------------------------------------
 * Print primitives:
 * ------------------------------------------------------------------------*/

static Cell consOpen,	consSpace,  consComma,	consClose;
static Cell consObrace, consCbrace, consOsq,	consCsq;
static Cell consBack,	consMinus,  consQuote,  consDQuote;

static Name nameLPrint, nameNLPrint;	/* list printing primitives	   */
static Name nameSPrint, nameNSPrint;	/* string printing primitives	   */

#define print(pr,d,e,ss)    ap(ap(ap(pr,mkInt(d)),e),ss)
#define lprint(pr,xs,ss)    ap(ap(pr,xs),ss)
#define printString(s,ss)   revOnto(stringOutput(s,NIL),ss)
#define printSChar(c,ss)    printString(unlexChar(c,'\"'),ss)

primFun(primPrint) {			/* evaluate and print term	   */
    Int  d    = intOf(primArg(3));	/*    :: Int->Expr->[Char]->[Char] */
    Cell e    = primArg(2);
    Cell ss   = primArg(1);
    Cell temp = evalWithNoError(e);
    if (nonNull(temp))
	updateRoot(printBadRedex(temp,ss));
    else
	printer(root,namePrint,d,ss);
}

primFun(primNPrint) {			/* print term without evaluation   */
    Int    d	  = intOf(primArg(3)); /*     :: Int->Expr->[Char]->[Char] */
    Cell   e	  = primArg(2);
    Cell   ss	  = primArg(1);
    unwind(e);
    printer(root,nameNPrint,d,ss);
}

static Void local printer(root,pr,d,ss)	/* Main part: primPrint/primNPrint */
StackPtr root;				/* root or print redex		   */
Name	 pr;				/* printer to use on components	   */
Int	 d;				/* precedence level		   */
Cell	 ss; {				/* rest of output		   */
    Int  used	= 0;
    Cell output = NIL;

    switch(whatIs(whnfHead)) {

	case NAME     : {   Syntax sy = syntaxOf(name(whnfHead).text);

			    if (name(whnfHead).defn!=CFUN ||
				    name(whnfHead).arity>whnfArgs)
				pr = nameNPrint;

			    if (whnfHead==nameCons && whnfArgs==2) {/*list */
				if (pr==namePrint)
				    startList(root,ss);
				else
				    startNList(root,ss);
				return;
			    }
			    if (whnfArgs==1 && sy!=APPLIC) {	  /* (e1+) */
				used   = 1;
				output = ap(consClose,
					  textAsOp(name(whnfHead).text,
					   ap(consSpace,
					    print(pr,FUN_PREC-1,pushed(0),
					     ap(consOpen,NIL)))));
			    }
			    else if (whnfArgs>=2 && sy!=APPLIC) { /* e1+e2 */
				Syntax a = assocOf(sy);
				Int    p = precOf(sy);
				used     = 2;
				if (whnfArgs>2 || d>p)
				     output = ap(consOpen,output);
				output = print(pr,(a==RIGHT_ASS?p:1+p),
					      pushed(1),
					  ap(consSpace,
					   textAsOp(name(whnfHead).text,
					    ap(consSpace,
					     print(pr,(a==LEFT_ASS? p:1+p),
						  pushed(0),
					      output)))));
				if (whnfArgs>2 || d>p)
				    output = ap(consClose,output);
			    }
			    else				  /* f ... */
				output = textAsVar(name(whnfHead).text,NIL);
			}
			break;

	case INTCELL  : {   Int digit;

			    if (intOf(whnfHead)<0 && d>=FUN_PREC)
				output = ap(consClose,output);

			    do {
				digit = whnfInt%10;
				if (digit<0)
				    digit= (-digit);
				output = ap(consChar('0'+digit),output);
			    } while ((whnfInt/=10)!=0);

			    if (intOf(whnfHead)<0) {
				output = ap(consMinus,output);
				if (d>=FUN_PREC)
				    output = ap(consOpen,output);
			    }

			    output = rev(output);
			    pr	   = nameNPrint;
			}
			break;

	case UNIT     : output = ap(consClose,ap(consOpen,NIL));
			pr     = nameNPrint;
			break;

	case TUPLE    : {   Int  tn   = tupleOf(whnfHead);
                            Cell punc = consOpen;
			    Int  i;

			    used      = tn<whnfArgs ? tn : whnfArgs;
			    output    = NIL;
			    for (i=0; i<used; ++i) {
				output = print(pr,MIN_PREC,pushed(i),
					  ap(punc,
					   output));
				punc   = consComma;
			    }
			    for (; i<tn; ++i) {
				output = ap(punc,output);
				punc   = consComma;
			    }
			    output = ap(consClose,output);
			}
			pr = nameNPrint;
			break;

	case CHARCELL : output = ap(consQuote,
                                  stringOutput(unlexChar(charOf(whnfHead),
                                                         '\''),
				   ap(consQuote,
				    output)));
			pr     = nameNPrint;
			break;

	case FLOATCELL: if (whnfFloat<0.0 && d>=FUN_PREC)
			    output = ap(consOpen,output);
			output = stringOutput(floatToString(whnfFloat),output);
			if (whnfFloat<0.0 && d>=FUN_PREC)
			    output = ap(consClose,output);
			pr = nameNPrint;
			break;

#if HASKELL_ARRAYS
	case ARRAY    : output = stringOutput("{array}",output);
			pr     = nameNPrint;
			break;
#endif

#if IO_MONAD
	case MUTVAR   : output = stringOutput("{mutable variable}",output);
			pr     = nameNPrint;
			break;
#endif

        case DICTCELL : output = stringOutput("{dict}",output);
			pr     = nameNPrint;
			break;

	case FILECELL : output = stringOutput("{file}",output);
			pr     = nameNPrint;
			break;

	default       : internal("Error in graph");
			break;
    }

    if (used<whnfArgs) {		/* Add remaining args to output	   */
	do
	    output = print(pr,FUN_PREC,pushed(used),ap(consSpace,output));
	while (++used<whnfArgs);

	if (d>=FUN_PREC) {		/* Determine if parens are needed  */
	    updapRoot(consOpen,revOnto(output,ap(consClose,ss)));
	    return;
	}
    }

    updateRoot(revOnto(output,ss));
}

/* --------------------------------------------------------------------------
 * List printing primitives:
 * ------------------------------------------------------------------------*/

static Void local startList(root,ss)	/* start printing evaluated list   */
StackPtr root;
Cell     ss; {
    Cell x    = pushed(0);
    Cell xs   = pushed(1);
    Cell temp = evalWithNoError(x);
    if (nonNull(temp))
	updapRoot(consOsq,
		   printBadRedex(temp,
		    lprint(nameLPrint,xs,ss)));
    else if (isChar(whnfHead) && whnfArgs==0)
	updapRoot(consDQuote,
		   printSChar(charOf(whnfHead),
		    lprint(nameSPrint,xs,ss)));
    else
	updapRoot(consOsq,
		   print(namePrint,MIN_PREC,x,
		    lprint(nameLPrint,xs,ss)));
}

static Void local startNList(root,ss)	/* start printing unevaluated list */
StackPtr root;
Cell     ss; {
    Cell x    = pushed(0);
    Cell xs   = pushed(1);
    unwind(x);
    if (isChar(whnfHead) && whnfArgs==0)
	updapRoot(consDQuote,
		   printSChar(charOf(whnfHead),
		    lprint(nameNSPrint,xs,ss)));
    else
	updapRoot(consOsq,
		   print(nameNPrint,MIN_PREC,x,
		    lprint(nameNLPrint,xs,ss)));
}

primFun(primLPrint) {			/* evaluate and print list	   */
    Cell e    = primArg(2);
    Cell ss   = primArg(1);
    Cell temp = evalWithNoError(e);

    if (nonNull(temp))
	updateRoot(printString("] ++ ",printBadRedex(temp,ss)));
    else if (whnfHead==nameCons && whnfArgs==2)
	updapRoot(consComma,
		   ap(consSpace,
		    print(namePrint,MIN_PREC,pushed(0),
		     lprint(nameLPrint,pushed(1),ss))));
    else if (whnfHead==nameNil && whnfArgs==0)
	updapRoot(consCsq,ss);
    else
	updateRoot(printString("] ++ ",printBadRedex(e,ss)));
}

primFun(primNLPrint) {			/* print list without evaluation   */
    Cell e  = primArg(2);
    Cell ss = primArg(1);
    unwind(e);
    if (whnfHead==nameCons && whnfArgs==2)
	updapRoot(consComma,
		   ap(consSpace,
		    print(nameNPrint,MIN_PREC,pushed(0),
		     lprint(nameNLPrint,pushed(1),ss))));
    else if (whnfHead==nameNil && whnfArgs==0)
	updapRoot(consCsq,ss);
    else
	updateRoot(printString("] ++ ",print(nameNPrint,FUN_PREC-1,e,ss)));
}

primFun(primSPrint) {			/* evaluate and print string	   */
    Cell e    = primArg(2);
    Cell ss   = primArg(1);
    Cell temp = evalWithNoError(e);

    if (nonNull(temp))
	updateRoot(printString("\" ++ ",printBadRedex(temp,ss)));
    else if (whnfHead==nameCons && whnfArgs==2) {
	Cell x  = pushed(0);
	Cell xs = pushed(1);
	temp    = evalWithNoError(x);
	if (nonNull(temp))
	    updateRoot(printString("\" ++ [",
			printBadRedex(temp,
			 lprint(nameLPrint,xs,ss))));
	else if (isChar(whnfHead) && whnfArgs==0)
	    updateRoot(printSChar(charOf(whnfHead),
		        lprint(nameSPrint,xs,ss)));
	else
	    updateRoot(printString("\" ++ [",
			printBadRedex(x,
			 lprint(nameLPrint,xs,ss))));
    }
    else if (whnfHead==nameNil && whnfArgs==0)
	updapRoot(consDQuote,ss);
    else
	updateRoot(printString("\" ++ ",printBadRedex(e,ss)));
}

primFun(primNSPrint) {			/* print string without eval	   */
    Cell e  = primArg(2);
    Cell ss = primArg(1);
    unwind(e);
    if (whnfHead==nameCons && whnfArgs==2) {
	Cell x  = pushed(0);
	Cell xs = pushed(1);
	unwind(x);
	if (isChar(whnfHead) && whnfArgs==0)
	    updateRoot(printSChar(charOf(whnfHead),
		        lprint(nameNSPrint,xs,ss)));
	else
	    updateRoot(printString("\" ++ [",
			print(nameNPrint,MIN_PREC,x,
			 lprint(nameNLPrint,xs,ss))));
    }
    else if (whnfHead==nameNil && whnfArgs==0)
	updapRoot(consDQuote,ss);
    else
	updateRoot(printString("\" ++ ",print(nameNPrint,FUN_PREC-1,e,ss)));
}

/* --------------------------------------------------------------------------
 * Auxiliary functions for printer(s):
 * ------------------------------------------------------------------------*/

static Cell local textAsVar(t,ss)	/* reverse t as function symbol	   */
Text t;					/* onto output ss		   */
Cell ss; {
    String s = textToStr(t);
    if ((isascii(s[0]) && isalpha(s[0])) || s[0]=='_' || strcmp(s,"[]")==0)
	return stringOutput(s,ss);
    else
	return ap(consClose,stringOutput(s,ap(consOpen,ss)));
}

static Cell local textAsOp(t,ss)	/* reverse t as op. symbol onto ss */
Text t;
Cell ss; {
    String s = textToStr(t);
    if (isascii(s[0]) && isalpha(s[0]))
	return ap(consBack,stringOutput(s,ap(consBack,ss)));
    else
	return stringOutput(s,ss);
}

static Cell local stringOutput(s,ss)	/* reverse string s onto output ss */
String s;
Cell   ss; {
    while (*s)
	ss = ap(consChar(*s++),ss);
    return ss;
}

static Cell local printBadRedex(rx,rs)	/* Produce expression to print bad */
Cell rx, rs; {				/* redex and then print rest ...   */
    return ap(consObrace,
	    print(nameNPrint,MIN_PREC,rx,
	     ap(consCbrace,
	      rs)));
}

static Cell local printDBadRedex(rx,rs) /* Produce expression for bad redex*/
Cell rx, rs; {				/* within a Dialogue, with special */
    if (isAp(rx) && fun(rx)==nameError) /* handling of {error str} redexes */
	return arg(rx);
    else
	return printBadRedex(rx,rs);
}

Void abandon(what,rx)			/* abandon computation		   */
String what;
Cell   rx; {
    outputString(errorStream,
		 revOnto(stringOutput("\n",NIL),
		 revOnto(stringOutput(what,NIL),
		 revOnto(stringOutput(" error: ",NIL),
			 printDBadRedex(rx,nameNil)))));
    errAbort();
}

/* --------------------------------------------------------------------------
 * Evaluate name, obtaining a C string from a Gofer string:
 * ------------------------------------------------------------------------*/

#if (IO_DIALOGUE | LAMBDAVAR | LAMBDANU)
static String local evalName(es)	/* evaluate es :: [Char] and save  */
Cell es; {				/* in char array... return ptr to  */
    static char buffer[FILENAME_MAX+1];	/* string or 0, if error occurs	   */
    Int         pos    = 0;
    StackPtr    saveSp = sp;

    while (isNull(evalWithNoError(es)))
	if (whnfHead==nameCons && whnfArgs==2) {
	    Cell e = pop();		/* avoid leaving anything on stack */
	    es	   = pop();
	    if (isNull(evalWithNoError(e))
			&& isChar(whnfHead) && whnfArgs==0
			&& pos<FILENAME_MAX)
		buffer[pos++] = charOf(whnfHead);
	    else
		break;
	}
	else if (whnfHead==nameNil && whnfArgs==0) {
	    buffer[pos] = '\0';
	    return buffer;
	}
	else
	    break;

    sp = saveSp;			/* stack pointer must be the same  */
    return 0;				/* as it was on entry		   */
}
#endif

/* --------------------------------------------------------------------------
 * Dialogue based input/output:
 *
 * N.B. take care when modifying this code - it is rather delicate and even
 * the simplest of changes might create a nasty space leak... you have been
 * warned (please let me know if you think there already is a space leak!).
 * ------------------------------------------------------------------------*/

#if IO_DIALOGUE
static Name nameInput;			/* For reading from stdin	   */
static Bool echoChanged;		/* TRUE => echo changed in dialogue*/
static Bool stdinUsed;			/* TRUE => ReadChan stdin has been */
					/*	   seen in dialogue	   */
static FILE *writingFile = 0;		/* points to file open for writing */

Void dialogue(prog)			/* carry out dialogue ...	   */
Cell prog; {				/* :: Dialog=[Response]->[Request] */
    static String ioerr = "Attempt to read response before request complete";
    Cell tooStrict      = mkStr(findText(ioerr));
    Cell resps		= prog = ap(prog,NIL);
    Cell temp;

    echoChanged = FALSE;
    stdinUsed   = FALSE;
    for (;;) {				/* Keep Responding to Requests	   */
	resps = snd(resps) = ap(nameError,tooStrict);
        clearStack();
	if (nonNull(temp=evalWithNoError(prog)))
	    abandonDialogue(temp);
	else if (whnfHead==nameCons && whnfArgs==2) {
	    if (nonNull(temp=evalWithNoError(pushed(0))))
		abandonDialogue(temp);

	    prog = pushed(1+whnfArgs);

	    if (whnfHead==nameReadFile && whnfArgs==1)
		fst(resps) = ap(nameCons,readFile());
	    else if (whnfHead==nameWriteFile && whnfArgs==2)
		fst(resps) = ap(nameCons,writeFile());
	    else if (whnfHead==nameAppendFile && whnfArgs==2)
		fst(resps) = ap(nameCons,appendFile());
	    else if (whnfHead==nameReadChan && whnfArgs==1)
		fst(resps) = ap(nameCons,readChan());
	    else if (whnfHead==nameAppendChan && whnfArgs==2)
		fst(resps) = ap(nameCons,appendChan());
	    else if (whnfHead==nameEcho && whnfArgs==1)
		fst(resps) = ap(nameCons,echo());
	    else if (whnfHead==nameGetArgs && whnfArgs==0)
		fst(resps) = ap(nameCons,getCLArgs());
	    else if (whnfHead==nameGetProgName && whnfArgs==0)
		fst(resps) = ap(nameCons,getProgName());
	    else if (whnfHead==nameGetEnv && whnfArgs==1)
		fst(resps) = ap(nameCons,getEnv());
	    else
		abandonDialogue(pushed(whnfArgs));
	}
	else if (whnfHead==nameNil && whnfArgs==0) {
	    normalTerminal();
	    return;
	}
	else
	    internal("Type error during Dialogue");
    }
}

static Void local abandonDialogue(rx)	/* abandon dialogue after failure  */
Cell rx; {				/* to reduce redex rx		   */
    abandon("Dialogue",rx);
}

static Cell local readFile() {		/* repond to ReadFile request	   */
    String s    = evalName(pushed(0));	/* pushed(0) = file name string	   */
    Cell   temp = NIL;			/* pushed(1) = ReadFile request	   */
					/* pushed(2) = rest of program	   */

    if (!s)				/* problem with filename?	   */
	abandonDialogue(pushed(1));
    if (access(s,0)!=0)			/* can't find file		   */ 
	return ap(nameFailure,ap(nameSearchError,pushed(0)));
    if (isNull(temp = openFile(s)))	/* can't open file		   */
	return ap(nameFailure,ap(nameReadError,pushed(0)));
    return ap(nameStr,temp);		/* otherwise we got a file!	   */
}

static Cell local writeFile() {		/* respond to WriteFile req.	   */
    String s    = evalName(pushed(0));	/* pushed(0) = file name string	   */
    FILE   *fp;				/* pushed(1) = output string	   */
    Cell   temp;			/* pushed(2) = output request	   */
					/* pushed(3) = rest of program	   */

    if (!s)				/* problem with filename?          */
        abandonDialogue(pushed(2));
    pushed(2) = NIL;			/* eliminate space leak!	   */
    if ((fp=fopen(s,FOPEN_WRITE))==0)	/* problem with output file?	   */
	return ap(nameFailure,ap(nameWriteError,pushed(0)));
    drop();
    temp = outputDString(writingFile = fp);
    fclose(fp);
    writingFile = 0;
    if (nonNull(temp))
	return ap(nameFailure,ap(nameWriteError,temp));
    else
	return nameSuccess;
}

static Cell local appendFile() {	/* respond to AppendFile req.	   */
    String s    = evalName(pushed(0));	/* pushed(0) = file name string	   */
    FILE   *fp;				/* pushed(1) = output string	   */
    Cell   temp;			/* pushed(2) = output request	   */
					/* pushed(3) = rest of program	   */

    if (!s)				/* problem with filename?          */
        abandonDialogue(pushed(2));
    pushed(2) = NIL;			/* eliminate space leak!	   */
    if (access(s,0)!=0)			/* can't find file?		   */
	return ap(nameFailure,ap(nameSearchError,pushed(0)));
    if ((fp=fopen(s,FOPEN_APPEND))==0)	/* problem with output file?	   */
	return ap(nameFailure,ap(nameWriteError,pushed(0)));
    drop();
    temp = outputDString(writingFile = fp);
    fclose(fp);
    writingFile = 0;
    if (nonNull(temp))
	return ap(nameFailure,ap(nameWriteError,temp));
    else
	return nameSuccess;
}

static Cell local readChan() {		/* respond to readChan req.	   */
    String s    = evalName(pushed(0));	/* pushed(0) = channel name string */
					/* pushed(1) = output request	   */
					/* pushed(2) = rest of program	   */

    if (!s)				/* problem with filename?	   */
	abandonDialogue(pushed(1));
    if (strcmp(s,"stdin")!=0)		/* only valid channel == stdin	   */
	return ap(nameFailure,ap(nameSearchError,pushed(0)));
    if (stdinUsed)			/* can't reuse stdin channel!      */
	return ap(nameFailure,ap(nameReadError,pushed(0)));
    stdinUsed = TRUE;
    return ap(nameStr,ap(nameInput,UNIT));
}

static Cell local appendChan() {	/* respond to AppendChannel req.   */
    String s = evalName(pushed(0));	/* pushed(0) = channel name string */
    FILE   *fp;				/* pushed(1) = output string	   */
    Cell   temp;			/* pushed(2) = output request	   */
					/* pushed(3) = rest of program	   */
    if (!s)				/* problem with filename?          */
        abandonDialogue(pushed(2));
    pushed(2) = NIL;			/* eliminate space leak!	   */
    if ((fp = validOutChannel(s))==0)	/* problem with output channel?	   */
	return ap(nameFailure,ap(nameSearchError,pushed(0)));
    drop();
    if (nonNull(temp=outputDString(fp)))
	return ap(nameFailure,ap(nameWriteError,temp));
    else
	return nameSuccess;
}

static FILE *local validOutChannel(s)	/* return FILE * for valid output  */
String s; {				/* channel name or 0 otherwise...  */
    if (strcmp(s,"stdout")==0)
	return stdout;
    if (strcmp(s,"stderr")==0)
	return stderr;
    if (strcmp(s,"stdecho")==0)		/* in Gofer, stdecho==stdout	   */
	return stdout;
    return 0;
}

static Cell local echo() {		/* respond to Echo request	   */
    					/* pushed(0) = boolean echo status */
					/* pushed(1) = echo request	   */
					/* pushed(2) = rest of program	   */
    static String inUse  = "stdin already in use";
    static String repeat = "repeated Echo request";

    if (isNull(evalWithNoError(pushed(0)))) {
	if (stdinUsed)
	    return ap(nameFailure,ap(nameOtherError,mkStr(findText(inUse))));
	if (echoChanged)
	    return ap(nameFailure,ap(nameOtherError,mkStr(findText(repeat))));
	if (whnfHead==nameFalse && whnfArgs==0) {
	    echoChanged = TRUE;
	    noechoTerminal();
	    return nameSuccess;
	}
	if (whnfHead==nameTrue && whnfArgs==0) {
	    echoChanged = TRUE;
	    return nameSuccess;
	}
    }
    abandonDialogue(pushed(1));
    return NIL;/*NOTREACHED*/
}

static Cell local getCLArgs() {		/* get command args -- always []   */
    return ap(nameStrList,nameNil);
}

static Cell local getProgName() {	/* get program name -- an error!   */
    return ap(nameFailure,ap(nameOtherError,nameNil));
}

static Cell local getEnv() {		/* get environment variable	   */
    String s = evalName(pushed(0));	/* pushed(0) = variable name string*/
    String r = 0;			/* pushed(1) = output request	   */
					/* pushed(2) = rest of program	   */
    if (!s)
        abandonDialogue(pushed(1));
    if (r=getenv(s))
	return ap(nameStr,revOnto(stringOutput(r,NIL),nameNil));
    else
	return ap(nameFailure,ap(nameSearchError,pushed(0)));
}

primFun(primInput) {			/* read single character from stdin*/
    Int c = readTerminalChar();

    if (c==EOF || c<0 || c>=NUM_CHARS) {
	clearerr(stdin);
	updateRoot(nameNil);
    }
    else
	updapRoot(consChar(c),ap(nameInput,UNIT));
}

primFun(primFopen) {			/* open file for reading as str	   */
    Cell   succ = primArg(1);		/*  :: String->a->(String->a)->a   */
    Cell   fail = primArg(2);
    String s    = evalName(primArg(3));

    if (s){
	Cell file = openFile(s);
	if (nonNull(file)) {
	    updapRoot(succ,file);
	    return;
	}
    }
    updateRoot(fail);
}

static Cell local outputDString(fp)	/* Evaluate string cs and print	   */
FILE *fp; {				/* on specified output stream fp   */
    Cell temp = NIL;
    for (;;) {				/* keep reducing and printing head */
	temp = evalWithNoError(pop());	/* character			   */
	if (nonNull(temp))
	    return printDBadRedex(temp,nameNil);
	else if (whnfHead==nameCons && whnfArgs==2) {
	    if (nonNull(temp=evalWithNoError(pop())))
		return printDBadRedex(temp,top());
	    else if (isChar(whnfHead) && whnfArgs==0) {
		fputc(charOf(whnfHead),fp);
		if (!writingFile)
		    fflush(fp);
	    }
	    else
		break;
	}
	else if (whnfHead==nameNil && whnfArgs==0) {
	    if (writingFile)
		fflush(fp);
	    return NIL;
	}
	else
	    break;
    }
    internal("runtime type error");
    return nameNil;/*NOTREACHED*/
}
#endif

/* --------------------------------------------------------------------------
 * Top-level printing mechanism:
 * ------------------------------------------------------------------------*/

Cell outputString(fp,cs)		/* Evaluate string cs and print	   */
FILE *fp;				/* on specified output stream fp   */
Cell cs; {
    Cell temp;

    for (;;) {				/* keep reducing and printing head */
	clearStack();			/* character			   */
	temp = evalWithNoError(cs);
	if (nonNull(temp))
	    cs = printBadRedex(temp,nameNil);
	else if (whnfHead==nameCons && whnfArgs==2) {
	    Cell c = pushed(0);
	    cs     = pushed(1);

	    if (nonNull(temp=evalWithNoError(c)))
		cs = printBadRedex(temp,cs);
	    else if (isChar(whnfHead) && whnfArgs==0) {
		fputc(charOf(whnfHead),fp);
		    fflush(fp);
	    }
	    else
		break;
	}
	else if (whnfHead==nameNil && whnfArgs==0)
	    return NIL;
	else
	    break;
    }
    internal("runtime type error");
    return nameNil;/*NOTREACHED*/
}

/* --------------------------------------------------------------------------
 * IO monad implementation.   Use these functions instead of old LAMBDAVAR
 *                                                           and LAMBDANU
 * Based on `Lazy State Threads' by Simon Peyton Jones and John Launchbury,
 * to appear in PLDI '94.
 *
 * type ST s a = State s -> (a, State s)
 * ------------------------------------------------------------------------*/

#if IO_MONAD
Void ioExecute(prog)			/* execute IO monad program of type*/
Cell prog; {				/* IO ()			   */
    Cell temp;
    noechoTerminal();
    if (nonNull(temp=evalWithNoError(ap(prog,UNIT))) ||
        nonNull(temp=evalWithNoError(pushed(1))))
	abandon("Program execution",temp);
}

primFun(primSTRun) {			/* ST monad encapsulate		   */
    updapRoot(nameFst,			/*  :: all s.(ST s a) -> a	   */
	      ap(primArg(1),UNIT));
}

primFun(primFst) {			/* fst primitive		   */
    eval(primArg(1));			/*  :: (a,s) -> a		   */
    updateRoot(top());
}

primFun(primSnd) {			/* snd primitive		   */
    eval(primArg(1));			/*  :: (a,s) -> s		   */
    updateRoot(pushed(1));
}

primFun(primSTReturn) {			/* ST monad return		   */
    updapRoot(mkTuple(2),primArg(1));	/* return    :: a -> ST s a	   */
}					/* return a   = \s -> (a,s)	   */

primFun(primIOBind) {			/* IO monad bind		   */
    Cell m = primArg(3);		/* :: ST s a ->			   */
    Cell f = primArg(2);		/*     (a -> ST s b) ->		   */
    Cell s = primArg(1);		/*	ST s b			   */
    eval(ap(m,s));
    updapRoot(ap(f,top()),pushed(1));	/* A strict bind operation on ST   */
}

primFun(primSTBind) {			/* ST monad bind		   */
    Cell m = primArg(3);		/* :: ST s a ->			   */
    Cell f = primArg(2);		/*     (a -> ST s b) ->		   */
    Cell s = primArg(1);		/*	ST s b			   */
    Cell r = ap(m,s);			/* lazy version of bind on ST	   */
    updapRoot(ap(f,ap(nameFst,r)),ap(nameSnd,r));
}

primFun(primSTInter) {			/* ST monad interleave		   */
    Cell m = primArg(2);		/*  :: ST s a ->		   */
    Cell s = primArg(1);		/*      ST s a			   */
    updapRoot(ap(mkTuple(2),ap(nameFst,ap(m,s))),s);
}

primFun(primSTNew) {			/* ST monad variable allocator	   */
    Cell i = primArg(2);		/*  :: a ->			   */
    Cell s = primArg(1);		/* 	ST s (MutVar s a)	   */
    eval(s);				/* force evaluation of state	   */
    updapRoot(ap(mkTuple(2),ap(MUTVAR,i)),s);
}

primFun(primSTAssign) {			/* ST monad assignment		   */
    Cell v = primArg(3);		/*  :: MutVar s a ->		   */
    Cell e = primArg(2);		/*	a ->			   */
    Cell s = primArg(1);		/*	 ST s ()		   */
    eval(s);				/* force evaluation of state	   */
    eval(v);
    if (!isPair(whnfHead) || fst(whnfHead)!=MUTVAR)
	internal("type error in assign");
    snd(whnfHead) = e;			/* Arrgh! impurity! :-)		   */
    updapRoot(ap(mkTuple(2),UNIT),s);
}

primFun(primSTDeref) {			/* ST monad dereference		   */
    Cell v = primArg(2);		/*  :: MutVar s a ->		   */
    Cell s = primArg(1);		/*	ST s a			   */
    eval(s);				/* force evaluation of state	   */
    eval(v);
    if (!isPair(whnfHead) || fst(whnfHead)!=MUTVAR)
	internal("type error in deref");
    updapRoot(ap(mkTuple(2),snd(whnfHead)),s);
}

primFun(primSTMutVarEq) {		/* ST monad variable equality	   */
    Cell x = primArg(2);		/*  :: MutVar s a -> 		   */
    Cell y = primArg(1);		/*      MutVar s a -> Bool	   */
    eval(x);
    x = whnfHead;
    eval(y);
    updateRoot(x==whnfHead ? nameTrue : nameFalse);
}

primFun(primIOGetch) {			/* get character from stdin	   */
    Cell s = primArg(1);		/*  :: IO Char			   */
    eval(s);
    updapRoot(ap(mkTuple(2),mkChar(readTerminalChar())),s);
}

primFun(primIOPutchar) {		/* print character on stdout	   */
    Cell c = primArg(2);		/*  :: Char ->			   */
    Cell s = primArg(1);		/*	IO ()			   */
    eval(s);
    eval(c);
    putchar(charOf(whnfHead));
    fflush(stdout);
    updapRoot(ap(mkTuple(2),UNIT),s);
}

#if HASKELL_ARRAYS
primFun(primSTNewArr) {			/* allocate mutable array	   */
    Cell range  = primArg(4);		/*  :: (a -> Int) ->		   */
    Cell bounds = primArg(3);		/*      (a,a) ->		   */
    Cell z	= primArg(2);		/*	 b ->			   */
    Cell s	= primArg(1);		/*	  ST s (MutArr s a b)	   */
    Int  size;
    eval(s);
    size = getSize(bounds,range);
    updapRoot(ap(mkTuple(2), ap(ARRAY, ap(bounds,copy(size,z)))), s);
}

primFun(primSTReadArr) {		/* read element in mutable array   */
    Cell index = primArg(4);		/*  :: ((a,a) -> a -> Int) ->	   */
    Cell a     = primArg(3);		/*	MutArr s a b ->		   */
    Cell i     = primArg(2);		/*       a ->			   */
    Cell s     = primArg(1);		/*	  ST s b		   */
    Cell vs    = NIL;
    eval(s);
    eval(a);
    vs = snd(whnfHead);
    eval(ap(ap(index,fst(vs)),i));
    while (whnfInt-- > 0)
	vs = snd(vs);
    updapRoot(ap(mkTuple(2),fst(snd(vs))),s);
}

primFun(primSTWriteArr) {		/* write element in mutable array  */
    Cell index = primArg(5);		/*  :: ((a,a) -> a -> Int) ->	   */
    Cell a     = primArg(4);		/*	MutArr s a b ->		   */
    Cell i     = primArg(3);		/*       a ->			   */
    Cell v     = primArg(2);		/*	  b ->			   */
    Cell s     = primArg(1);		/*	   ST s ()		   */
    Cell vs    = NIL;
    eval(s);
    eval(a);
    vs = snd(whnfHead);
    eval(ap(ap(index,fst(vs)),i));
    while (whnfInt-- > 0)
	vs = snd(vs);
    fst(snd(vs)) = v;
    updapRoot(ap(mkTuple(2),UNIT),s);
}

primFun(primSTFreeze) {			/* freeze mutable array		   */
    Cell arr = primArg(2);		/*  :: MutArr s a b ->		   */
    Cell s   = primArg(1);		/*	ST s (Array a b)	   */
    eval(s);
    eval(arr);
    updapRoot(ap(mkTuple(2),ap(ARRAY,dupList(snd(whnfHead)))),s);
}
#endif
#endif

/* --------------------------------------------------------------------------
 * Lambda-var prototype implementation:
 *
 * OBSOLETE: You are strongly advised NOT to use the following code
 * ------------------------------------------------------------------------*/

#ifdef LAMBDAVAR
Void lvExecute(prog)			/* execute lambda var prog of type */
Cell prog; {				/* Proc ()			   */
    Cell temp;
    noechoTerminal();
    temp = evalWithNoError(ap(prog,UNIT));
    if (nonNull(temp))
	abandon("Program execution",temp);
}

primFun(primLvReturn) {			/* lambda var return		   */
    updateRoot(primArg(2));		/* return    :: a -> Proc a	   */
					/* return e _ = e		   */
}

primFun(primLvPure) {			/* lambda var pure		   */
    updapRoot(primArg(1),UNIT);		/* pure  :: Proc a -> a		   */
					/* pure e = e ()		   */
}

primFun(primLvRead) {			/* lambda var reader		   */
    Cell v = primArg(3);		/* (?)::Var a->(a->Proc b)->Proc b */
    Cell f = primArg(2);		/* (Var v ? f) () ===> f v ()	   */
    eval(v);
    if (whnfHead!=nameVar || whnfArgs!=1)
	internal("type error in reader");
    updapRoot(ap(f,pushed(0)),UNIT);
}

primFun(primLvBind) {			/* lambda var bind		   */
    Cell m = primArg(3);		/*($=)::Proc a->(a->Proc b)->Proc b*/
    Cell f = primArg(2);		/* (m $= f) () ===> f (m ()) ()	   */
    Cell a = ap(m,UNIT);		/* strict in first argument	   */
    eval(a);
    updapRoot(ap(f,a),UNIT);
}

primFun(primLvVar) {			/* lambda var, new variable	   */
    updapRoot(ap(primArg(2),		/* var :: (Var a -> Proc b)->Proc b*/
		 ap(nameVar,		/* var f () = f {newvar} ()	   */
		    nameLvUnbound)),
	      UNIT);
}

primFun(primLvNewvar) {			/* lambda var, improved new var	   */
    updapRoot(nameVar,nameLvUnbound);	/* newvar   :: Proc (Var a)	   */
					/* newvar () = {newVar}		   */
}

primFun(primLvAssign) {			/* lambda var assign		   */
    Cell e = primArg(3);		/* assign :: a -> Var a -> Proc () */
    Cell v = primArg(2);	        /* assign e (Var v) () = ()	   */
    eval(v);
    if (whnfHead!=nameVar || whnfArgs!=1)
	internal("type error in assign");
    while (isPair(v) && fst(v)==INDIRECT) {
	v = arg(v);
	allowBreak();
    }
    snd(v) = e;				/* Arrgh! impurity!		   */
    updateRoot(UNIT);
}

primFun(primLvVarEq) {			/* lambda var equality for Vars	   */
    Cell x = primArg(2);		/* :: Var a -> Var a -> Bool	   */
    Cell y = primArg(1);
    eval(x);
    while (isPair(x) && fst(x)==INDIRECT) {
	x = arg(x);
	allowBreak();
    }
    eval(y);
    while (isPair(y) && fst(y)==INDIRECT) {
	y = arg(y);
	allowBreak();
    }
    updateRoot(x==y ? nameTrue : nameFalse);
}

primFun(primLvGetch) {			/* get character from stdin	   */
    updateRoot(mkChar(readTerminalChar()));
}

primFun(primLvPutchar) {		/* print character on stdout	   */
    eval(primArg(2));			/* putchar c () ==> ()		   */
    putchar(charOf(whnfHead));
    updateRoot(UNIT);
}

primFun(primLvSystem) {			/* do system call		   */
    String s = evalName(primArg(2));	/* system s () ==> int result	   */
    Int    n = s ? system(s) : 1;
    updateRoot(mkInt(n));
}
#endif

/* --------------------------------------------------------------------------
 * Lambda-nu prototype implementation:
 *
 * OBSOLETE: You are strongly advised NOT to use the following code
 * ------------------------------------------------------------------------*/

#ifdef LAMBDANU
Void lnExecute(prog)			/* execute lambda nu prog of type  */
Cell prog; {				/* Cmd a ()			   */
    Cell temp;
    noechoTerminal();
    temp = evalWithNoError(ap(prog,nameLnDone));
    if (nonNull(temp))
	abandon("Command execution",temp);
}

primFun(primLnDone) {			/* lambda nu done		   */
    updateRoot(UNIT);			/* behaviour is ignored, so isn't  */
}					/* really important		   */

primFun(primLnReturn) {			/* lambda nu return		   */
    updapRoot(primArg(1),primArg(2));	/* return    :: a -> Cmd d a	   */
}					/* return a c = c a		   */

primFun(primLnBind) {			/* lambda nu bind		   */
    Cell a = primArg(3);		/* (>>=)::Cmd c a -> (a -> Cmd c b)*/
    Cell b = primArg(2);		/*			-> Cmd c b */
    Cell c = primArg(1);		/* (a>>=b) c = a (flip b c)	   */
    updapRoot(a,ap(ap(nameLnFlip,b),c));
}

primFun(primLnFlip) {			/* flip primitive, for use in bind */
    updapRoot(ap(primArg(3),primArg(1)),primArg(2));
}

primFun(primLnNew) {			/* lambda nu allocate variable	   */
    Cell c = primArg(1);		/* new :: Cmd a (Tag b)		   */
    updapRoot(c,ap(nameTag,nameLnUnbound));
}

primFun(primLnAssign) {			/* lambda nu assign		   */
    Cell v = primArg(3);		/* assign:: Tag a -> a -> Cmd d () */
    Cell e = primArg(2);	        /* assign (Tag v) e c = c ()	   */
    Cell c = primArg(1);
    eval(v);
    if (whnfHead!=nameTag || whnfArgs!=1)
	internal("type error in assign");
    while (isPair(v) && fst(v)==INDIRECT) {
	v = arg(v);
	allowBreak();
    }
    snd(v) = e;				/* Arrgh! impurity!		   */
    updapRoot(c,UNIT);
}

primFun(primLnRead) {			/* lambda nu reader		   */
    Cell vv = primArg(3);		/* (?) :: Tag a -> (a -> Cmd d b)  */
    Cell b  = primArg(2);		/*			-> Cmd d b */
    Cell c  = primArg(1);		/* (Tag v ? b) c = b v c	   */
    eval(vv);
    if (whnfHead!=nameTag || whnfArgs!=1)
	internal("type error in reader");
    updapRoot(ap(b,pushed(0)),c);
}

primFun(primLnIo) {			/* lambda nu i/o		   */
    updapRoot(primArg(2),primArg(1));	/* io :: ((a->d)->d) -> Cmd d a	   */
}					/* io a c = a c			   */

primFun(primLnBegin) {			/* lambda nu begin		   */
    updapRoot(primArg(1),nameLnNocont);	/* begin :: Cmd d a -> d	   */
}

primFun(primLnTagEq) {			/* lambda nu equality for Tags	   */
    Cell x = primArg(2);		/* :: Tag a -> Tag a -> Bool	   */
    Cell y = primArg(1);
    eval(x);
    while (isPair(x) && fst(x)==INDIRECT) {
	x = arg(x);
	allowBreak();
    }
    eval(y);
    while (isPair(y) && fst(y)==INDIRECT) {
	y = arg(y);
	allowBreak();
    }
    updateRoot(x==y ? nameTrue : nameFalse);
}

primFun(primLnGetch) {			/* get character from stdin	   */
    updapRoot(primArg(1),mkChar(readTerminalChar()));
}

primFun(primLnPutchar) {		/* print character on stdout	   */
    Cell c = primArg(1);		/* putchar    :: Char -> Cmd a ()  */
    eval(primArg(2));			/* putchar x c = c ()		   */
    putchar(charOf(whnfHead));
    updapRoot(c,UNIT);
}

primFun(primLnSystem) {			/* do system call		   */
    Cell   c = primArg(1);		/* system    :: String -> Cmd a Int*/
    String s = evalName(primArg(2));	/* system s c = c (int result)	   */
    Int    n = s ? system(s) : 1;
    updapRoot(c,mkInt(n));
}
#endif

#endif

/* --------------------------------------------------------------------------
 * Build array of character conses:
 * ------------------------------------------------------------------------*/

static Cell consCharArray[NUM_CHARS];

Cell consChar(c)			/* return application (:) c	   */
Char c; {
    if (c<0)
	c += NUM_CHARS;
    return consCharArray[c];
}

/*-------------------------------------------------------------------------*/
