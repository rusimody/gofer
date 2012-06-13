/* --------------------------------------------------------------------------
 * gofc.h:      Copyright (c) Mark P Jones 1991-1994.   All rights reserved.
 *              See goferite.h for details and conditions of use etc...
 *		Gofer Compiler version 1.00 February 1992
 *              Gofer version 2.30 March 1994
 *
 * Header file for Gofer Compiler runtime system.
 * ------------------------------------------------------------------------*/

#include "prelude.h"

/*- Garbage collected heap ------------------------------------------------*/

#define GC_MARKSCAN	0			/* for mark/scan collector */
#define GC_TWOSPACE	1			/* for twospace collector  */

typedef Int		Cell;			/* general cell value	   */
typedef Cell far	*Heap;			/* storage of heap	   */
extern  Int		heapSize;		/* Pairs are stored in the */
extern  Void		garbageCollect Args((Void));

#if HASKELL_ARRAYS
extern  Void		allocArray Args((Int,Cell,Cell));
extern  Void		dupArray   Args((Cell));
#endif

/*- Mark-scan collector ---------------------------------------------------*/

#if      GC_MARKSCAN
#ifdef   GLOBALfst
register Heap heapTopFst GLOBALfst;		/* Cells with -ve indices  */
#else
extern   Heap heapTopFst;
#endif
#ifdef   GLOBALsnd
register Heap heapTopSnd GLOBALsnd;
#else
extern   Heap heapTopSnd;
#endif
#define fst(c)		heapTopFst[c]
#define snd(c)		heapTopSnd[c]
#define isPair(c)	((c)<0)
extern  Cell		pair	Args((Cell,Cell));

#if HASKELL_ARRAYS
typedef Cell ArrEltPtr;
#define arrElt(pa)          fst(pa)
#define nextElt(pa)         (pa=snd(pa))
#define arrBnds(arr)        fst(snd(arr))
#define setEltPtr(pa,arr,i) {	Int j = i;				   \
				for (pa=snd(snd(arr)); j>0; --j)	   \
				    nextElt(pa);			   \
			    }
#define arrMap(p,arr)       {   ArrEltPtr pa = snd(snd(arr));		   \
                                while (isPair(pa)) { p(pa); nextElt(pa); } \
                            }
#endif
#endif

/*- Two-space collector ---------------------------------------------------*/

#if	GC_TWOSPACE
#ifdef  GLOBALfst
register Heap		from GLOBALfst;
#else
extern  Heap		from;			/* top of from space	   */
#endif
#ifdef  GLOBALsnd
register Cell		hp GLOBALsnd;
#else
extern  Cell		hp;			/* last used heap loc	   */
#endif
#define fst(c)		from[c]
#define snd(c)		from[(c)+1]
#define isPair(c)	((c)<0)
#define INLINE_ALLOC    0			/* 1 => allocate inline	   */
#if     INLINE_ALLOC
#define pair(l,r)	((from[++hp]=(l)), (from[++hp]=(r)), (hp-1))
#else
extern  Cell		pair	Args((Cell,Cell));
#endif

#if HASKELL_ARRAYS
typedef Cell ArrEltPtr;
#define arrElt(pa)	    from[pa]
#define nextElt(pa)	    (++pa)
#define arrBnds(arr)	    from[arr+2]
#define setEltPtr(pa,arr,i) pa=(arr+i+3)
#define arrMap(p,arr)	    {	Int len = from[arr+1]-1;		   \
				ArrEltPtr pa = arr+3;			   \
				while (0<len--) { p(pa); nextElt(pa); }	   \
			    }
#endif
#endif

/*- Tags for fst() element in particular kinds of Pair ------------------- */

#define INDIRECT	0			/* Indirection		   */
#define INDIRECT1	1			/* Second form used in gc  */
#define FORWARD		2			/* Forwarding pointer	   */
#define INTCELL         3			/* (Big) Integer	   */
#define STRCELL		4			/* Character String	   */
#define SUPERCOMB	5			/* Supercombinator	   */
#define FILECELL	6			/* File value		   */
#define FLOATCELL	7			/* Floating point	   */
#define ARRAY		8			/* Array		   */
#define MUTVAR		9			/* Mutable variable	   */
#if BREAK_FLOATS
#define MAXBOXTAG       FILECELL		/* Last boxed cell tag	   */
extern  Cell		safeMkFloat	Args((FloatPro));
#else
#define MAXBOXTAG	FLOATCELL		/* Last boxed cell tag	   */
#define safeMkFloat(n)	mkFloat((FloatPro)n)
#endif
#define MAXTAG		MUTVAR			/* Last tag value	   */

#define mkBig(n)	pair(INTCELL,n)
#define bigOf(c)	((Int)(snd(c)))

typedef FloatImpType	Float;
extern  Cell		mkFloat		Args((FloatPro));
extern	FloatPro	floatOf		Args((Cell));
extern	String		floatToString	Args((FloatPro));
extern  FloatPro	stringToFloat	Args((String));

#define mkString(s)	pair(STRCELL,(Int)(s))
#define stringOf(c)	((String)(snd(c)))

#define mkSuper(sc)	pair(SUPERCOMB,(Int)(sc))
#define superOf(c)      ((Super *)(snd(c)))

/*- Cells>MAXTAG represent small integers, characters, dictionaries and -- */
/*- constructor functions -- we don't have to worry which since these ---- */
/*- routines will only be used with well-typed source programs ----------- */

#define SMALLMIN	(MAXTAG+2)
#define SMALLMAX        MAXPOSINT
#define SMALLZERO       (SMALLMIN/2 + SMALLMAX/2)
#define isSmall(c)      (SMALLMIN<=(c))
#define mkSmall(n)      (SMALLZERO+(n))
#define smallOf(c)      ((Int)(c-SMALLZERO))

#define mkInt(n)	(isSmall(mkSmall(n)) ? mkSmall(n) : mkBig(n))
#define intOf(c)	(isSmall(c) ? smallOf(c) : bigOf(c))

#define mkChar(c)	((Cell)(SMALLMIN+((unsigned)((c)%NUM_CHARS))))
#define charOf(c)       ((Char)((c)-SMALLMIN))

#define mkDict(n)	((Cell)(SMALLMIN+(n)))
#define dictOf(c)       ((Int)((c)-SMALLMIN))

#define mkCfun(n)	((Cell)(SMALLMIN+(n)))
#define cfunOf(c)	((Int)((c)-SMALLMIN))
#define FAIL		mkCfun(-1)		/* Every type has a Fail   */

/*- Control stack implementation ------------------------------------------*/

typedef Cell		*StackPtr;		 /* stack pointer	   */
extern	Cell		cellStack[];
#ifdef  GLOBALsp
register StackPtr	sp GLOBALsp;
#else
extern	StackPtr	sp;
#endif
#define clearStack()	sp=cellStack+NUM_STACK
#define stackLoop(i)	for (i=cellStack+NUM_STACK-1; i>=sp; i--)
#define push(c)      	if (sp>cellStack) *--sp=(c); else overflow()
#define	onto(c)		*--sp=(c)		/* fast form of push()	   */
#define pop()		*sp++
#define drop()		sp++
#define top()		*sp
#define pushed(n)	sp[n]
#define pushedSince(p)	((Int)((p)-sp))
#define offset(n)	root[-(n)]

/*- references to body of compiled code -----------------------------------*/

#define ARGCHECK 0		/* set to 1 for no. of argument checking   */
extern  int argcheck;		/* check for consistency between main	   */
				/* program and runtime library		   */

extern  int		num_scs;		/* supercombinators	   */
extern  Cell		sc[];
#if	ARGCHECK
typedef Void		Super Args((StackPtr));
#else
typedef Void		Super Args((Void));
#endif
extern  Super		*scNames[];

extern  int		num_dicts;		/* dictionaries		   */
extern  Cell		dict[];
extern  int		dictImps[];
#define dsel(n,d)	dict[dictOf(d)+n]

/*-Super combinator skeleton definition -------------------------------------
 * the following macros are used to construct the heading for a super-
 * combinator definition.  The combn() family of macros is used for the
 * benefit of compilers which do not automatically unroll small loops.
 * combinators with >9 args are headed using the comb macro, and a loop is
 * always used ... at least in the C code.  Adjust according to taste!
 * ------------------------------------------------------------------------*/

#if     ARGCHECK
#define defSc(nm,args)	Void nm(root)					   \
			register StackPtr root; {			   \
			    if (root-sp<=args)				   \
				insufficientArgs();			   \
			    root=sp;
#else
#define defSc(nm,args)	Void nm() {					   \
			    register StackPtr root=sp;
#endif
#define Arg		*root = snd(*(root+1)); root++;
#define needStack(n)	if (sp-cellStack<n) overflow()
#define End		}

#define comb(nm,n)	defSc(nm,n) {int i=n; do {Arg} while (--i>0);}
#define comb0(nm)	defSc(nm,0)
#define comb1(nm)	defSc(nm,1) Arg
#define comb2(nm)	defSc(nm,2) Arg Arg
#define comb3(nm)	defSc(nm,3) Arg Arg Arg
#define comb4(nm)	defSc(nm,4) Arg Arg Arg Arg
#define comb5(nm)	defSc(nm,5) Arg Arg Arg Arg Arg
#define comb6(nm)	comb(nm,6)
#define comb7(nm)	comb(nm,7)
#define comb8(nm)	comb(nm,8)
#define comb9(nm)	comb(nm,9)

/*- macros for simple steps in compiled code -------------------------------*/

extern  Cell whnf;		/* head of term in weak head normal form    */
extern  Int  whnfInt;		/* integer value for term in whnf	    */

#define pushInt(n)		onto(mkInt(n))
#define pushFloat(f)		onto(safeMkFloat(f))
#define pushStr(s)		onto(mkString(s))
#define mkap()			sp[1]=pair(*sp,sp[1]); sp++
#define toparg(e)		*sp=pair(*sp,e)
#define topfun(e)		*sp=pair(e,*sp)
#define pushpair(l,r)		onto(pair(l,r))
#define updap(o,l,r)		snd(root[-o])=r; fst(root[-o])=l
#define update(o,c)		updap(o,INDIRECT,c)
#define updap2(o)		updap(o,*sp,sp[1]); sp+=2
#define alloc()			pushpair(0,0)
#define slide(n,e)		pushed(n)=e; sp+=n
#define setstk(n)		sp=root-n
#define test(c)			if (whnf!=c)
#define inteq(n)		if (whnfInt!=n)
#define intge(h,n)		if (whnfInt>=n) {			   \
				    heap(h);				   \
				    onto(mkInt(whnfInt-n));		   \
				} else
#define intdv(h,n)		if (whnfInt>=0 && (whnfInt%n==0)) {	   \
				    heap(h);				   \
				    onto(mkInt(whnfInt/n));		   \
				} else
#define ret()			sp=root; return

/* N.B.  values in heap() calls are possibly overestimates of storage use
 * if INTCELL or FLOATCELL (with BREAK_FLOATS) values are ever allocated.
 * If you change the basic allocators used here so that the exact figure
 * is required, it will probably be best to make sure that an INTCELL is
 * _always_ heap allocated (including the two INTCELLs that make up a
 * BREAK_FLOATS FLOATCELL).  The alternative is to arrange that any unfilled
 * cells are filled in with blanks of an appropriate form.
 */
#if GC_MARKSCAN
#define heap(n)			/*do nothing*/
#endif
#if GC_TWOSPACE
#define heap(n)			if (hp+(2*n)>=0) garbageCollect()
#endif

/*- builtin primitive functions -------------------------------------------*/

extern Cell primFatbar,     primFail;	/* System (internal) primitives	   */
extern Cell primUndefMem,   primBlackHole;
extern Cell primSel,	    primIf;
extern Cell primStrict;

extern Cell primPlusInt,    primMinusInt;/* User (general) primitives	   */
extern Cell primMulInt,     primDivInt;
extern Cell primModInt,     primRemInt;
extern Cell primNegInt,	    primQuotInt;
extern Cell primCharToInt,  primIntToChar;
extern Cell primIntToFloat;
extern Cell primPlusFloat,  primMinusFloat;
extern Cell primMulFloat,   primDivFloat;
extern Cell primNegFloat;
extern Cell primEqInt,	    primLeInt;
extern Cell primEqChar,     primLeChar;
extern Cell primEqFloat,    primLeFloat;
extern Cell primGenericEq,  primGenericNe;
extern Cell primGenericGt,  primGenericGe;
extern Cell primGenericLt,  primGenericLe;
extern Cell primShowsInt,   primShowsFloat;
extern Cell primError;
extern Cell primFopen;

#if  IO_MONAD
extern Cell primSTRun,	    primSTReturn;/* IO and ST monad primitives	   */
extern Cell primIOBind,     primSTBind;
extern Cell primSTNew,	    primSTAssign;
extern Cell primSTDeref,    primSTMutVarEq;
extern Cell primIOGetch,    primIOPutchar;
#endif

#if  HAS_FLOATS
extern Cell primSinFloat,   primAsinFloat;
extern Cell primCosFloat,   primAcosFloat;
extern Cell primTanFloat,   primAtanFloat;
extern Cell primAtan2Float, primExpFloat;
extern Cell primLogFloat,   primLog10Float;
extern Cell primSqrtFloat,  primFloatToInt;
#endif

/*- runtime support functions and variables -------------------------------*/

typedef Void (*TopLevel)        Args((Cell));
extern  TopLevel topLevel;
#if IO_DIALOGUE
extern  Void dialogue		Args((Cell));
#endif
#if IO_MONAD
extern  Void iomonad		Args((Cell));
#endif

extern Void eval		Args((Cell));
extern Void overflow		Args((Void));
extern Void insufficientArgs	Args((Void));
extern Void fail		Args((Void));
extern Cell rootFst		Args((Cell));
extern Int  readTerminalChar	Args((Void));
extern Void noechoTerminal	Args((Void));
extern Void normalTerminal	Args((Void));

/* ----------------------------------------------------------------------- */
