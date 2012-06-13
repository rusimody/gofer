/* --------------------------------------------------------------------------
 * storage.h:   Copyright (c) Mark P Jones 1991-1994.   All rights reserved.
 *              See goferite.h for details and conditions of use etc...
 *              Gofer version 2.30 March 1994
 *
 * Defines storage datatypes: Text, Name, Module, Tycon, Cell, List, Pair,
 * Triple, ...
 * ------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 * Typedefs for main data types:
 * Many of these type names are used to indicate the intended us of a data
 * item, rather than for type checking purposes.  Sadly (although sometimes,
 * fortunately), the C compiler cannot distinguish between the use of two
 * different names defined to be synonyms for the same types.
 * ------------------------------------------------------------------------*/

typedef Int	     Text;			 /* text string 	   */
typedef Unsigned     Syntax;			 /* syntax (assoc,preced)  */
typedef Int	     Addr;			 /* address of code	   */
typedef Int	     Cell;			 /* general cell value	   */
typedef Cell far     *Heap;			 /* storage of heap	   */
typedef Cell	     Pair;			 /* pair cell		   */
typedef Int	     StackPtr;			 /* stack pointer	   */
typedef Cell	     Offset;			 /* offset/generic variable*/
typedef Cell	     Tycon;			 /* type constructor	   */
typedef Cell	     Type;			 /* type expression	   */
typedef Cell	     Kind;			 /* kind expression	   */
typedef Cell	     Constr;			 /* constructor expression */
typedef Cell	     Name;			 /* named value 	   */
typedef Void	     (*Prim) Args((StackPtr));	 /* primitive function	   */
typedef Cell	     Class;			 /* type class		   */
typedef Cell	     Inst;			 /* instance of type class */
typedef Int	     Idx;			 /* dictionary index tree  */
typedef Int	     Dict;			 /* dictionary values	   */
typedef Cell	     Triple;			 /* triple of cell values  */
typedef Cell	     List;			 /* list of cells	   */
typedef Int	     Module;			 /* module number	   */
typedef FloatImpType Float;			 /* implementation of Float*/

/* --------------------------------------------------------------------------
 * Text storage:
 * provides storage for the characters making up identifier and symbol
 * names, string literals, character constants etc...
 * ------------------------------------------------------------------------*/

extern	String	     textToStr		Args((Text));
extern	Text	     findText		Args((String));
extern	Text	     inventText		Args((Void));
extern  Text	     inventDictText	Args((Void));

/* --------------------------------------------------------------------------
 * Specification of syntax (i.e. default written form of application)
 * ------------------------------------------------------------------------*/

#define MIN_PREC  0		       /* weakest binding operator	   */
#define MAX_PREC  9		       /* strongest binding operator	   */
#define FUN_PREC  (MAX_PREC+2)	       /* binding of function symbols	   */
#define DEF_PREC  MAX_PREC
#define APPLIC	  00000 	       /* written applicatively 	   */
#define LEFT_ASS  02000 	       /* left associative infix	   */
#define RIGHT_ASS 04000 	       /* right associative infix	   */
#define NON_ASS   06000 	       /* non associative infix 	   */
#define DEF_ASS   NON_ASS

#define assocOf(x)	((x)&NON_ASS)
#define precOf(x)	((x)&(~NON_ASS))
#define mkSyntax(a,p)	((a)|(p))
#define DEF_OPSYNTAX	mkSyntax(DEF_ASS,DEF_PREC)

extern	Void   addSyntax  Args((Int,Text,Syntax));
extern	Syntax syntaxOf   Args((Text));

/* --------------------------------------------------------------------------
 * Primitive functions:
 * ------------------------------------------------------------------------*/

extern struct primitive {		/* table of primitives		   */
    String ref;				/* primitive reference string	   */
    Int	   arity;			/* primitive function arity	   */
    Prim   imp;				/* primitive implementation	   */
} primitives[];

#define PRIM_GOFC	((Prim)1)	/* primitive implemented by gofc   */
#define PRIM_NOGOFC	((Prim)2)	/* or not, as the case may be ...  */

/* --------------------------------------------------------------------------
 * Program code storage: for holding compiled function defns etc...
 * ------------------------------------------------------------------------*/

extern	Addr	     getMem Args((Int));

/* --------------------------------------------------------------------------
 * Heap storage:
 * Provides a garbage collectable heap for storage of expressions etc.
 * ------------------------------------------------------------------------*/

#define heapAlloc(s) (Heap)(farCalloc(s,sizeof(Cell)))
#define heapBuilt()  (heapFst)
extern  Int          heapSize;
extern  Heap	     heapFst, heapSnd;
#ifdef  GLOBALfst
register Heap	     heapTopFst GLOBALfst;
#else
extern   Heap 	     heapTopFst;
#endif
#ifdef  GLOBALsnd
register Heap	     heapTopSnd GLOBALsnd;
#else
extern   Heap 	     heapTopSnd;
#endif
#define fst(c)	     heapTopFst[c]
#define snd(c)	     heapTopSnd[c]

extern	Pair	     pair	     Args((Cell,Cell));
extern  Void	     overwrite	     Args((Pair,Pair));
extern  Cell	     markExpr	     Args((Cell));
extern  Void	     markWithoutMove Args((Cell));
extern  Void	     garbageCollect  Args((Void));

#define mark(v)      v=markExpr(v)

#define isPair(c)    ((c)<0)
#define isGenPair(c) (-heapSize<=(c) && (c)<0)

extern	Cell	     whatIs    Args((Cell));

/* --------------------------------------------------------------------------
 * Box cell tags are used as the fst element of a pair to indicate that
 * the snd element of the pair is to be treated in some special way, other
 * than as a Cell.  Examples include holding integer values, variable name
 * and string text etc.
 * ------------------------------------------------------------------------*/

#define TAGMIN	     1		  /* Box and constructor cell tag values   */
#define BCSTAG	     20 	  /* Box=TAGMIN..BCSTAG-1		   */
#define isTag(c)     (TAGMIN<=(c) && (c)<SPECMIN) /* Tag cell values	   */
#define isBoxTag(c)  (TAGMIN<=(c) && (c)<BCSTAG)  /* Box cell tag values   */
#define isConTag(c)  (BCSTAG<=(c) && (c)<SPECMIN) /* Constr cell tag values*/

#define INDIRECT     1		  /* Indirection node:	      snd :: Cell  */
#define INDIRECT1    2		  /* Temporary indirection:   snd :: Cell  */
#define VARIDCELL    3		  /* Identifier variable:     snd :: Text  */
#define VAROPCELL    4		  /* Operator variable:       snd :: Text  */
#define DICTVAR	     5		  /* Dictionary variable:     snd :: Text  */
#define CONIDCELL    6		  /* Identifier constructor:  snd :: Text  */
#define CONOPCELL    7		  /* Operator constructor:    snd :: Text  */
#define STRCELL      8		  /* String literal:	      snd :: Text  */
#define INTCELL	     9		  /* Integer literal:	      snd :: Int   */
#if NPLUSK
#define ADDPAT	     10		  /* (_+k) pattern discr:     snd :: Int   */
#define MULPAT       11		  /* (c*_) pattern discr:     snd :: Int   */
#endif
#define DICTCELL     12		  /* Dictionary value:        snd :: Dict  */
#define FILECELL     13		  /* Input file number:       snd :: Int   */
#if !BREAK_FLOATS
#define FLOATCELL    14		  /* Floating pt number:      snd :: Float */
#endif

#define textOf(c)	((Text)(snd(c)))
#define intValOf(c)	(snd(c))
#define mkVar(t)	ap(VARIDCELL,t)
#define mkVarop(t)	ap(VAROPCELL,t)
#define inventVar()	mkVar(inventText())
#define mkDictVar(t)	ap(DICTVAR,t)
#define inventDictVar() mkDictVar(inventDictText())
#define mkStr(t)	ap(STRCELL,t)
extern	Bool		isVar     Args((Cell));
extern	Bool		isCon     Args((Cell));
extern  Cell		openFile  Args((String));
extern  Void		evalFile  Args((Cell));

#define isFloat(c)      (isPair(c) && fst(c)==FLOATCELL)
extern	Cell		mkFloat		Args((FloatPro));
extern  FloatPro	floatOf		Args((Cell));
extern  String		floatToString   Args((FloatPro));
extern  FloatPro	stringToFloat   Args((String));
#if BREAK_FLOATS
extern  Cell		part1Float	Args((FloatPro));
extern  Cell		part2Float	Args((FloatPro));
extern  FloatPro	floatFromParts	Args((Cell,Cell));
#endif

/* --------------------------------------------------------------------------
 * Constructor cell tags are used as the fst element of a pair to indicate
 * a particular syntactic construct described by the snd element of the
 * pair.
 * Note that a cell c will not be treated as an application (AP/isAp) node
 * if its first element is a constructor cell tag, whereas a cell whose fst
 * element is a special cell will be treated as an application node.
 * ------------------------------------------------------------------------*/

#define LETREC	     20 	  /* LETREC	snd :: ([Decl],Exp)	   */
#define COND	     21 	  /* COND	snd :: (Exp,Exp,Exp)	   */
#define LAMBDA	     22 	  /* LAMBDA	snd :: Alt		   */
#define FINLIST      23 	  /* FINLIST	snd :: [Exp]		   */
#define COMP	     24		  /* COMP	snd :: (Exp,[Qual])	   */
#define LISTCOMP     25 	  /* LISCOMP	snd :: (Exp,[Qual])	   */
#define MONADCOMP    26		  /* MONADCOMP  snd :: (dicts,(Exp,[Qual]))*/
#define ASPAT	     27 	  /* ASPAT	snd :: (Var,Exp)	   */
#define ESIGN	     28 	  /* ESIGN	snd :: (Exp,Type)	   */
#define CASE	     29 	  /* CASE	snd :: (Exp,[Alt])	   */
#define FATBAR	     30 	  /* FATBAR	snd :: (Exp,Exp)	   */
#define LAZYPAT      31 	  /* LAZYPAT	snd :: Exp		   */
#define QUAL	     32 	  /* QUAL       snd :: ([Classes],Type)    */
#define RUNST	     33		  /* RUNST	snd :: Exp		   */
#define DOCOMP	     34		  /* DOCOMP	snd :: [DQual]		   */
#if BREAK_FLOATS
#define FLOATCELL    35		  /* FLOATCELL  snd :: (Int,Int)	   */
#endif

#define DOQUAL	     37		  /* DOQUAL	snd :: Exp		   */
#define BOOLQUAL     38 	  /* BOOLQUAL	snd :: Exp		   */
#define QWHERE	     39 	  /* QWHERE	snd :: [Decl]		   */
#define FROMQUAL     40 	  /* FROMQUAL	snd :: (Exp,Exp)	   */

#define GUARDED      42 	  /* GUARDED	snd :: [guarded exprs]	   */

#define ARRAY        45		  /* Array:     snd :: (Bounds,[Values])   */
#define MUTVAR	     46		  /* Mutvar:	snd :: Cell		   */

#define POLYTYPE     48		  /* POLYTYPE	snd :: (Kind,Type)	   */

#define C_TOPARG     50		  /* Compiler instruction codes:	   */
#define C_TOPFUN     51		  /* see cmachine.c for further details	   */
#define C_SETSTK     52
#define C_EVAL	     53
#define C_LABEL      54
#define C_GOTO	     55
#define C_FLUSH	     56
#define C_UPDAP2     57
#define C_HEAP	     58

#define C_PUSHPAIR   60
#define C_UPDATE     61
#define C_SLIDE	     62
#define C_INTEQ	     63
#define C_TEST	     64

#define C_UPDAP	     70
#define C_INTGE      71
#define C_INTDV      72

#define BRANCH	     80           /* Code generator continuation structures*/
#define FBRANCH	     81		  /* see cmachine.c for further details    */

/* --------------------------------------------------------------------------
 * Special cell values:
 * ------------------------------------------------------------------------*/

#define SPECMIN      101
#define isSpec(c)    (SPECMIN<=(c) && (c)<TUPMIN)/* Special cell values    */

#define UNIT	     101	  /* Unit type/value denoted () 	   */
#define STAR	     102	  /* Representing the kind of types	   */
#define LIST	     103	  /* Builtin list type constructor	   */
#define ARROW	     104	  /* Builtin function space constructor    */
#define WILDCARD     105	  /* Wildcard pattern			   */

#define NAME	     110	  /* whatIs code for isName		   */
#define TYCON	     111	  /* whatIs code for isTycon		   */
#define CLASS	     112	  /* whatIs code for isClass		   */
#define SELECT       113          /* whatIs code for isSelect              */
#define INSTANCE     114          /* whatIs code for isInst                */
#define TUPLE	     115	  /* whatIs code for tuple constructor	   */
#define OFFSET	     116	  /* whatis code for offset		   */
#define AP	     117	  /* whatIs code for application node	   */
#define CHARCELL     118	  /* whatIs code for isChar		   */

#define SIGDECL      120	  /* Signature declaration		   */
#define CFUN	     121	  /* Indicates name acting as constr fun   */
#define MFUN	     122	  /* Indicates name acting as member fun   */
#define UNDEFINED    123	  /* indicates name with syntax but no defn*/
#define PREDEFINED   124	  /* predefined name, not yet filled       */
#define NEEDED       125	  /* marks name as needed supercombinator  */

#define DATATYPE     130	  /* datatype type constructor		   */
#define SYNONYM	     131	  /* synonym type constructor		   */
#define RESTRICTSYN  132	  /* synonym with restricted scope	   */

#define NODEPENDS    135	  /* stop calculation of deps in type check*/

#define TOP	     140	  /* refers to top of stack in cmachine.c  */
#define POP	     141	  /* like TOP above, but remove from stack */

#define ROOTFST	     145	  /* represents func to move down from root*/

#define C_MKAP	     150          /* Compiler instruction codes:	   */
#define C_ALLOC	     151	  /* see cmachine.c for further details	   */
#define C_RETURN     152

#define ERRCONT	     160	  /* Code generator continuation structures*/
#define RUNONC	     161	  /* see cmachine.c for further details    */
#define FRUNONC	     162
#define UPDRETC	     163

#define fn(from,to)  pair(pair(ARROW,from),to)	 /* make type:	from -> to */

/* --------------------------------------------------------------------------
 * Tuple data/type constructors:
 * ------------------------------------------------------------------------*/

#define TUPMIN	     201
#define isTuple(c)   (TUPMIN<=(c) && (c)<OFFMIN)
#define mkTuple(n)   (TUPMIN+(n))
#define tupleOf(n)   ((Int)((n)-TUPMIN))

/* --------------------------------------------------------------------------
 * Offsets: (generic types/stack offsets)
 * ------------------------------------------------------------------------*/

#define OFFMIN	     (TUPMIN+NUM_TUPLES)
#define isOffset(c)  (OFFMIN<=(c) && (c)<TYCMIN)
#define offsetOf(c)  ((c)-OFFMIN)
#define mkOffset(o)  (OFFMIN+(o))

/* --------------------------------------------------------------------------
 * Type constructor names:
 * ------------------------------------------------------------------------*/

#define TYCMIN	     (OFFMIN+NUM_OFFSETS)
#define isTycon(c)   (TYCMIN<=(c) && (c)<NAMEMIN)
#define mkTycon(n)   (TCMIN+(n))
#define tycon(n)     tabTycon[(n)-TYCMIN]

struct Tycon {
    Text  text;
    Int   line;
    Int   arity;
    Kind  kind;				/* kind (includes arity) of Tycon  */
    Cell  what;				/* DATATYPE/SYNONYM/RESTRICTSYN... */
    Cell  defn;
    Tycon nextTyconHash;
};

extern struct Tycon tabTycon[];

extern Tycon newTycon	  Args((Text));
extern Tycon findTycon	  Args((Text));
extern Tycon addPrimTycon Args((String,Kind,Int,Cell,Cell));

#define isSynonym(h)	(isTycon(h) && tycon(h).what==SYNONYM)
#define mkPolyType(n,t)	pair(POLYTYPE,pair(n,t))
#define isPolyType(t)	(isPair(t) && fst(t)==POLYTYPE)
#define polySigOf(t)	fst(snd(t))
#define monoTypeOf(t)	snd(snd(t))

/* --------------------------------------------------------------------------
 * Globally defined name values:
 * ------------------------------------------------------------------------*/

#define NAMEMIN      (TYCMIN+NUM_TYCON)
#define isName(c)    (NAMEMIN<=(c) && (c)<SELMIN)
#define mkName(n)    (NAMEMIN+(n))
#define name(n)      tabName[(n)-NAMEMIN]

struct Name {
    Text text;
    Int  line;
    Int  arity;
    Int  number;     /* UNDEFINED : line number of first use		   */
		     /* CFUN	  : constructor number (e.g. Nil=0,Cons=1) */
		     /* MFUN	  : member number (offset into Dict!)	   */
    Cell type;
    Cell defn;
    Addr code;
    Prim primDef;
    Name nextNameHash;
};

extern struct Name tabName[];

extern Name newName	 Args((Text));
extern Name findName	 Args((Text));
extern Void addPrim	 Args((Int,Name,String,Type));
extern Name addPrimCfun  Args((String,Int,Int,Cell));

/* --------------------------------------------------------------------------
 * Type class values:
 * ------------------------------------------------------------------------*/

#define SELMIN       (NAMEMIN+NUM_NAME)          /* dictionary selectors   */
#define isSelect(c)  (SELMIN<=(c) && (c)<INSTMIN)
#define mkSelect(n)  (SELMIN+(n))
#define selectOf(c)  ((Int)((c)-SELMIN))

#define INSTMIN      (SELMIN+NUM_SELECTS)        /* instances              */
#define isInst(c)    (INSTMIN<=(c) && (c)<CLASSMIN)
#define mkInst(n)    (INSTMIN+(n))
#define inst(in)     tabInst[(in)-INSTMIN]

struct Inst {
    Class cl;
    Int   line;
    Kind  sig;				/* kinds of variables in header	   */
    Cell  head;				/* :: Pred			   */
    List  specifics;			/* :: [Pred]			   */
    Int   numSpecifics;			/* length(specifics)		   */
    List  implements;
};

/* a predicate (an element :: Pred) is an application of a Class to one or
 * more type expressions
 */

#define CLASSMIN     (INSTMIN+NUM_INSTS)
#define isClass(c)   (CLASSMIN<=(c) && (c)<CHARMIN)
#define mkClass(n)   (CLASSMIN+(n))
#define class(n)     tabClass[(n)-CLASSMIN]

struct Class {
    Text text;				/* Name of class		   */
    Int  line;
    Int  arity;
    Kind sig;				/* Sig ::= NIL | (Kind,Sig)	   */
    Cell head;				/* :: Pred			   */
    List supers;			/* :: [Pred]			   */
    Int  numSupers;			/* length(supers)		   */
    List members;			/* :: [Name]			   */
    Int  numMembers;			/* length(members)		   */
    List defaults;			/* :: [Name]			   */
    List instances;			/* :: [Inst]			   */
    Idx  dictIndex;
};

struct Idx {
    Cell test;
    Idx  fail;
    Idx  match;      /* may also be used as a Dict value ... */
};
#define NOIDX	     ((Idx)(-1))
#define NODICT	     ((Dict)(-1))

extern struct Class    tabClass[];
extern struct Inst far *tabInst;
extern struct Idx  far *tabIndex;
extern Cell	   far *tabDict;

#define idx(ix)        tabIndex[ix]
#define dict(at)       tabDict[at]
#define dictOf(c)      ((Dict)(snd(c)))
#define mkDict(d)      ap(DICTCELL,d)

extern Class newClass  Args((Text));
extern Class findClass Args((Text));
extern Inst  newInst   Args((Void));
extern Idx   newIdx    Args((Cell));
extern Dict  newDict   Args((Int));

/* --------------------------------------------------------------------------
 * Character values:
 * ------------------------------------------------------------------------*/

#define CHARMIN      (CLASSMIN+NUM_CLASSES)
#define MAXCHARVAL   (NUM_CHARS-1)
#define isChar(c)    (CHARMIN<=(c) && (c)<INTMIN)
#define charOf(c)    ((Char)(c-CHARMIN))
#define mkChar(c)    ((Cell)(CHARMIN+((unsigned)((c)%NUM_CHARS))))

/* --------------------------------------------------------------------------
 * Small Integer values:
 * ------------------------------------------------------------------------*/

#define INTMIN	     (CHARMIN+NUM_CHARS)
#define INTMAX	     MAXPOSINT
#define isSmall(c)   (INTMIN<=(c))
#define INTZERO      (INTMIN/2 + INTMAX/2)

extern	Bool isInt   Args((Cell));
extern	Int  intOf   Args((Cell));
extern	Cell mkInt   Args((Int));

/* --------------------------------------------------------------------------
 * Implementation of triples:
 * ------------------------------------------------------------------------*/

#define triple(x,y,z) pair(x,pair(y,z))
#define fst3(c)      fst(c)
#define snd3(c)      fst(snd(c))
#define thd3(c)      snd(snd(c))

/* --------------------------------------------------------------------------
 * Implementation of lists:
 * ------------------------------------------------------------------------*/

#define NIL	     0
#define isNull(c)    ((c)==NIL)
#define nonNull(c)   (c)
#define cons(x,xs)   pair(x,xs)
#define singleton(x) cons(x,NIL)
#define hd(c)	     fst(c)
#define tl(c)	     snd(c)

extern	Int	     length	  Args((List));
extern	List	     appendOnto   Args((List,List));
extern  List	     dupList	  Args((List));
extern	List	     revOnto	  Args((List, List));
#define rev(xs)      revOnto((xs),NIL)

extern	Cell	     cellIsMember Args((Cell,List));
extern	Cell	     varIsMember  Args((Text,List));
extern	List	     copy	  Args((Int,Cell));
extern	List	     diffList	  Args((List,List));
extern  List	     take	  Args((Int,List));
extern  List	     removeCell	  Args((Cell,List));

/* The following macros provide `inline expansion' of some common ways of
 * traversing, using and modifying lists:
 *
 * N.B. We use the names _f, _a, _xs, Zs, in an attempt to avoid clashes
 *	with identifiers used elsewhere.
 */

#define mapBasic(_init,_step)	  {List Zs=(_init);\
				   for(;nonNull(Zs);Zs=tl(Zs))\
				   _step;}
#define mapModify(_init,_step)	  mapBasic(_init,hd(Zs)=_step)

#define mapProc(_f,_xs) 	  mapBasic(_xs,_f(hd(Zs)))
#define map1Proc(_f,_a,_xs)	  mapBasic(_xs,_f(_a,hd(Zs)))
#define map2Proc(_f,_a,_b,_xs)	  mapBasic(_xs,_f(_a,_b,hd(Zs)))
#define map3Proc(_f,_a,_b,_c,_xs) mapBasic(_xs,_f(_a,_b,_c,hd(Zs)))

#define mapOver(_f,_xs) 	  mapModify(_xs,_f(hd(Zs)))
#define map1Over(_f,_a,_xs)	  mapModify(_xs,_f(_a,hd(Zs)))
#define map2Over(_f,_a,_b,_xs)	  mapModify(_xs,_f(_a,_b,hd(Zs)))
#define map3Over(_f,_a,_b,_c,_xs) mapModify(_xs,_f(_a,_b,_c,hd(Zs)))

/* --------------------------------------------------------------------------
 * Implementation of function application nodes:
 * ------------------------------------------------------------------------*/

#define ap(f,x)      pair(f,x)
#define fun(c)	     fst(c)
#define arg(c)	     snd(c)
#define isAp(c)      (isPair(c) && !isTag(fst(c)))
extern	Cell	     getHead	 Args((Cell));
extern	List	     getArgs	 Args((Cell));
extern	Int	     argCount;
extern  Cell         nthArg	 Args((Int,Cell));
extern  Int	     numArgs	 Args((Cell));
extern  Cell	     applyToArgs Args((Cell,List));

/* --------------------------------------------------------------------------
 * Stack implementation:
 * ------------------------------------------------------------------------*/

extern	Cell	     cellStack[];
#ifdef  GLOBALsp
register StackPtr    sp GLOBALsp;
#else
extern	StackPtr     sp;
#endif
#define clearStack() sp=(-1)
#define stackEmpty() (sp==(-1))
#define stack(p)     cellStack[p]
#define chkStack(n)  if (sp>=NUM_STACK-n) stackOverflow()
#define push(c)      chkStack(1); onto(c)
#define onto(c)	     stack(++sp)=(c)
#define pop()	     stack(sp--)
#define drop()	     sp--
#define top()	     stack(sp)
#define pushed(n)    stack(sp-(n))

extern Void	     stackOverflow Args((Void));

/* --------------------------------------------------------------------------
 * Module control:
 * The implementation of `module' storage is hidden.
 * ------------------------------------------------------------------------*/

extern Module	   startNewModule  Args((Void));
extern Bool        nameThisModule  Args((Name));
extern Module	   moduleThisName  Args((Name));
extern Void	   dropModulesFrom Args((Module));

/*-------------------------------------------------------------------------*/
