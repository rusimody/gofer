/* --------------------------------------------------------------------------
 * type.c:      Copyright (c) Mark P Jones 1991-1994.   All rights reserved.
 *              See goferite.h for details and conditions of use etc...
 *              Gofer version 2.30 March 1994
 *
 * This is the Gofer type checker:  Based on the extended algorithm in my
 * PRG technical report PRG-TR-10-91, supporting the use of qualified types
 * in the form of multi-parameter type classes, according to my `new
 * approach' to type classes posted to the Haskell mailing list.
 * This program uses the optimisations for constant and locally-constant
 * overloading.
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"

/*#define DEBUG_TYPES*/
/*#define DEBUG_KINDS*/

Bool coerceNumLiterals = FALSE;		/* TRUE => insert fromInteger calls*/
					/*         etc for numeric literals*/
Bool catchAmbigs       = FALSE;		/* TRUE => functions with ambig.   */
					/* 	   types produce error	   */
Bool overSingleton     = TRUE;		/* TRUE => overload singleton list */
					/*	   notation, [x]	   */

Type typeString, typeBool;		/* Important primitive types	   */
Type typeInt,    typeChar;
Type typeFloat;

Name nameTrue, nameFalse;		/* primitive boolean constructors  */
Name nameNil, nameCons;			/* primitive list constructors	   */

#if    IO_DIALOGUE
Type   typeDialogue;
Name   nameReadFile,    nameWriteFile;	/* I/O name primitives		   */
Name   nameAppendFile,  nameReadChan;
Name   nameAppendChan,  nameEcho;
Name   nameGetArgs,     nameGetProgName;
Name   nameGetEnv;
Name   nameSuccess,     nameStr;
Name   nameFailure,     nameStrList;
Name   nameWriteError;
Name   nameReadError,   nameSearchError;
Name   nameFormatError, nameOtherError;
#endif

#if    IO_MONAD
Type   typeIO, typeProgIO;		/* for the IO monad, IO and IO ()  */
Type   typeWorld, typeST;		/* built on top of IO = ST World   */
Type   typeMutVar;
#if    HASKELL_ARRAYS
Type   typeMutArr;
#endif
#endif

#if    HASKELL_ARRAYS
Type   typeArray;
#endif

#ifdef LAMBDAVAR
static Type typeProc, typeVar;		/* primitive Proc and Var types	   */
Name   nameVar;				/* primitive Var constructor	   */
Type   typeProg;			/* program Proc ()		   */
#endif

#ifdef LAMBDANU
static Type typeCmd, typeTag;		/* primitive Cmd and Tag types	   */
Name   nameTag;				/* primitive Tag constructor	   */
Type   typeLnProg;			/* program Cmd a ()		   */
#endif

/* --------------------------------------------------------------------------
 * Data structures for storing a substitution:
 *
 * For various reasons, this implementation uses structure sharing, instead of
 * a copying approach.	In principal, this is fast and avoids the need to
 * build new type expressions.	Unfortunately, this implementation will not
 * be able to handle *very* large expressions.
 *
 * The substitution is represented by an array of type variables each of
 * which is a triple:
 *	bound	a (skeletal) type expression, or NIL if the variable
 *		is not bound.
 *	offs	offset of skeleton in bound.  If isNull(bound), then offs is
 *		used to indicate whether that variable is generic (i.e. free
 *		in the current assumption set) or fixed (i.e. bound in the
 *		current assumption set).  Generic variables are assigned
 *		offset numbers whilst copying type expressions (t,o) to
 *		obtain their most general form.
 *	kind	kind of value bound to type variable (`type variable' is
 *		rather inaccurate -- `constructor variable' would be better).
 * ------------------------------------------------------------------------*/

typedef struct {			/* Each type variable contains:	   */
    Type bound;				/* A type skeleton (unbound==NIL)  */
    Int  offs;				/* Offset for skeleton		   */
    Kind kind;				/* kind annotation		   */
} Tyvar;

static	Int	  numTyvars;		/* no. type vars currently in use  */
#if     FIXED_SUBST
static	Tyvar	  tyvars[NUM_TYVARS];	/* storage for type variables	   */
#else
static  Tyvar    *tyvars = 0;		/* storage for type variables	   */
static  Int       maxTyvars = 0;
#endif
static	Int	  typeOff;		/* offset of result type 	   */
static	Type	  typeIs;		/* skeleton of result type	   */
static	List	  predsAre;		/* list of predicates in type	   */
#define tyvar(n)  (tyvars+(n))		/* nth type variable		   */
#define tyvNum(t) ((t)-tyvars)		/* and the corresp. inverse funct. */
static	Int	  nextGeneric;	        /* number of generics found so far */
static  List	  genericVars;		/* list of generic vars		   */

				        /* offs values when isNull(bound): */
#define FIXED_TYVAR    0	        /* fixed in current assumption	   */
#define UNUSED_GENERIC 1	        /* not fixed, not yet encountered  */
#define GENERIC        2	        /* GENERIC+n==nth generic var found*/

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static Void   local emptySubstitution Args((Void));
static Void   local expandSubst       Args((Int));
static Int    local newTyvars         Args((Int));
static Int    local newKindedVars     Args((Kind));
static Tyvar *local getTypeVar        Args((Type,Int));
static Void   local tyvarType         Args((Int));
static Void   local bindTv            Args((Int,Type,Int));
static Void   local expandSyn	      Args((Tycon, Int, Type *, Int *));
static Void   local expandSyn1	      Args((Tycon, Type *, Int *));
static Cell   local getDerefHead      Args((Type,Int));

static Void   local clearMarks        Args((Void));
static Void   local resetGenericsFrom Args((Int));
static Void   local markTyvar         Args((Int));
static Void   local markType          Args((Type,Int));

static Type   local copyTyvar         Args((Int));
static Type   local copyType          Args((Type,Int));
#ifdef DEBUG_TYPES
static Type   local debugTyvar	      Args((Int));
static Type   local debugType	      Args((Type,Int));
#endif

static Bool   local doesntOccurIn     Args((Type,Int));

static Bool   local varToVarBind      Args((Tyvar *,Tyvar *));
static Bool   local varToTypeBind     Args((Tyvar *,Type,Int));
static Bool   local kvarToVarBind     Args((Tyvar *,Tyvar *));
static Bool   local kvarToTypeBind    Args((Tyvar *,Type,Int));
static Bool   local unify             Args((Type,Int,Type,Int));
static Bool   local sameType          Args((Type,Int,Type,Int));
static Bool   local kunify	      Args((Kind,Int,Kind,Int));

static Void   local kindError	      Args((Int,Constr,Constr,String,Kind,Int));
static Void   local kindConstr	      Args((Int,Constr));
static Kind   local kindAtom	      Args((Constr));
static Void   local kindPred	      Args((Int,Cell));
static Void   local kindType	      Args((Int,String,Type));
static Void   local fixKinds	      Args((Void));

static Void   local initTCKind	      Args((Cell));
static Void   local kindTC	      Args((Cell));
static Void   local genTC	      Args((Cell));
static Kind   local copyKindvar	      Args((Int));
static Kind   local copyKind	      Args((Kind,Int));

static Bool   local eqKind	      Args((Kind,Kind));
static Kind   local getKind	      Args((Cell,Int));

static Kind   local makeSimpleKind    Args((Int));
static Kind   local simpleKind	      Args((Int));
static Kind   local makeVarKind	      Args((Int));
static Void   local varKind	      Args((Int));

static Void   local emptyAssumption   Args((Void));
static Void   local enterBindings     Args((Void));
static Void   local leaveBindings     Args((Void));
static Void   local markAssumList     Args((List));
static Cell   local findAssum         Args((Text));
static Pair   local findInAssumList   Args((Text,List));
static Int    local newVarsBind       Args((Cell));
static Void   local newDefnBind       Args((Cell,Type));
static Void   local instantiate       Args((Type));

static Void   local typeError         Args((Int,Cell,Cell,String,Type,Int));
static Void   local reportTypeError   Args((Int,Cell,Cell,String,Type,Type));
static Cell   local typeExpr          Args((Int,Cell));
static Cell   local varIntro          Args((Cell,Type));
static Void   local typeEsign         Args((Int,Cell));
static Void   local typeCase          Args((Int,Int,Cell));
static Void   local typeComp	      Args((Int,Type,Cell,List));
static Void   local typeMonadComp     Args((Int,Cell));
static Cell   local compZero	      Args((List,Int));
static Cell   local typeFreshPat      Args((Int,Cell));

static Cell   local typeAp            Args((Int,Cell));
static Void   local typeAlt           Args((Cell));
static Int    local funcType          Args((Int));

static Void   local typeTuple         Args((Cell));
static Type   local makeTupleType     Args((Int));

static Void   local typeBindings      Args((List));
static Void   local removeTypeSigs    Args((Cell));

static Void   local noOverloading     Args((List));
static Void   local restrictedBindAss Args((Cell));
static Void   local restrictedAss     Args((Int,Cell,Type));

static Void   local explicitTyping    Args((List));
static List   local gotoExplicit      Args((List));
static List   local explPreds         Args((Text,List,List));

static Void   local implicitTyping    Args((List));
static Void   local addEvidParams     Args((List,Cell));

static Void   local typeInstDefn      Args((Inst));
static Void   local typeClassDefn     Args((Class));
static Void   local typeMembers       Args((String,List,List,Cell,Kind));
static Void   local typeMember        Args((String,Name,Name,Cell,Kind));

static Void   local typeBind          Args((Cell));
static Void   local typeDefAlt        Args((Int,Cell,Pair));
static Cell   local typeRhs           Args((Cell));
static Void   local guardedType       Args((Int,Cell));

static Void   local generaliseBind    Args((Int,List,Cell));
static Void   local generaliseAss     Args((Int,List,Cell));
static Type   local generalise        Args((List,Type));

static Void   local checkBindSigs     Args((Cell));
static Void   local checkTypeSig      Args((Int,Cell,Type));
static Void   local tooGeneral        Args((Int,Cell,Type,Type));

static Bool   local equalSchemes      Args((Type,Type));
static Bool   local equalQuals        Args((List,List));
static Bool   local equalTypes        Args((Type,Type));

static Void   local typeDefnGroup     Args((List));

#if IO_DIALOGUE
static Void   local initIOtypes	      Args((Void));
#endif

/* --------------------------------------------------------------------------
 * Frequently used type skeletons:
 * ------------------------------------------------------------------------*/

static Type  var;			/* mkOffset(0)		   	   */
static Type  arrow;			/* mkOffset(0) -> mkOffset(1)      */
static Type  typeList;			/* [ mkOffset(0) ] 	    	   */
static Type  typeUnit;			/* ()				   */
static Type  typeIntToInt;		/* Int -> Int		   	   */
#if    IO_MONAD
static Type  typeSTab;			/* ST a b			   */
#endif

static Name  nameFromInt;		/* fromInteger function		   */
static Class classNum;			/* class Num			   */
static Cell  predNum;			/* Num (mkOffset(0))		   */
static Class classMonad;		/* class Monad			   */
static Cell  predMonad;			/* Monad (mkOffset(0))		   */
static Class classMonad0;		/* class Monad0			   */
static Cell  predMonad0;		/* Monad0 (mkOffset(0))		   */
static Kind  starToStar;		/* Type -> Type			   */
static Kind  monadSig;			/* [Type -> Type]		   */

/* --------------------------------------------------------------------------
 * Basic operations on current substitution:
 * ------------------------------------------------------------------------*/

#include "subst.c"

/* --------------------------------------------------------------------------
 * Kind expressions:
 *
 * In the same way that values have types, type constructors (and more
 * generally, expressions built from such constructors) have kinds.
 * The syntax of kinds in the current implementation is very simple:
 *
 *	  kind ::= STAR		-- the kind of types
 *		|  kind => kind -- constructors
 *		|  variables	-- either INTCELL or OFFSET
 *
 * ------------------------------------------------------------------------*/

#include "kind.c"

/* --------------------------------------------------------------------------
 * Assumptions:
 *
 * A basic typing statement is a pair (Var,Type) and an assumption contains
 * an ordered list of basic typing statements in which the type for a given
 * variable is given by the most recently added assumption about that var.
 *
 * In practice, the assumption set is split between a pair of lists, one
 * holding assumptions for vars defined in bindings, the other for vars
 * defined in patterns/binding parameters etc.	The reason for this
 * separation is that vars defined in bindings may be overloaded (with the
 * overloading being unknown until the whole binding is typed), whereas the
 * vars defined in patterns have no overloading.  A form of dependency
 * analysis (at least as far as calculating dependents within the same group
 * of value bindings) is required to implement this.  Where it is known that
 * no overloaded values are defined in a binding (i.e. when the `dreaded
 * monomorphism restriction' strikes), the list used to record dependents
 * is flagged with a NODEPENDS tag to avoid gathering dependents at that
 * level.
 *
 * To interleave between vars for bindings and vars for patterns, we use
 * a list of lists of typing statements for each.  These lists are always
 * the same length.  The implementation here is very similar to that of the
 * dependency analysis used in the static analysis component of this system.
 * ------------------------------------------------------------------------*/

static List defnBounds;		       	/*::[[(Var,Type)]] possibly ovrlded*/
static List varsBounds;		       	/*::[[(Var,Type)]] not overloaded  */
static List depends;		       	/*::[?[Var]] dependents/NODEPENDS  */

#define saveVarsAssump() List saveAssump = hd(varsBounds)
#define restoreVarsAss() hd(varsBounds)  = saveAssump

static Void local emptyAssumption() {  	/* set empty type assumption	   */
    defnBounds = NIL;
    varsBounds = NIL;
    depends    = NIL;
}

static Void local enterBindings() {    /* Add new level to assumption sets */
    defnBounds = cons(NIL,defnBounds);
    varsBounds = cons(NIL,varsBounds);
    depends    = cons(NIL,depends);
}

static Void local leaveBindings() {    /* Drop one level of assumptions    */
    defnBounds = tl(defnBounds);
    varsBounds = tl(varsBounds);
    depends    = tl(depends);
}

static Void local markAssumList(as)    /* Mark all types in assumption set */
List as; {			       /* :: [(Var, Type)]		   */
    for (; nonNull(as); as=tl(as))     /* No need to mark generic types;   */
	if (!isPolyType(snd(hd(as))))  /* the only free variables in those */
	    markType(snd(hd(as)),0);   /* must have been free earlier too  */
}

static Cell local findAssum(t)	       /* Find most recent assumption about*/
Text t; {			       /* variable named t, if any	   */
    List defnBounds1 = defnBounds;     /* return translated variable, with */
    List varsBounds1 = varsBounds;     /* type in typeIs		   */
    List depends1    = depends;

    while (nonNull(defnBounds1)) {
	Pair ass = findInAssumList(t,hd(varsBounds1));/* search varsBounds */
	if (nonNull(ass)) {
	    typeIs = snd(ass);
	    return fst(ass);
	}

	ass = findInAssumList(t,hd(defnBounds1));     /* search defnBounds */
	if (nonNull(ass)) {
	    Cell v = fst(ass);
            typeIs = snd(ass);

	    if (hd(depends1)!=NODEPENDS &&	      /* save dependent?   */
		  isNull(v=varIsMember(t,hd(depends1))))
		/* N.B. make new copy of variable and store this on list of*/
		/* dependents, and in the assumption so that all uses of   */
		/* the variable will be at the same node, if we need to    */
		/* overwrite the call of a function with a translation...  */
		hd(depends1) = cons(v=mkVar(t),hd(depends1));

	    return v;
	}

	defnBounds1 = tl(defnBounds1);		      /* look in next level*/
	varsBounds1 = tl(varsBounds1);		      /* of assumption set */
	depends1    = tl(depends1);
    }
    return NIL;
}

static Pair local findInAssumList(t,as)/* Search for assumption for var    */
Text t;				       /* named t in list of assumptions as*/
List as; {
    for (; nonNull(as); as=tl(as))
	if (textOf(fst(hd(as)))==t)
	    return hd(as);
    return NIL;
}

#define findTopBinding(v)  findInAssumList(textOf(v),hd(defnBounds))

static Int local newVarsBind(v)        /* make new assump for pattern var  */
Cell v; {
    Int beta	   = newTyvars(1);
    hd(varsBounds) = cons(pair(v,mkInt(beta)), hd(varsBounds));
#ifdef DEBUG_TYPES
    printf("variable, assume ");
    printExp(stdout,v);
    printf(" :: _%d\n",beta);
#endif
    return beta;
}

static Void local newDefnBind(v,type)  /* make new assump for defn var	   */
Cell v; 			       /* and set type if given (nonNull)  */
Type type; {
    Int beta	   = newTyvars(1);
    hd(defnBounds) = cons(pair(v,mkInt(beta)), hd(defnBounds));
    instantiate(type);
#ifdef DEBUG_TYPES
    printf("definition, assume ");
    printExp(stdout,v);
    printf(" :: _%d\n",beta);
#endif
    bindTv(beta,typeIs,typeOff);       /* Bind beta to new type skeleton   */
}

static Void local instantiate(type)    /* instantiate type expr, if nonNull*/
Type type; {
    predsAre = NIL;
    typeIs   = type;
    typeOff  = 0;

    if (nonNull(typeIs)) {	       /* instantiate type expression ?    */

	if (isPolyType(typeIs)) {      /* Polymorphic type scheme ?	   */
	    typeOff = newKindedVars(polySigOf(typeIs));
	    typeIs  = monoTypeOf(typeIs);
	}

	if (whatIs(typeIs)==QUAL) {    /* Qualified type?		   */
	    predsAre = fst(snd(typeIs));
	    typeIs   = snd(snd(typeIs));
	}
    }
}

/* --------------------------------------------------------------------------
 * Predicate sets:
 *
 * A predicate set is represented by a list of triples (C t, o, used)
 * which indicates that type (t,o) must be an instance of class C, with
 * evidence required at the node pointed to by used.  Note that the `used'
 * node may need to be overwritten at a later stage if this evidence is
 * to be derived from some other predicates by entailment.
 * ------------------------------------------------------------------------*/

#include "preds.c"

/* --------------------------------------------------------------------------
 * Type errors:
 * ------------------------------------------------------------------------*/

static Void local typeError(l,e,in,wh,t,o)
Int    l;			      /* line number near type error	   */
String wh;			      /* place in which error occurs	   */
Cell   e;			      /* source of error		   */
Cell   in;			      /* context if any (NIL if not)	   */
Type   t;			      /* should be of type (t,o)	   */
Int    o; {			      /* type inferred is (typeIs,typeOff) */

    clearMarks();		      /* types printed here are monotypes  */
				      /* use marking to give sensible names*/
#ifdef DEBUG_KINDS
{ List vs = genericVars;
  for (; nonNull(vs); vs=tl(vs)) {
     Int v = intOf(hd(vs));
     printf("%c :: ", ('a'+tyvar(v)->offs));
     printKind(stdout,tyvar(v)->kind);
     putchar('\n');
  }
}
#endif

    reportTypeError(l,e,in,wh,copyType(typeIs,typeOff),copyType(t,o));
}

static Void local reportTypeError(l,e,in,wh,inft,expt)
Int    l;				/* error printing part of typeError*/
Cell   e, in;				/* separated out for the benefit of*/
String wh;				/* typing runST			   */
Type   inft, expt; {
    ERROR(l) "Type error in %s", wh   ETHEN
    if (nonNull(in)) {
	ERRTEXT "\n*** expression     : " ETHEN ERREXPR(in);
    }
    ERRTEXT "\n*** term           : " ETHEN ERREXPR(e);
    ERRTEXT "\n*** type           : " ETHEN ERRTYPE(inft);
    ERRTEXT "\n*** does not match : " ETHEN ERRTYPE(expt);
    if (unifyFails) {
	ERRTEXT "\n*** because        : %s", unifyFails ETHEN
    }
    ERRTEXT "\n"
    EEND;
}

#define shouldBe(l,e,in,where,t,o) if (!unify(typeIs,typeOff,t,o)) \
				       typeError(l,e,in,where,t,o);
#define check(l,e,in,where,t,o)    e=typeExpr(l,e); shouldBe(l,e,in,where,t,o)
#define inferType(t,o)		   typeIs=t; typeOff=o

/* --------------------------------------------------------------------------
 * Typing of expressions:
 * ------------------------------------------------------------------------*/

#define EXPRESSION  0			/* type checking expression	   */
#define NEW_PATTERN 1			/* pattern, introducing new vars   */
#define OLD_PATTERN 2			/* pattern, involving bound vars   */
static int tcMode = EXPRESSION;

#ifdef DEBUG_TYPES
static Cell local mytypeExpr	Args((Int,Cell));
static Cell local typeExpr(l,e)
Int l;
Cell e; {
    static int number = 0;
    Cell   retv;
    int    mynumber   = number++;
    printf("%d) to check: ",mynumber);
    printExp(stdout,e);
    putchar('\n');
    retv = mytypeExpr(l,e);
    printf("%d) result: ",mynumber);
    printType(stdout,debugType(typeIs,typeOff));
    putchar('\n');
    return retv;
}
static Cell local mytypeExpr(l,e)	/* Determine type of expr/pattern  */
#else
static Cell local typeExpr(l,e)		/* Determine type of expr/pattern  */
#endif
Int  l;
Cell e; {
    static String cond	= "conditional";
    static String list	= "list";
    static String discr = "case discriminant";
    static String aspat = "as (@) pattern";

    switch (whatIs(e)) {

	/* The following cases can occur in either pattern or expr. mode   */

	case AP 	: return typeAp(l,e);

	case NAME	: if (isNull(name(e).type))
			      internal("typeExpr1");
			  else {
			      Cell tt = varIntro(e,name(e).type);
			      return (name(e).defn==CFUN) ? e : tt;
			  }

	case TUPLE	: typeTuple(e);
			  break;

	case INTCELL	: if (tcMode==EXPRESSION && coerceNumLiterals
						 && nonNull(predNum)) {
			      Int alpha = newTyvars(1);
			      inferType(var,alpha);
			      return ap(ap(nameFromInt,
					   assumeEvid(predNum,alpha)),
					   e);
			  }
			  else {
			      inferType(typeInt,0);
			  }
			  break;

	case FLOATCELL  : inferType(typeFloat,0);
			  break;

	case STRCELL	: inferType(typeString,0);
			  break;

	case UNIT	: inferType(typeUnit,0);
			  break;

	case CHARCELL	: inferType(typeChar,0);
			  break;

	case VAROPCELL	:
	case VARIDCELL	: if (tcMode!=NEW_PATTERN) {
			      Cell a = findAssum(textOf(e));
			      if (nonNull(a))
				  return varIntro(a,typeIs);
			      else {
				   a = findName(textOf(e));
				   if (isNull(a) || isNull(name(a).type))
				       internal("typeExpr2");
				   return varIntro(a,name(a).type);
			      }
			  }
			  else {
			      inferType(var,newVarsBind(e));
			  }
			  break;

	/* The following cases can only occur in expr mode		   */

	case COND	: {   Int beta = newTyvars(1);
			      check(l,fst3(snd(e)),e,cond,typeBool,0);
			      check(l,snd3(snd(e)),e,cond,var,beta);
			      check(l,thd3(snd(e)),e,cond,var,beta);
			      tyvarType(beta);
			  }
			  break;

	case LETREC	: enterBindings();
			  mapProc(typeBindings,fst(snd(e)));
			  snd(snd(e)) = typeExpr(l,snd(snd(e)));
			  leaveBindings();
			  break;

	case FINLIST	: if (tcMode==EXPRESSION && nonNull(nameResult)
						 && isNull(tl(snd(e)))
						 && overSingleton)
			     typeMonadComp(l,e);
			  else {
			      Int  beta = newTyvars(1);
			      List xs;
			      for (xs=snd(e); nonNull(xs); xs=tl(xs)) {
				 check(l,hd(xs),e,list,var,beta);
			      }
			      inferType(typeList,beta);
			  }
			  break;

#if DO_COMPS
	case DOCOMP	: if (isNull(nameResult)) {
			      ERROR(l)
				 "Prelude does not support do {...} notation"
			      EEND;
			  }
			  typeMonadComp(l,e);
			  break;
#endif

	case COMP	: if (nonNull(nameResult))
			      typeMonadComp(l,e);
			  else {
			      Int beta = newTyvars(1);
                              typeComp(l,typeList,snd(e),snd(snd(e)));
			      bindTv(beta,typeIs,typeOff);
			      inferType(typeList,beta);
			      fst(e) = LISTCOMP;
			  }
			  break;

#if IO_MONAD
	case RUNST	: {   Int beta = newTyvars(2);
			      static String enc = "encapsulation";
			      check(l,snd(e),e,enc,typeSTab,beta);
			      clearMarks();
			      mapProc(markAssumList,defnBounds);
			      mapProc(markAssumList,varsBounds);
			      mapProc(markPred,preds);
			      markTyvar(beta+1);
			      tyvarType(beta);
			      if (typeIs!=var
				   || tyvar(typeOff)->offs==FIXED_TYVAR) {
				  Int alpha = newTyvars(2);
				  bindTv(alpha+1,var,beta+1);
				  reportTypeError(l,snd(e),e,enc,
						  copyType(typeSTab,beta),
						  copyType(typeSTab,alpha));
			      }
			      tyvarType(beta+1);
			  }
			  break;
#endif

	case ESIGN	: typeEsign(l,e);
			  return fst(snd(e));

	case CASE	: {    Int beta = newTyvars(2);    /* discr result */
			       check(l,fst(snd(e)),NIL,discr,var,beta);
			       map2Proc(typeCase,l,beta,snd(snd(e)));
			       tyvarType(beta+1);
			  }
			  break;

	case LAMBDA	: typeAlt(snd(e));
			  break;

	/* The remaining cases can only occur in pattern mode: */

	case WILDCARD	: inferType(var,newTyvars(1));
			  break;

	case ASPAT	: {   Int beta = newTyvars(1);
			      snd(snd(e)) = typeExpr(l,snd(snd(e)));
			      bindTv(beta,typeIs,typeOff);
			      check(l,fst(snd(e)),e,aspat,var,beta);
			      tyvarType(beta);
			  }
			  break;

	case LAZYPAT	: snd(e) = typeExpr(l,snd(e));
			  break;

#if NPLUSK
	case ADDPAT	:
	case MULPAT	: inferType(typeIntToInt,0);
			  break;
#endif

	default 	: internal("typeExpr3");
   }

   return e;
}

static Cell local varIntro(v,type)	/* make translation of var v with  */
Cell v;					/* given type adding any extra dict*/
Type type; {				/* params required		   */
    /* N.B. In practice, v will either be a NAME or a VARID/OPCELL	   */
    for (instantiate(type); nonNull(predsAre); predsAre=tl(predsAre))
	v = ap(v,assumeEvid(hd(predsAre),typeOff));
    return v;
}

static Void local typeEsign(l,e)	/* Type check expression type sig  */
Int  l;
Cell e; {
    static String typeSig = "type signature expression";
    List savePreds = preds;
    Int  alpha 	   = newTyvars(1);
    List expPreds;			/* explicit preds in type sig	   */
    List qs;				/* qualifying preds in infered type*/
    Type nt;				/* complete infered type	   */

    preds = NIL;
    instantiate(snd(snd(e)));
    bindTv(alpha,typeIs,typeOff);
    expPreds = makeEvidArgs(predsAre,typeOff);
    check(l,fst(snd(e)),NIL,typeSig,var,alpha);

    clearMarks();
    mapProc(markAssumList,defnBounds);
    mapProc(markAssumList,varsBounds);
    mapProc(markPred,savePreds);

    savePreds = elimConstPreds(l,typeSig,e,savePreds);

    explicitProve(l,typeSig,fst(snd(e)),expPreds,preds);

    resetGenericsFrom(0);
    qs = copyPreds(expPreds);
    nt = generalise(qs,copyTyvar(alpha));

    if (!equalSchemes(nt,snd(snd(e))))
	tooGeneral(l,fst(snd(e)),snd(snd(e)),nt);

    tyvarType(alpha);
    preds = revOnto(expPreds,savePreds);
}

static Void local typeCase(l,beta,c)   /* type check case: pat -> rhs	   */
Int  l; 			       /* (case given by c == (pat,rhs))   */
Int  beta;			       /* need:  pat :: (var,beta)	   */
Cell c; {			       /*	 rhs :: (var,beta+1)	   */
    static String casePat  = "case pattern";
    static String caseExpr = "case expression";

    saveVarsAssump();

    fst(c) = typeFreshPat(l,fst(c));
    shouldBe(l,fst(c),NIL,casePat,var,beta);
    snd(c) = typeRhs(snd(c));
    shouldBe(l,rhsExpr(snd(c)),NIL,caseExpr,var,beta+1);

    restoreVarsAss();
}

static Void local typeComp(l,m,e,qs)	/* type check comprehension	   */
Int  l;
Type m;					/* monad (mkOffset(0))		   */
Cell e;
List qs; {
    static String boolQual = "boolean qualifier";
    static String genQual  = "generator";

    if (isNull(qs))			/* no qualifiers left		   */
	fst(e) = typeExpr(l,fst(e));
    else {
	Cell q   = hd(qs);
	List qs1 = tl(qs);
	switch (whatIs(q)) {
	    case BOOLQUAL : check(l,snd(q),NIL,boolQual,typeBool,0);
			    typeComp(l,m,e,qs1);
			    break;

	    case QWHERE   : enterBindings();
			    mapProc(typeBindings,snd(q));
			    typeComp(l,m,e,qs1);
			    leaveBindings();
			    break;

	    case FROMQUAL : {   Int beta = newTyvars(1);
				saveVarsAssump();
                                check(l,snd(snd(q)),NIL,genQual,m,beta);
				fst(snd(q)) = typeFreshPat(l,fst(snd(q)));
				shouldBe(l,fst(snd(q)),NIL,genQual,var,beta);
				typeComp(l,m,e,qs1);
				restoreVarsAss();
			    }
			    break;

	    case DOQUAL   : check(l,snd(q),NIL,genQual,m,newTyvars(1));
			    typeComp(l,m,e,qs1);
			    break;
	}
    }
}

static Void local typeMonadComp(l,e)	/* type check a monad comprehension*/
Int  l;
Cell e; {
    Int  alpha = newTyvars(1);
    Int  beta  = newKindedVars(monadSig);
    Cell mon   = ap(mkInt(beta),var);
    Cell m     = assumeEvid(predMonad,beta);
    typeComp(l,mon,snd(e),snd(snd(e)));
#if DO_COMPS
    if (fst(e)==DOCOMP) {
	static String finGen = "final generator";
	shouldBe(l,fst(snd(e)),NIL,finGen,mon,alpha);
    }
    else
#endif
    {
	bindTv(alpha,typeIs,typeOff);
	inferType(mon,alpha);
	fst(e) = MONADCOMP;
    }
    snd(e) = pair(pair(m,compZero(snd(snd(e)),beta)),snd(e));
}

static Cell local compZero(qs,beta)	/* return evidence for Monad0 beta */
List qs;				/* if needed for qualifiers qs	   */
Int  beta; {
    for (; nonNull(qs); qs=tl(qs))
	switch (whatIs(hd(qs))) {
	    case FROMQUAL : if (!refutable(fst(snd(hd(qs)))))
				break;
			    /* intentional fall-thru */
	    case BOOLQUAL : return assumeEvid(predMonad0,beta);
	}
    return NIL;
}

static Cell local typeFreshPat(l,p)    /* find type of pattern, assigning  */
Int  l; 			       /* fresh type variables to each var */
Cell p; {			       /* bound in the pattern		   */
    tcMode = NEW_PATTERN;
    p	   = typeExpr(l,p);
    tcMode = EXPRESSION;
    return p;
}

/* --------------------------------------------------------------------------
 * Note the pleasing duality in the typing of application and abstraction:-)
 * ------------------------------------------------------------------------*/

static Cell local typeAp(l,e)		/* Type check application	   */
Int  l;
Cell e; {
    static String app = "application";
    Cell h    = getHead(e);		/* e = h e1 e2 ... en		   */
    Int  n    = argCount;		/* save no. of arguments	   */
    Int  beta = funcType(n);
    Cell p    = NIL;			/* points to previous AP node	   */
    Cell a    = e;			/* points to current AP node	   */
    Int  i;

    check(l,h,e,app,var,beta);		/* check h::t1->t2->...->tn->rn+1  */
    for (i=n; i>0; --i) {		/* check e_i::t_i for each i	   */
	check(l,arg(a),e,app,var,beta+2*i-1);
	p = a;
	a = fun(a);
    }
    fun(p) = h;				/* replace head with translation   */
    tyvarType(beta+2*n);		/* inferred type is r_n+1	   */
    return e;
}

static Void local typeAlt(a)		/* Type check abstraction (Alt)	   */
Cell a; {				/* a = ( [p1, ..., pn], rhs )	   */
    List ps	  = fst(a);
    Int  n	  = length(ps);
    Int  beta	  = funcType(n);
    Int  l	  = rhsLine(snd(a));
    Int  i;

    saveVarsAssump();

    for (i=0; i<n; ++i) {
	hd(ps) = typeFreshPat(l,hd(ps));
	bindTv(beta+2*i+1,typeIs,typeOff);
	ps = tl(ps);
    }
    snd(a) = typeRhs(snd(a));
    bindTv(beta+2*n,typeIs,typeOff);
    tyvarType(beta);

    restoreVarsAss();
}

static Int local funcType(n)		/*return skeleton for function type*/
Int n; {				/*with n arguments, taking the form*/
    Int beta = newTyvars(2*n+1);	/*    r1 t1 r2 t2 ... rn tn rn+1   */
    Int i;				/* with r_i := t_i -> r_i+1	   */
    for (i=0; i<n; ++i)
	bindTv(beta+2*i,arrow,beta+2*i+1);
    return beta;
}

/* --------------------------------------------------------------------------
 * Tuple type constructors: are generated as necessary.  The most common
 * n-tuple constructors (n<MAXTUPCON) are held in a cache to avoid
 * repeated generation of the constructor types.
 *
 * ???Maybe this cache should extend to all valid tuple constrs???
 * ------------------------------------------------------------------------*/

#define MAXTUPCON 10
static Type tupleConTypes[MAXTUPCON];

static Void local typeTuple(e)	       /* find type for tuple constr, using*/
Cell e; {			       /* tupleConTypes to cache previously*/
    Int n   = tupleOf(e);	       /* calculated tuple constr. types.  */
    typeOff = newTyvars(n);
    if (n>=MAXTUPCON)
	 typeIs = makeTupleType(n);
    else if (tupleConTypes[n])
	 typeIs = tupleConTypes[n];
    else
	 typeIs = tupleConTypes[n] = makeTupleType(n);
}

static Type local makeTupleType(n)     /* construct type for tuple constr. */
Int n; {			       /* t1 -> ... -> tn -> (t1,...,tn)   */
    Type h = mkTuple(n);
    Int  i;

    for (i=0; i<n; ++i)
	h = ap(h,mkOffset(i));
    while (0<n--)
	h = fn(mkOffset(n),h);
    return h;
}

/* --------------------------------------------------------------------------
 * Type check group of bindings:
 * ------------------------------------------------------------------------*/

static Void local typeBindings(bs)     /* type check a single binding group*/
List bs; {
    Bool usesPatternBindings = FALSE;
    Bool usesSimplePatterns  = FALSE;
    Bool usesTypeSigs = FALSE;
    List bs1;

    for (bs1=bs; nonNull(bs1); bs1=tl(bs1)) {  /* Analyse binding group    */
	Cell b = hd(bs1);
	if (!isVar(fst(b)))
	    usesPatternBindings = TRUE;
	else if (isNull(fst(hd(snd(snd(b))))))
	    usesSimplePatterns = TRUE;

	if (nonNull(fst(snd(b))))	       /* any explicitly typed	   */
	    usesTypeSigs = TRUE;	       /* bindings in group?	   */
    }

    hd(defnBounds) = NIL;
    hd(depends)	   = NIL;

    if (usesPatternBindings || (usesSimplePatterns && !usesTypeSigs))
	noOverloading(bs);
    else if (usesTypeSigs)
	explicitTyping(bs);
    else
	implicitTyping(bs);

    mapProc(checkBindSigs,bs);		       /* compare with sig decls   */
    mapProc(removeTypeSigs,bs);		       /* Remove binding type info */

    hd(varsBounds) = revOnto(hd(defnBounds),   /* transfer completed assmps*/
			     hd(varsBounds));  /* out of defnBounds        */
    hd(defnBounds) = NIL;
    hd(depends)    = NIL;
}

static Void local removeTypeSigs(b)    /* Remove type info from a binding  */
Cell b; {
    snd(b) = snd(snd(b));
}

/* --------------------------------------------------------------------------
 * Restricted binding group:
 * ------------------------------------------------------------------------*/

static Void local noOverloading(bs)    /* Type restricted binding group    */
List bs; {
    List savePreds = preds;
    Cell v;
    Int  line;

    hd(depends) = NODEPENDS;	       /* No need for dependents here	   */
    preds       = NIL;

    mapProc(restrictedBindAss,bs);     /* add assumptions for vars in bs   */
    mapProc(typeBind,bs);	       /* type check each binding	   */

    clearMarks();		       /* mark fixed variables		   */
    mapProc(markAssumList,tl(defnBounds));
    mapProc(markAssumList,tl(varsBounds));
    mapProc(markPred,preds);

    if (isVar(v=fst(hd(bs))))
        line = rhsLine(snd(hd(snd(snd(hd(bs))))));
    else {
	line = rhsLine(snd(snd(snd(hd(bs)))));
	v    = hd(v);
    }
    savePreds = elimConstPreds(line,"binding group",v,savePreds);
    preds     = appendOnto(preds,savePreds);

    map2Proc(generaliseBind,0,NIL,bs); /* Generalise types of defined vars */
}

static Void local restrictedBindAss(b) /* make assums for vars in binding  */
Cell b; {			       /* gp with restricted overloading   */

    if (isVar(fst(b)))		       /* function-binding?		   */
	restrictedAss(intOf(rhsLine(snd(hd(snd(snd(b)))))),
		      fst(b),
		      fst(snd(b)));
    else {			       /* pattern-binding?		   */
	List vs   = fst(b);
	List ts   = fst(snd(b));
	Int  line = rhsLine(snd(snd(b)));

	for (; nonNull(vs); vs=tl(vs))
	    if (nonNull(ts)) {
		restrictedAss(line,hd(vs),hd(ts));
		ts = tl(ts);
	    }
	    else
		restrictedAss(line,hd(vs),NIL);
    }
}

static Void local restrictedAss(l,v,t) /* Assume that type of binding var v*/
Int  l; 			       /* is t (if nonNull) in restricted  */
Cell v; 			       /* binding group 		   */
Type t; {
    newDefnBind(v,t);
    if (nonNull(predsAre)) {
	ERROR(l) "Explicit overloaded type for \"%s\"",textToStr(textOf(v))
	ETHEN
	ERRTEXT  " not permitted in restricted binding"
	EEND;
    }
}

/* --------------------------------------------------------------------------
 * Type unrestricted binding group with explicitly declared types:
 * ------------------------------------------------------------------------*/

static Void local explicitTyping(bs)
List bs; {
    static String expBinds = "binding group";
    List savePreds  = preds;
    List evidParams = NIL;
    List locPreds   = NIL;
    List locDeps    = NIL;
    List bs1;
    List lps;
    List eps;
    Int  ng;

    preds = NIL;

    for (bs1=bs; nonNull(bs1); bs1=tl(bs1)) {   /* Add assumptions about   */
	Cell b = hd(bs1);			/* each bound var -- can   */
	newDefnBind(fst(b),fst(snd(b)));	/* assume function binding */
	if (nonNull(typeIs))
	    evidParams = cons(makeEvidArgs(predsAre,typeOff),evidParams);
    }
    evidParams	= rev(evidParams);

    for (bs1=bs; nonNull(bs1); bs1=tl(bs1))	/* Type implicitly-typed   */
	if (isNull(fst(snd(hd(bs1)))))		/* function bindings	   */
	    typeBind(hd(bs1));

    for (bs1=bs; nonNull(bs1); bs1=tl(bs1))	/* Type explicitly-typed   */
	if (nonNull(fst(snd(hd(bs1))))) {	/* binding and save local  */
	    typeBind(hd(bs1));			/* dependents and preds	   */
	    locPreds	= cons(preds,locPreds);
	    locDeps	= cons(hd(depends),locDeps);
	    preds	= NIL;
	    hd(depends) = NIL;
	}
    locPreds = rev(locPreds);
    locDeps  = rev(locDeps);

    /* ----------------------------------------------------------------------
     * At this point:
     *
     * bs         = group of bindings being typechecked
     * evidParams = list of explicit evidence parameters used in each
     *		    explicitly typed binding in bs, arranged in the order
     *		    that the explicitly typed bindings appear in bs.
     *		    The first element of evidParams is also used as the
     *		    explicit evidence parameters for any implicitly typed
     *		    bindings in the group.
     * locPreds   = list of predicates required in the body of each
     *		    explicitly typed binding in bs (arranged in the same
     *		    order as evidParams.  Once again, the first element of
     *		    locPreds also includes the predicates for the implicitly
     *		    typed bindings in bs.
     * locDeps    = list of immediate dependents of each binding within the
     *		    binding group bs.  Each of these variables must be
     *		    overwritten with an expression in which the variable is
     *		    applied to appropriate evidence parameters, as reqd by
     *		    the corresponding element of evidParams.
     * --------------------------------------------------------------------*/

    clearMarks();				/* Mark fixed variables	   */
    mapProc(markAssumList,tl(defnBounds));
    mapProc(markAssumList,tl(varsBounds));
    mapProc(markPred,savePreds);

    bs1 = gotoExplicit(bs);
    eps = evidParams;
    lps = locPreds;
    while (nonNull(eps)) {
	Cell b    = hd(bs1);
	Int  line = rhsLine(snd(hd(snd(snd(b)))));
	List dps;

	preds     = hd(lps);
	savePreds = elimConstPreds(line,expBinds,fst(b),savePreds);

	explicitProve(line,expBinds,fst(b),hd(eps),preds);

        for (dps=hd(locDeps); nonNull(dps); dps=tl(dps)) {
	    Cell f      = hd(dps);
	    Cell fQuals = explPreds(textOf(f),bs,evidParams);
 
	    if (nonNull(fQuals))
                overwrite(f,
			  addEvidArgs(line,
				      expBinds,
				      fst(b),
				      hd(eps),
				      fQuals,
				      mkVar(textOf(f))));
	}

	eps       = tl(eps);
	bs1       = gotoExplicit(tl(bs1));
	lps       = tl(lps);
	locDeps   = tl(locDeps);
    }

    eps = evidParams;				/* add extra dict params   */
    for (bs1=bs; nonNull(bs1); bs1=tl(bs1)) {	/* to each binding in bs   */
	Cell b = hd(bs1);

	if (nonNull(fst(snd(b)))) {
	    qualifyBinding(hd(eps),b);
	    eps = tl(eps);
	}
	else
	    qualifyBinding(hd(evidParams),b);
    }

    resetGenericsFrom(0);			/* Infer typing for each   */
    eps = copyPreds(hd(evidParams));		/* binding ....		   */
    ng  = nextGeneric;

    for (bs1=bs; nonNull(bs1); bs1=tl(bs1))	/* Start with implicitly   */
	if (isNull(fst(snd(hd(bs1)))))		/* typed bindings	   */
	    generaliseBind(ng,eps,hd(bs1));

    bs1 = gotoExplicit(bs);			/* Then first explicitly   */
    generaliseBind(ng,eps,hd(bs1));		/* typed binding	   */

    while (nonNull(bs1=gotoExplicit(tl(bs1)))) {/* followed by remaining   */
	evidParams = tl(evidParams);		/* explicitly typed bndings*/
        resetGenericsFrom(0);
	eps        = copyPreds(hd(evidParams));
	ng	   = nextGeneric;
	generaliseBind(ng,eps,hd(bs1));
    }

    preds = savePreds;				/* restore saved predicates*/
}

static List local gotoExplicit(bs)      /* skip through list of bindings   */
List bs; {				/* upto first explicit binding	   */
    while (nonNull(bs) && isNull(fst(snd(hd(bs)))))
	bs = tl(bs);
    return bs;
}

static List local explPreds(t,bs,locps)	/* look up explicit preds for t	in */
Text t;         			/* bindings bs with locps listing  */
List bs;				/* explicit type preds, implicit   */
List locps; {				/* included in hd(locps)	   */
    List lps = locps;

    for (; nonNull(bs); bs=tl(bs)) {
	Cell b = hd(bs);

	if (textOf(fst(b))==t)
	    if (isNull(fst(snd(b))))
		return hd(locps);
	    else
		return hd(lps);

	if (nonNull(fst(snd(b))))
	    lps = tl(lps);
    }
    internal("explPreds");
    return NIL; /*NOTREACHED*/
}

/* --------------------------------------------------------------------------
 * Type unrestricted binding group with no explicitly declared types:
 * ------------------------------------------------------------------------*/

static Void local implicitTyping(bs)
List bs; {
    static String impBinds = "implicitly typed binding group";
    Int  line      = rhsLine(snd(hd(snd(snd(hd(bs))))));
    Int  ng;
    List qs;
    List savePreds = preds;			/* Save and clear preds	   */
    preds	   = NIL;

#define addImplicit(b) newDefnBind(fst(b),NIL)	/* Add assumption for each */
    mapProc(addImplicit,bs);			/* variable defined in bs  */
#undef  addImplicit

    mapProc(typeBind,bs);			/* Type check each binding */

    clearMarks();				/* Mark fixed variables	   */
    mapProc(markAssumList,tl(defnBounds));
    mapProc(markAssumList,tl(varsBounds));
    mapProc(markPred,savePreds);

    savePreds = elimConstPreds(line,
			       impBinds,
			       fst(hd(bs)),
			       savePreds);	/* remove (loc) const preds*/
    preds     = simplify(preds);		/* simplify remaining preds*/
    if (nonNull(preds)) {
	map1Proc(addEvidParams,preds,hd(depends));
	map1Proc(qualifyBinding,preds,bs);
    }
    resetGenericsFrom(0);
    qs = copyPreds(preds);
    ng = nextGeneric;
    map2Proc(generaliseBind,ng,qs,bs);		/* find defn var types     */

    preds = savePreds;				/* restore predicates	   */
}

static Void local addEvidParams(qs,v)  /* overwrite VARID/OPCELL v with	   */
List qs;			       /* application of variable to evid. */
Cell v; {			       /* parameters given by qs	   */
    if (nonNull(qs)) {
	Cell nv;

	if (!isVar(v))
	    internal("addEvidParams");

	for (nv=mkVar(textOf(v)); nonNull(tl(qs)); qs=tl(qs))
	    nv = ap(nv,thd3(hd(qs)));
	fst(v) = nv;
	snd(v) = thd3(hd(qs));
    }
}

/* --------------------------------------------------------------------------
 * Type check bodies of class and instance declarations:
 * ------------------------------------------------------------------------*/

static Void local typeInstDefn(in)	/* type check implementations of   */
Inst in; {				/* member functions for instance in*/
    typeMembers("instance member binding",
		class(inst(in).cl).members,
		inst(in).implements,
		inst(in).head,
		inst(in).sig);
}

static Void local typeClassDefn(c)	/* type check implementations of   */
Class c; {				/* defaults for class c		   */
    typeMembers("default member binding",
		class(c).members,
		class(c).defaults,
		class(c).head,
		class(c).sig);
}

static Void local typeMembers(wh,ms,is,pi,ar)/* type check implementations */
String wh;				     /* `is' of members `ms' in    */
List   ms;				     /* class at instance `t' where*/
List   is;				     /* arity = #vars in t	   */
Cell   pi;
Kind   ar; {
    while (nonNull(is)) {
	if (isName(hd(is)))
	    typeMember(wh,hd(ms),hd(is),pi,ar);
	is = tl(is);
	ms = tl(ms);
    }
}

static Void local typeMember(wh,m,i,pi,ar)   /* type check implementation i*/
String wh;				     /* of member m at instance t  */
Name   m;				     /* where ar = sig of vars in t*/
Name   i;
Cell   pi;
Kind   ar; {
    Int  line = rhsLine(snd(hd(name(i).defn)));
    Int  alpha, beta;
    Type rt = NIL;				/* required type	   */
    Type it = NIL;				/* inferred type	   */
    List evid;					/* evidence assignment	   */
    List qs;					/* predicate list	   */

    emptySubstitution();
    hd(defnBounds) = NIL;
    hd(depends)    = NODEPENDS;
    preds	   = NIL;

    alpha = newTyvars(1);			/* record expected type	   */
    beta  = newKindedVars(ar);
    instantiate(name(m).type);
    bindTv(alpha,typeIs,typeOff);
    if (isNull(predsAre) || !oneWayMatches(hd(predsAre),typeOff,pi,beta))
	internal("typeMember1");
    evid = makeEvidArgs(predsAre,typeOff);

    resetGenericsFrom(0);			/* Set required type, rt   */
    qs = copyPreds(evid);
    rt = generalise(qs,copyTyvar(alpha));

    map2Proc(typeDefAlt,alpha,m,name(i).defn);	/* Type each alt in defn   */

    clearMarks();
    if (nonNull(elimConstPreds(line,wh,m,NIL)))	/* need to resolve constant*/
	internal("typeMember2");		/* overloading - shouldn't */
						/* be any locally constant */
						/* overloading at all!	   */

    explicitProve(line,wh,m,evid,preds);	/* resolve remaining preds */

    resetGenericsFrom(0);			/* Determine inferred type */
    qs = copyPreds(evid);
    it = generalise(qs,copyTyvar(alpha));

    if (!equalSchemes(rt,it))			/* check inferred type ok  */
	tooGeneral(line,m,rt,it);

    map1Proc(qualify,evid,name(i).defn);	/* add dictionary parameter*/

    overDefns = cons(i,overDefns);
}

/* --------------------------------------------------------------------------
 * Type check bodies of bindings:
 * ------------------------------------------------------------------------*/

static Void local typeBind(b)	       /* Type check binding		   */
Cell b; {
    if (isVar(fst(b))) {			       /* function binding */
	Cell ass = findTopBinding(fst(b));
	Int  beta;

	if (isNull(ass) || !isInt(snd(ass)))
	    internal("typeBind");

	beta = intOf(snd(ass));
	map2Proc(typeDefAlt,beta,fst(b),snd(snd(b)));
    }
    else {					       /* pattern binding  */
	static String lhsPat = "lhs pattern";
	static String rhs    = "right hand side";
	Int  beta	     = newTyvars(1);
	Pair pb		     = snd(snd(b));
	Int  l		     = rhsLine(snd(pb));

	tcMode  = OLD_PATTERN;
	check(l,fst(pb),NIL,lhsPat,var,beta);
	tcMode  = EXPRESSION;
	snd(pb) = typeRhs(snd(pb));
	shouldBe(l,rhsExpr(snd(pb)),NIL,rhs,var,beta);
    }
}

static Void local typeDefAlt(beta,v,a) /* type check alt in func. binding  */
Int  beta;
Cell v;
Pair a; {
    static String valDef = "function binding";
    Int l		 = rhsLine(snd(a));
    typeAlt(a);
    shouldBe(l,v,NIL,valDef,var,beta);
}

static Cell local typeRhs(e)	       /* check type of rhs of definition  */
Cell e; {
    switch (whatIs(e)) {
	case GUARDED : {   Int beta = newTyvars(1);
			   map1Proc(guardedType,beta,snd(e));
			   tyvarType(beta);
		       }
		       break;

	case LETREC  : enterBindings();
		       mapProc(typeBindings,fst(snd(e)));
		       snd(snd(e)) = typeRhs(snd(snd(e)));
		       leaveBindings();
		       break;

	default      : snd(e) = typeExpr(intOf(fst(e)),snd(e));
		       break;
    }
    return e;
}

static Void local guardedType(beta,gded)/* check type of guard (li,(gd,ex))*/
Int  beta;			       /* should have gd :: Bool,	   */
Cell gded; {			       /*	      ex :: (var,beta)	   */
    static String guarded = "guarded expression";
    static String guard   = "guard";
    Int line = intOf(fst(gded));

    gded     = snd(gded);
    check(line,fst(gded),NIL,guard,typeBool,0);
    check(line,snd(gded),NIL,guarded,var,beta);
}

Cell rhsExpr(rhs)		       /* find first expression on a rhs   */
Cell rhs; {
    switch (whatIs(rhs)) {
	case GUARDED : return snd(snd(hd(snd(rhs))));
	case LETREC  : return rhsExpr(snd(snd(rhs)));
	default      : return snd(rhs);
    }
}

Int rhsLine(rhs)		       /* find line number associated with */
Cell rhs; {			       /* a right hand side		   */
    switch (whatIs(rhs)) {
	case GUARDED : return intOf(fst(hd(snd(rhs))));
	case LETREC  : return rhsLine(snd(snd(rhs)));
	default      : return intOf(fst(rhs));
    }
}

/* --------------------------------------------------------------------------
 * Calculate generalisation of types:
 * ------------------------------------------------------------------------*/

static Void local generaliseBind(ng,qs,b)
Int  ng;                               /* generalise the types of each var */
List qs;			       /* defined in binding, qualifying   */
Cell b; {			       /* with predicates in qs		   */
    if (isVar(fst(b)))		       /* Assumes fixed vars already marked*/
	generaliseAss(ng,qs,fst(b));   /* with first ng generics used in qs*/
    else {
	map2Proc(generaliseAss,ng,qs,fst(b));
    }
}

static Void local generaliseAss(ng,qs,v)/* Lookup type of var v in current */
Int  ng;			       /* top level assumptions and replace*/
List qs;			       /* by its generalisation, qualified */
Cell v; {			       /* by qs, first ng generics already */
    List ass = findTopBinding(v);      /* used.				   */

    if (isNull(ass) || !isInt(snd(ass)))
	internal("generaliseAss");

    resetGenericsFrom(ng);
    snd(ass) = generalise(qs,copyTyvar(intOf(snd(ass))));
}

static Type local generalise(qs,t)	/* calculate generalisation of t   */
List qs;				/* having already marked fixed vars*/
Type t; {				/* with qualifying preds qs	   */
    if (nonNull(qs))
	t = ap(QUAL,pair(qs,t));
    if (nonNull(genericVars)) {
	Kind k  = STAR;
	List vs = genericVars;
	for (; nonNull(vs); vs=tl(vs))
	    k = ap(tyvar(intOf(hd(vs)))->kind,k);
	t = mkPolyType(k,t);
#ifdef DEBUG_KINDS
printf("Generalised type: ");
printType(stdout,t);
printf(" ::: ");
printKind(stdout,k);
printf("\n");
#endif
    }
    return t;
}

/* --------------------------------------------------------------------------
 * Compare declared type schemes with inferred type schemes:
 * ------------------------------------------------------------------------*/

static Void local checkBindSigs(b)     /* check explicit type signature in */
Cell b; {			       /* binding with inferred type	   */
    if (nonNull(fst(snd(b)))) {
	if (isVar(fst(b)))	       /* function-binding?		   */
	    checkTypeSig(rhsLine(snd(hd(snd(snd(b))))),
			 fst(b),
			 fst(snd(b)));
	else {			       /* pattern-binding?		   */
	    List vs   = fst(b);
	    List ts   = fst(snd(b));
	    Int  line = rhsLine(snd(snd(b)));

	    while (nonNull(vs) && nonNull(ts)) {
		if (nonNull(hd(ts)))
		    checkTypeSig(line,hd(vs),hd(ts));
		vs = tl(vs);
		ts = tl(ts);
	    }
	}
    }
}

static Void local checkTypeSig(l,v,t)  /* Compare explicit type scheme t   */
Int  l;				       /* declared for v with generalised  */
Cell v;				       /* type in current assumption       */
Type t; {
    Cell ass = findTopBinding(v);

    if (isNull(ass))
	internal("checkTypeSig");

    if (nonNull(t) && !equalSchemes(t,snd(ass)))
	tooGeneral(l,v,t,snd(ass));
}

static Void local tooGeneral(l,e,dt,it)	/* explicit type sig. too general  */
Int  l;
Cell e;
Type dt, it; {
    ERROR(l) "Declared type too general" ETHEN
    ERRTEXT  "\n*** Expression    : "	 ETHEN ERREXPR(e);
    ERRTEXT  "\n*** Declared type : "	 ETHEN ERRTYPE(dt);
    ERRTEXT  "\n*** Inferred type : "	 ETHEN ERRTYPE(it);
    ERRTEXT  "\n"
    EEND;
}

/* --------------------------------------------------------------------------
 * Compare type schemes:
 * ------------------------------------------------------------------------*/

static Bool local equalSchemes(s1,s2)  /* Compare type schemes for equality*/
Type s1, s2; {
    Bool b1 = isPolyType(s1);
    Bool b2 = isPolyType(s2);
    if (b1 || b2) {
        if (b1 && b2 && eqKind(polySigOf(s1),polySigOf(s2))) {
            s1 = monoTypeOf(s1);
            s2 = monoTypeOf(s2);
        }
        else
            return FALSE;
    }

    b1 = (whatIs(s1)==QUAL);
    b2 = (whatIs(s2)==QUAL);
    if (b1 && b2 && equalQuals(fst(snd(s1)),fst(snd(s2)))) {
	s1 = snd(snd(s1));
	s2 = snd(snd(s2));
    }
    else if (b1 && !b2 && isNull(fst(snd(s1))))	/* maybe somebody gave an   */
	s1 = snd(snd(s1));			/* explicitly null context? */
    else if (!b1 && b2 && isNull(fst(snd(s2))))
	s2 = snd(snd(s2));
    else if (b1 || b2)
	return FALSE;

    return equalTypes(s1,s2);
}

static Bool local equalQuals(qs1,qs2)  /* Compare lists of qualifying preds*/
List qs1, qs2; {
    while (nonNull(qs1) && nonNull(qs2)) {		/* loop thru lists */
	Cell q1 = hd(qs1);
	Cell q2 = hd(qs2);

        while (isAp(q1) && isAp(q2)) {			/* loop thru args  */
	    if (!equalTypes(arg(q1),arg(q2)))
		return FALSE;
	    q1 = fun(q1);
	    q2 = fun(q2);
	}
	if (q1!=q2)					/* compare classes */
	    return FALSE;
	qs1 = tl(qs1);
	qs2 = tl(qs2);
    }
    return isNull(qs1) && isNull(qs2);			/* compare lengths */
}

static Bool local equalTypes(t1,t2)    /* Compare simple types for equality*/
Type t1, t2; {

et: if (whatIs(t1)!=whatIs(t2))
	return FALSE;

    switch (whatIs(t1)) {
	case TYCON   :
	case OFFSET  :
	case TUPLE   : return t1==t2;

	case INTCELL : return intOf(t1)!=intOf(t2);

	case UNIT    :
	case ARROW   :
	case LIST    : return TRUE;

	case AP      : if (equalTypes(fun(t1),fun(t2))) {
			   t1 = arg(t1);
			   t2 = arg(t2);
			   goto et;
		       }
                       return FALSE;

	default      : internal("equalTypes");
    }

    return TRUE;/*NOTREACHED*/
}

/* --------------------------------------------------------------------------
 * Entry points to type checker:
 * ------------------------------------------------------------------------*/

Type typeCheckExp() {		       /* Type check top level expression  */
    Type type;
    List qs;

    typeChecker(RESET);
    enterBindings();

    inputExpr = typeExpr(0,inputExpr);
    clearMarks();
    type = copyType(typeIs,typeOff);
    if (nonNull(elimConstPreds(0,"expression",inputExpr,NIL)))
	internal("typeCheckExp");
    preds = simplify(preds);
    qs    = copyPreds(preds);
    type  = generalise(qs,type);
    if (nonNull(preds)) {		/* qualify input expression with   */
	if (whatIs(inputExpr)!=LAMBDA)	/* additional dictionary params	   */
	    inputExpr = ap(LAMBDA,pair(NIL,pair(mkInt(0),inputExpr)));
	qualify(preds,snd(inputExpr));
    }
    typeChecker(RESET);
    return type;
}

Void typeCheckDefns() { 	       /* Type check top level bindings    */
    Target t  = length(valDefns) + length(instDefns) + length(classDefns);
    Target i  = 0;
    List   gs;

    typeChecker(RESET);
    enterBindings();
    setGoal("Type checking",t);

    for (gs=valDefns; nonNull(gs); gs=tl(gs)) {
	typeDefnGroup(hd(gs));
	soFar(i++);
    }
    clearTypeIns();
    for (gs=instDefns; nonNull(gs); gs=tl(gs)) {
	typeInstDefn(hd(gs));
	soFar(i++);
    }
    for (gs=classDefns; nonNull(gs); gs=tl(gs)) {
	typeClassDefn(hd(gs));
	soFar(i++);
    }

    typeChecker(RESET);
    done();
}

static Void local typeDefnGroup(bs)	/* type check group of value defns */
List bs; {				/* (one top level scc)		   */
    List as;

    emptySubstitution();
    hd(defnBounds) = NIL;
    preds	   = NIL;
    setTypeIns(bs);
    typeBindings(bs);			/* find types for vars in bindings */

    if (nonNull(preds)) {		/* look for unresolved overloading */
	Cell b    = hd(bs);
        Cell ass;
        Int  line;
        Cell v;

	preds = simplify(preds);	/* Simplify context first ...	   */

        if (isVar(fst(b))) {		/* determine var name & line no.   */
	    v    = fst(b);
	    line = rhsLine(snd(hd(snd(b))));
	}
	else {
	    v    = hd(fst(b));
	    line = rhsLine(snd(snd(b)));
	}
        ass = findInAssumList(textOf(v),hd(varsBounds));

	ERROR(line) "Unresolved top-level overloading" ETHEN
        ERRTEXT     "\n*** Binding             : %s", textToStr(textOf(v))
        ETHEN
        if (nonNull(ass)) {
            ERRTEXT "\n*** Inferred type       : " ETHEN ERRTYPE(snd(ass));
        }
        ERRTEXT     "\n*** Outstanding context : " ETHEN
                                               ERRCONTEXT(copyPreds(preds));
        ERRTEXT     "\n"
	EEND;
    }

    for (as=hd(varsBounds); nonNull(as); as=tl(as)) {
	Cell a = hd(as);		/* add infered types to environment*/
	Name n = findName(textOf(fst(a)));

	if (isNull(n))
	    internal("typeDefnGroup");
	if (catchAmbigs && isAmbiguous(snd(a)))
	    ambigError(name(n).line,"inferred type",n,snd(a));
	name(n).type = snd(a);
    }
    hd(varsBounds) = NIL;
}

/* --------------------------------------------------------------------------
 * Type checker control:
 * ------------------------------------------------------------------------*/

Void typeChecker(what)
Int what; {
    Int  i;

    switch (what) {
	case RESET   : tcMode	   = EXPRESSION;
		       matchMode   = FALSE;
		       predProve   = NIL;
		       instPred	   = NIL;
		       instExpr	   = NIL;
		       unkindTypes = NIL;
		       emptySubstitution();
		       emptyAssumption();
		       preds       = NIL;
		       break;

	case MARK    : for (i=0; i<MAXTUPCON; ++i)
			   mark(tupleConTypes[i]);
		       for (i=0; i<MAXKINDFUN; ++i) {
			   mark(simpleKindCache[i]);
			   mark(varKindCache[i]);
		       }
		       for (i=0; i<numTyvars; ++i)
			   mark(tyvars[i].bound);
		       mark(typeIs);
		       mark(predsAre);
		       mark(defnBounds);
		       mark(varsBounds);
		       mark(depends);
		       mark(preds);
		       mark(predProve);
		       mark(instPred);
		       mark(instExpr);
		       mark(unkindTypes);
		       mark(genericVars);
		       mark(arrow);
		       mark(typeList);
		       mark(typeIntToInt);
		       mark(predNum);
		       mark(predMonad);
		       mark(predMonad0);
		       mark(starToStar);
		       mark(monadSig);
#if IO_MONAD
		       mark(typeProgIO);
		       mark(typeSTab);
#endif
#ifdef LAMBDAVAR
		       mark(typeProg);
#endif
#ifdef LAMBDANU
		       mark(typeLnProg);
#endif
		       break;

	case INSTALL : typeChecker(RESET);

		       for (i=0; i<MAXTUPCON; ++i)
			   tupleConTypes[i] = NIL;
		       for (i=0; i<MAXKINDFUN; ++i) {
			   simpleKindCache[i] = NIL;
			   varKindCache[i]    = NIL;
		       }

		       var	    = mkOffset(0);
		       arrow	    = fn(var,mkOffset(1));
		       starToStar   = simpleKind(1);

		       typeList     = ap(LIST,var);
		       nameNil	    = addPrimCfun("[]",0,0,
						   mkPolyType(starToStar,
							      typeList));
		       nameCons     = addPrimCfun(":",2,1,
						   mkPolyType(starToStar,
							      fn(var,
							      fn(typeList,
								 typeList))));

		       typeUnit     = UNIT;

		       typeBool     = addPrimTycon("Bool",STAR,0,DATATYPE,NIL);
		       nameFalse    = addPrimCfun("False",0,0,typeBool);
		       nameTrue     = addPrimCfun("True",0,1,typeBool);
                       tycon(typeBool).defn
				    = cons(nameFalse,cons(nameTrue,NIL));

		       typeInt	    = addPrimTycon("Int",STAR,0,DATATYPE,NIL);
		       typeFloat    = addPrimTycon("Float",STAR,0,DATATYPE,NIL);

		       typeChar     = addPrimTycon("Char",STAR,0,DATATYPE,NIL);
		       typeString   = addPrimTycon("String",STAR,0,SYNONYM,
							ap(LIST,typeChar));
		       typeIntToInt = ap(ap(ARROW,typeInt),typeInt);

#if HASKELL_ARRAYS
		       typeArray    = addPrimTycon("Array",simpleKind(2),2,
						   DATATYPE,NIL);
#endif
#if IO_MONAD
		       typeWorld    = addPrimTycon("RealWorld",STAR,0,
						   DATATYPE,NIL);
		       typeST	    = addPrimTycon("ST",simpleKind(2),2,
						   DATATYPE,NIL);
		       typeSTab	    = ap(ap(typeST,mkOffset(0)),mkOffset(1));
		       typeIO	    = addPrimTycon("IO",starToStar,0,SYNONYM,
						   ap(typeST,typeWorld));
		       typeProgIO   = ap(typeIO,UNIT);
		       typeMutVar   = addPrimTycon("MutVar",simpleKind(2),2,
						   DATATYPE,NIL);
#if HASKELL_ARRAYS
		       typeMutArr   = addPrimTycon("MutArr",simpleKind(3),3,
						   DATATYPE,NIL);
#endif
#endif
#ifdef LAMBDAVAR
		       typeProc     = addPrimTycon("Proc",starToStar,1,
						   DATATYPE,NIL);
		       typeProg	    = ap(typeProc,UNIT);
		       typeVar	    = addPrimTycon("Var",starToStar,1,
						   DATATYPE,NIL);
		       nameVar      = addPrimCfun("_LambdaVar",1,0,
						  mkPolyType(starToStar,
							     fn(var,
								ap(typeVar,
								   var))));
#endif
#ifdef LAMBDANU
		       typeCmd      = addPrimTycon("Cmd",simpleKind(2),2,
						   DATATYPE,NIL);
		       typeLnProg   = mkPolyType(starToStar,
						 ap(ap(typeCmd,var),UNIT));
		       typeTag	    = addPrimTycon("Tag",starToStar,1,
						   DATATYPE,NIL);
		       nameTag      = addPrimCfun("_LambdaNu",1,0,
						  mkPolyType(starToStar,
							     fn(var,
								ap(typeTag,
								   var))));
#endif
#if IO_DIALOGUE
                       initIOtypes();
#endif

		       nameFromInt  = NIL;
		       classNum	    = NIL;
		       predNum	    = NIL;
		       classMonad   = NIL;
		       predMonad    = NIL;
		       classMonad0  = NIL;
		       predMonad0   = NIL;
		       monadSig	    = NIL;

		       break;

	case PRELUDE : classNum    = findClass(findText("Num"));
		       nameFromInt = findName(findText("fromInteger"));
		       if (nonNull(classNum) && nonNull(nameFromInt))
			   predNum = ap(classNum,var);

		       classMonad  = findClass(findText("Monad"));
		       classMonad0 = findClass(findText("Monad0"));
		       nameResult  = findName(findText("result"));
		       nameBind	   = findName(findText("bind"));
		       nameZero    = findName(findText("zero"));
		       if (nonNull(classMonad)  &&
			   nonNull(classMonad0) &&
			   nonNull(nameResult)  &&
			   nonNull(nameBind)    &&
			   nonNull(nameZero)) {
			   predMonad  = ap(classMonad,var);
			   predMonad0 = ap(classMonad0,var);
			   monadSig   = singleton(starToStar);
		       }
		       else {
			   nameResult  = NIL;
			   nameBind    = NIL;
			   nameZero    = NIL;
			   classMonad  = NIL;
			   predMonad   = NIL;
			   classMonad0 = NIL;
			   predMonad0  = NIL;
			   monadSig    = NIL;
		       }
		       break;
    }
}

#if IO_DIALOGUE
static Void local initIOtypes() {	/* initialise I/O types and cfuns   */
    Type req	    = addPrimTycon("Request",STAR,0,DATATYPE,NIL);
    Type rsp	    = addPrimTycon("Response",STAR,0,DATATYPE,NIL);
    Type ioe	    = addPrimTycon("IOError",STAR,0,DATATYPE,NIL);
    Type si	    = fn(typeString, ioe);
    Type sreq	    = fn(typeString, req);
    Type ssreq	    = fn(typeString, sreq);

    nameReadFile    = addPrimCfun("ReadFile",   1, 0, sreq);
    nameWriteFile   = addPrimCfun("WriteFile",  2, 1, ssreq);
    nameAppendFile  = addPrimCfun("AppendFile", 2, 2, ssreq);
    nameReadChan    = addPrimCfun("ReadChan",   1, 3, sreq);
    nameAppendChan  = addPrimCfun("AppendChan", 2, 4, ssreq);
    nameEcho        = addPrimCfun("Echo",       1, 5, fn(typeBool,req));
    nameGetArgs     = addPrimCfun("GetArgs",    0, 6, req);
    nameGetProgName = addPrimCfun("GetProgName",0, 7, req);
    nameGetEnv      = addPrimCfun("GetEnv",     1, 8, sreq);
    tycon(req).defn = cons(nameReadFile,cons(nameWriteFile,
		       cons(nameAppendFile,cons(nameReadChan,
		        cons(nameAppendChan,cons(nameEcho,
		         cons(nameGetArgs,cons(nameGetProgName,
		          cons(nameGetEnv,NIL)))))))));

    nameSuccess     = addPrimCfun("Success",0,0,rsp);
    nameStr	    = addPrimCfun("Str",    1,1,fn(typeString,rsp));
    nameFailure     = addPrimCfun("Failure",1,2,fn(ioe,rsp));
    nameStrList     = addPrimCfun("StrList",1,3,fn(ap(LIST,typeString),rsp));
    tycon(rsp).defn = cons(nameSuccess,cons(nameStr,
		       cons(nameFailure,cons(nameStrList,NIL))));

    nameWriteError  = addPrimCfun("WriteError", 1, 0, si);
    nameReadError   = addPrimCfun("ReadError",  1, 1, si);
    nameSearchError = addPrimCfun("SearchError",1, 2, si);
    nameFormatError = addPrimCfun("FormatError",1, 3, si);
    nameOtherError  = addPrimCfun("OtherError", 1, 4, si);
    tycon(ioe).defn = cons(nameWriteError,cons(nameReadError,
		       cons(nameSearchError,cons(nameFormatError,
			cons(nameOtherError,NIL)))));

    typeDialogue    = addPrimTycon("Dialogue",STAR,0,SYNONYM,
					fn(ap(LIST,rsp),ap(LIST,req)));
}
#endif

/*-------------------------------------------------------------------------*/
