/* --------------------------------------------------------------------------
 * parser.y:    Copyright (c) Mark P Jones 1991-1994.   All rights reserved.
 *              See goferite.h for details and conditions of use etc...
 *              Gofer version 2.30 March 1994
 *
 *              You should expect 14 shift/reduce conflicts when passing
 *              this grammar through yacc.  Don't worry, they will all be
 *              resolved correctly as shifts.
 *
 *		There will also be 8 reduce/reduce conflicts.  These are
 *		more worrying although they will still be resolved correctly
 *		as long as you keep the two grammar rules concerned (see the
 *		y.output file for details) in the same order as used here.
 *
 * Gofer parser (included as part of input.c)
 * ------------------------------------------------------------------------*/

%{
#ifndef lint
#define lint
#endif
#define defTycon(n,l,lhs,rhs,w)	 tyconDefn(intOf(l),lhs,rhs,w); sp-=n
#define sigdecl(l,vs,t)		 ap(SIGDECL,triple(l,vs,t))
#define grded(gs)		 ap(GUARDED,gs)
#define letrec(bs,e)		 (nonNull(bs) ? ap(LETREC,pair(bs,e)) : e)
#define yyerror(s)		 /* errors handled elsewhere */
#define YYSTYPE			 Cell

static Cell   local gcShadow     Args((Int,Cell));
static Void   local syntaxError  Args((String));
static String local unexpected   Args((Void));
static Cell   local checkPrec    Args((Cell));
static Void   local fixDefn      Args((Syntax,Cell,Cell,List));
static Void   local setSyntax    Args((Int,Syntax,Cell));
static Cell   local buildTuple   Args((List));
static Cell   local checkClass   Args((Cell));
static List   local checkContext Args((List));
static Pair   local checkDo	 Args((List));
static Cell   local checkTyLhs	 Args((Cell));
static Cell   local tidyInfix    Args((Cell));

/* For the purposes of reasonably portable garbage collection, it is
 * necessary to simulate the YACC stack on the Gofer stack to keep
 * track of all intermediate constructs.  The lexical analyser
 * pushes a token onto the stack for each token that is found, with
 * these elements being removed as reduce actions are performed,
 * taking account of look-ahead tokens as described by gcShadow()
 * below.
 *
 * Of the non-terminals used below, only start, topDecl & begin do not leave
 * any values on the Gofer stack.  The same is true for the terminals
 * EVALEX and SCRIPT.  At the end of a successful parse, there should only
 * be one element left on the stack, containing the result of the parse.
 */

#define gc0(e)			 gcShadow(0,e)
#define gc1(e)			 gcShadow(1,e)
#define gc2(e)			 gcShadow(2,e)
#define gc3(e)			 gcShadow(3,e)
#define gc4(e)			 gcShadow(4,e)
#define gc5(e)			 gcShadow(5,e)
#define gc6(e)			 gcShadow(6,e)
#define gc7(e)			 gcShadow(7,e)

%}

%token EVALEX    SCRIPT
%token '='       COCO       INFIXL     INFIXR     INFIX      FUNARROW
%token '-'       ','        '@'        '('        ')'        '|'
%token ';'       UPTO       '['        ']'        CASEXP     OF
%token IF        THEN       ELSE       WHERE      TYPE       DATA
%token FROM      '\\'       '~'        LET        IN	     '`'
%token VAROP     VARID      NUMLIT     CHARLIT    STRINGLIT  REPEAT
%token CONOP     CONID
%token TCLASS    IMPLIES    TINSTANCE 
%token DO	 TRUNST

%token PRIMITIVE
				    /* Haskell keywords, for compatibility */
%token DEFAULT	 DERIVING   HIDING     IMPORT	  INTERFACE  MODULE
%token RENAMING  TO

%%
/*- Top level script/module structure -------------------------------------*/

start	  : EVALEX exp			{inputExpr = $2;	    sp-=1;}
	  | EVALEX exp wherePart	{inputExpr = letrec($3,$2); sp-=2;}
	  | SCRIPT topModule		{valDefns  = $2;	    sp-=1;}
	  | error			{syntaxError("input");}
	  ;

/*- Haskell module header/import parsing: ---------------------------------*/
/*  Syntax for Haskell modules (module headers and imports) is parsed but  */
/*  is otherwise ignored by Gofer.  This is for the benefit of those who   */
/*  use Gofer to develop code which will ultimately be fed into a full	   */
/*  Haskell system.  (default and deriving are treated in a similar way.)  */
/*									   */
/*  Note that we do not make any attempt to provide actions that store	   */
/*  the parsed structures in any way for later use.			   */
/*-------------------------------------------------------------------------*/

topModule : begin topDecls close	{$$ = gc2($2);}
	  | modules			{$$ = $1;}
	  ;
begin	  : error			{yyerrok; goOffside(startColumn);}
	  ;
topDecls  : topDecls ';' topDecl	{$$ = gc2($1);}
	  | topDecls ';' decl		{$$ = gc3(cons($3,$1));}
	  | topDecl			{$$ = gc0(NIL);}
	  | decl			{$$ = gc1(cons($1,NIL));}
	  | error			{syntaxError("definition");}
	  ;
modules	  : modules module		{$$ = gc2(appendOnto($2,$1));}
	  | module			{$$ = $1;}
	  ;
module	  : MODULE modid expspec WHERE '{' topDecls close
					{$$ = gc7($6);}
	  | MODULE error		{syntaxError("module definition");}
	  ;
topDecl	  : IMPORT modid impspec rename	{sp-=4;}
	  | IMPORT error		{syntaxError("import declaration");}
	  ;
modid	  : CONID			{$$ = $1;}
	  | STRINGLIT			{$$ = $1;}
	  ;
expspec	  : /* empty */			{$$ = gc0(NIL);}
	  | '(' exports ')'		{$$ = gc3(NIL);}
	  ;
exports	  : exports ',' export		{$$ = gc3(NIL);}
	  | export			{$$ = $1;}
	  ;
export	  : entity			{$$ = $1;}
	  | modid UPTO			{$$ = gc2(NIL);}
	  ;
impspec	  : /* empty */			{$$ = gc0(NIL);}
	  | HIDING '(' imports ')'	{$$ = gc4(NIL);}
	  | '(' imports0 ')'		{$$ = gc3(NIL);}
	  ;
imports0  : /* empty */			{$$ = gc0(NIL);}
	  | imports			{$$ = $1;}
	  ;
imports	  : imports ',' entity		{$$ = gc3(NIL);}
	  | entity			{$$ = $1;}
	  ;
rename	  : /* empty */			{$$ = gc0(NIL);}
	  | RENAMING '(' renamings ')'	{$$ = gc4(NIL);}
	  ;
renamings : renamings ',' renaming	{$$ = gc3(NIL);}
	  | renaming			{$$ = $1;}
	  ;
renaming  : var   TO var		{$$ = gc3(NIL);}
	  | conid TO conid		{$$ = gc3(NIL);}
	  ;
entity	  : var				{$$ = $1;}
	  | CONID			{$$ = $1;}
	  | CONID '(' UPTO ')'		{$$ = gc4(NIL);}
	  | CONID '(' conids ')'	{$$ = gc4(NIL);}
	  | CONID '(' vars0 ')'		{$$ = gc4(NIL);}
	  ;
conids	  : conids ',' conid		{$$ = gc3(NIL);}
	  | conid			{$$ = $1;}
	  ;
vars0	  : /* empty */			{$$ = gc0(NIL);}
	  | vars			{$$ = $1;}
	  ;

/*- Type declarations: ----------------------------------------------------*/

topDecl	  : TYPE tyLhs '=' type invars	{defTycon(5,$3,$2,$4,$5);}
	  | DATA type '=' constrs deriving	    /* deriving is IGNORED */
					{defTycon(5,$3,checkTyLhs($2),
							rev($4),DATATYPE);}
	  | DATA context IMPLIES tyLhs '=' constrs deriving
					{defTycon(7,$5,$4,
						  ap(QUAL,pair($2,rev($6))),
						  DATATYPE);}
	  ;
tyLhs	  : tyLhs VARID			{$$ = gc2(ap($1,$2));}
	  | CONID			{$$ = $1;}
	  | error			{syntaxError("type defn lhs");}
	  ;
invars	  : IN rsvars			{$$ = gc2($2);}
	  | /* empty */			{$$ = gc0(SYNONYM);}
	  ;
rsvars	  : rsvars ',' rsvar		{$$ = gc3(cons($3,$1));}
	  | rsvar			{$$ = gc1(cons($1,NIL));}
	  ;
rsvar	  : var COCO sigType		{$$ = gc3(sigdecl($2,singleton($1),
							     $3));}
	  | var				{$$ = $1;}
	  ;
constrs	  : constrs '|' constr		{$$ = gc3(cons($3,$1));}
	  | constr			{$$ = gc1(cons($1,NIL));}
	  ;
constr	  : type conop type		{$$ = gc3(ap(ap($2,$1),$3));}
	  | type			{if (!isCon(getHead($1)))
					     syntaxError("data constructor");
					 $$ = $1;}
	  | error			{syntaxError("data type definition");}
	  ;
deriving  : /* empty */			{$$ = gc0(NIL);}
	  | DERIVING CONID		{$$ = gc2(singleton($2));}
	  | DERIVING '(' derivs0 ')'	{$$ = gc4($3);}
	  ;
derivs0   : /* empty */			{$$ = gc0(NIL);}
	  | derivs			{$$ = $1;}
	  ;
derivs	  : derivs ',' CONID		{$$ = gc3(cons($3,$1));}
	  | CONID			{$$ = gc1(singleton($1));}
	  ;

/*- Type expressions: -----------------------------------------------------*/
/*  Parser is not sufficently powerful to distinguish between a predicate
 *  such as "Dual a b" and a type "Sum a b", or between a tuple type and
 *  a context (e.g. (Alpha a, Beta b) is a tuple or context?).  For this
 *  reason, individual predicates and contexts are parsed as types, with
 *  additional code to check for well formed context/classes.
 */

sigType	  : context IMPLIES type	{$$ = gc3(ap(QUAL,pair($1,$3)));}
	  | type			{$$ = $1;}
	  ;
context	  : type			{$$ = gc1(checkContext($1));}
	  ;
type	  : ctype			{$$ = $1;}
	  | ctype FUNARROW type		{$$ = gc3(ap(ap(ARROW,$1),$3));}
	  | error			{syntaxError("type expression");}
	  ;
ctype	  : ctype atype			{$$ = gc2(ap($1,$2));}
	  | atype			{$$ = $1;}
	  ;
atype	  : VARID			{$$ = $1;}
	  | CONID			{$$ = $1;}
	  | '(' ')'			{$$ = gc2(UNIT);}
	  | '(' FUNARROW ')'		{$$ = gc3(ARROW);}
	  | '(' type ')'		{$$ = gc3($2);}
	  | '(' ctype FUNARROW ')'	{$$ = gc4(ap(ARROW,$2));}
	  | '(' tupCommas ')'		{$$ = gc3($2);}
	  | '(' typeTuple ')'		{$$ = gc3(buildTuple($2));}
	  | '[' type ']'		{$$ = gc3(ap(LIST,$2));}
	  | '[' ']'			{$$ = gc2(LIST);}
	  ;
tupCommas : tupCommas ','		{$$ = gc2(mkTuple(tupleOf($1)+1));}
	  | ','				{$$ = gc1(mkTuple(2));}
	  ;
typeTuple : typeTuple ',' type		{$$ = gc3(cons($3,$1));}
	  | type ',' type		{$$ = gc3(cons($3,cons($1,NIL)));}
	  ;

/*- Fixity declarations: --------------------------------------------------*/

topDecl	  : INFIXL optdigit ops		{fixDefn(LEFT_ASS,$1,$2,$3); sp-=3;}
	  | INFIXR optdigit ops		{fixDefn(RIGHT_ASS,$1,$2,$3);sp-=3;}
	  | INFIX  optdigit ops		{fixDefn(NON_ASS,$1,$2,$3);  sp-=3;}
	  ;
optdigit  : NUMLIT			{$$ = gc1(checkPrec($1));}
	  | /* empty */			{$$ = gc0(mkInt(DEF_PREC));}
	  ;
ops	  : ops ',' op			{$$ = gc3(cons($3,$1));}
	  | op				{$$ = gc1(cons($1,NIL));}
	  ;
op	  : varop			{$$ = $1;}
	  | conop			{$$ = $1;}
	  | '-'				{$$ = gc1(varMinus);}
	  ;
varop	  : VAROP			{$$ = $1;}
	  | '`' VARID '`'		{$$ = gc3($2);}
	  ;
conop	  : CONOP			{$$ = $1;}
	  | '`' CONID '`'		{$$ = gc3($2);}
	  ;

/*- Processing definitions of primitives ----------------------------------*/

topDecl	  : PRIMITIVE prims COCO sigType{primDefn($1,$2,$4); sp-=4;}
	  ;
prims	  : prims ',' prim		{$$ = gc3(cons($3,$1));}
	  | prim			{$$ = gc1(cons($1,NIL));}
	  | error			{syntaxError("primitive defn");}
	  ;
prim	  : var STRINGLIT		{$$ = gc2(pair($1,$2));}
	  ;

/*- Class declarations: ---------------------------------------------------*/

topDecl	  : TCLASS classHead classBody	{classDefn(intOf($1),$2,$3); sp-=3;}
	  | TINSTANCE classHead instBody{instDefn(intOf($1),$2,$3);  sp-=3;}
	  | DEFAULT type		{sp-=2;}    /* default is IGNORED  */
	  ;
classHead : context IMPLIES type	{$$ = gc3(pair($1,checkClass($3)));}
	  | type			{$$ = gc1(pair(NIL,checkClass($1)));}
	  ;
classBody : WHERE '{' csigdecls close	{$$ = gc4($3);}
	  | /* empty */			{$$ = gc0(NIL);}
	  ;
instBody  : WHERE '{' decls close	{$$ = gc4($3);}
	  | /* empty */			{$$ = gc0(NIL);}
	  ;
csigdecls : csigdecls ';' csigdecl	{$$ = gc3(cons($3,$1));}
	  | csigdecl			{$$ = gc1(cons($1,NIL));}
	  ;
csigdecl  : decl			{$$ = gc1($1);}
	  | error			{syntaxError("class body");}
	  ;

/*- Value declarations: ---------------------------------------------------*/

decl	  : vars COCO sigType		{$$ = gc3(sigdecl($2,$1,$3));}
	  | opExp rhs			{$$ = gc2(pair($1,$2));}
	  ;
decls	  : decls ';' decl		{$$ = gc3(cons($3,$1));}
	  | decl			{$$ = gc1(cons($1,NIL));}
	  ;
rhs	  : rhs1 wherePart		{$$ = gc2(letrec($2,$1));}
	  | rhs1			{$$ = $1;}
	  | error			{syntaxError("declaration");}
	  ;
rhs1	  : '=' exp			{$$ = gc2(pair($1,$2));}
	  | gdefs			{$$ = gc1(grded(rev($1)));}
	  ;
wherePart : WHERE '{' decls close	{$$ = gc4($3);}
	  ;
gdefs	  : gdefs gdef			{$$ = gc2(cons($2,$1));}
	  | gdef			{$$ = gc1(cons($1,NIL));}
	  ;
gdef	  : '|' exp '=' exp		{$$ = gc4(pair($3,pair($2,$4)));}
	  /* Experimental, undocumented syntax for Orwell style guards     */
	  /* The corresponding forms for case definitions are NOT supported*/
	  /* because that would require a change to the original syntax for*/
          /* Gofer, rather than a simple extension as is the case here.    */
	  /* Perhaps a slight reworking of the grammar might eliminate this*/
	  /* problem...							   */
	  | '=' exp ',' IF exp		{$$ = gc5(pair($1,pair($5,$2)));}
	  | '=' exp ',' exp		{$$ = gc4(pair($1,pair($4,$2)));}
	  ;
vars	  : vars ',' var		{$$ = gc3(cons($3,$1));}
	  | var				{$$ = gc1(cons($1,NIL));}
	  ;
var	  : varid			{$$ = $1;}
	  | '(' '-' ')'			{$$ = gc3(varMinus);}
	  ;
varid	  : VARID			{$$ = $1;}
	  | '(' VAROP ')'		{$$ = gc3($2);}
	  ;
conid	  : CONID			{$$ = $1;}
	  | '(' CONOP ')'		{$$ = gc3($2);}
	  ;

/*- Expressions: ----------------------------------------------------------*/

exp	  : opExp COCO sigType		{$$ = gc3(ap(ESIGN,pair($1,$3)));}
	  | opExp			{$$ = $1;}
	  | error			{syntaxError("expression");}
	  ; 
opExp	  : pfxExp			{$$ = $1;}
	  | pfxExp op pfxExp		{$$ = gc3(ap(ap($2,$1),$3));}
	  | opExp0			{$$ = gc1(tidyInfix($1));}
	  ;
opExp0	  : opExp0 op pfxExp		{$$ = gc3(ap(ap($2,$1),$3));}
	  | pfxExp op pfxExp op pfxExp	{$$ = gc5(ap(ap($4,
							ap(ap($2,singleton($1)),
                                                           $3)),$5));}
	  ;
pfxExp	  : '-' appExp			{if (isInt($2))
					     $$ = gc2(mkInt(-intOf($2)));
					 else
					     $$ = gc2(ap(varNegate,$2));
					}
	  | '\\' pats FUNARROW exp	{$$ = gc4(ap(LAMBDA,
						     pair(rev($2),
						          pair($3,$4))));}
	  | LET '{' decls close IN exp	{$$ = gc6(letrec($3,$6));}
	  | IF exp THEN exp ELSE exp	{$$ = gc6(ap(COND,triple($2,$4,$6)));}
	  | CASEXP exp OF '{' alts close{$$ = gc6(ap(CASE,pair($2,rev($5))));}
	  | appExp			{$$ = $1;}
	  ;
pats	  : pats atomic			{$$ = gc2(cons($2,$1));}
	  | atomic			{$$ = gc1(cons($1,NIL));}
	  ;
appExp	  : appExp atomic		{$$ = gc2(ap($1,$2));}
	  | TRUNST atomic		{$$ = gc2(ap(RUNST,$2));}
	  | atomic			{$$ = $1;}
	  ;
atomic	  : var				{$$ = $1;}
	  | var '@' atomic		{$$ = gc3(ap(ASPAT,pair($1,$3)));}
	  | '~' atomic			{$$ = gc2(ap(LAZYPAT,$2));}
	  | '_'				{$$ = gc1(WILDCARD);}
	  | conid			{$$ = $1;}
	  | '(' ')'			{$$ = gc2(UNIT);}
	  | NUMLIT			{$$ = $1;}
	  | CHARLIT			{$$ = $1;}
	  | STRINGLIT			{$$ = $1;}
	  | REPEAT			{$$ = $1;}
	  | '(' exp ')'			{$$ = gc3($2);}
	  | '(' exps2 ')'		{$$ = gc3(buildTuple($2));}
	  | '[' list ']'		{$$ = gc3($2);}
	  | '(' pfxExp op ')'		{$$ = gc4(ap($3,$2));}
	  | '(' varop atomic ')'	{$$ = gc4(ap(ap(varFlip,$2),$3));}
	  | '(' conop atomic ')'	{$$ = gc4(ap(ap(varFlip,$2),$3));}
	  ;
exps2	  : exps2 ',' exp		{$$ = gc3(cons($3,$1));}
	  | exp ',' exp			{$$ = gc3(cons($3,cons($1,NIL)));}
	  ;
alts	  : alts ';' alt		{$$ = gc3(cons($3,$1));}
	  | alt				{$$ = gc1(cons($1,NIL));}
	  ;
alt	  : opExp altRhs		{$$ = gc2(pair($1,$2));}
	  ;
altRhs	  : altRhs1 wherePart		{$$ = gc2(letrec($2,$1));}
	  | altRhs1			{$$ = $1;}
	  ;
altRhs1	  : guardAlts			{$$ = gc1(grded(rev($1)));}
	  | FUNARROW exp		{$$ = gc2(pair($1,$2));}
	  | error			{syntaxError("case expression");}
	  ;
guardAlts : guardAlts guardAlt		{$$ = gc2(cons($2,$1));}
	  | guardAlt			{$$ = gc1(cons($1,NIL));}
	  ;
guardAlt  : '|' opExp FUNARROW exp	{$$ = gc4(pair($3,pair($2,$4)));}
	  ;

/*- List Expressions: -------------------------------------------------------*/

list	  : /* empty */			{$$ = gc0(nameNil);}
	  | exp				{$$ = gc1(ap(FINLIST,cons($1,NIL)));}
	  | exps2			{$$ = gc1(ap(FINLIST,rev($1)));}
	  | exp '|' quals		{$$ = gc3(ap(COMP,pair($1,rev($3))));}
	  | exp         UPTO exp	{$$ = gc3(ap(ap(varFromTo,$1),$3));}
	  | exp ',' exp UPTO		{$$ = gc4(ap(ap(varFromThen,$1),$3));}
	  | exp         UPTO		{$$ = gc2(ap(varFrom,$1));}
	  | exp ',' exp UPTO exp	{$$ = gc5(ap(ap(ap(varFromThenTo,
                                                               $1),$3),$5));}
	  ;
quals	  : quals ',' qual		{$$ = gc3(cons($3,$1));}
	  | qual			{$$ = gc1(cons($1,NIL));}
	  ;
qual	  : exp FROM exp		{$$ = gc3(ap(FROMQUAL,pair($1,$3)));}
	  | exp '=' exp			{$$ = gc3(ap(QWHERE,
						     singleton(
							pair($1,pair($2,
								     $3)))));}
	  | exp				{$$ = gc1(ap(BOOLQUAL,$1));}
	  | LET '{' decls close		{$$ = gc4(ap(QWHERE,$3));}
	  ;

/*- Do notation for monad comprehensions ----------------------------------*/
/* To experiment with the do notation for monad comprehensions, uncomment  */
/* the following productions, set the DO_COMPS flag to 1 in prelude.h and  */
/* recompile.  Note that this makes `do' a keyword, so any programs that   */
/* use this word as an identifier will need to be changed.		   */

/* DO_COMPS

pfxExp	  : DO '{' dquals close1	{$$ = gc4(ap(DOCOMP,checkDo($3)));}
	  ;
dqual	  : exp FROM exp		{$$ = gc3(ap(FROMQUAL,pair($1,$3)));}
	  | LET '{' decls close		{$$ = gc4(ap(QWHERE,$3));}
	  | IF exp			{$$ = gc2(ap(BOOLQUAL,$2));}
	  | exp				{$$ = gc1(ap(DOQUAL,$1));}
	  ;
dquals	  : dquals ';' dqual		{$$ = gc3(cons($3,$1));}
	  | dqual			{$$ = gc1(cons($1,NIL));}
	  ;
*/

/*- Find closing brace ----------------------------------------------------*/

					/* deal with trailing semicolon    */
close	  : ';' close1			{$$ = gc2($2);}
	  | close1			{$$ = $1;}
	  ;
close1	  : '}'				{$$ = $1;}
	  | error			{yyerrok;
                                         if (canUnOffside()) {
                                             unOffside();
					     /* insert extra token on stack*/
					     push(NIL);
					     pushed(0) = pushed(1);
					     pushed(1) = mkInt(column);
					 }
                                         else
                                             syntaxError("definition");
                                        }
	  ;

/*-------------------------------------------------------------------------*/

%%

static Cell local gcShadow(n,e)		/* keep parsed fragments on stack  */
Int  n;
Cell e; {
    /* If a look ahead token is held then the required stack transformation
     * is:
     *   pushed: n               1     0          1     0
     *           x1  |  ...  |  xn  |  la   ===>  e  |  la
     *                                top()            top()
     *
     * Othwerwise, the transformation is:
     *   pushed: n-1             0        0
     *           x1  |  ...  |  xn  ===>  e
     *                         top()     top()
     */
    if (yychar>=0) {
	pushed(n-1) = top();
        pushed(n)   = e;
    }
    else
	pushed(n-1) = e;
    sp -= (n-1);
    return e;
}

static Void local syntaxError(s)       /* report on syntax error           */
String s; {
    ERROR(row) "Syntax error in %s (unexpected %s)", s, unexpected()
    EEND;
}

static String local unexpected() {	/* find name for unexpected token  */
    static char buffer[100];
    static char *fmt = "%s \"%s\"";
    static char *kwd = "keyword";
    static char *hkw = "(Haskell) keyword";

    switch (yychar) {
	case 0	       : return "end of input";

#define keyword(kw) sprintf(buffer,fmt,kwd,kw); return buffer;
	case INFIXL    : keyword("infixl");
	case INFIXR    : keyword("infixr");
	case INFIX     : keyword("infix");
	case TINSTANCE : keyword("instance");
	case TCLASS    : keyword("class");
	case PRIMITIVE : keyword("primitive");
	case CASEXP    : keyword("case");
	case OF        : keyword("of");
	case IF        : keyword("if");
	case DO	       : keyword("do");
	case TRUNST    : keyword("runST");
	case THEN      : keyword("then");
	case ELSE      : keyword("else");
	case WHERE     : keyword("where");
	case TYPE      : keyword("type");
	case DATA      : keyword("data");
	case LET       : keyword("let");
	case IN        : keyword("in");
#undef keyword

#define hasword(kw) sprintf(buffer,fmt,hkw,kw); return buffer;
	case DEFAULT   : hasword("default");
	case DERIVING  : hasword("deriving");
	case HIDING    : hasword("hiding");
	case IMPORT    : hasword("import");
	case INTERFACE : hasword("interface");
	case MODULE    : hasword("module");
	case RENAMING  : hasword("renaming");
	case TO	       : hasword("to");
#undef hasword

	case FUNARROW  : return "`->'";
	case '='       : return "`='";
	case COCO      : return "`::'";
	case '-'       : return "`-'";
	case ','       : return "comma";
	case '@'       : return "`@'";
	case '('       : return "`('";
	case ')'       : return "`)'";
	case '|'       : return "`|'";
	case ';'       : return "`;'";
	case UPTO      : return "`..'";
	case '['       : return "`['";
	case ']'       : return "`]'";
	case FROM      : return "`<-'";
	case '\\'      : return "backslash (lambda)";
	case '~'       : return "tilde";
	case '`'       : return "backquote";
	case VAROP     :
	case VARID     :
	case CONOP     :
	case CONID     : sprintf(buffer,"symbol \"%s\"",
				 textToStr(textOf(yylval)));
			 return buffer;
	case NUMLIT    : return "numeric literal";
	case CHARLIT   : return "character literal";
	case STRINGLIT : return "string literal";
	case IMPLIES   : return "`=>";
	default	       : return "token";
    }
}

static Cell local checkPrec(p)         /* Check for valid precedence value */
Cell p; {
    if (!isInt(p) || intOf(p)<MIN_PREC || intOf(p)>MAX_PREC) {
        ERROR(row) "Precedence value must be an integer in the range [%d..%d]",
                   MIN_PREC, MAX_PREC
        EEND;
    }
    return p;
}

static Void local fixDefn(a,line,p,ops)/* Declare syntax of operators      */
Syntax a;
Cell   line;
Cell   p;
List   ops; {
    Int l = intOf(line);
    a     = mkSyntax(a,intOf(p));
    map2Proc(setSyntax,l,a,ops);
}

static Void local setSyntax(line,sy,op)/* set syntax of individ. operator  */
Int    line;
Syntax sy;
Cell   op; {
    addSyntax(line,textOf(op),sy);
    opDefns = cons(op,opDefns);
}

static Cell local buildTuple(tup)      /* build tuple (x1,...,xn) from list*/
List tup; {                            /* [xn,...,x1]                      */
    Int  n = 0;
    Cell t = tup;
    Cell x;

    do {                               /*     .                    .       */
        x      = fst(t);               /*    / \                  / \      */
        fst(t) = snd(t);               /*   xn  .                .   xn    */
        snd(t) = x;                    /*        .    ===>      .          */
        x      = t;                    /*         .            .           */
        t      = fun(x);               /*          .          .            */
        n++;                           /*         / \        / \           */
    } while (nonNull(t));              /*        x1  NIL   (n)  x1         */
    fst(x) = mkTuple(n);
    return tup;
}

/* The yacc parser presented above is not sufficiently powerful to
 * determine whether a tuple at the front of a sigType is part of a
 * context:    e.g. (Eq a, Num a) => a -> a -> a
 * or a type:  e.g.  (Tree a, Tree a) -> Tree a
 *
 * Rather than complicate the grammar, both are parsed as tuples of types,
 * using the following checks afterwards to ensure that the correct syntax
 * is used in the case of a tupled context.
 */

static List local checkContext(con)	/* validate type class context	   */
Type con; {
    if (con==UNIT)			/* allows empty context ()	   */
	return NIL;
    else if (whatIs(getHead(con))==TUPLE) {
	List qs = NIL;

	while (isAp(con)) {		/* undo work of buildTuple  :-(    */
	    Cell temp = fun(con);
	    fun(con)  = arg(con);
	    arg(con)  = qs;
	    qs	      = con;
	    con       = temp;
	    checkClass(hd(qs));
	}
	return qs;
    }
    else				/* single context expression	   */
	return singleton(checkClass(con));
}

static Cell local checkClass(c)		/* check that type expr is a class */
Cell c; {				/* constrnt of the form C t1 .. tn */
    Cell cn = getHead(c);

    if (!isCon(cn))
	syntaxError("class expression");
    else if (argCount<1) {
	ERROR(row) "Class \"%s\" must have at least one argument",
		   textToStr(textOf(cn))
	EEND;
    }
    return c;
}

static Pair local checkDo(dqs)		/* convert reversed list of dquals */
List dqs; {				/* to a (expr,quals) pair	   */
#if DO_COMPS
    if (isNull(dqs) || whatIs(hd(dqs))!=DOQUAL) {
	ERROR(row) "Last generator in do {...} must be an expression"
	EEND;
    }
    fst(dqs) = snd(fst(dqs));		/* put expression in fst of pair   */
    snd(dqs) = rev(snd(dqs));		/* & reversed list of quals in snd */
#endif
    return dqs;
}

static Cell local checkTyLhs(c)		/* check that lhs is of the form   */
Cell c; {				/* T a1 ... a			   */
    Cell tlhs = c;
    while (isAp(tlhs) && whatIs(arg(tlhs))==VARIDCELL)
	tlhs = fun(tlhs);
    if (whatIs(tlhs)!=CONIDCELL) {
	ERROR(row) "Illegal left hand side in datatype definition"
	EEND;
    }
    return c;
}

/* expressions involving a sequence of two or more infix operator symbols
 * are parsed as elements of type:
 *    InfixExpr ::= [Expr]
 *		 |  ap(ap(Operator,InfixExpr),Expr)
 *
 * thus x0 +1 x1 ... +n xn is parsed as: +n (....(+1 [x0] x1)....) xn
 *
 * Once the expression has been completely parsed, this parsed form is
 * `tidied' according to the precedences and associativities declared for
 * each operator symbol.
 *
 * The tidy process uses a `stack' of type:
 *    TidyStack ::= ap(ap(Operator,TidyStack),Expr)
 *		 |  NIL
 * when the ith layer of an InfixExpr has been transferred to the stack, the
 * stack is of the form: +i (....(+n NIL xn)....) xi
 *
 * The tidy function is based on a simple shift-reduce parser:
 *
 *  tidy                :: InfixExpr -> TidyStack -> Expr
 *  tidy [m]   ss        = foldl (\x f-> f x) m ss
 *  tidy (m*n) []        = tidy m [(*n)]
 *  tidy (m*n) ((+o):ss)
 *	       | amb     = error "Ambiguous"
 *	       | shift   = tidy m ((*n):(+o):ss)
 *	       | reduce  = tidy (m*(n+o)) ss
 *			   where sye     = syntaxOf (*)
 *				 (ae,pe) = sye
 *				 sys     = syntaxOf (+)
 *				 (as,ps) = sys
 *				 amb     = pe==ps && (ae/=as || ae==NON_ASS)
 *				 shift   = pe>ps || (ps==pe && ae==LEFT_ASS)
 *				 reduce  = otherwise
 *
 * N.B. the conditions amb, shift, reduce are NOT mutually exclusive and
 * must be tested in that order.
 *
 * As a concession to efficiency, we lower the number of calls to syntaxOf
 * by keeping track of the values of sye, sys throughout the process.  The
 * value APPLIC is used to indicate that the syntax value is unknown.
 */

static Cell local tidyInfix(e)         /* convert InfixExpr to Expr        */
Cell e; {                              /* :: InfixExpr                     */
    Cell   s   = NIL;                  /* :: TidyStack                     */
    Syntax sye = APPLIC;               /* Syntax of op in e (init unknown) */
    Syntax sys = APPLIC;               /* Syntax of op in s (init unknown) */
    Cell   temp;

    while (nonNull(tl(e))) {
        if (isNull(s)) {
            s           = e;
            e           = arg(fun(s));
            arg(fun(s)) = NIL;
            sys         = sye;
            sye         = APPLIC;
        }
        else {
            if (sye==APPLIC) {         /* calculate sye (if unknown)       */
                sye = syntaxOf(textOf(fun(fun(e))));
                if (sye==APPLIC) sye=DEF_OPSYNTAX;
            }
            if (sys==APPLIC) {         /* calculate sys (if unknown)       */
                sys = syntaxOf(textOf(fun(fun(s))));
                if (sys==APPLIC) sys=DEF_OPSYNTAX;
            }

            if (precOf(sye)==precOf(sys) &&                      /* amb    */
                   (assocOf(sye)!=assocOf(sys) || assocOf(sye)==NON_ASS)) {
                ERROR(row) "Ambiguous use of operator \"%s\" with \"%s\"",
                           textToStr(textOf(fun(fun(e)))),
                           textToStr(textOf(fun(fun(s))))
                EEND;
            }
            else if (precOf(sye)>precOf(sys) ||                  /* shift  */
                       (precOf(sye)==precOf(sys) && assocOf(sye)==LEFT_ASS)) {
                temp        = arg(fun(e));
                arg(fun(e)) = s;
                s           = e;
                e           = temp;
                sys         = sye;
                sye         = APPLIC;
            }
            else {                                               /* reduce */
                temp        = arg(fun(s));
                arg(fun(s)) = arg(e);
                arg(e)      = s;
                s           = temp;
                sys         = APPLIC;
                /* sye unchanged */
            }
        }
    }

    e = hd(e);
    while (nonNull(s)) {
        temp        = arg(fun(s));
        arg(fun(s)) = e;
        e           = s;
        s           = temp;
    }

    return e;
}

/*-------------------------------------------------------------------------*/
