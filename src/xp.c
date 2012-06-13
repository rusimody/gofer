#ifndef lint
#define lint
#endif

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "command.h"
#include "errors.h"
#include "input.h"
#include "parser.h"
#include <ctype.h>

#define defTycon(n,l,lhs,rhs,w)	 tyconDefn(intOf(l),lhs,rhs,w); sp-=n
#define sigdecl(l,vs,t)		 ap(SIGDECL,triple(l,vs,t))
#define grded(gs)		 ap(GUARDED,gs)
#define letrec(bs,e)		 (nonNull(bs) ? ap(LETREC,pair(bs,e)) : e)
#define yyerror(s)		 /* errors handled elsewhere */
#define YYSTYPE			 Cell

/*RPM*/
#define opap(lArg,op,rArg) (ap2(op,lArg,rArg))
#define opapd(lArg,op,rArg) (newSyntax && isDot(op) ? ap(lArg,rArg) :\
		    opap(lArg,op,rArg))
static Void local typeSyntaxChk(String context, Cell token);
static Cell local ct1Const(int line, Cell type, Cell conid);
static Cell local ct1Clause(int line, Cell type, Cell conids);
static Bool local tEquals(int line, Cell t1, Cell t2);
static Bool local isArrow(Cell x);
/*RPM*/

static Cell   local gcShadow     Args((Int,Cell));
Void   syntaxError  Args((String));
static String local unexpected   Args((Void));
static Cell   local checkPrec    Args((Cell));
static Void   local fixDefn      Args((Syntax,Cell,Cell,List));
static Void   local setSyntax    Args((Int,Syntax,Cell));
static Cell   local buildTuple   Args((List));
static Cell   local checkClass   Args((Cell));
static List   local checkContext Args((List));
static Pair   local checkDo	 Args((List));
static Cell   local checkTyLhs	 Args((Cell));
static Cell   local oTidyInfix    Args((Cell));
static Cell   local nTidyInfix    Args((Cell));
static Cell   local (*tidyInfix[2]) Args((Cell)) = {oTidyInfix, nTidyInfix};

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

# define EVALEX 257
# define SCRIPT 258
# define INFIXL 259
# define INFIXR 260
# define INFIX 261
# define FUNARROW 262
# define UPTO 263
# define CASEXP 264
# define OF 265
# define IF 266
# define THEN 267
# define ELSE 268
# define WHERE 269
# define TYPE 270
# define DATA 271
# define FROM 272
# define LET 273
# define IN 274
# define VAROP 275
# define VARID 276
# define NUMLIT 277
# define CHARLIT 278
# define STRINGLIT 279
# define REPEAT 280
# define CONOP 281
# define CONID 282
# define TCLASS 283
# define IMPLIES 284
# define TINSTANCE 285
# define DO 286
# define TRUNST 287
# define PRIMITIVE 288
# define DEFAULT 289
# define DERIVING 290
# define HIDING 291
# define IMPORT 292
# define INTERFACE 293
# define MODULE 294
# define RENAMING 295
# define TO 296
# define CTYPE 297

#include <malloc.h>
#include <memory.h>
#include <values.h>

#ifdef __cplusplus

#ifndef yyerror
	void yyerror(const char *);
#endif
#ifndef yylex
	int yylex(void);
#endif
	int yyparse(void);

#endif
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern int yyerrflag;
#ifndef YYSTYPE
#define YYSTYPE int
#endif
YYSTYPE yylval;
YYSTYPE yyval;
typedef int yytabelem;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
#if YYMAXDEPTH > 0
int yy_yys[YYMAXDEPTH], *yys = yy_yys;
YYSTYPE yy_yyv[YYMAXDEPTH], *yyv = yy_yyv;
#else	/* user does initial allocation */
int *yys;
YYSTYPE *yyv;
#endif
static int yymaxdepth = YYMAXDEPTH;
# define YYERRCODE 256


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

Void syntaxError(s)       /* report on syntax error           */
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
	case CTYPE     : keyword("ctype");
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
	case ':'      :
	   sprintf(buffer,"`%s'", typeStr[newSyntax]);
	   return buffer;
	case '-'       : return "`-'";
	case ','       : return "comma";
	case '@'       : return "`@'";
	case '('       : return "`('";
	case ')'       : return "`)'";
	case '|'       : return "`|'";
	case ';'       : return "separator";
	case UPTO      :
	   sprintf(buffer,"`%s'", uptoStr[newSyntax]);
	   return buffer;
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
	case IMPLIES   : return "`=>'";
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

static Cell local oTidyInfix(e)         /* convert InfixExpr to Expr        */
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
#ifdef __GNUC__
inline 
#endif
static Void local sc(e) /*short circuit dot applications */
Cell e;
{
  Cell temp;
  temp = fun(e);
  if (isDot(fun(temp)))
     fun(e) = arg(temp);
}

static Cell local nTidyInfix(e)         /* convert InfixExpr to Expr        */
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
		sc(arg(e)); /*RPM*/
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
	sc(e); /*RPM*/
    }

    return e;
}

/*RPM*/
/*Arunachala Siva Arunachala Ramana*/
#define lType(x) (snd(fst(x)))
#define rType(x) (snd(x))
static Bool local isArrow(Cell x)
{
  return isPair(x) && isPair(fst(x)) && fst(fst(x)) == ARROW;
}
static Bool local tEquals(int line, Cell t1, Cell t2)
/*Assumes Conid ('.' Varid)*  format*/
{
  if (whatIs(t1) == AP && whatIs(t2) == AP)
    return isVar(arg(t1)) && isVar(arg(t2)) &&
                 textOf(arg(t1)) == textOf(arg(t2)) &&
		 tEquals(line, fun(t1), fun(t2));
  else
    return isCon(t1) && isCon(t2) && textOf(t1) == textOf(t2);
}
static Cell local ct1Clause(int line, Cell type, Cell conids)
{
  push(NIL);
  for(; nonNull(conids); conids=tl(conids))
    top() = cons(ct1Const(line, type, hd(conids)), top());

  return pop();
}
static Cell local ct1Const(int line, Cell type, Cell conid)
{
  push(conid);
  for (; isArrow(type); type = rType(type))
    top() = ap(top(), lType(type));

  if (!tEquals(line, typeLhs, type))
  {
    ERROR(line) "ctype target " ETHEN
    ERREXPR(type);
    ERRTEXT " must match header\n" EEND;
  }
  return pop();
}

static Void local typeSyntaxChk(String context, Cell token)
{
  if (newSyntax && !isDot(token))
  {
    ERROR(row) "Syntax Error in %s (application expected)", context EEND;
  }
  else if (!newSyntax && !isNull(token))
  {
     ERROR(row) "Syntax error in %s (unexpected %s)", context,
     textToStr(textOf(token))
     EEND;
  }
}
yytabelem yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 27,
	93, 172,
	-2, 0,
-1, 83,
	44, 117,
	58, 117,
	-2, 143,
-1, 87,
	284, 49,
	-2, 48,
-1, 88,
	40, 18,
	91, 18,
	276, 18,
	282, 18,
	-2, 50,
-1, 119,
	93, 178,
	-2, 0,
-1, 125,
	284, 49,
	-2, 19,
-1, 143,
	284, 49,
	-2, 92,
-1, 163,
	44, 50,
	41, 50,
	-2, 18,
-1, 238,
	93, 177,
	-2, 0,
-1, 244,
	276, 18,
	-2, 21,
-1, 250,
	96, 52,
	281, 52,
	-2, 39,
	};
# define YYNPROD 190
# define YYLAST 821
yytabelem yyact[]={

   120,    85,   102,   185,   312,    87,     5,   305,   292,     6,
    40,   285,   284,    18,   306,   122,   208,    49,    50,   247,
    21,   248,   196,   192,   156,   129,    59,   348,   309,   334,
    97,    65,    93,   160,   316,    69,   166,    93,   262,   328,
   203,    96,    82,   133,   288,    93,    83,    97,   246,   159,
   225,   128,   157,   233,   140,   283,    34,   273,    82,   103,
   310,    42,    83,   159,   289,   159,    93,    35,   243,   204,
   210,   207,   309,   234,   236,   104,   238,   125,   323,   130,
   193,    33,   140,    94,   143,   143,   145,   197,    94,   329,
    90,   139,   241,   194,    82,   274,    94,   216,    83,   162,
   167,   274,   174,   172,     4,     2,     3,   175,   324,   151,
   153,   184,   280,   257,   177,   230,   178,    94,   137,   168,
   116,   182,   117,   187,   189,   173,   171,   320,   267,    39,
   255,    82,   242,    22,   176,    83,    84,   190,    10,    48,
   170,   121,   260,    53,   195,   327,   141,    88,   211,   331,
   202,   346,    63,   147,    36,   214,   218,   123,   217,   326,
   154,   212,   219,   220,   201,   101,   228,   146,   226,   229,
   330,   227,   154,    22,   123,   258,   232,   286,    10,   252,
   239,   112,   158,    82,    27,    11,   237,    83,    20,   190,
   345,   123,   108,   123,   123,    68,   107,   223,   127,   245,
   224,   249,   118,   253,   106,   111,   213,   123,   112,    93,
   183,   222,   109,   181,   256,   110,   139,   180,   123,    19,
   259,    93,   199,   200,    27,    11,   144,   272,    20,    43,
   261,   220,    38,   271,    22,   263,   266,   264,   265,    10,
    41,   163,   275,   270,   277,   278,    43,   269,    89,   221,
   155,   268,    82,    89,   161,    62,    83,   158,   293,    19,
    94,    22,   297,   215,   298,    82,    10,    82,    91,    83,
    29,    83,    94,    91,    92,   302,   315,   237,   159,    92,
   304,    91,    89,   190,   303,    27,    11,    92,   124,    20,
    30,   249,   319,   317,   159,   249,   321,   313,   138,   205,
   322,   308,    91,   240,   150,   124,   325,   314,    92,   318,
    22,    64,    27,    11,    29,    10,    20,   188,    30,   123,
    19,   254,   124,   293,   124,   124,   300,   338,   301,   336,
    82,   335,   340,   344,    83,   343,   190,   342,   124,   339,
   190,   119,   299,   347,   313,   308,   123,    19,   341,   124,
   308,   276,    74,    75,    76,   235,    46,    14,    60,    13,
     9,    27,    11,    72,    73,    20,    12,    28,   152,    30,
    23,    24,    25,    26,   149,    29,    78,   148,    79,   287,
    16,    77,    80,    22,    58,    81,    66,   291,    55,    70,
    71,   244,    74,    75,    76,   251,    19,    14,    22,    13,
   209,   206,   136,    72,    73,   165,    12,   164,   279,    30,
    23,    24,    25,    26,   333,    29,    78,     8,    79,   332,
    16,    77,    80,   295,   311,    89,   282,    22,   281,   191,
    71,   307,    10,   198,    27,    11,    15,   250,    20,    42,
    61,    67,    37,    44,    86,    91,    32,    45,    31,    27,
   337,    92,     1,    20,    22,    95,     0,    91,    14,    10,
    13,     0,    98,    92,     0,     0,     0,    12,   132,    19,
    30,    23,    24,    25,    26,     0,    29,   124,    27,    11,
     0,    16,    20,     0,    19,    14,    22,    13,     0,     0,
     0,    10,    45,     0,    12,   113,     0,    30,    23,    24,
    25,    26,     0,    29,     0,    27,    11,     0,    16,    20,
    22,   179,     0,    19,     0,    10,   126,     0,   131,     0,
     0,     0,     0,   142,   142,     0,     7,    22,     0,   169,
     0,    95,    10,     0,    14,     0,    13,    27,    11,     0,
    19,    20,    22,    12,   134,   135,    30,    23,    24,    25,
    26,     0,    29,    22,   106,     0,     0,    16,     0,     0,
     0,    27,    11,     0,     0,    20,     0,     0,     0,     0,
     0,     0,    19,     0,     0,     0,    22,     0,    27,    11,
     0,     0,    20,     0,     0,     0,     0,   231,     0,     0,
     0,     0,     0,    27,     0,     0,    19,    20,     0,     7,
     0,     0,     0,     0,    27,     0,     0,    14,    20,    13,
     0,     0,     0,    19,     0,     0,    12,     0,    56,    30,
    23,    24,    25,    26,    57,    29,     0,    27,    19,     0,
    16,    20,     0,     0,    30,    23,    24,    25,    26,    19,
    29,     0,     0,     7,     0,    16,     0,     0,     0,     0,
     0,    14,     0,   296,     0,     0,     0,     0,     0,     0,
    12,     0,    19,    30,    23,    24,    25,    26,     0,    29,
   294,     0,     0,     0,    16,     0,     0,     0,    14,     0,
    13,     0,     0,     0,     0,     0,   290,    12,     0,     0,
    30,    23,    24,    25,    26,     0,    29,     0,     0,     0,
     0,    16,     7,     0,     0,     0,     0,     0,     0,     0,
    14,     0,    13,     0,     0,     0,     0,     0,     0,   186,
     0,     0,    30,    23,    24,    25,    26,     0,    29,     0,
     0,     0,     0,    16,    14,     0,    13,     0,     0,     0,
     0,     0,     0,    12,     0,     0,    30,    23,    24,    25,
    26,    14,    29,    13,     0,     0,    17,    16,     0,     0,
    12,     0,     0,    30,    23,    24,    25,    26,    47,    29,
     0,     0,    51,    52,    16,     0,    54,     0,    30,    23,
    24,    25,    26,     0,    29,     0,     0,     0,     0,    30,
    23,    24,    25,    26,     0,    29,     0,     0,    99,     0,
    16,     0,    51,   100,     0,     0,     0,     0,     0,     0,
   105,     0,    30,    23,    24,    25,    26,     0,    29,   114,
   115 };
yytabelem yypact[]={

  -152,-32768,   270,  -175,-32768,  -202,    96,-32768,   -35,   -35,
   358,   502,    16,   270,   270,   502,   502,-32768,    79,   502,
-32768,-32768,   343,-32768,-32768,-32768,-32768,   270,-32768,-32768,
-32768,-32768,   133,-32768,-32768,    13,   169,   487,-32768,-32768,
-32768,-32768,  -235,-32768,   487,   502,   536,-32768,   487,  -208,
  -190,-32768,-32768,   502,-32768,   513,   155,   151,-32768,   171,
   164,   -35,   502,   502,    27,    78,   137,    82,-32768,-32768,
-32768,   169,  -231,   169,  -234,  -234,  -234,    42,   169,   169,
   169,   109,    48,    79,   487,-32768,  -260,-32768,  -210,-32768,
-32768,-32768,-32768,    -8,    26,   -35,    44,    30,-32768,   270,
-32768,    66,-32768,   270,    11,-32768,-32768,-32768,-32768,-32768,
   270,-32768,   270,   470,   176,   172,-32768,   270,   446,   270,
-32768,    93,-32768,-32768,-32768,-32768,  -261,    19,-32768,-32768,
    83,  -262,   -35,-32768,   -35,   -35,   106,-32768,-32768,  -239,
    24,  -198,  -268,-32768,  -199,-32768,   169,    14,-32768,  -202,
-32768,   270,    36,-32768,   270,    66,   169,   169,     5,-32768,
-32768,   170,   156,  -212,   127,   125,-32768,    22,-32768,   487,
-32768,-32768,-32768,   221,  -221,  -195,   487,-32768,-32768,-32768,
-32768,-32768,  -187,   136,-32768,    31,     9,-32768,-32768,-32768,
-32768,  -201,  -231,   169,  -228,   181,  -231,   135,-32768,   135,
   135,   169,    14,-32768,   163,   155,-32768,     7,   169,-32768,
   -10,-32768,-32768,-32768,   131,-32768,   270,    81,-32768,-32768,
-32768,-32768,-32768,-32768,   169,    -3,-32768,-32768,-32768,   169,
-32768,-32768,-32768,   270,   270,    69,-32768,   -29,   270,   446,
   270,   270,   487,   -11,  -226,  -219,-32768,  -113,-32768,   -52,
-32768,     3,   -35,-32768,-32768,   414,-32768,   487,   387,   131,
   270,-32768,-32768,-32768,-32768,-32768,-32768,   221,-32768,  -202,
   -23,   270,-32768,-32768,   487,-32768,-32768,-32768,-32768,    66,
   -12,  -209,-32768,    14,-32768,   181,    -6,   169,  -252,   181,
-32768,    68,-32768,-32768,-32768,    66,   270,-32768,-32768,-32768,
-32768,-32768,-32768,  -184,  -221,    49,-32768,   101,-32768,  -242,
   -34,   126,-32768,    91,-32768,-32768,  -253,-32768,  -113,-32768,
   194,-32768,  -208,   270,    32,-32768,   169,   -12,   151,   -12,
    14,   169,   149,   107,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,    49,-32768,-32768,-32768,  -255,-32768,-32768 };
yytabelem yypgo[]={

     0,   452,     3,    56,   448,   446,   441,     0,   195,     2,
     7,    14,   431,     5,    20,    93,   429,   444,   198,   428,
   426,    19,    12,   424,     4,    13,     1,    21,   129,   419,
   414,   147,    90,   407,   405,   468,    87,   433,   232,   402,
   118,   146,   401,   400,   387,   165,     8,   385,     9,   377,
   374,   368,   110,   367,   417,   360,   436,   356,   355,   756,
   358,   311,    74,   251,   247,   243,    57,   210,   111,    15 };
yytabelem yyr1[]={

     0,     1,     1,     1,     1,     4,     5,     6,     6,     6,
     6,     6,    10,    10,    11,    12,    12,    15,    15,    16,
     8,    19,     8,     8,     8,     8,    18,    18,    18,    20,
    20,    23,    23,    24,    24,    21,    21,    27,    27,    27,
    22,    22,    22,    29,    29,    30,    30,    26,    26,    17,
    13,    13,    13,    31,    31,    32,    32,    32,    32,    32,
    32,    32,    32,    32,    32,    33,    33,    34,    34,     8,
     8,     8,    35,    35,    36,    36,    37,    37,    37,    38,
    38,    28,    28,     8,    39,    39,    39,    40,     8,     8,
     8,    41,    41,    42,    42,    43,    43,    44,    44,    46,
    46,     9,     9,    45,    45,    49,    49,    49,    50,    50,
     3,    51,    51,    52,    52,    52,    47,    47,    25,    25,
    53,    53,    14,    14,     2,     2,     2,    48,    48,    48,
    55,    55,    54,    54,    54,    54,    54,    54,    57,    57,
    56,    56,    56,    59,    59,    59,    59,    59,    59,    59,
    59,    59,    59,    59,    59,    59,    59,    59,    59,    60,
    60,    58,    58,    62,    63,    63,    64,    64,    64,    65,
    65,    66,    61,    61,    61,    61,    61,    61,    61,    61,
    67,    67,    68,    68,    68,    68,     7,     7,    69,    69 };
yytabelem yyr2[]={

     0,     5,     7,     5,     3,     7,     3,     7,     7,     3,
     3,     3,     7,     3,     7,     7,     3,     3,     1,     1,
    15,     1,    19,    11,    11,    15,     7,     3,     3,     5,
     1,     7,     3,     7,     3,     7,     3,     7,     3,     3,
     1,     5,     9,     1,     3,     7,     3,     7,     3,     3,
     3,     7,     3,     7,     3,     3,     3,     5,     7,     7,
     9,     7,     7,     7,     5,     5,     3,     7,     7,     7,
     7,     7,     3,     1,     7,     3,     3,     3,     3,     3,
     7,     3,     7,     9,     7,     3,     3,     5,     7,     7,
     5,     7,     3,     9,     1,     9,     1,     7,     3,     3,
     3,     7,     5,     7,     3,     5,     3,     3,     5,     3,
     9,     5,     3,     9,    11,     9,     7,     3,     3,     7,
     3,     7,     3,     7,     7,     3,     3,     3,     7,     3,
     7,    11,     5,     9,    13,    13,    13,     3,     5,     3,
     5,     5,     3,     3,     7,     5,     3,     3,     5,     3,
     3,     3,     3,     7,     7,     7,     9,     9,     9,     7,
     7,     7,     3,     5,     5,     3,     3,     5,     3,     5,
     3,     9,     1,     3,     3,     7,     7,     9,     5,    11,
     7,     3,     7,     7,     3,     9,     5,     3,     3,     3 };
yytabelem yychk[]={

-32768,    -1,   257,   258,   256,    -2,   -48,   256,   -54,   -55,
    45,    92,   273,   266,   264,   -56,   287,   -59,   -25,   126,
    95,   -14,    40,   277,   278,   279,   280,    91,   -53,   282,
   276,    -4,    -5,   256,    -3,   269,    58,   -37,   -38,   -28,
    45,   275,    96,   281,   -37,   -56,   -57,   -59,   123,    -2,
    -2,   -59,   -59,    64,   -59,    45,   275,   281,    41,    -2,
   -60,   -54,   -38,   -28,   -61,    -2,   -60,    -6,    -8,    -9,
   256,   297,   270,   271,   259,   260,   261,   288,   283,   285,
   289,   -47,   -48,   -25,   123,   -26,   -17,   -13,   -31,   256,
   -32,   276,   282,    40,    91,   -54,   276,   282,   -54,   262,
   -59,   -45,    -9,   267,   265,   -59,    41,    41,    41,    41,
    44,    41,    44,   -37,   -59,   -59,    93,    44,   124,   263,
    -7,    59,   -69,   125,   256,   -13,   -17,   -18,   282,   256,
   -13,   -17,   -35,   277,   -35,   -35,   -39,   -40,   256,   -25,
    40,   -41,   -17,   -13,   -41,   -13,    58,    44,   -49,   -50,
   256,    61,   -51,   -52,   124,   -45,   284,   262,   -15,   275,
    41,   262,   -13,   -31,   -33,   -34,    44,   -13,    93,   -37,
    96,    96,    -2,    59,    -7,    -2,   123,    -2,    -2,    41,
    41,    41,    -2,   -67,   -68,    -2,   273,    -2,    -8,    -9,
   -69,   -16,   284,    61,   -15,    61,   284,   -36,   -37,   -36,
   -36,    58,    44,   279,    45,   275,   -42,   269,   284,   -43,
   269,   -26,   -25,    -3,    -2,   -52,    61,    -2,    -7,   -13,
   -13,   -32,    41,    41,    44,   262,    41,    44,    41,    44,
    93,   -54,    -9,   274,   268,   -58,   -62,   -48,   263,    44,
   272,    61,   123,   269,   -18,   -13,   276,   -21,   -27,   -13,
   256,   -18,    44,   -26,   -40,   123,   -13,   123,    44,    -2,
    61,   -13,    41,   -13,    -2,    -2,    -7,    59,   -63,   -64,
   -65,   262,   256,   -66,   124,    -2,   -68,    -2,    -2,   -45,
   123,   -19,   -20,   274,   -22,   124,   290,   -28,    96,    61,
   -37,   -44,   -46,    -9,   256,   -45,   266,    -2,    -2,   -62,
    -3,   -66,    -2,   -48,    -7,   -10,   -11,   -12,   -14,    40,
   269,   -23,   -24,   -25,   -27,   282,    40,   -13,   -21,    -7,
    59,    -7,    -2,   262,    59,    -7,    58,    44,   281,   123,
    44,    58,   -29,   -30,   282,   -22,   -46,   256,    -2,   -11,
   -13,   -14,   -10,   -24,   -26,    41,    44,    -7,   282 };
yytabelem yydef[]={

     0,    -2,     0,     0,     4,     1,   125,   126,   127,   129,
     0,     0,     0,     0,     0,   137,     0,   142,   143,     0,
   146,   147,     0,   149,   150,   151,   152,    -2,   118,   122,
   120,     3,     0,     6,     2,     0,     0,     0,    76,    77,
    78,    79,     0,    81,     0,   132,     0,   139,     0,     0,
     0,   140,   141,     0,   145,     0,    79,    81,   148,     0,
     0,   127,     0,     0,     0,   173,   174,     0,     9,    10,
    11,     0,     0,     0,    73,    73,    73,     0,     0,     0,
     0,     0,     0,    -2,     0,   124,     0,    -2,    -2,    52,
    54,    55,    56,     0,     0,   128,     0,     0,   130,     0,
   138,     0,   104,     0,     0,   144,   119,   121,   123,   153,
     0,   154,     0,     0,     0,     0,   155,     0,     0,    -2,
     5,     0,   187,   188,   189,    -2,     0,    18,    27,    28,
    49,     0,     0,    72,     0,     0,     0,    85,    86,     0,
     0,    94,     0,    -2,    96,    90,     0,     0,   102,   106,
   107,     0,   109,   112,     0,     0,     0,     0,     0,    17,
    57,     0,     0,    -2,     0,     0,    66,     0,    64,     0,
    80,    82,   133,     0,     0,     0,     0,   160,   159,   156,
   157,   158,   160,   175,   181,   184,     0,   176,     7,     8,
   186,     0,     0,     0,     0,     0,     0,    69,    75,    70,
    71,     0,     0,    87,     0,     0,    88,     0,     0,    89,
     0,   101,   116,   105,   108,   111,     0,     0,   110,    47,
    51,    53,    58,    59,     0,     0,    61,    65,    62,     0,
    63,   131,   103,     0,     0,     0,   162,     0,    -2,     0,
     0,     0,     0,     0,    -2,    30,    26,    40,    36,    38,
    -2,    18,     0,    83,    84,     0,    91,     0,     0,     0,
     0,    68,    60,    67,   134,   135,   136,     0,   163,   165,
   166,     0,   168,   170,     0,   179,   180,   182,   183,     0,
     0,     0,    23,     0,    24,     0,     0,     0,     0,     0,
    74,     0,    98,    99,   100,     0,     0,   115,   113,   161,
   164,   169,   167,     0,   185,     0,    13,     0,    16,     0,
     0,    29,    32,    34,    35,    41,    43,    37,    40,    93,
     0,    95,   114,     0,     0,    20,     0,     0,     0,     0,
     0,     0,     0,    44,    46,    25,    97,   100,   171,    12,
    14,    15,     0,    31,    33,    42,     0,    22,    45 };
typedef struct
#ifdef __cplusplus
	yytoktype
#endif
{ char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	0	/* don't allow debugging */
#endif

#if YYDEBUG

yytoktype yytoks[] =
{
	"EVALEX",	257,
	"SCRIPT",	258,
	"=",	61,
	"INFIXL",	259,
	"INFIXR",	260,
	"INFIX",	261,
	"FUNARROW",	262,
	"-",	45,
	",",	44,
	"@",	64,
	"(",	40,
	")",	41,
	"|",	124,
	";",	59,
	"UPTO",	263,
	"[",	91,
	"]",	93,
	"CASEXP",	264,
	"OF",	265,
	"IF",	266,
	"THEN",	267,
	"ELSE",	268,
	"WHERE",	269,
	"TYPE",	270,
	"DATA",	271,
	"FROM",	272,
	"\\",	92,
	"~",	126,
	"LET",	273,
	"IN",	274,
	"`",	96,
	"VAROP",	275,
	"VARID",	276,
	"NUMLIT",	277,
	"CHARLIT",	278,
	"STRINGLIT",	279,
	"REPEAT",	280,
	"CONOP",	281,
	"CONID",	282,
	"TCLASS",	283,
	"IMPLIES",	284,
	"TINSTANCE",	285,
	"DO",	286,
	"TRUNST",	287,
	"PRIMITIVE",	288,
	"DEFAULT",	289,
	"DERIVING",	290,
	"HIDING",	291,
	"IMPORT",	292,
	"INTERFACE",	293,
	"MODULE",	294,
	"RENAMING",	295,
	"TO",	296,
	"CTYPE",	297,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"start : EVALEX exp",
	"start : EVALEX exp wherePart",
	"start : SCRIPT topModule",
	"start : error",
	"topModule : begin topDecls close",
	"begin : error",
	"topDecls : topDecls ';' topDecl",
	"topDecls : topDecls ';' decl",
	"topDecls : topDecl",
	"topDecls : decl",
	"topDecls : error",
	"ctDecls : ctDecls ';' ctDecl",
	"ctDecls : ctDecl",
	"ctDecl : conids ':' type",
	"conids : conids ',' conid",
	"conids : conid",
	"pvarop : VAROP",
	"pvarop : /* empty */",
	"topDecl : CTYPE type",
	"topDecl : CTYPE type WHERE '{' ctDecls close",
	"topDecl : CTYPE context IMPLIES typeLhs",
	"topDecl : CTYPE context IMPLIES typeLhs WHERE '{' ctDecls close",
	"topDecl : TYPE typeLhs '=' type invars",
	"topDecl : DATA type '=' constrs deriving",
	"topDecl : DATA context IMPLIES typeLhs '=' constrs deriving",
	"typeLhs : typeLhs pvarop VARID",
	"typeLhs : CONID",
	"typeLhs : error",
	"invars : IN rsvars",
	"invars : /* empty */",
	"rsvars : rsvars ',' rsvar",
	"rsvars : rsvar",
	"rsvar : var ':' sigType",
	"rsvar : var",
	"constrs : constrs '|' constr",
	"constrs : constr",
	"constr : type conop type",
	"constr : type",
	"constr : error",
	"deriving : /* empty */",
	"deriving : DERIVING CONID",
	"deriving : DERIVING '(' derivs0 ')'",
	"derivs0 : /* empty */",
	"derivs0 : derivs",
	"derivs : derivs ',' CONID",
	"derivs : CONID",
	"sigType : context IMPLIES type",
	"sigType : type",
	"context : type",
	"type : ctype",
	"type : ctype FUNARROW type",
	"type : error",
	"ctype : ctype pvarop atype",
	"ctype : atype",
	"atype : VARID",
	"atype : CONID",
	"atype : '(' ')'",
	"atype : '(' FUNARROW ')'",
	"atype : '(' type ')'",
	"atype : '(' ctype FUNARROW ')'",
	"atype : '(' tupCommas ')'",
	"atype : '(' typeTuple ')'",
	"atype : '[' type ']'",
	"atype : '[' ']'",
	"tupCommas : tupCommas ','",
	"tupCommas : ','",
	"typeTuple : typeTuple ',' type",
	"typeTuple : type ',' type",
	"topDecl : INFIXL optdigit ops",
	"topDecl : INFIXR optdigit ops",
	"topDecl : INFIX optdigit ops",
	"optdigit : NUMLIT",
	"optdigit : /* empty */",
	"ops : ops ',' op",
	"ops : op",
	"op : varop",
	"op : conop",
	"op : '-'",
	"varop : VAROP",
	"varop : '`' VARID '`'",
	"conop : CONOP",
	"conop : '`' CONID '`'",
	"topDecl : PRIMITIVE prims ':' sigType",
	"prims : prims ',' prim",
	"prims : prim",
	"prims : error",
	"prim : var STRINGLIT",
	"topDecl : TCLASS classHead classBody",
	"topDecl : TINSTANCE classHead instBody",
	"topDecl : DEFAULT type",
	"classHead : context IMPLIES type",
	"classHead : type",
	"classBody : WHERE '{' csigdecls close",
	"classBody : /* empty */",
	"instBody : WHERE '{' decls close",
	"instBody : /* empty */",
	"csigdecls : csigdecls ';' csigdecl",
	"csigdecls : csigdecl",
	"csigdecl : decl",
	"csigdecl : error",
	"decl : vars ':' sigType",
	"decl : opExp rhs",
	"decls : decls ';' decl",
	"decls : decl",
	"rhs : rhs1 wherePart",
	"rhs : rhs1",
	"rhs : error",
	"rhs1 : '=' exp",
	"rhs1 : gdefs",
	"wherePart : WHERE '{' decls close",
	"gdefs : gdefs gdef",
	"gdefs : gdef",
	"gdef : '|' exp '=' exp",
	"gdef : '=' exp ',' IF exp",
	"gdef : '=' exp ',' exp",
	"vars : vars ',' var",
	"vars : var",
	"var : varid",
	"var : '(' '-' ')'",
	"varid : VARID",
	"varid : '(' VAROP ')'",
	"conid : CONID",
	"conid : '(' CONOP ')'",
	"exp : opExp ':' sigType",
	"exp : opExp",
	"exp : error",
	"opExp : pfxExp",
	"opExp : pfxExp op pfxExp",
	"opExp : opExp0",
	"opExp0 : opExp0 op pfxExp",
	"opExp0 : pfxExp op pfxExp op pfxExp",
	"pfxExp : '-' appExp",
	"pfxExp : '\\' pats FUNARROW exp",
	"pfxExp : LET '{' decls close IN exp",
	"pfxExp : IF exp THEN exp ELSE exp",
	"pfxExp : CASEXP exp OF '{' alts close",
	"pfxExp : appExp",
	"pats : pats atomic",
	"pats : atomic",
	"appExp : appExp atomic",
	"appExp : TRUNST atomic",
	"appExp : atomic",
	"atomic : var",
	"atomic : var '@' atomic",
	"atomic : '~' atomic",
	"atomic : '_'",
	"atomic : conid",
	"atomic : '(' ')'",
	"atomic : NUMLIT",
	"atomic : CHARLIT",
	"atomic : STRINGLIT",
	"atomic : REPEAT",
	"atomic : '(' exp ')'",
	"atomic : '(' exps2 ')'",
	"atomic : '[' list ']'",
	"atomic : '(' pfxExp op ')'",
	"atomic : '(' varop atomic ')'",
	"atomic : '(' conop atomic ')'",
	"exps2 : exps2 ',' exp",
	"exps2 : exp ',' exp",
	"alts : alts ';' alt",
	"alts : alt",
	"alt : opExp altRhs",
	"altRhs : altRhs1 wherePart",
	"altRhs : altRhs1",
	"altRhs1 : guardAlts",
	"altRhs1 : FUNARROW exp",
	"altRhs1 : error",
	"guardAlts : guardAlts guardAlt",
	"guardAlts : guardAlt",
	"guardAlt : '|' opExp FUNARROW exp",
	"list : /* empty */",
	"list : exp",
	"list : exps2",
	"list : exp '|' quals",
	"list : exp UPTO exp",
	"list : exp ',' exp UPTO",
	"list : exp UPTO",
	"list : exp ',' exp UPTO exp",
	"quals : quals ',' qual",
	"quals : qual",
	"qual : exp FROM exp",
	"qual : exp '=' exp",
	"qual : exp",
	"qual : LET '{' decls close",
	"close : ';' close1",
	"close : close1",
	"close1 : '}'",
	"close1 : error",
};
#endif /* YYDEBUG */
	/**********************************************************
	* Copyright (C) Data General Corporation, 1984 - 1990	  *
	* All Rights Reserved.					  *
	* Licensed Material-Property of Data General Corporation. *
	* This software is made available solely pursuant to the  *
	* terms of a DGC license agreement which governs its use. *
	**********************************************************/

/*  <@(#) yaccpar.c,v	5.1.1.1>  */

/* #ident	"@(#)yacc:yaccpar	1.17" */

/*
** Skeleton parser driver for yacc output
*/

/*
** yacc user known macros and defines
*/
#define YYERROR		goto yyerrlab
#define YYACCEPT	return(0)
#define YYABORT		return(1)
#define YYBACKUP( newtoken, newvalue )\
{\
	if ( yychar >= 0 || ( yyr2[ yytmp ] >> 1 ) != 1 )\
	{\
		yyerror( "syntax error - cannot backup" );\
		goto yyerrlab;\
	}\
	yychar = newtoken;\
	yystate = *yyps;\
	yylval = newvalue;\
	goto yynewstate;\
}
#define YYRECOVERING()	(!!yyerrflag)
#define YYNEW(type)	 malloc(sizeof(type) * yynewmax)
#define YYCOPY(to, from, type) \
	(type *) memcpy(to, (char *) from, yynewmax * sizeof(type))
#define YYENLARGE( from, type) \
	(type *) realloc((char *) from, yynewmax * sizeof(type))
#ifndef YYDEBUG
#	define YYDEBUG	1	/* make debugging available */
#endif

/*
** user known globals
*/
int yydebug;			/* set to 1 to get debugging */
int yysymdebug;			/* set to 1 to get debugging */

/*
** driver internal defines
*/
#define YYFLAG		(-32768)

/*
** global variables used by the parser
*/
YYSTYPE *yypv;			/* top of value stack */
int *yyps;			/* top of state stack */

int yystate;			/* current state */
int yytmp;			/* extra var (lasts between blocks) */

int yynerrs;			/* number of errors */
int yyerrflag;			/* error recovery flag */
int yychar;			/* current input token number */



/*
** yyparse - return 0 if worked, 1 if syntax error not recovered from
*/
#if defined(__STDC__) || defined(__cplusplus)
int yyparse(void)
#else
int yyparse()
#endif
{
	register YYSTYPE *yypvt;	/* top of value stack for $vars */

	/*
	** Initialize externals - yyparse may be called more than once
	*/
	yypv = &yyv[-1];
	yyps = &yys[-1];
	yystate = 0;
	yytmp = 0;
	yynerrs = 0;
	yyerrflag = 0;
	yychar = -1;

#if YYMAXDEPTH <= 0
	if (yymaxdepth <= 0)
	{
		if ((yymaxdepth = YYEXPAND(0)) <= 0)
		{
			yyerror("yacc initialization error");
			YYABORT;
		}
	}
#endif

	goto yystack;
	{
		register YYSTYPE *yy_pv;	/* top of value stack */
		register int *yy_ps;		/* top of state stack */
		register int yy_state;		/* current state */
		register int  yy_n;		/* internal state number info */

		/*
		** get globals into registers.
		** branch to here only if YYBACKUP was called.
		*/
	yynewstate:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;
		goto yy_newstate;

		/*
		** get globals into registers.
		** either we just started, or we just finished a reduction
		*/
	yystack:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;

		/*
		** top of for (;;) loop while no reductions done
		*/
	yy_stack:
		/*
		** put a state and value onto the stacks
		*/
#if YYDEBUG
		/*
		** if debugging, look up token value in list of value vs.
		** name pairs.  0 and negative (-1) are special values.
		** Note: linear search is used since time is not a real
		** consideration while debugging.
		*/
		if ( yydebug )
		{
			register int yy_i;

			printf( "State %d, token ", yy_state );
			if ( yychar == 0 )
				printf( "end-of-file\n" );
			else if ( yychar < 0 )
				printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ++yy_ps >= &yys[ yymaxdepth ] )	/* room on stack? */
		{
			int yynewmax, yys_off, yyv_off;
			int *yys_base = yys;
			YYSTYPE *yyv_base = yyv;
#ifdef YYEXPAND
			yynewmax = YYEXPAND(yymaxdepth);
#else
			yynewmax = 2 * yymaxdepth;	/* double table size */
			if (yymaxdepth == YYMAXDEPTH)	/* first time growth */
			{
				char *newyys = (char *) YYNEW(int);
				char *newyyv = (char *) YYNEW(YYSTYPE);
				if (newyys != 0 && newyyv != 0)
				{
					yys = YYCOPY(newyys, yys, int);
					yyv = YYCOPY(newyyv, yyv, YYSTYPE);
				}
				else
					yynewmax = 0;	/* failed */
			}
			else				/* not first time */
			{
				yys = YYENLARGE(yys, int);
				yyv = YYENLARGE(yyv, YYSTYPE);
				if (yys == 0 || yyv == 0)
					yynewmax = 0;	/* failed */
			}
#endif
			if (yynewmax <= yymaxdepth)	/* tables not expanded */
			{
				yyerror( "yacc stack overflow" );
				YYABORT;
			}
			yymaxdepth = yynewmax;

			/* reset pointers into yys */
			yys_off = yys - yys_base;
			yy_ps = yy_ps + yys_off;
			yyps = yyps + yys_off;

			/* reset pointers into yyv */
			yyv_off = yyv - yyv_base;
			yypvt = yypvt + yyv_off;
			yy_pv = yy_pv + yyv_off;
			yypv = yypv + yyv_off;
		}
		*yy_ps = yy_state;
		*++yy_pv = yyval;

		/*
		** we have a new state - find out what to do
		*/
	yy_newstate:
		if ( ( yy_n = yypact[ yy_state ] ) <= YYFLAG )
			goto yydefault;		/* simple state */
#if YYDEBUG
		/*
		** if debugging, need to mark whether new token grabbed
		*/
		yytmp = yychar < 0;
#endif
		if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
			yychar = 0;		/* reached EOF */
#if YYDEBUG
		if ( yydebug && yytmp )
		{
			register int yy_i;

			printf( "Received token " );
			if ( yychar == 0 )
				printf( "end-of-file\n" );
			else if ( yychar < 0 )
				printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ( ( yy_n += yychar ) < 0 ) || ( yy_n >= YYLAST ) )
			goto yydefault;
		if ( yychk[ yy_n = yyact[ yy_n ] ] == yychar )	/*valid shift*/
		{
			yychar = -1;
			yyval = yylval;
			yy_state = yy_n;
			if ( yyerrflag > 0 )
				yyerrflag--;
			goto yy_stack;
		}

	yydefault:
		if ( ( yy_n = yydef[ yy_state ] ) == -2 )
		{
#if YYDEBUG
			yytmp = yychar < 0;
#endif
			if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
				yychar = 0;		/* reached EOF */
#if YYDEBUG
			if ( yydebug && yytmp )
			{
				register int yy_i;

				printf( "Received token " );
				if ( yychar == 0 )
					printf( "end-of-file\n" );
				else if ( yychar < 0 )
					printf( "-none-\n" );
				else
				{
					for ( yy_i = 0;
						yytoks[yy_i].t_val >= 0;
						yy_i++ )
					{
						if ( yytoks[yy_i].t_val
							== yychar )
						{
							break;
						}
					}
					printf( "%s\n", yytoks[yy_i].t_name );
				}
			}
#endif /* YYDEBUG */
			/*
			** look through exception table
			*/
			{
				register int *yyxi = yyexca;

				while ( ( *yyxi != -1 ) ||
					( yyxi[1] != yy_state ) )
				{
					yyxi += 2;
				}
				while ( ( *(yyxi += 2) >= 0 ) &&
					( *yyxi != yychar ) )
					;
				if ( ( yy_n = yyxi[1] ) < 0 )
					YYACCEPT;
			}
		}

		/*
		** check for syntax error
		*/
		if ( yy_n == 0 )	/* have an error */
		{
			/* no worry about speed here! */
			switch ( yyerrflag )
			{
			case 0:		/* new error */
				yyerror( "syntax error" );
				goto skip_init;
			yyerrlab:
				/*
				** get globals into registers.
				** we have a user generated syntax type error
				*/
				yy_pv = yypv;
				yy_ps = yyps;
				yy_state = yystate;
				yynerrs++;
				/* FALLTHRU */
			skip_init:
			case 1:
			case 2:		/* incompletely recovered error */
					/* try again... */
				yyerrflag = 3;
				/*
				** find state where "error" is a legal
				** shift action
				*/
				while ( yy_ps >= yys )
				{
					yy_n = yypact[ *yy_ps ] + YYERRCODE;
					if ( yy_n >= 0 && yy_n < YYLAST &&
						yychk[yyact[yy_n]] == YYERRCODE)					{
						/*
						** simulate shift of "error"
						*/
						yy_state = yyact[ yy_n ];
						goto yy_stack;
					}
					/*
					** current state has no shift on
					** "error", pop stack
					*/
#if YYDEBUG
#	define _POP_ "Error recovery pops state %d, uncovers state %d\n"
					if ( yydebug )
						printf( _POP_, *yy_ps,
							yy_ps[-1] );
#	undef _POP_
#endif
					yy_ps--;
					yy_pv--;
				}
				/*
				** there is no state on stack with "error" as
				** a valid shift.  give up.
				*/
				YYABORT;
			case 3:		/* no shift yet; eat a token */
#if YYDEBUG
				/*
				** if debugging, look up token in list of
				** pairs.  0 and negative shouldn't occur,
				** but since timing doesn't matter when
				** debugging, it doesn't hurt to leave the
				** tests here.
				*/
				if ( yydebug )
				{
					register int yy_i;

					printf( "Error recovery discards " );
					if ( yychar == 0 )
						printf( "token end-of-file\n" );
					else if ( yychar < 0 )
						printf( "token -none-\n" );
					else
					{
						for ( yy_i = 0;
							yytoks[yy_i].t_val >= 0;
							yy_i++ )
						{
							if ( yytoks[yy_i].t_val
								== yychar )
							{
								break;
							}
						}
						printf( "token %s\n",
							yytoks[yy_i].t_name );
					}
				}
#endif /* YYDEBUG */
				if ( yychar == 0 )	/* reached EOF. quit */
					YYABORT;
				yychar = -1;
				goto yy_newstate;
			}
		}/* end if ( yy_n == 0 ) */
		/*
		** reduction by production yy_n
		** put stack tops, etc. so things right after switch
		*/
#if YYDEBUG
		/*
		** if debugging, print the string that is the user's
		** specification of the reduction which is just about
		** to be done.
		*/
		if ( yydebug )
			printf( "Reduce by (%d) \"%s\"\n",
				yy_n, yyreds[ yy_n ] );
#endif
		yytmp = yy_n;			/* value to switch over */
		yypvt = yy_pv;			/* $vars top of value stack */
		/*
		** Look in goto table for next state
		** Sorry about using yy_state here as temporary
		** register variable, but why not, if it works...
		** If yyr2[ yy_n ] doesn't have the low order bit
		** set, then there is no action to be done for
		** this reduction.  So, no saving & unsaving of
		** registers done.  The only difference between the
		** code just after the if and the body of the if is
		** the goto yy_stack in the body.  This way the test
		** can be made before the choice of what to do is needed.
		*/
		{
			/* length of production doubled with extra bit */
			register int yy_len = yyr2[ yy_n ];

			if ( !( yy_len & 01 ) )
			{
				yy_len >>= 1;
				yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
				yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
					*( yy_ps -= yy_len ) + 1;
				if ( yy_state >= YYLAST ||
					yychk[ yy_state =
					yyact[ yy_state ] ] != -yy_n )
				{
					yy_state = yyact[ yypgo[ yy_n ] ];
				}
				goto yy_stack;
			}
			yy_len >>= 1;
			yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
			yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
				*( yy_ps -= yy_len ) + 1;
			if ( yy_state >= YYLAST ||
				yychk[ yy_state = yyact[ yy_state ] ] != -yy_n )
			{
				yy_state = yyact[ yypgo[ yy_n ] ];
			}
		}
					/* save until reenter driver code */
		yystate = yy_state;
		yyps = yy_ps;
		yypv = yy_pv;
	}
	/*
	** code supplied by user is placed in this switch
	*/
	switch( yytmp )
	{
		
case 1:{inputExpr = yypvt[-0];	    sp-=1;} break;
case 2:{inputExpr = letrec(yypvt[-0],yypvt[-1]); sp-=2;} break;
case 3:{valDefns  = yypvt[-0];	    sp-=1;} break;
case 4:{syntaxError("input");} break;
case 5:{yyval = gc2(yypvt[-1]);} break;
case 6:{yyerrok; goOffside(startColumn);} break;
case 7:{yyval = gc2(yypvt[-2]);} break;
case 8:{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 9:{yyval = gc0(NIL);} break;
case 10:{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 11:{syntaxError("definition");} break;
case 12:{yyval = gc3(appendOnto(yypvt[-2], yypvt[-0]));} break;
case 13:{yyval = yypvt[-0];} break;
case 14:{yyval = gc3(ct1Clause(intOf(yypvt[-1]), yypvt[-0], yypvt[-2]));} break;
case 15:{yyval = gc3(cons(yypvt[-0], yypvt[-2]));} break;
case 16:{yyval = gc1(singleton(yypvt[-0]));} break;
case 17:{yyval = gc1(yypvt[-0]);} break;
case 18:{yyval = gc0(NIL);} break;
case 19:{typeLhs = yypvt[-0];} break;
case 20:{defTycon(6,yypvt[-6], checkTyLhs(yypvt[-5]), yypvt[-1], DATATYPE); typeLhs = NIL;} break;
case 21:{typeLhs = yypvt[-0];} break;
case 22:{defTycon(8,yypvt[-8], yypvt[-5], ap(QUAL,pair (yypvt[-7],yypvt[-1])), DATATYPE); typeLhs = NIL;} break;
case 23:{defTycon(5,yypvt[-2],yypvt[-3],yypvt[-1],yypvt[-0]);} break;
case 24:{defTycon(5,yypvt[-2],checkTyLhs(yypvt[-3]),
							rev(yypvt[-1]),DATATYPE);} break;
case 25:{defTycon(7,yypvt[-2],yypvt[-3],
						  ap(QUAL,pair(yypvt[-5],rev(yypvt[-1]))),
						  DATATYPE);} break;
case 26:{typeSyntaxChk("type Lhs", yypvt[-1]);
					 yyval = gc3(ap(yypvt[-2],yypvt[-0]));
					} break;
case 27:{yyval = yypvt[-0];} break;
case 28:{syntaxError("type defn lhs");} break;
case 29:{yyval = gc2(yypvt[-0]);} break;
case 30:{yyval = gc0(SYNONYM);} break;
case 31:{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 32:{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 33:{yyval = gc3(sigdecl(yypvt[-1],singleton(yypvt[-2]),
							     yypvt[-0]));} break;
case 34:{yyval = yypvt[-0];} break;
case 35:{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 36:{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 37:{yyval = gc3(ap(ap(yypvt[-1],yypvt[-2]),yypvt[-0]));} break;
case 38:{if (!isCon(getHead(yypvt[-0])))
					     syntaxError("data constructor");
					 yyval = yypvt[-0];} break;
case 39:{syntaxError("data type definition");} break;
case 40:{yyval = gc0(NIL);} break;
case 41:{yyval = gc2(singleton(yypvt[-0]));} break;
case 42:{yyval = gc4(yypvt[-1]);} break;
case 43:{yyval = gc0(NIL);} break;
case 44:{yyval = yypvt[-0];} break;
case 45:{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 46:{yyval = gc1(singleton(yypvt[-0]));} break;
case 47:{yyval = gc3(ap(QUAL,pair(yypvt[-2],yypvt[-0])));} break;
case 48:{yyval = yypvt[-0];} break;
case 49:{yyval = gc1(checkContext(yypvt[-0]));} break;
case 50:{yyval = yypvt[-0];} break;
case 51:{yyval = gc3(ap(ap(ARROW,yypvt[-2]),yypvt[-0]));} break;
case 52:{syntaxError("type expression");} break;
case 53:{typeSyntaxChk("type expression", yypvt[-1]);
					 yyval = gc3(ap(yypvt[-2],yypvt[-0]));
					} break;
case 54:{yyval = yypvt[-0];} break;
case 55:{yyval = yypvt[-0];} break;
case 56:{yyval = yypvt[-0];} break;
case 57:{yyval = gc2(UNIT);} break;
case 58:{yyval = gc3(ARROW);} break;
case 59:{yyval = gc3(yypvt[-1]);} break;
case 60:{yyval = gc4(ap(ARROW,yypvt[-2]));} break;
case 61:{yyval = gc3(yypvt[-1]);} break;
case 62:{yyval = gc3(buildTuple(yypvt[-1]));} break;
case 63:{yyval = gc3(ap(LIST,yypvt[-1]));} break;
case 64:{yyval = gc2(LIST);} break;
case 65:{yyval = gc2(mkTuple(tupleOf(yypvt[-1])+1));} break;
case 66:{yyval = gc1(mkTuple(2));} break;
case 67:{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 68:{yyval = gc3(cons(yypvt[-0],cons(yypvt[-2],NIL)));} break;
case 69:{fixDefn(LEFT_ASS,yypvt[-2],yypvt[-1],yypvt[-0]); sp-=3;} break;
case 70:{fixDefn(RIGHT_ASS,yypvt[-2],yypvt[-1],yypvt[-0]);sp-=3;} break;
case 71:{fixDefn(NON_ASS,yypvt[-2],yypvt[-1],yypvt[-0]);  sp-=3;} break;
case 72:{yyval = gc1(checkPrec(yypvt[-0]));} break;
case 73:{yyval = gc0(mkInt(DEF_PREC));} break;
case 74:{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 75:{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 76:{yyval = yypvt[-0];} break;
case 77:{yyval = yypvt[-0];} break;
case 78:{yyval = gc1(varMinus);} break;
case 79:{yyval = yypvt[-0];} break;
case 80:{yyval = gc3(yypvt[-1]);} break;
case 81:{yyval = yypvt[-0];} break;
case 82:{yyval = gc3(yypvt[-1]);} break;
case 83:{primDefn(yypvt[-3],yypvt[-2],yypvt[-0]); sp-=4;} break;
case 84:{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 85:{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 86:{syntaxError("primitive defn");} break;
case 87:{yyval = gc2(pair(yypvt[-1],yypvt[-0]));} break;
case 88:{classDefn(intOf(yypvt[-2]),yypvt[-1],yypvt[-0]); sp-=3;} break;
case 89:{instDefn(intOf(yypvt[-2]),yypvt[-1],yypvt[-0]);  sp-=3;} break;
case 90:{sp-=2;} break;
case 91:{yyval = gc3(pair(yypvt[-2],checkClass(yypvt[-0])));} break;
case 92:{yyval = gc1(pair(NIL,checkClass(yypvt[-0])));} break;
case 93:{yyval = gc4(yypvt[-1]);} break;
case 94:{yyval = gc0(NIL);} break;
case 95:{yyval = gc4(yypvt[-1]);} break;
case 96:{yyval = gc0(NIL);} break;
case 97:{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 98:{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 99:{yyval = gc1(yypvt[-0]);} break;
case 100:{syntaxError("class body");} break;
case 101:{yyval = gc3(sigdecl(yypvt[-1],yypvt[-2],yypvt[-0]));} break;
case 102:{yyval = gc2(pair(yypvt[-1],yypvt[-0]));} break;
case 103:{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 104:{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 105:{yyval = gc2(letrec(yypvt[-0],yypvt[-1]));} break;
case 106:{yyval = yypvt[-0];} break;
case 107:{syntaxError("declaration");} break;
case 108:{yyval = gc2(pair(yypvt[-1],yypvt[-0]));} break;
case 109:{yyval = gc1(grded(rev(yypvt[-0])));} break;
case 110:{yyval = gc4(yypvt[-1]);} break;
case 111:{yyval = gc2(cons(yypvt[-0],yypvt[-1]));} break;
case 112:{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 113:{yyval = gc4(pair(yypvt[-1],pair(yypvt[-2],yypvt[-0])));} break;
case 114:{yyval = gc5(pair(yypvt[-4],pair(yypvt[-0],yypvt[-3])));} break;
case 115:{yyval = gc4(pair(yypvt[-3],pair(yypvt[-0],yypvt[-2])));} break;
case 116:{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 117:{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 118:{yyval = yypvt[-0];} break;
case 119:{yyval = gc3(varMinus);} break;
case 120:{yyval = yypvt[-0];} break;
case 121:{yyval = gc3(yypvt[-1]);} break;
case 122:{yyval = yypvt[-0];} break;
case 123:{yyval = gc3(yypvt[-1]);} break;
case 124:{yyval = gc3(ap(ESIGN,pair(yypvt[-2],yypvt[-0])));} break;
case 125:{yyval = yypvt[-0];} break;
case 126:{syntaxError("expression");} break;
case 127:{yyval = yypvt[-0];} break;
case 128:{yyval = gc3(opapd(yypvt[-2],yypvt[-1],yypvt[-0]));} break;
case 129:{yyval = gc1((*tidyInfix[newSyntax])(yypvt[-0]));} break;
case 130:{yyval = gc3(opap(yypvt[-2],yypvt[-1],yypvt[-0]));} break;
case 131:{yyval =
 		  gc5(opap(opap(singleton(yypvt[-4]), yypvt[-3], yypvt[-2]),yypvt[-1],yypvt[-0]));} break;
case 132:{if (isInt(yypvt[-0]))
					     yyval = gc2(mkInt(-intOf(yypvt[-0])));
					 else
					     yyval = gc2(ap(varNegate,yypvt[-0]));
					} break;
case 133:{yyval = gc4(ap(LAMBDA,
						     pair(rev(yypvt[-2]),
						          pair(yypvt[-1],yypvt[-0]))));} break;
case 134:{yyval = gc6(letrec(yypvt[-3],yypvt[-0]));} break;
case 135:{yyval = gc6(ap(COND,triple(yypvt[-4],yypvt[-2],yypvt[-0])));} break;
case 136:{yyval = gc6(ap(CASE,pair(yypvt[-4],rev(yypvt[-1]))));} break;
case 137:{yyval = yypvt[-0];} break;
case 138:{yyval = gc2(cons(yypvt[-0],yypvt[-1]));} break;
case 139:{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 140:{
		if (newSyntax)
		{
		  ERROR(row) "Juxtaposition has no meaning. Use ."
		  EEND;
		}
		else
		  yyval = gc2(ap(yypvt[-1],yypvt[-0]));
	      } break;
case 141:{yyval = gc2(ap(RUNST,yypvt[-0]));} break;
case 142:{yyval = yypvt[-0];} break;
case 143:{yyval = yypvt[-0];} break;
case 144:{yyval = gc3(ap(ASPAT,pair(yypvt[-2],yypvt[-0])));} break;
case 145:{yyval = gc2(ap(LAZYPAT,yypvt[-0]));} break;
case 146:{yyval = gc1(WILDCARD);} break;
case 147:{yyval = yypvt[-0];} break;
case 148:{yyval = gc2(UNIT);} break;
case 149:{yyval = yypvt[-0];} break;
case 150:{yyval = yypvt[-0];} break;
case 151:{yyval = yypvt[-0];} break;
case 152:{yyval = yypvt[-0];} break;
case 153:{yyval = gc3(yypvt[-1]);} break;
case 154:{yyval = gc3(buildTuple(yypvt[-1]));} break;
case 155:{yyval = gc3(yypvt[-1]);} break;
case 156:{yyval = gc4(ap(yypvt[-1],yypvt[-2]));} break;
case 157:{yyval = gc4(ap(ap(varFlip,yypvt[-2]),yypvt[-1]));} break;
case 158:{yyval = gc4(ap(ap(varFlip,yypvt[-2]),yypvt[-1]));} break;
case 159:{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 160:{yyval = gc3(cons(yypvt[-0],cons(yypvt[-2],NIL)));} break;
case 161:{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 162:{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 163:{yyval = gc2(pair(yypvt[-1],yypvt[-0]));} break;
case 164:{yyval = gc2(letrec(yypvt[-0],yypvt[-1]));} break;
case 165:{yyval = yypvt[-0];} break;
case 166:{yyval = gc1(grded(rev(yypvt[-0])));} break;
case 167:{yyval = gc2(pair(yypvt[-1],yypvt[-0]));} break;
case 168:{syntaxError("case expression");} break;
case 169:{yyval = gc2(cons(yypvt[-0],yypvt[-1]));} break;
case 170:{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 171:{yyval = gc4(pair(yypvt[-1],pair(yypvt[-2],yypvt[-0])));} break;
case 172:{yyval = gc0(nameNil);} break;
case 173:{yyval = gc1(ap(FINLIST,cons(yypvt[-0],NIL)));} break;
case 174:{yyval = gc1(ap(FINLIST,rev(yypvt[-0])));} break;
case 175:{yyval = gc3(ap(COMP,pair(yypvt[-2],rev(yypvt[-0]))));} break;
case 176:{yyval = gc3(ap(ap(varFromTo,yypvt[-2]),yypvt[-0]));} break;
case 177:{yyval = gc4(ap(ap(varFromThen,yypvt[-3]),yypvt[-1]));} break;
case 178:{yyval = gc2(ap(varFrom,yypvt[-1]));} break;
case 179:{yyval = gc5(ap(ap(ap(varFromThenTo,
                                                               yypvt[-4]),yypvt[-2]),yypvt[-0]));} break;
case 180:{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 181:{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 182:{yyval = gc3(ap(FROMQUAL,pair(yypvt[-2],yypvt[-0])));} break;
case 183:{yyval = gc3(ap(QWHERE,
						     singleton(
							pair(yypvt[-2],pair(yypvt[-1],
								     yypvt[-0])))));} break;
case 184:{yyval = gc1(ap(BOOLQUAL,yypvt[-0]));} break;
case 185:{yyval = gc4(ap(QWHERE,yypvt[-1]));} break;
case 186:{yyval = gc2(yypvt[-0]);} break;
case 187:{yyval = yypvt[-0];} break;
case 188:{yyval = yypvt[-0];} break;
case 189:{yyerrok;
                                         if (canUnOffside()) {
                                             unOffside();
					     /* insert extra token on stack*/
					     push(NIL);
					     pushed(0) = pushed(1);
					     pushed(1) = mkInt(column);
					 }
                                         else
                                             syntaxError("definition");
                                        } break;
	}
	goto yystack;		/* reset registers in driver code */
}
