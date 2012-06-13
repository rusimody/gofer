/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     EVALEX = 258,
     SCRIPT = 259,
     INFIXL = 260,
     INFIXR = 261,
     INFIX = 262,
     FUNARROW = 263,
     UPTO = 264,
     CASEXP = 265,
     OF = 266,
     IF = 267,
     THEN = 268,
     ELSE = 269,
     WHERE = 270,
     TYPE = 271,
     DATA = 272,
     FROM = 273,
     LET = 274,
     IN = 275,
     VAROP = 276,
     VARID = 277,
     NUMLIT = 278,
     CHARLIT = 279,
     STRINGLIT = 280,
     REPEAT = 281,
     CONOP = 282,
     CONID = 283,
     TCLASS = 284,
     IMPLIES = 285,
     TINSTANCE = 286,
     DO = 287,
     TRUNST = 288,
     PRIMITIVE = 289,
     DEFAULT = 290,
     DERIVING = 291,
     HIDING = 292,
     IMPORT = 293,
     INTERFACE = 294,
     MODULE = 295,
     RENAMING = 296,
     TO = 297,
     CTYPE = 298
   };
#endif
/* Tokens.  */
#define EVALEX 258
#define SCRIPT 259
#define INFIXL 260
#define INFIXR 261
#define INFIX 262
#define FUNARROW 263
#define UPTO 264
#define CASEXP 265
#define OF 266
#define IF 267
#define THEN 268
#define ELSE 269
#define WHERE 270
#define TYPE 271
#define DATA 272
#define FROM 273
#define LET 274
#define IN 275
#define VAROP 276
#define VARID 277
#define NUMLIT 278
#define CHARLIT 279
#define STRINGLIT 280
#define REPEAT 281
#define CONOP 282
#define CONID 283
#define TCLASS 284
#define IMPLIES 285
#define TINSTANCE 286
#define DO 287
#define TRUNST 288
#define PRIMITIVE 289
#define DEFAULT 290
#define DERIVING 291
#define HIDING 292
#define IMPORT 293
#define INTERFACE 294
#define MODULE 295
#define RENAMING 296
#define TO 297
#define CTYPE 298




/* Copy the first part of user declarations.  */
#line 18 "parser.y"

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



/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 264 "y.tab.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  34
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   822

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  61
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  70
/* YYNRULES -- Number of rules.  */
#define YYNRULES  190
/* YYNRULES -- Number of states.  */
#define YYNSTATES  350

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   298

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      13,    14,     2,     2,    11,    10,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    57,    16,
       2,     5,     2,     2,    12,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    17,    28,    19,     2,    59,    32,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    58,    15,    60,    30,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       6,     7,     8,     9,    18,    20,    21,    22,    23,    24,
      25,    26,    27,    29,    31,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     6,    10,    13,    15,    19,    21,    25,
      29,    31,    33,    35,    39,    41,    45,    49,    51,    53,
      54,    55,    63,    64,    74,    80,    86,    94,    98,   100,
     102,   105,   106,   110,   112,   116,   118,   122,   124,   128,
     130,   132,   133,   136,   141,   142,   144,   148,   150,   154,
     156,   158,   160,   164,   166,   170,   172,   174,   176,   179,
     183,   187,   192,   196,   200,   204,   207,   210,   212,   216,
     220,   224,   228,   232,   234,   235,   239,   241,   243,   245,
     247,   249,   253,   255,   259,   264,   268,   270,   272,   275,
     279,   283,   286,   290,   292,   297,   298,   303,   304,   308,
     310,   312,   314,   318,   321,   325,   327,   330,   332,   334,
     337,   339,   344,   347,   349,   354,   360,   365,   369,   371,
     373,   377,   379,   383,   385,   389,   393,   395,   397,   399,
     403,   405,   409,   415,   418,   423,   430,   437,   444,   446,
     449,   451,   454,   457,   459,   461,   465,   468,   470,   472,
     475,   477,   479,   481,   483,   487,   491,   495,   500,   505,
     510,   514,   518,   522,   524,   527,   530,   532,   534,   537,
     539,   542,   544,   549,   550,   552,   554,   558,   562,   567,
     570,   576,   580,   582,   586,   590,   592,   597,   600,   602,
     604
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
      62,     0,    -1,     3,   112,    -1,     3,   112,   105,    -1,
       4,    63,    -1,     1,    -1,    64,    65,   129,    -1,     1,
      -1,    65,    16,    70,    -1,    65,    16,   101,    -1,    70,
      -1,   101,    -1,     1,    -1,    66,    16,    67,    -1,    67,
      -1,    68,    57,    84,    -1,    68,    11,   111,    -1,   111,
      -1,    34,    -1,    -1,    -1,    56,    84,    71,    25,    58,
      66,   129,    -1,    -1,    56,    83,    43,    73,    72,    25,
      58,    66,   129,    -1,    26,    73,     5,    84,    74,    -1,
      27,    84,     5,    77,    79,    -1,    27,    83,    43,    73,
       5,    77,    79,    -1,    73,    69,    35,    -1,    41,    -1,
       1,    -1,    33,    75,    -1,    -1,    75,    11,    76,    -1,
      76,    -1,   109,    57,    82,    -1,   109,    -1,    77,    15,
      78,    -1,    78,    -1,    84,    93,    84,    -1,    84,    -1,
       1,    -1,    -1,    49,    41,    -1,    49,    13,    80,    14,
      -1,    -1,    81,    -1,    81,    11,    41,    -1,    41,    -1,
      83,    43,    84,    -1,    84,    -1,    84,    -1,    85,    -1,
      85,     9,    84,    -1,     1,    -1,    85,    69,    86,    -1,
      86,    -1,    35,    -1,    41,    -1,    13,    14,    -1,    13,
       9,    14,    -1,    13,    84,    14,    -1,    13,    85,     9,
      14,    -1,    13,    87,    14,    -1,    13,    88,    14,    -1,
      17,    84,    19,    -1,    17,    19,    -1,    87,    11,    -1,
      11,    -1,    88,    11,    84,    -1,    84,    11,    84,    -1,
       6,    89,    90,    -1,     7,    89,    90,    -1,     8,    89,
      90,    -1,    36,    -1,    -1,    90,    11,    91,    -1,    91,
      -1,    92,    -1,    93,    -1,    10,    -1,    34,    -1,    32,
      35,    32,    -1,    40,    -1,    32,    41,    32,    -1,    47,
      94,    57,    82,    -1,    94,    11,    95,    -1,    95,    -1,
       1,    -1,   109,    38,    -1,    42,    96,    97,    -1,    44,
      96,    98,    -1,    48,    84,    -1,    83,    43,    84,    -1,
      84,    -1,    25,    58,    99,   129,    -1,    -1,    25,    58,
     102,   129,    -1,    -1,    99,    16,   100,    -1,   100,    -1,
     101,    -1,     1,    -1,   108,    57,    82,    -1,   113,   103,
      -1,   102,    16,   101,    -1,   101,    -1,   104,   105,    -1,
     104,    -1,     1,    -1,     5,   112,    -1,   106,    -1,    25,
      58,   102,   129,    -1,   106,   107,    -1,   107,    -1,    15,
     112,     5,   112,    -1,     5,   112,    11,    22,   112,    -1,
       5,   112,    11,   112,    -1,   108,    11,   109,    -1,   109,
      -1,   110,    -1,    13,    10,    14,    -1,    35,    -1,    13,
      34,    14,    -1,    41,    -1,    13,    40,    14,    -1,   113,
      57,    82,    -1,   113,    -1,     1,    -1,   115,    -1,   115,
      91,   115,    -1,   114,    -1,   114,    91,   115,    -1,   115,
      91,   115,    91,   115,    -1,    10,   117,    -1,    28,   116,
       9,   112,    -1,    31,    58,   102,   129,    33,   112,    -1,
      22,   112,    23,   112,    24,   112,    -1,    20,   112,    21,
      58,   120,   129,    -1,   117,    -1,   116,   118,    -1,   118,
      -1,   117,   118,    -1,    46,   118,    -1,   118,    -1,   109,
      -1,   109,    12,   118,    -1,    30,   118,    -1,    59,    -1,
     111,    -1,    13,    14,    -1,    36,    -1,    37,    -1,    38,
      -1,    39,    -1,    13,   112,    14,    -1,    13,   119,    14,
      -1,    17,   126,    19,    -1,    13,   115,    91,    14,    -1,
      13,    92,   118,    14,    -1,    13,    93,   118,    14,    -1,
     119,    11,   112,    -1,   112,    11,   112,    -1,   120,    16,
     121,    -1,   121,    -1,   113,   122,    -1,   123,   105,    -1,
     123,    -1,   124,    -1,     9,   112,    -1,     1,    -1,   124,
     125,    -1,   125,    -1,    15,   113,     9,   112,    -1,    -1,
     112,    -1,   119,    -1,   112,    15,   127,    -1,   112,    18,
     112,    -1,   112,    11,   112,    18,    -1,   112,    18,    -1,
     112,    11,   112,    18,   112,    -1,   127,    11,   128,    -1,
     128,    -1,   112,    29,   112,    -1,   112,     5,   112,    -1,
     112,    -1,    31,    58,   102,   129,    -1,    16,   130,    -1,
     130,    -1,    60,    -1,     1,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   109,   109,   110,   111,   112,   125,   128,   130,   131,
     132,   133,   134,   138,   139,   141,   143,   144,   147,   148,
     151,   151,   153,   153,   157,   158,   161,   167,   170,   171,
     173,   174,   176,   177,   179,   181,   183,   184,   186,   187,
     190,   192,   193,   194,   196,   197,   199,   200,   211,   212,
     214,   216,   217,   218,   220,   224,   226,   227,   228,   229,
     230,   231,   232,   233,   234,   235,   237,   238,   241,   242,
     247,   248,   249,   251,   252,   254,   255,   257,   258,   259,
     261,   262,   264,   265,   270,   272,   273,   274,   276,   281,
     282,   283,   285,   286,   288,   289,   291,   292,   294,   295,
     297,   298,   303,   304,   306,   307,   309,   310,   311,   313,
     314,   316,   318,   319,   321,   328,   329,   331,   332,   334,
     335,   337,   338,   340,   341,   346,   347,   348,   350,   351,
     353,   355,   356,   359,   364,   367,   368,   369,   370,   372,
     373,   375,   385,   386,   388,   389,   390,   391,   392,   393,
     394,   395,   396,   397,   398,   399,   400,   401,   402,   403,
     405,   406,   408,   409,   411,   413,   414,   416,   417,   418,
     420,   421,   423,   428,   429,   430,   431,   432,   433,   434,
     435,   438,   439,   441,   442,   446,   447,   473,   474,   476,
     477
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "EVALEX", "SCRIPT", "'='", "INFIXL",
  "INFIXR", "INFIX", "FUNARROW", "'-'", "','", "'@'", "'('", "')'", "'|'",
  "';'", "'['", "UPTO", "']'", "CASEXP", "OF", "IF", "THEN", "ELSE",
  "WHERE", "TYPE", "DATA", "'\\\\'", "FROM", "'~'", "LET", "'`'", "IN",
  "VAROP", "VARID", "NUMLIT", "CHARLIT", "STRINGLIT", "REPEAT", "CONOP",
  "CONID", "TCLASS", "IMPLIES", "TINSTANCE", "DO", "TRUNST", "PRIMITIVE",
  "DEFAULT", "DERIVING", "HIDING", "IMPORT", "INTERFACE", "MODULE",
  "RENAMING", "TO", "CTYPE", "':'", "'{'", "'_'", "'}'", "$accept",
  "start", "topModule", "begin", "topDecls", "ctDecls", "ctDecl", "conids",
  "pvarop", "topDecl", "@1", "@2", "typeLhs", "invars", "rsvars", "rsvar",
  "constrs", "constr", "deriving", "derivs0", "derivs", "sigType",
  "context", "type", "ctype", "atype", "tupCommas", "typeTuple",
  "optdigit", "ops", "op", "varop", "conop", "prims", "prim", "classHead",
  "classBody", "instBody", "csigdecls", "csigdecl", "decl", "decls", "rhs",
  "rhs1", "wherePart", "gdefs", "gdef", "vars", "var", "varid", "conid",
  "exp", "opExp", "opExp0", "pfxExp", "pats", "appExp", "atomic", "exps2",
  "alts", "alt", "altRhs", "altRhs1", "guardAlts", "guardAlt", "list",
  "quals", "qual", "close", "close1", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,    61,   260,   261,   262,   263,
      45,    44,    64,    40,    41,   124,    59,    91,   264,    93,
     265,   266,   267,   268,   269,   270,   271,   272,    92,   273,
     126,   274,    96,   275,   276,   277,   278,   279,   280,   281,
     282,   283,   284,   285,   286,   287,   288,   289,   290,   291,
     292,   293,   294,   295,   296,   297,   298,    58,   123,    95,
     125
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    61,    62,    62,    62,    62,    63,    64,    65,    65,
      65,    65,    65,    66,    66,    67,    68,    68,    69,    69,
      71,    70,    72,    70,    70,    70,    70,    73,    73,    73,
      74,    74,    75,    75,    76,    76,    77,    77,    78,    78,
      78,    79,    79,    79,    80,    80,    81,    81,    82,    82,
      83,    84,    84,    84,    85,    85,    86,    86,    86,    86,
      86,    86,    86,    86,    86,    86,    87,    87,    88,    88,
      70,    70,    70,    89,    89,    90,    90,    91,    91,    91,
      92,    92,    93,    93,    70,    94,    94,    94,    95,    70,
      70,    70,    96,    96,    97,    97,    98,    98,    99,    99,
     100,   100,   101,   101,   102,   102,   103,   103,   103,   104,
     104,   105,   106,   106,   107,   107,   107,   108,   108,   109,
     109,   110,   110,   111,   111,   112,   112,   112,   113,   113,
     113,   114,   114,   115,   115,   115,   115,   115,   115,   116,
     116,   117,   117,   117,   118,   118,   118,   118,   118,   118,
     118,   118,   118,   118,   118,   118,   118,   118,   118,   118,
     119,   119,   120,   120,   121,   122,   122,   123,   123,   123,
     124,   124,   125,   126,   126,   126,   126,   126,   126,   126,
     126,   127,   127,   128,   128,   128,   128,   129,   129,   130,
     130
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     2,     3,     2,     1,     3,     1,     3,     3,
       1,     1,     1,     3,     1,     3,     3,     1,     1,     0,
       0,     7,     0,     9,     5,     5,     7,     3,     1,     1,
       2,     0,     3,     1,     3,     1,     3,     1,     3,     1,
       1,     0,     2,     4,     0,     1,     3,     1,     3,     1,
       1,     1,     3,     1,     3,     1,     1,     1,     2,     3,
       3,     4,     3,     3,     3,     2,     2,     1,     3,     3,
       3,     3,     3,     1,     0,     3,     1,     1,     1,     1,
       1,     3,     1,     3,     4,     3,     1,     1,     2,     3,
       3,     2,     3,     1,     4,     0,     4,     0,     3,     1,
       1,     1,     3,     2,     3,     1,     2,     1,     1,     2,
       1,     4,     2,     1,     4,     5,     4,     3,     1,     1,
       3,     1,     3,     1,     3,     3,     1,     1,     1,     3,
       1,     3,     5,     2,     4,     6,     6,     6,     1,     2,
       1,     2,     2,     1,     1,     3,     2,     1,     1,     2,
       1,     1,     1,     1,     3,     3,     3,     4,     4,     4,
       3,     3,     3,     1,     2,     2,     1,     1,     2,     1,
       2,     1,     4,     0,     1,     1,     3,     3,     4,     2,
       5,     3,     1,     3,     3,     1,     4,     2,     1,     1,
       1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     5,     0,     0,     0,   127,     0,     0,     0,     0,
       0,     0,     0,     0,   121,   150,   151,   152,   153,   123,
       0,   147,   144,   119,   148,     2,   126,   130,   128,   138,
     143,     7,     4,     0,     1,   133,     0,   149,     0,    80,
      82,     0,     0,     0,   128,     0,   174,   175,     0,     0,
       0,     0,   140,   146,     0,   142,     0,     0,     3,     0,
      79,    80,    82,     0,    77,    78,     0,   141,    12,    74,
      74,    74,     0,     0,     0,     0,     0,     0,     0,     0,
      10,    11,     0,   144,     0,   120,     0,     0,   122,   124,
       0,     0,     0,   154,     0,     0,   155,     0,     0,     0,
     156,     0,     0,     0,   139,   105,     0,   145,     0,    53,
       0,     0,    56,    57,   125,     0,    49,    51,    55,   131,
     129,    73,     0,     0,     0,    29,    28,    19,     0,    50,
       0,    93,    95,    97,    87,     0,     0,    86,     0,    91,
       0,    20,   190,     0,   189,     6,   188,     0,     0,   108,
       0,     0,   103,   107,   110,   113,    81,    83,   158,   159,
     161,   157,   160,   161,     0,   185,   176,   182,   177,     0,
       0,   134,     0,     0,     0,     0,    67,    58,     0,    19,
       0,     0,    65,     0,     0,     0,    18,     0,     0,    70,
      76,    71,    72,     0,     0,     0,     0,     0,     0,    89,
       0,    90,     0,     0,     0,     0,    88,     0,     0,     8,
       9,   187,   117,   102,   109,     0,   106,     0,   112,     0,
       0,     0,     0,     0,     0,     0,   163,     0,   104,     0,
     111,    59,     0,    60,     0,    66,    62,     0,    63,    64,
      48,    52,    54,   132,     0,    31,    27,    19,    40,    41,
      37,    39,    92,     0,     0,    85,    84,    19,     0,     0,
       0,     0,   180,     0,   184,   183,   181,   169,     0,     0,
     164,   166,   167,   171,     0,   137,   136,   135,    69,    61,
      68,    75,     0,    24,     0,     0,     0,    25,     0,     0,
     101,     0,    99,   100,     0,     0,     0,     0,   116,   114,
     186,   168,     0,   165,   170,   162,    30,    33,    35,    41,
      36,    44,    42,    38,     0,    94,    96,     0,     0,     0,
      14,     0,    17,   115,     0,     0,     0,    26,    47,     0,
      45,   101,    98,     0,     0,     0,    21,     0,     0,   172,
      32,    34,    43,     0,     0,    13,    16,    15,    46,    23
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     4,    32,    33,    79,   319,   320,   321,   194,    80,
     208,   295,   127,   283,   306,   307,   249,   250,   287,   329,
     330,   114,   115,   116,   117,   118,   180,   181,   122,   189,
     190,    64,    65,   136,   137,   132,   199,   201,   291,   292,
     105,   106,   152,   153,    58,   154,   155,    82,    22,    23,
      24,   165,    26,    27,    28,    51,    29,    30,    45,   225,
     226,   270,   271,   272,   273,    48,   166,   167,   145,   146
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -272
static const yytype_int16 yypact[] =
{
     163,  -272,   554,    49,   106,  -272,   751,   426,   458,   554,
     554,   763,   763,    66,  -272,  -272,  -272,  -272,  -272,  -272,
     763,  -272,    96,  -272,  -272,   101,    94,   131,   131,   763,
    -272,  -272,  -272,   382,  -272,   763,   139,  -272,   127,   156,
     181,   763,   763,   192,   131,   219,   171,   186,   188,   190,
     205,   662,  -272,  -272,   724,  -272,   763,   151,  -272,   201,
    -272,  -272,  -272,   724,  -272,  -272,   724,  -272,  -272,   213,
     213,   213,    29,   201,   201,   201,    50,   201,   201,    17,
    -272,  -272,    10,     8,   104,  -272,   224,   229,  -272,  -272,
     239,   246,   554,  -272,   694,   554,  -272,   554,   586,   490,
    -272,   209,   554,   554,  -272,  -272,    22,  -272,   724,  -272,
     199,   296,  -272,  -272,  -272,   232,   233,   277,  -272,  -272,
     131,  -272,   131,   131,   131,  -272,  -272,    47,   244,   280,
     248,   233,   264,   271,  -272,    76,    23,  -272,   261,  -272,
     263,   233,  -272,   297,  -272,  -272,  -272,    40,   201,  -272,
     554,   554,  -272,   101,   176,  -272,  -272,  -272,  -272,  -272,
    -272,  -272,  -272,   290,   258,    89,   309,  -272,  -272,   724,
     302,  -272,   242,   288,    22,   315,  -272,  -272,   221,   103,
     254,   260,  -272,   311,   201,   201,  -272,   228,   724,   329,
    -272,   329,   329,   201,   320,    29,   390,   201,   292,  -272,
     301,  -272,   332,   156,    40,   201,  -272,    29,   335,  -272,
    -272,  -272,  -272,  -272,   351,   359,  -272,   554,  -272,   522,
     724,   554,   554,   586,   178,    27,  -272,   554,  -272,   554,
    -272,  -272,   201,  -272,   119,  -272,  -272,   201,  -272,  -272,
    -272,  -272,  -272,  -272,   131,   333,  -272,    69,    16,    34,
    -272,    91,  -272,   618,   724,  -272,  -272,   100,   307,   650,
     554,   351,  -272,    22,  -272,  -272,  -272,  -272,   554,   724,
    -272,   101,   352,  -272,   242,  -272,  -272,  -272,  -272,  -272,
    -272,  -272,    40,  -272,   390,   390,    18,  -272,   327,   201,
    -272,    31,  -272,  -272,    22,   345,    55,   554,  -272,  -272,
     288,  -272,   364,  -272,  -272,  -272,   363,  -272,   318,    34,
    -272,   340,  -272,  -272,   341,  -272,  -272,   326,   346,    44,
    -272,    35,  -272,   205,   554,    40,   201,  -272,  -272,   371,
     383,  -272,  -272,    55,   181,    28,  -272,    55,   201,  -272,
    -272,  -272,  -272,   355,    44,  -272,  -272,  -272,  -272,  -272
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -272,  -272,  -272,  -272,  -272,    60,    62,  -272,  -100,   255,
    -272,  -272,   -50,  -272,  -272,    80,   122,   126,   105,  -272,
    -272,  -134,   274,   -38,   305,   235,  -272,  -272,   -16,   114,
     -18,   409,    -3,  -272,   230,   357,  -272,  -272,  -272,   121,
     -22,   -93,  -272,  -272,  -141,  -272,   279,  -272,   -32,  -272,
    -271,    -2,   -30,  -272,    -5,  -272,    21,    87,   429,  -272,
     168,  -272,  -272,  -272,   172,  -272,  -272,   222,   -90,  -130
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -180
static const yytype_int16 yytable[] =
{
      25,    83,    44,    84,    42,    43,    46,    49,    50,    63,
      66,    81,   216,   211,   213,   174,   173,   187,   142,  -118,
      56,   147,    83,   142,    84,   322,    94,    35,   142,   142,
     125,   311,   142,   143,   204,   129,   131,   131,   172,   139,
     141,   318,   211,   274,   138,   142,   337,   314,   -53,   285,
      31,   134,   193,   135,   123,   124,   -53,    35,   119,   312,
     335,   120,   322,   135,   322,  -118,   346,   148,   318,    19,
     126,   256,   178,   183,   284,    14,    83,   144,    84,   187,
     205,   186,   144,   286,   230,    14,   202,   144,   144,   120,
     160,   144,   338,   162,   221,   163,    19,   168,    52,    53,
     170,   171,   188,   186,   144,   149,    34,    55,    56,   150,
     203,    83,   234,    84,   -51,   212,    67,   -51,   222,   151,
     109,   210,    67,   288,    54,   -22,    57,   263,    90,    91,
     303,    62,   110,   279,   186,   275,   111,   186,   104,   224,
      83,    60,    84,   107,   211,   247,   240,   241,   214,   215,
     228,    59,     7,    85,   112,   245,     8,   257,   251,   252,
     113,   294,    86,    38,     1,    61,     2,     3,    87,    12,
      88,    62,   138,   300,    14,    15,    16,    17,    18,   267,
      19,   217,    97,   243,   211,    20,    98,   268,    83,    99,
      84,   151,   341,   269,   278,    89,   241,    95,    21,   280,
     109,   315,   109,    92,   316,   211,    93,   100,   175,   108,
     176,   101,   110,   177,   110,   261,   111,   262,   111,   264,
     265,    83,    83,    84,    84,   276,   281,   277,   102,   336,
      95,   293,   232,    96,   112,   233,   112,   191,   192,   302,
     113,   110,   113,   142,   224,   111,   251,   251,   289,   121,
     308,   313,     6,   158,   349,     7,   156,   298,   299,     8,
     159,   157,     9,   112,    10,   235,   301,   169,   236,   113,
      11,   237,    12,    13,   238,   184,   -50,    14,    15,    16,
      17,    18,    83,    19,    84,   196,   185,   195,    20,   198,
     -19,   197,   293,   308,   -19,   323,   200,   109,   142,   206,
     347,    21,   144,    69,    70,    71,   207,     6,   219,   110,
       7,   186,   -19,   111,     8,   182,   220,     9,   -19,    10,
     223,   229,   339,    72,    73,    11,   227,    12,    13,   231,
     239,   112,    14,    15,    16,    17,    18,   113,    19,    74,
     244,    75,   331,    20,    76,    77,    85,   128,   130,   130,
     253,     6,   140,    78,     7,   246,    21,   144,     8,   254,
     258,     9,   259,    10,   260,   296,   282,   269,    87,    11,
     317,    12,    13,   324,   325,   326,    14,    15,    16,    17,
      18,   328,    19,    68,   333,   342,   334,    20,    69,    70,
      71,   248,     6,   344,   343,     7,   348,   345,   209,     8,
      21,   144,     9,   110,    10,   340,   309,   111,    72,    73,
      11,   310,    12,    13,   327,   179,    41,    14,    15,    16,
      17,    18,   242,    19,    74,   112,    75,     5,    20,    76,
      77,   113,   133,   218,   255,   332,    36,    47,    78,     7,
      37,    21,   305,     8,   304,   266,     9,     0,    10,     0,
       0,     0,     0,     0,    11,     0,    12,    13,    38,     5,
      39,    14,    15,    16,    17,    18,    40,    19,     6,     0,
       0,     7,    20,     0,     0,     8,     0,  -173,     9,     0,
      10,     0,     0,     0,     0,    21,    11,     0,    12,    13,
       0,     5,     0,    14,    15,    16,    17,    18,     0,    19,
       6,     0,     0,     7,    20,     0,     0,     8,     0,  -179,
       9,     0,    10,     0,     0,     0,     0,    21,    11,     0,
      12,    13,     0,     5,     0,    14,    15,    16,    17,    18,
       0,    19,     6,     0,     0,     7,    20,     0,     0,     8,
       0,  -178,     9,     0,    10,     0,     0,     0,     0,    21,
      11,     0,    12,    13,     0,     5,     0,    14,    15,    16,
      17,    18,     0,    19,     6,     0,     0,     7,    20,     0,
       0,     8,     0,     0,     9,     0,    10,     0,     0,     0,
       0,    21,    11,     0,    12,    13,     0,     5,     0,    14,
      15,    16,    17,    18,     0,    19,     6,     0,     0,     7,
      20,     0,     0,     8,     0,     0,     9,     0,    10,     0,
       0,     0,     0,    21,    11,     0,    12,   164,     0,   290,
       0,    14,    15,    16,    17,    18,     0,    19,     6,     0,
       0,     7,    20,     0,     0,     8,     0,     0,     9,     0,
      10,     0,     0,     0,     0,    21,    11,     0,    12,    13,
       0,     5,     0,    14,    15,    16,    17,    18,     0,    19,
       6,     0,     0,     7,    20,     0,     0,     8,     0,     0,
       9,   103,   297,     0,     0,     7,     0,    21,    11,     8,
      12,    13,     0,     0,     0,    14,    15,    16,    17,    18,
       0,    19,    12,     0,     0,     0,    20,    14,    15,    16,
      17,    18,     0,    19,     6,     0,     0,     7,   161,    21,
       0,     8,     0,     0,     9,     0,    10,     0,     0,     0,
       0,    21,    11,     0,    12,    13,     0,     0,     0,    14,
      15,    16,    17,    18,     6,    19,     0,     7,     0,     0,
      20,     8,     0,     0,     9,     0,    10,     0,     0,     0,
       0,     0,    11,    21,    12,    13,     0,     0,     0,    14,
      15,    16,    17,    18,     7,    19,     0,     0,     8,     0,
      20,     0,     0,     0,     0,     0,     7,     0,     0,     0,
       8,    12,     0,    21,     0,     0,    14,    15,    16,    17,
      18,     0,    19,    12,     0,     0,     0,    20,    14,    15,
      16,    17,    18,     0,    19,     0,     0,     0,     0,     0,
      21,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    21
};

static const yytype_int16 yycheck[] =
{
       2,    33,     7,    33,     7,     7,     8,     9,    10,    27,
      28,    33,   153,   143,   148,   108,   106,   117,     1,    11,
      12,    11,    54,     1,    54,   296,    44,     6,     1,     1,
       1,    13,     1,    16,    11,    73,    74,    75,    16,    77,
      78,    13,   172,    16,    76,     1,    11,    16,    32,    15,
       1,     1,     5,    13,    70,    71,    40,    36,    63,    41,
      16,    66,   333,    13,   335,    57,   337,    57,    13,    41,
      41,   205,   110,   111,     5,    35,   108,    60,   108,   179,
      57,    34,    60,    49,   174,    35,    10,    60,    60,    94,
      92,    60,    57,    95,     5,    97,    41,    99,    11,    12,
     102,   103,   120,    34,    60,     1,     0,    20,    12,     5,
      34,   143,     9,   143,    11,   147,    29,    14,    29,    15,
       1,   143,    35,    32,    58,    25,    25,   220,    41,    42,
     271,    40,    13,    14,    34,   225,    17,    34,    51,   169,
     172,    10,   172,    56,   274,   195,   184,   185,   150,   151,
     172,    57,    13,    14,    35,   193,    17,   207,   196,   197,
      41,   254,    35,    32,     1,    34,     3,     4,    41,    30,
      14,    40,   204,   263,    35,    36,    37,    38,    39,     1,
      41,     5,    11,   188,   314,    46,    15,     9,   220,    18,
     220,    15,   326,    15,   232,    14,   234,    11,    59,   237,
       1,   291,     1,    11,   294,   335,    14,    19,     9,    58,
      11,    21,    13,    14,    13,   217,    17,   219,    17,   221,
     222,   253,   254,   253,   254,   227,   244,   229,    23,   319,
      11,   253,    11,    14,    35,    14,    35,   123,   124,   269,
      41,    13,    41,     1,   274,    17,   284,   285,   251,    36,
     282,   289,    10,    14,   344,    13,    32,   259,   260,    17,
      14,    32,    20,    35,    22,    11,   268,    58,    14,    41,
      28,    11,    30,    31,    14,    43,    43,    35,    36,    37,
      38,    39,   314,    41,   314,     5,     9,    43,    46,    25,
      13,    43,   314,   325,    17,   297,    25,     1,     1,    38,
     338,    59,    60,     6,     7,     8,    43,    10,    18,    13,
      13,    34,    35,    17,    17,    19,    58,    20,    41,    22,
      11,    33,   324,    26,    27,    28,    24,    30,    31,    14,
      19,    35,    35,    36,    37,    38,    39,    41,    41,    42,
      11,    44,     1,    46,    47,    48,    14,    73,    74,    75,
      58,    10,    78,    56,    13,    35,    59,    60,    17,    58,
      25,    20,    11,    22,     5,    58,    33,    15,    41,    28,
      25,    30,    31,     9,    11,    57,    35,    36,    37,    38,
      39,    41,    41,     1,    58,    14,    40,    46,     6,     7,
       8,     1,    10,   333,    11,    13,    41,   335,   143,    17,
      59,    60,    20,    13,    22,   325,   284,    17,    26,    27,
      28,   285,    30,    31,   309,   110,     7,    35,    36,    37,
      38,    39,   187,    41,    42,    35,    44,     1,    46,    47,
      48,    41,    75,   154,   204,   314,    10,     8,    56,    13,
      14,    59,   274,    17,   272,   223,    20,    -1,    22,    -1,
      -1,    -1,    -1,    -1,    28,    -1,    30,    31,    32,     1,
      34,    35,    36,    37,    38,    39,    40,    41,    10,    -1,
      -1,    13,    46,    -1,    -1,    17,    -1,    19,    20,    -1,
      22,    -1,    -1,    -1,    -1,    59,    28,    -1,    30,    31,
      -1,     1,    -1,    35,    36,    37,    38,    39,    -1,    41,
      10,    -1,    -1,    13,    46,    -1,    -1,    17,    -1,    19,
      20,    -1,    22,    -1,    -1,    -1,    -1,    59,    28,    -1,
      30,    31,    -1,     1,    -1,    35,    36,    37,    38,    39,
      -1,    41,    10,    -1,    -1,    13,    46,    -1,    -1,    17,
      -1,    19,    20,    -1,    22,    -1,    -1,    -1,    -1,    59,
      28,    -1,    30,    31,    -1,     1,    -1,    35,    36,    37,
      38,    39,    -1,    41,    10,    -1,    -1,    13,    46,    -1,
      -1,    17,    -1,    -1,    20,    -1,    22,    -1,    -1,    -1,
      -1,    59,    28,    -1,    30,    31,    -1,     1,    -1,    35,
      36,    37,    38,    39,    -1,    41,    10,    -1,    -1,    13,
      46,    -1,    -1,    17,    -1,    -1,    20,    -1,    22,    -1,
      -1,    -1,    -1,    59,    28,    -1,    30,    31,    -1,     1,
      -1,    35,    36,    37,    38,    39,    -1,    41,    10,    -1,
      -1,    13,    46,    -1,    -1,    17,    -1,    -1,    20,    -1,
      22,    -1,    -1,    -1,    -1,    59,    28,    -1,    30,    31,
      -1,     1,    -1,    35,    36,    37,    38,    39,    -1,    41,
      10,    -1,    -1,    13,    46,    -1,    -1,    17,    -1,    -1,
      20,     9,    22,    -1,    -1,    13,    -1,    59,    28,    17,
      30,    31,    -1,    -1,    -1,    35,    36,    37,    38,    39,
      -1,    41,    30,    -1,    -1,    -1,    46,    35,    36,    37,
      38,    39,    -1,    41,    10,    -1,    -1,    13,    14,    59,
      -1,    17,    -1,    -1,    20,    -1,    22,    -1,    -1,    -1,
      -1,    59,    28,    -1,    30,    31,    -1,    -1,    -1,    35,
      36,    37,    38,    39,    10,    41,    -1,    13,    -1,    -1,
      46,    17,    -1,    -1,    20,    -1,    22,    -1,    -1,    -1,
      -1,    -1,    28,    59,    30,    31,    -1,    -1,    -1,    35,
      36,    37,    38,    39,    13,    41,    -1,    -1,    17,    -1,
      46,    -1,    -1,    -1,    -1,    -1,    13,    -1,    -1,    -1,
      17,    30,    -1,    59,    -1,    -1,    35,    36,    37,    38,
      39,    -1,    41,    30,    -1,    -1,    -1,    46,    35,    36,
      37,    38,    39,    -1,    41,    -1,    -1,    -1,    -1,    -1,
      59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    59
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     1,     3,     4,    62,     1,    10,    13,    17,    20,
      22,    28,    30,    31,    35,    36,    37,    38,    39,    41,
      46,    59,   109,   110,   111,   112,   113,   114,   115,   117,
     118,     1,    63,    64,     0,   117,    10,    14,    32,    34,
      40,    92,    93,   112,   115,   119,   112,   119,   126,   112,
     112,   116,   118,   118,    58,   118,    12,    25,   105,    57,
      10,    34,    40,    91,    92,    93,    91,   118,     1,     6,
       7,     8,    26,    27,    42,    44,    47,    48,    56,    65,
      70,   101,   108,   109,   113,    14,    35,    41,    14,    14,
     118,   118,    11,    14,    91,    11,    14,    11,    15,    18,
      19,    21,    23,     9,   118,   101,   102,   118,    58,     1,
      13,    17,    35,    41,    82,    83,    84,    85,    86,   115,
     115,    36,    89,    89,    89,     1,    41,    73,    83,    84,
      83,    84,    96,    96,     1,    13,    94,    95,   109,    84,
      83,    84,     1,    16,    60,   129,   130,    11,    57,     1,
       5,    15,   103,   104,   106,   107,    32,    32,    14,    14,
     112,    14,   112,   112,    31,   112,   127,   128,   112,    58,
     112,   112,    16,   129,   102,     9,    11,    14,    84,    85,
      87,    88,    19,    84,    43,     9,    34,    69,    91,    90,
      91,    90,    90,     5,    69,    43,     5,    43,    25,    97,
      25,    98,    10,    34,    11,    57,    38,    43,    71,    70,
     101,   130,   109,    82,   112,   112,   105,     5,   107,    18,
      58,     5,    29,    11,   113,   120,   121,    24,   101,    33,
     129,    14,    11,    14,     9,    11,    14,    11,    14,    19,
      84,    84,    86,   115,    11,    84,    35,    73,     1,    77,
      78,    84,    84,    58,    58,    95,    82,    73,    25,    11,
       5,   112,   112,   102,   112,   112,   128,     1,     9,    15,
     122,   123,   124,   125,    16,   129,   112,   112,    84,    14,
      84,    91,    33,    74,     5,    15,    49,    79,    32,    93,
       1,    99,   100,   101,   102,    72,    58,    22,   112,   112,
     129,   112,   113,   105,   125,   121,    75,    76,   109,    77,
      78,    13,    41,    84,    16,   129,   129,    25,    13,    66,
      67,    68,   111,   112,     9,    11,    57,    79,    41,    80,
      81,     1,   100,    58,    40,    16,   129,    11,    57,   112,
      76,    82,    14,    11,    66,    67,   111,    84,    41,   129
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 109 "parser.y"
    {inputExpr = (yyvsp[(2) - (2)]);	    sp-=1;}
    break;

  case 3:
#line 110 "parser.y"
    {inputExpr = letrec((yyvsp[(3) - (3)]),(yyvsp[(2) - (3)])); sp-=2;}
    break;

  case 4:
#line 111 "parser.y"
    {valDefns  = (yyvsp[(2) - (2)]);	    sp-=1;}
    break;

  case 5:
#line 112 "parser.y"
    {syntaxError("input");}
    break;

  case 6:
#line 125 "parser.y"
    {(yyval) = gc2((yyvsp[(2) - (3)]));}
    break;

  case 7:
#line 128 "parser.y"
    {yyerrok; goOffside(startColumn);}
    break;

  case 8:
#line 130 "parser.y"
    {(yyval) = gc2((yyvsp[(1) - (3)]));}
    break;

  case 9:
#line 131 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 10:
#line 132 "parser.y"
    {(yyval) = gc0(NIL);}
    break;

  case 11:
#line 133 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 12:
#line 134 "parser.y"
    {syntaxError("definition");}
    break;

  case 13:
#line 138 "parser.y"
    {(yyval) = gc3(appendOnto((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])));}
    break;

  case 14:
#line 139 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 15:
#line 141 "parser.y"
    {(yyval) = gc3(ct1Clause(intOf((yyvsp[(2) - (3)])), (yyvsp[(3) - (3)]), (yyvsp[(1) - (3)])));}
    break;

  case 16:
#line 143 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]), (yyvsp[(1) - (3)])));}
    break;

  case 17:
#line 144 "parser.y"
    {(yyval) = gc1(singleton((yyvsp[(1) - (1)])));}
    break;

  case 18:
#line 147 "parser.y"
    {(yyval) = gc1((yyvsp[(1) - (1)]));}
    break;

  case 19:
#line 148 "parser.y"
    {(yyval) = gc0(NIL);}
    break;

  case 20:
#line 151 "parser.y"
    {typeLhs = (yyvsp[(2) - (2)]);}
    break;

  case 21:
#line 152 "parser.y"
    {defTycon(6,(yyvsp[(1) - (7)]), checkTyLhs((yyvsp[(2) - (7)])), (yyvsp[(6) - (7)]), DATATYPE); typeLhs = NIL;}
    break;

  case 22:
#line 153 "parser.y"
    {typeLhs = (yyvsp[(4) - (4)]);}
    break;

  case 23:
#line 154 "parser.y"
    {defTycon(8,(yyvsp[(1) - (9)]), (yyvsp[(4) - (9)]), ap(QUAL,pair ((yyvsp[(2) - (9)]),(yyvsp[(8) - (9)]))), DATATYPE); typeLhs = NIL;}
    break;

  case 24:
#line 157 "parser.y"
    {defTycon(5,(yyvsp[(3) - (5)]),(yyvsp[(2) - (5)]),(yyvsp[(4) - (5)]),(yyvsp[(5) - (5)]));}
    break;

  case 25:
#line 159 "parser.y"
    {defTycon(5,(yyvsp[(3) - (5)]),checkTyLhs((yyvsp[(2) - (5)])),
							rev((yyvsp[(4) - (5)])),DATATYPE);}
    break;

  case 26:
#line 162 "parser.y"
    {defTycon(7,(yyvsp[(5) - (7)]),(yyvsp[(4) - (7)]),
						  ap(QUAL,pair((yyvsp[(2) - (7)]),rev((yyvsp[(6) - (7)])))),
						  DATATYPE);}
    break;

  case 27:
#line 167 "parser.y"
    {typeSyntaxChk("type Lhs", (yyvsp[(2) - (3)]));
					 (yyval) = gc3(ap((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])));
					}
    break;

  case 28:
#line 170 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 29:
#line 171 "parser.y"
    {syntaxError("type defn lhs");}
    break;

  case 30:
#line 173 "parser.y"
    {(yyval) = gc2((yyvsp[(2) - (2)]));}
    break;

  case 31:
#line 174 "parser.y"
    {(yyval) = gc0(SYNONYM);}
    break;

  case 32:
#line 176 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 33:
#line 177 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 34:
#line 179 "parser.y"
    {(yyval) = gc3(sigdecl((yyvsp[(2) - (3)]),singleton((yyvsp[(1) - (3)])),
							     (yyvsp[(3) - (3)])));}
    break;

  case 35:
#line 181 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 36:
#line 183 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 37:
#line 184 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 38:
#line 186 "parser.y"
    {(yyval) = gc3(ap(ap((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)])),(yyvsp[(3) - (3)])));}
    break;

  case 39:
#line 187 "parser.y"
    {if (!isCon(getHead((yyvsp[(1) - (1)]))))
					     syntaxError("data constructor");
					 (yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 40:
#line 190 "parser.y"
    {syntaxError("data type definition");}
    break;

  case 41:
#line 192 "parser.y"
    {(yyval) = gc0(NIL);}
    break;

  case 42:
#line 193 "parser.y"
    {(yyval) = gc2(singleton((yyvsp[(2) - (2)])));}
    break;

  case 43:
#line 194 "parser.y"
    {(yyval) = gc4((yyvsp[(3) - (4)]));}
    break;

  case 44:
#line 196 "parser.y"
    {(yyval) = gc0(NIL);}
    break;

  case 45:
#line 197 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 46:
#line 199 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 47:
#line 200 "parser.y"
    {(yyval) = gc1(singleton((yyvsp[(1) - (1)])));}
    break;

  case 48:
#line 211 "parser.y"
    {(yyval) = gc3(ap(QUAL,pair((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]))));}
    break;

  case 49:
#line 212 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 50:
#line 214 "parser.y"
    {(yyval) = gc1(checkContext((yyvsp[(1) - (1)])));}
    break;

  case 51:
#line 216 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 52:
#line 217 "parser.y"
    {(yyval) = gc3(ap(ap(ARROW,(yyvsp[(1) - (3)])),(yyvsp[(3) - (3)])));}
    break;

  case 53:
#line 218 "parser.y"
    {syntaxError("type expression");}
    break;

  case 54:
#line 220 "parser.y"
    {typeSyntaxChk("type expression", (yyvsp[(2) - (3)]));
					 (yyval) = gc3(ap((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])));
					}
    break;

  case 55:
#line 224 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 56:
#line 226 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 57:
#line 227 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 58:
#line 228 "parser.y"
    {(yyval) = gc2(UNIT);}
    break;

  case 59:
#line 229 "parser.y"
    {(yyval) = gc3(ARROW);}
    break;

  case 60:
#line 230 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 61:
#line 231 "parser.y"
    {(yyval) = gc4(ap(ARROW,(yyvsp[(2) - (4)])));}
    break;

  case 62:
#line 232 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 63:
#line 233 "parser.y"
    {(yyval) = gc3(buildTuple((yyvsp[(2) - (3)])));}
    break;

  case 64:
#line 234 "parser.y"
    {(yyval) = gc3(ap(LIST,(yyvsp[(2) - (3)])));}
    break;

  case 65:
#line 235 "parser.y"
    {(yyval) = gc2(LIST);}
    break;

  case 66:
#line 237 "parser.y"
    {(yyval) = gc2(mkTuple(tupleOf((yyvsp[(1) - (2)]))+1));}
    break;

  case 67:
#line 238 "parser.y"
    {(yyval) = gc1(mkTuple(2));}
    break;

  case 68:
#line 241 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 69:
#line 242 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),cons((yyvsp[(1) - (3)]),NIL)));}
    break;

  case 70:
#line 247 "parser.y"
    {fixDefn(LEFT_ASS,(yyvsp[(1) - (3)]),(yyvsp[(2) - (3)]),(yyvsp[(3) - (3)])); sp-=3;}
    break;

  case 71:
#line 248 "parser.y"
    {fixDefn(RIGHT_ASS,(yyvsp[(1) - (3)]),(yyvsp[(2) - (3)]),(yyvsp[(3) - (3)]));sp-=3;}
    break;

  case 72:
#line 249 "parser.y"
    {fixDefn(NON_ASS,(yyvsp[(1) - (3)]),(yyvsp[(2) - (3)]),(yyvsp[(3) - (3)]));  sp-=3;}
    break;

  case 73:
#line 251 "parser.y"
    {(yyval) = gc1(checkPrec((yyvsp[(1) - (1)])));}
    break;

  case 74:
#line 252 "parser.y"
    {(yyval) = gc0(mkInt(DEF_PREC));}
    break;

  case 75:
#line 254 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 76:
#line 255 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 77:
#line 257 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 78:
#line 258 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 79:
#line 259 "parser.y"
    {(yyval) = gc1(varMinus);}
    break;

  case 80:
#line 261 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 81:
#line 262 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 82:
#line 264 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 83:
#line 265 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 84:
#line 270 "parser.y"
    {primDefn((yyvsp[(1) - (4)]),(yyvsp[(2) - (4)]),(yyvsp[(4) - (4)])); sp-=4;}
    break;

  case 85:
#line 272 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 86:
#line 273 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 87:
#line 274 "parser.y"
    {syntaxError("primitive defn");}
    break;

  case 88:
#line 276 "parser.y"
    {(yyval) = gc2(pair((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])));}
    break;

  case 89:
#line 281 "parser.y"
    {classDefn(intOf((yyvsp[(1) - (3)])),(yyvsp[(2) - (3)]),(yyvsp[(3) - (3)])); sp-=3;}
    break;

  case 90:
#line 282 "parser.y"
    {instDefn(intOf((yyvsp[(1) - (3)])),(yyvsp[(2) - (3)]),(yyvsp[(3) - (3)]));  sp-=3;}
    break;

  case 91:
#line 283 "parser.y"
    {sp-=2;}
    break;

  case 92:
#line 285 "parser.y"
    {(yyval) = gc3(pair((yyvsp[(1) - (3)]),checkClass((yyvsp[(3) - (3)]))));}
    break;

  case 93:
#line 286 "parser.y"
    {(yyval) = gc1(pair(NIL,checkClass((yyvsp[(1) - (1)]))));}
    break;

  case 94:
#line 288 "parser.y"
    {(yyval) = gc4((yyvsp[(3) - (4)]));}
    break;

  case 95:
#line 289 "parser.y"
    {(yyval) = gc0(NIL);}
    break;

  case 96:
#line 291 "parser.y"
    {(yyval) = gc4((yyvsp[(3) - (4)]));}
    break;

  case 97:
#line 292 "parser.y"
    {(yyval) = gc0(NIL);}
    break;

  case 98:
#line 294 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 99:
#line 295 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 100:
#line 297 "parser.y"
    {(yyval) = gc1((yyvsp[(1) - (1)]));}
    break;

  case 101:
#line 298 "parser.y"
    {syntaxError("class body");}
    break;

  case 102:
#line 303 "parser.y"
    {(yyval) = gc3(sigdecl((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])));}
    break;

  case 103:
#line 304 "parser.y"
    {(yyval) = gc2(pair((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])));}
    break;

  case 104:
#line 306 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 105:
#line 307 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 106:
#line 309 "parser.y"
    {(yyval) = gc2(letrec((yyvsp[(2) - (2)]),(yyvsp[(1) - (2)])));}
    break;

  case 107:
#line 310 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 108:
#line 311 "parser.y"
    {syntaxError("declaration");}
    break;

  case 109:
#line 313 "parser.y"
    {(yyval) = gc2(pair((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])));}
    break;

  case 110:
#line 314 "parser.y"
    {(yyval) = gc1(grded(rev((yyvsp[(1) - (1)]))));}
    break;

  case 111:
#line 316 "parser.y"
    {(yyval) = gc4((yyvsp[(3) - (4)]));}
    break;

  case 112:
#line 318 "parser.y"
    {(yyval) = gc2(cons((yyvsp[(2) - (2)]),(yyvsp[(1) - (2)])));}
    break;

  case 113:
#line 319 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 114:
#line 321 "parser.y"
    {(yyval) = gc4(pair((yyvsp[(3) - (4)]),pair((yyvsp[(2) - (4)]),(yyvsp[(4) - (4)]))));}
    break;

  case 115:
#line 328 "parser.y"
    {(yyval) = gc5(pair((yyvsp[(1) - (5)]),pair((yyvsp[(5) - (5)]),(yyvsp[(2) - (5)]))));}
    break;

  case 116:
#line 329 "parser.y"
    {(yyval) = gc4(pair((yyvsp[(1) - (4)]),pair((yyvsp[(4) - (4)]),(yyvsp[(2) - (4)]))));}
    break;

  case 117:
#line 331 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 118:
#line 332 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 119:
#line 334 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 120:
#line 335 "parser.y"
    {(yyval) = gc3(varMinus);}
    break;

  case 121:
#line 337 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 122:
#line 338 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 123:
#line 340 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 124:
#line 341 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 125:
#line 346 "parser.y"
    {(yyval) = gc3(ap(ESIGN,pair((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]))));}
    break;

  case 126:
#line 347 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 127:
#line 348 "parser.y"
    {syntaxError("expression");}
    break;

  case 128:
#line 350 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 129:
#line 351 "parser.y"
    {(yyval) = gc3(opapd((yyvsp[(1) - (3)]),(yyvsp[(2) - (3)]),(yyvsp[(3) - (3)])));}
    break;

  case 130:
#line 353 "parser.y"
    {(yyval) = gc1((*tidyInfix[newSyntax])((yyvsp[(1) - (1)])));}
    break;

  case 131:
#line 355 "parser.y"
    {(yyval) = gc3(opap((yyvsp[(1) - (3)]),(yyvsp[(2) - (3)]),(yyvsp[(3) - (3)])));}
    break;

  case 132:
#line 356 "parser.y"
    {(yyval) =
 		  gc5(opap(opap(singleton((yyvsp[(1) - (5)])), (yyvsp[(2) - (5)]), (yyvsp[(3) - (5)])),(yyvsp[(4) - (5)]),(yyvsp[(5) - (5)])));}
    break;

  case 133:
#line 359 "parser.y"
    {if (isInt((yyvsp[(2) - (2)])))
					     (yyval) = gc2(mkInt(-intOf((yyvsp[(2) - (2)]))));
					 else
					     (yyval) = gc2(ap(varNegate,(yyvsp[(2) - (2)])));
					}
    break;

  case 134:
#line 364 "parser.y"
    {(yyval) = gc4(ap(LAMBDA,
						     pair(rev((yyvsp[(2) - (4)])),
						          pair((yyvsp[(3) - (4)]),(yyvsp[(4) - (4)])))));}
    break;

  case 135:
#line 367 "parser.y"
    {(yyval) = gc6(letrec((yyvsp[(3) - (6)]),(yyvsp[(6) - (6)])));}
    break;

  case 136:
#line 368 "parser.y"
    {(yyval) = gc6(ap(COND,triple((yyvsp[(2) - (6)]),(yyvsp[(4) - (6)]),(yyvsp[(6) - (6)]))));}
    break;

  case 137:
#line 369 "parser.y"
    {(yyval) = gc6(ap(CASE,pair((yyvsp[(2) - (6)]),rev((yyvsp[(5) - (6)])))));}
    break;

  case 138:
#line 370 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 139:
#line 372 "parser.y"
    {(yyval) = gc2(cons((yyvsp[(2) - (2)]),(yyvsp[(1) - (2)])));}
    break;

  case 140:
#line 373 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 141:
#line 376 "parser.y"
    {
		if (newSyntax)
		{
		  ERROR(row) "Juxtaposition has no meaning. Use ."
		  EEND;
		}
		else
		  (yyval) = gc2(ap((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])));
	      }
    break;

  case 142:
#line 385 "parser.y"
    {(yyval) = gc2(ap(RUNST,(yyvsp[(2) - (2)])));}
    break;

  case 143:
#line 386 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 144:
#line 388 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 145:
#line 389 "parser.y"
    {(yyval) = gc3(ap(ASPAT,pair((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]))));}
    break;

  case 146:
#line 390 "parser.y"
    {(yyval) = gc2(ap(LAZYPAT,(yyvsp[(2) - (2)])));}
    break;

  case 147:
#line 391 "parser.y"
    {(yyval) = gc1(WILDCARD);}
    break;

  case 148:
#line 392 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 149:
#line 393 "parser.y"
    {(yyval) = gc2(UNIT);}
    break;

  case 150:
#line 394 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 151:
#line 395 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 152:
#line 396 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 153:
#line 397 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 154:
#line 398 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 155:
#line 399 "parser.y"
    {(yyval) = gc3(buildTuple((yyvsp[(2) - (3)])));}
    break;

  case 156:
#line 400 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 157:
#line 401 "parser.y"
    {(yyval) = gc4(ap((yyvsp[(3) - (4)]),(yyvsp[(2) - (4)])));}
    break;

  case 158:
#line 402 "parser.y"
    {(yyval) = gc4(ap(ap(varFlip,(yyvsp[(2) - (4)])),(yyvsp[(3) - (4)])));}
    break;

  case 159:
#line 403 "parser.y"
    {(yyval) = gc4(ap(ap(varFlip,(yyvsp[(2) - (4)])),(yyvsp[(3) - (4)])));}
    break;

  case 160:
#line 405 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 161:
#line 406 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),cons((yyvsp[(1) - (3)]),NIL)));}
    break;

  case 162:
#line 408 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 163:
#line 409 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 164:
#line 411 "parser.y"
    {(yyval) = gc2(pair((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])));}
    break;

  case 165:
#line 413 "parser.y"
    {(yyval) = gc2(letrec((yyvsp[(2) - (2)]),(yyvsp[(1) - (2)])));}
    break;

  case 166:
#line 414 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 167:
#line 416 "parser.y"
    {(yyval) = gc1(grded(rev((yyvsp[(1) - (1)]))));}
    break;

  case 168:
#line 417 "parser.y"
    {(yyval) = gc2(pair((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])));}
    break;

  case 169:
#line 418 "parser.y"
    {syntaxError("case expression");}
    break;

  case 170:
#line 420 "parser.y"
    {(yyval) = gc2(cons((yyvsp[(2) - (2)]),(yyvsp[(1) - (2)])));}
    break;

  case 171:
#line 421 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 172:
#line 423 "parser.y"
    {(yyval) = gc4(pair((yyvsp[(3) - (4)]),pair((yyvsp[(2) - (4)]),(yyvsp[(4) - (4)]))));}
    break;

  case 173:
#line 428 "parser.y"
    {(yyval) = gc0(nameNil);}
    break;

  case 174:
#line 429 "parser.y"
    {(yyval) = gc1(ap(FINLIST,cons((yyvsp[(1) - (1)]),NIL)));}
    break;

  case 175:
#line 430 "parser.y"
    {(yyval) = gc1(ap(FINLIST,rev((yyvsp[(1) - (1)]))));}
    break;

  case 176:
#line 431 "parser.y"
    {(yyval) = gc3(ap(COMP,pair((yyvsp[(1) - (3)]),rev((yyvsp[(3) - (3)])))));}
    break;

  case 177:
#line 432 "parser.y"
    {(yyval) = gc3(ap(ap(varFromTo,(yyvsp[(1) - (3)])),(yyvsp[(3) - (3)])));}
    break;

  case 178:
#line 433 "parser.y"
    {(yyval) = gc4(ap(ap(varFromThen,(yyvsp[(1) - (4)])),(yyvsp[(3) - (4)])));}
    break;

  case 179:
#line 434 "parser.y"
    {(yyval) = gc2(ap(varFrom,(yyvsp[(1) - (2)])));}
    break;

  case 180:
#line 435 "parser.y"
    {(yyval) = gc5(ap(ap(ap(varFromThenTo,
                                                               (yyvsp[(1) - (5)])),(yyvsp[(3) - (5)])),(yyvsp[(5) - (5)])));}
    break;

  case 181:
#line 438 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 182:
#line 439 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 183:
#line 441 "parser.y"
    {(yyval) = gc3(ap(FROMQUAL,pair((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]))));}
    break;

  case 184:
#line 442 "parser.y"
    {(yyval) = gc3(ap(QWHERE,
						     singleton(
							pair((yyvsp[(1) - (3)]),pair((yyvsp[(2) - (3)]),
								     (yyvsp[(3) - (3)]))))));}
    break;

  case 185:
#line 446 "parser.y"
    {(yyval) = gc1(ap(BOOLQUAL,(yyvsp[(1) - (1)])));}
    break;

  case 186:
#line 447 "parser.y"
    {(yyval) = gc4(ap(QWHERE,(yyvsp[(3) - (4)])));}
    break;

  case 187:
#line 473 "parser.y"
    {(yyval) = gc2((yyvsp[(2) - (2)]));}
    break;

  case 188:
#line 474 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 189:
#line 476 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 190:
#line 477 "parser.y"
    {yyerrok;
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
    break;


/* Line 1267 of yacc.c.  */
#line 2867 "y.tab.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


#line 492 "parser.y"


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
static Cell local dupStruct(Cell l)
{
  return isAp(l) ? ap(dupStruct(fst(l)), dupStruct(snd(l))) : l;
}

static Cell local ct1Const(int line, Cell type, Cell conid)
{
  push(conid);
  for (; isArrow(type); type = rType(type))
    top() = ap(top(), dupStruct(lType(type)));

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


