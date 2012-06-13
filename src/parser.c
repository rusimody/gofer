
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

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
#define YYBISON_VERSION "2.4.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Copy the first part of user declarations.  */

/* Line 189 of yacc.c  */
#line 18 "parser.y"

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



/* Line 189 of yacc.c  */
#line 123 "y.tab.c"

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


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     EVALEX = 258,
     SCRIPT = 259,
     COCO = 260,
     INFIXL = 261,
     INFIXR = 262,
     INFIX = 263,
     FUNARROW = 264,
     UPTO = 265,
     CASEXP = 266,
     OF = 267,
     IF = 268,
     THEN = 269,
     ELSE = 270,
     WHERE = 271,
     TYPE = 272,
     DATA = 273,
     FROM = 274,
     LET = 275,
     IN = 276,
     VAROP = 277,
     VARID = 278,
     NUMLIT = 279,
     CHARLIT = 280,
     STRINGLIT = 281,
     REPEAT = 282,
     CONOP = 283,
     CONID = 284,
     TCLASS = 285,
     IMPLIES = 286,
     TINSTANCE = 287,
     DO = 288,
     TRUNST = 289,
     PRIMITIVE = 290,
     DEFAULT = 291,
     DERIVING = 292,
     HIDING = 293,
     IMPORT = 294,
     INTERFACE = 295,
     MODULE = 296,
     RENAMING = 297,
     TO = 298
   };
#endif
/* Tokens.  */
#define EVALEX 258
#define SCRIPT 259
#define COCO 260
#define INFIXL 261
#define INFIXR 262
#define INFIX 263
#define FUNARROW 264
#define UPTO 265
#define CASEXP 266
#define OF 267
#define IF 268
#define THEN 269
#define ELSE 270
#define WHERE 271
#define TYPE 272
#define DATA 273
#define FROM 274
#define LET 275
#define IN 276
#define VAROP 277
#define VARID 278
#define NUMLIT 279
#define CHARLIT 280
#define STRINGLIT 281
#define REPEAT 282
#define CONOP 283
#define CONID 284
#define TCLASS 285
#define IMPLIES 286
#define TINSTANCE 287
#define DO 288
#define TRUNST 289
#define PRIMITIVE 290
#define DEFAULT 291
#define DERIVING 292
#define HIDING 293
#define IMPORT 294
#define INTERFACE 295
#define MODULE 296
#define RENAMING 297
#define TO 298




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 264 of yacc.c  */
#line 251 "y.tab.c"

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
YYID (int yyi)
#else
static int
YYID (yyi)
    int yyi;
#endif
{
  return yyi;
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
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
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
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
	Stack = &yyptr->Stack_alloc;					\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  37
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   881

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  60
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  79
/* YYNRULES -- Number of rules.  */
#define YYNRULES  216
/* YYNRULES -- Number of states.  */
#define YYNSTATES  390

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
      14,    15,     2,     2,    12,    11,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,    17,
       2,     5,     2,     2,    13,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    19,    30,    20,     2,    58,    34,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    57,    16,    59,    31,     2,     2,     2,
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
       6,     7,     8,     9,    10,    18,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    32,    33,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     6,    10,    13,    15,    19,    21,    23,
      27,    31,    33,    35,    37,    40,    42,    50,    53,    58,
      61,    63,    65,    66,    70,    74,    76,    78,    81,    82,
      87,    91,    92,    94,    98,   100,   101,   106,   110,   112,
     116,   120,   122,   124,   129,   134,   139,   143,   145,   146,
     148,   154,   160,   168,   171,   173,   175,   178,   179,   183,
     185,   189,   191,   195,   197,   201,   203,   205,   206,   209,
     214,   215,   217,   221,   223,   227,   229,   231,   233,   237,
     239,   242,   244,   246,   248,   251,   255,   259,   264,   268,
     272,   276,   279,   282,   284,   288,   292,   296,   300,   304,
     306,   307,   311,   313,   315,   317,   319,   321,   325,   327,
     331,   336,   340,   342,   344,   347,   351,   355,   358,   362,
     364,   369,   370,   375,   376,   380,   382,   384,   386,   390,
     393,   397,   399,   402,   404,   406,   409,   411,   416,   419,
     421,   426,   432,   437,   441,   443,   445,   449,   451,   455,
     457,   461,   465,   467,   469,   471,   475,   477,   481,   487,
     490,   495,   502,   509,   516,   518,   521,   523,   526,   529,
     531,   533,   537,   540,   542,   544,   547,   549,   551,   553,
     555,   559,   563,   567,   572,   577,   582,   586,   590,   594,
     596,   599,   602,   604,   606,   609,   611,   614,   616,   621,
     622,   624,   626,   630,   634,   639,   642,   648,   652,   654,
     658,   662,   664,   669,   672,   674,   676
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
      61,     0,    -1,     3,   120,    -1,     3,   120,   113,    -1,
       4,    62,    -1,     1,    -1,    63,    64,   137,    -1,    65,
      -1,     1,    -1,    64,    17,    67,    -1,    64,    17,   109,
      -1,    67,    -1,   109,    -1,     1,    -1,    65,    66,    -1,
      66,    -1,    54,    68,    69,    26,    57,    64,   137,    -1,
      54,     1,    -1,    52,    68,    72,    75,    -1,    52,     1,
      -1,    42,    -1,    39,    -1,    -1,    14,    70,    15,    -1,
      70,    12,    71,    -1,    71,    -1,    78,    -1,    68,    18,
      -1,    -1,    51,    14,    74,    15,    -1,    14,    73,    15,
      -1,    -1,    74,    -1,    74,    12,    78,    -1,    78,    -1,
      -1,    55,    14,    76,    15,    -1,    76,    12,    77,    -1,
      77,    -1,   117,    56,   117,    -1,   119,    56,   119,    -1,
     117,    -1,    42,    -1,    42,    14,    18,    15,    -1,    42,
      14,    79,    15,    -1,    42,    14,    80,    15,    -1,    79,
      12,   119,    -1,   119,    -1,    -1,   116,    -1,    27,    81,
       5,    92,    82,    -1,    28,    92,     5,    85,    87,    -1,
      28,    91,    44,    81,     5,    85,    87,    -1,    81,    36,
      -1,    42,    -1,     1,    -1,    33,    83,    -1,    -1,    83,
      12,    84,    -1,    84,    -1,   117,     6,    90,    -1,   117,
      -1,    85,    16,    86,    -1,    86,    -1,    92,   101,    92,
      -1,    92,    -1,     1,    -1,    -1,    50,    42,    -1,    50,
      14,    88,    15,    -1,    -1,    89,    -1,    89,    12,    42,
      -1,    42,    -1,    91,    44,    92,    -1,    92,    -1,    92,
      -1,    93,    -1,    93,    10,    92,    -1,     1,    -1,    93,
      94,    -1,    94,    -1,    36,    -1,    42,    -1,    14,    15,
      -1,    14,    10,    15,    -1,    14,    92,    15,    -1,    14,
      93,    10,    15,    -1,    14,    95,    15,    -1,    14,    96,
      15,    -1,    19,    92,    20,    -1,    19,    20,    -1,    95,
      12,    -1,    12,    -1,    96,    12,    92,    -1,    92,    12,
      92,    -1,     7,    97,    98,    -1,     8,    97,    98,    -1,
       9,    97,    98,    -1,    37,    -1,    -1,    98,    12,    99,
      -1,    99,    -1,   100,    -1,   101,    -1,    11,    -1,    35,
      -1,    34,    36,    34,    -1,    41,    -1,    34,    42,    34,
      -1,    48,   102,     6,    90,    -1,   102,    12,   103,    -1,
     103,    -1,     1,    -1,   117,    39,    -1,    43,   104,   105,
      -1,    45,   104,   106,    -1,    49,    92,    -1,    91,    44,
      92,    -1,    92,    -1,    26,    57,   107,   137,    -1,    -1,
      26,    57,   110,   137,    -1,    -1,   107,    17,   108,    -1,
     108,    -1,   109,    -1,     1,    -1,   116,     6,    90,    -1,
     121,   111,    -1,   110,    17,   109,    -1,   109,    -1,   112,
     113,    -1,   112,    -1,     1,    -1,     5,   120,    -1,   114,
      -1,    26,    57,   110,   137,    -1,   114,   115,    -1,   115,
      -1,    16,   120,     5,   120,    -1,     5,   120,    12,    23,
     120,    -1,     5,   120,    12,   120,    -1,   116,    12,   117,
      -1,   117,    -1,   118,    -1,    14,    11,    15,    -1,    36,
      -1,    14,    35,    15,    -1,    42,    -1,    14,    41,    15,
      -1,   121,     6,    90,    -1,   121,    -1,     1,    -1,   123,
      -1,   123,    99,   123,    -1,   122,    -1,   122,    99,   123,
      -1,   123,    99,   123,    99,   123,    -1,    11,   125,    -1,
      30,   124,    10,   120,    -1,    32,    57,   110,   137,    33,
     120,    -1,    23,   120,    24,   120,    25,   120,    -1,    21,
     120,    22,    57,   128,   137,    -1,   125,    -1,   124,   126,
      -1,   126,    -1,   125,   126,    -1,    47,   126,    -1,   126,
      -1,   117,    -1,   117,    13,   126,    -1,    31,   126,    -1,
      58,    -1,   119,    -1,    14,    15,    -1,    37,    -1,    38,
      -1,    39,    -1,    40,    -1,    14,   120,    15,    -1,    14,
     127,    15,    -1,    19,   134,    20,    -1,    14,   123,    99,
      15,    -1,    14,   100,   126,    15,    -1,    14,   101,   126,
      15,    -1,   127,    12,   120,    -1,   120,    12,   120,    -1,
     128,    17,   129,    -1,   129,    -1,   121,   130,    -1,   131,
     113,    -1,   131,    -1,   132,    -1,    10,   120,    -1,     1,
      -1,   132,   133,    -1,   133,    -1,    16,   121,    10,   120,
      -1,    -1,   120,    -1,   127,    -1,   120,    16,   135,    -1,
     120,    18,   120,    -1,   120,    12,   120,    18,    -1,   120,
      18,    -1,   120,    12,   120,    18,   120,    -1,   135,    12,
     136,    -1,   136,    -1,   120,    29,   120,    -1,   120,     5,
     120,    -1,   120,    -1,    32,    57,   110,   137,    -1,    17,
     138,    -1,   138,    -1,    59,    -1,     1,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    86,    86,    87,    88,    89,   102,   103,   105,   107,
     108,   109,   110,   111,   113,   114,   116,   118,   120,   121,
     123,   124,   126,   127,   129,   130,   132,   133,   135,   136,
     137,   139,   140,   142,   143,   145,   146,   148,   149,   151,
     152,   154,   155,   156,   157,   158,   160,   161,   163,   164,
     169,   170,   173,   178,   179,   180,   182,   183,   185,   186,
     188,   190,   192,   193,   195,   196,   199,   201,   202,   203,
     205,   206,   208,   209,   220,   221,   223,   225,   226,   227,
     229,   230,   232,   233,   234,   235,   236,   237,   238,   239,
     240,   241,   243,   244,   246,   247,   252,   253,   254,   256,
     257,   259,   260,   262,   263,   264,   266,   267,   269,   270,
     275,   277,   278,   279,   281,   286,   287,   288,   290,   291,
     293,   294,   296,   297,   299,   300,   302,   303,   308,   309,
     311,   312,   314,   315,   316,   318,   319,   321,   323,   324,
     326,   333,   334,   336,   337,   339,   340,   342,   343,   345,
     346,   351,   352,   353,   355,   356,   357,   359,   360,   364,
     369,   372,   373,   374,   375,   377,   378,   380,   381,   382,
     384,   385,   386,   387,   388,   389,   390,   391,   392,   393,
     394,   395,   396,   397,   398,   399,   401,   402,   404,   405,
     407,   409,   410,   412,   413,   414,   416,   417,   419,   424,
     425,   426,   427,   428,   429,   430,   431,   434,   435,   437,
     438,   442,   443,   469,   470,   472,   473
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "EVALEX", "SCRIPT", "'='", "COCO",
  "INFIXL", "INFIXR", "INFIX", "FUNARROW", "'-'", "','", "'@'", "'('",
  "')'", "'|'", "';'", "UPTO", "'['", "']'", "CASEXP", "OF", "IF", "THEN",
  "ELSE", "WHERE", "TYPE", "DATA", "FROM", "'\\\\'", "'~'", "LET", "IN",
  "'`'", "VAROP", "VARID", "NUMLIT", "CHARLIT", "STRINGLIT", "REPEAT",
  "CONOP", "CONID", "TCLASS", "IMPLIES", "TINSTANCE", "DO", "TRUNST",
  "PRIMITIVE", "DEFAULT", "DERIVING", "HIDING", "IMPORT", "INTERFACE",
  "MODULE", "RENAMING", "TO", "'{'", "'_'", "'}'", "$accept", "start",
  "topModule", "begin", "topDecls", "modules", "module", "topDecl",
  "modid", "expspec", "exports", "export", "impspec", "imports0",
  "imports", "rename", "renamings", "renaming", "entity", "conids",
  "vars0", "tyLhs", "invars", "rsvars", "rsvar", "constrs", "constr",
  "deriving", "derivs0", "derivs", "sigType", "context", "type", "ctype",
  "atype", "tupCommas", "typeTuple", "optdigit", "ops", "op", "varop",
  "conop", "prims", "prim", "classHead", "classBody", "instBody",
  "csigdecls", "csigdecl", "decl", "decls", "rhs", "rhs1", "wherePart",
  "gdefs", "gdef", "vars", "var", "varid", "conid", "exp", "opExp",
  "opExp0", "pfxExp", "pats", "appExp", "atomic", "exps2", "alts", "alt",
  "altRhs", "altRhs1", "guardAlts", "guardAlt", "list", "quals", "qual",
  "close", "close1", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,    61,   260,   261,   262,   263,
     264,    45,    44,    64,    40,    41,   124,    59,   265,    91,
      93,   266,   267,   268,   269,   270,   271,   272,   273,   274,
      92,   126,   275,   276,    96,   277,   278,   279,   280,   281,
     282,   283,   284,   285,   286,   287,   288,   289,   290,   291,
     292,   293,   294,   295,   296,   297,   298,   123,    95,   125
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    60,    61,    61,    61,    61,    62,    62,    63,    64,
      64,    64,    64,    64,    65,    65,    66,    66,    67,    67,
      68,    68,    69,    69,    70,    70,    71,    71,    72,    72,
      72,    73,    73,    74,    74,    75,    75,    76,    76,    77,
      77,    78,    78,    78,    78,    78,    79,    79,    80,    80,
      67,    67,    67,    81,    81,    81,    82,    82,    83,    83,
      84,    84,    85,    85,    86,    86,    86,    87,    87,    87,
      88,    88,    89,    89,    90,    90,    91,    92,    92,    92,
      93,    93,    94,    94,    94,    94,    94,    94,    94,    94,
      94,    94,    95,    95,    96,    96,    67,    67,    67,    97,
      97,    98,    98,    99,    99,    99,   100,   100,   101,   101,
      67,   102,   102,   102,   103,    67,    67,    67,   104,   104,
     105,   105,   106,   106,   107,   107,   108,   108,   109,   109,
     110,   110,   111,   111,   111,   112,   112,   113,   114,   114,
     115,   115,   115,   116,   116,   117,   117,   118,   118,   119,
     119,   120,   120,   120,   121,   121,   121,   122,   122,   123,
     123,   123,   123,   123,   123,   124,   124,   125,   125,   125,
     126,   126,   126,   126,   126,   126,   126,   126,   126,   126,
     126,   126,   126,   126,   126,   126,   127,   127,   128,   128,
     129,   130,   130,   131,   131,   131,   132,   132,   133,   134,
     134,   134,   134,   134,   134,   134,   134,   135,   135,   136,
     136,   136,   136,   137,   137,   138,   138
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     2,     3,     2,     1,     3,     1,     1,     3,
       3,     1,     1,     1,     2,     1,     7,     2,     4,     2,
       1,     1,     0,     3,     3,     1,     1,     2,     0,     4,
       3,     0,     1,     3,     1,     0,     4,     3,     1,     3,
       3,     1,     1,     4,     4,     4,     3,     1,     0,     1,
       5,     5,     7,     2,     1,     1,     2,     0,     3,     1,
       3,     1,     3,     1,     3,     1,     1,     0,     2,     4,
       0,     1,     3,     1,     3,     1,     1,     1,     3,     1,
       2,     1,     1,     1,     2,     3,     3,     4,     3,     3,
       3,     2,     2,     1,     3,     3,     3,     3,     3,     1,
       0,     3,     1,     1,     1,     1,     1,     3,     1,     3,
       4,     3,     1,     1,     2,     3,     3,     2,     3,     1,
       4,     0,     4,     0,     3,     1,     1,     1,     3,     2,
       3,     1,     2,     1,     1,     2,     1,     4,     2,     1,
       4,     5,     4,     3,     1,     1,     3,     1,     3,     1,
       3,     3,     1,     1,     1,     3,     1,     3,     5,     2,
       4,     6,     6,     6,     1,     2,     1,     2,     2,     1,
       1,     3,     2,     1,     1,     2,     1,     1,     1,     1,
       3,     3,     3,     4,     4,     4,     3,     3,     3,     1,
       2,     2,     1,     1,     2,     1,     2,     1,     4,     0,
       1,     1,     3,     3,     4,     2,     5,     3,     1,     3,
       3,     1,     4,     2,     1,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     5,     0,     0,     0,   153,     0,     0,     0,     0,
       0,     0,     0,     0,   147,   176,   177,   178,   179,   149,
       0,   173,   170,   145,   174,     2,   152,   156,   154,   164,
     169,     8,     0,     4,     0,     7,    15,     1,   159,     0,
     175,     0,   106,   108,     0,     0,     0,   154,     0,   200,
     201,     0,     0,     0,     0,   166,   172,     0,   168,     0,
       0,     3,     0,   105,   106,   108,     0,   103,   104,     0,
     167,    17,    21,    20,    22,    13,   100,   100,   100,     0,
       0,     0,     0,     0,     0,     0,     0,    11,    12,     0,
     170,     0,    14,   146,     0,     0,   148,   150,     0,     0,
       0,   180,     0,     0,   181,     0,     0,     0,   182,     0,
       0,     0,   165,   131,     0,   171,     0,    79,     0,     0,
      82,    83,   151,     0,    75,    77,    81,   157,   155,     0,
       0,    99,     0,     0,     0,    55,    54,     0,     0,    76,
       0,   119,   121,   123,   113,     0,     0,   112,     0,   117,
      19,    28,   216,     0,   215,     6,   214,     0,     0,   134,
       0,     0,   129,   133,   136,   139,   107,   109,   184,   185,
     187,   183,   186,   187,     0,   211,   202,   208,   203,     0,
       0,   160,     0,     0,     0,     0,    93,    84,     0,    77,
       0,     0,    91,     0,     0,     0,    80,     0,    42,     0,
       0,    25,    26,    41,     0,    96,   102,    97,    98,     0,
      53,     0,     0,     0,     0,   115,     0,   116,     0,     0,
       0,     0,   114,    31,     0,    35,     9,    10,   213,   128,
     143,   135,     0,   132,     0,   138,     0,     0,     0,     0,
       0,     0,     0,   189,     0,   130,     0,   137,    85,     0,
      86,     0,    92,    88,     0,    89,    90,    74,    78,   158,
      48,    27,     0,    23,     0,     0,    57,     0,    66,    67,
      63,    65,   118,     0,     0,   110,   111,    42,     0,    32,
      34,     0,     0,    18,     0,     0,     0,   206,     0,   210,
     209,   207,   195,     0,     0,   190,   192,   193,   197,     0,
     163,   162,   161,    95,    87,    94,     0,     0,     0,     0,
      49,   144,    47,    24,     0,   101,     0,    50,     0,     0,
       0,    51,     0,     0,   127,     0,   125,   126,     0,    30,
       0,     0,     0,     0,   142,   140,   212,   194,     0,   191,
     196,   188,     0,    43,     0,    44,    45,    16,    56,    59,
      61,    67,    62,    70,    68,    64,     0,   120,   122,    33,
      29,     0,    38,     0,     0,   141,     0,     0,    46,     0,
       0,    52,    73,     0,    71,   127,   124,     0,    36,     0,
       0,   198,    58,    60,    69,     0,    37,    39,    40,    72
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     4,    33,    34,    86,    35,    36,    87,   199,   130,
     200,   201,   225,   278,   279,   283,   361,   362,   202,   308,
     309,   137,   317,   348,   349,   269,   270,   321,   373,   374,
     122,   123,   124,   125,   126,   190,   191,   132,   205,   206,
      67,    68,   146,   147,   142,   215,   217,   325,   326,   113,
     114,   162,   163,    61,   164,   165,    89,    22,    23,    24,
     175,    26,    27,    28,    54,    29,    30,    48,   242,   243,
     295,   296,   297,   298,    51,   176,   177,   155,   156
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -243
static const yytype_int16 yypact[] =
{
     263,  -243,   593,    13,    33,  -243,   780,   465,   497,   593,
     593,   823,   823,   -22,  -243,  -243,  -243,  -243,  -243,  -243,
     823,  -243,    24,  -243,  -243,    43,    98,    14,    14,   823,
    -243,  -243,    20,  -243,   425,    53,  -243,  -243,   823,   794,
    -243,   105,   112,   125,   823,   823,   177,    14,   204,   148,
     113,   181,   146,   185,   703,  -243,  -243,   765,  -243,   823,
     151,  -243,    75,  -243,  -243,  -243,   765,  -243,  -243,   765,
    -243,  -243,  -243,  -243,   209,  -243,   193,   193,   193,    26,
      75,    75,    75,   115,    75,    46,    19,  -243,  -243,    44,
     215,   179,  -243,  -243,   197,   225,  -243,  -243,   223,   234,
     593,  -243,   735,   593,  -243,   593,   625,   529,  -243,   230,
     593,   593,  -243,  -243,    22,  -243,   765,  -243,   171,    51,
    -243,  -243,  -243,   217,   249,   258,  -243,  -243,    14,   140,
     252,  -243,    14,    14,    14,  -243,  -243,    41,   265,   305,
     270,   249,   300,   302,  -243,   110,   138,  -243,   291,  -243,
    -243,    40,  -243,   340,  -243,  -243,  -243,    75,    82,  -243,
     593,   593,  -243,    43,    90,  -243,  -243,  -243,  -243,  -243,
    -243,  -243,  -243,   315,   279,    29,   322,  -243,  -243,   765,
     313,  -243,   285,   307,    22,   327,  -243,  -243,   236,   293,
     259,   261,  -243,   325,    75,    75,  -243,   765,   143,   328,
     273,  -243,  -243,  -243,   296,   343,  -243,   343,   343,    75,
    -243,    26,   239,    75,   299,  -243,   308,  -243,   342,   112,
      75,    82,  -243,   157,   348,   311,  -243,  -243,  -243,  -243,
    -243,   357,   368,  -243,   593,  -243,   561,   765,   593,   593,
     625,   190,    23,  -243,   593,  -243,   593,  -243,  -243,    75,
    -243,   210,  -243,  -243,    75,  -243,  -243,  -243,  -243,  -243,
     221,  -243,   140,  -243,   425,    14,   341,    56,    81,    10,
    -243,    92,  -243,   657,   765,  -243,  -243,   361,   366,   374,
    -243,   157,   376,  -243,   689,   593,   357,  -243,    22,  -243,
    -243,  -243,  -243,   593,   765,  -243,    43,   375,  -243,   285,
    -243,  -243,  -243,  -243,  -243,  -243,    89,   378,   277,   380,
     384,  -243,  -243,  -243,    19,  -243,    82,  -243,   239,   239,
      15,  -243,   358,    75,  -243,    27,  -243,  -243,    22,  -243,
     157,   286,   184,   593,  -243,  -243,   307,  -243,   391,  -243,
    -243,  -243,   125,  -243,   100,  -243,  -243,  -243,   393,  -243,
     397,    10,  -243,   365,  -243,  -243,   383,  -243,  -243,  -243,
    -243,   290,  -243,   352,   353,   185,   593,   369,  -243,    82,
      75,  -243,  -243,   396,   400,  -243,  -243,   184,  -243,    82,
     100,  -243,  -243,  -243,  -243,   382,  -243,  -243,  -243,  -243
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -243,  -243,  -243,  -243,   152,  -243,   392,   264,   -10,  -243,
    -243,   156,  -243,  -243,   147,  -243,  -243,    52,  -207,  -243,
    -243,   220,  -243,  -243,    66,   120,   118,    94,  -243,  -243,
    -148,   238,   -39,   329,  -106,  -243,  -243,   119,    54,   -15,
     433,    -6,  -243,   222,   367,  -243,  -243,  -243,    95,   -17,
    -105,  -243,  -243,  -153,  -243,   294,   194,   -19,  -243,  -242,
      -2,    -4,  -243,    -3,  -243,    45,   108,   442,  -243,   160,
    -243,  -243,  -243,   163,  -243,  -243,   229,  -111,  -151
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -206
static const yytype_int16 yytable[] =
{
      25,    45,   228,   183,    47,    46,    49,    52,    53,   229,
     233,   184,    66,    69,    31,    90,   280,    88,   312,   196,
     152,    71,    74,   152,   152,    63,   319,   135,   152,   353,
      91,   228,   102,    37,   238,    57,   153,    59,    90,   182,
     299,   139,   141,   141,   356,   149,   209,   150,    41,    64,
     157,    38,   117,    91,   223,    65,   158,   354,   239,    72,
     320,   318,    73,   127,   148,   118,   128,    32,   136,    60,
     119,   192,   275,   247,   280,   151,   117,   210,   154,   188,
     193,   154,   154,   196,    38,    72,   154,   120,    73,   118,
     364,   224,   210,   121,   119,   234,   145,    90,   170,   128,
     218,   172,   368,   173,    62,   178,   161,    32,   180,   181,
     203,   120,    91,   197,   367,   -79,   144,   121,    14,    55,
      56,   218,   -79,   359,   219,   103,   322,    96,    58,   145,
     342,   300,   288,    65,    90,   364,   227,    70,   388,   230,
      97,    94,    19,   339,   220,   219,    70,    95,   228,    91,
     221,    14,    98,    99,   145,   257,   258,   260,   231,   232,
     105,   -20,   112,    90,   106,   245,   107,   115,   109,   328,
     266,   145,   117,   271,   272,   241,    14,   336,    91,    72,
     159,   185,   198,   186,   160,   118,   187,   207,   208,   100,
     119,   292,   101,    14,   259,   161,   133,   134,   306,   277,
     293,   108,   148,   347,   203,   228,   294,   120,   116,   110,
     303,   117,   258,   121,   357,   305,   103,   358,    90,   104,
      14,  -144,   383,   129,   118,   304,    19,  -144,    59,   119,
     131,   166,   286,    91,   287,   306,   289,   290,   168,   307,
     268,   311,   301,   203,   302,    90,   120,    88,   249,   169,
     315,   250,   121,   118,    90,    90,   327,    14,   119,   167,
      91,   194,   203,    19,     1,   323,     2,     3,   195,    91,
      91,   252,   118,   254,   253,   120,   255,   119,   204,   271,
     271,   121,   334,   335,   355,   262,   152,   179,   263,   344,
     338,   337,   345,   -76,   120,   241,     6,   350,   330,     7,
     121,   360,   377,   251,     8,   378,     9,   118,    10,   211,
     212,   203,   119,   363,   213,    11,    12,    13,   138,   140,
     140,    14,    15,    16,    17,    18,   214,    19,   216,   120,
     222,   365,    20,   236,   240,   121,   237,    90,   244,   327,
     246,   152,   248,    21,   154,   256,   261,    76,    77,    78,
     350,     6,    91,   264,     7,   265,   273,    93,   363,     8,
     387,     9,   281,    10,   381,   274,   282,    79,    80,   284,
      11,    12,    13,   285,   316,   260,    14,    15,    16,    17,
      18,   329,    19,    81,   375,    82,   330,    20,    83,    84,
     332,   294,    85,   343,     6,   346,   158,     7,    21,   154,
      95,   366,     8,   370,     9,   369,    10,   372,   379,   380,
     342,   384,   385,    11,    12,    13,   314,   226,   313,    14,
      15,    16,    17,    18,   389,    19,    75,    92,   331,   386,
      20,   267,    76,    77,    78,   382,     6,   352,   351,     7,
      44,    21,   154,   276,     8,   371,     9,   189,    10,   143,
      50,   376,    79,    80,   310,    11,    12,    13,   235,   341,
     340,    14,    15,    16,    17,    18,     5,    19,    81,   291,
      82,     0,    20,    83,    84,     0,    39,    85,     0,     7,
      40,     0,     0,    21,     8,     0,     9,     0,    10,     0,
       0,     0,     0,     0,     0,    11,    12,    13,     5,    41,
      42,    14,    15,    16,    17,    18,    43,    19,     6,     0,
       0,     7,    20,     0,     0,     0,     8,  -199,     9,     0,
      10,     0,     0,    21,     0,     0,     0,    11,    12,    13,
       5,     0,     0,    14,    15,    16,    17,    18,     0,    19,
       6,     0,     0,     7,    20,     0,     0,     0,     8,  -205,
       9,     0,    10,     0,     0,    21,     0,     0,     0,    11,
      12,    13,     5,     0,     0,    14,    15,    16,    17,    18,
       0,    19,     6,     0,     0,     7,    20,     0,     0,     0,
       8,  -204,     9,     0,    10,     0,     0,    21,     0,     0,
       0,    11,    12,    13,     5,     0,     0,    14,    15,    16,
      17,    18,     0,    19,     6,     0,     0,     7,    20,     0,
       0,     0,     8,     0,     9,     0,    10,     0,     0,    21,
       0,     0,     0,    11,    12,    13,     5,     0,     0,    14,
      15,    16,    17,    18,     0,    19,     6,     0,     0,     7,
      20,     0,     0,     0,     8,     0,     9,     0,    10,     0,
       0,    21,     0,     0,     0,    11,    12,   174,   324,     0,
       0,    14,    15,    16,    17,    18,     0,    19,     6,     0,
       0,     7,    20,     0,     0,     0,     8,     0,     9,     0,
      10,     0,     0,    21,     0,     0,     0,    11,    12,    13,
       5,     0,     0,    14,    15,    16,    17,    18,     0,    19,
       6,     0,     0,     7,    20,     0,     0,     0,     8,     0,
       9,     0,   333,   111,     0,    21,     0,     7,     0,    11,
      12,    13,     8,     0,     0,    14,    15,    16,    17,    18,
       0,    19,     0,     0,    12,     0,    20,     0,     0,    14,
      15,    16,    17,    18,     0,    19,     6,    21,     0,     7,
     171,     0,     0,     0,     8,     0,     9,     0,    10,     0,
       0,    21,     0,     0,     0,    11,    12,    13,     0,     0,
       0,    14,    15,    16,    17,    18,     6,    19,     0,     7,
       0,     0,    20,     0,     8,     0,     9,     0,    10,     0,
       0,     0,     0,    21,     7,    11,    12,    13,     0,     8,
       0,    14,    15,    16,    17,    18,     0,    19,     7,    93,
       0,    12,    20,     8,     0,     0,    14,    15,    16,    17,
      18,     0,    19,    21,     0,    12,     0,    20,     0,     0,
      14,    15,    16,    17,    18,     0,    19,     7,    21,     0,
       0,    20,     8,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    21,     0,    12,     0,     0,     0,     0,    14,
      15,    16,    17,    18,     0,    19,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    21
};

static const yytype_int16 yycheck[] =
{
       2,     7,   153,   114,     7,     7,     8,     9,    10,   157,
     163,   116,    27,    28,     1,    34,   223,    34,   260,   125,
       1,     1,    32,     1,     1,    11,    16,     1,     1,    14,
      34,   182,    47,     0,     5,    57,    17,    13,    57,    17,
      17,    80,    81,    82,    17,    84,     5,     1,    34,    35,
       6,     6,     1,    57,    14,    41,    12,    42,    29,    39,
      50,     5,    42,    66,    83,    14,    69,    54,    42,    26,
      19,    20,   220,   184,   281,    85,     1,    36,    59,   118,
     119,    59,    59,   189,    39,    39,    59,    36,    42,    14,
     332,    51,    36,    42,    19,     5,    14,   116,   100,   102,
      11,   103,   344,   105,     6,   107,    16,    54,   110,   111,
     129,    36,   116,   128,    14,    34,     1,    42,    36,    11,
      12,    11,    41,   330,    35,    12,    34,    15,    20,    14,
      41,   242,   237,    41,   153,   377,   153,    29,   380,   158,
      15,    36,    42,   296,     6,    35,    38,    42,   299,   153,
      12,    36,    44,    45,    14,   194,   195,    14,   160,   161,
      12,    18,    54,   182,    16,   182,    18,    59,    22,   274,
     209,    14,     1,   212,   213,   179,    36,   288,   182,    39,
       1,    10,    42,    12,     5,    14,    15,   133,   134,    12,
      19,     1,    15,    36,   197,    16,    77,    78,    14,    42,
      10,    20,   221,   314,   223,   356,    16,    36,    57,    24,
     249,     1,   251,    42,   325,   254,    12,   328,   237,    15,
      36,     6,   370,    14,    14,    15,    42,    12,    13,    19,
      37,    34,   234,   237,   236,    14,   238,   239,    15,    18,
       1,   260,   244,   262,   246,   264,    36,   264,    12,    15,
     265,    15,    42,    14,   273,   274,   273,    36,    19,    34,
     264,    44,   281,    42,     1,   271,     3,     4,    10,   273,
     274,    12,    14,    12,    15,    36,    15,    19,    26,   318,
     319,    42,   284,   285,   323,    12,     1,    57,    15,    12,
     294,   293,    15,    44,    36,   299,    11,   316,    12,    14,
      42,    15,    12,    10,    19,    15,    21,    14,    23,    44,
       5,   330,    19,   332,    44,    30,    31,    32,    80,    81,
      82,    36,    37,    38,    39,    40,    26,    42,    26,    36,
      39,   333,    47,    18,    12,    42,    57,   356,    25,   356,
      33,     1,    15,    58,    59,    20,    18,     7,     8,     9,
     369,    11,   356,    57,    14,    12,    57,    15,   377,    19,
     379,    21,    14,    23,   366,    57,    55,    27,    28,    12,
      30,    31,    32,     5,    33,    14,    36,    37,    38,    39,
      40,    15,    42,    43,     1,    45,    12,    47,    48,    49,
      14,    16,    52,    15,    11,    15,    12,    14,    58,    59,
      42,    10,    19,     6,    21,    12,    23,    42,    56,    56,
      41,    15,    12,    30,    31,    32,   264,   153,   262,    36,
      37,    38,    39,    40,    42,    42,     1,    35,   281,   377,
      47,   211,     7,     8,     9,   369,    11,   319,   318,    14,
       7,    58,    59,   221,    19,   351,    21,   118,    23,    82,
       8,   356,    27,    28,   260,    30,    31,    32,   164,   299,
     297,    36,    37,    38,    39,    40,     1,    42,    43,   240,
      45,    -1,    47,    48,    49,    -1,    11,    52,    -1,    14,
      15,    -1,    -1,    58,    19,    -1,    21,    -1,    23,    -1,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,     1,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    11,    -1,
      -1,    14,    47,    -1,    -1,    -1,    19,    20,    21,    -1,
      23,    -1,    -1,    58,    -1,    -1,    -1,    30,    31,    32,
       1,    -1,    -1,    36,    37,    38,    39,    40,    -1,    42,
      11,    -1,    -1,    14,    47,    -1,    -1,    -1,    19,    20,
      21,    -1,    23,    -1,    -1,    58,    -1,    -1,    -1,    30,
      31,    32,     1,    -1,    -1,    36,    37,    38,    39,    40,
      -1,    42,    11,    -1,    -1,    14,    47,    -1,    -1,    -1,
      19,    20,    21,    -1,    23,    -1,    -1,    58,    -1,    -1,
      -1,    30,    31,    32,     1,    -1,    -1,    36,    37,    38,
      39,    40,    -1,    42,    11,    -1,    -1,    14,    47,    -1,
      -1,    -1,    19,    -1,    21,    -1,    23,    -1,    -1,    58,
      -1,    -1,    -1,    30,    31,    32,     1,    -1,    -1,    36,
      37,    38,    39,    40,    -1,    42,    11,    -1,    -1,    14,
      47,    -1,    -1,    -1,    19,    -1,    21,    -1,    23,    -1,
      -1,    58,    -1,    -1,    -1,    30,    31,    32,     1,    -1,
      -1,    36,    37,    38,    39,    40,    -1,    42,    11,    -1,
      -1,    14,    47,    -1,    -1,    -1,    19,    -1,    21,    -1,
      23,    -1,    -1,    58,    -1,    -1,    -1,    30,    31,    32,
       1,    -1,    -1,    36,    37,    38,    39,    40,    -1,    42,
      11,    -1,    -1,    14,    47,    -1,    -1,    -1,    19,    -1,
      21,    -1,    23,    10,    -1,    58,    -1,    14,    -1,    30,
      31,    32,    19,    -1,    -1,    36,    37,    38,    39,    40,
      -1,    42,    -1,    -1,    31,    -1,    47,    -1,    -1,    36,
      37,    38,    39,    40,    -1,    42,    11,    58,    -1,    14,
      15,    -1,    -1,    -1,    19,    -1,    21,    -1,    23,    -1,
      -1,    58,    -1,    -1,    -1,    30,    31,    32,    -1,    -1,
      -1,    36,    37,    38,    39,    40,    11,    42,    -1,    14,
      -1,    -1,    47,    -1,    19,    -1,    21,    -1,    23,    -1,
      -1,    -1,    -1,    58,    14,    30,    31,    32,    -1,    19,
      -1,    36,    37,    38,    39,    40,    -1,    42,    14,    15,
      -1,    31,    47,    19,    -1,    -1,    36,    37,    38,    39,
      40,    -1,    42,    58,    -1,    31,    -1,    47,    -1,    -1,
      36,    37,    38,    39,    40,    -1,    42,    14,    58,    -1,
      -1,    47,    19,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    58,    -1,    31,    -1,    -1,    -1,    -1,    36,
      37,    38,    39,    40,    -1,    42,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    58
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     1,     3,     4,    61,     1,    11,    14,    19,    21,
      23,    30,    31,    32,    36,    37,    38,    39,    40,    42,
      47,    58,   117,   118,   119,   120,   121,   122,   123,   125,
     126,     1,    54,    62,    63,    65,    66,     0,   125,    11,
      15,    34,    35,    41,   100,   101,   120,   123,   127,   120,
     127,   134,   120,   120,   124,   126,   126,    57,   126,    13,
      26,   113,     6,    11,    35,    41,    99,   100,   101,    99,
     126,     1,    39,    42,    68,     1,     7,     8,     9,    27,
      28,    43,    45,    48,    49,    52,    64,    67,   109,   116,
     117,   121,    66,    15,    36,    42,    15,    15,   126,   126,
      12,    15,    99,    12,    15,    12,    16,    18,    20,    22,
      24,    10,   126,   109,   110,   126,    57,     1,    14,    19,
      36,    42,    90,    91,    92,    93,    94,   123,   123,    14,
      69,    37,    97,    97,    97,     1,    42,    81,    91,    92,
      91,    92,   104,   104,     1,    14,   102,   103,   117,    92,
       1,    68,     1,    17,    59,   137,   138,     6,    12,     1,
       5,    16,   111,   112,   114,   115,    34,    34,    15,    15,
     120,    15,   120,   120,    32,   120,   135,   136,   120,    57,
     120,   120,    17,   137,   110,    10,    12,    15,    92,    93,
      95,    96,    20,    92,    44,    10,    94,    99,    42,    68,
      70,    71,    78,   117,    26,    98,    99,    98,    98,     5,
      36,    44,     5,    44,    26,   105,    26,   106,    11,    35,
       6,    12,    39,    14,    51,    72,    67,   109,   138,    90,
     117,   120,   120,   113,     5,   115,    18,    57,     5,    29,
      12,   121,   128,   129,    25,   109,    33,   137,    15,    12,
      15,    10,    12,    15,    12,    15,    20,    92,    92,   123,
      14,    18,    12,    15,    57,    12,    92,    81,     1,    85,
      86,    92,    92,    57,    57,    90,   103,    42,    73,    74,
      78,    14,    55,    75,    12,     5,   120,   120,   110,   120,
     120,   136,     1,    10,    16,   130,   131,   132,   133,    17,
     137,   120,   120,    92,    15,    92,    14,    18,    79,    80,
     116,   117,   119,    71,    64,    99,    33,    82,     5,    16,
      50,    87,    34,   101,     1,   107,   108,   109,   110,    15,
      12,    74,    14,    23,   120,   120,   137,   120,   121,   113,
     133,   129,    41,    15,    12,    15,    15,   137,    83,    84,
     117,    85,    86,    14,    42,    92,    17,   137,   137,    78,
      15,    76,    77,   117,   119,   120,    10,    14,   119,    12,
       6,    87,    42,    88,    89,     1,   108,    12,    15,    56,
      56,   120,    84,    90,    15,    12,    77,   117,   119,    42
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
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
    yytype_int16 *yybottom;
    yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
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
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      YYFPRINTF (stderr, "\n");
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


/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*-------------------------.
| yyparse or yypush_parse.  |
`-------------------------*/

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
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.

       Refer to the stacks thru separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yytoken = 0;
  yyss = yyssa;
  yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

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
	YYSTACK_RELOCATE (yyss_alloc, yyss);
	YYSTACK_RELOCATE (yyvs_alloc, yyvs);
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

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
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

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
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

/* Line 1455 of yacc.c  */
#line 86 "parser.y"
    {inputExpr = (yyvsp[(2) - (2)]);	    sp-=1;}
    break;

  case 3:

/* Line 1455 of yacc.c  */
#line 87 "parser.y"
    {inputExpr = letrec((yyvsp[(3) - (3)]),(yyvsp[(2) - (3)])); sp-=2;}
    break;

  case 4:

/* Line 1455 of yacc.c  */
#line 88 "parser.y"
    {valDefns  = (yyvsp[(2) - (2)]);	    sp-=1;}
    break;

  case 5:

/* Line 1455 of yacc.c  */
#line 89 "parser.y"
    {syntaxError("input");}
    break;

  case 6:

/* Line 1455 of yacc.c  */
#line 102 "parser.y"
    {(yyval) = gc2((yyvsp[(2) - (3)]));}
    break;

  case 7:

/* Line 1455 of yacc.c  */
#line 103 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 8:

/* Line 1455 of yacc.c  */
#line 105 "parser.y"
    {yyerrok; goOffside(startColumn);}
    break;

  case 9:

/* Line 1455 of yacc.c  */
#line 107 "parser.y"
    {(yyval) = gc2((yyvsp[(1) - (3)]));}
    break;

  case 10:

/* Line 1455 of yacc.c  */
#line 108 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 11:

/* Line 1455 of yacc.c  */
#line 109 "parser.y"
    {(yyval) = gc0(NIL);}
    break;

  case 12:

/* Line 1455 of yacc.c  */
#line 110 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 13:

/* Line 1455 of yacc.c  */
#line 111 "parser.y"
    {syntaxError("definition");}
    break;

  case 14:

/* Line 1455 of yacc.c  */
#line 113 "parser.y"
    {(yyval) = gc2(appendOnto((yyvsp[(2) - (2)]),(yyvsp[(1) - (2)])));}
    break;

  case 15:

/* Line 1455 of yacc.c  */
#line 114 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 16:

/* Line 1455 of yacc.c  */
#line 117 "parser.y"
    {(yyval) = gc7((yyvsp[(6) - (7)]));}
    break;

  case 17:

/* Line 1455 of yacc.c  */
#line 118 "parser.y"
    {syntaxError("module definition");}
    break;

  case 18:

/* Line 1455 of yacc.c  */
#line 120 "parser.y"
    {sp-=4;}
    break;

  case 19:

/* Line 1455 of yacc.c  */
#line 121 "parser.y"
    {syntaxError("import declaration");}
    break;

  case 20:

/* Line 1455 of yacc.c  */
#line 123 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 21:

/* Line 1455 of yacc.c  */
#line 124 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 22:

/* Line 1455 of yacc.c  */
#line 126 "parser.y"
    {(yyval) = gc0(NIL);}
    break;

  case 23:

/* Line 1455 of yacc.c  */
#line 127 "parser.y"
    {(yyval) = gc3(NIL);}
    break;

  case 24:

/* Line 1455 of yacc.c  */
#line 129 "parser.y"
    {(yyval) = gc3(NIL);}
    break;

  case 25:

/* Line 1455 of yacc.c  */
#line 130 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 26:

/* Line 1455 of yacc.c  */
#line 132 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 27:

/* Line 1455 of yacc.c  */
#line 133 "parser.y"
    {(yyval) = gc2(NIL);}
    break;

  case 28:

/* Line 1455 of yacc.c  */
#line 135 "parser.y"
    {(yyval) = gc0(NIL);}
    break;

  case 29:

/* Line 1455 of yacc.c  */
#line 136 "parser.y"
    {(yyval) = gc4(NIL);}
    break;

  case 30:

/* Line 1455 of yacc.c  */
#line 137 "parser.y"
    {(yyval) = gc3(NIL);}
    break;

  case 31:

/* Line 1455 of yacc.c  */
#line 139 "parser.y"
    {(yyval) = gc0(NIL);}
    break;

  case 32:

/* Line 1455 of yacc.c  */
#line 140 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 33:

/* Line 1455 of yacc.c  */
#line 142 "parser.y"
    {(yyval) = gc3(NIL);}
    break;

  case 34:

/* Line 1455 of yacc.c  */
#line 143 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 35:

/* Line 1455 of yacc.c  */
#line 145 "parser.y"
    {(yyval) = gc0(NIL);}
    break;

  case 36:

/* Line 1455 of yacc.c  */
#line 146 "parser.y"
    {(yyval) = gc4(NIL);}
    break;

  case 37:

/* Line 1455 of yacc.c  */
#line 148 "parser.y"
    {(yyval) = gc3(NIL);}
    break;

  case 38:

/* Line 1455 of yacc.c  */
#line 149 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 39:

/* Line 1455 of yacc.c  */
#line 151 "parser.y"
    {(yyval) = gc3(NIL);}
    break;

  case 40:

/* Line 1455 of yacc.c  */
#line 152 "parser.y"
    {(yyval) = gc3(NIL);}
    break;

  case 41:

/* Line 1455 of yacc.c  */
#line 154 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 42:

/* Line 1455 of yacc.c  */
#line 155 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 43:

/* Line 1455 of yacc.c  */
#line 156 "parser.y"
    {(yyval) = gc4(NIL);}
    break;

  case 44:

/* Line 1455 of yacc.c  */
#line 157 "parser.y"
    {(yyval) = gc4(NIL);}
    break;

  case 45:

/* Line 1455 of yacc.c  */
#line 158 "parser.y"
    {(yyval) = gc4(NIL);}
    break;

  case 46:

/* Line 1455 of yacc.c  */
#line 160 "parser.y"
    {(yyval) = gc3(NIL);}
    break;

  case 47:

/* Line 1455 of yacc.c  */
#line 161 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 48:

/* Line 1455 of yacc.c  */
#line 163 "parser.y"
    {(yyval) = gc0(NIL);}
    break;

  case 49:

/* Line 1455 of yacc.c  */
#line 164 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 50:

/* Line 1455 of yacc.c  */
#line 169 "parser.y"
    {defTycon(5,(yyvsp[(3) - (5)]),(yyvsp[(2) - (5)]),(yyvsp[(4) - (5)]),(yyvsp[(5) - (5)]));}
    break;

  case 51:

/* Line 1455 of yacc.c  */
#line 171 "parser.y"
    {defTycon(5,(yyvsp[(3) - (5)]),checkTyLhs((yyvsp[(2) - (5)])),
							rev((yyvsp[(4) - (5)])),DATATYPE);}
    break;

  case 52:

/* Line 1455 of yacc.c  */
#line 174 "parser.y"
    {defTycon(7,(yyvsp[(5) - (7)]),(yyvsp[(4) - (7)]),
						  ap(QUAL,pair((yyvsp[(2) - (7)]),rev((yyvsp[(6) - (7)])))),
						  DATATYPE);}
    break;

  case 53:

/* Line 1455 of yacc.c  */
#line 178 "parser.y"
    {(yyval) = gc2(ap((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])));}
    break;

  case 54:

/* Line 1455 of yacc.c  */
#line 179 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 55:

/* Line 1455 of yacc.c  */
#line 180 "parser.y"
    {syntaxError("type defn lhs");}
    break;

  case 56:

/* Line 1455 of yacc.c  */
#line 182 "parser.y"
    {(yyval) = gc2((yyvsp[(2) - (2)]));}
    break;

  case 57:

/* Line 1455 of yacc.c  */
#line 183 "parser.y"
    {(yyval) = gc0(SYNONYM);}
    break;

  case 58:

/* Line 1455 of yacc.c  */
#line 185 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 59:

/* Line 1455 of yacc.c  */
#line 186 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 60:

/* Line 1455 of yacc.c  */
#line 188 "parser.y"
    {(yyval) = gc3(sigdecl((yyvsp[(2) - (3)]),singleton((yyvsp[(1) - (3)])),
							     (yyvsp[(3) - (3)])));}
    break;

  case 61:

/* Line 1455 of yacc.c  */
#line 190 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 62:

/* Line 1455 of yacc.c  */
#line 192 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 63:

/* Line 1455 of yacc.c  */
#line 193 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 64:

/* Line 1455 of yacc.c  */
#line 195 "parser.y"
    {(yyval) = gc3(ap(ap((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)])),(yyvsp[(3) - (3)])));}
    break;

  case 65:

/* Line 1455 of yacc.c  */
#line 196 "parser.y"
    {if (!isCon(getHead((yyvsp[(1) - (1)]))))
					     syntaxError("data constructor");
					 (yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 66:

/* Line 1455 of yacc.c  */
#line 199 "parser.y"
    {syntaxError("data type definition");}
    break;

  case 67:

/* Line 1455 of yacc.c  */
#line 201 "parser.y"
    {(yyval) = gc0(NIL);}
    break;

  case 68:

/* Line 1455 of yacc.c  */
#line 202 "parser.y"
    {(yyval) = gc2(singleton((yyvsp[(2) - (2)])));}
    break;

  case 69:

/* Line 1455 of yacc.c  */
#line 203 "parser.y"
    {(yyval) = gc4((yyvsp[(3) - (4)]));}
    break;

  case 70:

/* Line 1455 of yacc.c  */
#line 205 "parser.y"
    {(yyval) = gc0(NIL);}
    break;

  case 71:

/* Line 1455 of yacc.c  */
#line 206 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 72:

/* Line 1455 of yacc.c  */
#line 208 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 73:

/* Line 1455 of yacc.c  */
#line 209 "parser.y"
    {(yyval) = gc1(singleton((yyvsp[(1) - (1)])));}
    break;

  case 74:

/* Line 1455 of yacc.c  */
#line 220 "parser.y"
    {(yyval) = gc3(ap(QUAL,pair((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]))));}
    break;

  case 75:

/* Line 1455 of yacc.c  */
#line 221 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 76:

/* Line 1455 of yacc.c  */
#line 223 "parser.y"
    {(yyval) = gc1(checkContext((yyvsp[(1) - (1)])));}
    break;

  case 77:

/* Line 1455 of yacc.c  */
#line 225 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 78:

/* Line 1455 of yacc.c  */
#line 226 "parser.y"
    {(yyval) = gc3(ap(ap(ARROW,(yyvsp[(1) - (3)])),(yyvsp[(3) - (3)])));}
    break;

  case 79:

/* Line 1455 of yacc.c  */
#line 227 "parser.y"
    {syntaxError("type expression");}
    break;

  case 80:

/* Line 1455 of yacc.c  */
#line 229 "parser.y"
    {(yyval) = gc2(ap((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])));}
    break;

  case 81:

/* Line 1455 of yacc.c  */
#line 230 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 82:

/* Line 1455 of yacc.c  */
#line 232 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 83:

/* Line 1455 of yacc.c  */
#line 233 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 84:

/* Line 1455 of yacc.c  */
#line 234 "parser.y"
    {(yyval) = gc2(UNIT);}
    break;

  case 85:

/* Line 1455 of yacc.c  */
#line 235 "parser.y"
    {(yyval) = gc3(ARROW);}
    break;

  case 86:

/* Line 1455 of yacc.c  */
#line 236 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 87:

/* Line 1455 of yacc.c  */
#line 237 "parser.y"
    {(yyval) = gc4(ap(ARROW,(yyvsp[(2) - (4)])));}
    break;

  case 88:

/* Line 1455 of yacc.c  */
#line 238 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 89:

/* Line 1455 of yacc.c  */
#line 239 "parser.y"
    {(yyval) = gc3(buildTuple((yyvsp[(2) - (3)])));}
    break;

  case 90:

/* Line 1455 of yacc.c  */
#line 240 "parser.y"
    {(yyval) = gc3(ap(LIST,(yyvsp[(2) - (3)])));}
    break;

  case 91:

/* Line 1455 of yacc.c  */
#line 241 "parser.y"
    {(yyval) = gc2(LIST);}
    break;

  case 92:

/* Line 1455 of yacc.c  */
#line 243 "parser.y"
    {(yyval) = gc2(mkTuple(tupleOf((yyvsp[(1) - (2)]))+1));}
    break;

  case 93:

/* Line 1455 of yacc.c  */
#line 244 "parser.y"
    {(yyval) = gc1(mkTuple(2));}
    break;

  case 94:

/* Line 1455 of yacc.c  */
#line 246 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 95:

/* Line 1455 of yacc.c  */
#line 247 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),cons((yyvsp[(1) - (3)]),NIL)));}
    break;

  case 96:

/* Line 1455 of yacc.c  */
#line 252 "parser.y"
    {fixDefn(LEFT_ASS,(yyvsp[(1) - (3)]),(yyvsp[(2) - (3)]),(yyvsp[(3) - (3)])); sp-=3;}
    break;

  case 97:

/* Line 1455 of yacc.c  */
#line 253 "parser.y"
    {fixDefn(RIGHT_ASS,(yyvsp[(1) - (3)]),(yyvsp[(2) - (3)]),(yyvsp[(3) - (3)]));sp-=3;}
    break;

  case 98:

/* Line 1455 of yacc.c  */
#line 254 "parser.y"
    {fixDefn(NON_ASS,(yyvsp[(1) - (3)]),(yyvsp[(2) - (3)]),(yyvsp[(3) - (3)]));  sp-=3;}
    break;

  case 99:

/* Line 1455 of yacc.c  */
#line 256 "parser.y"
    {(yyval) = gc1(checkPrec((yyvsp[(1) - (1)])));}
    break;

  case 100:

/* Line 1455 of yacc.c  */
#line 257 "parser.y"
    {(yyval) = gc0(mkInt(DEF_PREC));}
    break;

  case 101:

/* Line 1455 of yacc.c  */
#line 259 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 102:

/* Line 1455 of yacc.c  */
#line 260 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 103:

/* Line 1455 of yacc.c  */
#line 262 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 104:

/* Line 1455 of yacc.c  */
#line 263 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 105:

/* Line 1455 of yacc.c  */
#line 264 "parser.y"
    {(yyval) = gc1(varMinus);}
    break;

  case 106:

/* Line 1455 of yacc.c  */
#line 266 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 107:

/* Line 1455 of yacc.c  */
#line 267 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 108:

/* Line 1455 of yacc.c  */
#line 269 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 109:

/* Line 1455 of yacc.c  */
#line 270 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 110:

/* Line 1455 of yacc.c  */
#line 275 "parser.y"
    {primDefn((yyvsp[(1) - (4)]),(yyvsp[(2) - (4)]),(yyvsp[(4) - (4)])); sp-=4;}
    break;

  case 111:

/* Line 1455 of yacc.c  */
#line 277 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 112:

/* Line 1455 of yacc.c  */
#line 278 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 113:

/* Line 1455 of yacc.c  */
#line 279 "parser.y"
    {syntaxError("primitive defn");}
    break;

  case 114:

/* Line 1455 of yacc.c  */
#line 281 "parser.y"
    {(yyval) = gc2(pair((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])));}
    break;

  case 115:

/* Line 1455 of yacc.c  */
#line 286 "parser.y"
    {classDefn(intOf((yyvsp[(1) - (3)])),(yyvsp[(2) - (3)]),(yyvsp[(3) - (3)])); sp-=3;}
    break;

  case 116:

/* Line 1455 of yacc.c  */
#line 287 "parser.y"
    {instDefn(intOf((yyvsp[(1) - (3)])),(yyvsp[(2) - (3)]),(yyvsp[(3) - (3)]));  sp-=3;}
    break;

  case 117:

/* Line 1455 of yacc.c  */
#line 288 "parser.y"
    {sp-=2;}
    break;

  case 118:

/* Line 1455 of yacc.c  */
#line 290 "parser.y"
    {(yyval) = gc3(pair((yyvsp[(1) - (3)]),checkClass((yyvsp[(3) - (3)]))));}
    break;

  case 119:

/* Line 1455 of yacc.c  */
#line 291 "parser.y"
    {(yyval) = gc1(pair(NIL,checkClass((yyvsp[(1) - (1)]))));}
    break;

  case 120:

/* Line 1455 of yacc.c  */
#line 293 "parser.y"
    {(yyval) = gc4((yyvsp[(3) - (4)]));}
    break;

  case 121:

/* Line 1455 of yacc.c  */
#line 294 "parser.y"
    {(yyval) = gc0(NIL);}
    break;

  case 122:

/* Line 1455 of yacc.c  */
#line 296 "parser.y"
    {(yyval) = gc4((yyvsp[(3) - (4)]));}
    break;

  case 123:

/* Line 1455 of yacc.c  */
#line 297 "parser.y"
    {(yyval) = gc0(NIL);}
    break;

  case 124:

/* Line 1455 of yacc.c  */
#line 299 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 125:

/* Line 1455 of yacc.c  */
#line 300 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 126:

/* Line 1455 of yacc.c  */
#line 302 "parser.y"
    {(yyval) = gc1((yyvsp[(1) - (1)]));}
    break;

  case 127:

/* Line 1455 of yacc.c  */
#line 303 "parser.y"
    {syntaxError("class body");}
    break;

  case 128:

/* Line 1455 of yacc.c  */
#line 308 "parser.y"
    {(yyval) = gc3(sigdecl((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])));}
    break;

  case 129:

/* Line 1455 of yacc.c  */
#line 309 "parser.y"
    {(yyval) = gc2(pair((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])));}
    break;

  case 130:

/* Line 1455 of yacc.c  */
#line 311 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 131:

/* Line 1455 of yacc.c  */
#line 312 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 132:

/* Line 1455 of yacc.c  */
#line 314 "parser.y"
    {(yyval) = gc2(letrec((yyvsp[(2) - (2)]),(yyvsp[(1) - (2)])));}
    break;

  case 133:

/* Line 1455 of yacc.c  */
#line 315 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 134:

/* Line 1455 of yacc.c  */
#line 316 "parser.y"
    {syntaxError("declaration");}
    break;

  case 135:

/* Line 1455 of yacc.c  */
#line 318 "parser.y"
    {(yyval) = gc2(pair((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])));}
    break;

  case 136:

/* Line 1455 of yacc.c  */
#line 319 "parser.y"
    {(yyval) = gc1(grded(rev((yyvsp[(1) - (1)]))));}
    break;

  case 137:

/* Line 1455 of yacc.c  */
#line 321 "parser.y"
    {(yyval) = gc4((yyvsp[(3) - (4)]));}
    break;

  case 138:

/* Line 1455 of yacc.c  */
#line 323 "parser.y"
    {(yyval) = gc2(cons((yyvsp[(2) - (2)]),(yyvsp[(1) - (2)])));}
    break;

  case 139:

/* Line 1455 of yacc.c  */
#line 324 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 140:

/* Line 1455 of yacc.c  */
#line 326 "parser.y"
    {(yyval) = gc4(pair((yyvsp[(3) - (4)]),pair((yyvsp[(2) - (4)]),(yyvsp[(4) - (4)]))));}
    break;

  case 141:

/* Line 1455 of yacc.c  */
#line 333 "parser.y"
    {(yyval) = gc5(pair((yyvsp[(1) - (5)]),pair((yyvsp[(5) - (5)]),(yyvsp[(2) - (5)]))));}
    break;

  case 142:

/* Line 1455 of yacc.c  */
#line 334 "parser.y"
    {(yyval) = gc4(pair((yyvsp[(1) - (4)]),pair((yyvsp[(4) - (4)]),(yyvsp[(2) - (4)]))));}
    break;

  case 143:

/* Line 1455 of yacc.c  */
#line 336 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 144:

/* Line 1455 of yacc.c  */
#line 337 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 145:

/* Line 1455 of yacc.c  */
#line 339 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 146:

/* Line 1455 of yacc.c  */
#line 340 "parser.y"
    {(yyval) = gc3(varMinus);}
    break;

  case 147:

/* Line 1455 of yacc.c  */
#line 342 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 148:

/* Line 1455 of yacc.c  */
#line 343 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 149:

/* Line 1455 of yacc.c  */
#line 345 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 150:

/* Line 1455 of yacc.c  */
#line 346 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 151:

/* Line 1455 of yacc.c  */
#line 351 "parser.y"
    {(yyval) = gc3(ap(ESIGN,pair((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]))));}
    break;

  case 152:

/* Line 1455 of yacc.c  */
#line 352 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 153:

/* Line 1455 of yacc.c  */
#line 353 "parser.y"
    {syntaxError("expression");}
    break;

  case 154:

/* Line 1455 of yacc.c  */
#line 355 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 155:

/* Line 1455 of yacc.c  */
#line 356 "parser.y"
    {(yyval) = gc3(ap(ap((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)])),(yyvsp[(3) - (3)])));}
    break;

  case 156:

/* Line 1455 of yacc.c  */
#line 357 "parser.y"
    {(yyval) = gc1(tidyInfix((yyvsp[(1) - (1)])));}
    break;

  case 157:

/* Line 1455 of yacc.c  */
#line 359 "parser.y"
    {(yyval) = gc3(ap(ap((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)])),(yyvsp[(3) - (3)])));}
    break;

  case 158:

/* Line 1455 of yacc.c  */
#line 360 "parser.y"
    {(yyval) = gc5(ap(ap((yyvsp[(4) - (5)]),
							ap(ap((yyvsp[(2) - (5)]),singleton((yyvsp[(1) - (5)]))),
                                                           (yyvsp[(3) - (5)]))),(yyvsp[(5) - (5)])));}
    break;

  case 159:

/* Line 1455 of yacc.c  */
#line 364 "parser.y"
    {if (isInt((yyvsp[(2) - (2)])))
					     (yyval) = gc2(mkInt(-intOf((yyvsp[(2) - (2)]))));
					 else
					     (yyval) = gc2(ap(varNegate,(yyvsp[(2) - (2)])));
					}
    break;

  case 160:

/* Line 1455 of yacc.c  */
#line 369 "parser.y"
    {(yyval) = gc4(ap(LAMBDA,
						     pair(rev((yyvsp[(2) - (4)])),
						          pair((yyvsp[(3) - (4)]),(yyvsp[(4) - (4)])))));}
    break;

  case 161:

/* Line 1455 of yacc.c  */
#line 372 "parser.y"
    {(yyval) = gc6(letrec((yyvsp[(3) - (6)]),(yyvsp[(6) - (6)])));}
    break;

  case 162:

/* Line 1455 of yacc.c  */
#line 373 "parser.y"
    {(yyval) = gc6(ap(COND,triple((yyvsp[(2) - (6)]),(yyvsp[(4) - (6)]),(yyvsp[(6) - (6)]))));}
    break;

  case 163:

/* Line 1455 of yacc.c  */
#line 374 "parser.y"
    {(yyval) = gc6(ap(CASE,pair((yyvsp[(2) - (6)]),rev((yyvsp[(5) - (6)])))));}
    break;

  case 164:

/* Line 1455 of yacc.c  */
#line 375 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 165:

/* Line 1455 of yacc.c  */
#line 377 "parser.y"
    {(yyval) = gc2(cons((yyvsp[(2) - (2)]),(yyvsp[(1) - (2)])));}
    break;

  case 166:

/* Line 1455 of yacc.c  */
#line 378 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 167:

/* Line 1455 of yacc.c  */
#line 380 "parser.y"
    {(yyval) = gc2(ap((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])));}
    break;

  case 168:

/* Line 1455 of yacc.c  */
#line 381 "parser.y"
    {(yyval) = gc2(ap(RUNST,(yyvsp[(2) - (2)])));}
    break;

  case 169:

/* Line 1455 of yacc.c  */
#line 382 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 170:

/* Line 1455 of yacc.c  */
#line 384 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 171:

/* Line 1455 of yacc.c  */
#line 385 "parser.y"
    {(yyval) = gc3(ap(ASPAT,pair((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]))));}
    break;

  case 172:

/* Line 1455 of yacc.c  */
#line 386 "parser.y"
    {(yyval) = gc2(ap(LAZYPAT,(yyvsp[(2) - (2)])));}
    break;

  case 173:

/* Line 1455 of yacc.c  */
#line 387 "parser.y"
    {(yyval) = gc1(WILDCARD);}
    break;

  case 174:

/* Line 1455 of yacc.c  */
#line 388 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 175:

/* Line 1455 of yacc.c  */
#line 389 "parser.y"
    {(yyval) = gc2(UNIT);}
    break;

  case 176:

/* Line 1455 of yacc.c  */
#line 390 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 177:

/* Line 1455 of yacc.c  */
#line 391 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 178:

/* Line 1455 of yacc.c  */
#line 392 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 179:

/* Line 1455 of yacc.c  */
#line 393 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 180:

/* Line 1455 of yacc.c  */
#line 394 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 181:

/* Line 1455 of yacc.c  */
#line 395 "parser.y"
    {(yyval) = gc3(buildTuple((yyvsp[(2) - (3)])));}
    break;

  case 182:

/* Line 1455 of yacc.c  */
#line 396 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 183:

/* Line 1455 of yacc.c  */
#line 397 "parser.y"
    {(yyval) = gc4(ap((yyvsp[(3) - (4)]),(yyvsp[(2) - (4)])));}
    break;

  case 184:

/* Line 1455 of yacc.c  */
#line 398 "parser.y"
    {(yyval) = gc4(ap(ap(varFlip,(yyvsp[(2) - (4)])),(yyvsp[(3) - (4)])));}
    break;

  case 185:

/* Line 1455 of yacc.c  */
#line 399 "parser.y"
    {(yyval) = gc4(ap(ap(varFlip,(yyvsp[(2) - (4)])),(yyvsp[(3) - (4)])));}
    break;

  case 186:

/* Line 1455 of yacc.c  */
#line 401 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 187:

/* Line 1455 of yacc.c  */
#line 402 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),cons((yyvsp[(1) - (3)]),NIL)));}
    break;

  case 188:

/* Line 1455 of yacc.c  */
#line 404 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 189:

/* Line 1455 of yacc.c  */
#line 405 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 190:

/* Line 1455 of yacc.c  */
#line 407 "parser.y"
    {(yyval) = gc2(pair((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])));}
    break;

  case 191:

/* Line 1455 of yacc.c  */
#line 409 "parser.y"
    {(yyval) = gc2(letrec((yyvsp[(2) - (2)]),(yyvsp[(1) - (2)])));}
    break;

  case 192:

/* Line 1455 of yacc.c  */
#line 410 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 193:

/* Line 1455 of yacc.c  */
#line 412 "parser.y"
    {(yyval) = gc1(grded(rev((yyvsp[(1) - (1)]))));}
    break;

  case 194:

/* Line 1455 of yacc.c  */
#line 413 "parser.y"
    {(yyval) = gc2(pair((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])));}
    break;

  case 195:

/* Line 1455 of yacc.c  */
#line 414 "parser.y"
    {syntaxError("case expression");}
    break;

  case 196:

/* Line 1455 of yacc.c  */
#line 416 "parser.y"
    {(yyval) = gc2(cons((yyvsp[(2) - (2)]),(yyvsp[(1) - (2)])));}
    break;

  case 197:

/* Line 1455 of yacc.c  */
#line 417 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 198:

/* Line 1455 of yacc.c  */
#line 419 "parser.y"
    {(yyval) = gc4(pair((yyvsp[(3) - (4)]),pair((yyvsp[(2) - (4)]),(yyvsp[(4) - (4)]))));}
    break;

  case 199:

/* Line 1455 of yacc.c  */
#line 424 "parser.y"
    {(yyval) = gc0(nameNil);}
    break;

  case 200:

/* Line 1455 of yacc.c  */
#line 425 "parser.y"
    {(yyval) = gc1(ap(FINLIST,cons((yyvsp[(1) - (1)]),NIL)));}
    break;

  case 201:

/* Line 1455 of yacc.c  */
#line 426 "parser.y"
    {(yyval) = gc1(ap(FINLIST,rev((yyvsp[(1) - (1)]))));}
    break;

  case 202:

/* Line 1455 of yacc.c  */
#line 427 "parser.y"
    {(yyval) = gc3(ap(COMP,pair((yyvsp[(1) - (3)]),rev((yyvsp[(3) - (3)])))));}
    break;

  case 203:

/* Line 1455 of yacc.c  */
#line 428 "parser.y"
    {(yyval) = gc3(ap(ap(varFromTo,(yyvsp[(1) - (3)])),(yyvsp[(3) - (3)])));}
    break;

  case 204:

/* Line 1455 of yacc.c  */
#line 429 "parser.y"
    {(yyval) = gc4(ap(ap(varFromThen,(yyvsp[(1) - (4)])),(yyvsp[(3) - (4)])));}
    break;

  case 205:

/* Line 1455 of yacc.c  */
#line 430 "parser.y"
    {(yyval) = gc2(ap(varFrom,(yyvsp[(1) - (2)])));}
    break;

  case 206:

/* Line 1455 of yacc.c  */
#line 431 "parser.y"
    {(yyval) = gc5(ap(ap(ap(varFromThenTo,
                                                               (yyvsp[(1) - (5)])),(yyvsp[(3) - (5)])),(yyvsp[(5) - (5)])));}
    break;

  case 207:

/* Line 1455 of yacc.c  */
#line 434 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 208:

/* Line 1455 of yacc.c  */
#line 435 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 209:

/* Line 1455 of yacc.c  */
#line 437 "parser.y"
    {(yyval) = gc3(ap(FROMQUAL,pair((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]))));}
    break;

  case 210:

/* Line 1455 of yacc.c  */
#line 438 "parser.y"
    {(yyval) = gc3(ap(QWHERE,
						     singleton(
							pair((yyvsp[(1) - (3)]),pair((yyvsp[(2) - (3)]),
								     (yyvsp[(3) - (3)]))))));}
    break;

  case 211:

/* Line 1455 of yacc.c  */
#line 442 "parser.y"
    {(yyval) = gc1(ap(BOOLQUAL,(yyvsp[(1) - (1)])));}
    break;

  case 212:

/* Line 1455 of yacc.c  */
#line 443 "parser.y"
    {(yyval) = gc4(ap(QWHERE,(yyvsp[(3) - (4)])));}
    break;

  case 213:

/* Line 1455 of yacc.c  */
#line 469 "parser.y"
    {(yyval) = gc2((yyvsp[(2) - (2)]));}
    break;

  case 214:

/* Line 1455 of yacc.c  */
#line 470 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 215:

/* Line 1455 of yacc.c  */
#line 472 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 216:

/* Line 1455 of yacc.c  */
#line 473 "parser.y"
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



/* Line 1455 of yacc.c  */
#line 3443 "y.tab.c"
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
      /* If just tried and failed to reuse lookahead token after an
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

  /* Else will try to reuse lookahead token after shifting the error
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

#if !defined(yyoverflow) || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
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



/* Line 1675 of yacc.c  */
#line 488 "parser.y"


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

