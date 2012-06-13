/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

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




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;

