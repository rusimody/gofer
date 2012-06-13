/* --------------------------------------------------------------------------
 * input.c:     Copyright (c) Mark P Jones 1991-1994.   All rights reserved.
 *              See goferite.h for details and conditions of use etc...
 *              Gofer version 2.30 March 1994
 *
 * Input functions, lexical analysis parsing etc...
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "command.h"
#include "errors.h"
#include <ctype.h>

/* --------------------------------------------------------------------------
 * Global data:
 * ------------------------------------------------------------------------*/

List tyconDefns	     = NIL;		/* type constructor definitions	   */
List typeInDefns     = NIL;		/* type synonym restrictions 	   */
List valDefns	     = NIL;		/* value definitions in script	   */
List opDefns	     = NIL;		/* operator defns in script	   */
List classDefns      = NIL;		/* class defns in script 	   */
List instDefns       = NIL;		/* instance defns in script	   */
List overDefns	     = NIL;		/* overloaded implementation names */
List primDefns	     = NIL;		/* primitive definitions	   */

Cell inputExpr	     = NIL;		/* input expression		   */
Bool literateScripts = FALSE;		/* TRUE => default to lit scripts  */
Bool literateErrors  = TRUE;		/* TRUE => report errs in lit scrs */

String repeatStr     = 0;		/* Repeat last expr		   */

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static Void local fileInput	  Args((String,Long));
static Bool local literateMode	  Args((String));
static Void local skip		  Args((Void));
static Void local thisLineIs	  Args((Int));
static Void local newlineSkip	  Args((Void));
static Void local closeAnyInput   Args((Void));

       Int  yyparse	    Args((Void)); /* can't stop yacc making this   */
					  /* public, but don't advertise   */
					  /* it in a header file.	   */

static Void local endToken	  Args((Void));
static Text local readOperator	  Args((Void));
static Text local readIdent	  Args((Void));
static Cell local readNumber	  Args((Void));
static Cell local readChar	  Args((Void));
static Cell local readString	  Args((Void));
static Void local saveStrChr	  Args((Char));
static Cell local readAChar	  Args((Bool));

static Bool local lazyReadMatches Args((String));
static Cell local readEscapeChar  Args((Bool));
static Void local skipGap	  Args((Void));
static Cell local readCtrlChar	  Args((Void));
static Cell local readOctChar	  Args((Void));
static Int  local readOctDigit	  Args((Void));
static Cell local readHexChar	  Args((Void));
static Int  local readHexDigit	  Args((Void));
static Cell local readDecChar	  Args((Void));

static Void local goOffside	  Args((Int));
static Void local unOffside	  Args((Void));
static Bool local canUnOffside	  Args((Void));

static Void local skipWhitespace  Args((Void));
static Int  local yylex 	  Args((Void));
static Int  local repeatLast	  Args((Void));

static Void local parseInput	  Args((Int));

/* --------------------------------------------------------------------------
 * Text values for reserved words and special symbols:
 * ------------------------------------------------------------------------*/

static Text textCase,	textOfK,    textData,	textType,   textIf;
static Text textThen,	textElse,   textWhere,	textLet,    textIn;
static Text textInfix,  textInfixl, textInfixr, textPrim;

static Text textCoco,	textEq,     textUpto,	textAs,     textLambda;
static Text textBar,	textMinus,  textFrom,	textArrow,  textLazy;

static Text textClass,  textImplies,textInstance;

static Text textDefault, textDeriving, textHiding, textInterface;
static Text textImport,  textModule,   textTo,     textRenaming;

#if    IO_MONAD
static Text textRunST;
#endif

#if    DO_COMPS
static Text textDo;
#endif

static Cell varMinus;			/* (-)				   */
static Cell varNegate;			/* negate			   */
static Cell varFlip;			/* flip				   */
static Cell varFrom;			/* [_..]			   */
static Cell varFromTo;			/* [_.._]			   */
static Cell varFromThen;		/* [_,_..]			   */
static Cell varFromThenTo;		/* [_,_.._]			   */

Text   textPlus;			/* (+)				   */
Text   textMult;			/* (*)				   */

/* --------------------------------------------------------------------------
 * Single character input routines:
 *
 * At the lowest level of input, characters are read one at a time, with the
 * current character held in c0 and the following (lookahead) character in
 * c1.	The corrdinates of c0 within the file are held in (column,row).
 * The input stream is advanced by one character using the skip() function.
 * ------------------------------------------------------------------------*/

#define TABSIZE    8		       /* spacing between tabstops	   */

#define NOTHING    0		       /* what kind of input is being read?*/
#define KEYBOARD   1		       /* - keyboard/console?		   */
#define SCRIPTFILE 2		       /* - script file 		   */
#define PROJFILE   3		       /* - project file		   */

static Int    reading	= NOTHING;

static Target readSoFar;
static Int    row, column, startColumn;
static int    c0, c1;
static FILE   *inputStream;
static Bool   thisLiterate;

#if     USE_READLINE			/* for command line editors	   */
static  String currentLine;		/* editline or GNU readline	   */
static  String nextChar;
#define nextConsoleChar()   (*nextChar=='\0' ? '\n' : *nextChar++)
extern  Void add_history    Args((String));
extern  String readline	    Args((String));

#define PROMPTMAX	    20		/* max chars in a sensible prompt  */
static  String addSpace(str)		/* add trailing space to prompt	   */
String str; {
    static char promptBuf[PROMPTMAX+2];
    if (strlen(str)>PROMPTMAX)
	return str;
    strcpy(promptBuf,str);
    strcat(promptBuf," ");
    return promptBuf;
}
#else
#define nextConsoleChar()   getc(stdin)
#endif

static	Int litLines;		       /* count defn lines in lit script   */
#define DEFNCHAR  '>'		       /* definition lines begin with this */
static	Int lastLine;		       /* records type of last line read:  */
#define STARTLINE 0		       /* - at start of file, none read    */
#define BLANKLINE 1		       /* - blank (may preceed definition) */
#define TEXTLINE  2		       /* - text comment		   */
#define DEFNLINE  3		       /* - line containing definition	   */

Void consoleInput(prompt)		/* prepare to input characters from*/
String prompt; {			/* standard in (i.e. console/kbd)  */
    reading	= KEYBOARD;		/* keyboard input is Line oriented,*/
    c0		=			/* i.e. input terminated by '\n'   */
    c1		= ' ';
    column	= (-1);
    row 	= 0;

#if USE_READLINE
    if (currentLine)
	free(currentLine);
    currentLine = readline(addSpace(prompt));
    nextChar    = currentLine;
    if (currentLine) {
	if (*currentLine)
	    add_history(currentLine);
    }
    else
	c0 = c1 = EOF;
#else
    printf("%s ",prompt);
    fflush(stdout);
#endif
}

Void projInput(nm)		       /* prepare to input characters from */
String nm; {			       /* from named project file	   */
    if (inputStream = fopen(nm,"r")) {
	reading = PROJFILE;
	c0      = ' ';
        c1      = '\n';
        column  = 1;
        row     = 0;
    }
    else {
	ERROR(0) "Unable to open project file \"%s\"", nm
	EEND;
    }
}

static Void local fileInput(nm,len)	/* prepare to input characters from*/
String nm;				/* named file (specified length is */
Long   len; {				/* used to set target for reading) */
    if (inputStream = fopen(nm,"r")) {
	reading      = SCRIPTFILE;
	c0	     = ' ';
	c1	     = '\n';
	column	     = 1;
	row	     = 0;
	readSoFar    = 0;
	lastLine     = STARTLINE;
	litLines     = 0;
	thisLiterate = literateMode(nm);
	setGoal("Parsing", (Target)len);
    }
    else {
	ERROR(0) "Unable to open file"
	EEND;
    }
}

static Bool local literateMode(nm)	/* selecte literate mode for file  */
String nm; {
    String dot = 0;

#if !RISCOS
    for (; *nm; ++nm)			/* look for last dot in file name  */
	if (*nm == '.')
	    dot = nm+1;

    if (dot) {
	if (strcmp(dot,"hs")==0   ||	/* .hs, .gs, .has, .gof files are  */
	    strcmp(dot,"gs")==0   ||	/* never literate scripts	   */
	    strcmp(dot,"gof")==0  ||
	    strcmp(dot,"has")==0  ||
	    strcmp(dot,"prelude")==0)	/* special suffix for prelude files*/
	    return FALSE;

	if (strcmp(dot,"lhs")==0  ||	/* .lhs, .lgs, .verb, .lit scripts */
	    strcmp(dot,"lgs")==0  ||	/* are always literate scripts	   */
	    strcmp(dot,"verb")==0 ||
	    strcmp(dot,"lit")==0)
	    return TRUE;
    }
#else
    char *start = nm;
    for (; *nm; ++nm)                   /* look for last dot in file name  */
        if (*nm == '.')
            dot = nm+1;
    if (dot) {
	char *prev = dot-1;
	while (prev > start && *--prev != '.')
	    ;
	if (*prev == '.')
	    ++prev;
	if (namecmp(prev,"hs")      || namecmp(prev,"gs")
	     || namecmp(prev,"gof") || namecmp(prev,"has")
	     || namecmp(prev,"prelude"))
	    return FALSE;
	if (namecmp(prev,"lhs")     || namecmp(prev,"lgs")
	     || namecmp(prev,"lit") ||  namecmp(prev,"verb"))
	    return TRUE;
    }
#endif
    return literateScripts;		/* otherwise, use the default	   */
}

static Void local skip() {		/* move forward one char in input  */
    if (c0!=EOF) {			/* stream, updating c0, c1, ...	   */
	if (c0=='\n') {			/* Adjusting cursor coords as nec. */
	    row++;
	    column=1;
	    if (reading==SCRIPTFILE)
		soFar(readSoFar);
	}
	else if (c0=='\t')
	    column += TABSIZE - ((column-1)%TABSIZE);
	else
	    column++;

	c0 = c1;
	readSoFar++;

	if (c0==EOF) {
	    column = 0;
	    if (reading==SCRIPTFILE)
		done();
	    closeAnyInput();
	}
	else if (reading==KEYBOARD) {
	    if (c0=='\n')
		c1 = EOF;
	    else
		c1 = nextConsoleChar();
	}
	else
	    c1 = getc(inputStream);
    }
}

static Void local thisLineIs(kind)	/* register kind of current line   */
Int kind; {				/* & check for literate script errs*/
    if (literateErrors && ((kind==DEFNLINE && lastLine==TEXTLINE) ||
			   (kind==TEXTLINE && lastLine==DEFNLINE))) {
	ERROR(row) "Program line next to comment"
	EEND;
    }
    lastLine = kind;
}

static Void local newlineSkip() {      /* skip `\n' (supports lit scripts) */
    if (reading==SCRIPTFILE && thisLiterate) {
	do {
	    skip();
	    if (c0==DEFNCHAR) {        /* pass chars on definition lines   */
		thisLineIs(DEFNLINE);  /* to lexer (w/o leading DEFNCHAR)  */
		skip();
		litLines++;
		return;
	    }
	    while (c0==' ' || c0=='\t')/* maybe line is blank?		   */
		skip();
	    if (c0=='\n' || c0==EOF)
		thisLineIs(BLANKLINE);
	    else {
		thisLineIs(TEXTLINE);  /* otherwise it must be a comment   */
		while (c0!='\n' && c0!=EOF)
		    skip();
	    }			       /* by now, c0=='\n' or c0==EOF	   */
	} while (c0!=EOF);	       /* if new line, start again	   */

	if (litLines==0 && literateErrors) {
	    ERROR(row) "Empty script - perhaps you forgot the `%c's?",
		       DEFNCHAR
	    EEND;
	}
	return;
    }
    skip();
}

static Void local closeAnyInput() {	/* close input stream, if open	  */
    if (reading==SCRIPTFILE || reading==PROJFILE)
	fclose(inputStream);
    else if (reading==KEYBOARD)		/* or skip to end of console line  */
	while (c0!=EOF)
	    skip();
    reading=NOTHING;
}

/* --------------------------------------------------------------------------
 * Parser: Uses table driven parser generated from parser.y using yacc
 * ------------------------------------------------------------------------*/

#include "parser.c"

/* --------------------------------------------------------------------------
 * Single token input routines:
 *
 * The following routines read the values of particular kinds of token given
 * that the first character of the token has already been located in c0 on
 * entry to the routine.
 * ------------------------------------------------------------------------*/

#define MAX_TOKEN	    250
#define startToken()	    tokPos = 0
#define saveTokenChar(c)    if (tokPos<MAX_TOKEN) saveChar(c); else ++tokPos
#define saveChar(c)	    tokenStr[tokPos++]=(c)
#define SPECIALS	    "(),;[]_{}"
#define SYMBOLS 	    ":!#$%&*+./<=>?@\\^|-" /* For Haskell 1.1: `-' */
#define PRESYMBOLS 	    "~"			   /* should be a PRESYMBOL*/
                                                   /* but including it here*/
                                                   /* means we loose eg <- */
#define isoneof(c,cs)	    (strchr(cs,c)!=(char *)0)
#define overflows(n,b,d,m)  (n > ((m)-(d))/(b))

static char tokenStr[MAX_TOKEN+1];	/* token buffer			   */
static Int  tokPos;			/* input position in buffer	   */
static Int  identType;			/* identifier type: CONID / VARID  */
static Int  opType;			/* operator type  : CONOP / VAROP  */

static Void local endToken() {		/* check for token overflow	   */
    if (tokPos>MAX_TOKEN) {
	ERROR(row) "Maximum token length (%d) exceeded", MAX_TOKEN
	EEND;
    }
    tokenStr[tokPos] = '\0';
}

static Text local readOperator() {	/* read operator symbol		   */
    startToken();
    do {
	saveTokenChar(c0);
	skip();
    } while (c0!=EOF && isascii(c0) && isoneof(c0,SYMBOLS));
    opType = (tokenStr[0]==':' ? CONOP : VAROP);
    endToken();
    return findText(tokenStr);
}

static Text local readIdent() {		/* read identifier		   */
    startToken();
    do {
	saveTokenChar(c0);
	skip();
    } while ((c0!=EOF && isascii(c0) && isalnum(c0)) || c0=='_' || c0=='\'');
    endToken();
    identType = isupper(tokenStr[0]) ? CONID : VARID;
    return findText(tokenStr);
}

static Cell local readNumber() {       /* read numeric constant 	   */
    Int   n           = 0;
    Bool  intTooLarge = FALSE;

    startToken();
    do {
	if (overflows(n,10,(c0-'0'),MAXPOSINT))
	    intTooLarge = TRUE;
	n  = 10*n  + (c0-'0');
	saveTokenChar(c0);
	skip();
    } while (c0!=EOF && isascii(c0) && isdigit(c0));

    if (c0!='.' || !isascii(c1) || !isdigit(c1)) {
	endToken();
	if (!intTooLarge)
	    return mkInt(n);
#if 0
	if (intTooLarge)
	    return mkFloat(stringToFloat(tokenStr));
#endif
	ERROR(row) "Integer literal out of range"
	EEND;
    }

    saveTokenChar(c0);		        /* save decimal point		   */
    skip();
    do {				/* process fractional part ...	   */
	saveTokenChar(c0);
	skip();
    } while (c0!=EOF && isascii(c0) && isdigit(c0));

    if (c0=='e' || c0=='E') {		/* look for exponent part...	   */
	saveTokenChar('e');
	skip();
	if (c0=='-') {
	    saveTokenChar('-');
	    skip();
	}
	else if (c0=='+')
	    skip();

	if (!isascii(c0) || !isdigit(c0)) {
	    ERROR(row) "Missing digits in exponent"
	    EEND;
	}
	else {
	    do {
		saveTokenChar(c0);
		skip();
	    } while (c0!=EOF && isascii(c0) && isdigit(c0));
	}
    }

    endToken();
#if !HAS_FLOATS
    ERROR(row) "No floating point numbers in this implementation"
    EEND;
#endif

    return mkFloat(stringToFloat(tokenStr));
}

static Cell local readChar() {	       /* read character constant	   */
    Cell charRead;

    skip(/* '\'' */);
    if (c0=='\'' || c0=='\n' || c0==EOF) {
	ERROR(row) "Illegal character constant"
	EEND;
    }

    charRead = readAChar(FALSE);

    if (c0=='\'')
	skip(/* '\'' */);
    else {
	ERROR(row) "Improperly terminated character constant"
	EEND;
    }
    return charRead;
}

static Cell local readString() {       /* read string literal		   */
    Cell c;

    startToken();
    skip(/* '\"' */);
    while (c0!='\"' && c0!='\n' && c0!=EOF) {
	c = readAChar(TRUE);
	if (nonNull(c))
	    saveStrChr(charOf(c));
    }

    if (c0=='\"')
	skip(/* '\"' */);
    else {
	ERROR(row) "improperly terminated string"
	EEND;
    }
    endToken();
    return mkStr(findText(tokenStr));
}

static Void local saveStrChr(c)        /* save character in string	   */
Char c; {
    if (c!='\0' && c!='\\') {	       /* save non null char as single char*/
	saveTokenChar(c);
    }
    else {			       /* save null char as TWO null chars */
	if (tokPos+1<MAX_TOKEN) {
	    saveChar('\\');
	    if (c=='\\')
		saveChar('\\');
	    else
		saveChar('0');
	}
    }
}

static Cell local readAChar(allowEmpty)/* read single char constant	   */
Bool allowEmpty; {		       /* TRUE => enable use of \& and gaps*/
    Cell c = mkChar(c0);

    if (c0=='\\')		       /* escape character?		   */
	return readEscapeChar(allowEmpty);
    if (!isprint(c0)) {
	ERROR(row) "Non printable character '\\%d' in constant", ((int)c0)
	EEND;
    }
    skip();			       /* normal character?		   */
    return c;
}

/* --------------------------------------------------------------------------
 * Character escape code sequences:
 * ------------------------------------------------------------------------*/

static struct { 		       /* table of special escape codes    */
    char *codename;
    int  codenumber;
} escapes[] = {
   {"a",    7}, {"b",	 8}, {"f",   12}, {"n",   10},	/* common escapes  */
   {"r",   13}, {"t",	 9}, {"\\",'\\'}, {"\"",'\"'},
   {"\'",'\''}, {"v",	11},
   {"NUL",  0}, {"SOH",  1}, {"STX",  2}, {"ETX",  3},	/* ascii codenames */
   {"EOT",  4}, {"ENQ",  5}, {"ACK",  6}, {"BEL",  7},
   {"BS",   8}, {"HT",	 9}, {"LF",  10}, {"VT",  11},
   {"FF",  12}, {"CR",	13}, {"SO",  14}, {"SI",  15},
   {"DLE", 16}, {"DC1", 17}, {"DC2", 18}, {"DC3", 19},
   {"DC4", 20}, {"NAK", 21}, {"SYN", 22}, {"ETB", 23},
   {"CAN", 24}, {"EM",	25}, {"SUB", 26}, {"ESC", 27},
   {"FS",  28}, {"GS",	29}, {"RS",  30}, {"US",  31},
   {"SP",  32}, {"DEL", 127},
   {0,0}
};

static Int  alreadyMatched;	       /* Record portion of input stream   */
static char alreadyRead[10];	       /* that has been read w/o a match   */

static Bool local lazyReadMatches(s)   /* compare input stream with string */
String s; {			       /* possibly using characters that   */
    int i;			       /* have already been read	   */

    for (i=0; i<alreadyMatched; ++i)
	if (alreadyRead[i]!=s[i])
	    return FALSE;

    while (s[i] && s[i]==c0) {
	alreadyRead[alreadyMatched++]=c0;
	skip();
	i++;
    }

    return s[i]=='\0';
}

static Cell local readEscapeChar(allowEmpty) /* read escape character	   */
Bool allowEmpty; {
    int i=0;

    skip(/* '\\' */);
    switch (c0) {
	case '&'  : if (allowEmpty) {
			skip();
			return NIL;
		    }
		    ERROR(row) "Illegal use of \\& in character constant"
		    EEND;
		    break;/*NOTREACHED*/
	case ' '  :
	case '\n' :
	case '\t' : if (allowEmpty) {
			skipGap();
			return NIL;
		    }
		    ERROR(row) "Illegal use of gap in character constant"
		    EEND;
		    break;
	case '^'  : return readCtrlChar();
	case 'o'  : return readOctChar();
	case 'x'  : return readHexChar();
	default   : if (isdigit(c0))
			return readDecChar();
    }

    for (alreadyMatched=0; escapes[i].codename; i++)
	if (lazyReadMatches(escapes[i].codename))
	    return mkChar(escapes[i].codenumber);

    alreadyRead[alreadyMatched++] = c0;
    alreadyRead[alreadyMatched++] = '\0';
    ERROR(row) "Illegal character escape sequence \"\\%s\"",
	       alreadyRead
    EEND;
    return NIL;/*NOTREACHED*/
}

static Void local skipGap() {		/* skip over gap in string literal */
    do					/* (simplified in Haskell 1.1)	   */
	if (c0=='\n')
	    newlineSkip();
	else
	    skip();
    while (c0==' ' || c0=='\t' || c0=='\n');
    if (c0!='\\') {
	ERROR(row) "Missing `\\' terminating string literal gap"
	EEND;
    }
    skip(/* '\\' */);
}

static Cell local readCtrlChar() {     /* read escape sequence \^x	   */
    static String controls = "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_";
    String which;

    skip(/* '^' */);
    if ((which = strchr(controls,c0))==NULL) {
	ERROR(row) "Unrecognised escape sequence `\\^%c'", c0
	EEND;
    }
    skip();
    return mkChar(which-controls);
}

static Cell local readOctChar() {      /* read octal character constant    */
    Int n = 0;
    Int d;

    skip(/* 'o' */);
    if ((d = readOctDigit())<0) {
	ERROR(row) "Empty octal character escape"
	EEND;
    }
    do {
	if (overflows(n,8,d,MAXCHARVAL)) {
	    ERROR(row) "Octal character escape out of range"
	    EEND;
	}
	n = 8*n + d;
	skip();
    } while ((d = readOctDigit())>=0);

    return mkChar(n);
}

static Int local readOctDigit() {      /* read single octal digit	   */
    if ('0'<=c0 && c0<='7')
	return c0-'0';
    return -1;
}

static Cell local readHexChar() {      /* read hex character constant	   */
    Int n = 0;
    Int d;

    skip(/* 'x' */);
    if ((d = readHexDigit())<0) {
	ERROR(row) "Empty hexadecimal character escape"
	EEND;
    }
    do {
	if (overflows(n,16,d,MAXCHARVAL)) {
	    ERROR(row) "Hexadecimal character escape out of range"
	    EEND;
	}
	n = 16*n + d;
	skip();
    } while ((d = readHexDigit())>=0);

    return mkChar(n);
}

static Int local readHexDigit() {      /* read single hex digit 	   */
    if ('0'<=c0 && c0<='9')
	return c0-'0';
    if ('A'<=c0 && c0<='F')
	return 10 + (c0-'A');
    if ('a'<=c0 && c0<='f')
	return 10 + (c0-'a');
    return -1;
}

static Cell local readDecChar() {      /* read decimal character constant  */
    Int n = 0;

    do {
	if (overflows(n,10,(c0-'0'),MAXCHARVAL)) {
	    ERROR(row) "Decimal character escape out of range"
	    EEND;
	}
	n = 10*n + (c0-'0');
	skip();
    } while (c0!=EOF && isascii(c0) && isdigit(c0));

    return mkChar(n);
}

/* --------------------------------------------------------------------------
 * Produce printable representation of character:
 * ------------------------------------------------------------------------*/

String unlexChar(c,quote)		/* return string representation of */
Char c;					/* character...			   */
Char quote; {				/* protect quote character	   */
    static char buffer[12];

    if (c<0)				/* deal with sign extended chars.. */
	c += NUM_CHARS;

    if (isascii(c) && isprint(c)) {	/* normal printable character	   */
	if (c==quote) {			/* look for quote of approp. kind  */
	    buffer[0] = '\\';		
	    buffer[1] = c;
	    buffer[2] = '\0';
	}
	else {
            buffer[0] = c;
            buffer[1] = '\0';
	}
    }
    else {				/* look for escape code		   */
        Int escs;
        for (escs=0; escapes[escs].codename; escs++)
	    if (escapes[escs].codenumber==c) {
		sprintf(buffer,"\\%s",escapes[escs].codename);
		return buffer;
	    }
        sprintf(buffer,"\\%d",c);	/* otherwise use numeric escape	   */
    }
    return buffer;
}

/* --------------------------------------------------------------------------
 * Handle special types of input for use in interpreter:
 * ------------------------------------------------------------------------*/

Command readCommand(cmds,start,sys)	/* read command at start of input  */
struct cmd *cmds;			/* line in interpreter		   */
Char   start;				/* characters introducing a cmd    */
Char   sys; {				/* character for shell escape	   */
    while (c0==' ' || c0 =='\t')
	skip();

    if (c0=='\n')			/* look for blank command lines    */
	return NOCMD;
    if (c0==EOF)			/* look for end of input stream	   */
	return QUIT;
    if (c0==sys) {			/* single character system escape  */
	skip();
	return SYSTEM;
    }
    if (c0==start && c1==sys) {		/* two character system escape	   */
	skip();
	skip();
	return SYSTEM;
    }

    startToken();			/* All cmds start with start	   */
    if (c0==start)			/* except default (usually EVAL)   */
	do {				/* which is empty		   */
	    saveTokenChar(c0);
	    skip();
	} while (c0!=' ' && c0!='\t' && c0!='\n' && c0!=EOF);
    endToken();

    for (; cmds->cmdString; ++cmds)
	if (strcmp((cmds->cmdString),tokenStr)==0 ||
            (tokenStr[0]==start &&
             tokenStr[1]==(cmds->cmdString)[1] &&
             tokenStr[2]=='\0'))
	    return (cmds->cmdCode);
    return BADCMD;
}

String readFilename() { 	       /* Read filename from input (if any)*/
    if (reading==PROJFILE)
	skipWhitespace();
    else
	while (c0==' ' || c0=='\t')
	    skip();

    if (c0=='\n' || c0==EOF)	       /* return null string at end of line*/
	return 0;

    startToken();
    while (c0!=' ' && c0!='\t' && c0!='\n' && c0!='\r' && c0!=EOF) {
	saveTokenChar(c0);
	skip();
    }
    endToken();

    return tokenStr;
}

String readLine() {			/* Read command line from input	   */
    while (c0==' ' || c0=='\t')		/* skip leading whitespace	   */
	skip();

    startToken();
    while (c0!='\n' && c0!=EOF) {
	saveTokenChar(c0);
	skip();
    }
    endToken();

    return tokenStr;
}

/* --------------------------------------------------------------------------
 * This lexer supports the Haskell layout rule:
 *
 * - Layout area bounded by { ... }, with `;'s in between.
 * - A `{' is a HARD indentation and can only be matched by a corresponding
 *   HARD '}'
 * - Otherwise, if no `{' follows the keywords WHERE/LET or OF, a SOFT `{'
 *   is inserted with the column number of the first token after the
 *   WHERE/LET/OF keyword.
 * - When a soft indentation is uppermost on the indetation stack with
 *   column col' we insert:
 *    `}'  in front of token with column<col' and pop indentation off stack,
 *    `;'  in front of token with column==col'.
 * ------------------------------------------------------------------------*/

#define MAXINDENT  100		       /* maximum nesting of layout rule   */
static	Int	   layout[MAXINDENT+1];/* indentation stack		   */
#define HARD	   (-1) 	       /* indicates hard indentation	   */
static	Int	   indentDepth = (-1); /* current indentation nesting	   */

static Void local goOffside(col)       /* insert offside marker 	   */
Int col; {			       /* for specified column		   */
    if (indentDepth>=MAXINDENT) {
	ERROR(row) "Too many levels of program nesting"
	EEND;
    }
    layout[++indentDepth] = col;
}

static Void local unOffside() {        /* leave layout rule area	   */
    indentDepth--;
}

static Bool local canUnOffside() {     /* Decide if unoffside permitted    */
    return indentDepth>=0 && layout[indentDepth]!=HARD;
}

/* --------------------------------------------------------------------------
 * Main tokeniser:
 * ------------------------------------------------------------------------*/

static Void local skipWhitespace() {   /* skip over whitespace/comments    */

ws: while (c0==' ' || c0=='\t' || c0=='\r' || c0=='\f')
	skip();

    if (c0=='\n') {		       /* skip newline characters	   */
	newlineSkip();
	goto ws;
    }

    if (c0=='{' && c1=='-') {	       /* (potentially) nested comment	   */
	Int nesting = 1;
	Int origRow = row;	       /* save original row number	   */

        skip();
        skip();
	while (nesting>0 && c0!=EOF) {
	    if (c0=='{' && c1=='-') {
		skip();
		nesting++;
	    }
	    else if (c0=='-' && c1=='}') {
		skip();
		nesting--;
	    }

	    if (c0=='\n')
		newlineSkip();
	    else
		skip();
	}
	if (nesting>0) {
	    ERROR(origRow) "Unterminated nested comment {- ..."
	    EEND;
	}
	goto ws;
    }

    if (c0=='-' && c1=='-') {	       /* one line comment		   */
	do
	    skip();
	while (c0!='\n' && c0!=EOF)
	    ;
	if (c0=='\n')
	    newlineSkip();
	goto ws;
    }
}

static Bool firstToken; 	       /* Set to TRUE for first token	   */
static Int  firstTokenIs;	       /* ... with token value stored here */

static Int local yylex() {	       /* Read next input token ...	   */
    static Bool insertOpen    = FALSE;
    static Bool insertedToken = FALSE;
    static Text textRepeat;

#define lookAhead(t) {skipWhitespace(); insertOpen = (c0!='{'); return t;}

    if (firstToken) {		       /* Special case for first token	   */
	indentDepth   = (-1);
	firstToken    = FALSE;
	insertOpen    = FALSE;
	insertedToken = FALSE;
	if (reading==KEYBOARD)
	    textRepeat = findText(repeatStr);
	return firstTokenIs;
    }

    if (insertOpen) {		       /* insert `soft' opening brace	   */
	insertOpen    = FALSE;
	insertedToken = TRUE;
	goOffside(column);
	push(yylval = mkInt(row));
	return '{';
    }

    /* ----------------------------------------------------------------------
     * Skip white space, and insert tokens to support layout rules as reqd.
     * --------------------------------------------------------------------*/

    skipWhitespace();
    startColumn = column;
    push(yylval = mkInt(row));	       /* default token value is line no.  */
    /* subsequent changes to yylval must also set top() to the same value  */

    if (indentDepth>=0) 	       /* layout rule(s) active ?	   */
        if (insertedToken)	       /* avoid inserting multiple `;'s    */
	    insertedToken = FALSE;     /* or putting `;' after `{'	   */
        else if (layout[indentDepth]!=HARD)
	    if (column<layout[indentDepth]) {
		unOffside();
		return '}';
            }
            else if (column==layout[indentDepth] && c0!=EOF) {
                insertedToken = TRUE;
                return ';';
            }

    /* ----------------------------------------------------------------------
     * Now try to identify token type as one of:
     * - end of file character
     * - Special character:  ( ) , ; [ ] _ { } `
     * - Character constant
     * - String Constant
     * - Integer literal
     * - Alphanum: reservedid, VARID or CONID
     * - operator: reservedop, VAROP or CONOP
     * --------------------------------------------------------------------*/

    switch (c0) {
	case EOF  : return 0;

	case '('  : skip();
		    return '(';

	case ')'  : skip();
		    return ')';

	case ','  : skip();
		    return ',';

	case ';'  : skip();
		    return ';';

	case '['  : skip();
		    return '[';

	case ']'  : skip();
		    return ']';

	case '_'  : skip();
		    return '_';

	case '{'  : goOffside(HARD);
		    skip();
		    return '{';

	case '}'  : if (indentDepth<0) {
			ERROR(row) "Misplaced `}'"
			EEND;
		    }
		    if (layout[indentDepth]==HARD)	/* skip over hard } */
			skip();
		    unOffside();	/* otherwise, we have to insert a } */
		    return '}';		/* to (try to) avoid an error...    */

	case '\'' : top() = yylval = readChar();
		    return CHARLIT;

	case '\"' : top() = yylval = readString();
		    return STRINGLIT;

	case '`'  : skip();
		    return '`';
    }

    if (!(isascii(c0) && isprint(c0))) {
	ERROR(row) "Unrecognised character '\\%d' in column %d",
		   ((int)c0), column
	EEND;
    }

    if (isalpha(c0)) {
	Text it = readIdent();

	if (it==textCase)              return CASEXP;
	if (it==textOfK)               lookAhead(OF);
	if (it==textData)	       return DATA;
	if (it==textType)	       return TYPE;
	if (it==textIf) 	       return IF;
	if (it==textThen)	       return THEN;
	if (it==textElse)	       return ELSE;
	if (it==textWhere)             lookAhead(WHERE);
        if (it==textLet)               lookAhead(LET);
        if (it==textIn)                return IN;
	if (it==textInfix)	       return INFIX;
	if (it==textInfixl)	       return INFIXL;
	if (it==textInfixr)	       return INFIXR;
	if (it==textPrim)              return PRIMITIVE;
	if (it==textClass)	       return TCLASS;
	if (it==textInstance)	       return TINSTANCE;
	if (it==textDeriving)	       return DERIVING;
	if (it==textDefault)	       return DEFAULT;
	if (it==textHiding)	       return HIDING;
	if (it==textImport)	       return IMPORT;
	if (it==textInterface)	       return INTERFACE;
	if (it==textModule)	       return MODULE;
	if (it==textTo)		       return TO;
	if (it==textRenaming)	       return RENAMING;
	if (it==textRepeat && reading==KEYBOARD)
	    return repeatLast();
#if IO_MONAD
	if (it==textRunST)	       return TRUNST;
#endif
#if DO_COMPS
	if (it==textDo)		       lookAhead(DO);
#endif

	top() = yylval = ap((identType==CONID ? CONIDCELL : VARIDCELL),it);
	return identType;
    }

    if (isoneof(c0,SYMBOLS) || isoneof(c0,PRESYMBOLS)) {
	Text it = readOperator();

	if (it==textCoco)    return COCO;
	if (it==textEq)      return '=';
	if (it==textUpto)    return UPTO;
	if (it==textAs)      return '@';
	if (it==textLambda)  return '\\';
	if (it==textBar)     return '|';
	if (it==textFrom)    return FROM;/*relies on notElem '-' PRESYMBOLS*/
	if (it==textMinus)   return '-';
	if (it==textArrow)   return FUNARROW;
	if (it==textLazy)    return '~';
	if (it==textImplies) return IMPLIES;
	if (it==textRepeat && reading==KEYBOARD)
	    return repeatLast();

	top() = yylval = ap((opType==CONOP ? CONOPCELL : VAROPCELL),it);
	return opType;
    }

    if (isdigit(c0)) {
	top() = yylval = readNumber();
	return NUMLIT;
    }

    ERROR(row) "Unrecognised character '\\%d' in column %d", ((int)c0), column
    EEND;
    return 0; /*NOTREACHED*/
}

static Int local repeatLast() {		/* obtain last expression entered  */
    if (isNull(yylval=getLastExpr())) {
	ERROR(row) "Cannot use %s without any previous input", repeatStr
	EEND;
    }
    return REPEAT;
}

Syntax defaultSyntax(t) 	       /* find default syntax of var named */
Text t; {			       /* by t ...			   */
    String s = textToStr(t);
    if ((isascii(s[0]) && isalpha(s[0])) || s[0]=='_' || strcmp(s,"[]")==0)
	return APPLIC;
    else
	return DEF_OPSYNTAX;
}

/* --------------------------------------------------------------------------
 * main entry points to parser/lexer:
 * ------------------------------------------------------------------------*/

static Void local parseInput(startWith)/* parse input with given first tok,*/
Int startWith; {		       /* determining whether to read a    */
    firstToken	 = TRUE;	       /* script or an expression	   */
    firstTokenIs = startWith;

    clearStack();
    if (yyparse()) {		       /* This can only be parser overflow */
	ERROR(row) "Parser overflow"   /* as all syntax errors are caught  */
	EEND;			       /* in the parser...		   */
    }
    drop();
    if (!stackEmpty())		       /* stack should now be empty	   */
	internal("parseInput");
}

Void parseScript(nm,len)	       /* Read a script: sets valDefns and */
String nm;			       /*                tyconDefns	   */
Long   len; {			       /* used to set a target for reading)*/
    input(RESET);
    fileInput(nm,len);
    parseInput(SCRIPT);
}

Void parseExp() {		       /* read an expression to evaluate   */
    parseInput(EVALEX);
    setLastExpr(inputExpr);
}

/* --------------------------------------------------------------------------
 * Input control:
 * ------------------------------------------------------------------------*/

Void input(what)
Int what; {
    switch (what) {
	case RESET   : tyconDefns  = NIL;
		       typeInDefns = NIL;
		       valDefns    = NIL;
		       opDefns	   = NIL;
		       classDefns  = NIL;
                       instDefns   = NIL;
		       overDefns   = NIL;
		       primDefns   = NIL;
		       inputExpr   = NIL;
		       closeAnyInput();
		       break;

	case BREAK   : if (reading==KEYBOARD)
			   c0 = EOF;
		       break;

	case MARK    : mark(tyconDefns);
		       mark(typeInDefns);
		       mark(valDefns);
		       mark(opDefns);
		       mark(classDefns);
                       mark(instDefns);
		       mark(overDefns);
		       mark(primDefns);
		       mark(inputExpr);
		       mark(varNegate);
		       mark(varFlip);
		       mark(varMinus);
		       mark(varFrom);
		       mark(varFromTo);
		       mark(varFromThen);
		       mark(varFromThenTo);
		       break;

	case INSTALL : input(RESET);
		       textCase       = findText("case");
		       textOfK	      = findText("of");
		       textData       = findText("data");
		       textType       = findText("type");
		       textIf	      = findText("if");
		       textThen       = findText("then");
		       textElse       = findText("else");
		       textWhere      = findText("where");
                       textLet        = findText("let");
                       textIn         = findText("in");
		       textInfix      = findText("infix");
		       textInfixl     = findText("infixl");
		       textInfixr     = findText("infixr");
		       textPrim       = findText("primitive");
		       textCoco       = findText("::");
		       textEq	      = findText("=");
		       textUpto       = findText("..");
		       textAs	      = findText("@");
		       textLambda     = findText("\\");
		       textBar	      = findText("|");
		       textMinus      = findText("-");
		       textFrom       = findText("<-");
		       textArrow      = findText("->");
		       textLazy       = findText("~");
		       textClass      = findText("class");
		       textInstance   = findText("instance");
		       textImplies    = findText("=>");
		       textPlus	      = findText("+");
		       textMult	      = findText("*");
		       textDefault    = findText("default");
		       textDeriving   = findText("deriving");
		       textHiding     = findText("hiding");
		       textInterface  = findText("interface");
		       textImport     = findText("import");
		       textModule     = findText("module");
		       textTo         = findText("to");
		       textRenaming   = findText("renaming");
#if IO_MONAD
		       textRunST      = findText("runST");
#endif
#if DO_COMPS
		       textDo	      = findText("do");
#endif
		       varMinus	      = mkVar(findText("-"));
		       varNegate      = mkVar(findText("negate"));
		       varFlip	      = mkVar(findText("flip"));
		       varFrom	      = mkVar(findText("enumFrom"));
		       varFromTo      = mkVar(findText("enumFromTo"));
		       varFromThen    = mkVar(findText("enumFromThen"));
		       varFromThenTo  = mkVar(findText("enumFromThenTo"));
		       break;
    }
}

/*-------------------------------------------------------------------------*/
