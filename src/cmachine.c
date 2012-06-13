/* --------------------------------------------------------------------------
 * cmachine.c:  Copyright (c) Mark P Jones 1991-1994.   All rights reserved.
 *              See goferite.h for details and conditions of use etc...
 *		Gofer Compiler version 1.01 February 1992
 *              Incorporated into mainstream Gofer 2.25, October 1992.
 *              Gofer version 2.30 March 1994
 *
 * Compilation to simple G-code & (slightly) optimised translation to C code
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include <setjmp.h>
#include <ctype.h>

/* #define GOFC_INCLUDE  "\"gofc.h\"" */

#ifndef GOFC_INCLUDE
#if     (TURBOC | BCC | DJGPP | WATCOM)
#define GOFC_INCLUDE  "\"/gofer/gofc/gofc.h\""
#else
#if     RISCOS
#define GOFC_INCLUDE "\"Lib:h.gofc\""
#else
#define GOFC_INCLUDE  "\"/usr/local/lib/Gofer/gofc.h\""
#endif
#endif
#endif

/*#define DEBUG_CODE*/

Bool   andorOptimise = TRUE;		/* TRUE => optimise uses of &&, || */

/* --------------------------------------------------------------------------
 * Data structures for machine memory (program storage):
 * ------------------------------------------------------------------------*/

typedef enum {
    iLOAD,   iCELL,   iCHAR,   iINT,   iFLOAT,
    iSTRING, iMKAP,   iUPDATE, iUPDAP, iEVAL,
    iRETURN, iINTEQ,  iTEST,   iGOTO,  iSETSTK,
    iALLOC,  iSLIDE,  iROOT,   iDICT,  iFLUSH,
    iLABEL,  iSTKIS,  iEND,
#if NPLUSK
    iINTGE,  iINTDV,
#endif
    iEXTERN
} Instr;

typedef Int Label;

typedef union {
    Int   intVal;
#if !BREAK_FLOATS
    Float floatVal;
#endif
    Cell  cellVal;
    Text  textVal;
    Instr instrVal;
    Label labVal;
} MemCell;

typedef MemCell far *Memory;
static	Memory	    memory;
#define intAt(m)    memory[m].intVal
#if !BREAK_FLOATS
#define floatAt(m)  memory[m].floatVal
#endif
#define cellAt(m)   memory[m].cellVal
#define textAt(m)   memory[m].textVal
#define instrAt(m)  memory[m].instrVal
#define labAt(m)    memory[m].labVal

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static Void   local instrNone	 Args((Instr));
static Void   local instrInt	 Args((Instr,Int));
static Void   local instrFloat   Args((Instr,FloatPro));
static Void   local instrCell	 Args((Instr,Cell));
static Void   local instrText	 Args((Instr,Text));
static Void   local instrLab	 Args((Instr,Label));
static Void   local instrIntLab	 Args((Instr,Int,Label));
static Void   local instrCellLab Args((Instr,Cell,Label));

static Void   local asSTART	 Args((Void));
static Label  local newLabel	 Args((Void));
static Void   local asLABEL	 Args((Label));
static Void   local asEND	 Args((Void));

static Void   local asMKAP	 Args((Int));
static Void   local asUPDATE	 Args((Int));

#ifdef DEBUG_CODE
static Void   local dissassemble Args((Addr));
static Void   local printCell	 Args((Cell));
static Addr   local dissNone	 Args((Addr,String));
static Addr   local dissInt	 Args((Addr,String));
static Addr   local dissFloat    Args((Addr,String));
static Addr   local dissCell	 Args((Addr,String));
static Addr   local dissText	 Args((Addr,String));
static Addr   local dissLab	 Args((Addr,String));
static Addr   local dissIntLab	 Args((Addr,String));
static Addr   local dissCellLab	 Args((Addr,String));
#endif

static Void   local doCont	 Args((Pair));
static Pair   local flush	 Args((Pair));
static Void   local make	 Args((Cell,Int,Label,Pair));
static Void   local makeCond	 Args((Cell,Cell,Cell,Int,Label,Pair));
static Void   local makeCase	 Args((Cell,Int,Label,Pair));
static Void   local testCase	 Args((Pair,Int,Label,Label,Pair));
static Void   local makeGded	 Args((List,Int,Label,Pair));
static Bool   local testGuard	 Args((Pair,Int,Label,Label,Pair));

static Void   local dependsOn	 Args((Cell));
static Void   local build	 Args((Cell,Int));
static Void   local buildGuards	 Args((List,Int));
static Int    local buildLoc	 Args((List,Int));

static Void   local analyseAp	 Args((Cell));
static Void   local buildAp	 Args((Cell,Int,Label,Bool));

static List   local identifyDeps Args((Name));
static Void   local checkPrimDep Args((Name,Name));
static Void   local outputCDecls Args((FILE *,List));
static Void   local outputCDicts Args((FILE *));

static Void   local rspRecalc	 Args((Void));

static Void   local outputCSc	 Args((FILE *,Name));
static List   local cCode	 Args((Int,Addr));
static List   local heapUse	 Args((List));
static List   local heapAnalyse	 Args((List));
static Void   local outputCinst	 Args((FILE *,Cell));

static Void   local expr	 Args((FILE *,Cell));
static Void   local outputLabel  Args((FILE *,Int));
static Void   local outputJump	 Args((FILE *,Int));
static Void   local outputCStr	 Args((FILE *, String));
static Bool   local validCIdent  Args((String));
static String local scNameOf	 Args((Name));

static Void   local startTable	 Args((String,String,String));
static Void   local tableItem	 Args((FILE *,String));
static Void   local finishTable	 Args((FILE *));

static Int    local externArity	 Args((Type));
static Type   local transExtType Args((Type));
static String local showExtType  Args((Type));
static String local showExtRes   Args((Type));
static String local showExtRet   Args((Type));
static Void   local externBody	 Args((FILE *,String,Type));

/* --------------------------------------------------------------------------
 * Assembler: (Low level, instruction code storage)
 * ------------------------------------------------------------------------*/

static Addr  startInstr;		/* first instruction after START   */
static Addr  lastInstr;			/* last instr written (for peephole*/
					/* optimisations etc.)		   */
static Int   srsp;			/* simulated runtime stack pointer */
static Int   offsPosn[NUM_OFFSETS];	/* mapping from logical to physical*/
					/* offset positions		   */

static Void local instrNone(opc)	/* Opcode with no operands	   */
Instr opc; {
    lastInstr	       = getMem(1);
    instrAt(lastInstr) = opc;
}

static Void local instrInt(opc,n)	/* Opcode with integer operand	   */
Instr opc;
Int   n; {
    lastInstr	       = getMem(2);
    instrAt(lastInstr) = opc;
    intAt(lastInstr+1) = n;
}

static Void local instrFloat(opc,fl)	/* Opcode with Float operand	   */
Instr opc;
FloatPro fl; {
#if BREAK_FLOATS
    lastInstr		 = getMem(3);
    instrAt(lastInstr)	 = opc;
    cellAt(lastInstr+1)	 = part1Float(fl);
    cellAt(lastInstr+2)	 = part2Float(fl);
#else
    lastInstr            = getMem(2);
    instrAt(lastInstr)   = opc;
    floatAt(lastInstr+1) = fl;
#endif
}

static Void local instrCell(opc,c)	/* Opcode with Cell operand	   */
Instr opc;
Cell  c; {
    lastInstr		= getMem(2);
    instrAt(lastInstr)	= opc;
    cellAt(lastInstr+1) = c;
}

static Void local instrText(opc,t)	/* Opcode with Text operand	   */
Instr opc;
Text  t; {
    lastInstr		= getMem(2);
    instrAt(lastInstr)	= opc;
    textAt(lastInstr+1) = t;
}

static Void local instrLab(opc,l)	/* Opcode with label operand	   */
Instr opc;
Label l; {
    lastInstr	       = getMem(2);
    instrAt(lastInstr) = opc;
    labAt(lastInstr+1) = l;
    if (l<0)
	internal("bad Label");
}

static Void local instrIntLab(opc,n,l)	/* Opcode with int, label operands */
Instr opc;
Int   n;
Label l; {
    lastInstr	       = getMem(3);
    instrAt(lastInstr) = opc;
    intAt(lastInstr+1) = n;
    labAt(lastInstr+2) = l;
    if (l<0)
	internal("bad Label");
}

static Void local instrCellLab(opc,c,l)	/* Opcode with cell, label operands*/
Instr opc;
Cell  c;
Label l; {
    lastInstr		= getMem(3);
    instrAt(lastInstr)	= opc;
    cellAt(lastInstr+1) = c;
    labAt(lastInstr+2)	= l;
    if (l<0)
	internal("bad Label");
}

/* --------------------------------------------------------------------------
 * Main low level assembler control: (includes label assignment and fixup)
 * ------------------------------------------------------------------------*/

static	Label	    nextLab;		/* next label number to allocate   */
static  Label       fixups[NUM_FIXUPS]; /* fixups for label values	   */
#define FAIL	    0			/* special label for fail()	   */

#define fix(a)      labAt(a) = fixups[labAt(a)]

static Void local asSTART() {		/* initialise assembler		   */
    fixups[0]	= FAIL;			/* use label 0 for fail()	   */
    nextLab	= 1;
    startInstr	= getMem(0);
    lastInstr	= startInstr-1;
    srsp	= 0;
    offsPosn[0]	= 0;
}

static Label local newLabel() {		/* allocate new label		   */
    if (nextLab>=NUM_FIXUPS) {
	ERROR(0) "Compiled code too complex"
	EEND;
    }
    return nextLab++;
}

static Void local asLABEL(l)		/* indicate label reached	   */
Label l; {
    if (instrAt(lastInstr)==iGOTO && labAt(lastInstr+1)==l) {
	instrAt(lastInstr) = iLABEL;	/* GOTO l; LABEL l  ==>  LABEL l   */
	fixups[l] = l;
    }
    else if (instrAt(lastInstr)==iLABEL)/* code already labelled at this pt*/
	fixups[l] = labAt(lastInstr+1);	/* so use previous label	   */
    else {
	instrLab(iLABEL,l);		/* otherwise insert new label	   */
	fixups[l] = l;
    }
}

static Void local asEND() {		/* Fix addresses in assembled code */
    Addr pc = startInstr;

    instrNone(iEND);			/* insert END opcode		   */
    for (;;)
	switch (instrAt(pc)) {
	    case iEND	 : return;	/* end of code sequence		   */

	    case iEVAL	 :		/* opcodes taking no arguments	   */
	    case iFLUSH  :
	    case iRETURN : pc++;
			   break;

	    case iGOTO	 : fix(pc+1);	/* opcodes taking one argument	   */
	    case iLABEL	 : /* no need for a fix here !*/
	    case iSETSTK :
	    case iSTKIS  :
	    case iALLOC  :
	    case iSLIDE  :
	    case iROOT	 :
            case iDICT   :
	    case iLOAD	 :
	    case iCELL	 :
	    case iCHAR	 :
	    case iINT	 :
#if !BREAK_FLOATS
	    case iFLOAT  :
#endif
	    case iSTRING :
	    case iMKAP	 :
	    case iUPDATE :
	    case iUPDAP  : pc+=2;
			   break;
#if BREAK_FLOATS
	    case iFLOAT  : pc+=3;
			   break;
#endif

	    case iINTEQ  :		/* opcodes taking two arguments	   */
#if NPLUSK
	    case iINTGE	 :
	    case iINTDV  :
#endif
	    case iTEST	 : fix(pc+2);
			   pc+=3;
			   break;

	    default	 : internal("asEND");
	}
}

/* --------------------------------------------------------------------------
 * Assembler Opcodes: (includes simple peephole optimisations)
 * ------------------------------------------------------------------------*/

#define asINTEGER(n) instrInt(iINT,n);		srsp++
#define asFLOAT(fl)  instrFloat(iFLOAT,fl);	srsp++
#define asCHAR(n)    instrInt(iCHAR,n);		srsp++
#define asLOAD(n)    instrInt(iLOAD,n);		srsp++
#define asALLOC(n)   instrInt(iALLOC,n);	srsp+=n
#define asROOT(n)    instrInt(iROOT,n);		srsp++
#define asSETSTK(n)  instrInt(iSETSTK,n);	srsp=n
#define asSTKIS(n)   instrInt(iSTKIS,n);	srsp=n
#define asEVAL()     instrNone(iEVAL);		srsp--  /* inaccurate srsp */
#define asFLUSH()    instrNone(iFLUSH)
#define asRETURN()   instrNone(iRETURN)
#define asCELL(c)    instrCell(iCELL,c);	srsp++
#define asTEST(c,l)  instrCellLab(iTEST,c,l)		/* inaccurate srsp */
#define asINTEQ(n,l) instrIntLab(iINTEQ,n,l)
#if NPLUSK
#define asINTGE(n,l) instrIntLab(iINTGE,n,l)		/* inaccurate srsp */
#define asINTDV(n,l) instrIntLab(iINTDV,n,l)		/* inaccurate srsp */
#endif
#define asGOTO(l)    instrLab(iGOTO,l)
#define asSLIDE(n)   instrInt(iSLIDE,n);	srsp-=n
#define asDICT(n)    if (n>0) instrInt(iDICT,n)
#define asEXTERN(t)  instrText(iEXTERN,t)
#define asSTRING(t)  if (*textToStr(t))			\
			 instrText(iSTRING,t);		\
		     else				\
			 instrCell(iCELL,nameNil);	\
		     srsp++

static Void local asMKAP(n)		/* Make application nodes ...	   */
Int n; {
    if (instrAt(lastInstr)==iMKAP)	/* Peephole optimisation:	   */
	intAt(lastInstr+1)+=n;		/* MKAP n; MKAP m  ===> MKAP (n+m) */
    else
	instrInt(iMKAP,n);
    srsp -= n;
}

static Void local asUPDATE(n)		/* Update node ...		   */
Int n; {
    if (instrAt(lastInstr)==iMKAP) {	/* Peephole optimisations:	   */
	if (intAt(lastInstr+1)>1) {	/* MKAP (n+1); UPDATE p		   */
	    intAt(lastInstr+1)--;	/*	      ===> MKAP n; UPDAP p */
	    instrInt(iUPDAP,n);
	}
	else {
	    instrAt(lastInstr) = iUPDAP;
	    intAt(lastInstr+1) = n;	/* MKAP 1; UPDATE p ===> UPDAP p   */
	}
    }
    else
	instrInt(iUPDATE,n);
    srsp--;
}

/* --------------------------------------------------------------------------
 * Dissassembler:
 * ------------------------------------------------------------------------*/

#ifdef DEBUG_CODE
static Void local dissassemble(pc)	/* print dissassembly of code	   */
Addr pc; {
    for (;;)
	switch (instrAt(pc)) {
	    case iEND	 : return;
	    case iLOAD	 : pc = dissInt(pc,"LOAD");	 break;
	    case iCELL	 : pc = dissCell(pc,"CELL");	 break;
	    case iCHAR	 : pc = dissInt(pc,"CHAR");	 break;
	    case iINT	 : pc = dissInt(pc,"INT");	 break;
	    case iFLOAT  : pc = dissFloat(pc,"FLOAT");   break;
	    case iSTRING : pc = dissText(pc,"STRING");	 break;
	    case iMKAP	 : pc = dissInt(pc,"MKAP");	 break;
	    case iUPDATE : pc = dissInt(pc,"UPDATE");	 break;
	    case iUPDAP  : pc = dissInt(pc,"UPDAP");	 break;
	    case iEVAL	 : pc = dissNone(pc,"EVAL");	 break;
	    case iFLUSH  : pc = dissNone(pc,"FLUSH");	 break;
	    case iRETURN : pc = dissNone(pc,"RETURN");	 break;
	    case iSETSTK : pc = dissInt(pc,"SETSTK");	 break;
	    case iSTKIS  : pc = dissInt(pc,"STKIS");	 break;
	    case iALLOC  : pc = dissInt(pc,"ALLOC");	 break;
	    case iSLIDE  : pc = dissInt(pc,"SLIDE");	 break;
	    case iROOT	 : pc = dissInt(pc,"ROOT");	 break;
            case iDICT   : pc = dissInt(pc,"DICT");      break;
	    case iINTEQ  : pc = dissIntLab(pc,"INTEQ");	 break;
#if NPLUSK
	    case iINTGE  : pc = dissIntLab(pc,"INTGE");	 break;
	    case iINTDV  : pc = dissIntLab(pc,"INTDV");	 break;
#endif
	    case iTEST	 : pc = dissCellLab(pc,"TEST");	 break;
	    case iGOTO	 : pc = dissLab(pc,"GOTO");	 break;
	    case iLABEL  : pc = dissLab(pc,"LABEL");	 break;
	    default	 : internal("unknown instruction");
	}
}

static Void local printCell(c)	       /* printable representation of Cell */
Cell c; {
    if (isName(c))
	printf("%s",textToStr(name(c).text));
    else
	printf("$%d",c);
}

static Addr local dissNone(pc,s)       /* dissassemble instr no args	   */
Addr   pc;
String s; {
    printf("%s\n",s);
    return pc+1;
}

static Addr local dissInt(pc,s)        /* dissassemble instr with Int arg  */
Addr   pc;
String s; {
    printf("%s\t%d\n",s,intAt(pc+1));
    return pc+2;
}

static Addr local dissFloat(pc,s)      /* dissassemble instr with Float arg*/
Addr   pc;
String s; {
#if BREAK_FLOATS
    printf("%s\t%s\n",s,
	floatToString(floatFromParts(cellAt(pc+1),cellAt(pc+2))));
    return pc+3;
#else
    printf("%s\t%s\n",s,floatToString(floatAt(pc+1)));
    return pc+2;
#endif
}

static Addr local dissCell(pc,s)       /* dissassemble instr with Cell arg */
Addr   pc;
String s; {
    printf("%s\t",s);
    printCell(cellAt(pc+1));
    printf("\n");
    return pc+2;
}

static Addr local dissText(pc,s)       /* dissassemble instr with Text arg */
Addr   pc;
String s; {
    printf("%s\t%s\n",s,textToStr(textAt(pc+1)));
    return pc+2;
}

static Addr local dissLab(pc,s)       /* dissassemble instr with Label arg */
Addr   pc;
String s; {
    printf("%s\t%d\n",s,labAt(pc+1));
    return pc+2;
}

static Addr local dissIntLab(pc,s)    /* dissassemble instr with Int+Label */
Addr   pc;
String s; {
    printf("%s\t%d\t%d\n",s,intAt(pc+1),labAt(pc+2));
    return pc+3;
}

static Addr local dissCellLab(pc,s)   /* dissassemble instr with Cell+Label*/
Addr   pc;
String s; {
    printf("%s\t",s);
    printCell(cellAt(pc+1));
    printf("\t%d\n",labAt(pc+2));
    return pc+3;
}
#endif

/* --------------------------------------------------------------------------
 * Compile expression to code which will build expression evaluating guards
 * and testing cases to avoid building complete graph.
 *
 * This section of code has been rewritten from the original form in
 * version 2.21 of the interpreter to use a more sophisticated form of
 * continuation rather than the simple UPDRET/SHOULDNTFAIL/label etc
 * used in that program.  The aim of this rewrite is (of course) to try
 * and produce better output code.  The basic type for continuations is:
 *
 *	type Continuation = (Int, ThenWhat)
 *	data ThenWhat	  = RUNONC 		-- next instr
 *			  | FRUNONC		-- FLUSH then next instr
 *			  | BRANCH Label	-- branch to label
 *			  | FBRANCH Label	-- FLUSH then branch
 *			  | UPDRETC		-- UPDATE 0; RETURN
 *
 * As an example of the kind of optimisations we can get by this:
 *
 *  ...; MKAP 4; SLIDE m ; UPDATE 0 ; RETURN
 *				     ====> ...; MKAP 3; UPDAP 0; RETURN
 *
 *  ...; MKAP 2; FLUSH ; UPDATE 0; RETURN
 *				     ====> ...; MKAP 1; UPDAP 0; RETURN
 *
 *  ...; SLIDE m; SLIDE n; ...       ====> ...; SLIDE (m+n); ...
 *  (this one was previously obtained by a peephole optimisation)
 * ------------------------------------------------------------------------*/

static Pair shouldntFail;		/* error continuation		   */
static Pair functionReturn;		/* initial function continuation   */
static Pair noAction;			/* skip continuation		   */

static Void local doCont(c)		/* insert code for continuation    */
Pair c; {
    Int sl = intOf(fst(c));
    switch (whatIs(snd(c))) {
	case FRUNONC : asFLUSH();
	case RUNONC  : if (sl>0) {
			   asSLIDE(sl);
		       }
		       break;

	case FBRANCH : asFLUSH();
	case BRANCH  : if (sl>0) {
			   asSLIDE(sl);
		       }
		       asGOTO(intOf(snd(snd(c))));
		       break;

	case UPDRETC : asUPDATE(0);
		       asRETURN();
                       break;

	case ERRCONT :
	default	     : internal("doCont");
    }
}

#define slide(n,d)   pair(mkInt(intOf(fst(d))+n),snd(d))
#define isRunon(d)   (snd(d)==RUNONC || snd(d)==FRUNONC)
#define fbranch(l,d) pair(fst(d),ap(FBRANCH,l))
#define frunon(d)    pair(fst(d),FRUNONC)

static Pair local flush(d)		/* force flush on continuation	   */
Pair d; {
    switch (whatIs(snd(d))) {
	case RUNONC : return frunon(d);
	case BRANCH : return fbranch(snd(snd(d)),d);
	default	    : return d;
    }
}

static Void local make(e,co,f,d)	/* Construct code to build e, given*/
Cell  e;				/* current offset co, and branch   */
Int   co;				/* to f on failure, d on completion*/
Label f;
Pair  d; {
    switch (whatIs(e)) {

	case LETREC    : {   Int n = buildLoc(fst(snd(e)),co);
			     make(snd(snd(e)),co+n,f,slide(n,d));
		         }
		         break;

	case FATBAR    : if (isRunon(d)) {
			     Label l1     = newLabel();
			     Label l2     = newLabel();
			     Int   savesp = srsp;
			     make(fst(snd(e)),co,l1,fbranch(mkInt(l2),d));
			     asLABEL(l1);
			     asSETSTK(savesp);
			     make(snd(snd(e)),co,f,frunon(d));
			     asLABEL(l2);
			 }
			 else {
			     Label l  = newLabel();
			     Cell  d1 = flush(d);
			     Int   savesp = srsp;
			     make(fst(snd(e)),co,l,d1);
			     asLABEL(l);
			     asSETSTK(savesp);
			     make(snd(snd(e)),co,f,d1);
			 }
                         break;

	case COND      : makeCond(fst3(snd(e)),
				  snd3(snd(e)),
				  thd3(snd(e)),co,f,d);
		         break;

	case CASE      : makeCase(snd(e),co,f,d);
			 break;

	case GUARDED   : makeGded(snd(e),co,f,d);
		         break;

	case AP        : if (andorOptimise) {
			     Cell h = getHead(e);
			     if (h==nameAnd && argCount==2) {
				 /* x && y ==> if x then y else False	   */
				 makeCond(arg(fun(e)),arg(e),nameFalse,co,f,d);
				 break;
			     }
			     else if (h==nameOr && argCount==2) {
				 /* x || y ==> if x then True else y	   */
				 makeCond(arg(fun(e)),nameTrue,arg(e),co,f,d);
				 break;
			     }
			 }
                         buildAp(e,co,f,TRUE);
                         doCont(d);
                         break;

	case NAME      : dependsOn(e);
	case UNIT      :
	case TUPLE     : asCELL(e);
		         doCont(d);
		         break;

	/* for dict cells, ensure that CELL referred to in the code is the */
	/* dictionary cell at the head of the dictionary; not just a copy  */
	/* In the interpreter, this was needed for the benefit of garbage  */
	/* collection (and to avoid having multiple copies of a single	   */
	/* DICTCELL).  In the compiler, we need it to justify the use of   */
	/* cellIsMember() in dependsOn() below.				   */

	case DICTCELL  : asCELL(dict(dictOf(e)));
			 dependsOn(dict(dictOf(e)));
		         doCont(d);
		         break;

	case INTCELL   : asINTEGER(intOf(e));
		         doCont(d);
		         break;

        case FLOATCELL : asFLOAT(floatOf(e));
		         doCont(d);
			 break;

	case STRCELL   : asSTRING(textOf(e));
		         doCont(d);
		         break;

	case CHARCELL  : asCHAR(charOf(e));
		         doCont(d);
		         break;

	case OFFSET    : asLOAD(offsPosn[offsetOf(e)]);
		         doCont(d);
		         break;

	default        : internal("make");
    }
}

static Void local makeCond(i,t,e,co,f,d)/* Build code for conditional	   */
Cell  i,t,e;
Int   co;
Label f;
Pair  d; {
    if (andorOptimise && i==nameOtherwise)
	make(t,co,f,d);
    else {
	Label l1 = newLabel();
	Int   savesp;

	make(i,co,f,noAction);
	asEVAL();
	savesp = srsp;
	asTEST(nameTrue,l1);
	if (isRunon(d)) {
	    Label l2 = newLabel();

	    make(t,co,f,fbranch(mkInt(l2),d));
            asLABEL(l1);
	    if (srsp!=savesp)
		asSETSTK(savesp);
	    make(e,co,f,frunon(d));
	    asLABEL(l2);
	}
	else {
	    Cell d1 = flush(d);
	    make(t,co,f,d1);
	    asLABEL(l1);
	    if (srsp!=savesp)
		asSETSTK(savesp);
	    make(e,co,f,d1);
	}
    }
}

static Void local makeCase(c,co,f,d)	/* construct code to implement case*/
Cell  c;				/* makes the assumption that FLUSH */
Int   co;				/* will never be required	   */
Label f;
Pair  d; {
    List  cs = snd(c);
    Cell  d1 = d;
    Label l0;

    make(fst(c),co,shouldntFail,noAction);
    asEVAL();

    if (isRunon(d)) {
	l0 = newLabel();
	d1 = pair(mkInt(0),ap(BRANCH,mkInt(l0)));
    }

    for(; nonNull(tl(cs)); cs=tl(cs)) {
	Label l      = newLabel();
        Int   savesp = srsp;
	testCase(hd(cs),co,f,l,d1);
	asLABEL(l);
	asSTKIS(savesp);
    }

    if (isRunon(d)) {
        Int savesp = srsp;
	testCase(hd(cs),co,f,f,noAction);
	asLABEL(l0);
	asSTKIS(savesp);
    }
    else
	testCase(hd(cs),co,f,f,d1);
}

static Void local testCase(c,co,f,cf,d)	/* Produce code for guard	   */
Pair  c;
Int   co;				/* labels determine where to go if:*/
Label f;				/* match succeeds, but rest fails  */
Label cf;				/* this match fails		   */
Pair  d; {
    Int n = discrArity(fst(c));
    Int i;
    switch (whatIs(fst(c))) {
	case INTCELL : asINTEQ(intOf(fst(c)),cf);
		       break;
#if NPLUSK
	case ADDPAT  : asINTGE(intValOf(fst(c)),cf);
		       break;
	case MULPAT  : asINTDV(intValOf(fst(c)),cf);
		       break;
#endif
	default      : asTEST(fst(c),cf);
		       break;
    }
    for (i=1; i<=n; i++)
	offsPosn[co+i] = ++srsp;
    make(snd(c),co+n,f,d);
}

static Void local makeGded(gs,co,f,d)	/* construct code to implement gded*/
List  gs;				/* equations.  Makes the assumption*/
Int   co;				/* that FLUSH will never be reqd.  */
Label f;
Pair  d; {
    Cell  d1 = d;
    Label l0;

    if (isRunon(d)) {
	l0 = newLabel();
	d1 = pair(mkInt(0),ap(BRANCH,mkInt(l0)));
    }

    for(; nonNull(tl(gs)); gs=tl(gs)) {
	Label l = newLabel();
        Int   savesp = srsp;
	if (testGuard(hd(gs),co,f,l,d1))
	    return;
	asLABEL(l);
	asSTKIS(savesp);
    }

    if (isRunon(d)) {
        Int   savesp = srsp;
	testGuard(hd(gs),co,f,f,noAction);
	asLABEL(l0);
	asSTKIS(savesp);
    }
    else
	testGuard(hd(gs),co,f,f,d1);
}

static Bool local testGuard(g,co,f,cf,d) /* Produce code for guard	   */
Pair  g;				/* return TRUE if otherwise found  */
Int   co;
Label f;
Label cf;
Pair  d; {
    if (andorOptimise && fst(g)==nameOtherwise) {
	make(snd(g),co,f,d);
	return TRUE;
    }
    else {
	make(fst(g),co,shouldntFail,noAction);
	asEVAL();
	asTEST(nameTrue,cf);
	make(snd(g),co,f,d);
	return FALSE;
    }
}

/* --------------------------------------------------------------------------
 * Compile expression to code which will build expression without any
 * evaluation.
 * ------------------------------------------------------------------------*/

static List scDeps;			/* records immediate dependent	   */
					/* names and dictionaries	   */

static Void local dependsOn(n)		/* update scDeps with new name	   */
Cell n; {

    if (isName(n))			/* ignore:			   */
	if (name(n).defn == CFUN ||	/* - constructor functions	   */
	    name(n).defn == MFUN)	/* - member fns (shouldn't occur)  */
	    return;

    if (!cellIsMember(n,scDeps))	/* add to list of dependents	   */
	scDeps = cons(n,scDeps);
}

static Void local build(e,co)		/* Generate code which will build  */
Cell e; 				/* instance of given expression but*/
Int  co; {				/* perform no evaluation 	   */
    Int n;

    switch (whatIs(e)) {

	case LETREC    : n = buildLoc(fst(snd(e)),co);
		         build(snd(snd(e)),co+n);
		         asSLIDE(n);
		         break;

	case FATBAR    : build(snd(snd(e)),co);
		         build(fst(snd(e)),co);
		         asCELL(nameFatbar);
		         asMKAP(2);
		         break;

	case COND      : build(thd3(snd(e)),co);
		         build(snd3(snd(e)),co);
		         build(fst3(snd(e)),co);
		         asCELL(nameIf);
	  	         asMKAP(3);
	  	         break;

	case GUARDED   : buildGuards(snd(e),co);
		         break;

	case AP        : buildAp(e,co,shouldntFail,FALSE);
		         break;

	case NAME      : dependsOn(e);
	case UNIT      :
	case TUPLE     : asCELL(e);
			 break;

	case DICTCELL  : asCELL(dict(dictOf(e)));	/* see comments for*/
			 dependsOn(dict(dictOf(e)));	/* DICTCELL in make*/
			 break;				/* function above  */

	case INTCELL   : asINTEGER(intOf(e));
			 break;

        case FLOATCELL : asFLOAT(floatOf(e));
			 break;

	case STRCELL   : asSTRING(textOf(e));
			 break;

	case CHARCELL  : asCHAR(charOf(e));
			 break;

	case OFFSET    : asLOAD(offsPosn[offsetOf(e)]);
		         break;

	default        : internal("build");
    }
}

static Void local buildGuards(gs,co)	/* Generate code to compile list   */
List gs;				/* of guards to a conditional expr */
Int  co; {				/* without evaluation		   */
    if (isNull(gs)) {
	asCELL(nameFail);
    }
    else {
	buildGuards(tl(gs),co);
	build(snd(hd(gs)),co);
	build(fst(hd(gs)),co);
	asCELL(nameIf);
	asMKAP(3);
    }
}

static Int local buildLoc(vs,co)	/* Generate code to build local var*/
List vs;				/* bindings on stack,  with no eval*/
Int  co; {
    Int n = length(vs);
    Int i;

    for (i=1; i<=n; i++)
	offsPosn[co+i] = srsp+i;
    asALLOC(n);
    for (i=1; i<=n; i++) {
	build(hd(vs),co+n);
	asUPDATE(offsPosn[co+i]);
	vs = tl(vs);
    }
    return n;
}

/* --------------------------------------------------------------------------
 * We frequently encounter functions which call themselves recursively with
 * a number of initial arguments preserved:
 * e.g.  (map f) []	= []
 *	 (map f) (x:xs) = f x : (map f) xs
 * Lambda lifting, in particular, is likely to introduce such functions.
 * Rather than reconstructing a new instance of the recursive function and
 * it's arguments, we can extract the relevant portion of the root of the
 * current redex.
 *
 * The following functions implement this optimisation.
 * ------------------------------------------------------------------------*/

static Int  nonRoots;		       /* #args which can't get from root  */
static Int  rootPortion;	       /* portion of root used ...	   */
static Name definingName;	       /* name of func being defined,if any*/
static Int  definingArity;	       /* arity of definingName 	   */

static Void local analyseAp(e)	       /* Determine if any portion of an   */
Cell e; {			       /* application can be built using a */
    if (isAp(e)) {		       /* portion of the root		   */
	analyseAp(fun(e));
	if (nonRoots==0 && rootPortion>1
			&& isOffset(arg(e))
			&& offsetOf(arg(e))==rootPortion-1)
	    rootPortion--;
	else
	    nonRoots++;
    }
    else if (e==definingName)
	rootPortion = definingArity+1;
    else
	rootPortion = 0;
}

static Void local buildAp(e,co,f,str)	/* Build application, making use of*/
Cell  e;				/* root optimisation if poss.	   */
Int   co;
Label f;
Bool  str; {
    Int nr, rp, i;

    nonRoots = 0;
    analyseAp(e);
    nr = nonRoots;
    rp = rootPortion;

    for (i=0; i<nr; ++i) {
	build(arg(e),co);
	e = fun(e);
    }

    if (isSelect(e)) {
        if (selectOf(e)>0) {
	    asDICT(selectOf(e));
	}
    }
    else {
	if (isName(e) && name(e).defn==MFUN) {
	    asDICT(name(e).number);
	    nr--;	/* AP node for member function need never be built */
	}
	else {
	    if (0<rp && rp<=definingArity) {
		asROOT(rp-1);
	    }
	    else
		if (str)
		    make(e,co,f,noAction);
		else
		    build(e,co);
	}

	if (nr>0) {
	    asMKAP(nr);
	}
    }
}

/* --------------------------------------------------------------------------
 * Code generator entry point:
 * ------------------------------------------------------------------------*/

Addr codeGen(n,arity,e) 	       /* Generate code for expression e,  */
Name n; 			       /* treating return value of CAFs    */
Int  arity;			       /* differently to functs with args  */
Cell e; {
    extern Void pScDef Args((Text,Int,Cell));
    extern Bool dumpScs;

    definingName  = n;
    definingArity = arity;
    scDeps	  = NIL;
#ifdef DEBUG_CODE
printf("------------------\n");
if (nonNull(n)) printf("name=%s\n",textToStr(name(n).text));
printf("Arity   = %d\n",arity);
printf("codeGen = "); printExp(stdout,e); putchar('\n');
#endif
    if (dumpScs)
	pScDef(name(n).text,arity,e);
    else {
	Int i;
	asSTART();
	for (i=1; i<=arity; i++)
	    offsPosn[i] = ++srsp;
	make(e,arity,FAIL,functionReturn);
	asEND();
    }
    name(n).defn = scDeps;
    scDeps	 = NIL;
#ifdef DEBUG_CODE
dissassemble(startInstr);
printf("------------------\n");
#endif
    return startInstr;
}

Void externalPrim(n,s)		/* add name n as an external primitive	   */
Name   n;
String s; {
    asSTART();
    asEXTERN(findText(s));
    name(n).arity   = externArity(name(n).type);
    name(n).code    = startInstr;
    name(n).primDef = 0;
}

/* --------------------------------------------------------------------------
 * C code generator: produces (portable, I hope) C output to implement a
 * specified main program.
 * ------------------------------------------------------------------------*/

Void outputCode(fp,mn,topLevel)		/* print complete C program to	   */
FILE   *fp;				/* implement program with main mn  */
Name   mn;				/* using specified top level	   */
String topLevel; {
    List   scs = identifyDeps(mn);	/* determine which supercombinator */
    Target t   = length(scs);		/* definitions are needed in prog. */
    Target i   = 0;

    fprintf(fp,"#include %s\n\nint argcheck=ARGCHECK;\n\n",GOFC_INCLUDE);
    fprintf(fp,"TopLevel topLevel = %s;\n\n",topLevel);
    outputCDecls(fp,scs);
    outputCDicts(fp);

    setGoal("Compiling to C",t);
    for (; nonNull(scs); scs=tl(scs)) {
	outputCSc(fp,hd(scs));
	soFar(i++);
    }
    done();
}

static int *dictUse   = 0;		/* records dictionaries required   */
static int num_dicts  = 0;		/* dictionaries required	   */
static int num_sdicts = 0;		/* all dictionaries known to system*/

static List local identifyDeps(mn)	/* list all dependents scs for mn  */
Name mn; {
    List needed     = singleton(mn);	/* Start with dependents of mn	   */
    List scs        = NIL;
    List ns	    = NIL;
    Int  i;

    num_sdicts = newDict(0);
    dictUse    = (int *)calloc(num_sdicts,sizeof(int));
    if (!dictUse) {
	ERROR(0) "Cannot allocate dictionary use table"
	EEND;
    }
    for (i=0; i<num_sdicts; i++)
	dictUse[i] = (-1);		/* (-1) => not required		   */

    while (nonNull(needed)) {		/* Cycle through to find all	   */
	Cell t = needed;		/* dependents ...		   */
	Cell n = hd(t);
	needed = tl(needed);
	if (isName(n)) {		/* Dependent is a name		   */
	    if (!name(n).primDef && name(n).defn!=NEEDED) {
		tl(t)        = scs;
		scs	     = t;
		map1Proc(checkPrimDep,n,name(n).defn);
		needed       = appendOnto(name(n).defn,needed);
		name(n).defn = NEEDED;
	    }
	}
	else {				/* Dependent is a dictionary	   */
	    if (dictUse[dictOf(n)]<0)
		for (i=dictOf(n); (dictUse[i++]=0), i<num_sdicts; )
		    if (isAp(dict(i))) {	/* member function	   */
			if (isName(fun(dict(i))) &&
			    whatIs(arg(dict(i)))==DICTCELL)
			    needed = cons(fun(dict(i)),needed);
			else
			    if (fun(dict(i))!=nameUndefMem)
				internal("bad dict ap");
		    }
		    else			/* DICTCELL		   */
			if (dictOf(dict(i))==i)	/* past end of dictionary  */
			    break;
			else
			    needed = cons(dict(i),needed);
        }
    }

    ns = scs;				/* number supercombinators	   */
    for (i=0; nonNull(ns); ns=tl(ns))
	name(hd(ns)).number = i++;

    num_dicts = 0;			/* number dictionaries		   */
    for (i=0; i<num_sdicts; i++)
	if (dictUse[i]!=(-1))
	    dictUse[i] = num_dicts++;

    return scs;
}

static Void local checkPrimDep(n,m)	/* Check that primitive dependent  */
Name n;					/* m of n is supported by gofc	   */
Cell m; {
    if (isName(m) && name(m).primDef == PRIM_NOGOFC) {
	ERROR(0)
	 "Primitive function %s is not supported by the gofc runtime system\n",
         primitives[name(m).number].ref
	ETHEN
	ERRTEXT "(used in the definition of %s)", textToStr(name(n).text)
	EEND;
    }
}

static Void local outputCDecls(fp,scs)	/* print forward declarations for  */
FILE *fp;				/* supercombinators required	   */
List scs; {
    int num_scs = length(scs);

    startTable("extern Super ", ";", ";\n");
#define declareSc(n) tableItem(fp,scNameOf(n))
    mapProc(declareSc,scs);
#undef  declareSc
    finishTable(fp);

    fprintf(fp,"\nint   num_scs = %d;\nCell  sc[%d];",num_scs,num_scs);
    fprintf(fp,"\nSuper *scNames[] = {\n");
    startTable("  ", ", ", "\n");
#define inArraySc(n) tableItem(fp,scNameOf(n))
    mapProc(inArraySc,scs);
#undef  inArraySc
    finishTable(fp);
    fprintf(fp,"};\n\n");
}

static Void local outputCDicts(fp)	/* print definitions for dictionary*/
FILE *fp; {				/* storage			   */
    char buffer[100];

    fprintf(fp,"int  num_dicts = %d;\n",num_dicts);

    if (num_dicts==0) {
	fprintf(fp,"Cell dict[]     = {0}; /* dummy entries */\n");
	fprintf(fp,"int  dictImps[] = {0};\n\n");
    }
    else {
	Int dn;
	fprintf(fp,"Cell dict[] = {\n");
	startTable("  ", ",", "\n");
    	for (dn=0; dn<num_sdicts; dn++) {
	    if (dictUse[dn]>=0) {
                if (isAp(dict(dn))) {
		    if (fst(dict(dn))==nameUndefMem)
			tableItem(fp,"0");
		    else {
			sprintf(buffer,"mkDict(%d)",
					dictUse[dictOf(arg(dict(dn)))]);
			tableItem(fp,buffer);
		    }
		}
		else {
		    sprintf(buffer,"mkDict(%d)",dictUse[dictOf(dict(dn))]);
		    tableItem(fp,buffer);
		}
	    }
	}
	finishTable(fp);
	fprintf(fp,"};\nint dictImps[] = {\n");
	startTable("  ", ",", "\n");
	for (dn=0; dn<num_sdicts; dn++)
	    if (dictUse[dn]>=0)
		if (isAp(dict(dn))) {
		    sprintf(buffer,"%d",name(fun(dict(dn))).number);
		    tableItem(fp,buffer);
		}
		else
		    tableItem(fp,"-1");
	finishTable(fp);
	fprintf(fp,"};\n\n");
    }
}

/* --------------------------------------------------------------------------
 * Supercombinator C code generator:
 *
 * The C code generator re-interprets the sequence of machine instructions
 * produced by the G-code code generator given above, using a simulated
 * stack, in much the same way as described in Simon Peyton Jones's book,
 * section 19.3.2.  To be quite honest, I don't think I really understood
 * that section of the book until I started to work on this piece of code!
 * ------------------------------------------------------------------------*/

static  int    rsp;			/* Runtime stack pointer	   */
static  int    rspMax;			/* Maximum value of stack pointer  */
static  int    pushes;			/* number of actual pushes in code */

#define rPush  if (++rsp>=rspMax) rspMax=rsp

static Void local rspRecalc() {		/* Recalculate rsp after change to */
    Int i = sp;				/* simulated stack pointer sp	   */
    for (rsp=(-1); i>=0; --i)
	if (isNull(stack(i)) || stack(i)==mkOffset(i))
	    rsp++;
    if (rsp>rspMax)			/* should never happen!		   */
	rspMax = rsp;
}

/* --------------------------------------------------------------------------
 * Output code for a single supercombinator:
 * ------------------------------------------------------------------------*/

#define ppushed(n)  (isNull(pushed(n)) ? POP : pushed(n))
#define tpushed(n)  (isNull(pushed(n)) ? TOP : pushed(n))

static Void local outputCSc(fp,n)	/* Print C code for supercombinator*/
FILE *fp;
Name n; {
    String s = 0;

    if (name(n).arity<10)		/* Print header			   */
	fprintf(fp,"comb%d(%s)",name(n).arity,scNameOf(n));
    else
	fprintf(fp,"comb(%s,%d)",scNameOf(n),name(n).arity);

    fprintf(fp,"  /* ");		/* include supercombinator name	   */
    for (s=textToStr(name(n).text); *s; s++) {
	fputc(*s,fp);
	if (*s=='*' && *(s+1)=='/')	/* avoid premature comment ending  */
	    fputc(' ',fp);
    }
    fprintf(fp," */\n");

    if (instrAt(name(n).code)==iEXTERN) /* link to an external function	   */
	externBody(fp,textToStr(textAt(name(n).code+1)),name(n).type);
    else {				/* regular supercombinator	   */
	List instrs = heapUse(cCode(name(n).arity,name(n).code));

	if (pushes>0 && rspMax>name(n).arity)
	    fprintf(fp,"  needStack(%d);\n",rspMax-name(n).arity);

	for (; nonNull(instrs); instrs=tl(instrs)) {
	    Cell instr = hd(instrs);

	    if (whatIs(instr)==C_LABEL){/* Handle printing of labels	   */
		instrs = tl(instrs);	/* move on to next instruction	   */
		if (isNull(instrs))
		    internal("no instr for label");
		outputLabel(fp,intOf(snd(instr)));
		fputc(':',fp);
		instr   = hd(instrs);
    	    }
	    else
		fprintf(fp,"  ");

            outputCinst(fp,instr);
	    fprintf(fp,";\n");
	}
    }
    fprintf(fp,"End\n\n");
}

static List local cCode(arity,pc)	/* simulate execution of G-code to */
Int  arity;				/* calculate corresponding C code  */
Addr pc; {
    Cell instrs = NIL;			/* holds sequence of C instrs	   */
    Int  i;
    Cell t;

    clearStack();			/* initialise simulated stack	   */
    for (i=0; i<=arity; i++) {
	push(mkOffset(i));
    }
    rsp    = arity;			/* and set Real stack ptr to match */
    rspMax = rsp;
    pushes = 0;

#define outC0(c)	instrs = cons(c,instrs)
#define outC1(c,o)	instrs = cons(ap(c,o),instrs)
#define outC2(c,o,p)	instrs = cons(ap(c,pair(o,p)),instrs)
#define outC3(c,o,p,q)	instrs = cons(ap(c,triple(o,p,q)),instrs)

    for (;;)
	switch (instrAt(pc)) {

	    case iEND	 : return rev(instrs);		 /* end of code	   */

	    case iLABEL	 : outC1(C_LABEL,		 /* program label  */
				 mkInt(labAt(pc+1)));
			   pc+=2;
			   continue;

	    case iLOAD	 : push(mkOffset(intAt(pc+1)));	 /* load from stack*/
			   pc+=2;
			   continue;

	    case iCELL	 : push(cellAt(pc+1));		 /* load const Cell*/
			   pc+=2;
			   continue;

	    case iCHAR	 : push(mkChar(intAt(pc+1)));	 /* load char const*/
			   pc+=2;
			   continue;

	    /* the treatment of integers used here relies on the assumption*/
	    /* that any number represented by a small int in the compiler  */
	    /* can also be represented by a small int in the runtime system*/

	    case iINT	 : t = mkInt(intAt(pc+1));	 /* load int const */
			   if (!isSmall(t)) {		 /* assume BIG int */
			       push(NIL);
			       rPush;
			       pushes++;
			       outC0(t);
			   }
			   else {        		 /* assume SMALL   */
			       push(t);
			   }
			   pc+=2;
			   continue;

	    case iFLOAT  : push(NIL);			 /* load float cnst*/
			   rPush;
			   pushes++;
#if BREAK_FLOATS
			   outC0(mkFloat(floatFromParts
						(cellAt(pc+1),cellAt(pc+2))));
			   pc+=3;
#else
			   outC0(mkFloat(floatAt(pc+1)));
			   pc+=2;
#endif
			   continue;

	    case iFLUSH  : if (nonNull(top())) {	 /* force top of   */
			       outC1(C_FLUSH,top());	 /* simulated stack*/
			       top() = NIL;		 /* onto real stack*/
			       rPush;
			       pushes++;
			   }
			   pc++;
			   continue;

	    case iSTRING : push(NIL);			 /* load str const */
			   rPush;
			   pushes++;
			   outC0(mkStr(textAt(pc+1)));
			   pc+=2;
			   continue;

	    case iMKAP   : for (i=intAt(pc+1); i>0; --i){/* make AP nodes  */
			       if (isNull(pushed(0)))
				   if (isNull(pushed(1))) {
				       outC0(C_MKAP);
				       rsp--;
				   }
				   else
				       outC1(C_TOPARG,pushed(1));
			       else
				   if (isNull(pushed(1)))
				       outC1(C_TOPFUN,pushed(0));
				   else {
				       rPush;
				       pushes++;
				       outC2(C_PUSHPAIR,pushed(0),pushed(1));
				   }
			       drop();
			       top() = NIL;
			   }
			   pc+=2;
			   continue;

	    case iUPDATE : t = stack(intAt(pc+1));	 /* update cell ...*/
			   if (!isOffset(t))
			       internal("iUPDATE");
			   if (offsetOf(t)!=0)
			       stack(intAt(pc+1)) = NIL;
			   if (isNull(pushed(0)))	 /* update cell ...*/
			       rsp--;

			   outC2(C_UPDATE,t,ppushed(0));

			   drop();
			   pc+=2;
			   continue;

	    case iUPDAP  : t = stack(intAt(pc+1));	 /* update AP node */
			   if (!isOffset(t))
			       internal("iUPDAP");
			   if (offsetOf(t)!=0)
			       stack(intAt(pc+1)) = NIL;

			   if (isNull(pushed(0)))
			       if (isNull(pushed(1))) {
				   outC1(C_UPDAP2,t);
				   rsp-=2;
			       }
			       else {
				   outC3(C_UPDAP,t,POP,pushed(1));
				   rsp--;
			       }
			   else
			       if (isNull(pushed(1))) {
				   outC3(C_UPDAP,t,pushed(0),POP);
                                   rsp--;
			       }
			       else
				   outC3(C_UPDAP,t,pushed(0),pushed(1));

			   drop();
			   drop();
			   pc+=2;
			   continue;

	    case iALLOC  : for (i=intAt(pc+1); i>0; --i){/* alloc loc vars */
			       rPush;
			       pushes++;
			       outC0(C_ALLOC);
			       push(mkOffset(rsp));
			   }
			   pc+=2;
			   continue;

	    case iSLIDE  : i = intAt(pc+1);		 /* remove loc vars*/
			   if (nonNull(top()))
			       i--;
			   outC2(C_SLIDE,mkInt(i),tpushed(0));
			   rsp -= i;
			   sp  -= intAt(pc+1);
                           pc  += 2;
			   continue;

	    case iDICT	 : if (isNull(top()))		 /* dict lookup    */
			       internal("iDICT");

			   if (whatIs(top())==DICTCELL)
			       top() = mkDict(dictOf(top())+intAt(pc+1));
			   else
			       top() = ap(mkSelect(intAt(pc+1)),top());

                           pc+=2;                        /* dict lookup    */
                           continue;

	    case iROOT	 : t = mkOffset(0);		 /* partial root   */
			   for (i=intAt(pc+1); i>0; --i)
			       t = ap(ROOTFST,t);
			   push(t);
			   pc+=2;
			   continue;

	    case iRETURN : outC0(C_RETURN);		 /* terminate	   */
			   pc++;
			   continue;

	    case iGOTO	 : outC1(C_GOTO,		 /* goto label	   */
				 mkInt(labAt(pc+1)));
			   pc+=2;
			   continue;

	    case iSETSTK : sp = intAt(pc+1);		 /* set stack ptr  */
			   rspRecalc();
			   outC1(C_SETSTK,mkInt(rsp));
			   pc += 2;
			   continue;

	    case iSTKIS  : sp = intAt(pc+1);		 /* set stack ptr  */
			   rspRecalc();			 /* but no C code  */
			   pc += 2;
			   continue;

	    case iINTEQ	 : 				 /* test integer ==*/
			   outC2(C_INTEQ,mkInt(intAt(pc+1)),
					 mkInt(labAt(pc+2)));
			   pc+=3;
			   continue;

#if NPLUSK
	    case iINTGE	 : push(NIL);			 /* test integer >=*/
			   rPush;
			   pushes++;
			   outC3(C_INTGE,mkInt(0),
					 mkInt(intAt(pc+1)),
					 mkInt(labAt(pc+2)));
                           pc+=3;
			   continue;

	    case iINTDV	 : push(NIL);			 /* test for mult  */
			   rPush;
			   pushes++;
			   outC3(C_INTDV,mkInt(0),
					 mkInt(intAt(pc+1)),
					 mkInt(labAt(pc+2)));
			   pc+=3;
			   continue;
#endif

	    case iTEST	 : t = cellAt(pc+1);		 /* test for cell  */
			   switch (whatIs(t)) {
			       case UNIT     : i = 0;
					       break;

			       case TUPLE    : i = tupleOf(t);
					       break;

			       case NAME     : i = name(t).arity;
					       outC2(C_TEST,t,
						 mkInt(labAt(pc+2)));
					       break;

			       case CHARCELL : i = 0;
					       outC2(C_TEST,t,
						 mkInt(labAt(pc+2)));
					       break;

			       default	     : internal("iTEST");
			   }

			   while (i-- > 0) {
			       rPush;
			       push(mkOffset(rsp));
			   }
			   pc+=3;
			   continue;

	    case iEVAL	 : if (isNull(pushed(0)))	 /* evaluate top() */
			       rsp--;
			   outC1(C_EVAL,ppushed(0));
			   drop();
			   pc++;
			   continue;

	    default	 : internal("illegal instruction");
			   break;
	}

#undef outC0
#undef outC1
#undef outC2
#undef outC3
}

/* --------------------------------------------------------------------------
 * Insert heap use annotations:
 * ------------------------------------------------------------------------*/

static Int heapNeeded;			/* used to return # heap cells reqd*/

static List local heapUse(instrs)	/* add annotations for heap use	   */
List instrs; {
    instrs = heapAnalyse(instrs);
    if (heapNeeded>0)
	instrs = cons(ap(C_HEAP,mkInt(heapNeeded)),instrs);
    return instrs;
}

static List local heapAnalyse(instrs)	/* analyse heap use in instruction */
List instrs; {
    Int  heap = 0;			/* number of heap cells needed     */
    List next;

    for (next=instrs; nonNull(next); next=tl(next))
	switch (whatIs(hd(next))) {
	    case FLOATCELL  : heap+=4;		/*conservative overestimate*/
			      continue;		/*without BREAK_FLOATS this*/
						/*will always use just one */
						/*cell, with it may use 1-4*/

	    case INTCELL    :			/*conservative overestimate*/
						/*again. Small ints may not*/
						/*require any heap storage */
	    case STRCELL    :
	    case C_MKAP	    :
	    case C_TOPFUN   :
	    case C_TOPARG   :
	    case C_PUSHPAIR :
	    case C_ALLOC    : heap++;
	    case C_UPDAP    :
	    case C_UPDAP2   :
	    case C_UPDATE   :
	    case C_SLIDE    :
	    case C_SETSTK   :
	    case C_FLUSH    : continue;

#if NPLUSK
	    case C_INTGE    :
	    case C_INTDV    : tl(next)		  = heapAnalyse(tl(next));
			      fst3(snd(hd(next))) = mkInt(1+heapNeeded);
			      heapNeeded	  = heap;
			      return instrs;
#endif

	    case C_TEST	    :
	    case C_INTEQ    :
	    case C_LABEL    :
	    case C_GOTO     :
	    case C_RETURN   :
	    case C_EVAL	    : tl(next)   = heapUse(tl(next));
			      heapNeeded = heap;
                              return instrs;

	    default	    : internal("heapAnalyse");
	}

    heapNeeded = heap;
    return instrs;
}

/* --------------------------------------------------------------------------
 * Output individual C code instructions:
 * ------------------------------------------------------------------------*/

static Void local outputCinst(fp,instr)	/* Output single C instruction	   */
FILE *fp;
Cell instr; {
    switch (whatIs(instr)) {
	case INTCELL    : fprintf(fp,"pushInt(%d)",intOf(instr));
			  break;

	case FLOATCELL  : fprintf(fp,"pushFloat(%s)",
					floatToString(floatOf(instr)));
			  break;

	case STRCELL    : fprintf(fp,"pushStr(");
			  outputCStr(fp,textToStr(textOf(instr)));
			  fputc(')',fp);
			  break;

	case C_MKAP	: fprintf(fp,"mkap()");
			  break;

	case C_TOPARG   : fprintf(fp,"toparg(");
			  expr(fp,snd(instr));
			  fputc(')',fp);
			  break;

	case C_TOPFUN   : fprintf(fp,"topfun(");
			  expr(fp,snd(instr));
			  fputc(')',fp);
			  break;

	case C_PUSHPAIR : fprintf(fp,"pushpair(");
			  expr(fp,fst(snd(instr)));
			  fputc(',',fp);
			  expr(fp,snd(snd(instr)));
			  fputc(')',fp);
			  break;

	case C_UPDATE   : fprintf(fp,"update(%d,",offsetOf(fst(snd(instr))));
			  expr(fp,snd(snd(instr)));
			  fputc(')',fp);
			  break;

	case C_UPDAP    : fprintf(fp,"updap(%d,",offsetOf(fst3(snd(instr))));
			  expr(fp,snd3(snd(instr)));
			  fputc(',',fp);
			  expr(fp,thd3(snd(instr)));
			  fputc(')',fp);
			  break;

	case C_UPDAP2	: fprintf(fp,"updap2(%d)",offsetOf(snd(instr)));
			  break;

	case C_ALLOC    : fprintf(fp,"alloc()");
			  break;

	case C_SLIDE    : fprintf(fp,"slide(%d,",intOf(fst(snd(instr))));
			  expr(fp,snd(snd(instr)));
			  fputc(')',fp);
			  break;

	case C_RETURN   : fprintf(fp,"ret()");
			  break;

	case C_GOTO	: outputJump(fp,intOf(snd(instr)));
			  break;

	case C_FLUSH	: fprintf(fp,"onto(");
			  expr(fp,snd(instr));
			  fputc(')',fp);
			  break;

	case C_SETSTK   : fprintf(fp,"setstk(%d)",intOf(snd(instr)));
			  break;

	case C_HEAP	: fprintf(fp,"heap(%d)",intOf(snd(instr)));
			  break;

	case C_INTEQ	: fprintf(fp,"inteq(%d) ",intOf(fst(snd(instr))));
			  outputJump(fp,intOf(snd(snd(instr))));
			  break;

#if NPLUSK
	case C_INTGE	: fprintf(fp,"intge(%d,%d) ",intOf(fst3(snd(instr))),
						     intOf(snd3(snd(instr))));
			  outputJump(fp,intOf(thd3(snd(instr))));
			  break;

	case C_INTDV	: fprintf(fp,"intdv(%d,%d) ",intOf(fst3(snd(instr))),
						     intOf(snd3(snd(instr))));
			  outputJump(fp,intOf(thd3(snd(instr))));
			  break;
#endif

	case C_TEST	: fprintf(fp,"test(");
			  expr(fp,fst(snd(instr)));
			  fprintf(fp,") ");
			  outputJump(fp,intOf(snd(snd(instr))));
			  break;

	case C_EVAL	: fprintf(fp,"eval(");
			  expr(fp,snd(instr));
			  fputc(')',fp);
			  break;

	default		: internal("bad C code");
    }
}

/* --------------------------------------------------------------------------
 * Output small parts of an expression:
 * ------------------------------------------------------------------------*/

static Void local expr(fp,n)		/* print C expression for value	   */
FILE *fp;
Cell n; {

    switch (whatIs(n)) {

	case TOP      : fprintf(fp,"top()");
			break;

	case POP      : fprintf(fp,"pop()");
			break;

	case OFFSET   : fprintf(fp,"offset(%d)",offsetOf(n));
			break;

	case CHARCELL : fprintf(fp,"mkChar(%d)",charOf(n));
			break;

	case INTCELL  : fprintf(fp,"mkSmall(%d)",intOf(n));
			break;

	case AP	      : if (fst(n)==ROOTFST) {
			    fprintf(fp,"rootFst(");
			    expr(fp,arg(n));
			    fputc(')',fp);
			}
			else if (isSelect(fst(n))) {
			    fprintf(fp,"dsel(%d,",selectOf(fst(n)));
			    expr(fp,arg(n));
			    fputc(')',fp);
			}
			else
			    internal("exprAP");
			break;

	case DICTCELL : fprintf(fp,"dict[%d]",dictUse[dictOf(n)]);
			break;

	case UNIT     : fprintf(fp,"mkCfun(0)");
			break;

	case TUPLE    : fprintf(fp,"mkCfun(%d)",tupleOf(n));
			break;

	case NAME     : if (name(n).defn==CFUN)
			    fprintf(fp,"mkCfun(%d)",name(n).number);
			else if (name(n).primDef)
			    fprintf(fp,"%s",primitives[name(n).number].ref);
			else
			    fprintf(fp,"sc[%d]",name(n).number);
			break;

	default	      : internal("expr");
    }
}

static Void local outputLabel(fp,lab)	/* print C program label	   */
FILE *fp;
Int  lab; {
    if (lab<=26)
	fputc('a'+lab-1, fp);
    else
	fprintf(fp,"a%d",lab-26);
}

static Void local outputJump(fp,lab)	/* print jump to label, taking	   */
FILE *fp;				/* special account of FAIL label   */
Int  lab; {
    if (lab==FAIL)
	fprintf(fp,"fail()");
    else {
	fprintf(fp,"goto ");
	outputLabel(fp,lab);
    }
}

static Void local outputCStr(fp,s)	/* print out string, taking care   */
FILE   *fp;				/* to avoid problems with C escape */
String s; {				/* sequences			   */
    fputc('"',fp);
    for (; *s; s++) {
        if (*s=='\\' || *s=='"')
	    fprintf(fp,"\\%c",*s);
	else if (isprint(*s))
	    fputc(*s,fp);
	else if (*s=='\n')
	    fprintf(fp,"\\n");
	else
	    fprintf(fp,"\\%03o",(*s<0 ? *s+NUM_CHARS : *s));
    }
    fputc('"',fp);
}

static Bool local validCIdent(s)	/* check whether string s is valid */
String s; {				/* C identifier			   */
    if (*s=='v' && isdigit(s[1]))	/* avoid clashes with Gofer's own  */
	return FALSE;			/* generated function names ...	   */
    for (; *s && isascii(*s) && isalnum(*s); s++)
	;
    return *s=='\0';
}

static String local scNameOf(n)		/* get name of C implementation of */
Name n; {				/* a particular supercombinator	   */
    String s = textToStr(name(n).text);
    static char buffer[100];

    if (validCIdent(s) && strlen(s)<96)
	sprintf(buffer,"sc_%s",s);
    else
	sprintf(buffer,"sc_%d",name(n).number);

    return buffer;
}

/* --------------------------------------------------------------------------
 * Pretty printing of tables:
 * ------------------------------------------------------------------------*/

#define TABLEWIDTH 72
static int    tableCol;
static int    tableItems;
static String tableStart;
static String tableEndLine;
static String tableEndTab;

static Void local startTable(start,endLine,endTab)
String start;
String endLine;
String endTab; {
    tableStart   = start;
    tableEndLine = endLine;
    tableEndTab  = endTab;
    tableCol     = 0;
    tableItems   = 0;
}

static Void local finishTable(fp)
FILE *fp; {
    if (tableCol>0)
	fprintf(fp,tableEndTab);
}

static Void local tableItem(fp,s)
FILE   *fp;
String s; {
    int n = strlen(s);

    if (tableItems++ == 0) {
	fprintf(fp,tableStart);
	tableCol = strlen(tableStart);
    }
    else {
	if (tableCol+n+2>TABLEWIDTH) {
	    fprintf(fp,"%s\n%s",tableEndLine,tableStart);
	    tableCol = strlen(tableStart);
	}
	else {
	    fprintf(fp,", ");
	    tableCol+=2;
	}
    }
    fprintf(fp,"%s",s);
    tableCol += n;
}

/* --------------------------------------------------------------------------
 * Interfacing to external code:
 *
 * The following functions are used to support a simple external function
 * calling mechanism.  It currently supports argument values:
 *  NIL     a Cell value, passed or returned without evaluation
 *  other   special type, recognized, evaluated an returned
 *
 * When available, arrays, mutable arrays, and mutable variables are
 * allowed to be passed across the interface, after they have been
 * evaluated.
 *
 * The unit value UNIT is also used for a void return in the IO monad.
 * ------------------------------------------------------------------------*/

static Int local externArity(ty)	/* find arity of exernal function  */
Type ty; {				/* with type ty			   */
    Int arity = 0;
    if (isPolyType(ty))
	ty = monoTypeOf(ty);
    if (whatIs(ty)==QUAL) {
	arity = length(fst(snd(ty)));
	ty    = snd(snd(ty));
    }
    while (isAp(ty) && isAp(fun(ty)) && fun(fun(ty))==ARROW) {
	arity++;
	ty = arg(ty);
    }
#if IO_MONAD
    if (isAp(ty) && (fun(ty)==typeIO ||
		     (isAp(fun(ty)) && fun(fun(ty)==typeST))))
	arity++;
#endif
    return arity;
}

static Type local transExtType(ty)	/* translate to external type      */
Type ty; {
    if (ty==typeBool || ty==typeChar || ty==typeInt || ty==typeFloat)
	return ty;
    else {
#if (IO_MONAD | HASKELL_ARRAYS)
	Type h = getHead(ty);
#if HASKELL_ARRAYS
	if (h==typeArray  && argCount==2) return h;
#if IO_MONAD
	if (h==typeMutArr && argCount==3) return h;
#endif
#endif
#if IO_MONAD
	if (h==typeMutVar && argCount==2) return h;
#endif
#endif
	return NIL;
    }
}

static String local showExtType(ty)	/* give C type for Gofer type	   */
Type ty; {
    if (ty==typeBool || ty==typeChar || ty==typeInt || ty==typeFloat)
	return textToStr(tycon(ty).text);
    else if (ty==UNIT)
	return "Void";
    else
	return "Cell";
}

static String local showExtRes(ty)	/* expression to get result of	   */
Type ty; {				/* evaluation expr of type ty	   */
    if (ty==typeBool)  return "(whnf==mkCfun(1))";
    if (ty==typeChar)  return "charOf(whnf)";
    if (ty==typeInt)   return "intOf(whnf)";
    if (ty==typeFloat) return "floatOf(whnf)";
    return "whnf";
}

static String local showExtRet(ty)	/* expression to turn result r to  */
Type ty; {				/* a value of type ty		   */
    if (ty==typeBool)  return "mkCfun(r ? 1 : 0)";
    if (ty==typeChar)  return "mkChar(r)";
    if (ty==typeInt)   return "mkInt(r)";
    if (ty==typeFloat) return "mkFloat(r)";
    if (ty==UNIT)      return "mkCfun(0)";
    return "r";
}

static Void local externBody(fp,exn,ty)	/* generate body for call to extern*/
FILE   *fp;				/* function			   */
String exn;
Type   ty; {
    List argTypes = NIL;		/* list of types of arguments	   */
    Bool ioMonad  = FALSE;		/* TRUE => fn called via IO monad  */
    Int  args     = 0;
    Int  i;
    List ts;

    /* Step 1: analyse type, to determine args required etc.		   */

    if (isPolyType(ty))
	ty = monoTypeOf(ty);
    if (whatIs(ty)==QUAL)
	ty = snd(snd(ty));
    while (isAp(ty) && isAp(fun(ty)) && fun(fun(ty))==ARROW) {
	argTypes = cons(transExtType(arg(fun(ty))),argTypes);
	ty       = arg(ty);
	++args;
    }
#if IO_MONAD
    if (isAp(ty) && (fun(ty)==typeIO ||
		     (isAp(fun(ty)) && fun(fun(ty))==typeST))) {
	ioMonad = TRUE;
	ty      = arg(ty);
	++args;
    }
    if (ty!=UNIT)
	ty = transExtType(ty);
#else
    ty       = transExtType(ty);
#endif
    argTypes = rev(argTypes);

    /* Step 2: Print definitions for external function, and temp vars	   */

    fprintf(fp,"{   extern %s %s Args((",showExtType(ty),exn);
    if (isNull(argTypes))
	fprintf(fp,"Void));\n");
    else {
        for (ts=argTypes; nonNull(ts); ts=tl(ts)) {
	    fprintf(fp,showExtType(hd(ts)));
	    if (nonNull(tl(ts)))
		fprintf(fp,",");
	}
	fprintf(fp,"));\n");
        for (i=args, ts=argTypes; nonNull(ts); ts=tl(ts), --i)
	    if (nonNull(hd(ts)))
		fprintf(fp,"    %s o%d;\n",showExtType(hd(ts)),i);
    }
    if (ty!=UNIT)
	fprintf(fp,"    %s r;\n",showExtType(ty));

    /* Step 3: Evaluate arguments if necessary				   */

    if (ioMonad) {
	fprintf(fp,"    eval(offset(1));\n");
	i++;
    }
    for (i=args, ts=argTypes; nonNull(ts); ts=tl(ts), --i)
	if (nonNull(hd(ts))) {
	    fprintf(fp,"    eval(offset(%d));\n",i);
	    fprintf(fp,"    o%d = %s;\n",i,showExtRes(hd(ts)));
	}

    /* Step 4: Call function and return result				   */

    fprintf(fp,"    ");
    if (ty!=UNIT)
	fprintf(fp,"r = ");
    fprintf(fp,"%s(",exn);
    for (i=args, ts=argTypes; nonNull(ts); ts=tl(ts), --i) {
	if (isNull(hd(ts)))
	    fprintf(fp,"offset(%d)",i);
	else
	    fprintf(fp,"o%d",i);
	if (nonNull(tl(ts)))
	    fprintf(fp,",");
    }
    fprintf(fp,");\n");

    if (ioMonad) {
	fprintf(fp,"    heap(1);\n");
	fprintf(fp,"    updap(0,pair(mkCfun(2),%s),offset(1));\n",
		   showExtRet(ty));
    }
    else
	fprintf(fp,"    update(0,%s);\n",showExtRet(ty));

    fprintf(fp,"    ret();\n}\n");
}

/* --------------------------------------------------------------------------
 * Machine control:
 * ------------------------------------------------------------------------*/

Void machine(what)
Int what; {
    switch (what) {
	case RESET   : scDeps  = NIL;
		       break;

	case MARK    : mark(scDeps);
		       mark(shouldntFail);
		       mark(functionReturn);
		       mark(noAction);
		       break;

	case INSTALL : machine(RESET);
		       memory = (Memory)farCalloc(NUM_ADDRS,sizeof(MemCell));
		       if (memory==0)
			   fatal("Cannot allocate program memory");

		       shouldntFail   = pair(mkInt(0),ERRCONT);
		       functionReturn = pair(mkInt(0),UPDRETC);
		       noAction	      = pair(mkInt(0),RUNONC);
		       break;
    }
}

/* ------------------------------------------------------------------------*/
