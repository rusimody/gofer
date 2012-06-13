/* --------------------------------------------------------------------------
 * machine.c:   Copyright (c) Mark P Jones 1991-1994.   All rights reserved.
 *              See goferite.h for details and conditions of use etc...
 *              Gofer version 2.30 March 1994
 *
 * Graph reduction engine, code generation and execution
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include <setjmp.h>

/*#define DEBUG_CODE*/
Bool   andorOptimise = TRUE;		/* TRUE => optimise uses of &&, || */
Bool   failOnError   = TRUE;		/* TRUE => abort as soon as error  */
					/*	   occurs		   */

/* --------------------------------------------------------------------------
 * Data structures for machine memory (program storage):
 * ------------------------------------------------------------------------*/

/* This list defines the sequence of all instructions that can be used in
 * the abstract machine code for Gofer.  The Ins() macro is used to
 * ensure that the correct mapping of instructions to labels is used when
 * compiling the GCC_THREADED version.
 */
#if NPLUSK
#define INSTRLIST	Ins(iLOAD),  Ins(iCELL),   Ins(iCHAR),	  \
			Ins(iINT),   Ins(iFLOAT),  Ins(iSTRING),  \
			Ins(iMKAP),  Ins(iUPDATE), Ins(iUPDAP),	  \
			Ins(iEVAL),  Ins(iRETURN), Ins(iINTGE),   \
			Ins(iINTEQ), Ins(iINTDV),  Ins(iTEST),	  \
			Ins(iGOTO),  Ins(iSETSTK), Ins(iALLOC),	  \
			Ins(iSLIDE), Ins(iROOT),   Ins(iDICT),	  \
			Ins(iFAIL)
#else
#define INSTRLIST	Ins(iLOAD),  Ins(iCELL),   Ins(iCHAR),	  \
			Ins(iINT),   Ins(iFLOAT),  Ins(iSTRING),  \
			Ins(iMKAP),  Ins(iUPDATE), Ins(iUPDAP),	  \
			Ins(iEVAL),  Ins(iRETURN), Ins(iINTEQ),	  \
			Ins(iTEST),  Ins(iGOTO),   Ins(iSETSTK),  \
			Ins(iALLOC), Ins(iSLIDE),  Ins(iROOT),    \
			Ins(iDICT),  Ins(iFAIL)
#endif
  
#define Ins(x) x
typedef enum { INSTRLIST } Instr;
#undef  Ins

typedef Int Label;

typedef union {
    Int   mint;
#if !BREAK_FLOATS
    Float mfloat;
#endif
    Cell  cell;
    Text  text;
    Addr  addr;
    Instr instr;
    Label lab;
} MemCell;

typedef MemCell far *Memory;
static	Memory	    memory;
#define intAt(m)    memory[m].mint
#if !BREAK_FLOATS
#define floatAt(m)  memory[m].mfloat
#endif
#define cellAt(m)   memory[m].cell
#define textAt(m)   memory[m].text
#define addrAt(m)   memory[m].addr
#define instrAt(m)  memory[m].instr
#define labAt(m)    memory[m].lab

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static Void  local instrNone	Args((Instr));
static Void  local instrInt	Args((Instr,Int));
static Void  local instrFloat   Args((Instr,FloatPro));
static Void  local instrCell	Args((Instr,Cell));
static Void  local instrText	Args((Instr,Text));
static Void  local instrLab	Args((Instr,Label));
static Void  local instrIntLab	Args((Instr,Int,Label));
static Void  local instrCellLab Args((Instr,Cell,Label));

static Void  local asSTART	Args((Void));
static Label local newLabel	Args((Label));
static Void  local asEND	Args((Void));
static Void  local asDICT	Args((Int));
static Void  local asSLIDE	Args((Int));
static Void  local asMKAP	Args((Int));
static Void  local asUPDATE	Args((Int));
static Void  local asGOTO	Args((Label));

#ifdef DEBUG_CODE
static Void  local dissassemble Args((Addr,Addr));
static Void  local printCell	Args((Cell));
static Addr  local dissNone	Args((Addr,String));
static Addr  local dissInt	Args((Addr,String));
static Addr  local dissFloat    Args((Addr,String));
static Addr  local dissCell	Args((Addr,String));
static Addr  local dissText	Args((Addr,String));
static Addr  local dissAddr	Args((Addr,String));
static Addr  local dissIntAddr	Args((Addr,String));
static Addr  local dissCellAddr Args((Addr,String));
#endif

static Void  local build	Args((Cell,Int));
static Void  local buildGuards	Args((List,Int));
static Int   local buildLoc	Args((List,Int));

static Void  local make 	Args((Cell,Int,Label,Label));
static Void  local makeCond	Args((Cell,Cell,Cell,Int,Label,Label));
static Void  local testGuard	Args((Pair,Int,Label,Label,Label));
static Void  local testCase	Args((Pair,Int,Label,Label,Label));

static Void  local analyseAp	Args((Cell));
static Void  local buildAp	Args((Cell,Int,Label,Bool));

static Void  local evalString   Args((Cell));
static Void  local run		Args((Addr,StackPtr));

/* --------------------------------------------------------------------------
 * Assembler: (Low level, instruction code storage)
 * ------------------------------------------------------------------------*/

static Addr  startInstr;		/* first instruction after START   */
static Addr  lastInstr;			/* last instr written (for peephole*/
					/* optimisations etc.)		   */
static Addr  noMatch;			/* address of a single FAIL instr  */
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
Instr    opc;
FloatPro fl; {
#if BREAK_FLOATS
    lastInstr		 = getMem(3);
    instrAt(lastInstr)	 = opc;
    cellAt(lastInstr+1)	 = part1Float(fl);
    cellAt(lastInstr+2)  = part2Float(fl);
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
 *
 * Labels are used as a simple form of continuation during the code gen:
 *  RUNON    => produce code which does not make jump at end of construction
 *  UPDRET   => produce code which performs UPDATE 0, RETURN at end
 *  VALRET   => produce code which performs RETURN at end
 *  other(d) => produce code which branches to label d at end
 * ------------------------------------------------------------------------*/

static	Label	      nextLab;	       /* next label number to allocate    */
#define SHOULDNTFAIL  (-1)
#define RUNON	      (-2)
#define UPDRET	      (-3)
#define VALRET	      (-4)
static	Addr	      fixups[NUM_FIXUPS]; /* fixup table maps Label -> Addr*/
#define atLabel(n)    fixups[n] = getMem(0)
#define endLabel(d,l) if (d==RUNON) atLabel(l)
#define fix(a)	      addrAt(a) = fixups[labAt(a)]

static Void local asSTART() {	       /* initialise assembler		   */
    fixups[0]	= noMatch;
    nextLab	= 1;
    startInstr	= getMem(0);
    lastInstr	= startInstr-1;
    srsp	= 0;
    offsPosn[0] = 0;
}

static Label local newLabel(d)	       /* allocate new label		   */
Label d; {
    if (d==RUNON) {
	if (nextLab>=NUM_FIXUPS) {
	    ERROR(0) "Compiled code too complex"
	    EEND;
	}
	return nextLab++;
    }
    return d;
}

static Void local asEND() {	       /* Fix addresses in assembled code  */
    Addr pc = startInstr;

    while (pc<=lastInstr)
	switch (instrAt(pc)) {
	    case iEVAL	 :	       /* opcodes taking no arguments	   */
	    case iFAIL	 :
	    case iRETURN : pc++;
			   break;

	    case iGOTO	 : fix(pc+1);  /* opcodes taking one argument	   */
	    case iSETSTK :
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

	    case iINTEQ  :	       /* opcodes taking two arguments	   */
#if NPLUSK
	    case iINTGE  :
	    case iINTDV	 :
#endif
	    case iTEST	 : fix(pc+2);
			   pc+=3;
			   break;

	    default	 : internal("fixAddrs");
	}
}

/* --------------------------------------------------------------------------
 * Assembler Opcodes: (includes simple peephole optimisations)
 * ------------------------------------------------------------------------*/

#define asINTEGER(n) instrInt(iINT,n);		srsp++
#define asFLOAT(fl)  instrFloat(iFLOAT,fl);	srsp++
#define asSTRING(t)  instrText(iSTRING,t);	srsp++
#define asCHAR(n)    instrInt(iCHAR,n);		srsp++
#define asLOAD(n)    instrInt(iLOAD,n);		srsp++
#define asALLOC(n)   instrInt(iALLOC,n);	srsp+=n
#define asROOT(n)    instrInt(iROOT,n);		srsp++
#define asSETSTK(n)  instrInt(iSETSTK,n);	srsp=n
#define asEVAL()     instrNone(iEVAL);		srsp--	/* inaccurate srsp */
#define asRETURN()   instrNone(iRETURN)
#define asCELL(c)    instrCell(iCELL,c);	srsp++
#define asTEST(c,l)  instrCellLab(iTEST,c,l)		/* inaccurate srsp */
#define asINTEQ(n,l) instrIntLab(iINTEQ,n,l)
#if NPLUSK
#define asINTGE(n,l) instrIntLab(iINTGE,n,l)		/* inaccurate srsp */
#define asINTDV(n,l) instrIntLab(iINTDV,n,l)		/* inaccurate srsp */
#endif
#define asFAIL()     instrNone(iFAIL)

static Void local asDICT(n)		/* pick element of dictionary	   */
Int n; {
/* Sadly, the following optimisation cannot be used unless CELL references
 * in compiled code are garbage collected (and possibly modified when cell  
 * indirections are found).
 *
 *    if (instrAt(lastInstr)==iCELL)
 *	-- Peephole optimisation: CELL {dict m};DICT n ==> CELL dict(m+n)
 *	if (whatIs(cellAt(lastInstr+1))==DICTCELL)
 *	    cellAt(lastInstr+1) = dict(dictOf(cellAt(lastInstr+1))+n);
 *	else
 *	    internal("asDICT");
 *    else  ...
 */
    if (n!=0)				/* optimisation:DICT 0 has no use  */
	instrInt(iDICT,n);		/* for std dictionary construction */
}

static Void local asSLIDE(n)		/* Slide results down stack	   */
Int n; {
    if (instrAt(lastInstr)==iSLIDE)	/* Peephole optimisation:	   */
	intAt(lastInstr+1)+=n;		/* SLIDE n;SLIDE m ===> SLIDE (n+m)*/
    else
	instrInt(iSLIDE,n);
    srsp -= n;
}

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

static Void local asGOTO(l)		/* End evaluation of expr in manner*/
Label l; {				/* indicated by label l		   */
    switch (l) {					/* inaccurate srsp */
	case UPDRET : asUPDATE(0);
	case VALRET : asRETURN();
	case RUNON  : break;
	default     : instrLab(iGOTO,l);
		      break;
    }
}

/* --------------------------------------------------------------------------
 * Dissassembler:
 * ------------------------------------------------------------------------*/

#ifdef DEBUG_CODE
#define printAddr(a) printf("0x%04X",a)/* printable representation of Addr */

static Void local dissassemble(pc,end) /* print dissassembly of code	   */
Addr pc;
Addr end; {
    while (pc<=end) {
	printAddr(pc);
	printf("\t");
	switch (instrAt(pc)) {
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
	    case iRETURN : pc = dissNone(pc,"RETURN");	 break;
	    case iINTEQ  : pc = dissIntAddr(pc,"INTEQ"); break;
#if NPLUSK
	    case iINTGE  : pc = dissIntAddr(pc,"INTGE"); break;
	    case iINTDV  : pc = dissIntAddr(pc,"INTDV"); break;
#endif
	    case iTEST	 : pc = dissCellAddr(pc,"TEST"); break;
	    case iGOTO	 : pc = dissAddr(pc,"GOTO");	 break;
	    case iSETSTK : pc = dissInt(pc,"SETSTK");	 break;
	    case iALLOC  : pc = dissInt(pc,"ALLOC");	 break;
	    case iSLIDE  : pc = dissInt(pc,"SLIDE");	 break;
	    case iROOT	 : pc = dissInt(pc,"ROOT");	 break;
            case iDICT   : pc = dissInt(pc,"DICT");      break;
	    case iFAIL	 : pc = dissNone(pc,"FAIL");	 break;
	    default	 : internal("unknown instruction");
	}
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
    printf("%s\t%s\n",s,floatToString((FloatPro)floatAt(pc+1)));
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

static Addr local dissAddr(pc,s)       /* dissassemble instr with Addr arg */
Addr   pc;
String s; {
    printf("%s\t",s);
    printAddr(addrAt(pc+1));
    printf("\n");
    return pc+2;
}

static Addr local dissIntAddr(pc,s)    /* dissassemble instr with Int/Addr */
Addr   pc;
String s; {
    printf("%s\t%d\t",s,intAt(pc+1));
    printAddr(addrAt(pc+2));
    printf("\n");
    return pc+3;
}

static Addr local dissCellAddr(pc,s)   /* dissassemble instr with Cell/Addr*/
Addr   pc;
String s; {
    printf("%s\t",s);
    printCell(cellAt(pc+1));
    printf("\t");
    printAddr(addrAt(pc+2));
    printf("\n");
    return pc+3;
}
#endif

/* --------------------------------------------------------------------------
 * Compile expression to code which will build expression without any
 * evaluation.
 * ------------------------------------------------------------------------*/

static Void local build(e,co)		/* Generate code which will build  */
Cell e;					/* instance of given expression but*/
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

	case AP        : buildAp(e,co,SHOULDNTFAIL,FALSE);
		         break;

	case UNIT      :
	case TUPLE     :
	case NAME      : asCELL(e);
			 break;

	case DICTCELL  : asCELL(dict(dictOf(e)));	/* see comments for*/
			 break;				/* DICTCELL in make*/
							/* function below  */
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
 * Compile expression to code which will build expression evaluating guards
 * and testing cases to avoid building complete graph.
 * ------------------------------------------------------------------------*/

#define makeTests(ct,tests,co,f,d)     {   Label l1 = newLabel(d);	    \
					   List  xs = tests;		    \
					   while (nonNull(tl(xs))) {	    \
					       Label l2   = newLabel(RUNON);\
					       Int savesp = srsp;	    \
					       ct(hd(xs),co,f,l2,l1);	    \
					       atLabel(l2);		    \
					       srsp = savesp;		    \
					       xs   = tl(xs);		    \
					   }				    \
					   ct(hd(xs),co,f,f,d);		    \
					   endLabel(d,l1);		    \
				       }

static Void local make(e,co,f,d)       /* Construct code to build e, given */
Cell  e;			       /* current offset co, and branch	   */
Int   co;			       /* to f on failure, d on completion */
Label f;
Label d; {
    switch (whatIs(e)) {

	case LETREC    : {   Int n = buildLoc(fst(snd(e)),co);
			     make(snd(snd(e)),co+n,f,RUNON);
			     asSLIDE(n);
			     asGOTO(d);
		         }
		         break;

	case FATBAR    : {   Label l1     = newLabel(RUNON);
			     Label l2     = newLabel(d);
			     Int   savesp = srsp;

			     make(fst(snd(e)),co,l1,l2);

			     atLabel(l1);
			     srsp = savesp;
			     asSETSTK(srsp);
			     make(snd(snd(e)),co,f,l2);

			     endLabel(d,l2);
		         }
		         break;

	case COND      : makeCond(fst3(snd(e)),
				  snd3(snd(e)),
				  thd3(snd(e)),co,f,d);
		         break;

	case CASE      : make(fst(snd(e)),co,SHOULDNTFAIL,RUNON);
		         asEVAL();
		         makeTests(testCase,snd(snd(e)),co,f,d);
		         break;

	case GUARDED   : makeTests(testGuard,snd(e),co,f,d);
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
                         asGOTO(d);
                         break;

	case UNIT      :
	case TUPLE     :
	case NAME      : asCELL(e);
		         asGOTO(d);
		         break;

	/* for dict cells, ensure that CELL referred to in the code is the */
	/* dictionary cell at the head of the dictionary; not just a copy  */

	case DICTCELL  : asCELL(dict(dictOf(e)));
		         asGOTO(d);
		         break;

	case INTCELL   : asINTEGER(intOf(e));
		         asGOTO(d);
		         break;

        case FLOATCELL : asFLOAT(floatOf(e));
		         asGOTO(d);
			 break;

	case STRCELL   : asSTRING(textOf(e));
		         asGOTO(d);
		         break;

	case CHARCELL  : asCHAR(charOf(e));
		         asGOTO(d);
		         break;

	case OFFSET    : asLOAD(offsPosn[offsetOf(e)]);
		         asGOTO(d);
		         break;

	default        : internal("make");
    }
}

static Void local makeCond(i,t,e,co,f,d)/* Build code for conditional	   */
Cell  i,t,e;
Int   co;
Label f;
Label d; {
    Label l1 = newLabel(RUNON);
    Label l2 = newLabel(d);
    Int   savesp;

    make(i,co,f,RUNON);
    asEVAL();

    savesp = srsp;
    asTEST(nameTrue,l1);
    make(t,co,f,l2);

    srsp = savesp;
    atLabel(l1);
    make(e,co,f,l2);

    endLabel(d,l2);
}

static Void local testGuard(g,co,f,cf,d)/* Produce code for guard	   */
Pair  g;
Int   co;
Label f;
Label cf;
Label d; {
    make(fst(g),co,SHOULDNTFAIL,RUNON);
    asEVAL();
    asTEST(nameTrue,cf);
    make(snd(g),co,f,d);
}

static Void local testCase(c,co,f,cf,d) /* Produce code for guard	   */
Pair  c;
Int   co;				/* labels determine where to go if:*/
Label f;				/* match succeeds, but rest fails  */
Label cf;				/* this match fails		   */
Label d; {
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
	case UNIT    :			/* typing guarantees that tags will*/
	case TUPLE   : break;		/* match without further tests	   */
	default      : asTEST(fst(c),cf);
		       break;
    }
    for (i=1; i<=n; i++)
	offsPosn[co+i] = ++srsp;
    make(snd(c),co+n,f,d);
}

/* --------------------------------------------------------------------------
 * We frequently encounter functions which call themselves recursively with
 * a number of initial arguments preserved:
 * e.g.  (map f) []	= []
 *	 (map f) (x:xs) = f x : (map f) xs
 * Lambda lifting, in particular, is likely to introduce such functions.
 * Rather than reconstructing a new instance of the recursive function and
 * its arguments, we can extract the relevant portion of the root of the
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
		    make(e,co,f,RUNON);
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
    definingName  = n;
    definingArity = arity;
    asSTART();
    if (nonNull(n)) {
        Int i;
        for (i=1; i<=arity; i++)
	    offsPosn[i] = ++srsp;
        make(e,arity,noMatch,(arity>0 ? UPDRET : VALRET));
    }
    else {
        build(e,0);
        asRETURN();
    }
    asEND();
#ifdef DEBUG_CODE
    if (nonNull(n))
	printf("name=%s\n",textToStr(name(n).text));
    dissassemble(startInstr,lastInstr);
    printf("------------------\n");
#endif
    if (nonNull(n))
	name(n).defn  = NIL;
    return startInstr;
}

Void externalPrim(n,s)		/* Add name n as an external primitive;	   */
Name   n;			/* This is not currently implemented in	   */
String s; {			/* the current version of the interpreter  */
    ERROR(name(n).line) "Unknown primitive reference \"%s\"", s
    EEND;
}

/* --------------------------------------------------------------------------
 * Evaluator:
 * ------------------------------------------------------------------------*/

Int   whnfArgs;			       /* number of arguments of whnf term */
Cell  whnfHead;			       /* head cell of term in whnf	   */
Int   whnfInt;			       /* value of INTCELL (in whnf)	   */
Float whnfFloat;		       /* value of FLOATCELL (in whnf)     */
Long  numReductions;		       /* number of reductions counted	   */

static Cell    errorRedex;	       /* irreducible error expression	   */
static jmp_buf *evalError = 0;	       /* jump buffer for eval errors	   */

Void eval(n)				   /* Graph reduction evaluator    */
Cell n; {
    StackPtr base = sp;
    Int      ar;

unw:switch (whatIs(n)) {		   /* unwind spine of application  */

	case AP        : push(n);
			 n = fun(n);
			 goto unw;

	case INDIRECT  : n = arg(n);
			 allowBreak();
			 goto unw;

	case NAME      : ar = name(n).arity;
			 if (name(n).defn!=CFUN && sp-base>=ar) {
			     allowBreak();
			     if (ar>0) { 		    /* fn with args*/
				 StackPtr root;

				 push(NIL);		    /* rearrange   */
				 root = sp;
				 do {
				     stack(root) = arg(stack(root-1));
				     --root;
				 } while (--ar>0);

				 if (name(n).primDef)	    /* reduce	   */
				     (*name(n).primDef)(root);
				 else
				     run(name(n).code,root);

				 numReductions++;

				 sp = root;		    /* continue... */
				 n  = pop();
			     }
			     else {			    /* CAF	   */
				 if (isNull(name(n).defn)) {/* build CAF   */
				     push(n);		    /* save CAF    */

				     if (name(n).primDef)
					 (*name(n).primDef)(sp);
				     else
					 run(name(n).code,sp);

				     numReductions++;

				     name(n).defn = pop();
				     drop();		    /* drop CAF    */
				 }
				 n = name(n).defn;	    /*already built*/
				 if (sp>base)
				     fun(top()) = n;
			     }
			     goto unw;
			 }
			 break;

	case INTCELL   : whnfInt = intOf(n);
			 break;

        case FLOATCELL : whnfFloat = floatOf(n);
			 break;

	case STRCELL   : evalString(n);
			 goto unw;

	case FILECELL  : evalFile(n);
			 goto unw;
    }

    whnfHead = n;		       /* rearrange components of term on  */
    whnfArgs = sp - base;	       /* stack, now in whnf ...	   */
    for (ar=whnfArgs; ar>0; ar--) {
	fun(stack(base+ar)) = n;
	n		    = stack(base+ar);
	stack(base+ar)	    = arg(n);
    }
}

Void unwind(n)			       /* unwind spine of application;	   */
Cell n; {			       /* like eval except that we always  */
    whnfArgs = 0;		       /* treat the expression n as if it  */
				       /* were already in whnf. 	   */
unw:switch (whatIs(n)) {
	case AP        : push(arg(n));
			 whnfArgs++;
			 n = fun(n);
			 goto unw;

	case INDIRECT  : n = arg(n);
			 allowBreak();
			 goto unw;

	case INTCELL   : whnfInt = intOf(n);
			 break;

        case FLOATCELL : whnfFloat = floatOf(n);
			 break;

	case STRCELL   : evalString(n);
			 goto unw;
    }
    whnfHead = n;
}

static Void local evalString(n)		/* expand STRCELL at node n	   */
Cell n; {
    Text t = textOf(n);
    Int  c = textToStr(t)[0];
    if (c==0) {
	fst(n) = INDIRECT;
	snd(n) = nameNil;
	return;
    }
    else if (c=='\\') {
	c = textToStr(++t)[0];
        if (c!='\\')
	    c = 0;
    }
    fst(n) = consChar(c);
    snd(n) = mkStr(++t);
}

static Void local run(start,root)      /* execute code beginning at given  */
Addr	 start;			       /* address with local stack starting*/
StackPtr root; {		       /* at given root offset		   */
    register Memory pc = memory+start;

#if     GCC_THREADED
#define Ins(x)		&&l##x
static  void *labs[] = { INSTRLIST };
#undef  Ins
#define Case(x)		l##x
#define	Continue	goto *labs[(pc++)->instr]
#define	Dispatch	Continue;
#define EndDispatch
#else
#define Dispatch	for (;;) switch((pc++)->instr) {
#define	Case(x)		case x
#define	Continue	continue
#define EndDispatch	default : internal("illegal instruction"); \
				  break;			   \
			}
#endif

    Dispatch

	Case(iLOAD)   : push(stack(root+pc->mint));	 /* load from stack*/
			pc++;
			Continue;

	Case(iCELL)   : push(pc->cell);			 /* load const Cell*/
			pc++;
			Continue;

	Case(iCHAR)   : push(mkChar(pc->mint));		 /* load char const*/
			pc++;
			Continue;

	Case(iINT)    : push(mkInt(pc->mint));		 /* load int const */
			pc++;
			Continue;

#if BREAK_FLOATS
	Case(iFLOAT)  : push(mkFloat(floatFromParts	 /* load dbl const */
				(pc->cell,(pc+1)->cell)));
			pc+=2;
			Continue;
#else
	Case(iFLOAT)  : push(mkFloat(pc->mfloat));	 /* load float cnst*/
			pc++;
			Continue;
#endif

	Case(iSTRING) : push(mkStr(pc->text));		 /* load str const */
			pc++;
			Continue;

	Case(iMKAP)   : {   Cell t = pushed(0);		 /* make AP nodes  */
			    Int  i = pc->text;
			    while (0<i--) {
				drop();
				t=ap(t,pushed(0));
			    }
			    pushed(0)=t;
			}
			pc++;
			Continue;

	Case(iUPDATE) : {   Cell t = stack(root		/* update cell ...*/
					     + pc->mint);
			    fst(t) = INDIRECT;
			    snd(t) = pop();
			}
			pc++;
			Continue;

	Case(iUPDAP)  : {   Cell t = stack(root		 /* update AP node */
					     + pc->mint);
			    fst(t) = pop();
			    snd(t) = pop();
			}
			pc++;
			Continue;

	Case(iEVAL)   : eval(pop());			 /* evaluate top() */
			Continue;

	Case(iRETURN) : return;				 /* terminate	   */

	Case(iINTEQ)  : if (whnfInt==pc->mint)		 /* test integer ==*/
			    pc += 2;
			else
			    pc = memory + (pc+1)->addr;
			Continue;

#if NPLUSK
	Case(iINTGE)  : if (whnfInt>=pc->mint) {	 /* test integer >=*/
			    push(mkInt(whnfInt-pc->mint));
			    pc += 2;
			}
			else
			    pc = memory + (pc+1)->addr;
			Continue;

	Case(iINTDV)  : if (whnfInt>=0 &&		 /* test for mult  */
			    (whnfInt%(pc->mint)==0)) {
			    push(mkInt(whnfInt/(pc->mint)));
			    pc += 2;
			}
			else
			    pc = memory + (pc+1)->addr;
			Continue;
#endif

	Case(iTEST)   : if (whnfHead==pc->cell)		 /* test for cell  */
			    pc += 2;
			else
			    pc = memory + (pc+1)->addr;
			Continue;

	Case(iGOTO)   : pc = memory + pc->addr;		 /* goto label	   */
			Continue;

	Case(iSETSTK) : sp=root + pc->mint;	 	 /* set stack ptr  */
			pc++;
			Continue;

	Case(iALLOC)  : {   Int i = pc->mint;		 /* alloc loc vars */
			    chkStack(i);
			    while (0<i--)
				onto(ap(NIL,NIL));
			}
			pc++;
			Continue;

	Case(iDICT)   : top() = dict(dictOf(top()) + pc->mint);
			pc++;				 /* dict lookup    */
			Continue;

	Case(iROOT)   : {   Cell t = stack(root);	 /* partial root   */
			    Int  i = pc->mint;
			    while (fst(t)==INDIRECT) {
				allowBreak();
				t = arg(t);
			    }
			    while (0<i--) {
				t = fun(t);
				while (fst(t)==INDIRECT) {
				    allowBreak();
				    t = arg(t);
				}
			    }
			    push(t);
			}
			pc++;
			Continue;

	Case(iSLIDE)  : pushed(pc->mint) = top();	 /* remove loc vars*/
			sp -= pc->mint;
			pc++;
			Continue;

	Case(iFAIL)   : evalFails(root);		 /* cannot reduce  */
			return;/*NOT REACHED*/

    EndDispatch

#undef Dispatch
#undef Case
#undef Continue
#undef EndDispatch
}

Cell evalWithNoError(e) 	       /* Evaluate expression, returning   */
Cell e; {			       /* NIL if successful, irreducible   */
    Cell badRedex;		       /* expression if not...		   */
    jmp_buf *oldCatch = evalError;

#if JMPBUF_ARRAY
    jmp_buf catch[1];
    evalError = catch;
    if (setjmp(catch[0])==0) {
	eval(e);
	badRedex = NIL;
    }
    else
	badRedex = errorRedex;
#else
    jmp_buf catch;
    evalError = &catch;
    if (setjmp(catch)==0) {
        eval(e); 
	badRedex = NIL;
    }
    else
        badRedex = errorRedex;
#endif

    evalError = oldCatch;
    return badRedex;
}

Void evalFails(root)			/* Eval of current redex fails	   */
StackPtr root; {
    errorRedex = stack(root);		/* get error & bypass indirections */
    while (isPair(errorRedex) && fst(errorRedex)==INDIRECT)
	errorRedex = snd(errorRedex);

    if (failOnError)
	abandon("Program",errorRedex);
    else if (evalError)
	longjmp(*evalError,1);
    else
	internal("uncaught eval error");
}

Cell graphForExp() {			/* Build graph for expression to be*/
    clearStack();			/* reduced...			   */
    run(inputCode,sp);
    return pop();
}

/* --------------------------------------------------------------------------
 * Machine control:
 * ------------------------------------------------------------------------*/

Void machine(what)
Int what; {
    switch (what) {
	case INSTALL : machine(RESET);
		       memory  = (Memory)farCalloc(NUM_ADDRS,sizeof(MemCell));
		       if (memory==0)
			   fatal("Cannot allocate program memory");
		       instrNone(iFAIL);
		       noMatch = lastInstr;
		       break;
    }
}

/* ------------------------------------------------------------------------*/
