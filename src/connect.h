/* --------------------------------------------------------------------------
 * connect.h:	Copyright (c) Mark P Jones 1991-1994.   All rights reserved.
 *		See goferite.h for details and conditions of use etc...
 *		Gofer version 2.30 March 1994
 *
 * Connections between components of the Gofer system
 * ------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 * Standard data:
 * ------------------------------------------------------------------------*/

extern Name  nameFalse,   nameTrue;	/* primitive constructor functions */
extern Name  nameNil,     nameCons;
extern Name  nameFatbar,  nameFail;	/* primitives reqd for translation */
extern Name  nameIf,      nameSel;
extern Name  nameMinus,	  nameDivide;
extern Name  nameUndefMem;	 	/* undefined member primitive	   */
extern Name  nameError;			/* for runtime error messages	   */
extern Name  nameBlackHole;		/* for GC-detected black hole	   */
extern Name  nameAnd,	  nameOr;	/* for optimisation of && and ||   */
extern Name  nameOtherwise;

extern Name  nameResult,  nameBind;	/* for translation of monad comps  */
extern Name  nameZero;

extern Name  namePrint;			/* printing primitive		   */

#if IO_DIALOGUE
extern Name  nameReadFile,   nameWriteFile;/* I/O name primitives	   */
extern Name  nameAppendFile, nameReadChan;
extern Name  nameAppendChan, nameEcho;
extern Name  nameGetArgs,    nameGetProgName;
extern Name  nameGetEnv;
extern Name  nameSuccess,    nameStr;
extern Name  nameFailure,    nameStrList;
extern Name  nameWriteError;
extern Name  nameReadError,  nameSearchError;
extern Name  nameFormatError,nameOtherError;
#endif
#if    IO_MONAD
extern Type   typeIO, typeProgIO;	/* for the IO monad, IO and IO ()  */
extern Type   typeWorld, typeST;	/* built on top of IO = ST World   */
extern Void   ioExecute Args((Cell));	/* IO monad executor		   */
extern Name   nameSTRun;		/* encapsulator			   */
extern Type   typeMutVar;		/* type constr for mutable vars	   */
#if    HASKELL_ARRAYS
extern Type   typeMutArr;		/* type constr for mutable arrays  */
#endif
#endif
#ifdef LAMBDAVAR
extern Name  nameVar;			/* internal lambda var constructor */
extern Type  typeProg;			/* type of a lambda var program	   */
extern Void  lvExecute	Args((Cell));	/* lambda var executor		   */
#endif
#ifdef LAMBDANU
extern Name nameTag;			/* internal lambda nu constructor  */
extern Type typeLnProg;			/* type of a lambda nu prog	   */
extern Void lnExecute   Args((Cell));	/* Lambda nu executor		   */
#endif
#if    HASKELL_ARRAYS
extern Type typeArray;			/* type constr for arrays	   */
#endif

extern Text  textPlus,    textMult;	/* used to recognise n+k/c*n pats  */

extern String repeatStr;		/* repeat last command string	   */

extern Type  typeString;		/* String  == [Char]		   */
extern Type  typeDialogue;		/* Dialogue== [Response]->[Request]*/
extern Type  typeBool;
extern Type  typeInt;
extern Type  typeFloat;
extern Type  typeChar;		

extern Cell  *CStackBase;		/* pointer to base of C stack	   */

extern List  tyconDefns;		/* list of type constructor defns  */
extern List  typeInDefns;		/* list of synonym restrictions	   */
extern List  valDefns;			/* list of value definitions       */
extern List  opDefns;			/* list of operator definitions    */
extern List  classDefns;		/* list of class definitions       */
extern List  instDefns;			/* list of instance definitions    */
extern List  overDefns;			/* list of overloaded member defns */
extern List  primDefns;			/* list of primitive definitions   */
extern Cell  inputExpr;			/* evaluator input expression      */
extern Addr  inputCode;			/* Code for compiled input expr    */

extern Int   whnfArgs;			/* number of args of term in whnf  */
extern Cell  whnfHead;			/* head of term in whnf            */
extern Int   whnfInt;			/* integer value of term in whnf   */
extern Float whnfFloat;			/* float value of term in whnf	   */
extern Long  numReductions;		/* number of reductions used       */
extern Long  numCells;			/* number of cells allocated       */
extern Int   numberGcs;			/* number of garbage collections   */
extern Int   cellsRecovered;		/* cells recovered by last gc	   */

extern Bool  gcMessages;		/* TRUE => print GC messages	   */
extern Bool  literateScripts;		/* TRUE => default lit scripts     */
extern Bool  literateErrors;		/* TRUE => report errs in lit scrs */
extern Bool  useConformality;		/* TRUE => check patbind conform'ty*/
extern Bool  anyEvidence;		/* TRUE => don't search for best ev*/
extern Bool  coerceNumLiterals;		/* TRUE => insert fromInteger calls*/
					/*	   etc for numeric literals*/
extern Bool  andorOptimise;		/* TRUE => optimise uses of &&, || */
extern Bool  showDicts;			/* TRUE => show dictionary params  */
					/*	   in output expressions   */
extern Bool  catchAmbigs;		/* TRUE => functions with ambig.   */
					/*	   types produce error	   */
extern Bool  failOnError;		/* TRUE => error produces immediate*/
					/*	   termination		   */

extern Int   maxEvidLevel;		/* maximum no of selects in evid   */
extern Bool  silentEvFail;		/* TRUE => keep quiet if maxEvidLev*/
					/* 	   is exceeded.		   */

extern Bool  kindExpert;		/* TRUE => display kind errors in  */
					/* 	   full detail		   */
extern Bool  overSingleton;		/* TRUE => overload singleton list */
					/*	   notation, [x]	   */

/* --------------------------------------------------------------------------
 * Function prototypes etc...
 * ------------------------------------------------------------------------*/

extern Void   everybody        Args((Int));

#define RESET   1		/* reset subsystem                         */
#define MARK    2		/* mark parts of graph in use by subsystem */
#define INSTALL 3		/* install subsystem (executed once only)  */
#define EXIT	4		/* Take action immediately before exit()   */
#define BREAK   5		/* Take action after program break	   */
#define PRELUDE 6		/* Init. once prelude Tycons/Classes known */

typedef long   Target;
extern  Void   setGoal          Args((String, Target));
extern  Void   soFar            Args((Target));
extern  Void   done             Args((Void));
extern  String fromEnv		Args((String,String));

extern  Void   storage          Args((Int));
extern  Void   setLastExpr	Args((Cell));
extern  Cell   getLastExpr	Args((Void));
extern	List   addNamesMatching Args((String,List));

extern  Void   input            Args((Int));
extern  Void   consoleInput     Args((String));
extern  Void   projInput	Args((String));
extern  Void   parseScript      Args((String,Long));
extern  Void   parseExp         Args((Void));
extern  String readFilename     Args((Void));
extern  String readLine		Args((Void));
extern  Syntax defaultSyntax    Args((Text));
extern  String unlexChar        Args((Char,Char));

extern  Void   staticAnalysis	Args((Int));
extern  Void   tyconDefn	Args((Int,Cell,Cell,Cell));
extern  Void   setTypeIns	Args((List));
extern  Void   clearTypeIns	Args((Void));
extern  Bool   isAmbiguous	Args((Type));
extern  Void   ambigError	Args((Int,String,Cell,Type));
extern  Void   classDefn	Args((Int,Cell,Cell));
extern  Void   instDefn		Args((Int,Cell,Cell));
extern  Void   primDefn		Args((Cell,List,Cell));
extern  Void   checkExp		Args((Void));
extern  Void   checkDefns	Args((Void));

extern  Void   typeChecker	Args((Int));
extern  Type   typeCheckExp	Args((Void));
extern  Void   typeCheckDefns	Args((Void));
extern  Void   insertInst	Args((Int,Class,Inst));
extern  Cell   rhsExpr		Args((Cell));
extern  Int    rhsLine		Args((Cell));
extern  Bool   typeMatches	Args((Type,Type));
extern  Bool   typeInstOf	Args((Type,Type));
extern  Dict   listMonadDict	Args((Void));

extern  Void   kindTCGroup	Args((List));
extern  Void   kindSigType	Args((Int,Type));
extern  Void   kindInst		Args((Inst,Int));

extern  Void   compiler         Args((Cell));
extern  Void   compileDefns     Args((Void));
extern  Void   compileExp       Args((Void));
extern  Bool   refutable	Args((Cell));
extern  Int    discrArity       Args((Cell));

extern  Void   machine          Args((Int));
extern  Addr   codeGen          Args((Name,Int,Cell));
extern  Void   externalPrim	Args((Name,String));
extern  Void   unwind           Args((Cell));
extern  Void   eval             Args((Cell));
extern  Cell   evalWithNoError  Args((Cell));
extern  Void   evalFails        Args((StackPtr));
extern  Cell   graphForExp	Args((Void));

extern  Void   builtIn          Args((Int));
extern  Void   abandon		Args((String,Cell));
extern  Cell   outputString	Args((FILE *,Cell));
extern  Void   dialogue		Args((Cell));
extern  Cell   consChar		Args((Char));

extern  Void   machdep          Args((Int));
extern  String timeString	Args((Void));
extern  Int    shellEsc		Args((String));
extern  Int    getTerminalWidth Args((Void));
extern  Void   normalTerminal	Args((Void));
extern  Void   noechoTerminal	Args((Void));
extern  Int    readTerminalChar Args((Void));
extern  Void   gcStarted	Args((Void));
extern  Void   gcScanning	Args((Void));
extern  Void   gcRecovered	Args((Int));
extern  Void   gcCStack		Args((Void));

/*-------------------------------------------------------------------------*/
