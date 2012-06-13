/* --------------------------------------------------------------------------
 * errors.h:    Copyright (c) Mark P Jones 1991-1994.   All rights reserved.
 *              See goferite.h for details and conditions of use etc...
 *              Gofer version 2.30 March 1994
 *
 * Error handling support functions
 * ------------------------------------------------------------------------*/

#define errorStream	 stdout
#define ERROR(l)         errHead(l);fprintf(errorStream,
#define EEND       	 ); errFail()
#define ETHEN		 );
#define ERRTEXT		 fprintf(errorStream,
#define ERREXPR(e)	 printExp(errorStream,e)
#define ERRTYPE(e)	 printType(errorStream,e)
#define ERRCONTEXT(qs)   printContext(errorStream,qs)
#define ERRPRED(pi)      printPred(errorStream,pi)
#define ERRKIND(k)	 printKind(errorStream,k)
#define ERRSIG(sig)	 printSig(errorStream,sig)

extern Void errHead      Args((Int));              /* in main.c            */
extern Void errFail      Args((Void));
extern Void errAbort	 Args((Void));

extern sigProto(breakHandler);

extern Bool breakOn      Args((Bool));		   /* in machdep.c	   */

extern Void printExp     Args((FILE *,Cell));      /* in output.c          */
extern Void printType    Args((FILE *,Cell));
extern Void printContext Args((FILE *,List));
extern Void printPred    Args((FILE *,Cell));
extern Void printKind	 Args((FILE *,Kind));
extern Void printSig	 Args((FILE *,Cell));

/*-------------------------------------------------------------------------*/
