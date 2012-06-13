/* --------------------------------------------------------------------------
 * twospace.c:  Copyright (c) Mark P Jones 1991-1994.   All rights reserved.
 *              See goferite.h for details and conditions of use etc...
 *              Gofer version 2.30 March 1994
 *
 * Two space copying GC, optionally used for gofc runtime system.
 * ------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 * Simple twospace copying collector:
 * ------------------------------------------------------------------------*/

static Void   heapInit		Args((Void));
static Void   markPhase		Args((Void));
static Cell   forward		Args((Cell));
static Cell   copyCell		Args((Cell));
static Cell   copyArray		Args((Cell));

Int     heapSize = DEFAULTHEAP;		/* number of cells in heap	   */
static  Heap space1,space2;		/* the two heap spaces		   */
#ifndef GLOBALcar
Heap	from;				/* (top of) current from space	   */
#endif
static  Heap to;			/* (top of) current to space	   */
#ifndef GLOBALcdr
Cell	hp;				/* current heap pointer		   */
#endif
static  Bool fileUsed[NUM_FILES];	/* file in use flags		   */
#define mark(c)  c=forward(c)		/* mark graph and save new pointer */

static Void heapInit() {		/* initialise heap storage	   */
    space1 = (Heap)(farCalloc(heapSize,sizeof(Cell)));
    space2 = (Heap)(farCalloc(heapSize,sizeof(Cell)));
    if (space1==(Heap)0 || space2==(Heap)0)
	abandon("Cannot allocate heap storage");
    from = space1 + heapSize;
    to   = space2 + heapSize;
    hp   = -heapSize-1;
}

#if  !INLINE_ALLOC			/* allocation (not inlined) gives  */
Cell pair(l,r)				/* smaller object code size, but   */
Cell l,r; {				/* with a small hit on speed.	   */
    from[++hp] = l;
    from[++hp] = r;
    return (hp-1);
}
#endif

Void garbageCollect() {			/* garbage collector		   */
    register Cell toIn = -heapSize-1;
    Heap swap;
    Int  i;
    hp = toIn;

    for (i=0; i<NUM_FILES; ++i)		/* assume all files need collection*/
	fileUsed[i] = FALSE;

    markPhase();			/* mark all cells in use	   */

    while (toIn<hp) {			/* use tospace as a queue to copy  */
	Cell tag = to[++toIn];		/* the whole graph currently in use*/

	if (tag==ARRAY) {		/* must be array		   */
	    Int n = to[++toIn];	
	    for (; n>0; --n) {
		++toIn;
		to[toIn] = forward(to[toIn]);
	    }
	}
	else if (isPair(tag)) {		/* must be application node	   */
	    to[toIn] = forward(tag);
	    ++toIn;			/* to another cell		   */
	    to[toIn] = forward(to[toIn]);
	}
        else if (tag>MAXBOXTAG) {	/* application of unboxed value	   */
	    ++toIn;			/* to another cell		   */
	    to[toIn] = forward(to[toIn]);
	}
	else				/* tagged cell - value already	   */
	    ++toIn;			/* 		 copied across	   */
    }
    if (hp+1000>=0)
	abandon("Garbage collection fails to reclaim sufficient space");

    for (i=0; i<NUM_FILES; ++i)		/* close all files no longer in use*/
	if (!fileUsed[i])
	    closeFile(i);

    swap = from;			/* exchange tospace and fromspace  */
    from = to;
    to   = swap;
}

static Void markPhase() {		/* mark phase of garbage collector */
    StackPtr sp1;
    Int	     i;

    stackLoop(sp1)			/* mark nodes on stack		   */
	mark(*sp1);
    for (i=0; i<num_scs; i++)		/* mark supercombinator nodes	   */
	mark(sc[i]);
    for (i=0; i<num_dicts; i++)		/* mark dictionary entries	   */
	mark(dict[i]);
    for (i=0; i<NUM_CHARS; ++i)		/* mark character conses	   */
	mark(consCharArray[i]);
    mark(resps);			/* mark responses		   */
    primMark();				/* mark primitives		   */
}

static Cell forward(c)			/* find forwarding location of cell*/
Cell c; {
    if (isPair(c)) {			/* only pairs need be forwarded	   */
	Cell tag = fst(c);
	if (tag==INDIRECT) {		/* short out indirection nodes	   */
	    Cell back = mkCfun(0);
	    do {
		fst(c) = INDIRECT1;
		tag    = snd(c);
		snd(c) = back;
		back   = c;
		c      = tag;
		if (!isPair(c))
		    break;
		tag    = fst(c);
	    } while (tag==INDIRECT);

	    if (isPair(c)) {		/* a specialised form of forward() */
		if (tag==INDIRECT1)
		    c = primBlackHole;
		else if (tag==FORWARD)
		    c = snd(c);
		else if (tag==ARRAY)
		    c = copyArray(c);
		else {
		    if (tag==FILECELL)
			fileUsed[snd(c)] = TRUE;
		    c = copyCell(c);
		}
	    }
	
	    while (isPair(back)) {	/* update all indirections	   */
		tag	  = snd(back);
		fst(back) = FORWARD;
		snd(back) = c;
		back	  = tag;
	    }
	    return c;
	}
	else if (tag==FORWARD)		/* previously forwarded cell	   */
	    return snd(c);
        else if (tag==ARRAY)		/* array to be forwarded	   */
	    return copyArray(c);
	else {				/* pair to be forwarded		   */
	    if (tag==FILECELL)
		fileUsed[snd(c)] = TRUE;
	    return copyCell(c);
	}
    }
    return c;
}

static Cell copyCell(c)			/* copy pair fromspace --> tospace */
Cell c; {
#if SMALL_GOFER
    to[++hp]  = from[c];		/* not worth the trouble of a reg  */
    from[c]   = FORWARD;		/* assignment for far pointers	   */
    to[++hp]  = from[c+1];
    from[c+1] = hp-1;
    return hp-1;
#else
    register Cell *fp = from+c;
    to[++hp] = *fp;			/* don't need to check for heap	   */
    *fp++    = FORWARD;			/* overflow since no halfspace can */
    to[++hp] = *fp;			/* be bigger that the other!	   */
    return (*fp = hp-1);
#endif
}

static Cell copyArray(c)		/* copy array fromspace --> tospace*/
Cell c; {
    Cell fc;
    Int  n;
#if SMALL_GOFER
    to[++hp]  = ARRAY;			/* not worth the trouble of a reg  */
    from[c]   = FORWARD;		/* assignment for far pointers	   */
    fc        = hp;
    n         =				/* get length of array		   */
    to[++hp]  = from[c+1];
    from[c+1] = fc;
    while (n-- > 0)
	to[++hp] = from[c+1+n];
#else
    register Cell *fp = from+c;
    to[++hp] = ARRAY;			/* don't need to check for heap	   */
    *fp++    = FORWARD;			/* overflow since no halfspace can */
    fc       = hp;			/* be bigger that the other!	   */
    n        =
    to[++hp] = *fp;
    *fp++    = fc;
    while (n-- > 0)
	to[++hp] = *fp++;
#endif
    return fc;
}

/* --------------------------------------------------------------------------
 * Arrays (implemented by contiguous arrays of cells):
 * ------------------------------------------------------------------------*/

#if HASKELL_ARRAYS
Void allocArray(n,bds,z)		/* allocate array of cells	   */
Int  n;					/* n = length of array (assume>=0) */
Cell bds;				/* bds = bounds			   */
Cell z; {				/* z = default value		   */
    heap((n+3));			/* currently checks for twice the  */
					/* actual requirement ...	   */
    from[++hp] = ARRAY;
    push(hp);
    from[++hp] = n+1;
    from[++hp] = bds;
    for (; n>0; --n)
	from[++hp] = z;
}

Void dupArray(a)			/* duplicate array		   */
Cell a; {
    Int n = from[++a];
    heap((n+2));			/* also checks for twice as much   */
					/* as is strictly necessary	   */
    from[++hp] = ARRAY;
    push(hp);
    from[++hp] = n;
    for (; n>0; --n)
	from[++hp] = from[++a];
}

#endif

/*-------------------------------------------------------------------------*/
