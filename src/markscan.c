/* --------------------------------------------------------------------------
 * markscan.c:  Copyright (c) Mark P Jones 1991-1994.   All rights reserved.
 *              See goferite.h for details and conditions of use etc...
 *              Gofer version 2.30 March 1994
 *
 * Mark scan garbage collector, optionally used for gofc runtime system.
 * ------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 * Simple mark scan garbage collector based on the allocator and garbage
 * collector used in the full interpreter.  An important difference between
 * the two systems is that the Gofer compiler (i.e. this program) does not
 * use conservative garbage collection (there is no need to scan the C runtime
 * stack).  Obviously, this is a BIG improvement in terms of portability!
 * Another advantage is that the garbage collector given here can be
 * upgraded to use a more sophisticated algorithm (for example, some form
 * of compacting collector, possibly stop/copy), thus avoiding the need
 * for a free list and permitting extensions requiring variable length
 * cells (arrays perhaps?).  The basic reason for this flexibility is the
 * fact that any cell may now be relocated during garbage collection.
 * ------------------------------------------------------------------------*/

static Void   heapInit		Args((Void));
static Void   markPhase		Args((Void));
static Void   scanPhase		Args((Void));
static Cell   markCell		Args((Cell));
static Void   markSnd		Args((Cell));

Int     heapSize = DEFAULTHEAP;		/* number of cells in heap	   */
#ifndef GLOBALfst
Heap    heapTopFst;			/* tops of heap arrays		   */
#endif
#ifndef GLOBALsnd
Heap	heapTopSnd;
#endif
static  Heap heapFst, heapSnd;		/* bases of each heap array	   */
static  Cell freeList;			/* free list of unused cells	   */
static  Int *marks;			/* `Mark set' used during GC to	   */
static  Int marksSize;			/* flag visited (active) cells	   */
#define mark(c)  c=markCell(c)		/* mark graph and save new pointer */

static Void heapInit() {		/* initialise heap storage	   */
    Int i;

    heapFst = (Heap)(farCalloc(heapSize,sizeof(Cell)));
    heapSnd = (Heap)(farCalloc(heapSize,sizeof(Cell)));
    if (heapFst==(Heap)0 || heapSnd==(Heap)0)
	abandon("Cannot allocate heap storage");
    heapTopFst = heapFst + heapSize;
    heapTopSnd = heapSnd + heapSize;
    for (i=1; i<heapSize; ++i)
	snd(-i-1) = -i;
    snd(-1)   = mkCfun(0);
    freeList  = -heapSize;
    marksSize = bitArraySize(heapSize);
    if ((marks=(Int *)calloc(marksSize, sizeof(Int)))==0)
	abandon("Cannot allocate gc markspace");
}

Cell pair(l,r)				/* Allocate pair (l, r) from	   */
Cell l, r; {				/* heap, garbage collecting first  */
    Cell c = freeList;			/* if necessary ...		   */

    if (!isPair(c)) {
	markPhase();
	mark(l);
	mark(r);
	scanPhase();
	c = freeList;
    }
    freeList = snd(freeList);
    fst(c)   = l;
    snd(c)   = r;
    return c;
}

Void garbageCollect() {			/* garbage collector		   */
    markPhase();
    scanPhase();
}

static Void markPhase() {		/* mark phase of garbage collector */
    StackPtr sp1;
    Int	     i;

    for (i=0; i<marksSize; ++i)		/* initialise mark set to empty    */
	marks[i] = 0;
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

static Void scanPhase() {		/* scan phase of garbage collector */
    register Int mask  = 1;		/* scan heap and add unused cells  */
    register Int place = 0;		/* to the freeList		   */
    Int      recovered = 0;
    Int	     i,j=0;

    for (i=1; i<=heapSize; i++) {
	if ((marks[place] & mask) == 0) {
	    if (fst(-i)==FILECELL) {
		closeFile(snd(-i));
		fst(-i) = INTCELL;	/* turn file to something harmless */
	    }
	    snd(-i)  = freeList;
	    freeList = -i;
	    recovered++;
	}
	mask <<= 1;
	if (++j == bitsPerWord) {
	    place++;
	    mask = 1;
	    j    = 0;
	}
    }

    /* can only return if freeList is nonempty on return. */
    if (recovered<minRecovery || !isPair(freeList))
	abandon("Garbage collection fails to reclaim sufficient space");
}

static Cell markCell(c)			/* Traverse part of graph marking  */
Cell c; {				/* cells reachable from given root */

mc: if (!isPair(c))
	return c;

    if (fst(c)==INDIRECT) {
	c = snd(c);
	goto mc;
    }

    {   register place = placeInSet(c);
	register mask  = maskInSet(c);
	if (marks[place]&mask)
	    return c;
	else
	    marks[place] |= mask;
    }

    if (isPair(fst(c))) {
	fst(c) = markCell(fst(c));
	markSnd(c);
    }
    else if (fst(c) > MAXBOXTAG)
	markSnd(c);

    return c;
}

static Void markSnd(c)			/* Variant of markCell used to     */
Cell c; {				/* update snd component of cell    */
    Cell t;				/* using tail recursion		   */

ma: t = snd(c);
mb: if (!isPair(t))
	return;

    if (fst(t)==INDIRECT) {
	snd(c) = t = snd(t);
	goto mb;
    }
    c = snd(c) = t;

    {   register place = placeInSet(c);
	register mask  = maskInSet(c);
	if (marks[place]&mask)
	    return;
	else
	    marks[place] |= mask;
    }

    if (isPair(fst(c))) {
	fst(c) = markCell(fst(c));
	goto ma;
    }
    else if (fst(c) > MAXBOXTAG)
	goto ma;
    return;
}

/* --------------------------------------------------------------------------
 * Arrays (implemented using linked lists of cells:
 * ------------------------------------------------------------------------*/

#if HASKELL_ARRAYS
Void allocArray(n,bds,z)                /* allocate array of cells         */
Int  n;                                 /* n = length of array (assume>=0) */
Cell bds;                               /* bds = bounds                    */
Cell z; {                               /* z = default value               */
    onto(cfunNil);
    while (n-- > 0) {
	heap(1);
	topfun(z);
    }
    heap(2);
    topfun(bds);
    topfun(ARRAY);
}

Void dupArray(a)                        /* duplicate array                 */
Cell a; {
    for (onto(cfunNil); isPair(a); a=snd(a))
	topfun(fst(a));
    a = cfunNil;
    while (isPair(top())) {
	Cell tmp   = snd(top());
	snd(top()) = a;
        a          = top();
        top()      = tmp;
    }
    top() = a;
}
#endif

/*-------------------------------------------------------------------------*/
