/* This file provides a simple mechanism for measuring elapsed time
 * on Unix based machines (more precisely, on any machine with a
 * rusage() function).
 *
 * It is included in the Gofer distribution for the purpose of
 * benchmarking the Gofer interpreter, comparing its performance
 * across a variety of different machines, and with other systems
 * for similar languages.
 *
 * To make use of these functions, simply add -DWANT_TIMER to the
 * CFLAGS line in the Makefile, before compiling Gofer.
 *
 * It would be somewhat foolish to try to use the timings produced
 * in this way for anything other than the purpose described above.
 * In particular, using timings to compare the performance of different
 * versions of an algorithm is likely to give very misleading results.
 * The current implementation of Gofer as an interpreter, without any
 * significant optimizations, means that there are much more significant
 * overheads than can be accounted for by small variations in Gofer
 * code.
 */

#include <sys/time.h>
#include <sys/resource.h>

long userElapsed, systElapsed;

void updateTimers() {
    static long lastUser = 0;
    static long lastSyst = 0;
    long curr;
    struct rusage ruse;
    getrusage(RUSAGE_SELF,&ruse);

    curr        = ruse.ru_utime.tv_sec*1000000L + ruse.ru_utime.tv_usec;
    userElapsed = curr - lastUser;
    lastUser    = curr;

    curr        = ruse.ru_stime.tv_sec*1000000L + ruse.ru_stime.tv_usec;
    systElapsed = curr - lastSyst;
    lastSyst    = curr;
}

long millisecs(t)
long t; {
    return (t+500)/1000;
}
