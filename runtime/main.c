#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/time.h>

/* Heap GC threshold size, */
unsigned long heap_threshold_size;

unsigned long heap_start;
unsigned long heap_end;
unsigned long heap_alloc;
unsigned long heap_threshold;

unsigned long heap2_start;
unsigned long heap2_end;

char *verbose_gc;

extern void lisp();
char **lisp_argv;

static unsigned long alloc_heap(unsigned long size)
{
        void *heap = mmap(NULL, size, PROT_READ|PROT_WRITE,
                          MAP_PRIVATE|MAP_ANON, -1, 0);
        if (heap == MAP_FAILED)
                perror("mmap heap");

        return (unsigned long)heap;
}

int main(int argc, char **argv)
{
        /* HEAP_SIZE specifies the size of the heap semi-spaces, in MB. */
        char *heap_size_env = getenv("HEAP_SIZE");
        unsigned long heap_size = 32UL * 1024 * 1024;

        /* BENCHMARK_ITERATIONS specifies how many iterations of the
        program to run, for benchmarking purposes. */
        char *iterations = getenv("BENCHMARK_ITERATIONS");

        /* Setting VERBOSE_GC displays GC progress on stderr. */
        verbose_gc = getenv("VERBOSE_GC");
        
        if (heap_size_env)
                heap_size = (unsigned long)atoi(heap_size_env) * 1024 * 1024;
        
        /* Looking at this months later, it seems bogus.  I think the
           idea was to leave a bit of space in the heap for the GC to
           use up to the point when it switches allocations over to
           to-space.  But of course, any allocations before that will
           be subject to the old heap_threshold, and cause a recursive
           GC.  So evidently it is unncecessary.  For this to really
           work, the first thing the GC should do is to adjust
           heap_threshold to allow itself access to the slack. */
        heap_threshold_size = heap_size - 4 * 1024;
        
        heap_start = alloc_heap(heap_size);
        heap_end = heap_start + heap_size;

        heap2_start = alloc_heap(heap_size);
        heap2_end = heap2_start + heap_size;

        lisp_argv = argv;

        if (iterations == NULL) {
                heap_alloc = heap_end;
                heap_threshold = heap_end - heap_threshold_size;
                lisp();
        }
        else {
                int i;
                for (i = atoi(iterations); i > 0; i--) {
                        heap_alloc = heap_end;
                        heap_threshold = heap_end - heap_threshold_size;
                        memset((void *)heap_start, 0, heap_end - heap_start);
                        memset((void *)heap2_start, 0, heap2_end - heap2_start);
                        lisp();
                }
        }

        return 0;
}
