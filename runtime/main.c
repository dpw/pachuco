#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/time.h>

/* Size of the heap to allocate using mmap. */
#define HEAP_SIZE (32UL * 1024 * 1024)

/* Heap GC threshold size, */
unsigned long heap_threshold_size = HEAP_SIZE - 4 * 1024;

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
        void *heap = mmap(NULL, HEAP_SIZE, PROT_READ|PROT_WRITE,
                          MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
        if (heap == MAP_FAILED)
                perror("mmap heap");

        return (unsigned long)heap;
}

int main(int argc, char **argv)
{
        heap_start = alloc_heap(HEAP_SIZE);
        heap_end = heap_start + HEAP_SIZE;
        heap_threshold = heap_end - heap_threshold_size;

        heap2_start = alloc_heap(HEAP_SIZE);
        heap2_end = heap2_start + HEAP_SIZE;

        lisp_argv = argv;

        verbose_gc = getenv("VERBOSE_GC");
        char *iterations = getenv("BENCHMARK_ITERATIONS");
        if (iterations == NULL) {
                heap_alloc = heap_end;
                lisp();
        }
        else {
                int i;
                for (i = atoi(iterations); i > 0; i--) {
                        heap_alloc = heap_end;
                        lisp();
                }
        }

        return 0;
}
