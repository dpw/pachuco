#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/time.h>

/* Size of a heap semispace */
unsigned long heap_size = 32UL * 1024 * 1024;

unsigned long heap_start, heap_end, heap_alloc, heap_threshold;
unsigned long heap2_start, heap2_end;

char *verbose_gc;

extern void lisp();
char **lisp_argv;

unsigned long alloc_heap()
{
        void *heap = mmap(NULL, heap_size, PROT_READ|PROT_WRITE,
                          MAP_PRIVATE|MAP_ANON, -1, 0);
        if (heap == MAP_FAILED) {
                perror("mmap heap");
                abort();
        }

        return (unsigned long)heap;
}

void free_heap(unsigned long addr)
{
	if (munmap((void *)addr, heap_size)) {
		perror("munmap heap");
		abort();
	}
}

int main(int argc, char **argv)
{
	int i;

	/* How many iterations of the program to run, for better
	 * profiling results. */
	char *iterations = getenv("ITERATIONS");

        /* HEAP_SIZE specifies the size of the heap semi-spaces, in MB. */
        char *heap_size_env = getenv("HEAP_SIZE");

        /* Setting VERBOSE_GC displays GC progress on stderr. */
        verbose_gc = getenv("VERBOSE_GC");

        if (heap_size_env)
                heap_size = (unsigned long)atoi(heap_size_env) * 1024 * 1024;

        heap_threshold = heap_start = alloc_heap(heap_size);
        heap_alloc = heap_end = heap_start + heap_size;

        heap2_start = alloc_heap(heap_size);
        heap2_end = heap2_start + heap_size;

        lisp_argv = argv;

	for (i = iterations ? atoi(iterations) : 1; i > 0; i--)
		lisp();

        return 0;
}
