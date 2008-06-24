#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/time.h>

#define HEAP_SIZE (512UL * 1024 * 1024) /* 512MB */

unsigned long heap;
unsigned long heap_end;
unsigned long heap_alloc;

extern void lisp();
char **lisp_argv;

static void run_lisp(unsigned long *alloced, double *time_taken)
{
        struct timeval t1, t2;
        heap_alloc = heap_end;
        gettimeofday(&t1, NULL);
        lisp();
        gettimeofday(&t2, NULL);
        *alloced = heap_end - heap_alloc;
        *time_taken
                = (t2.tv_sec - t1.tv_sec) + (t2.tv_usec - t1.tv_usec) * 1.0e-6;
}

int main(int argc, char **argv)
{
        unsigned long alloced;
        double time_taken;
        void *heapptr = mmap(NULL, HEAP_SIZE, PROT_READ|PROT_WRITE,
                             MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
        if (heapptr == MAP_FAILED)
                perror("mmap heap");

        heap = (unsigned long)heapptr;
        heap_end = heap + HEAP_SIZE;
        lisp_argv = argv;

        char *iterations = getenv("BENCHMARK_ITERATIONS");
        if (iterations == NULL) {
                run_lisp(&alloced, &time_taken);
                fprintf(stderr, "(%lu bytes allocated; %g seconds)\n",
                        alloced, time_taken);
        }
        else {
                int i;
                for (i = atoi(iterations); i > 0; i--) {
                        run_lisp(&alloced, &time_taken);
                        fprintf(stderr, "%g\n", time_taken);
                }
        }

        return 0;
}
