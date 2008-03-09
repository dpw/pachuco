#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <sys/mman.h>

#define HEAP_SIZE (512UL * 1024 * 1024) /* 512MB */

unsigned long heap_alloc;

extern void lisp();
char **lisp_argv;

inline uint64_t rdtsc()
{
#if defined(__i386__)
        uint64_t val;
        __asm__ __volatile__ ("rdtsc" : "=A" (val));
        return val;
#elif defined(__x86_64__)
        unsigned int a,d;
        asm volatile("rdtsc" : "=a" (a), "=d" (d));
        return ((unsigned long)a) | (((unsigned long)d)<<32);
#else
#error "Unsupported architecture"
#endif
}

void run_lisp(void *heap)
{
        unsigned long heap_end = (unsigned long)heap + HEAP_SIZE;
        heap_alloc = heap_end;
        uint64_t start = rdtsc();
        lisp();
        uint64_t end = rdtsc();
        fprintf(stderr, "[%ld bytes allocated ; %" PRIu64 " cycles]\n",
                heap_end - heap_alloc, end - start);
}

int main(int argc, char **argv)
{
        lisp_argv = argv;
        void *heap = mmap(NULL, HEAP_SIZE, PROT_READ|PROT_WRITE,
                          MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
        if (heap == MAP_FAILED)
                perror("mmap heap");

        char *iterations = getenv("BENCHMARK_ITERATIONS");
        if (iterations == NULL) {
                run_lisp(heap);
        }
        else {
                int i;
                for (i = atoi(iterations); i > 0; i--)
                        run_lisp(heap);
        }

        return 0;
}
