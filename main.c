#include <stdio.h>
#include <sys/mman.h>

long heap_alloc;

extern long lisp();

int main(void)
{
	long heap_size = 512 * 1024 * 1024; /* 512MB */
	void *heap = mmap(NULL, heap_size, PROT_READ|PROT_WRITE,
			  MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
	long heap_end;

	if (heap == MAP_FAILED)
		perror("mmap heap");

	heap_end = (long)heap + heap_size;
        heap_alloc = heap_end;
	lisp();

	fprintf(stderr, "[%ld bytes allocated]\n", heap_end - heap_alloc);
	return 0;
}
