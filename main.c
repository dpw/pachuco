#include <stdio.h>
#include <sys/mman.h>

extern long lisp(long heap_end);

int main(void)
{
	long heap_size = 128 * 1024 * 1024; /* 128MB */
	void *heap = mmap(NULL, heap_size, PROT_READ|PROT_WRITE,
			  MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
	long alloc_before, alloc_after;

	if (heap == MAP_FAILED)
		perror("mmap heap");

	alloc_before = (long)heap + heap_size;
	alloc_after = lisp(alloc_before);

	fprintf(stderr, "[%ld bytes allocated]\n", alloc_before - alloc_after);
	return 0;
}
