#include <stdio.h>

extern long lisp(long *args, long heap);

long heap[1000000];

int main(void)
{
        long args[] = { 0 };
        long alloc_before = (long)heap + sizeof(heap);
        long alloc_after = lisp(args, alloc_before);

        fprintf(stderr, "[%ld bytes allocated]\n", alloc_before - alloc_after);
        return 0;
}
