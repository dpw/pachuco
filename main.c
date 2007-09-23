#include <stdio.h>

extern long lisp(long heap);

long heap[1000000];

int main(void)
{
        long alloc_before = (long)heap + sizeof(heap);
        long alloc_after = lisp(alloc_before);

        fprintf(stderr, "[%ld bytes allocated]\n", alloc_before - alloc_after);
        return 0;
}
