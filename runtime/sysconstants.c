#include <stdio.h>
#include <fcntl.h>

static void print_constant(char *name, int val) {
	printf("(defmacro %s %d)\n", name, val);
}

int main(void) {
	print_constant("syscall-o-rdonly", O_RDONLY);
	print_constant("syscall-o-wronly", O_WRONLY);
	print_constant("syscall-o-creat", O_CREAT);
	return 0;
}
