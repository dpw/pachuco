ARCH=$(shell uname -m | sed -e s/i.86/i386/)
TARGET=$(ARCH)
CFLAGS := -Wall -g

ifeq ($(ARCH),x86_64)
ifeq ($(TARGET),i386)
CFLAGS := $(CFLAGS) -m32
endif
endif

COMPILER_SOURCES=util.lisp expander.lisp interpreter.lisp mach.lisp mach-$(TARGET).lisp compiler.lisp codegen.lisp codegen-$(TARGET).lisp driver.lisp
CL_COMPILER_SOURCES=cl-dialect.lisp $(COMPILER_SOURCES)

TEST_SOURCES=runtime.lisp test.lisp

SL_COMPILER_SOURCES=runtime.lisp $(COMPILER_SOURCES) drivermain.lisp

listify=( $(foreach f,$(1),\"$(f)\") )
cl_expand=sbcl --noinform --noprint $(foreach f,$(CL_COMPILER_SOURCES),--load $(f)) --eval "(progn (do-expand-files '$(call listify,$(1))) (quit))"
cl_interp=sbcl --noinform --noprint $(foreach f,$(CL_COMPILER_SOURCES),--load $(f)) --eval "(progn (do-interpret-files '$(call listify,$(1)) '(main)) (quit))"
cl_compile=sbcl --noinform --noprint $(foreach f,$(CL_COMPILER_SOURCES),--load $(f)) --eval "(progn (do-compile-files '$(call listify,$(1)) '(main)) (quit))"

all: stage0-test-run

stage0-expand: $(TEST_SOURCES) $(CL_COMPILER_SOURCES)
	$(call cl_expand,$(TEST_SOURCES))

stage0-interp: $(TEST_SOURCES) $(CL_COMPILER_SOURCES)
	$(call cl_interp,$(TEST_SOURCES))

stage0-compile: $(TEST_SOURCES) $(CL_COMPILER_SOURCES)
	$(call cl_compile,$(TEST_SOURCES))

stage0-test.s: $(TEST_SOURCES) $(CL_COMPILER_SOURCES)
	$(call cl_compile,$(TEST_SOURCES)) >$@

stage0-test: main.o stage0-test.s
	gcc $(CFLAGS) $^ -o $@

stage0-test-run: stage0-test
	./stage0-test

stage1.s: $(SL_COMPILER_SOURCES) $(CL_COMPILER_SOURCES)
	$(call cl_compile,$(SL_COMPILER_SOURCES)) >$@

stage1: main.o stage1.s
	gcc $(CFLAGS) $^ -o $@

stage1_interp=echo "interpret $(call listify,$(1)) (main)" | ./stage1
stage1_compile=echo "compile $(call listify,$(1)) (main)" | ./stage1

stage1-interp: stage1 $(TEST_SOURCES)
	$(call stage1_interp,$(TEST_SOURCES))

stage1-compile: stage1 $(TEST_SOURCES)
	$(call stage1_compile,$(TEST_SOURCES))

stage1-test.s: stage1 $(TEST_SOURCES)
	$(call stage1_compile,$(TEST_SOURCES)) >$@

stage1-test: main.o stage1-test.s
	gcc $(CFLAGS) $^ -o $@

stage1-test-run: stage1-test
	./stage1-test

clean:
	rm -f *.s *.o stage0-test stage1

