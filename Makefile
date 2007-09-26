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

RUNTIME_SOURCES=runtime.lisp
TEST_SOURCES=$(RUNTIME_SOURCES) test.lisp

cl_expand=sbcl --noinform --noprint $(foreach f,$(CL_COMPILER_SOURCES),--load $(f)) --eval "(do-expand-files '( $(foreach f,$(1),\"$(f)\" )))"
cl_interp=sbcl --noinform --noprint $(foreach f,$(CL_COMPILER_SOURCES),--load $(f)) --eval "(do-interpret-files '( $(foreach f,$(1),\"$(f)\" )) '(main))"
cl_compile=sbcl --noinform --noprint $(foreach f,$(CL_COMPILER_SOURCES),--load $(f)) --eval "(do-compile-files '( $(foreach f,$(1),\"$(f)\" )) '(main))"

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

clean:
	rm -f stage0-test.s stage0-test

