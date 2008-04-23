ARCH=$(shell uname -m | sed -e s/i.86/i386/)
TARGET=$(ARCH)
CFLAGS := -Wall -g

ifeq ($(ARCH),x86_64)
ifeq ($(TARGET),i386)
CFLAGS := $(CFLAGS) -m32
endif
endif

STACK_REGIME=no-fp
ifeq ($(STACK_REGIME),no-fp)
STACK_SOURCES=stack-traditional.lisp stack-no-fp.lisp
else ifeq ($(STACK_REGIME),fp)
STACK_SOURCES=stack-traditional.lisp stack-fp.lisp
else
$(error unknown stack regime $(STACK_REGIME))
endif

COMPILER_SOURCES=util.lisp expander.lisp interpreter.lisp mach.lisp mach-$(TARGET).lisp compiler.lisp codegen.lisp $(STACK_SOURCES) codegen-$(TARGET).lisp driver.lisp

CL_COMPILER_SOURCES=cl-dialect.lisp $(COMPILER_SOURCES)
export CL_COMPILER_SOURCES

TEST_SOURCES=runtime.lisp test.lisp

SL_COMPILER_SOURCES=runtime.lisp $(COMPILER_SOURCES) drivermain.lisp

# The initial compiler used.  Default to bootstrapping from SBCL
BOOTSTRAP_COMPILER=scripts/sbcl-wrapper

all: stage2-test-run compare-stage3

benchmark: stage1
	BENCHMARK_ITERATIONS=10 ./stage1 compile $(SL_COMPILER_SOURCES) >/dev/null

define stage_template
$(2)interp: $(1) $(TEST_SOURCES)
	$(abspath $(1)) interpret $(TEST_SOURCES)

$(2)compile: $(1) $(TEST_SOURCES)
	$(abspath $(1)) compile $(TEST_SOURCES)

$(2)test.s: $(1) $(TEST_SOURCES)
	$(abspath $(1)) compile $(TEST_SOURCES) >$$@

$(2)test: main.o $(2)test.s
	gcc $(CFLAGS) $$^ -o $$@

$(2)test-run: $(2)test
	$(abspath $(2)test)

$(3).s: $(1) $(SL_COMPILER_SOURCES)
	$(abspath $(1)) compile $(SL_COMPILER_SOURCES) >$$@

$(3): main.o $(3).s
	gcc $(CFLAGS) $$^ -o $$@
endef

scripts/sbcl-wrapper: $(CL_COMPILER_SOURCES)
	touch scripts/sbcl-wrapper

$(eval $(call stage_template,$(BOOTSTRAP_COMPILER),stage0-,stage1))
$(eval $(call stage_template,stage1,stage1-,stage2))
$(eval $(call stage_template,stage2,stage2-,stage3))

compare-stage3: stage2.s stage3.s
	cmp -s stage2.s stage3.s

clean:
	rm -f *.s *.o stage0-test stage1 stage1-test stage2 stage2-test
