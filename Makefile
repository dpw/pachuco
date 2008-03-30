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
export CL_COMPILER_SOURCES

TEST_SOURCES=runtime.lisp test.lisp

SL_COMPILER_SOURCES=runtime.lisp $(COMPILER_SOURCES) drivermain.lisp

all: stage2-test-run compare-stage3

define stage_template
$(2)interp: $(1) $(TEST_SOURCES)
	./$(1) interpret $(TEST_SOURCES)

$(2)compile: $(1) $(TEST_SOURCES)
	./$(1) compile $(TEST_SOURCES)

$(2)test.s: $(1) $(TEST_SOURCES)
	./$(1) compile $(TEST_SOURCES) >$$@

$(2)test: main.o $(2)test.s
	gcc $(CFLAGS) $$^ -o $$@

$(2)test-run: $(2)test
	./$(2)test

$(3).s: $(1) $(SL_COMPILER_SOURCES)
	./$(1) compile $(SL_COMPILER_SOURCES) >$$@

$(3): main.o $(3).s
	gcc $(CFLAGS) $$^ -o $$@
endef

scripts/sbcl-wrapper: $(CL_COMPILER_SOURCES)

$(eval $(call stage_template,scripts/sbcl-wrapper,stage0,stage1))
$(eval $(call stage_template,stage1,stage1-,stage2))
$(eval $(call stage_template,stage2,stage2-,stage3))

compare-stage3: stage2.s stage3.s
	cmp -s stage2.s stage3.s

clean:
	rm -f *.s *.o stage0-test stage1 stage1-test stage2 stage2-test
