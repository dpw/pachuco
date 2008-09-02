ARCH=$(shell uname -m | sed -e s/i.86/i386/)
TARGET=$(ARCH)

STACK_REGIME=no-fp
ifeq ($(STACK_REGIME),no-fp)
STACK_SOURCES=stack-traditional.lisp stack-no-fp.lisp
else ifeq ($(STACK_REGIME),fp)
STACK_SOURCES=stack-traditional.lisp stack-fp.lisp
else
$(error unknown stack regime $(STACK_REGIME))
endif

COMPILER_SOURCES=util.lisp expander.lisp interpreter.lisp mach.lisp mach-$(TARGET).lisp compiler.lisp codegen.lisp $(STACK_SOURCES) codegen-$(TARGET).lisp driver.lisp

CL_COMPILER_SOURCES=cl-dialect.lisp runtime2.lisp $(COMPILER_SOURCES)
export CL_COMPILER_SOURCES

TEST_SOURCES=test.lisp

RUNTIME_SOURCES=runtime.lisp runtime2.lisp
SL_COMPILER_SOURCES=$(COMPILER_SOURCES) drivermain.lisp

# The initial compiler used.  Default to bootstrapping from SBCL
BOOTSTRAP_COMPILER=scripts/sbcl-wrapper

.PHONY: all clean print-compiler-sources

all: stage0-test-run compare-stage3

print-compiler-sources:
	@echo $(COMPILER_SOURCES) drivermain.lisp

define stage_template
.PHONY: $(2)interp $(2)compile $(2)test-run

$(2)interp: $(1) $(RUNTIME_SOURCES) $(TEST_SOURCES)
	$(abspath $(1)) interpret $(RUNTIME_SOURCES) $(TEST_SOURCES)

$(2)compile: $(1) $(RUNTIME_SOURCES) $(TEST_SOURCES)
	$(abspath $(1)) compile $(TEST_SOURCES)

$(2)test: $(1) $(RUNTIME_SOURCES) $(TEST_SOURCES)
	scripts/compile -C $(1) -s -o $$@ $(TEST_SOURCES)

$(2)test-run: $(2)test
	$(abspath $(2)test)

$(3) $(3).s: $(1) $(RUNTIME_SOURCES) $(SL_COMPILER_SOURCES)
	scripts/compile -C $(1) -s -o $(3) $(SL_COMPILER_SOURCES)
endef

scripts/sbcl-wrapper: $(CL_COMPILER_SOURCES)
	touch scripts/sbcl-wrapper

$(eval $(call stage_template,$(BOOTSTRAP_COMPILER),stage0-,stage1))
$(eval $(call stage_template,stage1,stage1-,stage2))
$(eval $(call stage_template,stage2,stage2-,stage3))

compare-stage3: stage2.s stage3.s
	cmp -s stage2.s stage3.s

clean:
	rm -f *.s *.o stage0-test stage1 stage1-test stage2 stage2-test stage3
