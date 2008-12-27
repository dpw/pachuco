ARCH=$(shell uname -m | sed -e s/i.86/i386/)
TARGET=$(ARCH)

STACK_REGIME=no-fp
ifeq ($(STACK_REGIME),no-fp)
STACK_SOURCES=compiler/stack-traditional.pco compiler/stack-no-fp.pco
else ifeq ($(STACK_REGIME),fp)
STACK_SOURCES=compiler/stack-traditional.pco compiler/stack-fp.pco
else
$(error unknown stack regime $(STACK_REGIME))
endif

COMPILER_SOURCES= \
    language/util.pco language/expander.pco language/interpreter.pco \
    compiler/mach.pco compiler/mach-$(TARGET).pco compiler/compiler.pco \
    compiler/codegen.pco $(STACK_SOURCES) compiler/codegen-$(TARGET).pco \
    compiler/driver.pco

CL_COMPILER_SOURCES= \
    bootstrap/cl-dialect.lisp runtime/runtime2.pco $(COMPILER_SOURCES)
export CL_COMPILER_SOURCES

TEST_SOURCES=test/test.pco
GC_TEST_SOURCES=test/gc-test.pco

RUNTIME_SOURCES=runtime/runtime.pco runtime/runtime2.pco runtime/gc.pco
SL_COMPILER_SOURCES=$(COMPILER_SOURCES) compiler/drivermain.pco

# The initial compiler used.  Default to bootstrapping from SBCL
BOOTSTRAP_COMPILER=scripts/sbcl-wrapper

.PHONY: all clean print-compiler-sources compare-stage3

all: stage0-test-run stage0-gc-test-run compare-stage3

print-compiler-sources:
	@echo $(SL_COMPILER_SOURCES)

define stage_template
.PHONY: $(2)-interp-test $(2)-compile $(2)-test-run

$(2)-interp-test: $(1) $(RUNTIME_SOURCES) $(TEST_SOURCES)
	$(1) interpret $(RUNTIME_SOURCES) $(TEST_SOURCES)

$(2)-compile: $(1) $(RUNTIME_SOURCES) $(TEST_SOURCES)
	$(1) compile $(RUNTIME_SOURCES) $(TEST_SOURCES)


build/$(2)-test build/$(2)-test.s: $(1) $(RUNTIME_SOURCES) $(TEST_SOURCES)
	mkdir -p build
	scripts/compile -C $(1) -s -o $$@ $(TEST_SOURCES)

$(2)-test-run: build/$(2)-test
	$$<


build/$(2)-gc-test build/$(2)-gc-test.s: $(1) $(RUNTIME_SOURCES) $(GC_TEST_SOURCES)
	mkdir -p build
	scripts/compile -C $(1) -s -o $$@ $(GC_TEST_SOURCES)

$(2)-gc-test-run: build/$(2)-gc-test
	$$<


$(2)-expand: $(1) $(RUNTIME_SOURCES) $(SL_COMPILER_SOURCES)
	$(1) expand $(RUNTIME_SOURCES) $(SL_COMPILER_SOURCES)

build/$(3) build/$(3).s: $(1) $(RUNTIME_SOURCES) $(SL_COMPILER_SOURCES)
	mkdir -p build
	scripts/compile -C $(1) -s -o build/$(3) $(SL_COMPILER_SOURCES)
endef

clean:
	rm -rf build

scripts/sbcl-wrapper: $(CL_COMPILER_SOURCES)
	touch scripts/sbcl-wrapper

$(eval $(call stage_template,$(BOOTSTRAP_COMPILER),stage0,stage1))
$(eval $(call stage_template,build/stage1,stage1,stage2))
$(eval $(call stage_template,build/stage2,stage2,stage3))

compare-stage3: build/stage2.s build/stage3.s
	cmp -s build/stage2.s build/stage3.s

build/repl build/repl.s: language/repl.pco language/util.pco language/interpreter.pco language/expander.pco | build/stage2
	scripts/compile $^ -s -o $@

.PHONY: repl
repl: build/repl
	build/repl $(RUNTIME_SOURCES)