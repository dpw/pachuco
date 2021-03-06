ARCH=$(shell uname -m | sed -e 's/i.86/i386/;s/arm.*/arm/')
TARGET=$(ARCH)

CODEGEN=old
COMPILEOPTS=-s

MACH_SOURCES=compiler/mach.pco
CODEGEN_SOURCES=

# Code generation stratey selection
ifeq ($(CODEGEN),old)
CODEGEN_SOURCES+=compiler/codegen-old.pco
else ifeq ($(CODEGEN),simple)
CODEGEN_SOURCES+=compiler/codegen-simple.pco
else ifeq ($(CODEGEN),context)
CODEGEN_SOURCES+=compiler/codegen-context.pco
else
$(error unknown codegen strategy $(CODEGEN))
endif

ifeq ($(TARGET),arm)
# ARM
MACH_SOURCES+=compiler/mach-32bit.pco compiler/mach-arm.pco
CODEGEN_SOURCES+=compiler/codegen-risc.pco compiler/codegen-arm.pco

else ifeq ($(TARGET),mips)
MACH_SOURCES+=compiler/mach-32bit.pco compiler/mach-mips32.pco
CODEGEN_SOURCES+=compiler/codegen-risc.pco compiler/codegen-mips32.pco

else
# x86
STACK_REGIME=no-fp

ifeq ($(STACK_REGIME),no-fp)
CODEGEN_SOURCES+=compiler/stack-traditional.pco compiler/stack-no-fp.pco
else ifeq ($(STACK_REGIME),fp)
CODEGEN_SOURCES+=compiler/stack-traditional.pco compiler/stack-fp.pco
else
$(error unknown stack regime $(STACK_REGIME))
endif

CODEGEN_SOURCES+=compiler/codegen-x86.pco
ifeq ($(TARGET),i386)
MACH_SOURCES+=compiler/mach-32bit.pco compiler/mach-i386.pco
CODEGEN_SOURCES+=compiler/codegen-i386.pco
else ifeq ($(TARGET),x86_64)
MACH_SOURCES+=compiler/mach-64bit.pco compiler/mach-x86_64.pco
CODEGEN_SOURCES+=compiler/codegen-x86_64.pco
else
$(error unknown target $(TARGET))
endif
MACH_SOURCES+=compiler/mach-x86.pco

endif

COMPILER_SOURCES= \
    language/util.pco language/expander.pco language/interpreter.pco \
    compiler/walker.pco $(MACH_SOURCES) compiler/front-end.pco \
    $(CODEGEN_SOURCES) compiler/codegen-generic.pco compiler/driver.pco

CL_COMPILER_SOURCES= \
    bootstrap/cl-dialect.lisp runtime/runtime-common.pco runtime/io-common.pco \
    $(COMPILER_SOURCES)
export CL_COMPILER_SOURCES

RUNTIME=runtime/runtime.pco runtime/cl-compat.pco runtime/runtime-common.pco \
    runtime/io.pco runtime/io-common.pco no-interp!runtime/sysconstants.pco \
    no-interp!runtime/compiled-builtins.pco no-interp!runtime/gc.pco
RUNTIME_SOURCES=$(patsubst no-interp!%,%,$(RUNTIME))
SL_COMPILER_SOURCES=$(COMPILER_SOURCES) compiler/drivermain.pco

# The initial compiler used.  Default to bootstrapping from SBCL
BOOTSTRAP_COMPILER=scripts/sbcl-wrapper

# The phase to stop at when dumping the intermediate program
DUMP_PHASE=fill-closures

.PHONY: all clean print-compiler-sources compare-stage3 mk-build-dir

all: stage0-test-run stage0-gc-test-run stage0-arity-mismatch-test-run compare-stage3 stage2-static-arity-mismatch-test-run

build/sysconstants: runtime/sysconstants.c | mk-build-dir
	$(CC) -Wall -Werror -o $@ $<

runtime/sysconstants.pco: build/sysconstants
	$< >$@

clean::
	rm -f runtime/sysconstants.pco

print-compiler-sources:
	@echo $(SL_COMPILER_SOURCES)

mk-build-dir:
	@mkdir -p build

# compile,compiler,dest,sources
define compile
$(2) $(2).s: $(1) $(RUNTIME_SOURCES) $(3) | mk-build-dir
	scripts/compile -C $(1) $(COMPILEOPTS) -o $(2) $(3)
endef

define stage_template
.PHONY: $(2)-interp-test $(2)-compile $(2)-test-run $(2)-time

$(2)-interp-test: $(1) $(RUNTIME_SOURCES) test/test.pco
	$(1) interpret $(RUNTIME) test/test.pco

$(2)-compile: $(1) $(RUNTIME_SOURCES) test/test.pco
	$(1) compile $(RUNTIME) test/test.pco

$(eval $(call compile,$(1),build/$(2)-test,test/test.pco))
$(eval $(call compile,$(1),build/$(2)-gc-test,test/gc-test.pco))
$(eval $(call compile,$(1),build/$(2)-arity-mismatch-test,test/arity-mismatch-test.pco))

$(2)-test-run: build/$(2)-test
	$$<

$(2)-gc-test-run: build/$(2)-gc-test
	$$<

$(2)-arity-mismatch-test-run: build/$(2)-arity-mismatch-test
	[ "$$$$( ( $$< 2>&1 ; [ $$$$? -ge 128 ] || echo 'wrong exit code' ) | sed '/^Aborted/d' )" = "expected 2 arguments, got 1" ]

$(2)-static-arity-mismatch-test-run: $(1) $(RUNTIME_SOURCES) test/static-arity-mismatch-test.pco
	@mkdir -p build
	[ "$$$$( ( scripts/compile -C $(1) $(COMPILEOPTS) -o build/static-arity-mismatch-test test/static-arity-mismatch-test.pco 2>&1 && echo 'should have failed' ) | sed '/Aborted/d' )" = "wrong number of arguments in call to func (expected 1, got 2)" ]

$(2)-expand: $(1) $(RUNTIME_SOURCES) $(SL_COMPILER_SOURCES)
	$(1) expand $(RUNTIME) $(SL_COMPILER_SOURCES)

$(2)-dump: $(1) $(RUNTIME_SOURCES) $(SL_COMPILER_SOURCES)
	$(1) dump $(DUMP_PHASE) $(RUNTIME) $(SL_COMPILER_SOURCES)

$(2)-expand-test: $(1) $(RUNTIME_SOURCES) test/test.pco
	$(1) expand $(RUNTIME) test/test.pco

$(2)-dump-test: $(1) $(RUNTIME_SOURCES) test/test.pco
	$(1) dump $(DUMP_PHASE) $(RUNTIME) test/test.pco

$(eval $(call compile,$(1),build/$(3),$(SL_COMPILER_SOURCES)))

$(2)-time: $(1) $(RUNTIME_SOURCES) $(SL_COMPILER_SOURCES)
	scripts/compile -C $(1) $(COMPILEOPTS) -T $(SL_COMPILER_SOURCES)
endef

clean::
	rm -rf build

scripts/sbcl-wrapper: $(CL_COMPILER_SOURCES)

$(eval $(call stage_template,$(BOOTSTRAP_COMPILER),stage0,stage1))
$(eval $(call stage_template,build/stage1,stage1,stage2))
$(eval $(call stage_template,build/stage2,stage2,stage3))

compare-stage3: build/stage2.s build/stage3.s
	cmp -s build/stage2.s build/stage3.s

build/repl build/repl.s: language/repl.pco language/util.pco language/interpreter.pco language/expander.pco | build/stage2
	scripts/compile $^ $(COMPILEOPTS) -o $@

.PHONY: repl
repl: build/repl
	build/repl $(patsubst no-interp!%,,$(RUNTIME))
