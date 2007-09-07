TARGET=x86_64
SOURCES=runtime.lisp util.lisp interpreter.lisp expander.lisp test.lisp
COMPILER_SOURCES=cl-dialect.lisp util.lisp expander.lisp interpreter.lisp mach.lisp mach-$(TARGET).lisp compiler3.lisp codegen.lisp codegen-$(TARGET).lisp driver.lisp

test: a.out
	./a.out

expand:
	sbcl --noinform --noprint $(foreach f,$(COMPILER_SOURCES),--load $(f)) --eval "(do-expand-files '( $(foreach f,$(SOURCES),\"$(f)\" )))"

interp:
	sbcl --noinform --noprint $(foreach f,$(COMPILER_SOURCES),--load $(f)) --eval "(do-interpret-files '( $(foreach f,$(SOURCES),\"$(f)\" )) '(main))"

lisp.s: $(COMPILER_SOURCES) $(SOURCES)
	sbcl --noinform --noprint $(foreach f,$(COMPILER_SOURCES),--load $(f)) --eval "(do-compile3-files '( $(foreach f,$(SOURCES),\"$(f)\" )) '(main))" >$@

compile3:
	sbcl --noinform --noprint $(foreach f,$(COMPILER_SOURCES),--load $(f)) --eval "(do-compile3-files '( $(foreach f,$(SOURCES),\"$(f)\" )) '(main))"

lisp.o: lisp.s
	gcc -c $^ -o $@

a.out: main.c lisp.s
	gcc -Wall -g $^ -o $@

clean:
	rm -f lisp.s lisp.o a.out

