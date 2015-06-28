# Introduction

Pachuco is a self-hosting compiler for a dialect of the Lisp
programming language.  It generates assembly code directly, rather
than compiling to another high-level language or virtual machine.  It
targets x86 (32- and 64-bit), ARM and MIPS. The goal of the compiler
is to be simple and fast: On my 2013-era x86 laptop, it compiles
itself in under 0.2s.

The system is about 10000 lines of code, including the compiler,
runtime, garbage collector, and interpreter (used in the macro
system).  The whole system is written in the Pachuco language, except
for a minimal amount of C and Common Lisp code for bootstrapping.  The
implementation remains very spartan in comparison to production
compilers (e.g. its error reporting is almost non-existent).

The Pachuco language is briefly described in the LANGUAGE file.  It is
modest in comparison to Common Lisp and Scheme.  Like Scheme, the
language features a small core, lexically-scoped variables, and
supports proper tail recursion.  But the macro system and the library
functions in the runtime are influenced more by Common Lisp.

Like a C compiler, the Pachuco compiler produces stand-alone
executables.  Unlike traditional Lisps, it is not integrated into a
read-eval-print loop, although the system does include a simple
interpreter-based REPL.

# Getting Started

Pachuco currently builds and runs under Linux.  It requires basic
development tools to be installed, such as gcc, binutils, and make.
It also uses the sbcl implementation of Common Lisp to bootstrap
itself.  This is available as the sbcl package on Debian, Ubuntu and
Fedora, or see [sbcl.org](http://sbcl.org).

Running `make all` bootstraps the compiler and runs some tests.  The
resulting compiler executable is in `build/stage2`, but it is normally
invoked via the shell script `scripts/compile`.  Among other things,
this script includes the standard runtime files into the compilation,
and invokes gcc to assemble the compiler output into a working
executable.

There is a simple example program to generate the Fibonacci sequence
in `examples/fib.pco`.  Compile it with:

    $ scripts/compile examples/fib.pco -o build/fib

(The `-o` option specifies the name of the destination executable.)

The result is a stand-alone executable that you can run from the
command line:

    $ build/fib 10
    89

To start the interpreter-based REPL, do `make repl`:

    $ make repl
    build/repl runtime/runtime.pco runtime/cl-compat.pco runtime/runtime-common.pco runtime/io.pco runtime/io-common.pco
    Reading runtime/runtime.pco
    Reading runtime/cl-compat.pco
    Reading runtime/runtime-common.pco
    Reading runtime/io.pco
    Reading runtime/io-common.pco
    >>>

You can type Pachuco expressions and programs at the `>>>` prompt:

    >>> (define (fact n)
          (if (= n 0)
              1
              (* n (fact (- n 1)))))
    #<function>
    >>> (fact 10)
    3628800

Type control-D to quit the REPL.
