;;; Machine definition

(defconstant value-scale 3)
(defconstant value-size (ash 1 value-scale))

;;; Value representation

;;; number-tag must be all zeros
(defconstant number-tag #b000)
(defconstant number-tag-bits 3)

(defconstant function-tag #b001)
(defconstant function-tag-bits 3)

(defconstant box-tag #b010)
(defconstant box-tag-bits 3)

(defconstant pair-tag #b011)
(defconstant pair-tag-bits 3)

(defconstant vector-tag #b100)
(defconstant vector-tag-bits 3)

(defconstant string-tag #b101)
(defconstant string-tag-bits 3)

(defconstant atom-tag #b111)
(defconstant atom-tag-bits 3)

(defconstant false-representation #b111)
(defconstant unspecified-representation #b10111)
(defconstant lowest-symbol-representation #b100000000111)

(defconstant simple-representations 
  ;; The quotes here are significant for CL compatibility
  (list (cons 'false false-representation)
        (cons 'true #b1111)
        (cons 'unspecified unspecified-representation)
        (cons () #b11111)))

;;; Registers

(define register-operands ())

(defmarco (define-register name . variants)
  (quasiquote (definitions
    (define (unquote name) (quote (unquote name)))
    (set! register-operands (acons (unquote name) (quote (unquote variants))
                                   register-operands)))))

(define-register %a "%al" "%ax" "%eax" "%rax")
(define-register %b "%bl" "%bx" "%ebx" "%rbx")
(define-register %c "%cl" "%cx" "%ecx" "%rcx")
(define-register %d "%dl" "%dx" "%edx" "%rdx")
(define-register %si "%sil" "%si" "%esi" "%rsi")
(define-register %di "%dil" "%di" "%edi" "%rdi")
(define-register %sp "%spl" "%sp" "%esp" "%rsp")
(define-register %bp "%bpl" "%bp" "%ebp" "%rbp")

(defmarco (define-extended-registers)
  (cons 'definitions
        (mapfor (n '(8 9 10 11 12 13 14 15))
          (list* 'define-register
                 (subject-language-intern (format nil "%r~D" n))
                 (mapfor (tmpl '("%r~Dl" "%r~Dw" "%r~Dd" "%r~D"))
                   (format nil tmpl n))))))

(define-extended-registers)

;;; Use RBX as the allocation pointer, since there are no relevant
;;; instructions that clobber it implicitly.
(define %alloc %r14)

(define general-registers (list %a %b %c %d %si %di))
(define general-register-count (length general-registers))

(define %func %r15)
(define %funcres (first general-registers))

;;; %nargs is use to pass the number of arguments to functions.  We
;;; reuse one of the general-registers for this, which means we have
;;; to be really careful about invoking the operators that use it
;;; (check-arg-count, arg-count).
(define %nargs (last-elem general-registers))

(define c-callee-saved-regs '(%b %bp %r12 %r13 %r14 %r15))
(define c-call-arg-regs '(%di %si %d %c %r8 %r9))

;;; Bitness-dependent assembler bits

(define (emit-literal out lit)
  (emit out ".quad ~A" lit))

(define (insn-size-suffix scale)
  (elt '("b" "w" "l" "q") scale))

(define (emit-movzx out src dest src-scale . dest-scale)
  (set! dest-scale (if (null? dest-scale) value-scale (car dest-scale)))
  (if (= dest-scale 3)
      (if (= src-scale 3)
          (emit out "movq ~A, ~A" (value-sized src) (value-sized dest))
          (emit-movzx-32 out src dest src-scale 2))
      (emit-movzx-32 out src dest src-scale dest-scale)))

(define (emit-extend-sign-bit out oper)
  (emit-sar out (immediate 63) oper))