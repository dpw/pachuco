;;; i386 machine definition

(defconstant value-scale 2)
(defconstant value-size (ash 1 value-scale))

;;; Value representation

;;; number-tag must be all zeros
(defconstant number-tag #b00)
(defconstant number-tag-bits 2)

(defconstant box-tag #b01)
(defconstant box-tag-bits 2)

(defconstant pair-tag #b10)
(defconstant pair-tag-bits 2)

(defconstant function-tag #b0011)
(defconstant function-tag-bits 4)

(defconstant vector-tag #b0111)
(defconstant vector-tag-bits 4)

(defconstant string-tag #b1011)
(defconstant string-tag-bits 4)

(defconstant atom-tag #b1111)
(defconstant atom-tag-bits 4)

(defconstant false-representation #b1111)
(defconstant unspecified-representation #b101111)
(defconstant lowest-symbol-representation #b100000001111)

(defconstant simple-representations 
  ;; The quotes here are significant for CL compatibility
  (list (cons 'false false-representation)
        (cons 'true #b11111)
        (cons 'unspecified unspecified-representation)
        (cons () #b111111)))

;;; Registers

(define-register %a "%al" "%ax" "%eax")
(define-register %b "%bl" "%bx" "%ebx")
(define-register %c "%cl" "%cx" "%ecx")
(define-register %d "%dl" "%dx" "%edx")
(define-register %si "%sil" "%si" "%esi")
(define-register %di "%dil" "%di" "%edi")
(define-register %sp "%spl" "%sp" "%esp")
(define-register %bp "%bpl" "%bp" "%ebp")

;;; Use RBX as the allocation pointer, since there are no relevant
;;; instructions that clobber it implicitly.
(define %alloc %b)

(define general-registers (list %a %c %d %di))
(define general-register-count (length general-registers))

(define %func %si)
(define %funcres (first general-registers))

;;; %nargs is use to pass the number of arguments to functions.  We
;;; reuse one of the general-registers for this, which means we have
;;; to be really careful about invoking the operators that use it
;;; (check-arg-count, arg-count).
(define %nargs (last-elem general-registers))

(define c-callee-saved-regs '(%b %bp))

;;; Bitness-dependent assembler bits

(define (emit-literal out lit)
  (emit out ".int ~A" lit))

(define (insn-size-suffix scale)
  (elt '("b" "w" "l") scale))

(define (emit-movzx out src dest src-scale . dest-scale)
  (set! dest-scale (if (null? dest-scale) value-scale (car dest-scale)))
  (emit-movzx-32 out src dest src-scale dest-scale))

(define (emit-extend-sign-bit out oper)
  (emit-sar out (immediate 31) oper))
