;;; i386 machine definition

;;; This is mildly broken: correct programs will work, but programs
;;; with type errors may produce surprising behaviour.  The issue is
;;; that we rely on alignment checks for type checks, so we don't
;;; really support tag-bits > bytes-per-value.  The common codegen
;;; code needs enhancing to emit type checks in the appropriate
;;; places.

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

(defconstant closure-tag #b0011)
(defconstant closure-tag-bits 4)

(defconstant vector-tag #b0111)
(defconstant vector-tag-bits 4)

(defconstant string-tag #b1011)
(defconstant string-tag-bits 4)

(defconstant atom-tag #b1111)
(defconstant atom-tag-bits 4)

(defconstant false-representation #b1111)
(defconstant lowest-symbol-representation #b100000001111)

(defconstant simple-representations 
  ;; The quotes here are significant for CL compatibility
  (list (cons 'false false-representation)
        (cons 'true #b11111)
        (cons 'unspecified #b101111)
        (cons () #b111111)))

;;; Registers

(define-register %a "%al" "%ax" "%eax")
(define-register %b "%bl" "%bx" "%ebx")
(define-register %c "%cl" "%cx" "%ecx")
(define-register %d "%dl" "%dx" "%edx")
(define-register %si false "%si" "%esi")
(define-register %di false "%di" "%edi")
(define-register %sp false "%sp" "%esp")
(define-register %bp false "%bp" "%ebp")

(add-general-registers (list %a %b %c %d %di))
(define %closure %si)
(define %funcres (first general-registers))

(define c-callee-saved-regs (list %b %bp))

;;; Bitness-dependent assembler bits

(define (emit-literal cg lit)
  (emit cg ".int ~A" lit))

(define (insn-size-suffix scale)
  (elt '("b" "w" "l") scale))

(define (emit-movzx cg src dest src-scale . dest-scale)
  (set! dest-scale (if (null? dest-scale) value-scale (car dest-scale)))
  (emit-movzx-32 cg src dest src-scale dest-scale))

(define (emit-extend-sign-bit cg oper)
  (emit-sar cg 31 oper))

;;; No special handling required for indirect operands on i386

(defmarco (indirect-operand addr)
  addr)
