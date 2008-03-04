;;; i386-specific code generation

;;; C-callable program wrapper

(define c-callee-saved-regs-without-%bp (remove %bp c-callee-saved-regs))

(define (emit-program-prologue out)
  (emit out ".text")
  (emit out ".globl lisp")
  (emit out "lisp:")
  (emit-push out %bp)
  (emit-mov out %sp %bp)
  (dolist (reg c-callee-saved-regs-without-%bp) (emit-push out reg))
  (emit-set-ac-flag out true)
  (emit-mov out (immediate closure-tag) %closure)
  (emit-function-prologue out))

(define (emit-program-epilogue out)
  (emit out "leave")
  (emit out "cld")
  (emit-set-ac-flag out false)
  (dolist (reg (reverse c-callee-saved-regs-without-%bp)) (emit-pop out reg))
  (emit out "leave")
  (emit out "ret"))

;;; C calls

(define-reg-use (c-call c-function-name . args)
  (rplaca (cdr form) (list (cons 'c-function-name c-function-name)))
  (operator-args-reg-use form)
  general-register-count)

(define (c-callee-saved reg) (member? reg c-callee-saved-regs))

(define-codegen (c-call attrs . args)
  (let* ((new-frame-base in-frame-base))
    (dolist (arg (reverse args))
      (reg-use arg dest-type-value)
      (codegen arg (dest-value (first general-registers))
               new-frame-base new-frame-base general-registers out)
      (emit-frame-push out new-frame-base (first general-registers)))

    (emit out "cld")
    (emit-set-ac-flag out false)
    (emit out "call ~A" (attr-ref attrs 'c-function-name))
    (emit-set-ac-flag out true)
    (unless (c-callee-saved %closure) (emit-restore-%closure out))
    (emit-convert-value out %a dest in-frame-base out-frame-base)))

;;; Cycle counter access

;;; On i386, we can't return enough bits in a fixnum to make this
;;; really useful

(define-reg-use (raw-rdtsc attrs)
  (if (dest-type-discard? dest-type) 0 general-register-count))

(define-codegen (raw-rdtsc attrs)
  (unless (dest-discard? dest)
    (emit out "rdtsc")
    (emit-shl out (immediate number-tag-bits) %a))
  (emit-convert-value out %a dest in-frame-base out-frame-base))
