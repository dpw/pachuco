;;; x86_64-specific code generation

;;; C-callable program wrapper

(define c-callee-saved-regs-without-%bp (remove %bp c-callee-saved-regs))

(define (emit-program-prologue out)
  (emit out ".text")
  (emit out ".globl lisp")
  (emit out "lisp:")
  (dolist (reg c-callee-saved-regs-without-%bp) (emit-push out reg))
  (emit-set-ac-flag out true)
  (emit-mov out (immediate function-tag) %func)
  (emit-function-prologue out))

(define (emit-program-epilogue out)
  (emit out "leave")
  (emit out "cld")
  (emit-set-ac-flag out false)
  (dolist (reg (reverse c-callee-saved-regs-without-%bp)) (emit-pop out reg))
  (emit out "ret"))

;;; C calls

(define-reg-use (c-call c-function-name . args)
  (rplaca (cdr form) (list (cons 'c-function-name c-function-name)))
  (operator-args-reg-use form)
  general-register-count)

(define (c-callee-saved reg) (member? reg c-callee-saved-regs))

(define-codegen (c-call attrs . args)
  (when (> (length args) (length c-call-arg-regs))
    (error "too many arguments to c-call"))
  (operator-args-codegen form in-frame-base
                      (move-regs-to-front c-call-arg-regs general-registers)
                      out)
  (emit out "cld")
  (emit-set-ac-flag out false)

  ;; C ABI requires us to align stack to 16 byte bundary
  (emit-push out %bp)
  (emit-mov out %sp %bp)
  (emit-and out (immediate -16) %sp)
  (emit out "call ~A" (attr-ref attrs 'c-function-name))
  (emit out "leave")

  (emit-set-ac-flag out true)
  (unless (c-callee-saved %func) (emit-restore-%func out))
  (emit-convert-value out %a dest in-frame-base out-frame-base))

;;; Cycle counter access

(define-reg-use (raw-rdtsc attrs)
  (if (dest-type-discard? dest-type) 0 general-register-count))

(define-codegen (raw-rdtsc attrs)
  (unless (dest-discard? dest)
    (emit out "rdtsc")
    (emit-shl out (immediate (+ 32 number-tag-bits)) %d)
    (emit-shl out (immediate number-tag-bits) %a)
    (emit-or out %d %a))
  (emit-convert-value out %a dest in-frame-base out-frame-base))
