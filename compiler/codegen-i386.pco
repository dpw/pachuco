;;; i386-specific code generation

;;; C calls

(define-reg-use (c-call c-function-name . args)
  (rplaca (cdr form) (list (cons 'c-function-name c-function-name)))
  (operator-args-reg-use form)
  general-register-count)

(define-codegen (c-call attrs . args)
  (dolist (arg (reverse args))
    (reg-use arg dest-type-value)
    (codegen arg (dest-value (first general-registers))
             (codegen-frame-base cg) general-registers cg)
    (emit-frame-push cg (first general-registers)))

  (emit cg "cld")
  (emit-set-ac-flag cg false)
  (emit cg "call ~A" (attr-ref attrs 'c-function-name))
  (emit-set-ac-flag cg true)
  (unless (member? %closure c-callee-saved-regs) (emit-restore-%closure cg))
  (emit-convert-value cg %a dest out-frame-base))
