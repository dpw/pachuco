;;; i386-specific code generation

;;; C calls

(define-reg-use (c-call c-function-name . args)
  (rplaca (cdr form) (list (cons 'c-function-name c-function-name)))
  (operator-args-reg-use form)
  general-register-count)

(define-codegen (c-call attrs . args)
  (let* ((new-frame-base in-frame-base))
    (dolist (arg (reverse args))
      (reg-use arg dest-type-value)
      (codegen arg (dest-value (first general-registers))
               new-frame-base new-frame-base general-registers cg)
      (emit-frame-push cg new-frame-base (first general-registers)))

    (emit cg "cld")
    (emit-set-ac-flag cg false)
    (emit cg "call ~A" (attr-ref attrs 'c-function-name))
    (emit-set-ac-flag cg true)
    (unless (member? %closure c-callee-saved-regs)
      (emit-restore-%closure cg new-frame-base))
    (emit-convert-value cg %a dest new-frame-base out-frame-base)))
