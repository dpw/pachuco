;;; x86_64-specific code generation

;;; C calls

(define-reg-use (c-call c-function-name . args)
  (rplaca (cdr form) (list (cons 'c-function-name c-function-name)))
  (operator-args-reg-use form)
  general-register-count)

(define-codegen (c-call attrs . args)
  (when (> (length args) (length c-call-arg-regs))
    (error "too many arguments to c-call"))
  (operator-args-codegen form
                      (move-regs-to-front c-call-arg-regs general-registers) cg)
  (emit cg "cld")
  (emit-set-ac-flag cg false)

  ;; C ABI requires us to align stack to 16 byte bundary
  (emit-mov cg %sp %b)
  (emit-and cg -16 %sp)
  (emit cg "call ~A" (attr-ref attrs 'c-function-name))
  (emit-mov cg %b %sp)
  
  (emit-set-ac-flag cg true)
  (unless (member? %closure c-callee-saved-regs) (emit-restore-%closure cg))
  (emit-convert-value cg %a dest out-frame-base))
