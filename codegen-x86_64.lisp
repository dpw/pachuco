;;; x86_64-specific code generation

;;; C calls

(define-reg-use (c-call c-function-name . args)
  (rplaca (cdr form) (list (cons 'c-function-name c-function-name)))
  (operator-args-reg-use form)
  general-register-count)

(define-codegen (c-call attrs . args)
  (when (> (length args) (length c-call-arg-regs))
    (error "too many arguments to c-call"))
  (operator-args-codegen form in-frame-base
                      (move-regs-to-front c-call-arg-regs general-registers)
                      out)
  (emit out "cld")
  (emit-set-ac-flag out false)

  ;; C ABI requires us to align stack to 16 byte bundary
  (emit-mov out %sp %b)
  (emit-and out -16 %sp)
  (emit out "call ~A" (attr-ref attrs 'c-function-name))
  (emit-mov out %b %sp)
  
  (emit-set-ac-flag out true)
  (unless (member? %closure c-callee-saved-regs)
    (emit-restore-%closure out in-frame-base))
  (emit-convert-value out %a dest in-frame-base out-frame-base))
