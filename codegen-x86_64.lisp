;;; x86_64-specific code generation

;;; Vector copy.  This is not shared with i386, because i386 has to do
;;; special things to avoid running out of register

(define-reg-use (vec-copy attrs src-addr dst-addr len)
  (operator-args-reg-use form)
  general-register-count)

(define-codegen (vec-copy attrs src-addr dst-addr len)
  (unless (dest-discard? dest)
    (error "vec-copy result not discarded"))
  (let* ((tag (attr-ref attrs 'tag))
         (scale (attr-ref attrs 'scale)))
    (operator-args-codegen form in-frame-base
                     (move-regs-to-front '(%si %di %c) general-registers)
                     out)
    (emit out "~A" (if (attr-ref attrs 'forward) "cld" "std"))
    (emit-sar out (immediate number-tag-bits) %c)
    (emit-rep-movs out scale)
    (emit-adjust-frame-base out in-frame-base out-frame-base)))

;;; C-callable program wrapper

(define c-callee-saved-regs-without-%bp (remove %bp c-callee-saved-regs))

(define (emit-program-prologue out)
  (emit out ".text")
  (emit out ".globl lisp")
  (emit out "lisp:")
  (dolist (reg c-callee-saved-regs-without-%bp) (emit-push out reg))
  (emit-mov out (first c-call-arg-regs) %alloc)
  (emit-set-ac-flag out true)
  (emit-mov out (immediate function-tag) %func)
  (emit-function-prologue out))

(define (emit-program-epilogue out)
  (emit out "leave")
  (emit out "cld")
  (emit-set-ac-flag out false)
  ;; use the alloc pointer as the result
  (emit-mov out %alloc %a)
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
  (unless (c-callee-saved %alloc) (emit-push out %alloc))
  (emit out "cld")
  (emit-set-ac-flag out false)

  ;; C ABI requires us to align stack to 16 byte bundary
  (emit-push out %bp)
  (emit-mov out %sp %bp)
  (emit-and out (immediate -16) %sp)
  (emit out "call ~A" (attr-ref attrs 'c-function-name))
  (emit out "leave")

  (emit-set-ac-flag out true)
  (unless (c-callee-saved %alloc) (emit-pop out %alloc))
  (unless (c-callee-saved %func) (emit-restore-%func out))
  (emit-convert-value out %a dest in-frame-base out-frame-base))
