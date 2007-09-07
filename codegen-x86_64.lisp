;;; x86_64-specific code generation

;;; Vector copy.  i386 has trouble with this due to lack of registers.

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

(define (emit-program-prologue out)
  (emit out ".text")
  (emit out ".globl lisp")
  (emit out "lisp:")
  (dolist (reg c-callee-saved-regs) (emit-push out reg))
  (emit-mov out %si %alloc)
  (emit-set-ac-flag out true)
  (emit-mov out (immediate function-tag) %func)
  (emit-function-prologue out))

(define (emit-program-epilogue out)
  ;; use the alloc pointer as the result
  (emit-mov out %alloc %a)
  (emit out "leave")
  (emit out "cld")
  (emit-set-ac-flag out false)
  (dolist (reg (reverse c-callee-saved-regs)) (emit-pop out reg))
  (emit out "ret"))
