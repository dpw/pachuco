;;; i386-specific code generation

;;; Vector copy.  We don't share this with x86-64, because we have to
;;; avoid running out of registers

(define-reg-use (vec-copy attrs src-addr dst-addr len)
  (operator-args-reg-use form)
  general-register-count)

(define-codegen (vec-copy attrs src-addr dst-addr len)
  (unless (dest-discard? dest)
    (error "vec-copy result not discarded"))
  (let* ((tag (attr-ref attrs 'tag))
         (scale (attr-ref attrs 'scale)))
    (operator-args-codegen form in-frame-base
                     (move-regs-to-front '(%a %di %c) general-registers)
                     out)
    ;; %alloc is %si
    (emit-push out %si)
    (emit-mov out %a %si)
    (emit out "~A" (if (attr-ref attrs 'forward) "cld" "std"))
    (emit-sar out (immediate number-tag-bits) %c)
    (emit-rep-movs out scale)
    (emit-pop out %si)
    (emit-adjust-frame-base out in-frame-base out-frame-base)))

;;; C-callable program wrapper

(define c-callee-saved-regs-without-%bp (remove %bp c-callee-saved-regs))

(define (emit-program-prologue out)
  (emit out ".text")
  (emit out ".globl lisp")
  (emit out "lisp:")
  (emit-push out %bp)
  (emit-mov out %sp %bp)
  (dolist (reg c-callee-saved-regs-without-%bp) (emit-push out reg))
  (emit-mov out (dispmem 0 (* 2 value-size) %bp) %alloc)
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
    (unless (c-callee-saved %alloc) (emit-pop out %alloc))
    (unless (c-callee-saved %func) (emit-restore-%func out))
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
