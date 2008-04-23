;;; Stack handling, without a frame pointer

;;; %bp is available as a general register
(add-general-registers (list %bp))

;;; Stack layout:
;;;
;;; param N
;;; ...
;;; param 1
;;; param 0
;;; Return address
;;; %closure  <--- (mem %sp frame-base)
;;; Local var 0
;;; ...
;;; Local var N
;;; in-progress param N
;;; in-progress param N-1
;;; ...
;;;
;;; Functions are called with the closure in %closure, arg-count in
;;; %nargs.  They return with the result in %funcres.

(define (param-slot index frame-base)
  (mem %sp (+ 2 index frame-base)))

(define (local-slot index frame-base)
  (mem %sp (- frame-base index 1)))

(define (return-address-slot frame-base)
  (mem %sp (+ 1 frame-base)))

(define (closure-address-slot frame-base)
  (mem %sp (+ 0 frame-base)))

;;; Functions, calls, returns, etc.

(define (codegen-function label body out) 
  (emit out ".text")
  (emit-label out label)
  (emit-push out %closure)
  (codegen body dest-discard 0 -1 general-registers out))

(define-codegen (return attrs body)
  (codegen body (dest-value %funcres) in-frame-base out-frame-base
           general-registers out)
  (emit-ret out (* value-size (attr-ref attrs 'nparams))))

(define-codegen (varargs-return attrs arg-count body)
  (let* ((regs-without-%funcres (remove %funcres general-registers))
         (retaddr (return-address-slot in-frame-base))
         (arg-count-reg (first regs-without-%funcres))
         (retaddr-reg (second regs-without-%funcres)))
    (operator-args-codegen form in-frame-base
                 (list* arg-count-reg %funcres (cdr regs-without-%funcres)) out)
    ;; We need to clean up the stack before returning, but the return
    ;; address is on the top.  And we can't simply pop the return
    ;; address and later do an indirect branch to it, because that is
    ;; bad for branch prediction.  So we copy the return address into
    ;; to the top of the argument area, then return
    (emit-scale-number out value-scale arg-count-reg)
    (emit-mov out retaddr retaddr-reg)
    (emit-lea out (mem retaddr arg-count-reg) %sp)
    (emit-mov out retaddr-reg (mem %sp))
    (emit out "ret")))

(define (codegen-tail-call func retaddr out-arg-count out-retaddr regs out)
  (let* ((tmpreg (first regs))
         (retaddr-reg (second regs)))
    (emit-mov out retaddr retaddr-reg)
    (copy-tail-call-args out-arg-count out-retaddr tmpreg out)
    (emit-lea out (mem out-retaddr) %sp)
    (emit-mov out retaddr-reg (mem %sp))
    ;; setting up %nargs must be the last thing we do, since %nargs is
    ;; in general-regs and so might be the same as one of our temp
    ;; regs
    (emit-mov out (fixnum-representation out-arg-count) %nargs)
    (emit-call-or-jump out "jmp" func)))

(define-codegen (tail-call attrs func . args)
  (let* ((new-frame-base (codegen-call-args out func args in-frame-base))
         (in-arg-count (attr-ref attrs 'nparams))
         (out-arg-count (length args))
         (retaddr (return-address-slot new-frame-base))
         (out-retaddr (mem retaddr (- in-arg-count out-arg-count))))
    (if (= in-arg-count out-arg-count)
        (begin 
          (copy-tail-call-args out-arg-count out-retaddr 
                               (first general-registers) out)
          (emit-adjust-frame-base out new-frame-base out-frame-base)
          ;; setting up %nargs must be the last thing we do, since
          ;; %nargs is in general-regs and so might be the same as one
          ;; of our temp regs
          (emit-mov out (fixnum-representation out-arg-count) %nargs)
          (emit-call-or-jump out "jmp" func))
        (codegen-tail-call func retaddr out-arg-count out-retaddr
                           general-registers out))))

(define-codegen (varargs-tail-call attrs arg-count func . args)
  (let* ((new-frame-base (codegen-call-args out func args in-frame-base))
         (in-arg-count-reg (first general-registers))
         (out-arg-count (length args))
         (retaddr (return-address-slot new-frame-base)))
    ;; here we assume that the arg-count is just a ref, and so won't
    ;; access %closure
    (codegen arg-count (dest-value in-arg-count-reg)
             new-frame-base new-frame-base general-registers out)
    (emit-scale-number out value-scale in-arg-count-reg)
    (emit-lea out (mem retaddr in-arg-count-reg (- out-arg-count)) 
              in-arg-count-reg)
    (codegen-tail-call func retaddr out-arg-count in-arg-count-reg 
                       (cdr general-registers) out)))

;;; Apply support

(define-codegen (raw-jump-with-arg-space attrs before-arg-count after-arg-count
                                         bodyfunc)
  (operator-args-codegen form in-frame-base regs out)
  (bind (before-arg-count after-arg-count bodyfunc retaddr-reg . others) regs
    (let* ((retaddr (return-address-slot in-frame-base)))
      ;; calculate how far up to move %sp
      (emit-sub out after-arg-count before-arg-count)
      (emit-mov out bodyfunc %closure)
      (emit-scale-number out value-scale before-arg-count)
      (emit-mov out retaddr retaddr-reg)
      (emit-lea out (mem retaddr before-arg-count) %sp)
      (emit-mov out retaddr-reg (mem %sp))
      (emit-clear out %nargs)
      (emit out "jmp *~A" (value-sized (tagged-mem closure-tag bodyfunc))))))

(define-codegen (raw-apply-jump attrs func arg-count)
  (let* ((regs-without-%nargs (remove %nargs regs))
         (func (first regs-without-%nargs)))
    (operator-args-codegen form in-frame-base
                           (list* func %nargs (cddr regs-without-%nargs)) out)
    (emit-adjust-frame-base out in-frame-base out-frame-base)
    (emit-mov out func %closure)
    (emit out "jmp *~A" (value-sized (tagged-mem closure-tag %closure)))))

;;; C-callable program wrapper

(define (codegen-program program out)
  (codegen-sections program out)
  (reg-use program dest-type-discard)

  (emit out ".text")
  (emit out ".globl lisp")
  (emit out "lisp:")
  (dolist (reg c-callee-saved-regs) (emit-push out reg))
  (emit-set-ac-flag out true)
  (emit-mov out closure-tag %closure)
  (emit-push out %closure)
  (codegen program dest-discard 0 -1 general-registers out)
  (emit-set-ac-flag out false)
  (emit out "cld")
  (dolist (reg (reverse c-callee-saved-regs)) (emit-pop out reg))
  (emit out "ret"))
