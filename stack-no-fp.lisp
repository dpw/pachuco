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

(define (return-address-slot cg)
  (mem %sp (+ (codegen-frame-base cg) (if (codegen-have-closure cg) 1 0))))

(define (closure-address-slot cg)
  (mem %sp (codegen-frame-base cg)))

;;; Functions, calls, returns, etc.

(define (codegen-function label closure-size body cg) 
  (codegen-function-intro label closure-size cg)
  (when (codegen-have-closure cg) (emit-push cg %closure))
  (codegen body dest-discard (if (codegen-have-closure cg) -1 0)
           general-registers cg))

(define-codegen (return attrs body)
  (codegen body (dest-value %funcres) out-frame-base general-registers cg)
  (emit-ret cg (* value-size (attr-ref attrs 'nparams))))

(define-codegen (varargs-return attrs arg-count body)
  (let* ((regs-without-%funcres (remove %funcres general-registers))
         (retaddr (return-address-slot cg))
         (arg-count-reg (first regs-without-%funcres))
         (retaddr-reg (second regs-without-%funcres)))
    (operator-args-codegen form
                  (list* arg-count-reg %funcres (cdr regs-without-%funcres)) cg)
    ;; We need to clean up the stack before returning, but the return
    ;; address is on the top.  And we can't simply pop the return
    ;; address and later do an indirect branch to it, because that is
    ;; bad for branch prediction.  So we copy the return address into
    ;; to the top of the argument area, then return
    (emit-scale-number cg value-scale arg-count-reg)
    (emit-mov cg retaddr retaddr-reg)
    (emit-lea cg (mem retaddr arg-count-reg) %sp)
    (emit-mov cg retaddr-reg (mem %sp))
    (emit cg "ret")))

(define (codegen-tail-call func retaddr out-arg-count out-retaddr regs cg)
  (let* ((tmpreg (first regs))
         (retaddr-reg (second regs)))
    (emit-mov cg retaddr retaddr-reg)
    (copy-tail-call-args out-arg-count out-retaddr tmpreg cg)
    (emit-lea cg (mem out-retaddr) %sp)
    (emit-mov cg retaddr-reg (mem %sp))
    ;; setting up %nargs must be the last thing we do, since %nargs is
    ;; in general-regs and so might be the same as one of our temp
    ;; regs
    (emit-mov cg (fixnum-representation out-arg-count) %nargs)
    (emit-call-or-jump cg "jmp" func)))

(define-codegen (tail-call attrs func . args)
  (codegen-call-args cg func args)
  (let* ((in-arg-count (attr-ref attrs 'nparams))
         (out-arg-count (length args))
         (retaddr (return-address-slot cg))
         (out-retaddr (mem retaddr (- in-arg-count out-arg-count))))
    (if (= in-arg-count out-arg-count)
        (begin 
          (copy-tail-call-args out-arg-count out-retaddr 
                               (first general-registers) cg)
          (emit-reset-frame-base cg out-frame-base)
          ;; setting up %nargs must be the last thing we do, since
          ;; %nargs is in general-regs and so might be the same as one
          ;; of our temp regs
          (emit-mov cg (fixnum-representation out-arg-count) %nargs)
          (emit-call-or-jump cg "jmp" func))
        (codegen-tail-call func retaddr out-arg-count out-retaddr
                           general-registers cg))))

(define-codegen (varargs-tail-call attrs arg-count func . args)
  (codegen-call-args cg func args)
  (let* ((in-arg-count-reg (first general-registers))
         (out-arg-count (length args))
         (retaddr (return-address-slot cg)))
    ;; here we assume that the arg-count is just a ref, and so won't
    ;; access %closure
    (codegen arg-count (dest-value in-arg-count-reg)
             (codegen-frame-base cg) general-registers cg)
    (emit-scale-number cg value-scale in-arg-count-reg)
    (emit-lea cg (mem retaddr in-arg-count-reg (- out-arg-count)) 
              in-arg-count-reg)
    (codegen-tail-call func retaddr out-arg-count in-arg-count-reg 
                       (cdr general-registers) cg)))

;;; Apply support

(define-codegen (raw-jump-with-arg-space attrs before-arg-count after-arg-count
                                         bodyfunc)
  (operator-args-codegen form regs cg)
  (bind (before-arg-count after-arg-count bodyfunc retaddr-reg . others) regs
    (let* ((retaddr (return-address-slot cg)))
      ;; calculate how far up to move %sp
      (emit-sub cg after-arg-count before-arg-count)
      (emit-mov cg bodyfunc %closure)
      (emit-scale-number cg value-scale before-arg-count)
      (emit-mov cg retaddr retaddr-reg)
      (emit-lea cg (mem retaddr before-arg-count) %sp)
      (emit-mov cg retaddr-reg (mem %sp))
      (emit-clear cg %nargs)
      (emit cg "jmp *~A" (value-sized (tagged-mem closure-tag bodyfunc))))))

(define-codegen (raw-apply-jump attrs func arg-count)
  (let* ((regs-without-%nargs (remove %nargs regs))
         (func (first regs-without-%nargs)))
    (operator-args-codegen form (list* func %nargs (cddr regs-without-%nargs))
                           cg)
    (emit-reset-frame-base cg out-frame-base)
    (emit-mov cg func %closure)
    (emit cg "jmp *~A" (value-sized (tagged-mem closure-tag %closure)))))

;;; C-callable program wrapper

(define (codegen-program program)
  (let* ((cg (make-codegen)))
    (codegen-program-sections program cg)
    (reg-use program dest-type-discard)
    
    (emit cg ".text")
    (emit cg ".globl lisp")
    (emit cg "lisp:")
    (dolist (reg c-callee-saved-regs) (emit-push cg reg))
    (emit-set-ac-flag cg true)

    (codegen-set-frame-base! cg 0)
    (codegen-set-have-closure! cg false)
    (codegen program dest-discard 0 general-registers cg)

    (emit-set-ac-flag cg false)
    (emit cg "cld")
    (dolist (reg (reverse c-callee-saved-regs)) (emit-pop cg reg))
    (emit cg "ret")))
