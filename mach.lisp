;;; Common x86 machine definition.  The real machine definitions are
;;; in mach-*.lisp, but this stuff needs to come first.

;;; Registers

(defmarco (define-register name . variants)
  (quasiquote 
    (define (unquote name)
      (make-vector-from-list (quote (unquote variants))))))

(define general-registers ())
(define general-register-count 0)

;;; %nargs is used to pass the number of arguments to functions.  We
;;; reuse one of the general-registers for this, which means we have
;;; to be really careful about invoking the operators that use it
;;; (check-arg-count, arg-count).
(define %nargs)

(define (add-general-registers regs)
  (set! general-register-count (+ general-register-count (length regs)))
  (set! general-registers (nconc general-registers regs))
  (set! %nargs (last-elem general-registers)))

;;; Assembler bits

(defmarco (emit-without-flushing template . args)
  (quasiquote
    (let* ((*print-pretty* false))
      (format~ true (unquote (string-concat template "~%"))
               (unquote-splicing args)))))

(defmarco (emit cg template . args)
  (quasiquote (begin
    (flush-labels-and-jumps (unquote cg))
    (emit-without-flushing (unquote template) (unquote-splicing args)))))

(defmarco (emit-comment cg template . args)
  (quasiquote
    (let* ((*print-pretty* false))
      (format~ true (unquote (string-concat "# " template "~%"))
               (unquote-splicing args)))))

(define (emit-movzx-32 cg src dest src-scale dest-scale)
  (emit cg "mov~A ~A,~A"
        (elt (elt '(("b")
                    ("zbw" "w")
                    ("zbl" "zwl" "l")) dest-scale) src-scale)
        (insn-operand src src-scale)
        (insn-operand dest dest-scale)))

(defmarco (with-saved-frame-base cg . body)
  (let* ((orig-frame-base (gensym)))
    (quasiquote
      (let* (((unquote orig-frame-base) (codegen-frame-base (unquote cg))))
        (unquote-splicing body)
        (codegen-set-frame-base! (unquote cg) (unquote orig-frame-base))))))