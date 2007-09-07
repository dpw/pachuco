;;; Common x86 machine definition.  The real machine definitions are
;;; in mach-*.lisp, but this stuff needs to come first.

;;; Registers

(define register-operands ())

(defmarco (define-register name . variants)
  (quasiquote (definitions
    (define (unquote name) (quote (unquote name)))
    (set! register-operands (acons (unquote name) (quote (unquote variants))
                                   register-operands)))))


;;; Assembler bits

(defmarco (emit-without-flushing template . args)
  (quasiquote (format t (unquote (string-concat template "~%"))
                      (unquote-splicing args))))

(defmarco (emit out template . args)
  (quasiquote (begin
    (flush-asm-output (unquote out))
    (emit-without-flushing (unquote template) (unquote-splicing args)))))

(defmarco (emit-comment out template . args)
  (quasiquote
    (let* ((*print-pretty* false))
      (format t (unquote (string-concat "# " template "~%"))
              (unquote-splicing args)))))

(defmarco (emit-frame-push out frame-base reg)
  (quasiquote (begin
    (emit-push (unquote out) (unquote reg))
    (set! (unquote frame-base) (1+ (unquote frame-base))))))

(defmarco (emit-frame-pop out frame-base reg)
  (quasiquote (begin
    (emit-pop (unquote out) (unquote reg))
    (set! (unquote frame-base) (1- (unquote frame-base))))))

(define (emit-movzx-32 out src dest src-scale dest-scale)
  (emit out "mov~A ~A,~A"
        (elt (elt '(("b")
                    ("zbw" "w")
                    ("zbl" "zwl" "l")) dest-scale) src-scale)
        (insn-operand src src-scale)
        (insn-operand dest dest-scale)))
