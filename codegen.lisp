;;; Machine-specific code generation

;;; Literals

(define (codegen-quoted quoted out)
  (cond ((pair? quoted) (codegen-quoted-pair quoted out))
        ((number? quoted) (fixnum-representation quoted))
        ((characterp quoted) (fixnum-representation (char-code quoted)))
        ((string? quoted) (codegen-quoted-string quoted out))
        (true (let* ((c (assoc quoted simple-representations)))
             (cond (c (cdr c))
                   ((symbol? quoted) (codegen-quoted-symbol quoted out))
                   (t (error "unrecognised quoted form ~S" quoted)))))))

(define (codegen-quoted-pair quoted out)
  (let* ((label (gen-label))
         (a (codegen-quoted (car quoted) out))
         (d (codegen-quoted (cdr quoted) out)))
    (emit out ".data")
    (emit out ".align ~D" allocation-alignment)
    (emit-label out label)
    (emit-literal out a)
    (emit-literal out d)
    (format nil "~A+~D" label pair-tag)))

(define (codegen-quoted-string str out)
  (let* ((label (gen-label)))
    (emit out ".data")
    (emit out ".align ~D" allocation-alignment)
    (emit-label out label)
    (emit-literal out (fixnum-representation (length str)))
    (emit out ".ascii ~S" str)
    (format nil "~A+~D" label string-tag)))

(define emitted-symbols ())

(define (codegen-quoted-symbol sym out)
  (let ((emitted (assoc sym emitted-symbols)))
    (if emitted (cdr emitted)
        (let* ((label (gen-label))
               (name (codegen-quoted-string (subject-language-symbol-name sym)
                                            out)))
          (emit out ".data")
          (emit out ".align ~D" allocation-alignment)
          (emit-label out label)
          (emit-literal out name)
          (let* ((lit (format nil "~A+~D" label atom-tag)))
            (set! emitted-symbols (acons sym lit emitted-symbols))
            lit)))))

;;; Quote

(define-reg-use (quote attrs) (convert-value-reg-use dest-type))

(define-codegen (quote attrs)
  (unless (dest-discard? dest)
    (let* ((reg (destination-reg dest regs)))
    (emit-mov out (immediate (attr-ref attrs 'value)) reg)
    (emit-convert-value out reg dest))))

;;; Variables

(define-reg-use (set! varrec val)
  (max (reg-use val dest-type-value)
       (convert-value-reg-use dest-type)))

(define-codegen (set! varrec val)
  (let* ((reg (destination-reg dest regs)))
    (codegen val reg regs frame-base out)
    (emit-mov out reg (varrec-operand varrec frame-base))
    (emit-convert-value out reg dest)))

(define-reg-use (ref varrec) (convert-value-reg-use dest-type))

(define-codegen (ref varrec)
  (unless (dest-discard? dest)
    (let* ((reg (destination-reg dest regs)))
      (emit-mov out (varrec-operand varrec frame-base) reg)
      (emit-convert-value out reg dest))))

;;; Operator definitions

(defmarco (define-tag-check name tag)
  (quasiquote
    (define-cc-operator ((unquote name) val) "e" ()
      ;; just check the low-order byte
      (emit-and out (immediate tag-mask) val 0)
      (emit-cmp out (immediate (unquote tag)) val 0))))

(define-tag-check function? function-tag)

;;; Function call-related internals

(define-cc-operator (check-arg-count nparams) "e" ()
  (emit-cmp out nparams %nargs))

(define-pure-operator (arg-count) result ()
  (emit-mov out %nargs result))

(define-pure-operator (args-pointer) result ()
  (emit-lea out (param-slot 0 frame-base pointer-tag) result))

;;; Comparisons

(defmarco (define-cmp-operator name cc)
  (quasiquote (define-cc-operator ((unquote name) a b) (unquote cc) () 
                (emit-cmp out b a))))
 
(define-cmp-operator eq? "e")
(define-cmp-operator = "e")
(define-cmp-operator /= "ne")
(define-cmp-operator > "g")
(define-cmp-operator >= "ge")
(define-cmp-operator < "l")
(define-cmp-operator <= "le")

;;; Conses

(define-tag-check pair? pair-tag)

(define-simplify (null? attrs val)
  (overwrite-form form (list 'eq? attrs val '(quote ()))))

(define-pure-operator (cons a d) result ()
  (emit-mov out a (dispmem pair-size 0 %alloc))
  (emit-mov out d (dispmem pair-size value-size %alloc))
  (emit-lea out (dispmem pair-size pair-tag %alloc) result)
  (emit-sub out (immediate pair-size) %alloc))

(define-pure-operator (car a) result ()
  (emit-mov out (dispmem pair-tag 0 a) result))

(define-pure-operator (cdr a) result ()
  (emit-mov out (dispmem pair-tag value-size a) result))

(define-operator (rplaca c a) c ()
  (emit-mov out a (dispmem pair-tag 0 c)))

(define-operator (rplacd c d) c ()
  (emit-mov out d (dispmem pair-tag 0 c)))

;;; Boxes

(define-pure-operator (make-box val) result ()
  (emit-mov out val (dispmem box-size 0 %alloc))
  (emit-lea out (dispmem box-size box-tag %alloc) result)
  (emit-sub out (immediate box-size) %alloc))

(define-operator (box-set! box val) val ()
  (emit-mov out val (dispmem box-tag 0 box)))

(define-operator (box-ref box) result ()
  (emit-mov out (dispmem box-tag 0 box) result))

;;; Symbols

(define-cc-operator (symbol? val) "e" ()
  (let* ((l (gen-label)))
    (emit-cmp out (immediate lowest-symbol-representation) val)
    (emit-jcc out "l" l)
    (emit-and out (immediate tag-mask) val 0)
    (emit-cmp out (immediate atom-tag) val 0)
    (emit-label out l)))

(define-pure-operator (symbol-name sym) result ()
  (emit-mov out (dispmem atom-tag 0 sym) result))

;;;  Numbers

(define-tag-check number? number-tag)

;(define-pure-operator (+ a b) a ()
;  (emit-add out b a))

;;; Misc. runtime

(define-reg-use (error-halt attrs message args) 0)
(define-codegen (error-halt attrs message args)
  (let* ((l (gen-label)))
    (emit-label out l)
    (emit-comment out "error-halt: ~S"
                  (if (eq? 'quote (first message)) 
                      (form-attr message 'quoted)
                      "unknown"))
    (emit out "hlt")
    (emit-jump out l)))
