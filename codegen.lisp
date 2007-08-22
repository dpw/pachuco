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
  (if (dest-discard? dest)
      (emit-adjust-frame-base out in-frame-base out-frame-base)
      (let* ((reg (destination-reg dest regs)))
        (emit-mov out (immediate (attr-ref attrs 'value)) reg)
        (emit-convert-value out reg dest in-frame-base out-frame-base))))

;;; Variables

(define-reg-use (set! varrec val)
  (max (reg-use val dest-type-value)
       (convert-value-reg-use dest-type)))

(define-codegen (set! varrec val)
  (let* ((reg (destination-reg dest regs)))
    (codegen val reg in-frame-base in-frame-base regs out)
    (emit-mov out reg (varrec-operand varrec in-frame-base))
    (emit-convert-value out reg dest in-frame-base out-frame-base)))

(define-reg-use (ref varrec) (convert-value-reg-use dest-type))

(define-codegen (ref varrec)
  (if (dest-discard? dest)
      (emit-adjust-frame-base out in-frame-base out-frame-base)
      (let* ((reg (destination-reg dest regs)))
        (emit-mov out (varrec-operand varrec in-frame-base) reg)
        (emit-convert-value out reg dest in-frame-base out-frame-base))))

;;; Operator definitions

(defmarco (define-tag-check name tag)
  (quasiquote
    (define-cc-operator ((unquote name) val) "e" ()
      ;; just check the low-order byte
      (emit-and out (immediate tag-mask) val 0)
      (emit-cmp out (immediate (unquote tag)) val 0))))

(define-tag-check function? function-tag)

;;; Function call-related internals

(define-cc-operator (check-arg-count) "e" ()
  (emit-cmp out (immediate (fixnum-representation (attr-ref attrs 'nparams)))
            %nargs))

(define-pure-operator (arg-count) result ()
  (emit-mov out %nargs result))

(define-pure-operator (raw-args-address) result ()
  (emit-lea out (param-slot 0 in-frame-base) result))

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
  (emit-mov out d (dispmem pair-tag value-size c)))

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

(define-pure-operator (primitive-make-symbol str) result ()
  (emit-mov out str (dispmem allocation-alignment 0 %alloc))
  (emit-lea out (dispmem allocation-alignment atom-tag %alloc) result)
  (emit-sub out (immediate allocation-alignment) %alloc))

;;;  Numbers

(define-tag-check number? number-tag)

(defmarco (define-simplify-binary-op op identity unary-op)
  (quasiquote
    (define-simplify ((unquote op) attrs . args)
      (simplify-recurse form)
      (cond ((null? args)
             (overwrite-form form (list 'quote (unquote identity))))
            ((null? (cdr args))
             (overwrite-form form (list* '(unquote unary-op) () args)))
            (true
             (overwrite-form form 
                             (reduce (lambda (a b) (list '(unquote op) () a b))
                                     args)))))))

(define-simplify-binary-op + 0 begin) 
(define-pure-operator (+ a b) a ()
  (emit-add out b a))

(define-simplify-binary-op * 1 begin) 
(define-pure-operator (* a b) a ()
  (emit-sar out (immediate tag-bits) a)
  (emit-imul out b a))

(define-simplify (- attrs a . args)
  (simplify-recurse form)
  (if (null? args) (rplaca form 'negate)
      (overwrite-form form
                      (reduce (lambda (a b) (list '- () a b)) (cons a args)))))

(define-pure-operator (negate a) a ()
  (emit-neg out a))

(define-pure-operator (- a b) a ()
  (emit-sub out b a))

(define (div-operator-reg-use form dest-type)
  (if (dest-type-discard? dest-type)
      (operator-args-reg-use-discarding form)
      (begin
        (operator-args-reg-use form)
        general-register-count)))

(define-reg-use (truncate attrs a b)
  (div-operator-reg-use form dest-type))

(define-codegen (truncate attrs a b)
  (if (dest-discard? dest)
      (operator-args-codegen-discarding form in-frame-base out-frame-base
                                        general-registers out)
      (begin
        (operator-args-codegen form in-frame-base general-registers out)
        (emit-mov out %a %d)
        (emit-sar out (immediate 63) %d)
        (emit-idiv out %c)
        (emit-shl out (immediate tag-bits) %a)
        (emit-convert-value out %a dest in-frame-base out-frame-base))))

(define-reg-use (rem attrs a b)
  (div-operator-reg-use form dest-type))

(define-codegen (rem attrs a b)
  (if (dest-discard? dest)
      (operator-args-codegen-discarding form in-frame-base out-frame-base 
                                        general-registers out)
      (begin
        (operator-args-codegen form in-frame-base general-registers out)
        (emit-mov out %a %d)
        (emit-sar out (immediate 63) %d)
        (emit-idiv out %c)
        (emit-convert-value out %d dest in-frame-base out-frame-base))))

;;; Strings and vectors

(define-tag-check string? string-tag)
(define-tag-check vector? vector-tag)

(define-pure-operator (make-vec len) result (raw-len)
  (let* ((tag (attr-ref attrs 'tag))
         (scale (attr-ref attrs 'scale)))
    (if (= scale tag-bits)
      (begin
        (emit-sub out (immediate value-size) %alloc)
        (emit-sub out len %alloc)
        (unless (= allocation-alignment-scale scale)
          (emit-and out (immediate allocation-alignment-mask) %alloc))
        (emit-mov out len (dispmem 0 0 %alloc))
        (emit-lea out (dispmem 0 tag %alloc) result))
      (begin
        (emit-mov out len raw-len)
        (emit-sar out (immediate (- tag-bits scale)) raw-len)
        (emit-sub out (immediate value-size) %alloc)
        (emit-sub out raw-len %alloc)
        (emit-and out (immediate allocation-alignment-mask) %alloc)
        (emit-mov out len (dispmem 0 0 %alloc))
        (emit-lea out (dispmem 0 tag %alloc) result)))))

(define-pure-operator (vec-length vec) result ()
  (emit-mov out (dispmem (attr-ref attrs 'tag) 0 vec) result))

(define-pure-operator (raw-vec-address vec index) result ()
  (let* ((tag (attr-ref attrs 'tag))
         (scale (attr-ref attrs 'scale)))
    (unless (= scale tag-bits)
      (emit-sar out (immediate (- tag-bits scale)) index))
    (emit-lea out (dispmem tag value-size vec index) result)))

(define-pure-operator (vec-ref vec index) result ()
  (let* ((tag (attr-ref attrs 'tag))
         (scale (attr-ref attrs 'scale)))
    (unless (= scale tag-bits)
      (emit-sar out (immediate (- tag-bits scale)) index))
    (emit-movzx out (dispmem tag value-size vec index) result scale)))

(define-operator (vec-set! vec index val) val ()
  (let* ((tag (attr-ref attrs 'tag))
         (scale (attr-ref attrs 'scale)))
    (unless (= scale tag-bits)
      (emit-sar out (immediate (- tag-bits scale)) index))
    (emit-mov out val (dispmem tag value-size vec index) scale)))

(define-reg-use (vec-copy attrs src src-index dst dst-index len)
  (operator-args-reg-use form)
  general-register-count)

(define-codegen (vec-copy attrs src src-index dst dst-index len)
  (let* ((regs (list            %si %a        %di  %d         %c))
         (tag (attr-ref attrs 'tag))
         (scale (attr-ref attrs 'scale)))
    (operator-args-codegen form in-frame-base regs out)
    (emit out "~A" (if (attr-ref attrs 'forward) "cld" "std"))
    (unless (= scale tag-bits) (emit-sar out (immediate (- tag-bits scale)) %a))
    (emit-lea out (dispmem tag value-size %si %a) %si)
    (unless (= scale tag-bits) (emit-sar out (immediate (- tag-bits scale)) %d))
    (emit-lea out (dispmem tag value-size %di %d) %di)
    (emit-sar out (immediate tag-bits) %c)
    (emit-rep-movs out scale)
    (if (dest-discard? dest)
        (emit-adjust-frame-base out in-frame-base out-frame-base)
        (let* ((result (destination-reg dest regs)))
          (emit-mov out (immediate unspecified-representation) result)
          (emit-convert-value out result dest in-frame-base out-frame-base)))))

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

(define-pure-operator (fixnum->raw val) val ()
  (emit-sar out (immediate tag-bits) val))

(define-pure-operator (raw->fixnum val) val ()
  (emit-shl out (immediate tag-bits) val))

(define (emit-set-ac-flag out enable)
  (emit-pushf out)
  (if enable
      (emit-or out (immediate #x40000) (mem %sp) 2)
      (emit-and out (immediate #xfffbffff) (mem %sp) 2))
  (emit-popf out))

(define-reg-use (c-call c-function-name . args)
  (rplaca (cdr form) (list (cons 'c-function-name c-function-name)))
  (operator-args-reg-use form)
  general-register-count)

(define-codegen (c-call attrs . args)
  (let* ((regs (list %di %si %d %c)))
    (operator-args-codegen form in-frame-base regs out)
    (emit out "cld")
    (emit-set-ac-flag out false)
    ;;; XXX should align stack to 16 byte bundary
    (emit out "call ~A" (attr-ref attrs 'c-function-name))
    (emit-set-ac-flag out true)
    (emit-convert-value out %a dest in-frame-base out-frame-base)))

(define-reg-use (raw-apply-with-args attrs nargs bodyfunc)
  (reg-use nargs dest-type-value)
  ;; raw-apply-with-args is only intended for restricted
  ;; circumstances, so we make this assumption:
  (unless (< (reg-use bodyfunc dest-type-value) general-register-count)
    (error "all registers needed for ~S" bodyfunc))
  general-register-count)

(define-codegen (raw-apply-with-args attrs nargs bodyfunc)
  (let* ((result (destination-reg dest general-registers))
         (saved-sp (first (remove result general-registers))))
    (codegen nargs (dest-value %nargs) in-frame-base in-frame-base
             general-registers out)
    (codegen bodyfunc (dest-value %func) in-frame-base in-frame-base
             (remove %nargs general-registers) out)
    (emit-mov out %sp saved-sp)
    (emit-sub out %nargs %sp)
    (emit-clear out %nargs)
    (emit-push out saved-sp)
    (emit out "call *~A" (usual-register (dispmem function-tag 0 %func)))
    (emit-pop out saved-sp)
    (emit-mov out saved-sp %sp)
    ;; Restore %func
    (emit-mov out (dispmem (* in-frame-base value-size) 0 saved-sp) %func)
    (emit-convert-value out result dest in-frame-base out-frame-base)))

(define-reg-use (raw-apply-jump attrs func nargs)
  ;; raw-apply-call is only intended for restricted circumstances, so
  ;; we make this assumption:
  (unless (< (reg-use func dest-type-value) general-register-count)
    (error "all registers needed for ~S" func))
  (reg-use func dest-type-value)
  general-register-count)

(define-codegen (raw-apply-jump attrs func nargs)
  (codegen nargs (dest-value %nargs) in-frame-base in-frame-base
           general-registers out)
  (codegen func (dest-value %func) in-frame-base in-frame-base
           (remove %nargs general-registers) out)
  (emit-adjust-frame-base out in-frame-base -1)
  (emit out "jmp *~A" (usual-register (dispmem function-tag 0 %func))))

(define-operator (raw-arg-set! args-base index val) val ()
  (emit-mov out val (dispmem 0 0 args-base index)))

(define-pure-operator (raw-arg-ref args-base index) result ()
  (emit-mov out (dispmem 0 0 args-base index) result))
