;;; Code generation

(define label-counter 0)

(define (gen-label)
  (format~ false ".L~D" (set! label-counter (1+ label-counter))))

(define (emit-data out label scale)
  (emit out ".data")
  (emit out ".align ~D" (ash 1 scale))
  (emit-label out label))

(define (escape-string-literal str)
  (string-replace (string-replace (string-replace str "\\" "\\\\")
                                  "\"" "\\\"") "
" "\\n"))

;;; Generic representation bits

(define (fixnum-representation n) (ash n number-tag-bits))
(define (low-bits-mask bits) (1- (ash 1 bits)))

;;; Registers and address modes
;;;
;;; - immediates are simply numbers or strings (representing assembler
;;; labels)
;;;
;;; - registers a vector of strings, giving the name of the register
;;; for the various scales
;;;
;;; - a memory reference a list of the form (offset regs...)

(define (immediate? x) (or (number? x) (string? x)))
(define (register? x) (vector? x))
(define (mem? x) (pair? x))

(define (mem1 x)
  (cond ((register? x) (list 0 x))
        ((immediate? x) (list x))
        ((mem? x) x)
        (true (error "bad operand ~S" x))))

(define (mem-list args)
  (let* ((n 0) (regs ()))
    (dolist (arg args)
      (cond ((number? arg)
             (set! n (+ n (* value-size arg))))
            ((string? arg)
             (unless (eq? n 0) (error "adding label to number"))
             (set! n arg))
            ((register? arg)
             (set! regs (nconc regs (list arg))))
            ((pair? arg)
             (set! n (+ n (car arg)))
             (set! regs (append regs (copy-list (cdr arg)))))
            (true
             (error "bad operand ~S" arg))))

    (when (> (length regs) 2) (error "too many registers"))
    (cons n regs)))

(define (mem . args)
  (mem-list args))

(define (mem1+ m n)
  (let* ((memm (mem m)))
    (cons (+ (car memm) n) (cdr memm))))

(define (tagged-mem tag . args)
  (let* ((m (mem-list args)))
    (cons (- (car m) tag) (cdr m))))

(define (insn-operand x scale)
  (cond ((immediate? x) (format~ false "$~A" x))
        ((vector? x) (vector-ref x scale))
        ((pair? x)
         (cond ((null? (cdr x)) (car x))
               ((null? (cddr x))
                (format~ false "~A(~A)" (if (eq? 0 (car x)) "" (car x))
                         (vector-ref (second x) value-scale)))
               (true
                (format~ false "~A(~A,~A)" (if (eq? 0 (car x)) "" (car x))
                         (vector-ref (second x) value-scale)
                         (vector-ref (third x) value-scale)))))
        (true (error "strange operand ~S" x))))

(define (value-sized operand)
  (insn-operand operand value-scale))

(define (move-regs-to-front regs all-regs)
  (append regs (filterfor (reg all-regs) (not (member? reg regs)))))

;;; Condition codes

(define (negate-cc cc)
  (if (eq? (string-ref cc 0) #\n)
      (substring cc 1 (1- (string-length cc)))
      (string-concat "n" cc)))

;;; Instructions

(defconstant value-insn-size-suffix (insn-size-suffix value-scale))

(defmarco (define-insn-2 name insn)
  (quasiquote
    (define ((unquote name) out src dest . scale)
      (emit-insn-2 out (unquote insn) src dest scale))))

(define (emit-insn-2 out insn src dest scale)
  (set! scale (if (null? scale) value-scale (car scale)))
  (emit out "~A~A ~A, ~A" insn (insn-size-suffix scale)
        (insn-operand src scale) (insn-operand dest scale)))

(define-insn-2 emit-lea "lea")
(define-insn-2 emit-add "add")
(define-insn-2 emit-sub "sub")
(define-insn-2 emit-imul "imul")
(define-insn-2 emit-and "and")
(define-insn-2 emit-or "or")
(define-insn-2 emit-xor "xor")
(define-insn-2 emit-cmp "cmp")
(define-insn-2 emit-shl "shl")
(define-insn-2 emit-sar "sar")

(define (emit-clear out reg . scale)
  (set! scale (if (null? scale) value-scale (car scale)))
  (emit-xor out reg reg (min 2 scale)))

(define (emit-mov out src dest . scale)
  (cond ((and (eq? src 0) (register? dest))
         (emit-clear out dest))
        ((and (number? src) (register? dest) (> src 0) (< src 1000000))
         (emit-movzx out src dest
                     (min 2 (if (null? scale) value-scale (car scale)))))
        (true
         (emit-insn-2 out "mov" src dest scale))))

(define (emit-push out reg)
  (emit out "push~A ~A" value-insn-size-suffix (value-sized reg)))

(define (emit-pop out reg)
  (emit out "pop~A ~A" value-insn-size-suffix (value-sized reg)))

(define (emit-set out cc reg)
  (emit out "set~A ~A" cc (insn-operand reg 0)))

(defmarco (define-insn-1 name insn)
  (quasiquote
    (define ((unquote name) out oper . scale)
      (emit-insn-1 out (unquote insn) oper
                   (if (null? scale) value-scale (car scale))))))

(define (emit-insn-1 out insn oper scale)
  (emit out "~A~A ~A" insn (insn-size-suffix scale) (insn-operand oper scale)))

(define-insn-1 emit-neg "neg")
(define-insn-1 emit-not "not")
(define-insn-1 emit-idiv "idiv")

(define (emit-ret out imm)
  (if (= 0 imm)
      (emit out "ret")
      (emit out "ret $~D" imm)))

(defmarco (define-insn-0 name insn)
  (quasiquote
    (define ((unquote name) out . scale)
      (emit out "~A~A" (unquote insn)
            (insn-size-suffix (if (null? scale) value-scale (car scale)))))))

(define-insn-0 emit-rep-movs "rep ; movs")
(define-insn-0 emit-pushf "pushf")
(define-insn-0 emit-popf "popf")

(define (emit-scale-number out scale oper)
  (unless (= scale number-tag-bits)
    (if (< scale number-tag-bits)
        (emit-sar out (- number-tag-bits scale) oper)
        (emit-shl out (- scale number-tag-bits) oper))))

;;; Branching, jumping, and output handling

(define (make-asm-output) (list false ()))

(define (optimizing-jumps? out) (car out))

(define (merge-pending-labels out to-label)
  (rplacd (cdr out) (nconc (nmapfor (l (second out)) (cons l to-label))
                           (cddr out)))
  (rplaca (cdr out) ()))

(define (scan-merged-labels merged-prev l labels)
  (let* ((merged (cdr merged-prev)))
    (if (null? merged) labels
        (if (eq? l (cdar merged))
            (begin
              (rplacd merged-prev (cdr merged))
              (scan-merged-labels merged-prev l (cons (caar merged) labels)))
            (scan-merged-labels merged l labels)))))

(define (take-merged-labels out l)
  (scan-merged-labels (cdr out) l ()))

(define (pend-label out l)
  (rplaca (cdr out) (nconc (take-merged-labels out l)
                           (cons l (second out)))))

(define (emit-label out l)
  (if (optimizing-jumps? out)
      (pend-label out l)
      (dolist (ml (cons l (take-merged-labels out l)))
        (emit-without-flushing "~A:" l))))

(define (emit-jump out label)
  (if (optimizing-jumps? out)
      (merge-pending-labels out label)
      (rplaca out label)))

(define (emit-branch out cc conddest)
  (flush-asm-output out)
  (rplaca out (list cc (dest-conditional-tlabel conddest)
                    (dest-conditional-flabel conddest))))

(define (emit-jcc out cc label)
  (emit-without-flushing "j~A ~A" cc label))

(define (emit-jmp label)
  (emit-without-flushing "jmp ~A" label))

(define (flush-asm-output out)
  (when (optimizing-jumps? out)
    (let* ((branch (first out))
           (pending-labels (second out)))
      (if (pair? branch)
          (let* ((cc (first branch))
                 (tlabel (second branch))
                 (flabel (third branch)))
            (if (member? tlabel pending-labels)
                (unless (member? flabel pending-labels)
                  (emit-jcc out (negate-cc cc) flabel))
                (begin
                 (emit-jcc out cc tlabel)
                 (unless (member? flabel pending-labels)
                   (emit-jmp flabel)))))
          (unless (member? branch pending-labels)
            (emit-jmp branch)))

      (if (null? pending-labels)
          (emit-comment out "unreachable")
          (dolist (l pending-labels)
            (emit-without-flushing "~A:" l)))

      (rplaca out false)
      (rplaca (cdr out) ()))))

;;; Dest conversions

(define (convert-value-reg-use dest-type)
  (if (dest-type-conditional? dest-type) 1 0))

(define (destination-reg dest regs)
  (if (dest-value? dest) (dest-value-reg dest) (first regs)))

(define (emit-convert-value out operand dest in-frame-base out-frame-base)
  (cond ((dest-value? dest)
         (let* ((dr (dest-value-reg dest)))
           (unless (eq? operand dr) (emit-mov out operand dr)))
         (emit-adjust-frame-base out in-frame-base out-frame-base))
        ((dest-conditional? dest)
         (emit-adjust-frame-base out in-frame-base out-frame-base)
         (emit-cmp out false-representation operand)
         (emit-branch out "ne" dest))
        ((dest-discard? dest)
         (emit-adjust-frame-base out in-frame-base out-frame-base))
        (true
         (error "can't handle dest ~S" dest))))

(define (emit-prepare-convert-cc-value out reg)
  (emit-clear out reg))

(define (emit-convert-cc-value out cc reg)
  (emit-set out cc reg)
  (emit-shl out atom-tag-bits reg 0)
  (emit-or out atom-tag reg 0))

;;; Heap allocation

(define (emit-alloc out tag-bits size allocreg . scale)
  (emit-mov out (mem "heap_alloc") allocreg)
  (emit-sub out size allocreg)
  (set! scale (if (null? scale) value-scale (car scale)))
  (unless (= tag-bits scale) (emit-and out (- (ash 1 tag-bits)) allocreg))
  (emit-mov out allocreg (mem "heap_alloc")))

;;; Variable access

(define (closure-slot closure index)
  (tagged-mem closure-tag closure (1+ index)))

(define (varrec-operand varrec frame-base)
  (let* ((mode (varrec-attr varrec 'mode)))
    (if (eq? mode 'self) %closure
        (let* ((index (varrec-attr varrec 'index)))
          (cond ((eq? mode 'closure) (closure-slot %closure index))
                ((eq? mode 'param) (param-slot index frame-base))
                ((eq? mode 'local) (local-slot index frame-base))
                ((eq? mode 'register) index)
                (true (error "strange variable mode ~S" mode)))))))

;;; Functions and closures

(define-pure-operator (alloc-closure) result ()
  (emit-alloc out closure-tag-bits (* value-size (1+ (attr-ref attrs 'size)))
              result)
  (emit-add out closure-tag result))

(define-reg-use (fill-closure attrs closure . refs)
  (let* ((ref-rus (mapfor (ref refs) (reg-use ref dest-type-value))))
    (max (reg-use closure dest-type-value)
         (1+ (reduce~ (car ref-rus) (cdr ref-rus) (function max))))))

(define-codegen (fill-closure attrs closure . refs)
  (let* ((closure-reg (first regs))
         (ref-reg (second regs))
         (index 0))
    (codegen closure (dest-value closure-reg) in-frame-base in-frame-base regs
             out)
    (emit-mov out (attr-ref attrs 'label) (tagged-mem closure-tag closure-reg))
    (dolist (ref refs)
      (codegen ref (dest-value ref-reg) in-frame-base in-frame-base (cdr regs)
               out)
      (emit-mov out ref-reg (closure-slot closure-reg index))
      (set! index (1+ index)))
    (emit-convert-value out closure-reg dest in-frame-base out-frame-base)))

(define-reg-use (return attrs body)
  (reg-use body dest-type-value)
  0)

(define-reg-use (varargs-return attrs arg-count body)
  (operator-args-reg-use form)
  0)

(define (emit-call-or-jump out insn func)
  (let* ((func-varrec (and (eq? 'ref (first func)) (second func)))
         (label (and func-varrec
                     (varrec-origin-attr func-varrec 'lambda-label)))
         (comment (comment-form func)))
    (if label
        (emit out "~A ~A # ~S" insn label comment)
        (emit out "~A *~A # ~S" insn
              (value-sized (tagged-mem closure-tag %closure)) comment))))

(define-reg-use ((call tail-call varargs-tail-call) attrs . args)
  (reg-use-recurse form dest-type-value)
  general-register-count)

(define (codegen-call-args out func args in-frame-base)
  (dolist (arg (reverse args))
    (codegen arg (dest-value (first general-registers))
             in-frame-base in-frame-base general-registers out)
    (emit-frame-push out in-frame-base (first general-registers)))
  (codegen func (dest-value %closure)
           in-frame-base in-frame-base general-registers out)
  in-frame-base)

(define-codegen (call attrs func . args)
  (codegen-call-args out func args in-frame-base)
  (emit-mov out (fixnum-representation (length args)) %nargs)
  (emit-call-or-jump out "call" func)
  (emit-restore-%closure out in-frame-base)
  (emit-convert-value out %funcres dest in-frame-base out-frame-base))

(define (emit-restore-%closure out frame-base)
  (emit-mov out (closure-address-slot frame-base) %closure))

;;; Literals

(define (codegen-quoted quoted out)
  (cond ((pair? quoted) (codegen-quoted-pair quoted out))
        ((number? quoted) (fixnum-representation quoted))
        ((character? quoted) (fixnum-representation (character-code quoted)))
        ((string? quoted) (codegen-quoted-string quoted out))
        (true (let* ((c (assoc quoted simple-representations)))
             (cond (c (cdr c))
                   ((symbol? quoted) (codegen-quoted-symbol quoted out))
                   (true (error "unrecognised quoted form ~S" quoted)))))))

(define (codegen-quoted-pair quoted out)
  (let* ((label (gen-label))
         (a (codegen-quoted (car quoted) out))
         (d (codegen-quoted (cdr quoted) out)))
    (emit-data out label pair-tag-bits)
    (emit-literal out a)
    (emit-literal out d)
    (format~ false "~A+~D" label pair-tag)))

(define (codegen-quoted-string str out)
  (let* ((label (gen-label)))
    (emit-data out label string-tag-bits)
    (emit-literal out (fixnum-representation (string-length str)))
    (emit out ".ascii \"~A\"" (escape-string-literal str))
    (format~ false "~A+~D" label string-tag)))

(define emitted-symbols ())

(define (codegen-quoted-symbol sym out)
  (let* ((emitted (assoc sym emitted-symbols)))
    (if emitted (cdr emitted)
        (let* ((label (gen-label))
               (name (codegen-quoted-string (subject-language-symbol-name sym)
                                            out)))
          (emit-data out label atom-tag-bits)
          (emit-literal out name)
          (let* ((lit (format~ false "~A+~D" label atom-tag)))
            (set! emitted-symbols (acons sym lit emitted-symbols))
            lit)))))

;;; Quote

(define-reg-use (quote attrs) (convert-value-reg-use dest-type))

(define-codegen (quote attrs)
  (cond ((dest-discard? dest)
         (emit-adjust-frame-base out in-frame-base out-frame-base))
        ((dest-conditional? dest)
         (emit-adjust-frame-base out in-frame-base out-frame-base)
         (emit-jump out (if (= false-representation (attr-ref attrs 'value))
                            (dest-conditional-flabel dest)
                            (dest-conditional-tlabel dest))))
        ((dest-value? dest)
         (let* ((reg (destination-reg dest regs)))
           (emit-mov out (attr-ref attrs 'value) reg)
           (emit-convert-value out reg dest in-frame-base out-frame-base)))
        (true
         (error "can't handle dest ~S" dest))))

;;; Variables

(define-reg-use ((set! define) varrec val)
  (max (reg-use val dest-type-value)
       (convert-value-reg-use dest-type)))

(define-codegen (set! varrec val)
  (if (and (dest-discard? dest) 
           (eq? 'register (varrec-attr varrec 'mode)))
      (begin
        (codegen val (dest-value (varrec-attr varrec 'index))
                 in-frame-base in-frame-base regs out)
        (emit-adjust-frame-base out in-frame-base out-frame-base))
      (let* ((reg (destination-reg dest regs)))
        (codegen val (dest-value reg) in-frame-base in-frame-base regs out)
        (emit-mov out reg (varrec-operand varrec in-frame-base))
        (emit-convert-value out reg dest in-frame-base out-frame-base))))

(define-codegen (define varrec val)
  (error "codegen for define"))

(define (codegen-define varrec val frame-base regs out)
  (let* ((reg (first regs)))
    (codegen val (dest-value reg) frame-base frame-base regs out)
    (emit-push out reg)))

(define-reg-use (ref varrec) (convert-value-reg-use dest-type))

(define-codegen (ref varrec)
  (emit-convert-value out (varrec-operand varrec in-frame-base)
                      dest in-frame-base out-frame-base))

;;; Operator definitions

(defmarco (define-tag-check name tag tag-bits)
  (quasiquote
    (define-cc-operator ((unquote name) val) "e" ()
      ;; just check the low-order byte
      (emit-and out (low-bits-mask (unquote tag-bits)) val 0)
      (emit-cmp out (unquote tag) val 0))))

(define-tag-check function? closure-tag closure-tag-bits)

;;; Function call-related internals

(define-cc-operator (check-arg-count) "e" ()
  (emit-cmp out (fixnum-representation (attr-ref attrs 'nparams)) %nargs))

(define-pure-operator (arg-count) result ()
  (emit-mov out %nargs result))

(define-pure-operator (raw-args-base) result ()
  (emit-lea out (param-slot 0 in-frame-base) result))

;;; Apply support

(define-reg-use (raw-jump-with-arg-space attrs before-arg-count after-arg-count
                                         bodyfunc)
  (operator-args-reg-use form))

(define-reg-use (raw-apply-jump attrs func arg-count)
  (operator-args-reg-use form))

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

(define-tag-check pair? pair-tag pair-tag-bits)

(define-pure-operator (cons a d) result (alloc)
  (emit-alloc out pair-tag-bits (* 2 value-size) alloc)
  (emit-mov out a (mem alloc))
  (emit-mov out d (mem alloc 1))
  (emit-lea out (mem1+ alloc pair-tag) result))

(define-pure-operator (car a) result ()
  (emit-mov out (tagged-mem pair-tag a) result))

(define-pure-operator (cdr a) result ()
  (emit-mov out (tagged-mem pair-tag a 1) result))

(define-operator (rplaca c a) c ()
  (emit-mov out a (tagged-mem pair-tag c)))

(define-operator (rplacd c d) c ()
  (emit-mov out d (tagged-mem pair-tag c 1)))

;;; Boxes

(define-pure-operator (make-box val) result (alloc)
  (emit-alloc out box-tag-bits value-size alloc)
  (emit-mov out val (mem alloc))
  (emit-lea out (mem1+ alloc box-tag) result))

(define-operator (box-set! box val) val ()
  (emit-mov out val (tagged-mem box-tag box)))

(define-operator (box-ref box) result ()
  (emit-mov out (tagged-mem box-tag box) result))

;;; Symbols

(define-cc-operator (symbol? val) "e" ()
  (let* ((l (gen-label)))
    (emit-cmp out lowest-symbol-representation val)
    (emit-jcc out "b" l)
    (emit-and out (low-bits-mask atom-tag-bits) val 0)
    (emit-cmp out atom-tag val 0)
    (emit-label out l)))

(define-pure-operator (symbol-name sym) result ()
  (emit-mov out (tagged-mem atom-tag sym) result))

(define-pure-operator (raw-make-symbol str) result (alloc)
  (emit-alloc out atom-tag-bits value-size alloc)
  (emit-mov out str (mem alloc))
  (emit-lea out (mem1+ alloc atom-tag) result))

;;;  Numbers

(define-tag-check number? number-tag number-tag-bits)

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
                             (reduce~ (car args) (cdr args)
                                      (lambda (a b)
                                        (list '(unquote op) () a b)))))))))

(define-simplify-binary-op + 0 begin) 
(define-pure-operator (+ a b) a ()
  (emit-add out b a))

(define-simplify-binary-op * 1 begin) 
(define-pure-operator (* a b) a ()
  (emit-sar out number-tag-bits a)
  (emit-imul out b a))

(define-simplify (- attrs a . args)
  (simplify-recurse form)
  (if (null? args) (rplaca form 'negate)
      (overwrite-form form
                      (reduce~ a args (lambda (a b) (list '- () a b))))))

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
        (operator-args-codegen form in-frame-base 
                             (move-regs-to-front (list %a %c) general-registers)
                             out)
        (emit-mov out %a %d)
        (emit-extend-sign-bit out %d)
        (emit-idiv out %c)
        (emit-shl out number-tag-bits %a)
        (emit-convert-value out %a dest in-frame-base out-frame-base))))

(define-reg-use (rem attrs a b)
  (div-operator-reg-use form dest-type))

(define-codegen (rem attrs a b)
  (if (dest-discard? dest)
      (operator-args-codegen-discarding form in-frame-base out-frame-base 
                                        general-registers out)
      (begin
        (operator-args-codegen form in-frame-base
                             (move-regs-to-front (list %a %c) general-registers)
                             out)
        (emit-mov out %a %d)
        (emit-extend-sign-bit out %d)
        (emit-idiv out %c)
        (emit-convert-value out %d dest in-frame-base out-frame-base))))

;;; Strings and vectors

(define-tag-check string? string-tag string-tag-bits)
(define-tag-check vector? vector-tag vector-tag-bits)

(define-pure-operator (make-vec len) result (alloc saved-len)
  (let* ((tag (attr-ref attrs 'tag))
         (tag-bits (attr-ref attrs 'tag-bits))
         (scale (attr-ref attrs 'scale)))
    (emit-mov out len saved-len)
    (emit-scale-number out scale len)
    (emit-add out value-size len)
    (emit-alloc out tag-bits len alloc scale)
    (emit-mov out saved-len (mem alloc))
    (emit-lea out (mem1+ alloc tag) result)))

(define-pure-operator (vec-length vec) result ()
  (emit-mov out (tagged-mem (attr-ref attrs 'tag) vec) result))

(define (vec-slot attrs vec index)
  (mem1+ (tagged-mem (attr-ref attrs 'tag) vec index)
         (attr-ref attrs 'header-size)))

(define-pure-operator (vec-address vec index) result ()
  (emit-scale-number out (attr-ref attrs 'scale) index)
  (emit-lea out (vec-slot attrs vec index) result))

(define-pure-operator (raw-vec-ref vec index) result ()
  (let* ((scale (attr-ref attrs 'scale)))
    (emit-scale-number out scale index)
    (emit-movzx out (vec-slot attrs vec index) result scale)))

(define-operator (raw-vec-set! vec index val) val ()
  (let* ((scale (attr-ref attrs 'scale)))
    (emit-scale-number out scale index)
    (emit-mov out val (vec-slot attrs vec index) scale)))

(define-simplify (copy-mem attrs src-addr dest-addr len)
  ;; this is an utter hack: we use the presence of the forward attr
  ;; to decide if we already simplified this copy-mem
  (unless (eq? 'forward (caar attrs))
    (overwrite-form form
      (let* ((sa-name (gensym))
             (da-name (gensym))
             (len-name (gensym)))
        (quasiquote
          (begin (((unquote sa-name)) ((unquote da-name)) ((unquote len-name)))
            (define (unquote sa-name) (unquote src-addr))
            (define (unquote da-name) (unquote dest-addr))
            (define (unquote len-name) (unquote len))
            (if () (> () (ref (unquote sa-name)) (ref (unquote da-name)))
                (copy-mem ((forward . (unquote true)) (unquote-splicing attrs))
                          (ref (unquote sa-name)) (ref (unquote da-name))
                          (ref (unquote len-name)))
                (copy-mem ((forward . (unquote false)) (unquote-splicing attrs))
                          (ref (unquote sa-name)) (ref (unquote da-name))
                          (ref (unquote len-name))))
            (quote unspecified)))))
    (simplify form)))

(define-reg-use (copy-mem attrs src-addr dest-addr len)
  (operator-args-reg-use form)
  general-register-count)

(define-codegen (copy-mem attrs src-addr dest-addr len)
  (unless (dest-discard? dest)
    (error "mem-copy result not discarded"))
  (let* ((tag (attr-ref attrs 'tag))
         (scale (attr-ref attrs 'scale)))
    (operator-args-codegen form in-frame-base
                     (move-regs-to-front (list %si %di %c) general-registers)
                     out)
    (if (attr-ref attrs 'forward)
        (emit out "cld")
        (begin
          ;; when coping backwards, we need to offset src-addr and dest-addr
          (emit out "std")
          (emit-mov out %c %a)
          (emit-scale-number out scale %a)
          (emit-lea out (tagged-mem (ash 1 scale) %si %a) %si)
          (emit-lea out (tagged-mem (ash 1 scale) %di %a) %di)))
    (emit-sar out number-tag-bits %c)
    (emit-rep-movs out scale)
    (emit-adjust-frame-base out in-frame-base out-frame-base)))

;;; Raw memory access

(define-pure-operator (raw-ref addr) result ()
  (emit-movzx out (mem addr) result (attr-ref attrs 'scale)))

(define-operator (raw-set! addr val) val ()
  (emit-mov out val (mem addr) (attr-ref attrs 'scale)))

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
  (emit-sar out number-tag-bits val))

(define-pure-operator (raw->fixnum val) val ()
  (emit-shl out number-tag-bits val))

(define (emit-set-ac-flag out enable)
  (emit-pushf out)
  (if enable
      (emit-or out #x40000 (mem %sp) 2)
      (begin
        ;; we can't use an immediate mask value, due to fixnum limitations
        (define reg (first general-registers))
        (emit-mov out #x40000 reg 2)
        (emit-not out reg 2)
        (emit-and out reg (mem %sp) 2)))
  (emit-popf out))

(define-simplify (c-global name)
  (rplaca (cdr form) (list (cons 'name name))))

(define-reg-use (c-global attrs) (convert-value-reg-use dest-type))

(define-codegen (c-global attrs)
  (emit-convert-value out (mem (attr-ref attrs 'name)) dest
                      in-frame-base out-frame-base))
