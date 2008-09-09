;;; Code generation

(define (make-codegen) (list false true false ()))

(define (codegen-frame-base cg) (first cg))
(define (codegen-set-frame-base! cg frame-base) (rplaca cg frame-base))

(define (codegen-have-closure cg) (second cg))
(define (codegen-set-have-closure! cg have-closure)
  (rplaca (cdr cg) have-closure))

(define (codegen-deferred-jump cg) (third cg))
(define (codegen-set-deferred-jump! cg jump) (rplaca (cddr cg) jump))

(define (codegen-here-labels cg) (fourth cg))
(define (codegen-set-here-labels! cg labels) (rplaca (cdddr cg) labels))

(define (codegen-redirect-labels cg) (cddddr cg))
(define (codegen-set-redirect-labels! cg rl) (rplacd (cdddr cg) rl))


(define label-counter 0)

(define (gen-label)
  (format~ false ".L~D" (set! label-counter (1+ label-counter))))

(define (label-name-ok? name i)
  (or (= i (string-length name))
      (let* ((ch (string-ref name i)))
        (and (or (character-alphanumeric? ch) (eq? ch #\-))
             (label-name-ok? name (1+ i))))))

(define function-label-prefix "lf_")
(define variable-label-prefix "lv_")

(define (make-label-for sym prefix)
  ;; map a lisp symbol to an assembly label, returning a gen-labelled
  ;; label if necessary
  (let* ((name (subject-language-symbol-name sym)))
    (if (label-name-ok? name 0)
        (string-concat prefix (string-replace name "-" "_"))
        (gen-label))))

(define (emit-data cg label scale)
  (emit cg ".section .rodata")
  (emit cg ".align ~D" (ash 1 scale))
  (emit-smart-label cg label))

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
         (cond ((null? (cdr x)) (indirect-operand (car x)))
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
    (define ((unquote name) cg src dest . scale)
      (emit-insn-2 cg (unquote insn) src dest scale))))

(define (emit-insn-2 cg insn src dest scale)
  (set! scale (if (null? scale) value-scale (car scale)))
  (emit cg "~A~A ~A, ~A" insn (insn-size-suffix scale)
        (insn-operand src scale) (insn-operand dest scale)))

(define-insn-2 emit-lea "lea")
(define-insn-2 emit-add "add")
(define-insn-2 emit-sub "sub")
(define-insn-2 emit-imul "imul")
(define-insn-2 emit-and "and")
(define-insn-2 emit-or "or")
(define-insn-2 emit-xor "xor")
(define-insn-2 emit-cmp "cmp")
(define-insn-2 emit-test "test")
(define-insn-2 emit-shl "shl")
(define-insn-2 emit-sar "sar")

(define (emit-clear cg reg . scale)
  (set! scale (if (null? scale) value-scale (car scale)))
  (emit-xor cg reg reg (min 2 scale)))

(define (emit-mov cg src dest . scale)
  (cond ((and (eq? src 0) (register? dest))
         (emit-clear cg dest))
        ((and (number? src) (register? dest) (> src 0) (< src 1000000))
         (emit-movzx cg src dest
                     (min 2 (if (null? scale) value-scale (car scale)))))
        (true
         (emit-insn-2 cg "mov" src dest scale))))

(define (emit-push cg reg)
  (emit cg "push~A ~A" value-insn-size-suffix (value-sized reg)))

(define (emit-pop cg reg)
  (emit cg "pop~A ~A" value-insn-size-suffix (value-sized reg)))

(define (emit-set cg cc reg)
  (emit cg "set~A ~A" cc (insn-operand reg 0)))

(defmarco (define-insn-1 name insn)
  (quasiquote
    (define ((unquote name) cg oper . scale)
      (emit-insn-1 cg (unquote insn) oper
                   (if (null? scale) value-scale (car scale))))))

(define (emit-insn-1 cg insn oper scale)
  (emit cg "~A~A ~A" insn (insn-size-suffix scale) (insn-operand oper scale)))

(define-insn-1 emit-neg "neg")
(define-insn-1 emit-not "not")
(define-insn-1 emit-idiv "idiv")

(define (emit-ret cg imm)
  (if (= 0 imm)
      (emit cg "ret")
      (emit cg "ret $~D" imm)))

(defmarco (define-insn-0 name insn)
  (quasiquote
    (define ((unquote name) cg . scale)
      (emit cg "~A~A" (unquote insn)
            (insn-size-suffix (if (null? scale) value-scale (car scale)))))))

(define-insn-0 emit-rep-movs "rep ; movs")
(define-insn-0 emit-pushf "pushf")
(define-insn-0 emit-popf "popf")

(define (emit-scale-number cg scale oper)
  (unless (= scale number-tag-bits)
    (if (< scale number-tag-bits)
        (emit-sar cg (- number-tag-bits scale) oper)
        (emit-shl cg (- scale number-tag-bits) oper))))

;;; "Smart" branching, jumping, and labels.
;;;
;;; These do some simple flow control optimizations.  For example,
;;;
;;;     (if (and A B) C D)
;;;
;;; which after macro-expansion is
;;;
;;;     (if (if A B false) C D)
;;;
;;; might naively yield
;;;
;;;     <code to yield A as boolean in CC1>
;;;     jnCC1 L1b ; branch to code for A false
;;;     jmp L1a   ; jump to code for A true
;;;
;;;     L1a:      ; A true
;;;     <code to yield B as boolean in CC2>
;;;     jnCC2 L2b ; branch to code for D
;;;     jmp L2a   ; branch to code for C
;;;     jmp L1c   ; unreachable branch to code after (if A B false)
;;;
;;;     L1b:      ; A false
;;;     jmp L2b   ; branch to code for D
;;;
;;;     L1c:      ; code following (if A B false) would go here... 
;;;     
;;;     L2a:
;;;     <code for C>
;;;     jmp L2c
;;;
;;;     L2b:
;;;     <code for D>
;;;
;;;     L2c:
;;;
;;; instead yields
;;;
;;;     <code to yield A as boolean in CC1>
;;;     jnCC1 L1b ; branch to code for A false
;;;
;;;     L1a:      ; A true
;;;     <code to yield B as boolean in CC2>
;;;     jnCC2 L2b ; branch to code for D
;;;     
;;;     L2a:
;;;     <code for C>
;;;     jmp L2c
;;;
;;;     L1b:      ; A false
;;;     L2b:
;;;     <code for D>
;;;
;;;     L2c:
;;;
;;; The basic order of code is unchanged, but redundant jumps have
;;; been eliminated, and labels have been moved accordingly.  The key
;;; thing to notice here is that labels have all been moved later in
;;; the instruction stream.  This works because the compiler always
;;; generates jumps/branches forwards, never backwards.
;;;
;;; Thus we simply have to maintain a small amonut of state as we
;;; output instructions to tell us what special treatment is required
;;; for jumps, branches and labels.  This consists of three slots in
;;; the codegen object:
;;;
;;; - deferred-jump: A function representing the pending jump or
;;; branch, if any.  This function is called to emit the jump in
;;; flush-labels-and-jumps.  It's false when not optimising flow.
;;;
;;; - here-labels: The list of labels attached to the current point in
;;; the instruction stream.
;;;
;;; - redirect-labels: an alist from labels to the labels they
;;; redirect to.  We never change the label of a jump/branch; instead,
;;; when we emit a label, we also emit all the labels that redirect to
;;; it.

(define (emit-smart-jump cg label)
  (if (codegen-deferred-jump cg)
      (begin
        ;; there is no direct way to reach this jump, so all the here
        ;; labels now redirect to the jumped-to label
        (codegen-set-redirect-labels! cg
          (nconc (nmapfor (l (codegen-here-labels cg)) (cons l label))
                 (codegen-redirect-labels cg)))
        (codegen-set-here-labels! cg ()))
      (codegen-set-deferred-jump! cg
        (lambda ()
          ;; emit a jump, unless we are jumping to a here label
          (unless (member? label (codegen-here-labels cg))
            (emit-jmp cg label))))))

(define (emit-smart-branch cg cc conddest)
  (flush-labels-and-jumps cg)
  (codegen-set-deferred-jump! cg
    (lambda ()
      (let* ((here-labels (codegen-here-labels cg))
             (tlabel (dest-conditional-tlabel conddest))
             (flabel (dest-conditional-flabel conddest)))
        (if (member? tlabel here-labels)
            (unless (member? flabel here-labels)
              (emit-jcc cg (negate-cc cc) flabel))
            (begin
             (emit-jcc cg cc tlabel)
             (unless (member? flabel here-labels)
               (emit-jmp cg flabel))))))))
 
(define (emit-raw-label cg label)
  (emit-without-flushing "~A:" label))

(define (emit-smart-label cg label)
  (let* ((here-labels (cons label (nconc (codegen-here-labels cg)
                                         (take-merged-labels cg label)))))
    (if (codegen-deferred-jump cg)
        (codegen-set-here-labels! cg here-labels)
        (dolist (ml here-labels) (emit-raw-label cg ml)))))

(define (take-merged-labels cg label)
  ;; extract the labels which lead to label
  (let* ((res ()))
    (codegen-set-redirect-labels! cg
      (nfilterfor (redirect (codegen-redirect-labels cg))
        (if (eq? label (cdr redirect))
            (begin
              (push (car redirect) res)
              false)
            true)))
    res))

(define (flush-labels-and-jumps cg)
  (let* ((jump-func (codegen-deferred-jump cg)))
    (when jump-func
      (funcall jump-func)
      (let* ((here-labels (codegen-here-labels cg)))
        (if (null? here-labels)
            (emit-comment cg "unreachable")
            (dolist (l here-labels)
              (emit-without-flushing "~A:" l))))

      (codegen-set-deferred-jump! cg false)
      (codegen-set-here-labels! cg ()))))

(define (emit-jcc cg cc label)
  (emit-without-flushing "j~A ~A" cc label))

(define (emit-jmp cg label)
  (emit-without-flushing "jmp ~A" label))

;;; Dest conversions

(define (convert-value-reg-use dest-type)
  (if (dest-type-conditional? dest-type) 1 0))

(define (destination-reg dest regs)
  (if (dest-value? dest) (dest-value-reg dest) (first regs)))

(define (emit-convert-value cg operand dest out-frame-base)
  (cond ((dest-value? dest)
         (let* ((dr (dest-value-reg dest)))
           (unless (eq? operand dr) (emit-mov cg operand dr)))
         (emit-reset-frame-base cg out-frame-base))
        ((dest-conditional? dest)
         (emit-reset-frame-base cg out-frame-base)
         (emit-cmp cg false-representation operand)
         (emit-smart-branch cg "ne" dest))
        ((dest-discard? dest)
         (emit-reset-frame-base cg out-frame-base))
        (true
         (error "can't handle dest ~S" dest))))

(define (emit-prepare-convert-cc-value cg reg)
  (emit-clear cg reg))

(define (emit-convert-cc-value cg cc reg)
  ;; this embeds special-tag and the representations of false and true
  (emit-set cg cc reg)
  (emit-shl cg special-tag-bits reg 0)
  (emit-or cg special-tag reg 0))

;;; Heap allocation

(define (register-bit reg)
  (labels ((scan-regs (all-regs bit)
             (if (eq? reg (car all-regs)) bit
                 (scan-regs (cdr all-regs) (+ bit bit)))))
    (scan-regs general-registers 1)))

(define (register-bitset regs)
  (reduce~ (register-bit (car regs)) (cdr regs)
           (lambda (bits reg) (logior bits (register-bit reg)))))

(define (emit-alloc cg tag-bits size allocreg spare-regs . scale)
  (let* ((again-label (gen-label))
         (ok-label (gen-label)))
    (flush-labels-and-jumps cg)
    (emit-raw-label cg again-label)
    (emit-mov cg (mem "heap_alloc") allocreg)
    (emit-sub cg size allocreg)
    (set! scale (if (null? scale) value-scale (car scale)))
    (unless (= tag-bits scale) (emit-and cg (- (ash 1 tag-bits)) allocreg))
    (emit-cmp cg (mem "heap_threshold") allocreg)
    (emit-jcc cg "ge" ok-label)
    (emit-mov cg (register-bitset (cons allocreg spare-regs)) %closure)
    (emit cg "call heap_exhausted")
    (emit-restore-%closure cg)
    (emit-jmp cg again-label)
    (emit-raw-label cg ok-label)
    (emit-mov cg allocreg (mem "heap_alloc"))))

(define gc-label (make-label-for 'gc function-label-prefix))

(define (codegen-heap-exhausted cg)
  (labels ((for-live-registers (regs op)
             (dolist (reg regs)
               (let* ((l (gen-label)))
                 (emit-test cg (register-bit reg) %closure)
                 (emit-jcc cg "nz" l)
                 (funcall op cg reg)
                 (emit-raw-label cg l)))))
    (emit cg ".text")
    (emit cg ".globl heap_exhausted")
    (emit cg "heap_exhausted:")
    
    ;; Push live registers onto the stack, guided by the bitset in
    ;; %closure.  This preserves their values, and also means that
    ;; they get treated as part of the root set by the GC.
    (for-live-registers general-registers (function emit-push))
    
    ;; We need to save the live reg bitset from %closure, in order to
    ;; restore the live registers after the GC.  But if we put it on
    ;; the stack it it's original form, the GC will see it and try to
    ;; interpret it as an object reference.  So we disguise it as a
    ;; fixnum first.
    (emit-shl cg number-tag-bits %closure)
    (emit-push cg %closure)
    
    (emit-mov cg (fixnum-representation 0) %nargs)
    (emit cg "call ~A" gc-label)
    
    (emit-pop cg %closure)
    (emit-sar cg number-tag-bits %closure)
  
    ;; Restore live registers
    (for-live-registers (reverse general-registers) (function emit-pop))

    (emit cg "ret")))

;; the gc uses the raw-alloc operation

(define-simplify (raw-alloc tag-bits-ccsym size)
  (rplaca (cdr form) (list (cons 'tag-bits tag-bits-ccsym))))

(define-pure-operator (raw-alloc size) result (alloc)
  (emit-scale-number cg value-scale size)
  (emit-alloc cg (compiler-constant-value (attr-ref attrs 'tag-bits))
              size alloc spare-regs)
  (emit-mov cg alloc result))

;;; Variable access

(define (closure-slot closure index)
  (tagged-mem closure-tag closure (1+ index)))

(define (varrec-operand varrec cg)
  (let* ((mode (varrec-attr varrec 'mode)))
    (cond ((eq? mode 'self) %closure)
          ((eq? mode 'top-level) (mem (varrec-attr varrec 'label)))
          (true
           (let* ((index (varrec-attr varrec 'index)))
             (cond ((eq? mode 'closure) (closure-slot %closure index))
                   ((eq? mode 'param) (param-slot cg index))
                   ((eq? mode 'local) (local-slot cg index))
                   (true (error "strange variable mode ~S" mode))))))))

(define (codegen-top-level-variable cg name label)
  (emit-comment cg "top-level ~S" name)
  (emit cg ".section .bss")
  (emit cg "~A:" label)
  (emit-literal cg 0))

(define (codegen-program-sections program cg)
  (define (emit-bss-label l)
    (emit cg ".section .bss")
    (emit cg ".align ~D" value-size)
    (emit cg ".globl ~A" l)
    (emit cg "~A:" l))

  (emit-bss-label "top_level_start")
  (codegen-sections program cg)
  (emit-bss-label "top_level_end"))

;;; Functions and closures

(define-pure-operator (alloc-closure) result (alloc)
  (emit-alloc cg closure-tag-bits
              (* value-size (1+ (length (attr-ref attrs 'closure))))
              alloc spare-regs)
  (emit-mov cg (attr-ref attrs 'label) (mem alloc))
  (emit-lea cg (mem1+ alloc closure-tag) result))

(define-reg-use (fill-closure attrs closure . refs)
  (max (reg-use closure dest-type-value)
       (1+ (max$ 0 (mapfor (ref refs) (reg-use ref dest-type-value))))))

(define-codegen (fill-closure attrs closure . refs)
  (let* ((closure-reg (first regs))
         (ref-reg (second regs))
         (index 0))
    (codegen closure (dest-value closure-reg) (codegen-frame-base cg) regs
             cg)
    (dolist (ref refs)
      (codegen ref (dest-value ref-reg) (codegen-frame-base cg) (cdr regs)
               cg)
      (emit-mov cg ref-reg (closure-slot closure-reg index))
      (set! index (1+ index)))
    (emit-convert-value cg closure-reg dest out-frame-base)))

(define (codegen-function-intro label closure-size cg)
  (emit cg ".text")
  (emit cg ".align ~D" (ash 1 value-scale))
  (emit-literal cg (fixnum-representation closure-size))
  (emit-smart-label cg label))

(define-reg-use (return attrs body)
  (reg-use body dest-type-value)
  0)

(define-reg-use (varargs-return attrs arg-count body)
  (operator-args-reg-use form)
  0)

(define (emit-call-or-jump cg insn func)
  (let* ((func-varrec (and (eq? 'ref (first func)) (second func)))
         (label (and func-varrec
                     (varrec-origin-attr func-varrec 'lambda-label)))
         (comment (comment-form func)))
    (if label
        (emit cg "~A ~A # ~S" insn label comment)
        (emit cg "~A *~A # ~S" insn
              (value-sized (tagged-mem closure-tag %closure)) comment))))

(define-reg-use ((call tail-call varargs-tail-call) attrs . args)
  (reg-use-recurse form dest-type-value)
  general-register-count)

(define (codegen-call-args cg func args)
  (dolist (arg (reverse args))
    (codegen arg (dest-value (first general-registers))
             (codegen-frame-base cg) general-registers cg)
    (emit-frame-push cg (first general-registers)))
  (unless (and (eq? 'ref (first func))
               (varrec-origin-attr (second func) 'lambda-label)
               (varrec-origin-attr (second func) 'no-closure))
    (codegen func (dest-value %closure) (codegen-frame-base cg)
             general-registers cg)))

(define-codegen (call attrs func . args)
  (with-saved-frame-base cg
    (codegen-call-args cg func args)
    (emit-mov cg (fixnum-representation (length args)) %nargs)
    (emit-call-or-jump cg "call" func))
  (emit-restore-%closure cg)
  (emit-convert-value cg %funcres dest out-frame-base))

(define (emit-restore-%closure cg)
  (when (codegen-have-closure cg)
    (emit-mov cg (closure-address-slot cg) %closure)))

;;; Literals

(define (codegen-quoted quoted cg)
  (cond ((pair? quoted) (codegen-quoted-pair quoted cg))
        ((number? quoted) (fixnum-representation quoted))
        ((character? quoted) (fixnum-representation (character-code quoted)))
        ((string? quoted) (codegen-quoted-string quoted cg))
        (true (let* ((c (assoc quoted simple-representations)))
             (cond (c (cdr c))
                   ((symbol? quoted) (codegen-quoted-symbol quoted cg))
                   (true (error "unrecognised quoted form ~S" quoted)))))))

(define (codegen-quoted-pair quoted cg)
  (let* ((label (gen-label))
         (a (codegen-quoted (car quoted) cg))
         (d (codegen-quoted (cdr quoted) cg)))
    (emit-data cg label pair-tag-bits)
    (emit-literal cg a)
    (emit-literal cg d)
    (format~ false "~A+~D" label pair-tag)))

(define (codegen-quoted-string str cg)
  (let* ((label (gen-label)))
    (emit-data cg label string-tag-bits)
    (emit-literal cg (fixnum-representation (string-length str)))
    (emit cg ".ascii \"~A\"" (escape-string-literal str))
    (format~ false "~A+~D" label string-tag)))

(define emitted-symbols ())

(define (codegen-quoted-symbol sym cg)
  (let* ((emitted (assoc sym emitted-symbols)))
    (if emitted (cdr emitted)
        (let* ((label (gen-label))
               (name (codegen-quoted-string (subject-language-symbol-name sym)
                                            cg)))
          (emit-data cg label symbol-tag-bits)
          (emit-literal cg name)
          (let* ((lit (format~ false "~A+~D" label symbol-tag)))
            (set! emitted-symbols (acons sym lit emitted-symbols))
            lit)))))

;;; Quote

(define-reg-use (quote attrs) (convert-value-reg-use dest-type))

(define-codegen (quote attrs)
  (cond ((dest-discard? dest)
         (emit-reset-frame-base cg out-frame-base))
        ((dest-conditional? dest)
         (emit-reset-frame-base cg out-frame-base)
         (emit-smart-jump cg (if (= false-representation (attr-ref attrs 'value))
                            (dest-conditional-flabel dest)
                            (dest-conditional-tlabel dest))))
        ((dest-value? dest)
         (let* ((reg (destination-reg dest regs)))
           (emit-mov cg (attr-ref attrs 'value) reg)
           (emit-convert-value cg reg dest out-frame-base)))
        (true
         (error "can't handle dest ~S" dest))))

;;; Variables

(define-reg-use (set! varrec val)
  (max (reg-use val dest-type-value)
       (convert-value-reg-use dest-type)))

(define-codegen (set! varrec val)
  (let* ((reg (destination-reg dest regs)))
    (codegen val (dest-value reg) (codegen-frame-base cg) regs cg)
    (emit-mov cg reg (varrec-operand varrec cg))
    (emit-convert-value cg reg dest out-frame-base)))

(define-reg-use (ref varrec) (convert-value-reg-use dest-type))

(define-codegen (ref varrec)
  (emit-convert-value cg (varrec-operand varrec cg) dest out-frame-base))

;;; Operator definitions

(defmarco (define-tag-check name tag tag-bits)
  (quasiquote
    (define-cc-operator ((unquote name) val) "e" ()
      ;; just check the low-order byte
      (emit-and cg (low-bits-mask (unquote tag-bits)) val 0)
      (emit-cmp cg (unquote tag) val 0))))

(define-tag-check function? closure-tag closure-tag-bits)

;;; Function call-related internals

(define-cc-operator (check-arg-count) "e" ()
  (emit-cmp cg (fixnum-representation (attr-ref attrs 'nparams)) %nargs))

(define-pure-operator (arg-count) result ()
  (emit-mov cg %nargs result))

(define-pure-operator (raw-args-base) result ()
  (emit-lea cg (param-slot cg 0) result))

;;; Apply support

(define-reg-use (raw-jump-with-arg-space attrs before-arg-count after-arg-count
                                         bodyfunc)
  (operator-args-reg-use form))

(define-reg-use (raw-apply-jump attrs func arg-count)
  (operator-args-reg-use form))

;;; Comparisons

(defmarco (define-cmp-operator name cc)
  (quasiquote (define-cc-operator ((unquote name) a b) (unquote cc) () 
                (emit-cmp cg b a))))
 
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
  (emit-alloc cg pair-tag-bits (* 2 value-size) alloc spare-regs)
  (emit-mov cg a (mem alloc))
  (emit-mov cg d (mem alloc 1))
  (emit-lea cg (mem1+ alloc pair-tag) result))

(define-pure-operator (car a) result ()
  (emit-mov cg (tagged-mem pair-tag a) result))

(define-pure-operator (cdr a) result ()
  (emit-mov cg (tagged-mem pair-tag a 1) result))

(define-operator (rplaca c a) c ()
  (emit-mov cg a (tagged-mem pair-tag c)))

(define-operator (rplacd c d) c ()
  (emit-mov cg d (tagged-mem pair-tag c 1)))

;;; Boxes

(define-pure-operator (raw-make-box val) result (alloc)
  (emit-alloc cg box-tag-bits value-size alloc spare-regs)
  (emit-mov cg val (mem alloc))
  (emit-lea cg (mem1+ alloc box-tag) result))

(define-operator (raw-box-set! box val) val ()
  (emit-mov cg val (tagged-mem box-tag box)))

(define-operator (raw-box-ref box) result ()
  (emit-mov cg (tagged-mem box-tag box) result))

;;; Symbols

(define-tag-check symbol? symbol-tag symbol-tag-bits)

(define-pure-operator (symbol-name sym) result ()
  (emit-mov cg (tagged-mem symbol-tag sym) result))

(define-pure-operator (raw-make-symbol str) result (alloc)
  (emit-alloc cg symbol-tag-bits value-size alloc spare-regs)
  (emit-mov cg str (mem alloc))
  (emit-lea cg (mem1+ alloc symbol-tag) result))

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
  (emit-add cg b a))

(define-simplify-binary-op * 1 begin) 
(define-pure-operator (* a b) a ()
  (emit-sar cg number-tag-bits a)
  (emit-imul cg b a))

(define-simplify (- attrs a . args)
  (simplify-recurse form)
  (if (null? args) (rplaca form 'negate)
      (overwrite-form form
                      (reduce~ a args (lambda (a b) (list '- () a b))))))

(define-pure-operator (negate a) a ()
  (emit-neg cg a))

(define-pure-operator (- a b) a ()
  (emit-sub cg b a))

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
      (operator-args-codegen-discarding form out-frame-base general-registers
                                        cg)
      (begin
        (operator-args-codegen form
                         (move-regs-to-front (list %a %c) general-registers) cg)
        (emit-mov cg %a %d)
        (emit-extend-sign-bit cg %d)
        (emit-idiv cg %c)
        (emit-shl cg number-tag-bits %a)
        (emit-convert-value cg %a dest out-frame-base))))

(define-reg-use (rem attrs a b)
  (div-operator-reg-use form dest-type))

(define-codegen (rem attrs a b)
  (if (dest-discard? dest)
      (operator-args-codegen-discarding form out-frame-base general-registers
                                        cg)
      (begin
        (operator-args-codegen form
                         (move-regs-to-front (list %a %c) general-registers) cg)
        (emit-mov cg %a %d)
        (emit-extend-sign-bit cg %d)
        (emit-idiv cg %c)
        (emit-convert-value cg %d dest out-frame-base))))

(define-pure-operator (logior a b) a ()
  (emit-or cg b a))

;;; Strings and vectors

(define-tag-check string? string-tag string-tag-bits)
(define-tag-check vector? vector-tag vector-tag-bits)

(define-pure-operator (make-vec len) result (alloc saved-len)
  (let* ((tag (attr-ref attrs 'tag))
         (tag-bits (attr-ref attrs 'tag-bits))
         (scale (attr-ref attrs 'scale)))
    (emit-mov cg len saved-len)
    (emit-scale-number cg scale len)
    (emit-add cg value-size len)
    (emit-alloc cg tag-bits len alloc spare-regs scale)
    (emit-mov cg saved-len (mem alloc))
    (emit-lea cg (mem1+ alloc tag) result)))

(define-pure-operator (vec-length vec) result ()
  (emit-mov cg (tagged-mem (attr-ref attrs 'tag) vec) result))

(define (vec-slot attrs vec index)
  (mem1+ (tagged-mem (attr-ref attrs 'tag) vec index)
         (attr-ref attrs 'header-size)))

(define-pure-operator (vec-address vec index) result ()
  (emit-scale-number cg (attr-ref attrs 'scale) index)
  (emit-lea cg (vec-slot attrs vec index) result))

(define-pure-operator (raw-vec-ref vec index) result ()
  (let* ((scale (attr-ref attrs 'scale)))
    (emit-scale-number cg scale index)
    (emit-movzx cg (vec-slot attrs vec index) result scale)))

(define-operator (raw-vec-set! vec index val) val ()
  (let* ((scale (attr-ref attrs 'scale)))
    (emit-scale-number cg scale index)
    (emit-mov cg val (vec-slot attrs vec index) scale)))

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
    (operator-args-codegen form
                    (move-regs-to-front (list %si %di %c) general-registers) cg)
    (if (attr-ref attrs 'forward)
        (emit cg "cld")
        (begin
          ;; when coping backwards, we need to offset src-addr and dest-addr
          (emit cg "std")
          (emit-mov cg %c %a)
          (emit-scale-number cg scale %a)
          (emit-lea cg (tagged-mem (ash 1 scale) %si %a) %si)
          (emit-lea cg (tagged-mem (ash 1 scale) %di %a) %di)))
    (emit-sar cg number-tag-bits %c)
    (emit-rep-movs cg scale)
    (emit-reset-frame-base cg out-frame-base)))

;;; Raw memory access

(define-pure-operator (raw-ref addr) result ()
  (emit-movzx cg (mem addr) result (attr-ref attrs 'scale)))

(define-operator (raw-set! addr val) val ()
  (emit-mov cg val (mem addr) (attr-ref attrs 'scale)))

;;; Raw ops for the GC

(define-pure-operator (raw-logand a b) a ()
  (emit-and cg b a))

(define-pure-operator (raw-- a b) a ()
  (emit-sub cg b a))

(define-pure-operator (raw-+ a b) a ()
  (emit-add cg b a))

;;; Misc. runtime

(define-reg-use (error-halt attrs message args) 0)
(define-codegen (error-halt attrs message args)
  (let* ((l (gen-label)))
    (emit-smart-label cg l)
    (emit-comment cg "error-halt: ~S"
                  (if (eq? 'quote (first message)) 
                      (form-attr message 'quoted)
                      "unknown"))
    (emit cg "hlt")
    (emit-smart-jump cg l)))

(define-pure-operator (fixnum->raw val) val ()
  (emit-sar cg number-tag-bits val))

(define-pure-operator (raw->fixnum val) val ()
  (emit-shl cg number-tag-bits val))

(define (emit-set-ac-flag cg enable)
  (emit-pushf cg)
  (if enable
      (emit-or cg #x40000 (mem %sp) 2)
      (begin
        ;; we can't use an immediate mask value, due to fixnum limitations
        (define reg (first general-registers))
        (emit-mov cg #x40000 reg 2)
        (emit-not cg reg 2)
        (emit-and cg reg (mem %sp) 2)))
  (emit-popf cg))

(define-simplify ((raw-c-global raw-label) name)
  (rplaca (cdr form) (list (cons 'name name))))

(define-reg-use ((raw-c-global raw-label) attrs)
  (convert-value-reg-use dest-type))

(define-codegen (raw-c-global attrs)
  (emit-convert-value cg (mem (attr-ref attrs 'name)) dest out-frame-base))

(define-codegen (raw-label attrs)
  (emit-convert-value cg (attr-ref attrs 'name) dest out-frame-base))
