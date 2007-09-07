;;; Code generation

(define label-counter 0)

(define (gen-label)
  (format nil ".L~D" (incf label-counter)))

(define (emit-data out label scale)
  (emit out ".data")
  (emit out ".align ~D" (ash 1 scale))
  (emit-label out label))

;;; Generic representation bits

(define (fixnum-representation n) (ash n number-tag-bits))
(define (low-bits-mask bits) (1- (ash 1 bits)))

;;; Registers and address modes

(define (immediate x)
  (if (number? x) x (format nil "$~A" x)))

(define (dispmem correction offset reg . reg2)
  (if (null reg2)
      (format nil "~A(~A)" (- offset correction) (value-sized reg))
      (format nil "~A(~A,~A)" (- offset correction) (value-sized reg)
              (value-sized (first reg2)))))

(define (mem reg)
  (format nil "(~A)" (value-sized reg)))

(define (register? reg)
  (symbol? reg))

(define (insn-operand operand scale)
  (cond ((symbol? operand)
         (elt (cdr (assoc operand register-operands)) scale))
        ((string? operand) operand)
        ((number? operand) (format nil "$~D" operand))
        (true (error "strange operand ~S" operand))))

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
      (emit-insn-1 out (unquote insn) oper (and scale (car scale))))))

(define (emit-insn-1 out insn oper scale)
  (unless scale
    (set! scale value-scale))
  (emit out "~A~A ~A" insn (insn-size-suffix scale) (insn-operand oper scale)))

(define-insn-1 emit-neg "neg")
(define-insn-1 emit-idiv "idiv")

(defmarco (define-insn-0 name insn)
  (quasiquote
    (define ((unquote name) out . scale)
      (emit out "~A~A" (unquote insn)
            (insn-size-suffix (if scale (car scale) value-scale))))))

(define-insn-0 emit-rep-movs "rep ; movs")
(define-insn-0 emit-pushf "pushf")
(define-insn-0 emit-popf "popf")

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
  (emit-adjust-frame-base out in-frame-base out-frame-base)
  (cond ((dest-value? dest)
         (let* ((dr (dest-value-reg dest)))
           (unless (eq? operand dr) (emit-mov out operand dr))))
        ((dest-conditional? dest)
         (emit-cmp out (immediate false-representation) operand)
         (emit-branch out "ne" dest))
        ((dest-discard? dest))
        (true
         (error "can't handle dest ~S" dest))))
         

(define (emit-prepare-convert-cc-value out reg)
  (emit-clear out reg))

(define (emit-convert-cc-value out cc reg)
  (emit-set out cc reg)
  (emit-shl out (immediate atom-tag-bits) reg 0)
  (emit-or out (immediate atom-tag) reg 0))

;;; Heap allocation

(define (emit-align-%alloc out tag-bits . scale)
  (set! scale (if (null? scale) value-scale (car scale)))
  (unless (= tag-bits scale)
    (emit-and out (immediate (- (ash 1 tag-bits))) %alloc)))

;;; Stack handling

;;; Layout:

;;; param N
;;; ...
;;; param 1
;;; param 0
;;; Return address
;;; Saved %bp <--- %bp (and (+ %sp (* frame-base value-size)))
;;; %func
;;; Local var 0
;;; ...
;;; Local var N
;;; in-progress param N
;;; in-progress param N-1
;;; ...
;;;
;;; Functions are always called with at least 1 param slot, to allow for
;;; varargs functions
;;;
;;; Functions are called with the closure in %func, arg-count in
;;; %nargs.  They return with the result in a %funcres.

(define (emit-allocate-locals out n)
  (emit-sub out (immediate (* value-size n)) %sp))

(define (emit-adjust-frame-base out in-frame-base out-frame-base)
  (unless (= in-frame-base out-frame-base)
    (emit-add out (immediate (* value-size (- in-frame-base out-frame-base)))
              %sp)))

(define (closure-slot func index)
  (dispmem function-tag (* value-size (1+ index)) func))

(define (param-slot index)
  (dispmem 0 (* value-size (+ 2 index)) %bp))

(define (local-slot index)
  (dispmem (* value-size (+ 1 index)) 0 %bp))

(define (varrec-operand varrec frame-base)
  (let* ((mode (varrec-attr varrec 'mode)))
    (if (eq? mode 'self) %func
        (let* ((index (varrec-attr varrec 'index)))
          (cond ((eq? mode 'closure) (closure-slot %func index))
                ((eq? mode 'param) (param-slot index))
                ((eq? mode 'local) (local-slot index))
                ((eq? mode 'register) index)
                (true (error "strange variable mode ~S" mode)))))))

;;; Functions

(define function-in-frame-base 1)
(define function-out-frame-base 0)

(define (emit-function-prologue out)
  (emit-push out %bp)
  (emit-mov out %sp %bp)
  (emit-push out %func))

(define (emit-function-epilogue out)
  (emit out "leave ; ret"))

(define (emit-restore-%func out)
  (emit-mov out (dispmem value-size 0 %bp) %func))

(define (emit-indirect-call out)
  (emit out "call *~A" (value-sized (dispmem function-tag 0 %func))))

(define (emit-call out label)
  (emit out "call ~A" label))

(define (emit-alloc-function out result-reg label slot-count)
  (emit-sub out (immediate (* value-size (1+ slot-count))) %alloc)
  (emit-align-%alloc out function-tag-bits)
  (emit-mov out (immediate label) (dispmem 0 0 %alloc))
  (emit-lea out (dispmem 0 function-tag %alloc) result-reg))

(define (emit-closure-slot-set out func-reg varrec val-reg)
  (emit-mov out val-reg (closure-slot func-reg (varrec-attr varrec 'index))))

;;; C-callable program wrapper

(define c-callee-saved-regs '(%b %bp %r12 %r13 %r14 %r15))

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
    (emit-data out label pair-tag-bits)
    (emit-literal out a)
    (emit-literal out d)
    (format nil "~A+~D" label pair-tag)))

(define (codegen-quoted-string str out)
  (let* ((label (gen-label)))
    (emit-data out label string-tag-bits)
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
          (emit-data out label atom-tag-bits)
          (emit-literal out name)
          (let* ((lit (format nil "~A+~D" label atom-tag)))
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
           (emit-mov out (immediate (attr-ref attrs 'value)) reg)
           (emit-convert-value out reg dest in-frame-base out-frame-base)))
        (true
         (error "can't handle dest ~S" dest))))

;;; Variables

(define-reg-use (set! varrec val)
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

(define-reg-use (ref varrec) (convert-value-reg-use dest-type))

(define-codegen (ref varrec)
  (emit-convert-value out (varrec-operand varrec in-frame-base)
                      dest in-frame-base out-frame-base))

;;; Operator definitions

(defmarco (define-tag-check name tag tag-bits)
  (quasiquote
    (define-cc-operator ((unquote name) val) "e" ()
      ;; just check the low-order byte
      (emit-and out (immediate (low-bits-mask (unquote tag-bits))) val 0)
      (emit-cmp out (immediate (unquote tag)) val 0))))

(define-tag-check function? function-tag function-tag-bits)

;;; Function call-related internals

(define-cc-operator (check-arg-count) "e" ()
  (emit-cmp out (immediate (fixnum-representation (attr-ref attrs 'nparams)))
            %nargs))

(define-pure-operator (arg-count) result ()
  (emit-mov out %nargs result))

(define-pure-operator (raw-args-address) result ()
  (emit-lea out (param-slot 0) result))

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

(define-pure-operator (cons a d) result ()
  (emit-sub out (immediate (* 2 value-size)) %alloc)
  (emit-align-%alloc out pair-tag-bits)
  (emit-mov out a (dispmem 0 0 %alloc))
  (emit-mov out d (dispmem 0 value-size %alloc))
  (emit-lea out (dispmem 0 pair-tag %alloc) result))

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
  (emit-sub out (immediate value-size) %alloc)
  (emit-align-%alloc out box-tag-bits)
  (emit-mov out val (dispmem 0 0 %alloc))
  (emit-lea out (dispmem 0 box-tag %alloc) result))

(define-operator (box-set! box val) val ()
  (emit-mov out val (dispmem box-tag 0 box)))

(define-operator (box-ref box) result ()
  (emit-mov out (dispmem box-tag 0 box) result))

;;; Symbols

(define-cc-operator (symbol? val) "e" ()
  (let* ((l (gen-label)))
    (emit-cmp out (immediate lowest-symbol-representation) val)
    (emit-jcc out "l" l)
    (emit-and out (immediate (low-bits-mask atom-tag-bits)) val 0)
    (emit-cmp out (immediate atom-tag) val 0)
    (emit-label out l)))

(define-pure-operator (symbol-name sym) result ()
  (emit-mov out (dispmem atom-tag 0 sym) result))

(define-pure-operator (primitive-make-symbol str) result ()
  (emit-sub out (immediate value-size) %alloc)
  (emit-align-%alloc out atom-tag-bits)
  (emit-mov out str (dispmem 0 0 %alloc))
  (emit-lea out (dispmem 0 atom-tag %alloc) result))

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
                             (reduce (lambda (a b) (list '(unquote op) () a b))
                                     args)))))))

(define-simplify-binary-op + 0 begin) 
(define-pure-operator (+ a b) a ()
  (emit-add out b a))

(define-simplify-binary-op * 1 begin) 
(define-pure-operator (* a b) a ()
  (emit-sar out (immediate number-tag-bits) a)
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
        (operator-args-codegen form in-frame-base 
                               (move-regs-to-front '(%a %c) general-registers)
                               out)
        (emit-mov out %a %d)
        (emit-sar out (immediate 63) %d)
        (emit-idiv out %c)
        (emit-shl out (immediate number-tag-bits) %a)
        (emit-convert-value out %a dest in-frame-base out-frame-base))))

(define-reg-use (rem attrs a b)
  (div-operator-reg-use form dest-type))

(define-codegen (rem attrs a b)
  (if (dest-discard? dest)
      (operator-args-codegen-discarding form in-frame-base out-frame-base 
                                        general-registers out)
      (begin
        (operator-args-codegen form in-frame-base
                               (move-regs-to-front '(%a %c) general-registers)
                               out)
        (emit-mov out %a %d)
        (emit-sar out (immediate 63) %d)
        (emit-idiv out %c)
        (emit-convert-value out %d dest in-frame-base out-frame-base))))

(define (emit-scale-number out scale oper)
  (unless (= scale number-tag-bits)
    (if (< scale number-tag-bits)
        (emit-sar out (immediate (- number-tag-bits scale)) oper)
        (emit-shl out (immediate (- scale number-tag-bits)) oper))))

;;; Strings and vectors

(define-tag-check string? string-tag string-tag-bits)
(define-tag-check vector? vector-tag vector-tag-bits)

(define-pure-operator (make-vec len) result (raw-len)
  (let* ((tag (attr-ref attrs 'tag))
         (tag-bits (attr-ref attrs 'tag-bits))
         (scale (attr-ref attrs 'scale)))
    (if (= scale number-tag-bits)
      (begin
        (emit-sub out (immediate value-size) %alloc)
        (emit-sub out len %alloc)
        (emit-align-%alloc out tag-bits scale)
        (emit-mov out len (dispmem 0 0 %alloc))
        (emit-lea out (dispmem 0 tag %alloc) result))
      (begin
        (emit-mov out len raw-len)
        (emit-scale-number out scale raw-len)
        (emit-sub out (immediate value-size) %alloc)
        (emit-sub out raw-len %alloc)
        (emit-align-%alloc out tag-bits scale)
        (emit-mov out len (dispmem 0 0 %alloc))
        (emit-lea out (dispmem 0 tag %alloc) result)))))

(define-pure-operator (vec-length vec) result ()
  (emit-mov out (dispmem (attr-ref attrs 'tag) 0 vec) result))

(define-pure-operator (raw-vec-address vec index) result ()
  (let* ((tag (attr-ref attrs 'tag))
         (scale (attr-ref attrs 'scale)))
    (emit-scale-number out scale index)
    (emit-lea out (dispmem tag value-size vec index) result)))

(define-pure-operator (vec-ref vec index) result ()
  (let* ((tag (attr-ref attrs 'tag))
         (scale (attr-ref attrs 'scale)))
    (emit-scale-number out scale index)
    (emit-movzx out (dispmem tag value-size vec index) result scale)))

(define-operator (vec-set! vec index val) val ()
  (let* ((tag (attr-ref attrs 'tag))
         (scale (attr-ref attrs 'scale)))
    (emit-scale-number out scale index)
    (emit-mov out val (dispmem tag value-size vec index) scale)))

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
  (emit-sar out (immediate number-tag-bits) val))

(define-pure-operator (raw->fixnum val) val ()
  (emit-shl out (immediate number-tag-bits) val))

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

(define (c-callee-saved reg) (member? reg c-callee-saved-regs))

(define-codegen (c-call attrs . args)
  (when (> (length args) 4)
    (error "too many arguments to c-call"))
  (operator-args-codegen form in-frame-base
                      (move-regs-to-front '(%di %si %d %c) general-registers)
                      out)
  (unless (c-callee-saved %alloc) (emit-push out %alloc))
  (emit out "cld")
  (emit-set-ac-flag out false)

  ;; C ABI requires us to align stack to 16 byte bundary
  (emit-push out %bp)
  (emit-mov out %sp %bp)
  (emit-and out (immediate -16) %sp)
  (emit out "call ~A" (attr-ref attrs 'c-function-name))
  (emit out "leave")

  (emit-set-ac-flag out true)
  (unless (c-callee-saved %alloc) (emit-pop out %alloc))
  (unless (c-callee-saved %func) (emit-restore-%func out))
  (emit-convert-value out %a dest in-frame-base out-frame-base))

(define-reg-use (raw-apply-with-args attrs nargs bodyfunc)
  (reg-use nargs dest-type-value)
  ;; raw-apply-with-args is only intended for restricted
  ;; circumstances, so we make this assumption:
  (unless (< (reg-use bodyfunc dest-type-value) general-register-count)
    (error "all registers needed for ~S" bodyfunc))
  general-register-count)

(define-codegen (raw-apply-with-args attrs nargs bodyfunc)
  (codegen nargs (dest-value %nargs) in-frame-base in-frame-base
           general-registers out)
  (codegen bodyfunc (dest-value %func) in-frame-base in-frame-base
           (remove %nargs general-registers) out)
  (emit-push out %nargs)
  (emit-scale-number out value-scale %nargs)
  (emit-sub out %nargs %sp)
  (emit-clear out %nargs)
  (emit out "call *~A" (value-sized (dispmem function-tag 0 %func)))
  (emit-add out (param-slot in-frame-base) %sp)
  (emit-add out (immediate value-size) %sp)
  (emit-restore-%func out)
  (emit-convert-value out %funcres dest in-frame-base out-frame-base))

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
  ;; Don't need to adjust frame base, because leave will handle it
  (emit out "leave ; jmp *~A" (value-sized (dispmem function-tag 0 %func))))

(define-operator (raw-arg-set! args-base index val) val ()
  (emit-scale-number out value-scale index)
  (emit-mov out val (dispmem 0 0 args-base index)))

(define-pure-operator (raw-arg-ref args-base index) result ()
  (emit-scale-number out value-scale index)
  (emit-mov out (dispmem 0 0 args-base index) result))

(define-reg-use (raw-rdtsc attrs)
  (if (dest-type-discard? dest-type) 0 general-register-count))

(define-codegen (raw-rdtsc attrs)
  (unless (dest-discard? dest)
    (emit out "rdtsc")
    (emit-shl out (immediate number-tag-bits) %a))
  (emit-convert-value out %a dest in-frame-base out-frame-base))
