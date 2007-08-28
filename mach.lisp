;;; Assembler

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

(define (emit-literal out lit)
  (emit out ".quad ~A" lit))

(define label-counter 0)

(define (gen-label)
  (format nil ".L~D" (incf label-counter)))

;;; Machine definition

(defconstant value-scale 3)
(defconstant value-size (ash 1 value-scale))

(defconstant allocation-alignment-scale 3)
(defconstant allocation-alignment (ash 1 allocation-alignment-scale))
(defconstant allocation-alignment-mask (- allocation-alignment))

;;; Value representation

(defconstant tag-bits 3)
(defconstant tag-mask (1- (ash 1 tag-bits)))

(defconstant number-tag #b000)
(defconstant function-tag #b001)
(defconstant box-tag #b010)
(defconstant pair-tag #b011)
(defconstant vector-tag #b100)
(defconstant string-tag #b101)
(defconstant atom-tag #b111)

(defconstant char-scale 0)

(defconstant function-size (* 2 value-size))
(defconstant box-size value-size)
(defconstant pair-size (* 2 value-size))

(define (fixnum-representation n) (ash n tag-bits))

(defconstant false-representation #b111)
(defconstant unspecified-representation #b10111)
(defconstant lowest-symbol-representation #b100000000111)

(defconstant simple-representations 
  ;; The quotes here are significant for CL compatibility
  (list (cons 'false false-representation)
        (cons 'true #b1111)
        (cons 'unspecified unspecified-representation)
        (cons () #b11111)))

;;; Registers

(define register-operands ())

(defmarco (define-register name . variants)
  (quasiquote (definitions
    (define (unquote name) (quote (unquote name)))
    (set! register-operands (acons (unquote name) (quote (unquote variants))
                                   register-operands)))))

(define-register %a "%al" "%ax" "%eax" "%rax")
(define-register %b "%bl" "%bx" "%ebx" "%rbx")
(define-register %c "%cl" "%cx" "%ecx" "%rcx")
(define-register %d "%dl" "%dx" "%edx" "%rdx")
(define-register %si "%sil" "%si" "%esi" "%rsi")
(define-register %di "%dil" "%di" "%edi" "%rdi")
(define-register %sp "%spl" "%sp" "%esp" "%rsp")
(define-register %bp "%bpl" "%bp" "%ebp" "%rbp")

(defmarco (define-extended-registers)
  (cons 'definitions
        (mapfor (n '(8 9 10 11 12 13 14 15))
          (list* 'define-register
                 (subject-language-intern (format nil "%r~D" n))
                 (mapfor (tmpl '("%r~Dl" "%r~Dw" "%r~Dd" "%r~D"))
                   (format nil tmpl n))))))

(define-extended-registers)

;;; Use RBX as the allocation pointer, since there are no relevant
;;; instructions that clobber it implicitly.
(define %alloc %r14)

(define general-registers (list %a %b %c %d %si %di))
(define general-register-count (length general-registers))

(define %func %r15)
(define %funcres (first general-registers))

;;; %nargs is use to pass the number of arguments to functions.  We
;;; %reuse one of the general-registers for this, which means we have
;;; %to be really careful about invoking the operators that use it
;;; %(check-arg-count, arg-count).
(define %nargs (last-elem general-registers))

(define (move-regs-to-front regs all-regs)
  (append regs (filterfor (reg all-regs) (not (member? reg regs)))))

;;; Address modes

(define (immediate x)
  (format nil "$~A" x))

(define (dispmem correction offset reg . reg2)
  (if (null reg2)
      (format nil "~A(~A)" (- offset correction) (value-sized reg))
      (format nil "~A(~A,~A)" (- offset correction) (value-sized reg)
              (value-sized (first reg2)))))

(define (mem reg)
  (format nil "(~A)" (value-sized reg)))

(define (insn-operand operand scale)
  (cond ((symbol? operand)
         (elt (cdr (assoc operand register-operands)) scale))
        ((string? operand) operand)
        (true (error "strange operand ~S" operand))))

(define (value-sized operand)
  (insn-operand operand value-scale))

;;; Condition codes

(define (negate-cc cc)
  (if (eq (string-ref cc 0) #\n)
      (substring cc 1 (1- (string-length cc)))
      (string-concat "n" cc)))

;;; Instructions

(define (insn-size-suffix scale)
  (elt '("b" "w" "l" "q") scale))

(defconstant value-insn-size-suffix (insn-size-suffix value-scale))

(defmarco (define-insn-2 name insn)
  (quasiquote
    (define ((unquote name) out src dest . scale)
      (emit-insn-2 out (unquote insn) src dest (and scale (car scale))))))

(define (emit-insn-2 out insn src dest scale)
  (unless scale
    (set! scale value-scale))
  (emit out "~A~A ~A, ~A" insn (insn-size-suffix scale)
        (insn-operand src scale) (insn-operand dest scale)))

(define-insn-2 emit-mov "mov")
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

(define (emit-movzx out src dest src-scale . dest-scale)
  (set! dest-scale (if (null dest-scale) value-scale (car dest-scale)))
  (labels ((movzx (src-scale dest-scale)
             (emit out "mov~A ~A,~A"
                   (elt (elt '(("b")
                               ("zbw" "w")
                               ("zbl" "zwl" "l")) dest-scale) src-scale)
                   (insn-operand src src-scale)
                   (insn-operand dest dest-scale))))
    (if (= dest-scale 3)
        (if (= src-scale 3)
            (emit-mov out src dest 3)
            (movzx src-scale 2))
        (movzx src-scale dest-scale))))

(define (emit-clear out reg)
  (emit-xor out reg reg 2))

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

(define (emit-convert-value out reg dest in-frame-base out-frame-base)
  (emit-adjust-frame-base out in-frame-base out-frame-base)
  (cond ((dest-value? dest)
         (let* ((dr (dest-value-reg dest)))
           (unless (eq? reg dr) (emit-mov out reg dr))))
        ((dest-conditional? dest)
         (emit-cmp out (immediate false-representation) reg)
         (emit-branch out "ne" dest))
        ((dest-discard? dest))
        (true
         (error "can't handle dest ~S" dest))))
         

(define (emit-prepare-convert-cc-value out reg)
  (emit-clear out reg))

(define (emit-convert-cc-value out cc reg)
  (emit-set out cc reg)
  (emit-shl out (immediate tag-bits) reg 0)
  (emit-or out (immediate atom-tag) reg 0))

;;; Stack handling

;;; Layout:

;;; param N
;;; ...
;;; param 1
;;; param 0
;;; Silly slot for the benefit of apply
;;; Return address
;;; Saved %rbp
;;; func slot (filled by callee) <--- (+ %sp (* frame-base value-size))
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

(defmarco (emit-frame-push out frame-base reg)
  (quasiquote (begin
    (emit-push (unquote out) (unquote reg))
    (set! (unquote frame-base) (1+ (unquote frame-base))))))

(defmarco (emit-frame-pop out frame-base reg)
  (quasiquote (begin
    (emit-pop (unquote out) (unquote reg))
    (set! (unquote frame-base) (1- (unquote frame-base))))))

(define (closure-slot func index)
  (dispmem function-tag (* value-size (1+ index)) func))

(define (param-slot index frame-base)
  (dispmem 0 (* value-size (+ frame-base 4 index)) %sp))

(define (local-slot index frame-base)
  (dispmem 0 (* value-size (- frame-base 1 index)) %sp))

(define (varrec-operand varrec frame-base)
  (let* ((mode (varrec-attr varrec 'mode))
         (index (varrec-attr varrec 'index)))
    (cond ((eq? mode 'closure) (closure-slot %func index))
          ((eq? mode 'param) (param-slot index frame-base))
          ((eq? mode 'local) (local-slot index frame-base))
          ((eq? mode 'register) index)
          (true (error "strange variable mode ~S" mode)))))

;;; Functions

(define (emit-function-prologue out)
  (emit-push out %bp)
  (emit-mov out %sp %bp)
  (emit-push out %func))

(define (emit-function-epilogue out)
  (emit out "leave ; ret"))

(define (emit-restore-%func out frame-base)
  (emit-mov out (dispmem 0 (* (1+ frame-base) value-size) %sp) %func))

(define (emit-call out frame-base)
  (emit-push out %nargs)
  (emit out "call *~A" (value-sized (dispmem function-tag 0 %func)))
  (emit-restore-%func out frame-base)
  (emit-pop out %nargs))

(define (emit-alloc-function out result-reg label slot-count)
  (emit-sub out (immediate (* value-size (1+ slot-count))) %alloc)
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
