;;; Assembler

(define (make-asm-output))

(defmarco (emit out template . args)
  (quasiquote (format t (unquote (string-concat template "~%"))
                      (unquote-splicing args))))

(defmarco (emit-comment out template . args)
  (quasiquote (format t (unquote (string-concat "# " template "~%"))
                      (unquote-splicing args))))

(define (emit-literal out lit)
  (emit ouy ".quad ~A" lit))

(define label-counter 0)

(define (gen-label)
  (format nil ".L~D" (incf label-counter)))

(define (emit-label out l)
  (emit out "~A:" l))

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

;;; This is used for raw pointers in varargs handling.  We can re-use
;;; the number tag for this (but nothing should rely on the low-bits
;;; being clear)
(defconstant pointer-tag number-tag)

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

(define (usual-register reg)
  (funcall reg value-scale))

(defmarco (define-register name . variants)
  (quasiquote (define (unquote name)
                (lambda (scale) (elt (quote (unquote variants)) scale)))))

(define-register %a "%al" "%ax" "%eax" "%rax")
(define-register %b "%bl" "%bx" "%ebx" "%rbx")
(define-register %c "%cl" "%cx" "%ecx" "%rcx")
(define-register %d "%dl" "%dx" "%edx" "%rdx")
(define-register %si "%sil" "%si" "%esi" "%rsi")
(define-register %di "%dil" "%di" "%edi" "%rdi")
(define-register %sp "%spl" "%sp" "%esp" "%rsp")
(define-register %bp "%bpl" "%bp" "%ebp" "%rbp")

;;; Use RBX as the allocation pointer, since there are no relevant
;;; instructions that clobber it implicitly.
(define %alloc %b)

;;; Note that the implementations of rem and truncate depend on this
;;; definitions of general-registers
(define general-registers (list %a %c %d %si %di))
(define general-register-count (length general-registers))

(define %func %bp)
(define %funcres (first general-registers))

;;; %nargs is use to pass the number of arguments to functions.  We
;;; %reuse one of the general-registers for this, which means we have
;;; %to be really careful about invoking the operators that use it
;;; %(check-arg-count, arg-count).
(define %nargs (last-elem general-registers))

;;; Address modes

(define (immediate x)
  (lambda (scale) (format nil "$~A" x)))

(define (dispmem correction offset reg . reg2)
  (lambda (scale)
    (if (null reg2)
        (format nil "~A(~A)" (- offset correction) (usual-register reg))
        (format nil "~A(~A,~A)" (- offset correction) (usual-register reg)
                (usual-register (first reg2))))))

(define (mem reg)
  (lambda (scale) (format nil "(~A)" (usual-register reg))))

;;; Condition codes

(define (negate-cc cc)
  (if (eq (string-ref cc 0) #\n)
      (substring cc 1 (1- (string-length cc)))
      (string-concat "n" cc)))

;;; Instructions

(defconstant insn-size-suffix '("b" "w" "l" "q"))
(define usual-size-suffix (elt insn-size-suffix value-scale))

(defmarco (define-insn-2 name insn)
  (quasiquote
    (define ((unquote name) out src dest . scale)
      (emit-insn-2 out (unquote insn) src dest (and scale (car scale))))))

(define (emit-insn-2 out insn src dest scale)
  (unless scale
    (setq scale value-scale))
  (emit out "~A~A ~A, ~A" insn (elt insn-size-suffix scale)
        (funcall src scale) (funcall dest scale)))

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
  (setq dest-scale (if (null dest-scale) value-scale (car dest-scale)))
  (labels ((movzx (src-scale dest-scale)
             (emit out "mov~A ~A,~A"
                   (elt (elt '(("b")
                               ("zbw" "w")
                               ("zbl" "zwl" "l")) dest-scale) src-scale)
                   (funcall src src-scale)
                   (funcall dest dest-scale))))
    (if (= dest-scale 3)
        (if (= src-scale 3)
            (emit-mov out src dest 3)
            (movzx src-scale 2))
        (movzx src-scale dest-scale))))

(define (emit-clear out reg)
  (emit-xor out reg reg 2))

(define (emit-push out reg)
  (emit out "push~A ~A" usual-size-suffix (usual-register reg)))

(define (emit-pop out reg)
  (emit out "pop~A ~A" usual-size-suffix (usual-register reg)))

(define (emit-jump out label)
  (emit out "jmp ~A" label))

(define (emit-jcc out cc label)
  (emit out "j~A ~A" cc label))

(define (emit-branch out cc conddest)
  (emit-jcc out (negate-cc cc) (cdr conddest))
  (emit-jump out (car conddest)))

(define (emit-set out cc reg)
  (emit out "set~A ~A" cc (funcall reg 0)))

(defmarco (define-insn-1 name insn)
  (quasiquote
    (define ((unquote name) out oper . scale)
      (emit-insn-1 out (unquote insn) oper (and scale (car scale))))))

(define (emit-insn-1 out insn oper scale)
  (unless scale
    (setq scale value-scale))
  (emit out "~A~A ~A" insn (elt insn-size-suffix scale) (funcall oper scale)))

(define-insn-1 emit-neg "neg")
(define-insn-1 emit-idiv "idiv")

(defmarco (define-insn-0 name insn)
  (quasiquote
    (define ((unquote name) out . scale)
      (emit out "~A~A" (unquote insn)
            (elt insn-size-suffix (if scale (car scale) value-scale))))))

(define-insn-0 emit-rep-movs "rep ; movs")
(define-insn-0 emit-pushf "pushf")
(define-insn-0 emit-popf "popf")

;;; Dest conversions

(define (convert-value-reg-use dest-type)
  (if (dest-type-conditional? dest-type) 1 0))

(define (destination-reg dest regs)
  (if (dest-value? dest) dest (first regs)))

(define (emit-convert-value out reg dest)
  (cond ((dest-value? dest)
         (unless (eq? reg dest) (emit-mov out reg dest)))
        ((dest-conditional? dest)
         (emit-cmp out (immediate false-representation) reg)
         (emit-branch out "e" dest))))

(define (convert-cc-reg-use dest-type)
  (if (dest-type-value? dest-type) 1 0))

(define (emit-prepare-convert-cc out dest regs)
  (if (dest-value? dest)
      (begin
        (emit-clear out (car regs))
        (cdr regs))
      regs))

(define (emit-convert-cc out cc dest regs)
  (cond ((dest-value? dest)
         (let* ((reg (car regs)))
           (emit-set out cc reg)
           (emit-shl out (immediate tag-bits) reg 0)
           (emit-add out (immediate atom-tag) reg 0)))
        ((dest-conditional? dest)
         (emit-branch out cc dest))))

;;; Stack handling

;;; Layout:

;;; param N
;;; ...
;;; param 1
;;; param 0
;;; Return address
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

(define (emit-allocate-locals out n frame-base)
  (emit-sub out (immediate (* value-size n)) %sp)
  (+ frame-base (* value-size n)))

(define (emit-restore-frame-base out new old)
  (unless (= new old)
    (emit-add out (immediate (* value-size (- new old))) %sp)))

(defmarco (emit-frame-push out frame-base reg)
  (quasiquote (begin
    (emit-push (unquote out) (unquote reg))
    (set! (unquote frame-base) (1+ (unquote frame-base))))))

(defmarco (emit-frame-pop out frame-base . reg)
  (let* ((insn (if (null? reg)
                   (quasiquote (emit-add out (immediate value-size) %sp))
                   (quasiquote (emit-pop out (quote (car reg)))))))
    (quasiquote (begin
      (quote insn)
      (set! (unquote frame-base) (1- (unquote frame-base)))))))

(define (closure-slot index frame-base)
  (dispmem 0 (* value-size (1+ index)) %func))

(define (param-slot index frame-base offset)
  (dispmem 0 (+ offset (* value-size (+ frame-base 2 index))) %sp))

(define (local-slot index frame-base)
  (dispmem 0 (* value-size (+ frame-base -1 (- index))) %sp))

(define (varrec-operand varrec frame-base)
  (let* ((mode (varrec-attr varrec 'mode))
         (index (varrec-attr varrec 'index)))
    (cond ((eq mode 'closure) (closure-slot index frame-base))
          ((eq mode 'param) (param-slot index frame-base 0))
          ((eq mode 'local) (local-slot index frame-base)))))

;;; Functions

(define (emit-function-prologue out attrs arity-mismatch-label)
  (emit-push out %func))

(define (emit-function-epilogue out attrs arity-mismatch-label)
  (emit-add out (immediate value-size) %sp)
  (emit out "ret"))

(define (emit-call out frame-base)
  (emit out "call *~A" (usual-register (dispmem function-tag 0 %func)))
  ;; Restore %proc
  (emit-mov out (dispmem (* frame-base value-size) 0 %sp) %func))
