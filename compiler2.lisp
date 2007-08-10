;;; Compiler core

(defvar form-compilers ())

(defun unexpected-body-form (env form)
  (error "unexpected ~S in ~S" (car form) form))

(defun form-compiler-lambda (template body)
  `(lambda (env form)
     (destructuring-bind ,(restify-params (cdr template)) (cdr form) ,@body)))

(defmacro define-form-compiler (template &rest body)
  `(let ((f ,(form-compiler-lambda template body)))
     (push (list ',(car template) f f) form-compilers)))

(defmacro define-body-form-compiler (template &rest body)
  `(let ((f ,(form-compiler-lambda template body)))
     (push (list ',(car template) #'unexpected-body-form f) form-compilers)))

(defmacro define-operator (template &rest body)
  (let* ((keyword (car template))
         (func-name (intern (string-concat "OPERATOR-"
                                           (symbol-name keyword)))))
    `(progn (define (,func-name ,@(cdr template)) ,@body)
            (let ((f (lambda (env form)
                       (apply #',func-name (mapfor (arg (cdr form))
                                                   (compile-form env arg))))))
              (push (list ',(car template) f f) form-compilers)))))

(defun find-variable (env var)
  (let ((frame 0))
    (labels ((find-var (env var)
               (when (null env)
                 (error "unbound variable ~S" var))

               (let ((pos (position var (car env))))
                 (if pos (cons pos frame)
                     (progn (incf frame)
                            (find-var (cdr env) var))))))
      (find-var env var))))

(defun compile-form-aux (env form compiler-form-selector)
  (surrounding-comment form
    (cond ((pair? form)
           (let* ((keyword (car form))
                  (builtin (and (symbol? keyword)
                                (assoc keyword form-compilers))))
             (if builtin
                 (funcall (funcall compiler-form-selector builtin) env form)
                 (call (mapfor (subform form) (compile-form env subform))))))
          ((and (symbol? form) (not (null? form)))
           (variable-ref (find-variable env form)))
          (t
           (literal form)))))

(defun compile-form (env form)
  (compile-form-aux env form #'second))

(defun compile-body-form (env form)
  (compile-form-aux env form #'third))

(defun compile-body (env body)
  (let ((empty true)
        (defined ()))
    (labels ((scan (body)
               (dolist (form body)
                 (if (pair? form)
                   (let ((keyword (first form)))
                     (cond ((eq 'define keyword)
                            (push (second form) defined))
                           ((eq 'definitions keyword)
                            (scan (cdr form)))
                           (t
                            (setq empty false))))
                   (setq empty false)))))
      (scan body))
    (if (null? defined)
        (if empty compiled-unspecified
            (emitter-sequence (mapfor (form body)
                                      (compile-body-form env form))))
        (let ((child-env (cons defined env)))
          (allocate-frame (length defined)
            (emitter-sequence
              (append (mapfor (d defined)
                              (compile-variable-set! child-env d
                                                     compiled-unspecified))
                      (mapfor (form body)
                              (compile-body-form child-env form)))))))))

(define-body-form-compiler (definitions . body)
  (emitter-sequence (mapfor (form body) (compile-body-form env form))))

(define-form-compiler (begin . body)
  (compile-body env body))

(defun compile-variable-set! (env var val)
  (variable-set! (find-variable env var) val))

(define-body-form-compiler (define var . val)
  (compile-variable-set! env var (if (null val)
                                     compiled-unspecified
                                     (compile-form env (first val)))))

(define-form-compiler (set! var val)
  (compile-variable-set! env var (compile-form env val)))

(define-form-compiler (quote literal)
  (literal literal))

(define-form-compiler (lambda params . body)
  (let* ((param-count 0)
         (varargs false)
         (function-env
          (cons (labels ((scan-params (params)
                           (cond ((pair? params)
                                  (incf param-count)
                                  (cons (car params)
                                        (scan-params (cdr params))))
                                 ((null? params) ())
                                 ((symbol? params)
                                  (setq varargs true)
                                  (incf param-count)
                                  (cons params ())))))
                  (scan-params params)) env)))
    (lambda-value function-env param-count varargs
                  (compile-body function-env body))))

(define-operator (if test then . else)
  (operator-if-3 test then (if (null else) compiled-unspecified (car else))))

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
(defconstant pair-tag #b010)
(defconstant vector-tag #b011)
(defconstant string-tag #b100)
(defconstant atom-tag #b111)

(defconstant char-scale 0)

(defconstant function-size (* 2 value-size))
(defconstant pair-size (* 2 value-size))

(defun fixnum-representation (n) (ash n tag-bits))

(defconstant false-representation #b111)
(defconstant unspecified-representation #b10111)
(defconstant lowest-symbol-representation #b100000000111)

(defconstant simple-representations 
  `((false . ,false-representation)
    (true . #b1111)
    (unspecified . ,unspecified-representation)
    (() . #b11111)))

;;; Output

(defun make-output ()
  (cons (make-buffer) 0))

(defun fork-output (out)
  (cons (make-buffer) (cdr out)))

(defun join-output (out1 out2)
  (buffer-concat (car out1) (car out2)))

(defun emit (out template &rest args)
  (buffer-add (car out) (apply #'format nil template args)))

(defun write-output (out)
  (dolist (line (buffer-list (car out)))
    (format t "~A~%" line)))

(defun compile-program (program)
  (let* ((compiled-program (compile-body '((interned-symbols)) program))
         (compiled-define-symbols (compile-body-form '((interned-symbols)) `(set! interned-symbols ',(mapfor (sym-cons emitted-symbols) (car sym-cons)))))
         (compiled-wrapper (c-wrapper (allocate-frame 1 (emitter-sequence (list compiled-define-symbols compiled-program)))))
         (out (make-output)))
    (emit-for-value out compiled-wrapper general-registers)
    (write-output out)))

;;; Stack offsets

(defun stack-offset (out)
  (cdr out))

(defun offset-stack (out delta)
  (rplacd out (+ delta (cdr out))))

(defmacro with-frame (out &rest body)
  `(let ((saved-stack-offset (cdr ,out)))
     (rplacd ,out 0)
     ,@body
     (rplacd ,out saved-stack-offset)))

;;; Value emitters

(defun make-value-emitter (reg-usage emitter-func)
  (list reg-usage emitter-func reg-usage
        (lambda (out regs ctargets)
          (funcall emitter-func out regs)
          (emit-convert-value-to-conditional out (first regs) ctargets))))

(defun make-trashy-value-emitter (emitter-func)
  (make-value-emitter 1000 emitter-func))

(defun make-conditional-emitter (value-reg-usage value-emitter-func
                             conditional-reg-usage conditional-emitter-func)
  (list value-reg-usage value-emitter-func
        conditional-reg-usage conditional-emitter-func))

(defun value-register-usage (ve)
  (first ve))

(defun emit-for-value (out ve regs)
  (funcall (second ve) out regs))

(defun conditional-register-usage (ve)
  (third ve))

(defun emit-for-conditional (out ve regs ctargets)
  (funcall (fourth ve) out regs ctargets))

(defmacro make-simple-value-emitter (template-regs &rest template)
  `(make-value-emitter ,(length template-regs)
     (lambda (out regs)
       (let ((result (first regs)))
         (destructuring-bind (,@template-regs &rest others) regs ,@template)))))

(defun emitter-sequence (emitters)
  (if (null emitters)
      (make-value-emitter 0 (lambda (out regs)))
      (emitter-sequence-aux emitters)))

(defun emitter-sequence-aux (emitters)
  (if (null (cdr emitters)) (car emitters)
      (let ((emitter (car emitters))
            (rest-emitter (emitter-sequence-aux (cdr emitters))))
        (make-conditional-emitter
          (max (value-register-usage emitter)
               (value-register-usage rest-emitter))
          (lambda (out regs)
            (emit-for-value out emitter regs)
            (emit-for-value out rest-emitter regs))
          (max (value-register-usage emitter)
               (conditional-register-usage rest-emitter))
          (lambda (out regs ctargets)
            (emit-for-value out emitter regs)
            (emit-for-simple-conditional out rest-emitter regs ctargets))))))

(defun surrounding-comment (cmt emitter)
  (make-conditional-emitter
    (value-register-usage emitter)
    (lambda (out regs)
      (let* ((*print-pretty* nil)) (emit out "# ~S" cmt))
      (emit-for-value out emitter regs)
      (let* ((*print-pretty* nil)) (emit out "# Done ~S" cmt)))
    (conditional-register-usage emitter)
    (lambda (out regs ctargets)
      (let* ((*print-pretty* nil)) (emit out "# ~S" cmt))
      (let ((target-label (emit-for-conditional out emitter regs ctargets)))
        (let* ((*print-pretty* nil)) (emit out "# Done conditional ~S" cmt))
        target-label))))

;;; Operator support

(defmacro asm-value-operator (args temp-regs &rest template)
  `(make-value-emitter (max (args-register-usage (list ,@args))
                            ,(+ (length args) (length temp-regs)))
     (lambda (out regs)
       (emit-asm-operator-args out (list ,@args) regs ())
       (let ((result (car regs)))
         (destructuring-bind (,@args ,@temp-regs &rest others) regs
           ,@template)))))

(defun make-cc-emitter (part1-reg-usage part1-emitter-func
                          part2-reg-usage part2-emitter-func
                          cc)
  (make-conditional-emitter (max part1-reg-usage (1+ part2-reg-usage))
    (lambda (out regs)
      (funcall part1-emitter-func out (append (cdr regs) (list (car regs))))
      (emit-prepare-convert-cc-to-value out (car regs))
      (funcall part2-emitter-func out (cdr regs))
      (emit-convert-cc-to-value out cc (car regs)))
    (max part1-reg-usage part2-reg-usage)
    (lambda (out regs ctargets)
      (funcall part1-emitter-func out regs)
      (funcall part2-emitter-func out regs)
      (emit-cc-to-conditional out cc ctargets))))

(defmacro asm-cc-operator (args temp-regs cc &rest template)
  `(make-cc-emitter (args-register-usage (list ,@args))
                      (lambda (out regs) 
                        (emit-asm-operator-args out (list ,@args) regs ()))
                      ,(+ (length args) (length temp-regs))
                      (lambda (out regs)
                        (destructuring-bind (,@args ,@temp-regs &rest others)
                                            regs
                          ,@template))
                      ,cc))

(defun args-register-usage (args)
  (if (null args) 0
      (max (value-register-usage (car args))
           (1+ (args-register-usage (cdr args))))))

(defun emit-asm-operator-args (out args arg-regs supplemental-regs)
  (let ((trashy-args ())
        (trashy-arg-regs ())
        (non-trashy-args ())
        (non-trashy-arg-regs ())
        (other-regs ())
        (reg-count (+ (length arg-regs) (length supplemental-regs)))
        (untouched-regs 1000))
    ;; decide which arg-regs will receive trashy and non-trashy
    ;; args. note that this leaves the lists reversed
    (labels
      ((partition-args (args regs)
         (if (null args)
             (setq other-regs (append supplemental-regs regs))
             (let* ((arg (car args))
                    (reg-usage (value-register-usage arg)))
               (if (> reg-usage reg-count)
                   ;; trashy
                   (progn (push arg trashy-args)
                          (push (car regs) trashy-arg-regs))
                   ;; non-trashy
                   (progn (push arg non-trashy-args)
                          (push (car regs) non-trashy-arg-regs)
                          (setq untouched-regs
                                (min untouched-regs (- reg-count reg-usage)))
                          (decf reg-count)))
               (partition-args (cdr args) (cdr regs))))))
      (partition-args args arg-regs))
      
    ;; emit code for trashy args
    (unless (null trashy-args)
      (labels ((emit-trashy-args (out args)
                 (dolist (arg args)
                   (emit-for-value out arg general-registers)
                   (asm-push out (first general-registers)))))
        ;; if there is a register available to hold the result of
        ;; the last trashy arg, we don't need to store it on the stack
        (if (= untouched-regs 0)
            (emit-trashy-args out (nreverse trashy-args))
            (progn
              (emit-trashy-args out (nreverse (cdr trashy-args)))
              (emit-for-value out (car trashy-args)
                              (append trashy-arg-regs supplemental-regs
                                      non-trashy-arg-regs other-regs))
              (setq trashy-arg-regs (cdr trashy-arg-regs))))))

    ;; emit code for non-trashy args
    (labels ((emit-non-trashy-args (out args regs)
               (unless (null args)
                 (emit-for-value out (car args) regs)
                 (emit-non-trashy-args out (cdr args) (cdr regs)))))
      (emit-non-trashy-args out (nreverse non-trashy-args)
                            (append (nreverse non-trashy-arg-regs)
                                    supplemental-regs trashy-arg-regs
                                    other-regs)))

    ;; restore trashy arg results
    (dolist (reg trashy-arg-regs)
      (asm-pop out reg))))

;;; Assembler

(defvar label-counter 0)

(defun gen-label ()
  (format nil ".L~D" (incf label-counter)))

(defun emit-literal (out lit)
  (emit out ".quad ~A" lit))

(defun emit-label (out l)
  (emit out "~A:" l))

;;; Address modes

(defun immediate (x)
  (lambda (scale) (format nil "$~A" x)))

(defun dispmem (correction offset reg &rest reg2)
  (lambda (scale)
    (if (null reg2)
        (format nil "~A(~A)" (- offset correction) (usual-register reg))
        (format nil "~A(~A,~A)" (- offset correction) (usual-register reg)
                (usual-register (first reg2))))))

(defun mem (reg)
  (lambda (scale) (format nil "(~A)" (usual-register reg))))

;;; Registers

(defun usual-register (reg)
  (funcall reg value-scale))

(defmacro define-register (name &rest variants)
  `(defvar ,name (lambda (scale) (elt ',variants scale))))

(define-register %a "%al" "%ax" "%eax" "%rax")
(define-register %b "%bl" "%bx" "%ebx" "%rbx")
(define-register %c "%cl" "%cx" "%ecx" "%rcx")
(define-register %d "%dl" "%dx" "%edx" "%rdx")
(define-register %si "%sil" "%si" "%esi" "%rsi")
(define-register %di "%dil" "%di" "%edi" "%rdi")
(define-register %sp "%spl" "%sp" "%esp" "%rsp")

;;; Use RBX as the allocation pointer, since there are no relevant
;;; instructions that clobber it implicitly.
(defvar %alloc %b)

;;; (first general-registers) is used to hold the result of body forms.
;;; (second general-registers) is used to pass the arg frame to a function.
(defvar general-registers (list %a %c %d %si %di))

;;; Instructions

(defconstant insn-size-suffix '("b" "w" "l" "q"))
(defvar usual-size-suffix (elt insn-size-suffix value-scale))

(defmacro define-asm-op-2 (name insn)
  `(defun ,name (out src dest &rest scale)
     (asm-op-2* out ,insn src dest (and scale (car scale)))))

(defun asm-op-2* (out insn src dest scale)
  (unless scale
    (setq scale value-scale))
  (emit out "~A~A ~A, ~A" insn (elt insn-size-suffix scale)
        (funcall src scale) (funcall dest scale)))

(define-asm-op-2 asm-mov "mov")
(define-asm-op-2 asm-lea "lea")
(define-asm-op-2 asm-add "add")
(define-asm-op-2 asm-sub "sub")
(define-asm-op-2 asm-imul "imul")
(define-asm-op-2 asm-and "and")
(define-asm-op-2 asm-or "or")
(define-asm-op-2 asm-xor "xor")
(define-asm-op-2 asm-cmp "cmp")
(define-asm-op-2 asm-shl "shl")
(define-asm-op-2 asm-sar "sar")

(defun asm-movzx (out src dest src-scale &rest dest-scale)
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
            (asm-mov out src dest 3)
            (movzx src-scale 2))
        (movzx src-scale dest-scale))))

(defun asm-clear (out reg)
  (asm-xor out reg reg 2))

(defun asm-push (out reg)
  (emit out "push~A ~A" usual-size-suffix (usual-register reg))
  (offset-stack out value-size))

(defun asm-pop (out &rest reg)
  (if (null reg)
      (asm-add out (immediate value-size) %sp)
      (emit out "pop~A ~A" usual-size-suffix (usual-register (car reg))))
  (offset-stack out (- value-size)))

(defun asm-branch (out cc dest)
  (emit out "j~A ~A" cc dest))

(defun asm-set (out cc reg)
  (emit out "set~A ~A" cc (funcall reg 0)))

(defun asm-call (out rm)
  (emit out "call *~A" (usual-register rm)))

(defmacro define-asm-op-1 (name insn)
  `(defun ,name (out oper &rest scale)
     (asm-op-1* out ,insn oper (and scale (car scale)))))

(defun asm-op-1* (out insn oper scale)
  (unless scale
    (setq scale value-scale))
  (emit out "~A~A ~A" insn (elt insn-size-suffix scale) (funcall oper scale)))

(define-asm-op-1 asm-neg "neg")
(define-asm-op-1 asm-idiv "idiv")

(defmacro define-asm-op-0 (name insn)
  `(defun ,name (out &rest scale)
     (emit out "~A~A" ,insn
           (elt insn-size-suffix (if scale (car scale) value-scale)))))

(define-asm-op-0 asm-rep-movs "rep ; movs")
(define-asm-op-0 asm-pushf "pushf")
(define-asm-op-0 asm-popf "popf")

(defun emit-convert-value-to-conditional (out reg ctargets)
  (asm-cmp out (immediate false-representation) reg)
  (emit-cc-to-conditional out "ne" ctargets))

(defun emit-prepare-convert-cc-to-value (out reg)
  (asm-clear out reg))

(defun emit-convert-cc-to-value (out cc reg)
  (asm-set out cc reg)
  (asm-shl out (immediate tag-bits) reg 0)
  (asm-add out (immediate atom-tag) reg 0))

;;; Literals

(defun compile-literal (lit)
  (cond ((pair? lit) (compile-cons-literal lit))
        ((integerp lit) (fixnum-representation lit))
        ((characterp lit) (fixnum-representation (char-code lit)))
        ((stringp lit) (compile-string-literal lit))
        (t (let* ((c (assoc lit simple-representations)))
             (cond (c (cdr c))
                   ((symbol? lit) (compile-symbol-literal lit))
                   (t (error "unrecognised literal ~S" lit)))))))

(defun compile-cons-literal (lit)
  (let* ((l (gen-label))
         (a (compile-literal (car lit)))
         (d (compile-literal (cdr lit)))
         (out (make-output)))
    (emit out ".data")
    (emit out ".align ~D" allocation-alignment)
    (emit-label out l)
    (emit-literal out a)
    (emit-literal out d)
    (write-output out)
    (format nil "~A+~D" l pair-tag)))

(defun compile-string-literal (str)
  (let* ((l (gen-label))
         (out (make-output)))
    (emit out ".data")
    (emit out ".align ~D" value-size)
    (emit-label out l)
    (emit-literal out (fixnum-representation (length str)))
    (emit out ".ascii ~S" str)
    (write-output out)
    (format nil "~A+~D" l string-tag)))

(defvar emitted-symbols ())

(defun compile-symbol-literal (sym)
  (let ((emitted (assoc sym emitted-symbols)))
    (if emitted (cdr emitted)
        (let* ((l (gen-label))
               (out (make-output)))
          (emit out ".data")
          (emit out ".align ~D" value-size)
          (emit-label out l)
          (emit-literal out 
                 (compile-string-literal (string-downcase (symbol-name sym))))
          (write-output out)
          (let* ((const (format nil "~A+~D" l atom-tag)))
            (push (cons sym const) emitted-symbols)
            const)))))

(defvar false-literal
  (make-conditional-emitter 1
    (lambda (out regs)
      (asm-mov out (immediate false-representation) (car regs)))
    0
    (lambda (out regs ctargets)
      (emit-conditional-false out ctargets))))

(defun true-literal (lit)
  (let ((compiled-lit (compile-literal lit)))
    (make-conditional-emitter 1
      (lambda (out regs)
        (asm-mov out (immediate compiled-lit) (car regs)))
      0
      (lambda (out regs ctargets)
        (emit-conditional-true out ctargets)))))

(defun literal (lit)
  (if (eq lit 'false) false-literal (true-literal lit)))

(defvar compiled-unspecified (literal 'unspecified))

;;; Frame structure

(defun frame-length (var-count) (1+ var-count))
(defun frame-var-offset (var) (* value-size (+ 2 var)))
(defun frame-link (frame) (dispmem 0 value-size frame))

(defun current-frame (out)
  (dispmem 0 (stack-offset out) %sp))

;;; Variables

(defun find-frame (out frame reg)
  (asm-mov out (current-frame out) reg)
  (dotimes (count frame) (asm-mov out (frame-link reg) reg)))

(defun variable-set! (var val)
  (asm-value-operator (val) (frame)
    (find-frame out (cdr var) frame)
    (asm-mov out val (dispmem 0 (frame-var-offset (car var)) frame))))

(defun variable-ref (var)
  (make-simple-value-emitter (frame)
    (find-frame out (cdr var) frame)
    (asm-mov out (dispmem 0 (frame-var-offset (car var)) frame) frame)))

(defun allocate-frame (var-count emitter)
  (make-value-emitter
    (max 1 (value-register-usage emitter))
    (lambda (out regs)
      (asm-sub out (immediate (frame-var-offset var-count)) %alloc)
      (asm-mov out (current-frame out) (first regs))
      (asm-push out %alloc)
      ;; when we have a GC, we should fill in the size of the frame
      (asm-mov out (first regs) (frame-link %alloc))
      (with-frame out (emit-for-value out emitter regs))
      (asm-pop out))))

;;; Functions

(defvar arity-mismatch-label "arity_mismatch")
(defvar handle-varargs-label "handle_varargs")

;;; Registers used in the call sequence.  Since calls are always
;;; trashy, these are based on general-registers
(defvar %call-frame (second general-registers))
(defvar %param-frame-length (first general-registers))
(defvar non-call-frame-general-registers (cons (car general-registers)
                                               (cddr general-registers)))
(defvar %result (first general-registers))
(defvar %function-frame (third general-registers))

(defun emit-call-handler (out env handler param-count)
  (let ((handler-ve (compile-form env handler)))
    (emit out "# call-handler ~A" handler)
    (emit-for-value out handler-ve non-call-frame-general-registers)
    (destructuring-bind (handler-func function-frame &rest others)
                        non-call-frame-general-registers 
      (asm-mov out (dispmem function-tag value-size handler-func)
                   function-frame)
      (asm-mov out (immediate (* 3 value-size))
                   (dispmem (* 4 value-size) 0 %alloc))
      (asm-add out (immediate vector-tag) %call-frame)
      (asm-mov out function-frame (dispmem (* 3 value-size) 0 %alloc))
      (asm-mov out %call-frame
                   (dispmem (* 4 value-size) (frame-var-offset 0) %alloc))
      (asm-mov out
               (immediate (fixnum-representation (frame-length param-count)))
               (dispmem (* 4 value-size) (frame-var-offset 1) %alloc))
      (asm-sub out (immediate (* 4 value-size)) %alloc)
      (asm-mov out %alloc %call-frame)
      (asm-call out (dispmem function-tag 0 handler-func))
      (asm-sub out (immediate vector-tag) %result)
      (asm-mov out %result (current-frame out)))))

(defun lambda-value (env param-count varargs body)
  (let ((l (gen-label))
        (arity-mismatch-label (gen-label))
        (out (make-output)))
    (emit out ".text")
    (emit-label out l)
    (asm-push out %call-frame)

    (with-frame out
      (if varargs (emit-call-handler out env 'handle-varargs param-count)
          (progn
            (asm-cmp out 
                (immediate (fixnum-representation (frame-length param-count)))
                (dispmem 0 0 %call-frame))
            (asm-branch out "ne" arity-mismatch-label)))
      
      (emit-for-value out body general-registers))

    (asm-pop out)
    (emit out "ret")

    (unless varargs
      (emit-label out arity-mismatch-label)
      (emit-call-handler out env 'arity-mismatch param-count))

    (write-output out)
      
    (make-simple-value-emitter (frame)
      (asm-mov out (current-frame out) frame)
      (asm-mov out (immediate l) (dispmem function-size 0 %alloc))
      (asm-mov out frame (dispmem function-size value-size %alloc))
      (asm-lea out (dispmem function-size function-tag %alloc) result)
      (asm-sub out (immediate function-size) %alloc))))

(defun generally-trashy? (ve)
  (>= (value-register-usage ve) (length general-registers)))

(defun call (func-and-args)
  (make-trashy-value-emitter
    (lambda (out regs)
      (let* ((args (cdr func-and-args))
             (arg-count (length args))
             (trashy-args
              (findfor (arg func-and-args) (generally-trashy? arg))))
        ;; Allocate the frame for the call
        (asm-sub out (immediate (frame-var-offset arg-count)) %alloc)
        (when trashy-args
          (asm-push out %alloc))
        (asm-mov out
                 (immediate (fixnum-representation (frame-length arg-count)))
                 (dispmem 0 0 %alloc))
        
        ;; If the first argument is not trashy, setup %call-frame
        (when (or (null args) (not (generally-trashy? (car args))))
          (asm-mov out %alloc %call-frame))

        (labels ((emit-arg (arg)
                   (if (generally-trashy? arg)
                       (progn
                         (emit-for-value out arg general-registers)
                         (asm-mov out (dispmem 0 0 %sp) %call-frame))
                       (emit-for-value out arg
                                       non-call-frame-general-registers))))
          ;; evaluate arguments
          (let ((param 0))
            (dolist (arg args)
              (emit-arg arg)
              (asm-mov out %result
                       (dispmem 0 (frame-var-offset param) %call-frame))
              (incf param)))

          ;; evaluate function
          (emit-arg (first func-and-args))

          ;; make the call
          (asm-mov out (dispmem function-tag value-size %result)
                   %function-frame)
          (when trashy-args (asm-pop out))
          (asm-mov out %function-frame (frame-link %call-frame))
          (asm-call out (dispmem function-tag 0 %result))
          (unless (eq %result (car regs))
            (asm-mov out %result (car regs))))))))

(define-operator (apply-frame func vec)
  (make-trashy-value-emitter
    (lambda (out regs)
      (emit-asm-operator-args out (list func vec) general-registers ())
      (asm-sub out (immediate vector-tag) %call-frame)
      (asm-mov out (dispmem function-tag value-size %result) %function-frame)
      (asm-mov out %function-frame (frame-link %call-frame))
      (asm-call out (dispmem function-tag 0 %result))
      (unless (eq %result (car regs))
            (asm-mov out %result (car regs))))))

;;; Conditionals

(defun operator-if-3 (test then else)
  (let ((ru (max (conditional-register-usage test)
                 (value-register-usage then)
                 (value-register-usage else))))
    (make-conditional-emitter ru
      (lambda (out regs)
        (let ((l1 (gen-label)) (l2 (gen-label)) (l3 (gen-label)))
          (emit-for-simple-conditional out test regs (list* l1 l1 l2))
          (emit-label out l1)
          (emit-for-value out then regs)
          (emit out "jmp ~A" l3)
          (emit-label out l2)
          (emit-for-value out else regs)
          (emit-label out l3)))
      ru
      (lambda (out regs ctargets)
        (let* ((true-false-labels (cdr ctargets))
               (else-part (frob-part out else regs (car ctargets)
                                     true-false-labels))
               (then-part (frob-part out then regs
                                     (part-initial-label else-part)
                                     true-false-labels))
               (test-part (frob-part out test regs
                                     (part-initial-label then-part)
                                     (cons (part-target-label then-part)
                                           (part-target-label else-part)))))
          (dolist (part (list test-part then-part else-part))
            (join-output out (part-output part))))))))

(defun frob-part (orig-out emitter regs following-label true-false-labels)
  (let ((out (fork-output orig-out))
        (initial-label (gen-label)))
    (emit-label out initial-label)
    (let ((target-label (emit-for-conditional out emitter regs
                                              (cons following-label
                                                    true-false-labels))))
      (if target-label
          (list following-label target-label out)
          (list initial-label initial-label out)))))

(defun part-initial-label (part) (first part))
(defun part-target-label (part) (second part))
(defun part-output (part) (third part))

(defun emit-for-simple-conditional (out emitter regs ctargets)
  (let ((target-label (emit-for-conditional out emitter regs ctargets)))
    (when (and target-label (not (eq target-label (car ctargets))))
      (emit out "jmp ~A" target-label))))

(defun emit-conditional-false (out ctargets)
  (cddr ctargets))

(defun emit-conditional-true (out ctargets)
  (cadr ctargets))

(defun emit-cc-to-conditional (out cc ctargets)
  (let ((following-label (car ctargets))
        (true-label (cadr ctargets))
        (false-label (cddr ctargets)))
    (if (eq following-label false-label)
        (asm-branch out cc true-label)
        (progn
          (asm-branch out (negate-cc cc) false-label)
          (unless (eq following-label true-label)
            (emit out "jmp ~A" true-label))))
    false))

(defun negate-cc (cc)
  (if (eq (string-ref cc 0) #\n)
      (substring cc 1 (1- (string-length cc)))
      (string-concat "n" cc)))

;;; Comparisons

(defmacro define-comparison-operator (name cc)
  `(define-operator (,name a b)
     (asm-cc-operator (a b) () ,cc (asm-cmp out b a))))

(define-comparison-operator eq? "e")
(define-comparison-operator = "e")
(define-comparison-operator /= "ne")
(define-comparison-operator > "g")
(define-comparison-operator >= "ge")
(define-comparison-operator < "l")
(define-comparison-operator <= "le")

;;; Tagging etc.

(defun tag-check (tag val)
  (asm-cc-operator (val) () "e"
    (asm-and out (immediate tag-mask) val 0)
    (asm-cmp out (immediate tag) val 0)))

(define-operator (function? val)
  (tag-check function-tag val))

;;; Conses

(define-operator (pair? val)
  (tag-check pair-tag val))

(define-operator (null? val)
  (operator-eq? val (literal ())))

(define-operator (cons a b)
  (asm-value-operator (a b) ()
    (asm-mov out a (dispmem pair-size 0 %alloc))
    (asm-mov out b (dispmem pair-size value-size %alloc))
    (asm-lea out (dispmem pair-size pair-tag %alloc) result)
    (asm-sub out (immediate pair-size) %alloc)))

(define-operator (car x)
  (asm-value-operator (x) () 
    (asm-mov out (dispmem pair-tag 0 x) result)))

(define-operator (cdr x)
  (asm-value-operator (x) ()
    (asm-mov out (dispmem pair-tag value-size x) result)))

(define-operator (rplaca c a)
  (asm-value-operator (c a) ()
    (asm-mov out a (dispmem pair-tag 0 c))))

(define-operator (rplacd c d)
  (asm-value-operator (c d) ()
    (asm-mov out d (dispmem pair-tag value-size c))))

;;; Symbols

(define-operator (symbol? val)
  (asm-cc-operator (val) () "e"
    (let ((l (gen-label)))
      (asm-cmp out (immediate lowest-symbol-representation) val)
      (asm-branch out "l" l)
      (asm-and out (immediate tag-mask) val)
      (asm-cmp out (immediate atom-tag) val)
      (emit-label out l))))

(define-operator (symbol-name sym)
  (asm-value-operator (sym) ()
    (asm-mov out (dispmem atom-tag 0 sym) result)))

(define-operator (primitive-make-symbol str)
  (asm-value-operator (str) ()
    (asm-mov out str (dispmem allocation-alignment 0 %alloc))
    (asm-lea out (dispmem allocation-alignment atom-tag %alloc) result)
    (asm-sub out (immediate allocation-alignment) %alloc)))

;;; Numbers

(define-operator (number? val)
  (tag-check number-tag val))

(defun operator-identity (val)
  (asm-value-operator (val) ()))

(defun operator-+-2 (a b)
  (asm-value-operator (a b) ()
    (asm-add out b a)))

(define-operator (+ . args)
  (if (null? args) (literal 0)
      (if (null? (cdr args)) (operator-identity (car args))
          (reduce #'operator-+-2 args))))

(defun operator-*-2 (a b)
  (asm-value-operator (a b) ()
    (asm-sar out (immediate tag-bits) a)
    (asm-imul out b a)))

(define-operator (* . args)
  (if (null? args) (literal 1)
      (if (null? (cdr args)) (operator-identity (car args))
          (reduce #'operator-*-2 args))))

(defun operator---2 (a b)
  (asm-value-operator (a b) ()
    (asm-sub out b a)))

(define-operator (- arg . args)
  (if (null args) (asm-value-operator (arg) () (asm-neg out arg))
      (reduce #'operator---2 (cons arg args))))

(define-operator (truncate a b)
  (make-trashy-value-emitter
    (lambda (out regs)
      (emit-asm-operator-args out (list a b) (list %a %c) ())
      (asm-mov out %a %d)
      (asm-sar out (immediate 63) %d)
      (asm-idiv out %c)
      (asm-shl out (immediate tag-bits) %a)
      (unless (eq %a (car regs))
        (asm-mov out %a (car regs))))))

(define-operator (rem a b)
  (make-trashy-value-emitter
    (lambda (out regs)
      (emit-asm-operator-args out (list a b) (list %a %c) ())
      (asm-mov out %a %d)
      (asm-sar out (immediate 63) %d)
      (asm-idiv out %c)
      (unless (eq %d (car regs))
        (asm-mov out %d (car regs))))))

(define-operator (fixnum->raw val)
  (asm-value-operator (val) () (asm-sar out (immediate tag-bits) val)))

(define-operator (raw->fixnum val)
  (asm-value-operator (val) () (asm-shl out (immediate tag-bits) val)))

;;; Primitive vectors

(defun operator-make-vec (tag index-scale len)
  (if (= index-scale tag-bits)
      (asm-value-operator (len) ()
        (asm-sub out (immediate value-size) %alloc)
        (asm-sub out len %alloc)
        (unless (= allocation-alignment-scale index-scale)
          (asm-and out (immediate allocation-alignment-mask) %alloc))
        (asm-mov out len (dispmem 0 0 %alloc))
        (asm-lea out (dispmem 0 tag %alloc) result))
      (asm-value-operator (len) (raw-len)
        (asm-mov out len raw-len)
        (asm-sar out (immediate (- tag-bits index-scale)) raw-len)
        (asm-sub out (immediate value-size) %alloc)
        (asm-sub out raw-len %alloc)
        (asm-and out (immediate allocation-alignment-mask) %alloc)
        (asm-mov out len (dispmem 0 0 %alloc))
        (asm-lea out (dispmem 0 tag %alloc) result))))

(defun operator-vec-length (tag index-scale vec)
  (asm-value-operator (vec) () (asm-mov out (dispmem tag 0 vec) result)))

(defun operator-vec-ref (tag index-scale vec index)
  (asm-value-operator (vec index) ()
    (unless (= index-scale tag-bits)
      (asm-sar out (immediate (- tag-bits index-scale)) index))
    (asm-movzx out (dispmem tag value-size vec index) result index-scale)))

(defun operator-vec-set! (tag index-scale vec index val)
  (asm-value-operator (vec index val) ()
    (unless (= index-scale tag-bits)
      (asm-sar out (immediate (- tag-bits index-scale)) index))
    (asm-mov out val (dispmem tag value-size vec index) index-scale)))

(defun operator-vec-address (tag index-scale vec index)
  (asm-value-operator (vec index) ()
    (unless (= index-scale tag-bits)
      (asm-sar out (immediate (- tag-bits index-scale)) index))
    (asm-lea out (dispmem tag value-size vec index) result)))


(defun operator-vec-copy (tag index-scale src src-index dest dest-index len)
  (labels ((memory-copy (forward src-index dest-index)
             (make-trashy-value-emitter
               (lambda (out regs)
                 (let ((src-addr
                        (operator-vec-address tag index-scale src src-index))
                       (dest-addr
                        (operator-vec-address tag index-scale dest dest-index)))
                   (emit-asm-operator-args out
                                           (list src-addr dest-addr len)
                                           (list %si      %di       %c)
                                           ())
                   (destructuring-bind (src-addr dest-addr len)
                                  (list %si      %di       %c)
                     (emit out (if forward "cld" "std"))
                     (asm-sar out (immediate tag-bits) len)
                     (asm-rep-movs out index-scale))))))
           (offset-index (index)
             (operator---2 (operator-+-2 index len) (literal 1))))
    (operator-if-3 (operator-> src-index dest-index)
                   (memory-copy true src-index dest-index)
                   (memory-copy false (offset-index src-index)
                                (offset-index dest-index)))))

;;; Vectors

(define-operator (make-vector len)
  (operator-make-vec vector-tag value-scale len))

(define-operator (vector-length vec)
  (operator-vec-length vector-tag value-scale vec))

(define-operator (primitive-vector-ref vec index)
  (operator-vec-ref vector-tag value-scale vec index))

(define-operator (primitive-vector-set! vec index val)
  (operator-vec-set! vector-tag value-scale vec index val))

(define-operator (primitive-vector-copy src src-index dest dest-index len)
  (operator-vec-copy vector-tag value-scale src src-index dest dest-index len))

;;; Strings

(define-operator (string? val)
  (tag-check string-tag val))

(define-operator (make-string len)
  (operator-make-vec string-tag char-scale len))

(define-operator (string-length str)
  (operator-vec-length string-tag char-scale str))

(define-operator (primitive-string-ref str index)
  (operator-raw->fixnum (operator-vec-ref string-tag char-scale str index)))

(define-operator (primitive-string-set! str index val)
  (operator-vec-set! string-tag char-scale str index
                     (operator-fixnum->raw val)))

(define-operator (string-address str index)
  (operator-vec-address string-tag char-scale str index))

(define-operator (primitive-string-copy src src-index dest dest-index len)
  (operator-vec-copy string-tag char-scale src src-index dest dest-index len))

;;; Misc. runtime

(define-operator (error-halt message args)
  (asm-value-operator (message args) ()
    (let ((l (gen-label)))
      (emit-label out l)
      (emit out "hlt")
      (emit out "jmp ~A" l))))

(defun emit-set-ac-flag (out enable)
  (asm-pushf out)
  (if enable
      (asm-or out (immediate #x40000) (mem %sp) 2)
      (asm-and out (immediate #xfffbffff) (mem %sp) 2))
  (asm-popf out))

(defun c-wrapper (program)
  (make-trashy-value-emitter
    (lambda (out regs)
      (emit out ".text")
      (emit out ".globl lisp")
      (emit out "lisp:")
      (asm-mov out %si %alloc)
      (emit-set-ac-flag out true)
    
      (emit-for-value out program regs)

      ;; use the alloc pointer as the result
      (asm-mov out %alloc %a)

      (emit out "cld")
      (emit-set-ac-flag out false)
      (emit out "ret"))))

(defvar c-call-registers (list %di %si %d %c))

(define-form-compiler (c-call function-name . args)
  (let ((compiled-args (mapfor (arg args) (compile-form env arg))))
    (make-trashy-value-emitter
      (lambda (out regs)
        (emit-asm-operator-args out compiled-args c-call-registers ())
        (emit out "cld")
        (emit-set-ac-flag out false)
        (emit out "call ~A" function-name)
        (unless (eq %a (car regs))
          (asm-mov out %a (car regs)))
        (emit-set-ac-flag out true)))))
