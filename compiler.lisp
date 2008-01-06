;;; The compiler

(define (compile-program program)
  ;; Compile the program, printing the resulting assembly on standard
  ;; output
  (set! program (list* 'begin () (normalize-forms program)))
  (set! program (gather-symbols program))
  (set! program (car (eliminate-definitions program)))
  (replace-empty-bodies program)
  (normalize-lambdas program)
  (collect-defines program)
  (simplify program)
  (resolve-variables program)
  (classify-variables program)
  (collect-closures program)
  (introduce-boxes program)
  (lambda-label program)
  (codegen-program program)
  ;(format~ true "~S~%" program)
  )

(define keywords-1
  '(begin definitions if

    error-halt
    
    eq? function?
    
    symbol? symbol-name primitive-make-symbol
    
    pair? car cdr cons rplaca rplacd
    
    number? < <= > >= = /= + * - rem truncate 
    
    character? char-code code-char

    string? make-string string-length raw-string-address
    primitive-string-ref primitive-string-set! primitive-string-copy
    
    vector? make-vector vector-length raw-vector-address
    primitive-vector-ref primitive-vector-set! primitive-vector-copy

    fixnum->raw raw->fixnum

    raw-args-address
    raw-apply-with-args raw-apply-jump
    raw-arg-set! raw-arg-ref
    raw-rdtsc))

(define keywords-2 '(define lambda set! quote
                     c-call))

(define internal-keywords '(ref call
                            make-box box-ref box-set!
                            check-arg-count arg-count
                            negate
                            make-vec vec-length raw-vec-address
                            vec-ref vec-set! vec-copy))

(define all-keywords (append keywords-2 internal-keywords keywords-1))

;;; Transform a program so that every form begins with a
;;; distinguishing keyword:
;;;
;;; if, definitions, define, lambda, set!: as in source
;;; language
;;;
;;; quote: All quoted and literal forms
;;;
;;; call: a function call
;;;
;;; ref: a variable reference
;;;
;;; And the builtin operators

(define (normalize form)
  (cond ((pair? form)
           (let* ((keyword (car form)))
             (cond ((member? keyword keywords-2) (normalize-2 form))
                   ((member? keyword keywords-1) (normalize-1 form))
                   (true (list* 'call () (normalize-forms form))))))
        ((symbol? form) (list 'ref form))
        (true (list 'quote form))))

(define (normalize-forms forms)
  (mapfor (form forms) (normalize form)))

(define (normalize-1 form)
  (list* (car form) () (normalize-forms (cdr form))))

(define (normalize-2 form)
  (list* (car form) (cadr form) (normalize-forms (cddr form))))


(define (compound-symbol . pieces)
  (intern (string-flatten (mapfor (p pieces)
                            (cond ((string? p) (string-symbolcase p))
                                  ((symbol? p) (symbol-name p))
                                  (true (error "~S" p)))))))


;;; A walker is a function which recurses over a program tree,
;;; dispatching forms to handlers based on their keywords.
;;; define-walker is a macro providing a convenient way to define
;;; walkers.
;;;
;;; A define-walker needs to be paired with a recursion function,
;;; which defines how to handle forms that don't match one of the
;;; handlers

(define (multi-acons keys val rest)
  (if (pair? keys)
      (append (mapfor (key keys) (cons key val)) rest)
      (acons keys val rest)))

(defmarco (define-walker name implicit-vars)
  (let* ((form-name name)
         (recurse-name (compound-symbol name "-recurse"))
         (define-name (compound-symbol "define-" name))
         (alist-name (compound-symbol name "-alist")))
    (quasiquote (definitions
      (define (unquote alist-name) ())

      (define ((unquote form-name) form (unquote-splicing implicit-vars))
        (let* ((kw (car form))
               (a (assoc kw (unquote alist-name))))
          (if a
              (funcall (cdr a) form (unquote-splicing implicit-vars))
              (if (member? kw all-keywords)
                  ((unquote recurse-name) form (unquote-splicing implicit-vars))
                  (error "unknown form ~S" form)))))

      (defmarco ((unquote define-name) template . body)
        (quasiquote
          (set! (unquote (unquote alist-name))
                (multi-acons '(unquote (car template))
                       (lambda (form (unquote-splicing (unquote implicit-vars)))
                         (bind (unquote (cdr template)) (cdr form)
                               (unquote-splicing body)))
                       (unquote (unquote alist-name))))))))))

;;; define-trivial-walker is like define-walker, but it also provides
;;; a recusrion function that does a straightforward recursion into
;;; subforms

(defmarco (define-trivial-walker name implicit-vars)
  (let* ((form-name name)
         (recurse-name (compound-symbol name "-recurse"))
         (forms-name (compound-symbol name "-forms")))
    (quasiquote (definitions
      (define-walker (unquote name) (unquote implicit-vars))

      (define ((unquote forms-name) forms (unquote-splicing implicit-vars))
        (dolist (form forms)
          ((unquote form-name) form (unquote-splicing implicit-vars))))

      (define ((unquote recurse-name) form (unquote-splicing implicit-vars))
        ((unquote forms-name) (cddr form) (unquote-splicing implicit-vars)))))))

;;; Utils

(define (attr-ref attrs attr)
  (let* ((a (assoc attr attrs)))
    (unless a (error "no attribute ~S in ~S" attr attrs))
    (cdr a)))

(define (attr-set! attrs attr val)
  (let* ((a (assoc attr attrs)))
    (if a (begin (rplacd a val)
                 attrs)
        (acons attr val attrs))))

(define (attr-remove attrs-head attr)
  (labels ((aux (l1 l2)
             (cond ((null? l2)
                    false)
                   ((eq? (caar l2) attr)
                    (rplacd l1 (cdr l2))
                    (cdar l2))
                   (true
                    (aux l2 (cdr l2))))))
    (aux attrs-head (cdr attrs-head))))


(define (form-attr form attr)
  (attr-ref (second form) attr))

(define (form-attr-set! form attr val)
  (rplaca (cdr form) (attr-set! (second form) attr val)))


(define (varrec-attr varrec attr)
  (attr-ref (cdr varrec) attr))

(define (varrec-attr-set! varrec attr val)
  (rplacd varrec (attr-set! (cdr varrec) attr val)))

(define (varrec-attr-remove varrec attr)
  (attr-remove varrec attr))

(define (varrec-var varrec) (first varrec))

(define (overwrite-form form replacement)
  (rplaca form (car replacement))
  (rplacd form (cdr replacement)))


(define (quoted-unspecified)
  (list 'quote unspecified))

;;; Gather all symbols used into the program into the interned-symbols
;;; definition

(define (gather-symbols-from-quoted-form quoted cell)
  (cond ((pair? quoted)
         (gather-symbols-from-quoted-form (car quoted) cell)
         (gather-symbols-from-quoted-form (cdr quoted) cell))
        ((symbol? quoted)
         (rplaca cell (adjoin quoted (car cell))))))
         
(define-trivial-walker gather-symbols-aux (cell))

(define-gather-symbols-aux (quote quoted)
  (gather-symbols-from-quoted-form quoted cell))

(define (gather-symbols form)
  (let* ((cell (cons () ())))
    (gather-symbols-aux form cell)
    (quasiquote (begin ()
                       (define interned-symbols (quote (unquote (car cell))))
                       (unquote form)))))

;;; Remove the definitions forms, splicing them into the surrounding
;;; begin/lambda

(define-walker eliminate-definitions ())

(define (eliminate-definitions-forms forms)
  (flatten*-mapfor (form forms) (eliminate-definitions form)))

(define (eliminate-definitions-recurse form)
  (list (list* (first form) (second form)
               (eliminate-definitions-forms (cddr form)))))

(define-eliminate-definitions (definitions attrs . body)
  (eliminate-definitions-forms body))

;;; Replace empty bodies with unspecified

(define-trivial-walker replace-empty-bodies ())

(define-replace-empty-bodies (begin attrs . body)
  (if (null? body)
      (overwrite-form form (quoted-unspecified))
      (replace-empty-bodies-forms body)))

(define-replace-empty-bodies (lambda attrs . body)
  (if (null? body)
      (rplacd (cdr form) (list (quoted-unspecified)))
      (replace-empty-bodies-forms body)))

;;; Convert lambda forms to attributed forms, and convert parameter
;;; names to trivial varrecs
;;;
;;; Attributes of lambda after normalize-lambdas:
;;;
;;; params: The parameter list (undotted if it was dotted)
;;;
;;; varags: true for varargs functions
;;;
;;;  E.g.
;;;
;;; (lambda (x y) ...)
;;; => (lambda ((params (x) (y)) (varargs . false)) ...)
;;;
;;; (lambda (x . y) ...)
;;;  => (lambda ((params (x) (y)) (varargs . true)) ...)

(define (normalize-lambda-params params)
  (let* ((dotted false))
    (labels ((undot (params)
               (cond ((pair? params)
                      (rplaca params (cons (car params) ()))
                      (rplacd params (undot (cdr params)))
                      params)
                     ((null? params)
                      ())
                     (true
                      (set! dotted params)
                      ()))))
      (let* ((undotted-params (undot params)))
        (list (cons 'params undotted-params) (cons 'vararg dotted))))))

(define-trivial-walker normalize-lambdas ())

(define-normalize-lambdas (lambda params . body)
  (rplaca (cdr form) (normalize-lambda-params params))
  (normalize-lambdas-recurse form))

;;; Collect definitions in begins.  The second element of a begin form
;;; holds a list of varrecs.  For a lambda enclosing defines, we wrap
;;; the lambda body in a begin, e.g:
;;;
;;; (lambda (...) ... (define x ...) ...)
;;; => (lambda (...) (begin ((x)) ... (define x ...) ...))

(define-trivial-walker collect-defines-aux (cell))

(define-collect-defines-aux (define var . val)
  (rplaca cell (cons (list var) (car cell)))
  (collect-defines-aux-recurse form cell))

(define-collect-defines-aux (begin attrs . body)
  (collect-defines-aux-forms body (cdr form)))

(define-collect-defines-aux (lambda attrs . body)
  (let* ((cell (cons () body)))
    (collect-defines-aux-recurse form cell)
    (unless (null? (car cell))
      (rplacd (cdr form) (list (cons 'begin cell))))))

(define (collect-defines form)
  (let* ((cell (cons () ())))
    (collect-defines-aux form cell)
    (unless (null? (car cell))
      (error "what do you expect me to do with ~S?" (car cell)))))

;;; This phase is for miscellaneous simplifications

(define-trivial-walker simplify ())

(define-simplify (lambda attrs . body)
  ;; Adjust lambdas so that they only take one body form.
  (simplify-recurse form)
  (rplacd (cdr form) (list (wrap-lambda-body attrs body))))

(define-simplify (define varrec . val)
  ;; Normalize defines to always include a value
  (if (null? val)
      (rplacd (cdr form) (list (quoted-unspecified)))
      (simplify (car val))))

;;; We currently conflate character and numbers.  So eliminate
;;; character-related operators:

(define-simplify ((char-code code-char) attrs ch)
  (overwrite-form form ch)
  (simplify-recurse form))

(define-simplify (character? attrs ch)
  (rplaca form 'number?)
  (simplify-recurse form))

;;; Convert all variable names (in defines, refs, set!s) to references
;;; to the relevant varrec

(define-trivial-walker resolve-variables-aux (frames))

(define (resolve-variable var frames)
  (let* ((varrec (lassoc var frames)))
    (when (null? varrec) (error "unbound variable ~S" var))
    varrec))

(define-resolve-variables-aux (ref var)
  (rplaca (cdr form) (resolve-variable var frames)))

(define-resolve-variables-aux (set! var val)
  (rplaca (cdr form) (resolve-variable var frames))
  (resolve-variables-aux val frames))

(define-resolve-variables-aux (define var val)
  (rplaca (cdr form) (resolve-variable var frames))
  (resolve-variables-aux val frames))

(define-resolve-variables-aux (begin varrecs . body)
  (resolve-variables-aux-recurse form (cons varrecs frames)))

(define-resolve-variables-aux (lambda attrs body)
  (resolve-variables-aux body (cons (attr-ref attrs 'params) frames)))

(define (resolve-variables form)
  (resolve-variables-aux form ()))
            
;;; Classify all variables into read-only and potentially read-write
;;;
;;; I (0) - initial state
;;; D (1) - defined
;;; W (2) - defined, and written
;;; E (3) - early access (encountered a potential access before the define)
;;; ! (4) - error
;;;
;;;          I D W E !
;;;
;;; define   D ! ! E !
;;; ref      E D W E !
;;; set!     E W W E !


;;; A "written" boolean attribute is added to each varrec.

(define-trivial-walker classify-variables ())

(define varrec-define-state-table (make-vector-from-list '(1 4 4 3 4)))
(define varrec-ref-state-table    (make-vector-from-list '(3 1 2 3 4)))
(define varrec-set!-state-table   (make-vector-from-list '(3 2 2 3 4)))

(define (update-varrec-state varrec table)
  (varrec-attr-set! varrec 'state
                    (vector-ref table (varrec-attr varrec 'state))))

(define-classify-variables (ref varrec)
  (update-varrec-state varrec varrec-ref-state-table))

(define-classify-variables (set! varrec val)
  (classify-variables val)
  (update-varrec-state varrec varrec-set!-state-table))

(define-classify-variables (define varrec val)
  (labels ((update-define-varrec-state ()
             (let* ((state (varrec-attr varrec 'state)))
               (when (= state 3)
                 (rplaca form 'set!))
               (varrec-attr-set! varrec 'state
                                (vector-ref varrec-define-state-table state)))))
    (classify-variables val)
    (if (eq? 'lambda (car val))
        (begin (update-define-varrec-state)
               (classify-variables val)
               (form-attr-set! val 'self varrec))
        (begin (classify-variables val)
               (update-define-varrec-state)))))

(define (classify-block-variables varrecs init form)
  (dolist (varrec varrecs)
    (varrec-attr-set! varrec 'state init))
  (classify-variables-recurse form)
  (dolist (varrec varrecs)
    (let* ((state (varrec-attr-remove varrec 'state))
           (written false))
      (cond ((= state 2) 
             (set! written true))
            ((= state 3)
             (set! written true)
             (rplacd (cdr form) (cons (list 'define varrec (quoted-unspecified))
                                      (cddr form))))
            ((= state 4)
             (error "bad variable ~S" (car varrec))))
      (varrec-attr-set! varrec 'written written))))

(define-classify-variables (begin varrecs . body)
  (classify-block-variables varrecs 0 form))

(define-classify-variables (lambda attrs body)
  (form-attr-set! form 'self false)
  (classify-block-variables (attr-ref attrs 'params) 1 form))
 
(define (varrec-written? varrec) (varrec-attr varrec 'written))

;;; Make the closure list for each lambda (i.e. variables unbound
;;; within the lambda body).  After this, lambdas look like:
;;;
;;; (lambda (...) ... (fv ...) ...)
;;; => (lambda ((closure (fv (source fv ...) ...)) ...) ... (fv ...) ...)
;;;
;;; Also adds the boxed attribute to origin varrecs.

(define (resolve-closure-var varrec depth closure-cell)
  (let* ((closure-stack (varrec-attr varrec 'closure-stack))
         (depth-local (first closure-stack)))
    (if (= depth (car depth-local))
        (cdr depth-local)
        ;; The (cdddr varrec) here is a hack - it assumes that the
        ;; boxed and closure-stack attrs are on the front of the
        ;; alist, which happens to be true, and skips them
        (let* ((local-varrec (list* (car varrec) (cons 'origin varrec)
                                    (cdddr varrec))))
          (varrec-attr-set! varrec 'closure-stack
                            (acons depth local-varrec closure-stack))
          (varrec-attr-set! varrec 'boxed (varrec-attr varrec 'written))
          (rplaca closure-cell (cons local-varrec (car closure-cell)))
          local-varrec))))

(define-trivial-walker collect-closures-aux (depth closure-cell))

(define-collect-closures-aux (ref varrec)
  (rplaca (cdr form) (resolve-closure-var varrec depth closure-cell)))

(define-collect-closures-aux ((define set!) varrec val)
  (rplaca (cdr form) (resolve-closure-var varrec depth closure-cell))
  (collect-closures-aux val depth closure-cell))

(define (collect-closures-body form varrecs depth closure-cell)
  (dolist (varrec varrecs)
    (varrec-attr-set! varrec 'boxed false)
    (varrec-attr-set! varrec 'closure-stack (acons depth varrec ())))
  (collect-closures-aux-recurse form depth closure-cell)
  (dolist (varrec varrecs)
    (varrec-attr-set! varrec 'origin false)
    (varrec-attr-remove varrec 'closure-stack)))

(define-collect-closures-aux (begin varrecs . body)
  (collect-closures-body form varrecs depth closure-cell))

(define-collect-closures-aux (lambda attrs body)
  (let* ((local-closure-cell (cons () ()))
         (self-closure-cell (cons () ())))
    ;; if we have an unwritten self-varrec, exclude it from the closure
    (let* ((self-varrec (attr-ref attrs 'self)))
      (when (and self-varrec (not (varrec-attr self-varrec 'written)))
        (attr-set! attrs 'self (resolve-closure-var self-varrec (1+ depth)
                                                    self-closure-cell))))

    (collect-closures-body form (attr-ref attrs 'params) (1+ depth)
                           local-closure-cell)
    (form-attr-set! form 'closure (car local-closure-cell))
    (dolist (local-varrec (append (car local-closure-cell)
                                  (car self-closure-cell)))
      (let* ((origin-varrec (varrec-attr local-varrec 'origin)))
        (varrec-attr-set! origin-varrec 'closure-stack 
                          (cdr (varrec-attr origin-varrec 'closure-stack)))
        (varrec-attr-set! local-varrec 'source
                   (resolve-closure-var origin-varrec depth closure-cell))))))

(define (collect-closures form)
  (collect-closures-aux form 0 ()))

;;; Introduce storage boxes
;;;
;;; We need storage boxes for variables that get written to *and* get
;;; stashed into closures (read-only variables can be stored directly
;;; in closures; written variables that don't put in closures can just
;;; live on the stack).  So we rewrite all set!s and refs for such
;;; variables to go via the storage boxes.
;;;
;;; For variables introduced by begin, we also need to allocate the
;;; storage boxes, but these always get initialized to unspecified, so
;;; this is straightforward.  We initialize other written vars while
;;; we are doing this.
;;;
;;; Variables introduced by lambdas are most complicated, since we need
;;; to copy the original parameter value into the storage box.  So we
;;; have to rename the original parameter.  This involves wrapping the
;;; lambda body with a begin form.

(define (varrec-origin-attr varrec attr)
  (varrec-attr (or (varrec-attr varrec 'origin) varrec) attr))

(define (varrec-boxed? varrec)
  (varrec-origin-attr varrec 'boxed))

(define-trivial-walker introduce-boxes ())

(define-introduce-boxes (ref varrec)
  (when (varrec-boxed? varrec)
    (overwrite-form form 
                    (quasiquote (box-ref () (ref (unquote varrec)))))))

(define-introduce-boxes (set! varrec val)
  (when (varrec-boxed? varrec)
    (overwrite-form form (quasiquote (box-set! () (ref (unquote varrec))
                                               (unquote val)))))
  (introduce-boxes val))

(define-introduce-boxes (define varrec val)
  (when (varrec-boxed? varrec)
    (rplaca (cddr form) (list 'make-box () val)))
  (introduce-boxes val))

(define (init-boxed-param-form varrec temprec)
  (quasiquote (define (unquote varrec) (make-box () (ref (unquote temprec))))))

(define-introduce-boxes (lambda attrs body)
  (let* ((box-varrecs ())
         (box-init-forms ()))
    (nmapfor (varrec (attr-ref attrs 'params))
      (if (varrec-boxed? varrec)
          (let* ((temprec (list (gensym))))
            (set! box-varrecs (cons varrec box-varrecs))
            (set! box-init-forms (cons (init-boxed-param-form varrec temprec)
                                       box-init-forms))
            temprec)
          varrec))
    (unless (null? box-varrecs)
      (rplaca (cddr form)
              (quasiquote (begin (unquote box-varrecs)
                                 (unquote-splicing box-init-forms)
                                 (unquote body))))))
  (introduce-boxes body))

;;; Lambda labelling:
;;;
;;; Assign the code label for each lambda, and where possible,
;;; propogate it to the associated variable

(define-trivial-walker lambda-label ())

(define-lambda-label (begin varrecs . body)
  (dolist (varrec varrecs)
    (varrec-attr-set! varrec 'lambda-label false))
  (lambda-label-forms body))

(define-lambda-label (lambda attrs body)
  (form-attr-set! form 'label (gen-label))
  (dolist (varrec (attr-ref attrs 'params))
    (varrec-attr-set! varrec 'lambda-label false))
  (lambda-label body))

(define-lambda-label (set! varrec val)
  (lambda-label val)
  (when (and (eq? 'lambda (car val))
             (not (varrec-written? varrec)))
    (varrec-attr-set! varrec 'lambda-label (form-attr val 'label))))

;;; Produce a comment form

(define (comment-form-forms forms)
    (mapfor (form forms) (comment-form form)))
(define (comment-form-recurse form)
    (list* (car form) (comment-form-forms (cddr form))))
(define-walker comment-form ())

(define-comment-form (lambda attrs body)
  (list 'lambda (attr-ref attrs 'label) 'etc.))

(define-comment-form (begin . rest) '(begin etc.))
(define-comment-form (if attrs test then else)
  (list 'if (comment-form test) 'etc.))

(define-comment-form (ref varrec) (list 'ref (car varrec)))
(define-comment-form ((set! define) varrec val)
  (list (car form) (car varrec) (comment-form val)))

(define-comment-form (quote attrs)
  (let* ((val (attr-ref attrs 'quoted)))
    (list 'quote (if (string? val) (escape-string-literal val) val))))


(define (emit-comment-form out form)
  (unless (eq? 'begin (car form))
    (emit-comment out "~S" (comment-form form))))


;;; Arch-independent code generation

;;; dest-types describe the destination of the result of an
;;; expression, with enough information to determine the number of
;;; registers needs by the code for the expression.

(defconstant dest-type-discard
  ;; the result of the expression is not needed
  'dest-type-discard)
(defconstant dest-type-value
  ;; the result of the expression should end up in a register
  'dest-type-value)
(defconstant dest-type-conditional
  ;; the result of the expression is used to branch
  'dest-type-conditional)

(define (dest-type-discard? dest-type)
  (eq? dest-type dest-type-discard))
(define (dest-type-value? dest-type)
  (eq? dest-type dest-type-value))
(define (dest-type-conditional? dest-type)
  (eq? dest-type dest-type-conditional))

;;; dests fully describe the destination of the result 

(defconstant dest-discard false)

(define (dest-value reg) (cons 'dest-value reg))
(define (dest-value-reg dest) (cdr dest))

(define (dest-conditional tlabel flabel) (list 'dest-conditional tlabel flabel))
(define (dest-conditional-tlabel dest) (second dest))
(define (dest-conditional-flabel dest) (third dest))

(define (dest-discard? dest) (not dest))
(define (dest-value? dest) (and dest (eq? (car dest) 'dest-value)))
(define (dest-conditional? dest) (and dest (eq? (car dest) 'dest-conditional)))

;;; Top-level sections:  Lambdas and quoted forms

(define (codegen-program program)
  (let* ((out (make-asm-output)))
    (codegen-sections program out)
    (emit-program-prologue out)
    (reg-use program dest-type-discard)
    (codegen program dest-discard
             function-in-frame-base function-out-frame-base
             general-registers out)
    (emit-program-epilogue out)))

(define-trivial-walker codegen-sections (out))

(define-codegen-sections (quote quoted)
  (rplaca (cdr form) (list (cons 'value (codegen-quoted quoted out))
                           (cons 'quoted quoted))))

(define-codegen-sections (begin varrecs . body)
  (codegen-sections-recurse form out))

(define (assign-varrec-indices varrecs mode)
  ;; assign slot indices to varrecs
  (let* ((index 0))
    (dolist (varrec varrecs)
      (varrec-attr-set! varrec 'index index)
      (varrec-attr-set! varrec 'mode mode)
      (set! index (1+ index)))))

(define-codegen-sections (lambda attrs body)
  (let* ((self-varrec (attr-ref attrs 'self)))
    ;; generate code for nested lambdas and data for quoted forms
    (codegen-sections body out)

    (emit out ".text")

    (assign-varrec-indices (attr-ref attrs 'closure) 'closure)
    (assign-varrec-indices (attr-ref attrs 'params) 'param)

    (when self-varrec
      (emit-comment out "~S" (car self-varrec))
      (unless (varrec-written? self-varrec)
        (varrec-attr-set! self-varrec 'mode 'self)))

    (emit-label out (attr-ref attrs 'label))
    (emit-function-prologue out)
    (emit-comment-form out body)

    ;; we don't care about the number of registers used, but we still
    ;; need to do the reg-use pass to "prime" forms for codegen pass
    (reg-use body dest-type-value)
    (codegen body (dest-value %funcres)
             function-in-frame-base function-out-frame-base
             general-registers out)

    (emit-function-epilogue out)))

(define (wrap-lambda-body attrs body)
  ;; wrap a lambda body with code required to check that the number of
  ;; arguments matches the number of parameters, or to handle varargs
  (let* ((nparams (length (attr-ref attrs 'params)))
         (vararg (attr-ref attrs 'vararg)))
    (if vararg
        (list* 'begin (list (list vararg))
               (quasiquote
                 (define (unquote vararg)
                   (call () (ref handle-varargs) (quote (unquote nparams))
                         (arg-count ()) (raw-args-address ()))))
               body)
        (quasiquote
          (if () (check-arg-count ((nparams . (unquote nparams))))
              (begin () (unquote-splicing body))
              (call () (ref arity-mismatch)
                    (quote (unquote nparams))
                    (arg-count ())))))))

(define-trivial-walker reg-use (dest-type))
(define-trivial-walker codegen (dest in-frame-base out-frame-base regs out))

;;; Begin

(define-reg-use (begin varrecs . body)
  (labels ((block-reg-use (form forms max-ru)
             (if (null? forms) (max (reg-use form dest-type) max-ru)
                 (block-reg-use (car forms) (cdr forms)
                                (max (reg-use form dest-type-discard)
                                     max-ru)))))
    (block-reg-use (car body) (cdr body) 0)))

(define-codegen (begin varrecs . body)
  (labels ((block-codegen (form forms)
             (emit-comment-form out form)
             (emit-comment out "in-frame-base: ~D" in-frame-base)
             (if (null? forms)
                 (codegen (if (eq? 'define (car form)) (third form) form)
                          dest in-frame-base out-frame-base regs out)
                 (begin
                    (if (eq? 'define (car form))
                        (let* ((varrec (second form)))
                          (varrec-attr-set! varrec 'mode 'local)
                          (varrec-attr-set! varrec 'index in-frame-base)
                          (codegen-define varrec (third form) in-frame-base
                                          regs out)
                          (set! in-frame-base (1+ in-frame-base)))
                        (codegen form dest-discard in-frame-base in-frame-base
                                 regs out))
                    (block-codegen (car forms) (cdr forms))))))
    (block-codegen (car body) (cdr body))))

;;; If

(define-simplify (if attrs test then . else)
  (when (null? else)
    (rplacd (cdddr form) (list (quoted-unspecified))))
  (simplify-recurse form))

(define-reg-use (if attrs test then else)
  (max (reg-use test dest-type-conditional)
       (reg-use then dest-type)
       (reg-use else dest-type)))

(define-codegen (if attrs test then else)
  (let* ((l1 (gen-label))
         (l2 (gen-label))
         (l3 (gen-label)))
    (codegen test (dest-conditional l1 l2) in-frame-base in-frame-base
             regs out)
    (emit-label out l1)
    (emit-comment-form out then)
    (codegen then dest in-frame-base out-frame-base regs out)
    (emit-jump out l3)
    (emit-label out l2)
    (emit-comment-form out else)
    (codegen else dest in-frame-base out-frame-base regs out)
    (emit-label out l3)))

;;; Operator support

(define (operator-args-reg-use form)
  ;; find the number of registers required for evaluation of the
  ;; arguments of an operator
  (let* ((index-count 0)
         (args-ru (mapfor (arg (cddr form))
                    (let* ((index index-count))
                      (set! index-count (1+ index-count))
                      (cons (reg-use arg dest-type-value) index))))
         (trashy-args-ru (filterfor (arg-ru args-ru)
                           (>= (car arg-ru) general-register-count)))
         (non-trashy-args-ru (sort (filterfor (arg-ru args-ru)
                                     (< (car arg-ru) general-register-count))
                                   (lambda (a b) (< (car a) (car b)))))
         (ru 0))
    (unless (null? non-trashy-args-ru)
      ;; some of the non-trashy-args-ru may in fact be trashy once we
      ;; account for the need to hold other args in registers
      (set! ru (caar args-ru))
      (labels ((find-trashy-args (args-ru)
                 (unless (null? (cdr args-ru))
                   (let* ((arg (cadr args-ru)))
                     (set! ru (max (car arg) (1+ ru)))
                     (if (< ru general-register-count)
                         (find-trashy-args (cdr args-ru))
                         (begin
                           (set! trashy-args-ru
                                 (nconc (cdr args-ru) trashy-args-ru))
                           (rplacd args-ru ())))))))
        (find-trashy-args non-trashy-args-ru)))
    
    (rplaca (cdr form) (list* (cons 'trashy-args-ru trashy-args-ru)
                              (cons 'non-trashy-args-ru non-trashy-args-ru)
                              (cadr form)))
    (if (null? trashy-args-ru) ru general-register-count)))

(define (operator-args-codegen form frame-base regs out)
  ;; generate the code to evaluate the arguments of an operator
  (let* ((trashy-args-ru (form-attr form 'trashy-args-ru))
         (non-trashy-args-ru (form-attr form 'non-trashy-args-ru))
         (args (cddr form)))
    (when (> (length args) (length regs))
      (error "more operator arguments than available registers in ~S"
             (comment-form form)))

    (unless (null? trashy-args-ru)
      ;; First, the trashy args.  We save the results of all but one
      ;; of these on the stack.  The result of the last one can reside
      ;; in a register, so we reclassify it as non-trashy.
      (set! non-trashy-args-ru
            (nconc non-trashy-args-ru (list (car trashy-args-ru))))
      (set! trashy-args-ru (cdr trashy-args-ru))

      (dolist (arg-ru trashy-args-ru)
        (codegen (elt args (cdr arg-ru)) (dest-value (first regs))
                 frame-base frame-base regs out)
        (emit-frame-push out frame-base (first regs))))
    
    (let* ((non-trashy-regs (copy-list regs))
           (result-regs ()))
      ;; work out the register list for the non-trashy args.  this is
      ;; derived from the target registers of each arg.  Note that
      ;; operator-args-reg-use left the non-trashy-arg list in reverse
      ;; order of evaluation
      (dolist (arg-ru non-trashy-args-ru)
        (let* ((c (nthcdr (cdr arg-ru) non-trashy-regs)))
          (set! result-regs (cons (car c) result-regs))
          (rplaca c false)))

      (set! non-trashy-regs (nconc result-regs (delete false non-trashy-regs)))
      
      ;; now generate code for non-trashy args, reducing the registers
      ;; available for each one
      (dolist (arg-ru (reverse non-trashy-args-ru))
        (codegen (elt args (cdr arg-ru)) (dest-value (first non-trashy-regs))
                 frame-base frame-base non-trashy-regs out)
        (set! non-trashy-regs (cdr non-trashy-regs))))
  
    ;; reload trashy arg results from stack
    (dolist (arg-ru (reverse trashy-args-ru))
      (emit-frame-pop out frame-base (elt regs (cdr arg-ru))))))

(defmarco (define-operator template outreg supplemental-regs . body)
  ;; define how to generate code for an operator
  (let* ((body-ru (max (+ (length (cdr template)) (length supplemental-regs))))
         (name (car template))
         (params (cdr template)))
    (quasiquote (definitions
      (define-reg-use ((unquote name) attrs (unquote-splicing params))
        (max (operator-args-reg-use form)
             (unquote body-ru)
             (convert-value-reg-use dest-type)))

      (define-codegen ((unquote name) attrs (unquote-splicing params))
        (operator-args-codegen form in-frame-base regs out)
        (bind ((unquote-splicing (cdr template))
               (unquote-splicing supplemental-regs) . others) regs
          (let* ((result (destination-reg dest regs)))
            (unquote-splicing body)
            (emit-convert-value out (unquote outreg) dest
                                in-frame-base out-frame-base))))))))

(define (operator-args-reg-use-discarding form)
  ;; find the number of registers required for evaluation of the
  ;; arguments of an operator, when the operator itself has been
  ;; eliminated
  (if (null? (cddr form)) 0
      (let* ((rus (mapfor (arg (cddr form)) (reg-use arg dest-type-discard))))
        (reduce~ (car rus) (cdr rus) (function max)))))

(define (operator-args-codegen-discarding form in-frame-base out-frame-base
                                          regs out)
  ;; generate the code to evaluate the arguments of an operator, when
  ;; the operator itself has been eliminated
  (dolist (arg (cddr form))
    (codegen arg dest-discard in-frame-base in-frame-base regs out))
  (emit-adjust-frame-base out in-frame-base out-frame-base))

(defmarco (define-pure-operator template outreg supplemental-regs . body)
  ;; define how to generate code for an operator that is free of
  ;; side-effects
  (let* ((body-ru (+ (length (cdr template)) (length supplemental-regs)))
         (name (car template))
         (params (cdr template)))
    (quasiquote (definitions
      (define-reg-use ((unquote name) attrs (unquote-splicing params))
        (if (dest-type-discard? dest-type)
            (operator-args-reg-use-discarding form)
            (max (unquote body-ru)
                 (operator-args-reg-use form)
                 (convert-value-reg-use dest-type))))

      (define-codegen ((unquote name) attrs (unquote-splicing params))
        (if (dest-discard? dest)
            (operator-args-codegen-discarding form in-frame-base
                                              out-frame-base regs out)
            (begin
              (operator-args-codegen form in-frame-base regs out)
              (bind ((unquote-splicing (cdr template))
                     (unquote-splicing supplemental-regs) . others) regs
                (let* ((result (destination-reg dest regs)))
                  (unquote-splicing body)
                  (emit-convert-value out (unquote outreg) dest
                                      in-frame-base out-frame-base))))))))))

(defmarco (define-cc-operator template cc supplemental-regs . body)
  ;; define how to generate code for an operator that is free of
  ;; side-effects and produces its result in a condition code
  (let* ((body-ru (+ (length (cdr template)) (length supplemental-regs)))
         (name (car template))
         (params (cdr template)))
    (quasiquote (definitions
      (define-reg-use ((unquote name) attrs (unquote-splicing params))
        (if (dest-type-discard? dest-type)
            (operator-args-reg-use-discarding form)
            (max (+ (if (dest-type-conditional? dest-type) 0 1)
                    (unquote body-ru))
                 (operator-args-reg-use form))))

      (define-codegen ((unquote name) attrs (unquote-splicing params))
        (cond ((dest-discard? dest)
               (operator-args-codegen-discarding form in-frame-base
                                                 out-frame-base regs out))
              ((dest-conditional? dest)
               (operator-args-codegen form in-frame-base regs out)
               
               ;; Adjust the frame base here, since it could mess with the ccs
               ;; later on.  This assumes that cc ops don't depend on %sp
               (emit-adjust-frame-base out in-frame-base out-frame-base)

               (bind ((unquote-splicing (cdr template))
                      (unquote-splicing supplemental-regs) . others)
                     regs
                 (unquote-splicing body)
                 (emit-branch out (unquote cc) dest)))
              ((dest-value? dest)
               (let* ((args-count (length (cddr form)))
                      (args-regs (append (sublist (cdr regs) 0 args-count)
                                         (list (car regs))
                                         (sublist (cdr regs) args-count)))
                      (op-regs (cdr regs)))
                 (operator-args-codegen form in-frame-base args-regs out)
                 (emit-prepare-convert-cc-value out (car regs))
                 (bind ((unquote-splicing (cdr template))
                        (unquote-splicing supplemental-regs) . others)
                       op-regs
                   (unquote-splicing body)
                   (emit-convert-cc-value out (unquote cc) (car regs))
                   (emit-convert-value out (car regs) dest
                                       in-frame-base out-frame-base))))
              (true
               (error "can't handle dest ~S" dest))))))))

;;; Functions

(define-reg-use (lambda attrs body)
  (let* ((closure (attr-ref attrs 'closure)))
    (unless (null? closure) 
      (nmapfor (varrec closure)
        (cons varrec (list 'ref (varrec-attr varrec 'source))))
      (let* ((closure-ref-rus (mapfor (varrec-ref closure)
                                (reg-use (cdr varrec-ref) dest-type-value))))
        (1+ (reduce~ (car closure-ref-rus) (cdr closure-ref-rus)
                     (function max)))))))

(define-codegen (lambda attrs body)
  (let* ((closure (attr-ref attrs 'closure)))
    (emit-alloc-function out (first regs) (attr-ref attrs 'label)
                         (length closure))
    (dolist (varrec-ref (attr-ref attrs 'closure))
      (codegen (cdr varrec-ref) (dest-value (second regs))
               in-frame-base in-frame-base (cdr regs) out)
      (emit-closure-slot-set out (first regs) (car varrec-ref) (second regs)))
    (emit-convert-value out (first regs) dest in-frame-base out-frame-base)))

(define-reg-use (call attrs . args) general-register-count)

(define-codegen (call attrs func . args)
  (let* ((new-frame-base in-frame-base))
    (dolist (arg (reverse args))
      (reg-use arg dest-type-value)
      (codegen arg (dest-value (first general-registers))
               new-frame-base new-frame-base general-registers out)
      (emit-frame-push out new-frame-base (first general-registers)))
    
    (reg-use func dest-type-value)
    (codegen func (dest-value %func)
             new-frame-base new-frame-base general-registers out)
    (emit-mov out (immediate (fixnum-representation (length args)))
              %nargs)

    (let* ((label (and (eq? 'ref (first func))
                       (varrec-origin-attr (second func) 'lambda-label))))
      (if label
          (emit-call out label)
          (emit-indirect-call out)))

    (emit-restore-%func out)
    (emit-convert-value out %funcres dest new-frame-base out-frame-base)))

;;; Strings and vectors

(define (genericize-vec-op form name tag tag-bits scale)
  ;; convert a vector-type-specific operator into a generic vec operator
  (rplaca form name)
  (rplaca (cdr form) (list (cons 'tag tag) (cons 'scale scale)
                           (cons 'tag-bits tag-bits)))
  (simplify-recurse form))

(define (make-vec-copy-form src src-index dst dst-index len tag scale)
  ;; generate code to perform a vector copy
  (let* ((si-name (gensym))
         (di-name (gensym))
         (len-name (gensym))
         (attrs (list (cons 'tag tag) (cons 'scale scale))))
    (quasiquote
      (begin (((unquote si-name)) ((unquote di-name)) ((unquote len-name)))
        (define (unquote si-name) (unquote src-index))
        (define (unquote di-name) (unquote dst-index))
        (define (unquote len-name) (unquote len))
        (if () (> () (ref (unquote si-name)) (ref (unquote di-name)))
            (vec-copy ((forward . (unquote true))
                       (unquote-splicing attrs))
                      (raw-vec-address (unquote attrs) (unquote (copy-tree src))
                                       (ref (unquote si-name)))
                      (raw-vec-address (unquote attrs) (unquote (copy-tree dst))
                                       (ref (unquote di-name)))
                      (ref (unquote len-name)))
            (vec-copy ((forward . (unquote false))
                       (unquote-splicing attrs))
                      (raw-vec-address (unquote attrs) (unquote (copy-tree src))
                                       (+ () (ref (unquote si-name))
                                          (ref (unquote len-name)) (quote -1)))
                      (raw-vec-address (unquote attrs) (unquote (copy-tree dst))
                                       (+ () (ref (unquote di-name))
                                          (ref (unquote len-name)) (quote -1)))
                      (ref (unquote len-name))))
        (quote unspecified)))))

(defmarco (define-vector-type name tag tag-bits scale from-vec-rep to-vec-rep)
  ;; define how to convert from the operators on a specific vector
  ;; type into generic vec operators
  (quasiquote (definitions
    (define-simplify ((unquote (compound-symbol "make-" name)) attrs len)
      (genericize-vec-op form 'make-vec (unquote tag) (unquote tag-bits)
                         (unquote scale)))
    (define-simplify ((unquote (compound-symbol name "-length")) attrs vec)
      (genericize-vec-op form 'vec-length (unquote tag) (unquote tag-bits)
                         (unquote scale)))
    (define-simplify ((unquote (compound-symbol "raw-" name "-address"))
                      attrs vec index)
      (genericize-vec-op form 'raw-vec-address (unquote tag) (unquote tag-bits)
                         (unquote scale)))
    (define-simplify ((unquote (compound-symbol "primitive-" name "-ref"))
                      attrs vec index)
      (genericize-vec-op form 'vec-ref (unquote tag) (unquote tag-bits)
                         (unquote scale))
      (overwrite-form form ((unquote from-vec-rep) (copy-list form))))
    (define-simplify ((unquote (compound-symbol "primitive-" name "-set!"))
                      attrs vec index val)
      (rplaca (cddddr form) ((unquote to-vec-rep) val))
      (genericize-vec-op form 'vec-set! (unquote tag) (unquote tag-bits)
                         (unquote scale)))
    (define-simplify ((unquote (compound-symbol "primitive-" name "-copy"))
                      attrs src src-index dst dst-index len)
      (overwrite-form form (make-vec-copy-form src src-index dst dst-index len
                                               (unquote tag) (unquote scale)))
      (simplify form)))))

(define-vector-type string string-tag string-tag-bits 0
                    (lambda (form) (list 'raw->fixnum () form))
                    (lambda (form) (list 'fixnum->raw () form)))
(define-vector-type vector vector-tag vector-tag-bits value-scale
                    identity identity)
