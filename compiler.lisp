;;; The compiler

(define (compile-program program)
  ;; Compile the program, printing the resulting assembly on standard
  ;; output
  (set! program (normalize-forms program))
  (set! program (gather-symbols-forms program))
  (set! program (eliminate-definitions-forms program))
  (set! program (list* 'begin () program))
  (replace-empty-bodies program)
  (normalize-lambdas program)
  (collect-defines program)
  (simplify program)
  (resolve-variables program)
  (classify-variables program)
  (mark-top-level-definitions program)
  (collect-closures program)
  (introduce-boxes program)
  (decompose-lambdas program false)
  (codegen-program program)
  ;(format~ true "~S~%" (debug-form program))
  )

(define keywords-1
  '(begin definitions if

    error-halt
    
    eq? function?
    
    symbol? symbol-name raw-make-symbol
    
    pair? car cdr cons rplaca rplacd
    
    number? < <= > >= = /= + * - rem truncate 
    
    character? character-code code-character

    string? make-string string-length raw-string-address
    raw-string-ref raw-string-set! raw-string-copy
    
    vector? make-vector vector-length raw-vector-address
    raw-vector-ref raw-vector-set! raw-vector-copy
    raw-vec-set! raw-vec-ref raw-ref raw-set! raw-copy
    raw-1-vec-set! raw-1-vec-ref raw-1-ref raw-1-set! raw-1-copy

    raw-make-box raw-box-ref raw-box-set!

    fixnum->raw raw->fixnum

    raw-logand raw-- raw-+
    
    raw-args-base raw-jump-with-arg-space raw-apply-jump))

(define keywords-2 '(define lambda set! quote
                     c-call raw-c-global raw-label compiler-constant raw-alloc))

(define internal-keywords '(ref call return varargs-return
                            tail-call varargs-tail-call
                            alloc-closure fill-closure
                            check-arg-count arg-count
                            negate
                            make-vec vec-length vec-address
                            copy-mem))

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
  (list* (car form) (copy-tree (cadr form)) (normalize-forms (cddr form))))

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
  (rplaca (cdr form) (attr-set! (second form) attr val))
  val)


(define (varrec-attr varrec attr)
  (attr-ref (cdr varrec) attr))

(define (varrec-attr-set! varrec attr val)
  (rplacd varrec (attr-set! (cdr varrec) attr val))
  val)

(define (varrec-attr-remove varrec attr)
  (attr-remove varrec attr))

(define (overwrite-form form replacement)
  (rplaca form (car replacement))
  (rplacd form (cdr replacement)))


(define (quoted-unspecified)
  (list 'quote unspecified))

;;; Gather all symbols used into the program into the initial-symbols
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

(define (gather-symbols-forms forms)
  (let* ((cell (cons () ())))
    (gather-symbols-aux-forms forms cell)
    (cons (quasiquote (define initial-symbols (quote (unquote (car cell)))))
          forms)))

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
;;; vararg: the vararg variable symbol, or false
;;;
;;;  E.g.
;;;
;;; (lambda (x y) ...)
;;; => (lambda ((params (x) (y)) (vararg . false)) ...)
;;;
;;; (lambda (x . y) ...)
;;;  => (lambda ((params (x)) (vararg . y)) ...)

(define (normalize-lambda-params params)
  (let* ((dotted false))
    (labels ((undot (params)
               (cond ((pair? params)
                      (rplaca params (list (car params)))
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

;;; This phase is for miscellaneous simplifications and source
;;; rearrangements

(define-trivial-walker simplify ())

(define-simplify (lambda attrs . body)
  ;; Adjust lambdas so that they only take one body form, wrapped in
  ;; the argument handling code
  (wrap-lambda-body form)
  (simplify-recurse form))

(define-simplify (define varrec . val)
  ;; Normalize defines to always include a value
  (if (null? val)
      (rplacd (cdr form) (list (quoted-unspecified)))
      (simplify (car val))))

(define-simplify (if attrs test then . else)
  (when (null? else)
    (rplacd (cdddr form) (list (quoted-unspecified))))
  (simplify-recurse form))

(define-walker propogate (operator))

(define (propogate-recurse form operator)
  ;; merge the operator into the form.
  (overwrite-form form (append operator (list (cons (car form) (cdr form))))))

(define-simplify (return attrs body)
  (simplify body)
  (overwrite-form form body)
  (propogate body (list 'return attrs)))

(define-simplify (varargs-return attrs arg-count body)
  (simplify body)
  (overwrite-form form body)
  (propogate body (list 'varargs-return attrs arg-count)))

(define (propogate-recurse-last form rest operator)
  (if (null? rest)
      (propogate form operator)
      (propogate-recurse-last (car rest) (cdr rest) operator)))

(define-propogate (begin attrs . body)
  (propogate-recurse-last (car body) (cdr body) operator))

(define-propogate (if attrs test then else)
  ;; we need to avoid aliasing operator, so one of these needs to copy
  (propogate then operator)
  (propogate else (copy-tree operator)))

(define tail-call-types
  (list (cons 'return 'tail-call)
        (cons 'varargs-return 'varargs-tail-call)))

(define-propogate (call attrs . args)
  (let* ((tc (assoc (first operator) tail-call-types)))
    (if tc
      (overwrite-form form (list* (cdr tc) (append (second operator) attrs)
                                  (append (cddr operator) args)))
      (propogate-recurse form operator))))

;;; We currently conflate character and numbers.  So eliminate
;;; character-related operators:

(define-simplify ((character-code code-character) attrs ch)
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
    (unless varrec (error "unbound variable ~S" var))
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
            
;;; Classify all variables into read-only and (potentially)
;;; read-write, or early function (a read-only variable with a
;;; function value that is potential ready before its definition).
;;;
;;; A state is attached to each varrec:
;;;
;;; I (4) - initial state
;;; r (5) - read before a define
;;; F (3) - an early-use function (reads before its define)
;;; E (2) - an early-use (reads and/or writes before define)
;;; W (1) - written (after definition)
;;; R (0) - read (after definition)
;;; ! (6) - error
;;;
;;;               R W E F I r !
;;;                           
;;; define        ! W E ! R E !
;;; define lambda ! W E ! R F !
;;; ref           R W E F r r !
;;; set!          W W E E E E !
;;;
;;; When we finish traversing a binding form, an "acccess" attribute
;;; is added to each varrec, indicating whether the access style is
;;; read-only, written, or early-function.

(define-trivial-walker classify-variables ())

(define varrec-define-state-table (make-vector-from-list '(6 1 2 6 0 2 6)))
(define varrec-define-lambda-state-table
                                  (make-vector-from-list '(6 1 2 6 0 3 6)))
(define varrec-ref-state-table    (make-vector-from-list '(0 1 2 3 5 5 6)))
(define varrec-set!-state-table   (make-vector-from-list '(1 1 2 2 2 2 6)))
(define varrec-access-table
    (make-vector-from-list '(read-only written written early-function)))

(define (update-varrec-state varrec table)
  (varrec-attr-set! varrec 'state
                    (vector-ref table (varrec-attr varrec 'state))))

(define-classify-variables (ref varrec)
  (update-varrec-state varrec varrec-ref-state-table))

(define-classify-variables (set! varrec val)
  (classify-variables val)
  (update-varrec-state varrec varrec-set!-state-table))

(define-classify-variables (define varrec val)
  (classify-variables val)
  (let* ((lambda? (eq? 'lambda (car val)))
         (state (update-varrec-state varrec (if lambda?
                                                varrec-define-lambda-state-table
                                                varrec-define-state-table))))
    (when (= state 2) (rplaca form 'set!))
    (when lambda? (form-attr-set! val 'self varrec))))

(define (classify-block-variables varrecs init form)
  (dolist (varrec varrecs)
    (varrec-attr-set! varrec 'state init))
  (classify-variables-recurse form)
  (dolist (varrec varrecs)
    (let* ((state (varrec-attr-remove varrec 'state)))
      (cond ((= state 6) (error "bad variable ~S" (car varrec)))
            ((= state 2)
             (rplacd (cdr form) (cons (list 'define varrec (quoted-unspecified))
                                      (cddr form)))))
      (varrec-attr-set! varrec 'access
                        (vector-ref varrec-access-table state)))))

(define-classify-variables (begin varrecs . body)
  (classify-block-variables varrecs 4 form))

(define-classify-variables (lambda attrs body)
  (form-attr-set! form 'self false)
  (classify-block-variables (attr-ref attrs 'params) 0 form))

(define (varrec-written? varrec)
  (eq? 'written (varrec-attr varrec 'access)))
(define (varrec-early-function? varrec)
  (eq? 'early-function (varrec-attr varrec 'access)))

;;; Mark the top-level definitions as such, so we can omit them from closures
;;;
;;; The program should always consist of a begin form at this point

(define-trivial-walker mark-non-top-level-definitions ())

(define (mark-top-level-definitions program)
  (dolist (varrec (second program))
    (varrec-attr-set! varrec 'mode 'top-level))
  (mark-non-top-level-definitions-recurse program))

(define-mark-non-top-level-definitions (begin varrecs . body)
  (dolist (varrec varrecs)
    (varrec-attr-set! varrec 'mode 'local))
  (mark-non-top-level-definitions-forms body))

(define-mark-non-top-level-definitions (lambda attrs body)
  (dolist (varrec (form-attr form 'params))
    (varrec-attr-set! varrec 'mode 'param))
  (mark-non-top-level-definitions body))

(define (varrec-top-level? varrec)
  (eq? 'top-level (varrec-attr varrec 'mode)))

;;; Make the closure list for each lambda (i.e. variables unbound
;;; within the lambda body).  After this, lambdas look like:
;;;
;;; (lambda (...) ... (fv ...) ...)
;;; => (lambda ((closure (fv (source fv ...) ...)) ...) ... (fv ...) ...)
;;;
;;; Also adds the boxed attribute to origin varrecs.

;;; When we reach this phase, all uses of a variable refer to the same
;;; varrec.  This phase determines which uses can access the variable
;;; directly, and which need to go via a closure.  The former get the
;;; "origin" varrec (this has an origin attribute of false).  The
;;; latter get new varrecs, with their origin attribute pointing to
;;; the origin varrec.
;;;
;;; This is implemented by adding a closure stack to each origin
;;; varrec.  The closure stack is an alist from the binding form depth
;;; to the appropriate varrec for uses within that binding form.

(define (resolve-closure-var varrec mode depth closure-cell)
  (let* ((closure-stack (varrec-attr varrec 'closure-stack)))
    (cond ((not closure-stack)
           varrec)
          ((= depth (caar closure-stack))
           (cdar closure-stack))
          (true
           (let* ((local-varrec
                    (list (car varrec)
                          (cons 'origin varrec)
                          (cons 'access (varrec-attr varrec 'access))
                          (cons 'mode mode))))
             (varrec-attr-set! varrec 'closure-stack
                               (acons depth local-varrec closure-stack))
             (varrec-attr-set! varrec 'boxed (varrec-written? varrec))
             (rplaca closure-cell (cons local-varrec (car closure-cell)))
             local-varrec)))))

(define-trivial-walker collect-closures-aux (depth closure-cell))

(define-collect-closures-aux (ref varrec)
  (rplaca (cdr form) (resolve-closure-var varrec 'closure depth closure-cell)))

(define-collect-closures-aux ((define set!) varrec val)
  (rplaca (cdr form) (resolve-closure-var varrec 'closure depth closure-cell))
  (collect-closures-aux val depth closure-cell))

(define (collect-closures-body form varrecs depth closure-cell)
  (dolist (varrec varrecs)
    (varrec-attr-set! varrec 'boxed false)
    (varrec-attr-set! varrec 'origin false)
    (varrec-attr-set! varrec 'closure-stack
                      (if (varrec-top-level? varrec) false
                          (acons depth varrec ()))))
  (collect-closures-aux-recurse form depth closure-cell)
  (dolist (varrec varrecs)
    (varrec-attr-remove varrec 'closure-stack)))

(define-collect-closures-aux (begin varrecs . body)
  (collect-closures-body form varrecs depth closure-cell))

(define-collect-closures-aux (lambda attrs body)
  (let* ((local-closure-cell (cons () ()))
         (self-closure-cell (cons () ())))
    ;; if we have an unwritten self varrec, we don't want it to get
    ;; captured in the closure.  And we need to distinguish it from
    ;; the origin varrec, used to refer to the function outside of the
    ;; function.  So we create a "self-closure" varrec, to refer to
    ;; the function inside the function.
    (let* ((self-varrec (attr-ref attrs 'self)))
      (when (and self-varrec (not (varrec-written? self-varrec)))
        (resolve-closure-var self-varrec 'self (1+ depth) self-closure-cell)))

    (collect-closures-body form (attr-ref attrs 'params) (1+ depth)
                           local-closure-cell)
    (form-attr-set! form 'closure (car local-closure-cell))
    (dolist (local-varrec (append (car self-closure-cell)
                                  (car local-closure-cell)))
      (let* ((origin-varrec (varrec-attr local-varrec 'origin)))
        (varrec-attr-set! origin-varrec 'closure-stack 
                          (cdr (varrec-attr origin-varrec 'closure-stack)))
        (varrec-attr-set! local-varrec 'source
            (resolve-closure-var origin-varrec 'closure depth closure-cell))))))

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
;;; storage boxes, but classify-variables ensured that a define always
;;; precedes any other other of a variable, so we simply patch the
;;; define to do the raw-make-box.
;;;
;;; Variables introduced by lambdas are more complicated, since we need
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
                    (quasiquote (raw-box-ref () (ref (unquote varrec)))))))

(define-introduce-boxes (set! varrec val)
  (when (varrec-boxed? varrec)
    (overwrite-form form (quasiquote (raw-box-set! () (ref (unquote varrec))
                                                   (unquote val)))))
  (introduce-boxes val))

(define-introduce-boxes (define varrec val)
  (when (varrec-boxed? varrec)
    (rplaca (cddr form) (list 'raw-make-box () val)))
  (introduce-boxes val))

(define (init-boxed-param-form varrec temprec)
  (quasiquote (define (unquote varrec)
                (raw-make-box () (ref (unquote temprec))))))

(define-introduce-boxes (lambda attrs body)
  ;; Where a parameter should be boxed, we need to replace the
  ;; substitute the original parameter variable with a temporary
  ;; parameter variable with a gensymmed name, and then add code to
  ;; the lambda body to copy the parameter value from the temporary to
  ;; the original parameter variable.
  ;;
  ;; When we add this copying code, we put it at the start of the
  ;; wrapped-begin form that was provided by wrap-lambda-body.  (We
  ;; don't want to put it right at the start of the lambda body
  ;; because that would interfere with the magic of arg-count.)
  (let* ((wrapped-begin (attr-ref attrs 'wrapped-begin)))
    (nmapfor (varrec (attr-ref attrs 'params))
      (if (varrec-boxed? varrec)
          (let* ((temprec (list* (gensym)
                                 (cons 'origin false)
                                 (cons 'boxed false)
                                 '((mode . param)))))
            (varrec-attr-set! varrec 'mode 'local)
            (rplaca (cdr wrapped-begin) (cons varrec (cadr wrapped-begin)))
            ;; we just add a simple define - the introduce-boxes on
            ;; the body below will find it and elaborate it with the
            ;; make-box form.
            (rplacd (cdr wrapped-begin)
                    (cons (list 'define varrec (list 'ref temprec))
                          (cddr wrapped-begin)))
            temprec)
          varrec)))
  (introduce-boxes body))

;;; Decompose lambdas into alloc-closure and fill-closure forms.
;;;
;;; alloc-closure allocate a closure representing a function, and sets
;;; its code pointer.  fill-closure fills the variable reference slots
;;; in a closure.
;;;
;;; The point of this is to support recursive functions (including
;;; mutually recursive functions), without introducing boxes.  If we
;;; have two mutually recursive non-top-level functions, represented
;;; by closures F and G, then F needs to contain a slot pointing to G,
;;; and G needs to contain a slot pointing to F.  Thus we allocate F
;;; and G first, and then fill in their slots.
;;;
;;; The functions that need treating in this way were identified in
;;; the classify-variables phase as early-access functions (with
;;; predicate varrec-early-function?).
;;;
;;; This phase also assigns a label for the code for each function (in
;;; the label attribute of the alloc-closure), and where possible,
;;; propogates it to the associated variable (in the lambda-label
;;; attribute of the varrec).
;;;
;;; So, for example, the effect is similar to a transformation from:
;;;
;;; (begin
;;;   (define g (lambda () (h)))
;;;   (define h (lambda () (g))))
;;;
;;; to
;;;
;;; (begin
;;;   (define g (alloc-closure ((body (h)) (label "g") (closure h))))
;;;   (define h (alloc-closure ((body (g)) (label "h") (closure g))))
;;;   (fill-closure g h)
;;;   (fill-closure h g))

;;; We need a reference to the begin form in which eligible function
;;; definitions occur, in order to hoist the define...alloc-closure to
;;; the start
(define-trivial-walker decompose-lambdas (begin-form))

(define-decompose-lambdas (begin varrecs . body)
  (dolist (varrec varrecs)
    (varrec-attr-set! varrec 'lambda-label false))
  (decompose-lambdas-forms body form))

(define-decompose-lambdas (lambda attrs body)
  (decompose-lambda form (gen-label) begin-form))

(define (make-varrec-label varrec prefix)
  (if (varrec-top-level? varrec)
      (make-label-for (car varrec) prefix)
      (gen-label)))

(define-decompose-lambdas (define varrec val)
  (if (and (eq? 'lambda (car val)) (not (varrec-written? varrec)))
      (let* ((label (make-varrec-label varrec function-label-prefix)))
        (varrec-attr-set! varrec 'lambda-label label)
        (varrec-attr-set! varrec 'no-closure (null? (form-attr val 'closure)))
        (if (varrec-early-function? varrec)
            (begin
              (rplacd (cdr begin-form)
                      (cons (list 'define varrec
                                  (make-alloc-closure val label begin-form))
                            (cddr begin-form)))
              (overwrite-form form (make-fill-closure val (list 'ref varrec))))
            (decompose-lambda val label begin-form)))
      (decompose-lambdas val begin-form)))

(define (decompose-lambda lambda-form label begin-form)
  (overwrite-form lambda-form
                  (make-fill-closure lambda-form
                                     (make-alloc-closure lambda-form label
                                                         begin-form))))

(define (make-alloc-closure lambda-form label begin-form)
  (dolist (varrec (form-attr lambda-form 'params))
    (varrec-attr-set! varrec 'lambda-label false))
  (decompose-lambdas (third lambda-form) begin-form)
  (list 'alloc-closure (list* (cons 'label label)
                              (cons 'body (third lambda-form))
                              (second lambda-form))))

(define (make-fill-closure lambda-form closure-form)
  (let* ((closure (form-attr lambda-form 'closure)))
    (if (null? closure) closure-form
        (list* 'fill-closure () closure-form
               (mapfor (varrec closure)
                 (list 'ref (varrec-attr varrec 'source)))))))

;;; Produce a debug form

(define (debug-forms forms)
  (mapfor (form forms) (debug-form form)))

(define (debug-form-recurse form)
  (list* (car form) (cadr form) (debug-forms (cddr form))))

(define-walker debug-form ())

(define (debug-substitute-varrec varrec)
  (list (car varrec) (cadr varrec)))

(define (debug-uniquify-varrec varrec)
  (rplacd varrec
          (cons (gensym)
                (flatten*-mapfor (attr (cdr varrec))
                  (let* ((key (car attr))
                         (val (cdr attr)))
                    (cond ((not val)
                           ())
                          ((or (eq? key 'source) (eq? key 'origin))
                           (list (cons key (debug-substitute-varrec val))))
                          (true
                           (list attr))))))))

(define (debug-uniquify-varrecs varrecs)
  (mapfor (varrec varrecs) (debug-uniquify-varrec varrec)))

(define-debug-form (begin varrecs . body)
  (list* 'begin (debug-uniquify-varrecs varrecs) (debug-forms body)))

(define (debug-lambda-attrs attrs)
  (flatten*-mapfor (attr attrs)
    (let* ((key (car attr))
           (val (cdr attr)))
      (cond ((not val)
             ())
            ((or (eq? key 'params) (eq? key 'closure))
             (list (cons key (debug-uniquify-varrecs val))))
            ((eq? key 'self)
             (list (cons key (debug-substitute-varrec val))))
            ((eq? key 'body)
             (list (cons key (debug-form val))))
            (true
             (list attr))))))

(define-debug-form (lambda attrs body)
  (list 'lambda (debug-lambda-attrs attrs) (debug-form body)))

(define-debug-form (alloc-closure attrs)
  (list* 'alloc-closure (debug-lambda-attrs attrs)))

(define-debug-form ((define set!) varrec val)
  (list (car form) (debug-substitute-varrec varrec) (debug-form val)))

(define-debug-form (ref varrec)
  (list 'ref (debug-substitute-varrec varrec)))

;;; Produce a comment form

(define (comment-form-recurse form)
  (list* (car form) (mapfor (subform (cddr form)) (comment-form subform))))
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


(define (emit-comment-form cg form)
  (unless (eq? 'begin (car form))
    (emit-comment cg "~S" (comment-form form))))


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

(define-trivial-walker codegen-sections (cg))

(define-codegen-sections (quote quoted)
  (rplaca (cdr form) (list (cons 'value (codegen-quoted quoted cg))
                           (cons 'quoted quoted))))

(define-codegen-sections (begin varrecs . body)
  (dolist (varrec varrecs)
    (when (varrec-top-level? varrec)
      (let* ((label (make-varrec-label varrec variable-label-prefix)))
        (varrec-attr-set! varrec 'label label)
        (codegen-top-level-variable cg (car varrec) label))))
  (codegen-sections-forms body cg))

(define-codegen-sections (define varrec val)
  ;; convert define for top-level variables to set!
  (when (varrec-top-level? varrec) (rplaca form 'set!))
  (codegen-sections val cg))

(define (assign-varrec-indices varrecs)
  ;; assign slot indices to varrecs
  (let* ((index 0))
    (dolist (varrec varrecs)
      (varrec-attr-set! varrec 'index index)
      (set! index (1+ index)))))

(define-codegen-sections (alloc-closure attrs)
  (let* ((self-varrec (attr-ref attrs 'self))
         (closure (attr-ref attrs 'closure))
         (body (attr-ref attrs 'body)))
    ;; generate code for nested lambdas and data for quoted forms
    (codegen-sections body cg)

    (assign-varrec-indices closure)
    (assign-varrec-indices (attr-ref attrs 'params))

    (when self-varrec
      (emit-comment cg "~S" (car self-varrec)))
    
    ;; we don't care about the number of registers used, but we still
    ;; need to do the reg-use pass to "prime" forms for codegen pass
    (reg-use body dest-type-value)
    (codegen-set-frame-base! cg 0)
    (emit-comment-form cg body)
    (codegen-set-have-closure! cg (not (null? closure)))
    (codegen-function (attr-ref attrs 'label) (1+ (length closure)) body cg)))

(define (wrap-lambda-body lambda-form)
  ;; wrap a lambda body with code required to check that the number of
  ;; arguments matches the number of parameters, or to handle varargs
  (let* ((nparams (length (form-attr lambda-form 'params)))
         (vararg (form-attr lambda-form 'vararg))
         (wrapped-begin (list* 'begin () (cddr lambda-form))))
    ;; wrapped-begin is the begin form that surrounds the original
    ;; lambda body.  We stash it in an attribute, because the
    ;; introduce-boxes phase needs to get access to it later.
    (form-attr-set! lambda-form 'wrapped-begin wrapped-begin)
    (rplacd (cdr lambda-form)
      (if vararg
          (quasiquote
            ((begin ((raw-arg-count) ((unquote vararg)))
                    (define raw-arg-count (arg-count ()))
                    (define (unquote vararg) (call () (ref handle-varargs)
                                                   (quote (unquote nparams))
                                                   (ref raw-arg-count)
                                                   (raw-args-base ())))
                    (varargs-return () (ref raw-arg-count)
                                    (unquote wrapped-begin)))))
          (quasiquote
            ((return ((nparams . (unquote nparams)))
                     (if () (check-arg-count ((nparams . (unquote nparams))))
                         (unquote wrapped-begin)
                         (call () (ref arity-mismatch)
                               (quote (unquote nparams))
                               (arg-count ()))))))))))

(define-trivial-walker reg-use (dest-type))
(define-trivial-walker codegen (dest out-frame-base regs cg))

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
             (emit-comment-form cg form)
             (if (null? forms)
                 (codegen (if (eq? 'define (car form)) (third form) form)
                          dest out-frame-base regs cg)
                 (begin
                   (if (eq? 'define (car form))
                       (let* ((varrec (second form)))
                         (varrec-attr-set! varrec 'index
                                           (codegen-frame-base cg))
                         (codegen (third form) (dest-value (first regs))
                                  (codegen-frame-base cg) regs cg)
                         (emit-frame-push cg (first regs)))
                       (codegen form dest-discard (codegen-frame-base cg)
                                regs cg))
                   (block-codegen (car forms) (cdr forms))))))
    (block-codegen (car body) (cdr body))))

(define-reg-use (define varrec val) (reg-use val dest-type-value))
(define-codegen (define varrec val) (error "codegen on define"))

;;; If

(define-reg-use (if attrs test then else)
  (max (reg-use test dest-type-conditional)
       (reg-use then dest-type)
       (reg-use else dest-type)))

(define-codegen (if attrs test then else)
  (let* ((l1 (gen-label))
         (l2 (gen-label))
         (l3 (gen-label)))
    (codegen test (dest-conditional l1 l2) (codegen-frame-base cg)
             regs cg)
    (emit-label cg l1)
    (emit-comment-form cg then)
    (with-saved-frame-base cg
      (codegen then dest out-frame-base regs cg))
    (emit-jump cg l3)
    (emit-label cg l2)
    (emit-comment-form cg else)
    (codegen else dest out-frame-base regs cg)
    (emit-label cg l3)))

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

(define (operator-args-codegen form regs cg)
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
                 (codegen-frame-base cg) regs cg)
        (emit-frame-push cg (first regs))))
    
    (let* ((non-trashy-regs (copy-list regs))
           (result-regs ()))
      ;; work cg the register list for the non-trashy args.  this is
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
                 (codegen-frame-base cg) non-trashy-regs cg)
        (set! non-trashy-regs (cdr non-trashy-regs))))
  
    ;; reload trashy arg results from stack
    (dolist (arg-ru (reverse trashy-args-ru))
      (emit-frame-pop cg (elt regs (cdr arg-ru))))))

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
        (operator-args-codegen form regs cg)
        (bind ((unquote-splicing (cdr template))
               (unquote-splicing supplemental-regs) . others) regs
          (let* ((result (destination-reg dest regs)))
            (unquote-splicing body)
            (emit-convert-value cg (unquote outreg) dest out-frame-base))))))))

(define (operator-args-reg-use-discarding form)
  ;; find the number of registers required for evaluation of the
  ;; arguments of an operator, when the operator itself has been
  ;; eliminated
  (if (null? (cddr form)) 0
      (max$ 0 (mapfor (arg (cddr form)) (reg-use arg dest-type-discard)))))

(define (operator-args-codegen-discarding form out-frame-base regs cg)
  ;; generate the code to evaluate the arguments of an operator, when
  ;; the operator itself has been eliminated
  (dolist (arg (cddr form))
    (codegen arg dest-discard (codegen-frame-base cg) regs cg))
  (emit-reset-frame-base cg out-frame-base))

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
            (operator-args-codegen-discarding form out-frame-base regs cg)
            (begin
              (operator-args-codegen form regs cg)
              (bind ((unquote-splicing (cdr template))
                     (unquote-splicing supplemental-regs) . others) regs
                (let* ((result (destination-reg dest regs)))
                  (unquote-splicing body)
                  (emit-convert-value cg (unquote outreg) dest
                                      out-frame-base))))))))))

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
               (operator-args-codegen-discarding form out-frame-base regs cg))
              ((dest-conditional? dest)
               (operator-args-codegen form regs cg)
               
               ;; Adjust the frame base here, since it could mess with the ccs
               ;; later on.  This assumes that cc ops don't depend on %sp
               (emit-reset-frame-base cg out-frame-base)

               (bind ((unquote-splicing (cdr template))
                      (unquote-splicing supplemental-regs) . others)
                     regs
                 (unquote-splicing body)
                 (emit-branch cg (unquote cc) dest)))
              ((dest-value? dest)
               (let* ((args-count (length (cddr form)))
                      (args-regs (append (sublist (cdr regs) 0 args-count)
                                         (list (car regs))
                                         (sublist (cdr regs) args-count)))
                      (op-regs (cdr regs)))
                 (operator-args-codegen form args-regs cg)
                 (emit-prepare-convert-cc-value cg (car regs))
                 (bind ((unquote-splicing (cdr template))
                        (unquote-splicing supplemental-regs) . others)
                       op-regs
                   (unquote-splicing body)
                   (emit-convert-cc-value cg (unquote cc) (car regs))
                   (emit-convert-value cg (car regs) dest out-frame-base))))
              (true
               (error "can't handle dest ~S" dest))))))))

;;; Strings and vectors

(define (genericize-vec-op form name attrs)
  ;; convert a vector-type-specific operator into a generic vec operator
  (rplaca form name)
  (rplaca (cdr form) attrs)
  (simplify-recurse form))

(define (make-vec-copy-form src src-index dest dest-index len attrs)
  (quasiquote
    (copy-mem (unquote attrs)
              (vec-address (unquote attrs) (unquote src) (unquote src-index))
              (vec-address (unquote attrs) (unquote dest) (unquote dest-index))
              (unquote len))))

(defmarco (define-vector-type name tag tag-bits scale from-vec-rep to-vec-rep)
  (let* ((type-attrs (compound-symbol name "-type-attrs")))
    ;; define how to convert from the operators on a specific vector
    ;; type into generic vec operators
    (quasiquote (definitions
      (define (unquote type-attrs)
        (list (cons 'tag (unquote tag)) (cons 'tag-bits (unquote tag-bits))
              (cons 'scale (unquote scale)) (cons 'header-size value-size)))

      (define-simplify ((unquote (compound-symbol "make-" name)) attrs len)
        (genericize-vec-op form 'make-vec (unquote type-attrs)))

      (define-simplify ((unquote (compound-symbol name "-length")) attrs vec)
        (genericize-vec-op form 'vec-length (unquote type-attrs)))

      (define-simplify ((unquote (compound-symbol "raw-" name "-address"))
                        attrs vec index)
        (genericize-vec-op form 'vec-address (unquote type-attrs)))

      (define-simplify ((unquote (compound-symbol "raw-" name "-ref"))
                        attrs vec index)
        (genericize-vec-op form 'raw-vec-ref (unquote type-attrs))
        (overwrite-form form ((unquote from-vec-rep) (copy-list form))))

      (define-simplify ((unquote (compound-symbol "raw-" name "-set!"))
                        attrs vec index val)
        (rplaca (cddddr form) ((unquote to-vec-rep) val))
        (genericize-vec-op form 'raw-vec-set! (unquote type-attrs)))

      (define-simplify ((unquote (compound-symbol "raw-" name "-copy"))
                        attrs src src-index dest dest-index len)
        (overwrite-form form (make-vec-copy-form src src-index dest dest-index
                                                 len (unquote type-attrs)))
        (simplify form))))))

(define-vector-type string string-tag string-tag-bits 0
                    (lambda (form) (list 'raw->fixnum () form))
                    (lambda (form) (list 'fixnum->raw () form)))
(define-vector-type vector vector-tag vector-tag-bits value-scale
                    identity identity)

;;; Raw accesss to memory

(define raw-base-type-attrs 
  (list (cons 'tag 0) (cons 'tag-bits 0) (cons 'header-size 0)))

(defmarco (define-raw-type name scale)
  (let* ((type-attrs (compound-symbol name "-type-attrs")))
    ;; define how to convert from the operators on a specific raw type
    ;; into generic raw operators
    (quasiquote (definitions
      (define (unquote type-attrs)
        (list* (cons 'scale (unquote scale)) raw-base-type-attrs))

      (define-simplify ((unquote (compound-symbol name "-vec-ref"))
                        attrs addr index)
        (rplaca form 'raw-vec-ref)
        (rplaca (cdr form) (unquote type-attrs))
        (simplify-recurse form))

      (define-simplify ((unquote (compound-symbol name "-vec-set!"))
                        attrs addr index val)
        (rplaca form 'raw-vec-set!)
        (rplaca (cdr form) (unquote type-attrs))
        (simplify-recurse form))

      (define-simplify ((unquote (compound-symbol name "-ref"))
                        attrs addr)
        (rplaca form 'raw-ref)
        (rplaca (cdr form) (unquote type-attrs))
        (simplify-recurse form))

      (define-simplify ((unquote (compound-symbol name "-set!"))
                        attrs addr val)
        (rplaca form 'raw-set!)
        (rplaca (cdr form) (unquote type-attrs))
        (simplify-recurse form))

      (define-simplify ((unquote (compound-symbol name "-copy"))
                        attrs src-addr dest-addr len)
        (rplaca form 'copy-mem)
        (rplaca (cdr form) (unquote type-attrs))
        (simplify form))))))

(define-raw-type "raw" value-scale)    ; memory as value-sized words
(define-raw-type "raw-1" 0)            ; memory as bytes

;; Access to compiler constants for GC

(define compiler-constants
  (list (cons 'value-size value-size)
        (cons 'number-tag number-tag)
        (cons 'number-tag-bits number-tag-bits)
        (cons 'special-tag special-tag)
        (cons 'special-tag-bits special-tag-bits)
        (cons 'pair-tag pair-tag)
        (cons 'pair-tag-bits pair-tag-bits)
        (cons 'vector-tag vector-tag)
        (cons 'vector-tag-bits vector-tag-bits)
        (cons 'string-tag string-tag)
        (cons 'string-tag-bits string-tag-bits)
        (cons 'symbol-tag symbol-tag)
        (cons 'symbol-tag-bits symbol-tag-bits)
        (cons 'closure-tag closure-tag)
        (cons 'closure-tag-bits closure-tag-bits)
        (cons 'box-tag box-tag)
        (cons 'box-tag-bits box-tag-bits)))

(define (compiler-constant-value ccsym)
  (let* ((cc (assoc ccsym compiler-constants)))
    (unless cc (error "unknown compiler constant ~S" ccsym))
    (cdr cc)))

(define-simplify (compiler-constant ccsym)
  (overwrite-form form (list 'quote (compiler-constant-value ccsym))))
