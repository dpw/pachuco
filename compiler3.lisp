(define (compile3-program program)
  (set! program (list* 'begin (normalize-forms program)))
  (set! program (gather-symbols program))
  (set! program (car (eliminate-definitions program)))
  (replace-empty-bodies program)
  (normalize-lambdas program)
  (collect-defines program)
  (simplify program)
  (resolve-variables program)
  (classify-variables program)
  (eliminate-defines program)
  (collect-closures program)
  (introduce-boxes program)
  (assign-indices program)
  (codegen-program program)
  ;(format t "~S~%" program)
  )

(define keywords-1
  '(begin definitions if

    error-halt
    
    eq? function?
    
    symbol? symbol-name primitive-make-symbol
    
    pair? car cdr cons rplaca rplacd
    
    number? < <= > >= = /= + * - rem truncate 
    
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

(define internal-keywords '(let ref call
                            make-box box-ref box-set!
                            check-arg-count arg-count
                            negate
                            make-vec vec-length raw-vec-address
                            vec-ref vec-set! vec-copy))

(define all-keywords (append keywords-2 internal-keywords keywords-1))

;;; Transform a program so that every form begins with a
;;; distinguishing keyword:
;;;
;;; if, begin, definitions, define, lambda, set!, quote: as in source
;;; language
;;;
;;; builtin: builtin operators
;;;
;;; call: a function call
;;;
;;; ref: a variable reference
;;;
;;; let: variable declarations, derived from defines
;;;
;;; All literals are also quoted during this transformation

(define (normalize form)
  (cond ((pair? form)
           (let* ((keyword (car form)))
             (cond ((member? keyword keywords-2) (normalize-2 form))
                   ((member? keyword keywords-1) (normalize-1 form))
                   (true (list* 'call () (normalize-forms form))))))
        ((symbol? form) (list 'ref form))
        (t (list 'quote form))))

(define (normalize-forms forms)
  (mapfor (form forms) (normalize form)))

(define (normalize-1 form)
  (list* (car form) () (normalize-forms (cdr form))))

(define (normalize-2 form)
  (list* (car form) (cadr form) (normalize-forms (cddr form))))

;;; Improved form walker

;;; define-walker creates:
;;; - The name-alist
;;; - The define-name macro
;;; - A name-form function
;;; - A name-forms function, which combines results

(define (compound-symbol . pieces)
  (intern (string-flatten (mapfor (p pieces)
                            (cond ((string? p) (string-symbolcase p))
                                  ((symbol? p) (symbol-name p))
                                  (t (error "~S" p)))))))

(defmarco (define-walker name implicit-vars . domain-lit)
  (let* ((form-name name)
         (recurse-name (compound-symbol name "-recurse"))
         (forms-name (compound-symbol name "-forms"))
         (define-name (compound-symbol "define-" name))
         (alist-name (compound-symbol name "-alist")))
    (quasiquote (definitions
      (unquote-splicing (if (null? domain-lit) ()
        (let* ((domain (eval (car domain-lit)))
               (unit (first domain))
               (reduce (second domain))
               (replace-2 (third domain)))
          (quasiquote (
      (define ((unquote forms-name) forms (unquote-splicing implicit-vars))
        ((unquote reduce) (form forms)
          ((unquote form-name) form (unquote-splicing implicit-vars))))

      (define ((unquote recurse-name) forms (unquote-splicing implicit-vars))
        ((unquote replace-2) forms ((unquote forms-name) (cddr forms)
                                             (unquote-splicing implicit-vars))))

)))))

         
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
                (acons '(unquote (car template))
                       (lambda (form (unquote-splicing (unquote implicit-vars)))
                         (bind (unquote (cdr template)) (cdr form)
                               (unquote-splicing body)))
                       (unquote (unquote alist-name))))))))))


(define ignore-domain '(ignore-unit dolist ignore-replace-2))
(defmarco (ignore-unit form) (begin))
(defmarco (ignore-replace-2 forms replacement) replacement)

(define append*-domain
  '(list flatten*-mapfor append*-replace-2))
(define (append*-replace-2 forms replacement)
  (rplacd (cdr forms) replacement)
  (list forms))


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
         
(define-walker gather-symbols-aux (cell) ignore-domain)

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

(define-walker eliminate-definitions () append*-domain)

(define-eliminate-definitions (definitions attrs . body)
  body)

;;; Replace empty bodies with unspecified

(define-walker replace-empty-bodies () ignore-domain)

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
                     (t
                      (set! dotted true)
                      (list (cons params ()))))))
      (let* ((undotted-params (undot params)))
        (list (cons 'params undotted-params) (cons 'varargs dotted))))))

(define-walker normalize-lambdas () ignore-domain)

(define-normalize-lambdas (lambda params . body)
  (rplaca (cdr form) (acons 'name false (normalize-lambda-params params)))
  (normalize-lambdas-recurse form))

(define-normalize-lambdas (define var . val)
  (normalize-lambdas-recurse form)
  (when (and val (eq (caar val) 'lambda))
    ;; propogate variable name into the lambda, as a hint for readable
    ;; code generation
    (form-attr-set! (car val) 'name var)))

;;; Collect definitions to introduce let forms.  The second element of
;;; a let form is a list of varrecs.
;;;
;;; For a begin enclosing defines, we simply convert the begin to a
;;; let, e.g.:
;;;
;;; (begin ... (define x ...) ...)
;;; => (let ((x)) ...)
;;;
;;; For a lambda enclosing defines, we wrap the lambda body in a let,
;;; e.g:
;;;
;;; (lambda (...) ... (define x ...) ...)
;;; => (lambda (...) (let ((x)) ... (define x ...) ...))

(define-walker collect-defines-aux (cell) ignore-domain)

(define-collect-defines-aux (define var . val)
  (rplaca cell (cons (list var) (car cell)))
  (collect-defines-aux-recurse form cell))

(define-collect-defines-aux (begin attrs . body)
  (let* ((cell (cons () body)))
    (collect-defines-aux-forms body cell)
    (unless (null? (car cell))
      (rplaca form 'let)
      (rplacd form cell))))

(define-collect-defines-aux (lambda attrs . body)
  (let* ((cell (cons () body)))
    (collect-defines-aux-recurse form cell)
    (unless (null? (car cell))
      (rplacd (cdr form) (list (cons 'let cell))))))

(define (collect-defines form)
  (let* ((cell (cons () ())))
    (collect-defines-aux form cell)
    (unless (null? (car cell))
      (error "what do you expect me to do with ~S?" (car cell)))))

;;; This phase is for miscellaneous simplifications

(define-walker simplify () ignore-domain)

;;; Adjust lambdas and lets so that the only take one body form.

(define-simplify (lambda attrs . body)
  (simplify-forms body)
  (rplacd (cdr form) (list (wrap-lambda-body attrs body))))

(define-simplify (let varrecs . body)
  (simplify-forms body)
  (unless (null? (cdr body))
    (rplacd (cdr form) (list (list* 'begin () body)))))

;;; Convert all variable names (in defines, refs, set!s) to references
;;; to the relevant varrec

(define-walker resolve-variables-aux (frames) ignore-domain)

(define (resolve-variable var frames)
  (let* ((varrec (lassoc var frames)))
    (when (null? varrec) (error "unbound variable ~S" var))
    varrec))

(define-resolve-variables-aux (ref var)
  (rplaca (cdr form) (resolve-variable var frames)))

(define-resolve-variables-aux (set! var val)
  (rplaca (cdr form) (resolve-variable var frames))
  (resolve-variables-aux val frames))

(define-resolve-variables-aux (define var . val)
  (rplaca (cdr form) (resolve-variable var frames))
  (resolve-variables-aux-recurse form frames))

(define-resolve-variables-aux (let varrecs body)
  (resolve-variables-aux-recurse form (cons varrecs frames)))

(define-resolve-variables-aux (lambda attrs body)
  (resolve-variables-aux-recurse form (cons (attr-ref attrs 'params) frames)))

(define (resolve-variables form)
  (resolve-variables-aux form ()))
            
;;; Classify all variables into read-only and potentially read-write
;;;
;;; A "written" boolean attribute is added to each varrec.

(define-walker classify-variables () ignore-domain)

(define (mark-variable varrec use)
  (varrec-attr-set! varrec 'state
                    (if (> use (varrec-attr varrec 'state)) 0 2)))
 
(define-classify-variables (ref varrec)
  (mark-variable varrec 1))

(define-classify-variables (set! varrec val)
  (classify-variables val)
  (mark-variable varrec 0))

(define-classify-variables (define varrec . val)
  (classify-variables-recurse form)
  (mark-variable varrec 2))

(define (classify-block-variables varrecs init form)
  (dolist (varrec varrecs)
    (varrec-attr-set! varrec 'state init))
  (classify-variables-recurse form)
  (dolist (varrec varrecs)
    (varrec-attr-set! varrec 'written
                      (= 2 (varrec-attr-remove varrec 'state)))))

(define-classify-variables (let varrecs body)
  (classify-block-variables varrecs 1 form))

(define-classify-variables (lambda attrs body)
  (classify-block-variables (attr-ref attrs 'params) 0 form))
 
;;; Now definition information is captured in the lets, replace
;;; defines with set!s

(define (varrec-written? varrec) (varrec-attr varrec 'written))

(define-walker eliminate-defines () ignore-domain)

(define-eliminate-defines (define var . val)
  (rplaca form 'set!)
  (when (null? val)
    (if (varrec-written? (second form))
        ;; we clear written vars at the start of the let
        ;; in the introduce-boxes phase, so these defines
        ;; can be eliminated
        (overwrite-form form (quoted-unspecified))
        (rplacd (cdr form) (quoted-unspecified))))
  (eliminate-defines-recurse form))

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

(define-walker collect-closures-aux (depth closure-cell) ignore-domain)

(define-collect-closures-aux (ref varrec)
  (rplaca (cdr form) (resolve-closure-var varrec depth closure-cell)))

(define-collect-closures-aux (set! varrec val)
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

(define-collect-closures-aux (let varrecs body)
  (collect-closures-body form varrecs depth closure-cell))

(define-collect-closures-aux (lambda attrs body)
  (let* ((local-closure-cell (cons () ())))
    (collect-closures-body form (attr-ref attrs 'params)
                           (1+ depth) local-closure-cell)
    (form-attr-set! form 'closure (car local-closure-cell))
    (dolist (local-varrec (car local-closure-cell))
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
;;; stashed into closures (read only variables can be stored directly
;;; in closures).  So we rewrite all set!s and refs for such variables
;;; to go via the storage boxes.
;;;
;;; For variables introduced by let, we also need to allocate the
;;; storage boxes, but these always get initialized to unspecified, so
;;; this is straightforward.  We initialize other written vars while
;;; we are doing this.
;;;
;;; Variables introduced by lambdas are most complicated, since we need
;;; to copy the original parameter value into the storage box.  So we
;;; have to rename the original parameter.  This involves wrapping the
;;; lambda body with a let form.

(define (varrec-boxed? varrec)
  ;; Always look to the origin varrec for the boxed attribute
  (varrec-attr (or (varrec-attr varrec 'origin) varrec) 'boxed))

(define-walker introduce-boxes () ignore-domain)

(define-introduce-boxes (ref varrec)
  (when (varrec-boxed? varrec)
    (overwrite-form form 
                    (quasiquote (box-ref () (ref (unquote varrec)))))))

(define-introduce-boxes (set! varrec val)
  (when (varrec-boxed? varrec)
    (overwrite-form form (quasiquote (box-set! () (ref (unquote varrec))
                                               (unquote val)))))
  (introduce-boxes val))

(define (initialize-var-form varrec)
  (list 'set! varrec
        (if (varrec-boxed? varrec)
            (list 'make-box () (quoted-unspecified))
            (quoted-unspecified))))

(define-introduce-boxes (let varrecs body)
  (introduce-boxes-recurse form)
  (let* ((written-varrecs (filterfor (varrec varrecs) 
                            (varrec-written? varrec))))
    (unless (null? written-varrecs)
      (rplaca (cddr form) (nconc (list 'begin ())
                                 (nmapfor (varrec written-varrecs)
                                   (initialize-var-form varrec))
                                 (list body))))))

(define (init-boxed-param-form varrec temprec)
  (quasiquote (set! (unquote varrec) (make-box () (ref (unquote temprec))))))

(define-introduce-boxes (lambda attrs body)
  (introduce-boxes-recurse form)
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
              (quasiquote (let (unquote box-varrecs)
                               (begin ()
                                 (unquote-splicing box-init-forms)
                                 (unquote body))))))))

;;; Calculate indices for variables

(define (assign-varrec-indices varrecs mode index)
  (dolist (varrec varrecs)
    (varrec-attr-set! varrec 'index index)
    (varrec-attr-set! varrec 'mode mode)
    (set! index (1+ index)))
  index)

(define-walker assign-indices-aux (index) ignore-domain)

(define-assign-indices-aux (let varrecs body)
  (assign-indices-aux-recurse form
                              (assign-varrec-indices varrecs 'local index)))

(define-assign-indices-aux (lambda attrs body)
  (assign-varrec-indices (attr-ref attrs 'closure) 'closure 0)
  (assign-varrec-indices (form-attr form 'params) 'param 0)
  (assign-indices-aux-recurse form 0))

(define (assign-indices form)
  (assign-indices-aux form 0))


;;; Produce a comment form

(define (comment-form-forms forms)
    (mapfor (form forms) (comment-form form)))
(define (comment-form-recurse form)
    (list* (car form) (cadr form) (comment-form-forms (cddr form))))
(define-walker comment-form ())

(define-comment-form (begin . rest) '(begin etc.))
(define-comment-form (lambda attrs body) (list 'lambda attrs 'etc.))
(define-comment-form (let . rest) '(let etc.))
(define-comment-form (if attrs test then else)
  (list 'if (comment-form test) 'etc.))

(define (emit-comment-form out form)
  (unless (eq? 'begin (car form))
    (emit-comment out "~S" (comment-form form))))

;;; code generation

;;; Result types are represented as:
;;; true - value wanted
;;; false - value not wanted
;;; (tlabel . flabel) - conditional

(defconstant dest-type-discard 'dest-type-discard)
(defconstant dest-type-value 'dest-type-value)
(defconstant dest-type-conditional 'dest-type-conditional)

(define (dest-type-discard? dest-type)
  (eq? dest-type 'dest-type-discard))
(define (dest-type-value? dest-type)
  (eq? dest-type 'dest-type-value))
(define (dest-type-conditional? dest-type)
  (eq? dest-type 'dest-type-conditional))


(defconstant dest-discard false)
(define (dest-value reg) reg)
(define (dest-conditional tlabel flabel) (cons tlabel flabel))

(define (dest-discard? dest) (not dest))
(define (dest-value? dest) (function? dest))
(define (dest-conditional? dest) (pair? dest))

;;; Top-level sections:  Lambdas and quoted forms

(define (codegen-program program)
  (let* ((out (make-asm-output)))
    (codegen-sections program out)
    (emit-program-prologue out)
    (reg-use program dest-type-discard)
    (codegen program dest-discard 0 0 general-registers out)
    (emit-program-epilogue out)))

(define-walker codegen-sections (out) ignore-domain)

(define-codegen-sections (quote quoted)
  (rplaca (cdr form) (list (cons 'value (codegen-quoted quoted out))
                           (cons 'quoted quoted))))

(define-codegen-sections (lambda attrs body)
  (codegen-sections body out)
  (form-attr-set! form 'label (codegen-lambda-section attrs body out)))

(define (codegen-lambda-section attrs body out)
  (let ((label (gen-label))
        (arity-mismatch-label (gen-label)))
    (emit out ".text")
    (let ((name (attr-ref attrs 'name)))
      (when name (emit-comment out "~S" name)))
    (emit-label out label)
    (emit-function-prologue out attrs arity-mismatch-label)
    (emit-comment-form out body)
    ;; need to do reg-use pass to "prime" forms for codegen pass
    (reg-use body dest-type-value)
    (codegen body (dest-value %funcres) 0 -1 general-registers out)
    (emit-function-epilogue out attrs arity-mismatch-label)
    label))

(define (wrap-lambda-body attrs body)
  (let* ((nparams (length (attr-ref attrs 'params))))
    (if (attr-ref attrs 'varargs)
        (list* 'begin ()
               (quasiquote
                 (set! (unquote (car (last-elem (attr-ref attrs 'params))))
                       (call () (ref handle-varargs) (quote (unquote nparams))
                             (arg-count ()) (raw-args-address ()))))
               body)
        (quasiquote
          (if () (check-arg-count ((nparams . (unquote nparams))))
              (begin () (unquote-splicing body))
              (call () (ref arity-mismatch)
                    (quote (unquote nparams))
                    (arg-count ())))))))

(define-walker reg-use (dest-type) ignore-domain)
(define-walker codegen (dest in-frame-base out-frame-base regs out)
               ignore-domain)

;;; Begin

(define-reg-use (begin attrs . body)
  (labels ((find-max-subform-ru (max-ru forms)
             (if (null? (cdr forms))
                 (max max-ru (reg-use (car forms) dest-type))
                 (find-max-subform-ru (max max-ru (reg-use (car forms)
                                                           dest-type-discard))
                                      (cdr forms)))))
    (find-max-subform-ru 0 body)))

(define-codegen (begin attrs . body)
  (labels ((codegen-body-forms (forms)
             (emit-comment-form out (car forms))
             (if (null? (cdr forms))
                 (codegen (car forms) dest in-frame-base out-frame-base regs
                          out)
                 (begin (codegen (car forms) dest-discard 
                                 in-frame-base in-frame-base regs out)
                        (codegen-body-forms (cdr forms))))))
    (codegen-body-forms body)))

;;; Let

(define-reg-use (let varrecs body) (reg-use body dest-type))

(define-codegen (let varrecs body)
  (codegen body dest (emit-allocate-locals out (length varrecs) in-frame-base)
           out-frame-base regs out))

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
  (let* ((index-count 0)
         (args-ru (mapfor (arg (cddr form))
                    (let ((index index-count))
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
  (let* ((trashy-args-ru (form-attr form 'trashy-args-ru))
         (non-trashy-args-ru (form-attr form 'non-trashy-args-ru))
         (args (cddr form)))
    (unless (null? trashy-args-ru)
      ;; first, the trashy args.  we need to save the results of these
      ;; on the stack.  but we consider one to be non-trashy, since
      ;; its result can reside in a register
      (set! non-trashy-args-ru (nconc non-trashy-args-ru
                                      (list (car trashy-args-ru))))
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
  (let* ((body-ru (max (+ (length (cdr template)) (length supplemental-regs))))
         (attr-template (list* (car template) 'attrs (cdr template))))
    (quasiquote (definitions
      (define-reg-use (unquote attr-template)
        (max (operator-args-reg-use form)
             (unquote body-ru)
             (convert-value-reg-use dest-type)))

      (define-codegen (unquote attr-template)
        (operator-args-codegen form in-frame-base regs out)
        (bind ((unquote-splicing (cdr template))
               (unquote-splicing supplemental-regs) . others) regs
          (let* ((result (destination-reg dest regs)))
            (unquote-splicing body)
            (emit-convert-value out (unquote outreg) dest
                                in-frame-base out-frame-base))))))))

(define (operator-args-reg-use-discarding form)
  (reduce (function max)
          (mapfor (arg (cddr form)) (reg-use arg dest-type-discard))))

(define (operator-args-codegen-discarding form in-frame-base out-frame-base
                                          regs out)
  (dolist (arg (cddr form))
    (codegen arg dest-discard frame-base frame-base regs out))
  (emit-adjust-frame-base out in-frame-base out-frame-base))

(defmarco (define-pure-operator template outreg supplemental-regs . body)
  (let* ((body-ru (+ (length (cdr template)) (length supplemental-regs)))
         (attr-template (list* (car template) 'attrs (cdr template))))
    (quasiquote (definitions
      (define-reg-use (unquote attr-template)
        (if (dest-type-discard? dest-type)
            (operator-args-reg-use-discarding form)
            (max (unquote body-ru)
                 (operator-args-reg-use form)
                 (convert-value-reg-use dest-type))))

      (define-codegen (unquote attr-template)
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
  (let* ((body-ru (+ (length (cdr template)) (length supplemental-regs)))
         (attr-template (list* (car template) 'attrs (cdr template))))
    (quasiquote (definitions
      (define-reg-use (unquote attr-template)
        (if (dest-type-discard? dest-type)
            (operator-args-reg-use-discarding form)
            (max (+ (if (dest-type-conditional? dest-type) 0 1)
                    (unquote body-ru))
                 (operator-args-reg-use form))))

      (define-codegen (unquote attr-template)
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
                      (args-regs (append (subseq (cdr regs) 0 args-count)
                                         (list (car regs))
                                         (subseq (cdr regs) args-count)))
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
    (nmapfor (varrec closure)
      (cons varrec (list 'ref (varrec-attr varrec 'source))))
    (1+ (reduce (function max)
                (mapfor (varrec-ref closure)
                        (reg-use (cdr varrec-ref) dest-type-value))))))

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

(define-codegen (call attrs . forms)
  (let* ((new-frame-base in-frame-base)
         (args (cdr forms)))
    ;; functions are called with a spare arg slot on the stack, in
    ;; order to support varargs.  The value we save there is not
    ;; significant.
    (emit-frame-push out new-frame-base %func)

    (dolist (arg (reverse args))
      (reg-use arg dest-type-value)
      (codegen arg (dest-value (first general-registers))
               new-frame-base new-frame-base general-registers out)
      (emit-frame-push out new-frame-base (first general-registers)))
    
    (reg-use (first forms) dest-type-value)
    (codegen (first forms) (dest-value %func)
             new-frame-base new-frame-base general-registers out)

    (emit-mov out (immediate (fixnum-representation (length args))) %nargs)
    (emit-call out new-frame-base)
    (emit-convert-value out %funcres dest new-frame-base out-frame-base)))

;;; Strings and vectors

(define (genericize-vec-op form name tag scale)
  (rplaca form name)
  (rplaca (cdr form) (list (cons 'tag tag) (cons 'scale scale)))
  (simplify-recurse form))

(define (make-vec-copy-form src src-index dst dst-index len tag scale)
  (let* ((si-name (gensym))
         (di-name (gensym))
         (len-name (gensym)))
    (quasiquote
      (let (((unquote si-name)) ((unquote di-name)) ((unquote len-name)))
        (define (unquote si-name) (unquote src-index))
        (define (unquote di-name) (unquote dst-index))
        (define (unquote len-name) (unquote len))
        (if () (> () (ref (unquote si-name)) (ref (unquote di-name)))
            (vec-copy ((tag . (unquote tag))
                       (scale . (unquote scale))
                       (forward . (unquote true)))
                      (unquote (copy-tree src)) (ref (unquote si-name))
                      (unquote (copy-tree dst)) (ref (unquote di-name))
                      (ref (unquote len-name)))
            (vec-copy ((tag . (unquote tag))
                       (scale . (unquote scale))
                       (forward . (unquote false)))
                      (unquote (copy-tree src))
                      (+ () (ref (unquote si-name)) (ref (unquote len-name))
                         (quote -1))
                      (unquote (copy-tree dst))
                      (+ () (ref (unquote di-name)) (ref (unquote len-name))
                         (quote -1))
                      (ref (unquote len-name))))))))

(defmarco (define-vector-type name tag scale from-vec-rep to-vec-rep)
  (quasiquote (definitions
    (define-simplify ((unquote (compound-symbol "make-" name)) attrs len)
      (genericize-vec-op form 'make-vec (unquote tag) (unquote scale)))
    (define-simplify ((unquote (compound-symbol name "-length")) attrs vec)
      (genericize-vec-op form 'vec-length (unquote tag) (unquote scale)))
    (define-simplify ((unquote (compound-symbol "raw-" name "-address"))
                      attrs vec index)
      (genericize-vec-op form 'raw-vec-address (unquote tag) (unquote scale)))
    (define-simplify ((unquote (compound-symbol "primitive-" name "-ref"))
                      attrs vec index)
      (genericize-vec-op form 'vec-ref (unquote tag) (unquote scale))
      (overwrite-form form ((unquote from-vec-rep) (copy-list form))))
    (define-simplify ((unquote (compound-symbol "primitive-" name "-set!"))
                      attrs vec index val)
      (rplaca (cddddr form) ((unquote to-vec-rep) val))
      (genericize-vec-op form 'vec-set! (unquote tag) (unquote scale)))
    (define-simplify ((unquote (compound-symbol "primitive-" name "-copy"))
                      attrs src src-index dst dst-index len)
      (overwrite-form form (make-vec-copy-form src src-index dst dst-index len
                                               (unquote tag) (unquote scale)))
      (simplify form)))))

(define-vector-type string string-tag char-scale
                    (lambda (form) (list 'raw->fixnum () form))
                    (lambda (form) (list 'fixnum->raw () form)))
(define-vector-type vector vector-tag value-scale identity identity)
