;;; Interpreter

;;; An interpreter environment is a list of alists, each mapping 

(define interpreter-builtin-forms ())
(define interpreter-builtin-body-forms ())

(defmarco (define-interpreter-builtin-form keyword . body)
  (quasiquote
    (let* ((a (cons (unquote keyword)
                    (lambda (keyword args env) (unquote-splicing body)))))
     (push a interpreter-builtin-forms)
     (push a interpreter-builtin-body-forms))))

(defmarco (define-interpreter-builtin-body-form keyword . body)
  (quasiquote
    (let* ((a (cons (unquote keyword)
                    (lambda (keyword args env) (unquote-splicing body)))))
      (push a interpreter-builtin-body-forms))))

(define (make-initial-interpreter-env) (list ()))

(define (eval-form-aux form env builtin-forms)
  (cond ((pair? form)
         (let* ((keyword (car form))
                (args (cdr form))
                (builtin (and (symbol? keyword) (assoc keyword builtin-forms))))
           (if builtin
               (funcall (cdr builtin) keyword args env)
               (eval-call form env))))
        ((symbol? form)
         (let* ((var (lassoc form env)))
           (if var (cdr var)
               (error "unbound variable ~S" form))))
        (true form)))

(define (eval-form form env)
  (eval-form-aux form env interpreter-builtin-forms))

(define (eval-body-form form env)
  (eval-form-aux form env interpreter-builtin-body-forms))

(define (eval-body forms env)
  (let* ((result unspecified))
    (dolist (form forms)
      (set! result (eval-body-form form env)))
    result))

(define (make-lambda-alist formals actuals)
  (cond ((null? formals)
         (if (null? actuals) ()
             (error "surplus arguments ~S" actuals)))
        ((symbol? formals)
         (acons formals actuals ()))
        (true
         (if (null? actuals) (error "insufficient arguments")
             (acons (car formals) (car actuals)
                    (make-lambda-alist (cdr formals) (cdr actuals)))))))

(define-interpreter-builtin-form 'lambda
  (lambda (actuals)
    (eval-body (cdr args) (cons (make-lambda-alist (car args) actuals) env))))

(define (eval-call form env)
  (let* ((evaled-subforms (mapfor (subform form) (eval-form subform env))))
    (funcall (car evaled-subforms) (cdr evaled-subforms))))

(define-interpreter-builtin-form 'begin
  (eval-body args (cons () env)))

(define-interpreter-builtin-body-form 'definitions
  (eval-body args env))

(define-interpreter-builtin-body-form 'define
  (let* ((var (first args))
         (val (if (null? (cdr args)) unspecified
                  (eval-form (second args) env))))
    (unless (symbol? var) (error "bad variable name ~S" var))
    (when (assoc var (car env)) (error "variable ~S already bound" var))
    (lapush var val env)
    val))

(define-interpreter-builtin-form 'set!
  (let* ((val (eval-form (second args) env))
         (var (lassoc (first args) env)))
    (if var (rplacd var val)
        (error "unbound variable ~S" (first args)))
    val))

(define-interpreter-builtin-form 'if
  (if (subject-language-boolean (eval-form (first args) env))
      (eval-form (second args) env)
      (if (null? (cddr args)) 'unspecified (eval-form (third args) env))))

(define-interpreter-builtin-form 'quote
  (first args))

(defmarco (define-interpreter-builtin-op op . body)
  (quasiquote
    (define-interpreter-builtin-form '(unquote op)
      (let* ((evaled-args (mapfor (arg args) (eval-form arg env))))
        (unquote (cond ((null? body)
                        (quasiquote
                          (apply (function (unquote op)) evaled-args)))
                       ((null? (cdr body))
                        (quasiquote
                          (apply (function (unquote (car body))) evaled-args)))
                       (true
                        (quasiquote
                          (bind (unquote (car body)) evaled-args
                                (unquote-splicing (cdr body)))))))))))

(defmarco (define-interpreter-builtin-boolean-op op)
  (quasiquote
    (define-interpreter-builtin-form '(unquote op)
      (if (apply (function (unquote op))
                 (mapfor (arg args) (eval-form arg env)))
         'true 'false))))

(define-interpreter-builtin-op error-halt)
       
(define-interpreter-builtin-boolean-op eq?)
(define-interpreter-builtin-boolean-op function?)

(define-interpreter-builtin-boolean-op symbol?)
(define-interpreter-builtin-op gensym)
(define-interpreter-builtin-op symbol-name (sym)
  (subject-language-symbol-name sym))
(define-interpreter-builtin-op intern (str)
  (subject-language-intern str))

(define-interpreter-builtin-boolean-op pair?)
(define-interpreter-builtin-op car)
(define-interpreter-builtin-op cdr)
(define-interpreter-builtin-op cons)
(define-interpreter-builtin-op rplaca)
(define-interpreter-builtin-op rplacd)

(define-interpreter-builtin-boolean-op number?)
(define-interpreter-builtin-boolean-op <)
(define-interpreter-builtin-boolean-op <=)
(define-interpreter-builtin-boolean-op >)
(define-interpreter-builtin-boolean-op >=)
(define-interpreter-builtin-boolean-op =)
(define-interpreter-builtin-boolean-op /=)
(define-interpreter-builtin-op +)
(define-interpreter-builtin-op -)
(define-interpreter-builtin-op *)
(define-interpreter-builtin-op rem)
(define-interpreter-builtin-op truncate)

(define-interpreter-builtin-op char-code)
(define-interpreter-builtin-op code-char)

(define-interpreter-builtin-boolean-op string?)
(define-interpreter-builtin-op make-string)
(define-interpreter-builtin-op string-length)
(define-interpreter-builtin-op primitive-string-ref string-ref)
(define-interpreter-builtin-op primitive-string-set! string-set!)
(define-interpreter-builtin-op primitive-string-copy string-copy)

(define-interpreter-builtin-boolean-op vector?)
(define-interpreter-builtin-op make-vector)
(define-interpreter-builtin-op vector-length)
(define-interpreter-builtin-op primitive-vector-ref vector-ref)
(define-interpreter-builtin-op primitive-vector-set! vector-set!)
(define-interpreter-builtin-op primitive-vector-copy vector-copy)

(define-interpreter-builtin-op stdout)
(define-interpreter-builtin-op stderr)
(define-interpreter-builtin-op open-file-for-reading)
(define-interpreter-builtin-op close-file)
(define-interpreter-builtin-op make-file-reader)

(define-interpreter-builtin-op apply (func . args)
  (funcall func (apply (function list*) args)))
