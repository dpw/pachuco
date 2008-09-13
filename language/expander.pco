;;; Macro expansion

(define builtin-expanders ())

;; The macro environment consists of a pair of lalists, the first for
;; normal macro expanders, the second for symbol macro expansions.

(define (make-initial-macro-env)
  (cons (list builtin-expanders) (list ())))

(defmarco (define-form-expander keyword . body)
  (quasiquote
    (push (cons (unquote keyword) (lambda (form keyword args dual-env)
                                    (unquote-splicing body)))
          builtin-expanders)))

(define (add-form-expander macro-env keyword expander)
  (lapush keyword expander (car macro-env)))

(define (add-symbol-expansion macro-env sym expansion)
  (lapush sym expansion (cdr macro-env)))

;; A dual environment consits of a pair of the macro environment and
;; the evaluation environment for evaluation of macros.

(define (expand-form form dual-env)
  (if (pair? form)
      (let* ((keyword (car form))
             (expander (lassoc keyword (caar dual-env))))
        (if expander
            (funcall (cdr expander) form keyword (cdr form) dual-env)
            (mapfor (subform form) (expand-form subform dual-env))))
      (let* ((expansion (lassoc form (cdar dual-env))))
        (if expansion (expand-form (cdr expansion) dual-env) form))))

(define (expand-define keyword args dual-env)
  ;; normalize and expand a let/set!/define/defmacro
  (let* ((lhs (car args)))
    (cond ((pair? lhs)
           (list (car lhs)
                 (list* 'lambda (cdr lhs) (expand-body (cdr args) dual-env))))
          ((symbol? lhs)
           (if (null? (cdr args)) args
               (list lhs (expand-form (second args) dual-env))))
          (true
           (error "strange arguments to ~S: ~S" keyword args)))))

(define (do-defmacro args dual-env)
  (if (pair? (first args))
      (let* ((def (expand-define 'defmacro args dual-env))
             (macro-lambda (eval-form (second def) (cdr dual-env))))
        (add-form-expander (car dual-env) (first def)
                           (lambda (form keyword args dual-env)
                             (expand-form (funcall macro-lambda args)
                                          dual-env))))
      (add-symbol-expansion (car dual-env) (first args) (second args)))
  '(definitions))

(define (expand-body-form form dual-env)
  (set! form (expand-form form dual-env))
  (if (and (pair? form) (eq? 'defmacro (first form)))
      (do-defmacro (cdr form) dual-env)
      form))

(define (expand-body body dual-env)
  (mapfor (form body) (expand-body-form form dual-env)))

(define-form-expander 'quote
  form)

(define-form-expander 'lambda
  (list* keyword (car args) (expand-body (cdr args) dual-env)))

(dolist (k '(begin definitions))
  (define-form-expander k
    (cons keyword (expand-body args dual-env))))

(dolist (k '(define let set!))
  (define-form-expander k
      (cons keyword (expand-define keyword args dual-env))))

(define-form-expander 'defmacro
    ;; defmacros are left unexpanded until do-defmacro deals with them
    form)
