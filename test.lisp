;;; Test

(defmacro (assert-result expr expect)
  (quasiquote
    (begin
      (define res (unquote expr))
      (define expected (unquote expect))
      (unless (equal? res expected)
        (error (unquote (format "Evaluation of ~S gave ~~S, expected ~~S" expr))
               res expected)))))

(define (main)
  (assert-result (function? ()) false)
  (assert-result (function? (lambda ())) true)

  (begin
    (define x '(1 2))
    
    (assert-result (quasiquote (a b c)) '(a b c))
    (assert-result (quasiquote ((unquote x) b c)) '((1 2) b c))
    (assert-result (quasiquote (a (unquote x) c)) '(a (1 2) c))
    (assert-result (quasiquote (a b (unquote x))) '(a b (1 2)))

    (assert-result (quasiquote ((unquote-splicing x) b c)) '(1 2 b c))
    (assert-result (quasiquote (a (unquote-splicing x) c)) '(a 1 2 c))
    (assert-result (quasiquote (a b (unquote-splicing x))) '(a b 1 2)))

  (assert-result (length '(1 2 3)) 3)
  (assert-result (list 1 2 3) '(1 2 3))

  (assert-result (and) true)
  (assert-result (and true) true)
  (assert-result (and false) false)
  (assert-result (and true false) false)
  (assert-result (and false true) false)
  (assert-result (and true true true) true)

  (assert-result (or) false)
  (assert-result (or true) true)
  (assert-result (or false) false)

  (assert-result (eq? (intern "foo") 'foo) true)
  (assert-result (symbol-name (intern "bar")) "bar")

  (assert-result (1+ 1) 2)
  (assert-result (1- 1) 0)

  (assert-result (mapfor (x '(1 2 3)) (1+ x)) '(2 3 4))
  (assert-result (mapfor (x ()) (error "Whoa!")) ())
  (assert-result (mapfor (x '(a)) x) '(a))

  (assert-result (findfor (x '(1 2 3)) (> x 1)) 2)
  (assert-result (findfor (x ()) (error "Whoa!")) false)
  (assert-result (findfor (x '(1)) (> x 1)) false)

  (begin
    (defmacro (assert-applied-result expr expect)
      (quasiquote (begin
                    (assert-result (unquote expr) (unquote expect))
                    (assert-result (apply (unquote (car expr))
                                          (quote (unquote (cdr expr))))
                                   (unquote expect)))))
    (assert-applied-result (+) 0)
    (assert-applied-result (+ 1) 1)
    (assert-applied-result (+ 1 2) 3)
    (assert-applied-result (+ 1 2 3) 6)

    (assert-applied-result (*) 1)
    (assert-applied-result (* 1) 1)
    (assert-applied-result (* 1 2) 2)
    (assert-applied-result (* 1 2 3) 6)

    (assert-applied-result (- 1) -1)
    (assert-applied-result (- 1 2) -1)
    (assert-applied-result (- 1 2 3) -4))

  (begin
    (define (fac n)
        (if (< n 2) 1
            (* n (fac (- n 1)))))
    (assert-result (fac 6) 720))

  (begin
    (define b (make-buffer))
    (buffer-add b 1)
    (buffer-add b 2)
    (buffer-add b 3)
    (assert-result (buffer-list b) '(1 2 3)))

  ;; Test string ref, string copy
  (assert-result (string-equal? "foo" "foo") true)
  (assert-result (string-equal? "foo" "bar") false)

  (begin
    (define (assert-uniform-format a res)
      (assert-result (format "~S" a) res)
      (assert-result (format "~A" a) res))

    (define (assert-non-uniform-format a sres ares)
      (assert-result (format "~S" a) sres)
      (assert-result (format "~A" a) ares))

    (assert-uniform-format 0 "0")
    (assert-uniform-format 242143 "242143")
    (assert-uniform-format -1 "-1")
    (assert-uniform-format () "()")
    (assert-uniform-format 'foo "foo")
    (assert-uniform-format '(a b) "(a b)")
    (assert-uniform-format '(a . b) "(a . b)")
    (assert-uniform-format (lambda ()) "#<function>")
    
    (assert-non-uniform-format "Hello" "\"Hello\"" "Hello")
    (assert-non-uniform-format '("Hello" "there") "(\"Hello\" \"there\")"
                               "(Hello there)"))

  (assert-result (apply append '((1 2) (3 4)) '((5 6) (7 8)))
                 '((1 2) (3 4) 5 6 7 8))
  )

(define (foo)
  (define dual-env (cons (make-initial-macro-env) (make-initial-interpreter-env)))
  (define expanded (expand-body-form '((lambda (x y) (+ x y)) 1 2) dual-env))
  (define res (eval-body-form expanded (cdr dual-env)))
  (formout stdout ">>> ~S~%" res))

(define (bar x)
  (lambda ()
    (define res x)
    (set! x (1+ x))
    res))

;(define vec (raw-make-vector 5 3 10))
;(raw-vector-set! 5 3 vec 5 "Hello")
;(define vec2 (raw-make-vector 5 3 10))
;(raw-vector-copy 5 3 true vec 0 vec2 0 10)
;(formout stdout "~A~%" (raw-vector-ref 5 3 vec2 5))

;(error "Whoops!")