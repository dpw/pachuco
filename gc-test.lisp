;;; GC Tests

(defmacro (assert-result expr expect)
  (quasiquote
    (begin
      (define res (unquote expr))
      (define expected (unquote expect))
      (unless (equal? res expected)
        (error "Evaluation of ~S gave ~S, expected ~S" (quote (unquote expr))
               res expected)))))

(defmacro (assert expr)
  (quasiquote
    (begin
      (define res (unquote expr))
      (unless res
        (error "Evaluation of ~S gave false" (quote (unquote expr)))))))

(define (loopy-equal? a b)
  ;; determine if a and b are equal, handling circularities
  (define (le1? a b corr)
    (if (eq? a b) true
      (begin
        (define (le2? c)
          ;; walk the correspondence list c to see if we have met a or
          ;; b already
          (cond ((null? c)
                 ;; no correspondences left, so examine a and b further
                 (define corr2 (acons a b corr))
                 (cond ((pair? a)
                        (and (pair? b)
                             (le1? (car a) (car b) corr2)
                             (le1? (cdr a) (cdr b) corr2)))

                       ((vector? a)
                        (define len (vector-length a))
                        (and (vector? b)
                             (= len (vector-length b))
                             (begin
                               (define (walk-vec i)
                                 (if (< i len)
                                     (and (le1? (vector-ref a i)
                                                (vector-ref b i) corr2)
                                          (walk-vec (1+ i)))
                                     true))
                               (walk-vec 0))))

                       (true
                        (equal? a b))))
                ((eq? a (caar c))
                 (eq? b (cdar c)))
                ((eq? b (cdar c))
                 false)
                (true
                 (le2? (cdr c)))))
        (le2? corr))))
  (le1? a b ()))

(define (gc-tests)
  (define (looped-list . l)
    (rplacd (last-cons l) l)
    l)

  (define (looped-vector)
    (define v (make-vector 3))
    (vector-set! v 0 v)
    (vector-set! v 1 42)
    (vector-set! v 2 v)
    v)

  ;; some tests for loopy-equal
  (assert (loopy-equal? () ()))
  (assert (loopy-equal? (list 'x) (list 'x)))
  (assert (not (loopy-equal? (list 'x) (list 'y))))
  
  (assert (loopy-equal? (looped-list 1) (looped-list 1)))
  (assert (not (loopy-equal? (looped-list 1) (looped-list 2))))

  (assert (loopy-equal? (looped-list 1 2) (looped-list 1 2)))
  (assert (not (loopy-equal? (looped-list 1) (looped-list 1 1))))
  (assert (loopy-equal? (looped-vector) (looped-vector)))

  ;; now the actual gc tests
  (assert-result (gc ()) ())
  (assert-result (gc (list 1 2 3)) '(1 2 3))
  (assert-result (gc (string-concat "A" "B")) "AB")
  (begin
    (define str "test-symbol")
    (define sym (raw-make-symbol str))
    (assert-result (symbol-name (gc sym)) str))
  (assert (loopy-equal? (gc (looped-list 1 2 3)) (looped-list 1 2 3)))
  (assert (loopy-equal? (gc (looped-vector)) (looped-vector))))

(define (main)
  (gc-tests)
  (formout stdout "GC tests done~%"))


