(define (main)
  (define files (read stdin))
  (define form (read stdin))
  (do-interpret-files files form))

(define (compile-program prog))
