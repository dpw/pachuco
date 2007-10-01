;;; The runtime does not yet support command line arguments.  So this
;;; main function reads what action to perform from stdin, and then
;;; calls into the driver.lisp functions to do it.

(define (main)
  (time
    (define command (read stdin))
    (define files (read stdin))
    (cond ((eq? command 'expand) (do-expand-files files))
          ((eq? command 'interpret) (do-interpret-files files (read stdin)))
          ((eq? command 'compile) (do-compile-files files (read stdin))))))
