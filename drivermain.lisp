;;; Process command line arguments, and call into the driver.lisp
;;; functions to perform the specified action.

(define (main)
  (define command (second (command-line)))
  (define files (cddr (command-line)))
  (cond ((string-equal? command "expand")
         (do-expand-files files))
        ((string-equal? command "interpret")
         (do-interpret-files files '(main)))
        ((string-equal? command "compile")
         (do-compile-files files '(main)))))
