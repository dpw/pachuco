(defun read-file-to-buffer (file buf)
  (with-open-file (s file)
    (do ((form (read s false 'eof) (read s false 'eof)))
        ((eq form 'eof))
      (buffer-add buf form))))

(defun read-file (file)
  (let ((buf (make-buffer)))
    (read-file-to-buffer file buf)
    (buffer-list buf)))

(defun make-initial-dual-env ()
  (let ((eval-env (make-initial-interpreter-env)))
    (eval-body-form '(define compiling false) eval-env)
    (cons (make-initial-macro-env) eval-env)))

(defun expand-files (files target)
  (let ((dual-env (make-initial-dual-env)))
    (dolist (f files)
      (dolist (form (read-file f))
        (eval-form '(set! compiling false) (cdr dual-env))
        (eval-body-form (expand-body-form form dual-env) (cdr dual-env))

        (eval-form '(set! compiling true) (cdr dual-env))
        (funcall target (expand-body-form form dual-env))))
    dual-env))

(defun do-interpret-files (files form)
  (let ((dual-env (expand-files files (lambda (expanded)))))
    (eval-form form (cdr dual-env))
    (quit)))

(defun do-expand-files (files)
  (expand-files files (lambda (expanded) (format t "~S~%" expanded)))
  (quit))

;(defun do-compile-files (files form)
;  (let ((program (make-buffer)))
;    (expand-files files (lambda (expanded) (buffer-add program expanded)))
;    (buffer-add program form)
;    (compile-program (buffer-list program))
;    (quit)))

(defun do-compile3-files (files form)
  (let ((program (make-buffer)))
    (expand-files files (lambda (expanded) (buffer-add program expanded)))
    (buffer-add program form)
    (compile3-program (buffer-list program))
    (quit)))