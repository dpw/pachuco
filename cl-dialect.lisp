(declaim (optimize (debug 2)))

;;; Subject-language compatibility

(define-symbol-macro true t)
(define-symbol-macro false nil)
(define-symbol-macro unspecified 'unspecified)

(defun subject-language-boolean (sym)
  (not (eq sym 'false)))

(defun subject-language-symbol-name (sym)
  (string-downcase (symbol-name sym)))

(defun subject-language-intern (str)
  (intern (string-upcase str)))

;; Functions from the interpreter basic env list
(defun null? (a) (null a))
(defun pair? (a) (consp a))
(defun eq? (a b) (eql a b))
(defun function? (a) (functionp a))
(defun number? (a) (numberp a))
(defun string? (a) (stringp a))
(defun vector? (a) (vectorp a))
(defun error-halt (message args) (apply #'error (cons message args)))

;;; This provides the subject language semantics
(defun symbol? (a)
  (if (member a '(false true () unspecified)) false
      (symbolp a)))

;;; mapfor - a bit like list comprehensions
(defmacro mapfor (decl &rest body)
  (let ((var (first decl))
        (l (second decl)))
    `(mapcar #'(lambda (,var) ,@body) ,l)))

(defmacro findfor (decl &rest body)
  (let ((var (first decl))
        (l (second decl)))
    `(member-if #'(lambda (,var) ,@body) ,l)))

(defmacro filterfor ((var l) &rest body)
  `(mapcan #'(lambda (,var) (and (progn ,@body) (list ,var))) ,l))

(defmacro nmapfor ((var l) &rest body)
  (let ((c (gensym)))
  `(mapl #'(lambda (,c) (rplaca ,c (let ((,var (car ,c))) ,@body))) ,l)))

(defmacro comprehend ((var l) test map)
  `(mapcan #'(lambda (,var) (and ,test (list ,map))) ,l))

(defmacro flatten*-mapfor ((var l) &rest body)
  `(mapcan #'(lambda (,var) ,@body) ,l)) 


(defun member? (item l) (member item l))

(defun last-elem (l) (first (last l)))

(defmacro set! (var val)
  `(setq ,var ,val))

(defun substring (str start len)
  (subseq str start (+ start len)))

(defun string-flatten (strs)
  (apply #'concatenate 'string strs))

(defun string-concat (&rest strs)
  (string-flatten strs))

(defun string-symbolcase (str) (string-upcase str))

;;; String primitives

(defun string-length (str)
  (length str))

(defun string-ref (str index)
  (elt str index))

(defun string-set! (str index ch)
  (setf (elt str index) ch))

(defun string-copy (src src-index dest dest-index len)
  (replace dest src :start1 dest-index :end1 (+ dest-index len)
           :start2 src-index))

;;; Vector primitives

(defun make-vector (len)
  (make-array len))

(defun vector-length (vec)
  (length vec))

(defun vector-ref (vec index)
  (elt vec index))

(defun vector-set! (vec index val)
  (setf (elt vec index) val))

(defun vector-copy (src src-index dest dest-index len)
  (replace dest src :start1 dest-index :end1 (+ dest-index len)
           :start2 src-index))

;;; IO

(defun stdout (str offset len)
  (write-sequence str *standard-output* :start offset :end (+ offset len)))
(defun stderr (str offset len)
  (write-sequence str *standard-output* :start offset :end (+ offset len)))

(defun open-file-for-reading (pathname)
  (open pathname))
(defun close-file (f)
  (close f))
(defun make-file-reader (f)
  (lambda (args)
    (destructuring-bind (str offset len) args
      (let ((pos (read-sequence str f :start offset :end (+ offset len))))
        (- pos offset)))))

;; Quasiquote

(defun quasiquote-transform (innermost form)
  (cond ((pair? form)
         (let ((keyword (car form)))
           (cond ((eq? 'unquote keyword)
                  (if innermost (cadr form)
                      (list 'list '(quote quote) (cadr form))))
                 ((eq? 'quasiquote keyword)
                  (quasiquote-transform false
                                      (quasiquote-transform true (cadr form))))
                 ((and (pair? keyword) (eq? 'unquote-splicing (car keyword)))
                  (list 'append (cadr keyword)
                        (quasiquote-transform innermost (cdr form))))
                 (t
                  (list 'cons (quasiquote-transform innermost keyword)
                        (quasiquote-transform innermost (cdr form)))))))
        ((null? form) ())
        ((symbolp form)
         (list 'quote form))
        (t form)))

(defmacro quasiquote (form)
  (quasiquote-transform true form))

;; Other bits and pieces

(defmacro begin (&rest body) `(progn ,@body))

(defmacro set! (var val) `(setq ,var ,val))

(defun restify-params (args)
  (cond ((null args) ())
        ((consp args) (cons (car args) (restify-params (cdr args))))
        ((symbolp args) (list '&rest args))
        (t (error "strange lambda arg ~S" args))))

(defmacro define (lhs &rest body)
  (if (consp lhs)
      `(defun ,(car lhs) ,(restify-params (cdr lhs)) ,@body)
      `(defvar ,lhs ,@body)))

(defmacro definitions (&rest body)
  ;; This isn't very general, but it's good enough for top-level defines
  `(progn ,@body))

(defmacro defmarco (lhs &rest body)
  `(defmacro ,(car lhs) ,(restify-params (cdr lhs)) ,@body))

(defmacro bind (to val &rest body)
  `(destructuring-bind ,(restify-params to) ,val ,@body))

(defun string-replace (str old new)
  (labels ((string-replace-from (str start)
             (let ((pos (search old str :start2 start)))
               (if pos
                   (let ((res (concatenate 'string (subseq str 0 pos) new
                                           (subseq str (+ pos (length old))))))
                     (string-replace-from res (+ pos (length new))))
                   str))))
    (string-replace-from str 0)))
