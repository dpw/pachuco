;;; The file provides the definitions required to support the hybrid
;;; dialect under Common Lisp implementations.

(declaim (optimize (debug 2)))
(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
(declaim (sb-ext:muffle-conditions style-warning))

(define-symbol-macro true t)
(define-symbol-macro false nil)
(define-symbol-macro unspecified 'unspecified)

(defun subject-language-boolean (sym)
  (not (eq sym 'false)))

(defun subject-language-symbol-name (sym)
  (string-downcase (symbol-name sym)))

(defun subject-language-intern (str)
  (intern (string-upcase str)))

(defun null? (a) (null a))
(defun pair? (a) (consp a))
(defun eq? (a b) (eql a b))
(defun function? (a) (functionp a))
(defun number? (a) (numberp a))
(defun character? (a) (characterp a))
(defun string? (a) (stringp a))
(defun vector? (a) (vectorp a))
(defun error-halt (message args) (apply #'error (cons message args)))

;;; This provides the subject language semantics
(defun symbol? (a)
  (if (member a '(false true () unspecified)) false
      (symbolp a)))

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
(defmacro nfilterfor ((var l) &rest body)
  `(filterfor (,var ,l) ,@body))

(defmacro nmapfor ((var l) &rest body)
  (let ((c (gensym)))
  `(mapl #'(lambda (,c) (rplaca ,c (let ((,var (car ,c))) ,@body))) ,l)))

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

(defun string-range-equal? (a astart b bstart len)
  (equalp (make-array len :element-type 'character 
:displaced-to a :displaced-index-offset astart)
          (make-array len :element-type 'character 
:displaced-to b :displaced-index-offset bstart)))

;;; Vector primitives

(defun make-vector (len)
  (make-array len))

(defun vector-length (vec)
  (length vec))

(defun vector-ref (vec index)
  (elt vec index))
(defun raw-vector-ref (vec index)
  (elt vec index))

(defun vector-set! (vec index val)
  (setf (elt vec index) val))
(defun raw-vector-set! (vec index val)
  (setf (elt vec index) val))

(defun vector-copy (src src-index dest dest-index len)
  (replace dest src :start1 dest-index :end1 (+ dest-index len)
           :start2 src-index))

;;; IO

(defvar raw-stdout *standard-output*)
(defvar raw-stderr *error-output*)
(defvar raw-stdin *standard-input*)

(defun raw-write-substring (fd str pos len)
  (write-sequence str fd :start pos :end (+ pos len)))

(defun raw-read-substring (fd str pos len)
  (let ((newpos (read-sequence str fd :start pos :end (+ pos len))))
    (- newpos pos)))

(defun open-file-for-reading (pathname)
  (open pathname))

(defun close-file (f)
  (close f))

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

(defmacro format~ (&rest args)
  `(format ,@args))

(defun read~ (in &rest eos)
  (if eos
      (read in nil (car eos))
      (read in)))

(defmacro with-open-file-for-reading (&rest body)
  `(with-open-file ,@body))

(defmacro sublist (&rest args)
  `(subseq ,@args))

(defun reduce~ (initial l f)
  (reduce f l :initial-value initial))

(defun max$ (init nums)
  (reduce (function max) nums :initial-value init))

(defun min$ (init nums)
  (reduce (function min) nums :initial-value init))

(defun make-vector-from-list (l)
  (make-array (length l) :initial-contents l))

(defun code-character (a) (code-char a))
(defun character-code (a) (char-code a))
(defun character-alphanumeric? (a) (alphanumericp a))
