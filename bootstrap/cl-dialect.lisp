;;; The file provides the definitions required to support the hybrid
;;; dialect under Common Lisp implementations.

(declaim (optimize (debug 2)))
(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
(declaim (sb-ext:muffle-conditions style-warning))

;;; The handling of special values (true, false, unspecified) is
;;; subtle.
;;;
;;; 'true' and 'false' are used throughout the code that can be read
;;; as CL or pachuco, and are symbol macros that resolve to the
;;; appropriate values for the language being executed (i.e. t and nil
;;; in CL, #t and #f in pachuco).
(define-symbol-macro true t)
(define-symbol-macro false nil)

;;; We also need values to represent true, false and unspecified in
;;; the subject language (whether being parsed, compiled or
;;; interpreted).  CL conflates false and nil, so these have to be
;;; distinct from 'true', 'false', etc.  We always denote these
;;; subject-language values by #f, #t and #u in the
;;; compiler/interpreter codebase, because those correspond to the
;;; values produced by the reader (both in CL, via the reader macro
;;; chars below, and in the native pachuco reader).

;;; In CL, we use some distinguished symbols to represent these
;;; subject language values (sl-true, sl-false and sl-unspecified).
;;; Pachuco already has the semantics we want, so we can use its real
;;; true, false and unspecified values (i.e. its #t, #f and #u).

;;; From CL, we want #t, #f, and #u to be self-evaluating, so that the
;;; value returned by the reader when parsing subject language code
;;; match those when used directly.
(define-symbol-macro sl-true 'sl-true)
(define-symbol-macro sl-false 'sl-false)
(define-symbol-macro sl-unspecified 'sl-unspecified)

;; The reader macros, to support #f, #t, #u from CL
(set-dispatch-macro-character #\# #\t #'(lambda (s c n) 'sl-true))
(set-dispatch-macro-character #\# #\f #'(lambda (s c n) 'sl-false))
(set-dispatch-macro-character #\# #\u #'(lambda (s c n) 'sl-unspecified))

(define-symbol-macro subject-language false)

(defun subject-language-boolean (sym)
  (not (eq sym #f)))

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
(defun vector? (a) (and (vectorp a) (not (stringp a))))
(defun error-halt () (error "error-halt"))

;;; This provides the subject language semantics
(defun symbol? (a)
  (and (not (member a '(#t #f #u ()))) (symbolp a)))

(defvar symbol-id-counter 0)

(defun symbol-id (sym)
  (let* ((id (get sym 'id)))
    (or id
        (setf (get sym 'id) (setq symbol-id-counter (1+ symbol-id-counter))))))

(shadow 'map)
(defmacro map (f l) `(mapcar ,f ,l))

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

(shadow 'make-string)
(defun make-string (len init)
  (common-lisp:make-string len :initial-element init))

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

(defun string-equal? (a b) (equalp a b))

;;; Vector primitives

(defun make-vector (len init)
  (make-array len :initial-element init))

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
(defun vector-set-range! (vec index len val)
  (fill vec val :start index :end (+ index len)))

(defun vector-copy (src src-index dest dest-index len)
  (replace dest src :start1 dest-index :end1 (+ dest-index len)
           :start2 src-index))

;;; IO

(defvar raw-stdout *standard-output*)
(defvar raw-stderr *error-output*)
(defvar raw-stdin *standard-input*)
(defvar stdout *standard-output*)
(defvar stderr *error-output*)

(defun raw-write-substring (fd str pos len)
  (write-sequence str fd :start pos :end (+ pos len)))

(defun raw-read-substring (fd str pos len)
  (let ((newpos (read-sequence str fd :start pos :end (+ pos len))))
    (- newpos pos)))

(defun open-file-for-reading (pathname)
  (open pathname))

(defun close-istream (f)
  (close f))

(defun open-file-for-writing (pathname)
  (open pathname :direction :output))

(defun close-ostream (f)
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
                                  (quasiquote-transform innermost (cadr form))))
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

(shadow 'format)
(defmacro format (&rest args)
  `(let* ((*print-pretty* false))
     (common-lisp:format false ,@args)))

(defmacro formout (os &rest args)
  `(let* ((*print-pretty* false))
     (common-lisp:format ,os ,@args)))

(defmacro formout-pretty (os &rest args)
  `(common-lisp:format ,os ,@args))

(shadow 'read)
(defun read (in &rest eos)
  (if eos
      (common-lisp:read in nil (car eos))
      (common-lisp:read in)))

(defmacro sublist (&rest args)
  `(subseq ,@args))

(shadow 'reduce)
(defun reduce (initial l f)
  (common-lisp:reduce f l :initial-value initial))

(defun max$ (init nums)
  (common-lisp:reduce (function max) nums :initial-value init))

(defun min$ (init nums)
  (common-lisp:reduce (function min) nums :initial-value init))

(defun make-vector-from-list (l)
  (make-array (length l) :initial-contents l))

(defun vector-to-list (v)
  (coerce v 'list))

(defun code-character (a) (code-char a))
(defun character-code (a) (char-code a))
(defun character-alphanumeric? (a) (alphanumericp a))

(shadow 'defmacro)
(common-lisp:defmacro defmacro (lhs &rest body)
  `(common-lisp:defmacro ,(car lhs) ,(restify-params (cdr lhs)) ,@body))
