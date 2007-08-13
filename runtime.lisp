(defmacro (when-compiling . rest)
  (if compiling (cons 'definitions rest) '(definitions)))
(defmacro (when-interpreting . rest)
  (if compiling '(definitions) (cons 'definitions rest)))

;;; The compiler emits references to these.  Put stub definitions in
;;; place early, just in case

(when-compiling
  (define (arity-mismatch arg-frame param-frame-length)
    (error-halt "arity-mismatch" ()))

  (define (handle-varargs arg-frame param-frame-length)
    (error-halt "handle-varargs" ())))

;;; Basics needed by quasiquote

(define (list . l) l)

(defmacro (caar x) (list 'car (list 'car x)))
(defmacro (cadr x) (list 'car (list 'cdr x)))
(defmacro (cdar x) (list 'cdr (list 'car x)))
(defmacro (cddr x) (list 'cdr (list 'cdr x)))
(defmacro (cadar x) (list 'car (list ' cdr(list 'car x))))

(defmacro (first x) (list 'car x))
(defmacro (second x) (list 'car (list 'cdr x)))
(defmacro (third x) (list 'car (list 'cdr (list 'cdr x))))

(define (reduce initial l op)
  (if (null? l) initial
      (reduce (op initial (car l)) (cdr l) op)))

(defmacro (and . args)
  (if (null? args) true
      (if (null? (cdr args)) (car args)
          (reduce (car args) (cdr args) (lambda (a b) (list 'if a b false))))))

(defmacro (cond . clauses)
  (if (null? clauses) '(begin)
      (list 'if (caar clauses) (cons 'begin (cdar clauses))
            (cons 'cond (cdr clauses)))))

;;; Quasiquote and prerequisites

(define (quasiquote-transform innermost form)
  (cond ((pair? form)
         (define keyword (car form))
         (cond ((eq? 'unquote keyword)
                (if innermost (cadr form)
                    (list 'list '(quote quote) (cadr form))))
               ((eq? 'quasiquote keyword)
                (quasiquote-transform false
                                      (quasiquote-transform true (cadr form))))
               ((and (pair? keyword) (eq? 'unquote-splicing (car keyword)))
                (list 'append (cadr keyword)
                      (quasiquote-transform innermost (cdr form))))
               (true
                (list 'cons (quasiquote-transform innermost keyword)
                            (quasiquote-transform innermost (cdr form))))))
        ((symbol? form)
         (list 'quote form))
        (true form)))

(defmacro (quasiquote form)
  (quasiquote-transform true form))

(define (append . lists)
  (if (null? lists) ()
      (append-1 lists)))

(define (append-1 lists)
  ;; Finds the first non-empty list, and hands over to append-2
  (define l1 (car lists))
  (define rest (cdr lists))
  (if (null? rest) l1 ; only one list, so that is the answer
      (if (null? l1) (append-1 rest) ; list empty, move on to the next
          (begin
            (define c (cons (car l1) ()))
            (append-2 c (cdr l1) rest)
            c))))

(define (append-2 c l1 rest)
  ;; Attach elements from the list l1 and list of lists rest onto the cons c
  (if (null? l1)
      (begin
        (define l2 (car rest))
        (define rest2 (cdr rest))
        (if (null? rest2)
          (rplacd c l2) ; only one more list, so stick it on the end
          (append-2 c l2 rest2)))
      (begin
        (define c2 (cons (car l1) ()))
        (rplacd c c2)
        (append-2 c2 (cdr l1) rest))))

;;; Reified builtins

(defmacro (reify form)
  (quasiquote (define (unquote form) (unquote form))))

(reify (error-halt message args))

(reify (eq? a b))
(reify (function? a))

(reify (symbol? a))
(reify (gensym a))
(reify (symbol-name a))

(reify (pair? a))
(reify (null? a))
(reify (car a))
(reify (cdr a))
(reify (cons a b))
(reify (rplaca a b))
(reify (rplacd a b))

(reify (number? a))
(reify (< a b))
(reify (<= a b))
(reify (> a b))
(reify (>= a b))
(reify (= a b))
(reify (/= a b))

(define (+ . args)
  (if (null? args) 0
      (if (null? (cdr args)) (car args)
          (reduce (car args) (cdr args) (lambda (a b) (+ a b))))))

(define (* . args)
  (if (null? args) 1
      (if (null? (cdr args)) (car args)
          (reduce (car args) (cdr args) (lambda (a b) (* a b))))))

(define (- a . args)
  (if (null? args) (- a)
      (reduce a args (lambda (a b) (- a b)))))

(reify (rem a b))
(reify (truncate a b))

(reify (string? a))
(reify (make-string a))
(reify (string-length a))

(reify (vector? a))
(reify (make-vector a))
(reify (vector-length a))

(when-interpreting
  ;; The interpreter defined stdout and stderr as builtins, so we need
  ;; to provide function forms
  (reify (stdout a b c))
  (reify (stderr a b c))
  (reify (intern a)))

;;; Arithmetic

(defmacro (1- n) (quasiquote (- (unquote n) 1)))
(defmacro (1+ n) (quasiquote (+ (unquote n) 1)))

;;; Lists

(define (length l)
  (if (null? l) 0 (1+ (length (cdr l)))))

(define (last-n-conses l n)
  (define (aux l)
    (if (null? l) ()
        (begin
          (define res (aux (cdr l)))
          (if (= n 0) res
              (begin
                (set! n (1- n))
                l)))))
  (aux l))

(define (last-cons l)
  (last-n-conses l 1))

(define (list* l1 . l)
  (if (null? l) l1
    (begin
      (set! l (cons l1 l))
      (define penultimate (last-n-conses l 2))
      (rplacd penultimate (cadr penultimate))
      l)))

(define (copy-list l)
  (if (pair? l) (cons (car l) (copy-list (cdr l))) l))

;;; Booleans

(defmacro (not a) (quasiquote (if (unquote a) false true)))

(defmacro (or . args)
  (if (null? args) false
      (if (null? (cdr args)) (car args)
          (reduce (car args) (cdr args)
                  (lambda (a b)
                    (define tmp (gensym))
                    (quasiquote (begin (define (unquote tmp) (unquote a))
                                       (if (unquote tmp) (unquote tmp)
                                           (unquote b)))))))))

(defmacro (when test . rest)
  (quasiquote (if (unquote test) (begin (unquote-splicing rest)))))

(defmacro (unless test . rest)
  (quasiquote (if (not (unquote test)) (begin (unquote-splicing rest)))))

;;; Control structures

(defmacro (prog1 form . forms)
  (define temp (gensym))
  (quasiquote
    (begin
      (define (unquote temp) (unquote form))
      (unquote-splicing forms)
      (unquote temp))))

(defmacro (while test . rest)
  (define name (gensym))
  (quasiquote (begin
               (define ((unquote name))
                   (when (unquote test)
                     (unquote-splicing rest)
                     ((unquote name))))
               ((unquote name)))))

(defmacro (until test . rest)
  (quasiquote (while (not (unquote test)) (unquote-splicing rest))))

;;; Lists

(defmacro (dolist binding . body)
  (define l (gensym))
  (quasiquote
    (begin
      (define (unquote l) (unquote (second binding)))
      (until (null? (unquote l))
        (begin
          (define (unquote (first binding)) (car (unquote l)))
          (unquote-splicing body))
        (set! (unquote l) (cdr (unquote l)))))))

(defmacro (mapfor binding . body)
  (define bodyf (gensym))
  (define loopf (gensym))
  (define in (gensym))
  (define out (gensym))
  (define tail (gensym))
  (quasiquote
    (begin
      (define (unquote in) (unquote (second binding)))
      (if (null? (unquote in)) ()
          (begin
            (define ((unquote bodyf) (unquote (first binding)))
                (unquote-splicing body))

            (define (unquote out)
                (cons ((unquote bodyf) (car (unquote in))) ()))
            (define (unquote tail) (unquote out))
            (define ((unquote loopf) (unquote in))
              (unless (null? (unquote in))
                (define (unquote out)
                    (cons ((unquote bodyf) (car (unquote in))) ()))
                (rplacd (unquote tail) (unquote out))
                (set! (unquote tail) (unquote out))
                ((unquote loopf) (cdr (unquote in)))))

            ((unquote loopf) (cdr (unquote in)))
            (unquote out))))))

(defmacro (findfor binding . body)
  (define loopf (gensym))
  (define l (gensym))
  (quasiquote
    (begin
      (define ((unquote loopf) (unquote l))
        (if (null? (unquote l)) false
            (begin (define (unquote (car binding)) (car (unquote l)))
                   (if (begin (unquote-splicing body)) (unquote (car binding))
                       ((unquote loopf) (cdr (unquote l)))))))
      ((unquote loopf) (unquote (cadr binding))))))

;;; Symbols

(when-compiling
  (define (intern str)
    (or (findfor (sym interned-symbols)
                 (string-equal? str (symbol-name sym)))
        (begin
          (define sym (primitive-make-symbol str))
          (set! interned-symbols (cons sym interned-symbols))
          sym))))

;;; Strings

(define (check-string-index str index)
  (define str-len (string-length str))
  (unless (and (>= index 0) (< index str-len))
    (error "string index out of bounds (index: ~A, string length: ~A)"
           index str-len)))

(define (check-string-range str offset len)
  (define str-len (string-length str))
  (unless (and (>= offset 0) (<= (+ offset len) str-len))
    (error "string range out of bounds (offset: ~A, length: ~A, string length ~A"
           offset len str-len)))

(when-compiling
  (define (string-ref str index)
    (check-string-index str index)
    (primitive-string-ref str index))

  (define (string-set! str index ch)
    (check-string-index str index)
    (primitive-string-set! str index ch)
    ch)

  (define (string-copy src src-offset dest dest-offset len)
    (check-string-range src src-offset len)
    (check-string-range dest dest-offset len)
    (primitive-string-copy src src-offset dest dest-offset len)))

;;; Vector

(define (check-vector-index vec index)
  (define vec-len (vector-length vec))
  (unless (and (> index 0) (< index vec-len))
    (error "vector index out of bounds (index: ~A, vector length ~A)"
           index vec-len)))

(define (check-vector-range vec offset len)
  (define vec-len (vector-length vec))
  (unless (and (>= offset 0) (<= (+ offset len) vec-len))
    (error "vector range out of bounds (offset: ~A, length: ~A, vector length ~A)"
           offset len vec-len)))

(when-compiling
  (define (vector-ref vec index)
    (check-vector-index vec index)
    (primitive-vector-ref vec index))

  (define (vector-set! vec index val)
    (check-vector-index vec index)
    (primitive-vector-set! vec index val)
    val)

  (define (vector-copy src src-offset dest dest-offset len)
    (check-vector-range src src-offset len)
    (check-vector-range dest dest-offset len)
    (primitive-vector-copy src src-offset dest dest-offset len)))

;;; I/O

(defmacro (define-syscall-read-write name sysname)
  (quasiquote (define ((unquote name) fd str offset len)
                (check-string-range str offset len)
                (raw->fixnum (c-call (unquote sysname) (fixnum->raw fd)
                                     (string-address str offset)
                                     (fixnum->raw len))))))

(when-compiling
  (define-syscall-read-write syscall-read "read")
  (define-syscall-read-write syscall-write "write")

  (define (stdout str offset len)
      (syscall-write 1 str offset len))

  (define (stderr str offset len)
      (syscall-write 2 str offset len)))

;;; Standard library

;;; Lists

(define (assoc key l)
  (cond ((null? l) false)
        ((eq? key (caar l)) (car l))
        (true (assoc key (cdr l)))))

(defmacro (push el place)
  (quasiquote (set! (unquote place) (cons (unquote el) (unquote place)))))

(define (acons key val tail)
  (cons (cons key val) tail))

;;; Dynamic scope

(defmacro (with-value binding . body)
  (define temp (gensym))
  (quasiquote
    (begin
      (define (unquote temp) (unquote (first binding)))
      (set! (unquote (first binding)) (unquote (second binding)))
      (prog1
        (begin (unquote-splicing body))
        (set! (unquote (first binding)) (unquote temp))))))

;;; Strings

(define (substring str offset len)
  (check-string-range str offset len)
  (define newstr (make-string len))
  (string-copy str offset newstr 0 len)
  newstr)

(define (chomp str)
  (define len (string-length str))
  (if (and (> len 0) (eq? #\Newline (string-ref str (1- len))))
      (substring str 0 (1- len))
      str))

(define (string-equal? a b)
  (define len (string-length a))
  (and (= (string-length b) len)
       (begin
         (define pos 0)
         (define (aux)
           ;;; CHARS-ARE-NUMBERS
           (or (= pos len)
               (and (eq? (string-ref a pos) (string-ref b pos))
                    (begin (set! pos (1+ pos))
                           (aux)))))
         (aux))))

;;; Equality

(define (equal? a b)
  (cond ((pair? a)
         (and (pair? b) (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
        ((string? a)
         (and (string? b) (string-equal? a b)))
        (true (eq? a b))))

;;; IO

(define (read-line)
  (defmacro buffer-size 1000)
  (define str (make-string buffer-size))
  (substring str 0 (syscall-read 0 str 0 buffer-size)))

(define (write-substring stream str pos len)
  (stream str pos len))

(define (write-string stream str)
  (stream str 0 (string-length str)))

;;; Printing and formatting

(define print-number-digits "0123456789abcdefghijklmnopqrstuvwxyz")

(define *print-radix* 10)
(define *print-readably* false)

(define (character-string ch)
  (define buf (make-string 1))
  (string-set! buf 0 ch)
  buf)

(defmacro (define-char-printer name ch)
  (quasiquote
    (define (unquote name)
      (begin
        (define str (character-string (unquote ch)))
        (lambda (stream) (write-string stream str))))))

(define-char-printer print-newline #\Newline)
(define-char-printer print-double-quote #\")

(define (print-number stream num)
  (if (= num 0)
      (write-string stream "0")
      (begin
        (unless (and (<= 2 *print-radix*)
                     (<= *print-radix* (string-length print-number-digits)))
          (error "Bad radix ~D" *print-radix*))
        
        (define negated false)
        (when (< num 0)
          (set! negated true)
          (set! num (- num)))
        
        (defmacro buf-size 21)
        (define buf (make-string buf-size))
        (define pos buf-size)
        
        (while (> num 0)
          (set! pos (1- pos))
          (string-set! buf pos (string-ref print-number-digits
                                           (rem num *print-radix*)))
          (set! num (truncate num *print-radix*)))
        
        (when negated
         (set! pos (1- pos))
         (string-set! buf pos #\-))
        
        (write-substring stream buf pos (- buf-size pos)))))

(define (print-string stream str)
  (if *print-readably*
      (begin
        (print-double-quote stream)
        (write-string stream str)
        (print-double-quote stream))
      (write-string stream str)))


(define (print-char stream ch)
  ;; CHARS-ARE-NUMBERS
  (if (number? ch)
      (if *print-readably*
          (begin
            (write-string stream "#\\")
            (write-string (cond ((eq? ch #\Newline) "Newline")
                                    (true (character-string ch)))))
          (write-string (character-string ch)))
      (print stream ch)))

(define (print-symbol stream sym)
  (define str (symbol-name sym))
  (write-substring stream str 0 (string-length str)))

(define special-printed-forms
  '((false . "false")
    (true . "true")
    (unspecified . "unspecified")
    (() . "()")))

(define (print-list stream l)
  (write-string stream "(")
  (print stream (car l))
  (set! l (cdr l))

  (while (pair? l)
    (write-string stream " ")
    (print stream (car l))
    (set! l (cdr l)))

  (unless (null? l)
    (write-string stream " . ")
    (print stream l))

  (write-string stream ")"))

(define (print stream obj)
  (cond ((pair? obj)
         (print-list stream obj))
        ((number? obj)
         (print-number stream obj))
        ((string? obj)
         (print-string stream obj))
        ((symbol? obj)
         (print-symbol stream obj))
        ((function? obj)
         (write-string stream "#<function>"))
        (true
         (define special (assoc obj special-printed-forms))
         (if special
             (write-string stream (cdr special))
             (error "cannot print object")))))

;;; Formatted IO

(define (formout-list stream control args)
  (define pos 0)
  (define write-from 0)
  (define control-len (string-length control))
  (define ch)

  (define (flush)
    (write-substring stream control write-from (- pos write-from)))

  (while (< pos control-len)
    (set! ch (string-ref control pos)) 
    (if (not (eq? ch #\~)) (set! pos (1+ pos))
        (begin
          (flush)
          (set! pos (1+ pos))
          (if (>= pos control-len) (set! write-from pos)
              (begin
                (set! ch (string-ref control pos))
                (set! pos (1+ pos))
                (set! write-from pos)
                (cond ((eq? ch #\~)
                       (set! write-from (1- write-from)))

                      ((eq? ch #\%)
                       (print-newline stream))

                      ((or (eq? ch #\A) (eq? ch #\a))
                       (with-value (*print-readably* false)
                         (print stream (car args)))
                       (set! args (cdr args)))

                      ((or (eq? ch #\S) (eq? ch #\s))
                       (with-value (*print-readably* true)
                         (print stream (car args)))
                       (set! args (cdr args)))

                      ((or (eq? ch #\D) (eq? ch #\d))
                       (with-value (*print-radix* 10)
                         (print stream (car args)))
                       (set! args (cdr args)))

                      ((or (eq? ch #\X) (eq? ch #\x))
                       (with-value (*print-radix* 16)
                         (print stream (car args)))
                       (set! args (cdr args)))

                      ((or (eq? ch #\C) (eq? ch #\c))
                       (print-char stream (car args))
                       (set! args (cdr args)))

                      (true
                       (error "Unknown format character ~C" ch))))))))

  (flush))

(define (formout stream control . args)
  (formout-list stream control args))

(define (format-list control args)
  (define space 100)
  (define buf (make-string space))
  (define pos 0)

  (define (out str offset len)
    (when (< space len)
      (set! space (* 2 (string-length buf)))
      (when (< space len)
        (set! space (* 2 len)))

      (define new-buf (make-string space))
      (string-copy buf 0 new-buf 0 pos)
      (set! buf new-buf))

    (string-copy str offset buf pos len)
    (set! pos (+ pos len))
    (set! space (- space len)))

  (formout-list out control args)
  (substring buf 0 pos))

(define (format control . args)
  (format-list control args))

;;; With formatted IO in place, define error support and handlers

(define (error message . args)
  (formout stderr "~A~%" (format-list message args))
  (error-halt message args))

(when-compiling
  (set! (arity-mismatch arg-frame param-frame-length)
    (error "expected ~S arguments, got ~S" (1- param-frame-length)
           (1- (vector-length arg-frame))))

  (set! (handle-varargs arg-frame param-frame-length)
    (define arg-frame-length (vector-length arg-frame))
    (if (< arg-frame-length param-frame-length)
        (begin
          (when (/= (1- param-frame-length) arg-frame-length)
            (error "expected ~S arguments or more, got ~S"
                   (- param-frame-length 2) (1- arg-frame-length)))
        
          ;; the nil varargs list case
          (define new-arg-frame (make-vector param-frame-length))
          (vector-copy arg-frame 0 new-arg-frame 0 arg-frame-length)
          (vector-set! new-arg-frame arg-frame-length ())
          new-arg-frame)
        (begin
         (define (cons-varargs pos)
           (if (>= pos (vector-length arg-frame)) ()
               (cons (vector-ref arg-frame pos)
                     (cons-varargs (1+ pos)))))
         (define last-param-slot (1- param-frame-length))
         (vector-set! arg-frame last-param-slot (cons-varargs last-param-slot))
         arg-frame)))

  (define (apply func arg1 . args)
    (define (args-length args)
      (if (null? (cdr args)) (length (car args))
          (1+ (args-length (cdr args)))))

    (define (copy-list-to-vector l vec index)
      (unless (null? l)
        (vector-set! vec index (car l))
        (copy-list-to-vector (cdr l) vec (1+ index))))
      
    (define (fill-args-vector args vec index)
      (if (null? (cdr args)) (copy-list-to-vector (car args) vec index)
          (begin
            (vector-set! vec index (car args))
            (fill-args-vector (cdr args) vec (1+ index)))))

    (set! args (cons arg1 args))
    (define args-vec (make-vector (1+ (args-length args))))
    (fill-args-vector args args-vec 1)
    (apply-frame func args-vec)))

;;; CL compatibility

(defmacro (let* bindings . body)
  (quasiquote (begin
               (unquote-splicing (mapfor (binding bindings)
                                         (if (pair? binding)
                                             (cons 'define binding)
                                             (list 'define binding))))
               (unquote-splicing body))))

(defmacro (defmarco template . body)
  (quasiquote (defmacro (unquote template) (unquote-splicing body))))

(defmacro (funcall . form) form)
(defmacro (function f) f)

(defmacro (subject-language-boolean bool) bool)
(defmacro (subject-language-symbol-name sym) (symbol-name sym))
(defmacro (subject-language-intern str) (intern str))

;; Not a full destructuring-bind, obviously
(defmacro (destructuring-bind vars values . body)
  (quasiquote
    (apply (lambda (unquote vars) (unquote-splicing body)) (unquote values))))
