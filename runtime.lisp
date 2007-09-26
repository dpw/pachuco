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
(defmacro (list . l)
  (define (liszt . l) l)
  (define (aux l)
    (if (eq? () l) ()
        (liszt 'cons (car l) (aux (cdr l)))))
  (aux l))

(defmacro (caar x) (list 'car (list 'car x)))
(defmacro (cadr x) (list 'car (list 'cdr x)))
(defmacro (cdar x) (list 'cdr (list 'car x)))
(defmacro (cddr x) (list 'cdr (list 'cdr x)))
(defmacro (cadar x) (list 'car (list ' cdr (list 'car x))))
(defmacro (cdddr x) (list 'cdr (list ' cdr (list 'cdr x))))
(defmacro (cddddr x) (list 'cdr (list 'cdr (list ' cdr (list 'cdr x)))))

(defmacro (first x) (list 'car x))
(defmacro (second x) (list 'car (list 'cdr x)))
(defmacro (third x) (list 'car (list 'cdr (list 'cdr x))))

(defmacro (null? x) (list 'eq? () x))

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

(define (append . lists)
  (if (null? lists) ()
      (append-1 lists)))

;;; Reified builtins

(defmacro (reify form)
  (quasiquote (define (unquote form) (unquote form))))

(reify (error-halt message args))

(reify (eq? a b))
(reify (function? a))

(reify (symbol? a))
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

(reify (char-code a))
(reify (code-char a))

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
  (reify (stdin-reader a b c))
  (reify (intern a))

  ;; Gensym is an interesting case.  We need it early on for macros.
  ;; So when interpreting, we get it from the host.  When compiling,
  ;; we introduce it much later
  (reify (gensym a)))

;;; Arithmetic

(defmacro (1- n) (quasiquote (- (unquote n) 1)))
(defmacro (1+ n) (quasiquote (+ (unquote n) 1)))

(define (max a . nums)
  (reduce a nums (lambda (a b) (if (> a b) a b))))

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

(define (identity x) x)

;;; Lists

(define (length l)
  (if (null? l) 0 (1+ (length (cdr l)))))

(define (member? item l)
  (if (eq? item (car l)) true
      (member? item (cdr l))))

(define (adjoin item l)
  (if (member? item l) l (cons item l)))

(define (elt l index)
  (if (= 0 index) (car l) (elt (cdr l) (1- index))))

(define (nthcdr index l)
  (if (= 0 index) l (nthcdr (1- index) (cdr l))))

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

(define (last-elem l)
  (car (last-cons l)))

(define (list* l1 . l)
  (if (null? l) l1
    (begin
      (set! l (cons l1 l))
      (define penultimate (last-n-conses l 2))
      (rplacd penultimate (cadr penultimate))
      l)))

(define (copy-list l)
  (if (pair? l) (cons (car l) (copy-list (cdr l))) l))

(define (copy-tree l)
  (if (pair? l) (cons (copy-tree (car l)) (copy-tree (cdr l)))))

(define (flatten* ls)
  (define (find-end l ls)
    (if (null? (cdr l))
        (find-start l ls)
        (find-end (cdr l) ls)))

  (define (find-start l ls)
    (unless (null? ls)
      (if (null? (car ls))
          (find-start l (cdr ls))
          (begin
            (rplacd l (car ls))
            (find-end (car ls) (cdr ls))))))
  
  (if (null? ls) ()
      (if (null? (car ls))
          (flatten* (cdr ls))
          (begin
            (find-end (car ls) (cdr ls))
            (car ls)))))

(define (nconc . ls)
  (flatten* ls))

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
  (quasiquote (begin
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

(defmacro (nmapfor binding . body)
  (define loopf (gensym))
  (define l (gensym))
  (quasiquote (begin
    (define ((unquote loopf) (unquote l))
      (unless (null? (unquote l))
        (define (unquote (first binding)) (car (unquote l)))
        (rplaca (unquote l) (begin (unquote-splicing body)))))
    ((unquote loopf) (unquote (second binding))))))

(defmacro (flatten*-mapfor binding . body)
  (quasiquote (flatten* (mapfor (unquote binding) (unquote-splicing body)))))

(defmacro (findfor binding . body)
  (define loopf (gensym))
  (define l (gensym))
  (quasiquote (begin
    (define ((unquote loopf) (unquote l))
      (if (null? (unquote l)) false
          (begin (define (unquote (first binding)) (car (unquote l)))
                 (if (begin (unquote-splicing body)) (unquote (first binding))
                     ((unquote loopf) (cdr (unquote l)))))))
    ((unquote loopf) (unquote (second binding))))))

(defmacro (filterfor binding . body)
  (quasiquote
    (flatten*-mapfor (unquote binding)
      (if (begin (unquote-splicing body))
          (list (unquote (first binding)))
          ()))))

(define (remove item l)
  (cond ((null? l) l)
        ((eq? item (car l)) (remove item (cdr l)))
        (true (cons (car l) (remove item (cdr l))))))

(define (delete item l)
  (define (aux c l)
    (unless (null? l)
      (if (eq? item (car l))
          (begin
            (rplacd c (cdr l))
            (aux c (cdr l)))
          (aux l (cdr l)))))

  (if (null? l) ()
      (if (eq? item (car l))
          (delete item (cdr l))
          (begin
            (aux l (cdr l))
            l))))

(define (sort l pred)
  (if (null? l) l
      (begin
        (define pivot (car l))
        (nconc (sort (filterfor (x (cdr l)) (pred x pivot)) pred)
               (list pivot)
               (sort (filterfor (x (cdr l)) (pred pivot x)) pred)))))

(define (reverse l)
  (define (aux acc l)
    (if (null? l) acc
        (aux (cons (car l) acc) (cdr l))))
  (aux () l))

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
    (error "string range out of bounds (offset: ~A, length: ~A, string length: ~A"
           offset len str-len)))

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
  (primitive-string-copy src src-offset dest dest-offset len))

;;; Vectors

(define (check-vector-index vec index)
  (define vec-len (vector-length vec))
  (unless (and (>= index 0) (< index vec-len))
    (error "vector index out of bounds (index: ~A, vector length: ~A)"
           index vec-len)))

(define (check-vector-range vec offset len)
  (define vec-len (vector-length vec))
  (unless (and (>= offset 0) (<= (+ offset len) vec-len))
    (error "vector range out of bounds (offset: ~A, length: ~A, vector length ~A)"
           offset len vec-len)))

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
  (primitive-vector-copy src src-offset dest dest-offset len))

(define (vector-set-range! vec index len val)
  (while (/= len 0)
    (vector-set! vec index val)
    (set! index (1+ index))
    (set! len (1- len))))

;;; I/O

(define (make-c-string str)
  (define len (string-length str))
  (define c-str (make-string (1+ len)))
  (string-copy str 0 c-str 0 len)
  (string-set! c-str len (code-char 0))
  (raw-string-address c-str 0))

(when-compiling
  (define (check-syscall-result name res)
    (when (< res 0)
      (error "~A: ~D" name (- res)))
    res)

  (defmacro (checked-syscall name . args)
    (quasiquote
      (check-syscall-result (unquote name)
        (raw->fixnum (c-call (unquote name) (unquote-splicing args))))))

  (defmacro (define-syscall-read-write name sysname)
    (quasiquote (define ((unquote name) fd str offset len)
                  (check-string-range str offset len)
                  (checked-syscall  (unquote sysname) (fixnum->raw fd)
                                    (raw-string-address str offset)
                                    (fixnum->raw len)))))

  (define-syscall-read-write syscall-read "read")
  (define-syscall-read-write syscall-write "write")

  (define (stdout str offset len)
    (syscall-write 1 str offset len))

  (define (stderr str offset len)
    (syscall-write 2 str offset len))

  (define (stdin-reader str offset len)
    (syscall-read 0 str offset len))

  (define (syscall-open pathname flags mode)
    (checked-syscall "open" (make-c-string pathname)
                     (fixnum->raw flags) (fixnum->raw mode)))

  (define (syscall-close fd)
    (checked-syscall "close" (fixnum->raw fd)))

  ;; O_* values for Linux x86/x86_64

  (define (open-file-for-reading pathname)
    (syscall-open pathname 0 0))

  (define (close-file fd)
    (syscall-close fd))

  (define (make-file-reader fd)
    (lambda (str offset len) (syscall-read fd str offset len))))

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
  (define newstr (make-string len))
  (string-copy str offset newstr 0 len)
  newstr)

(define (string-equal? a b)
  (define len (string-length a))
  (and (= (string-length b) len)
       (begin
         (define pos 0)
         (define (compare-chars)
           (or (= pos len)
               (and (eq? (string-ref a pos) (string-ref b pos))
                    (begin (set! pos (1+ pos))
                           (compare-chars)))))
         (compare-chars))))

(define (string-flatten strs)
  (define pos 0)
  (dolist (str strs)
    (set! pos (+ pos (string-length str))))
  (define res (make-string pos))
  (set! pos 0)
  (dolist (str strs)
    (string-copy str 0 res pos (string-length str))
    (set! pos (+ pos (string-length str))))
  res)

(define (string-concat . strs)
  (string-flatten strs))


;;; Vectors

(define (make-vector-from-list l)
  (define (copy-list-to-vector l vec pos)
    (unless (null? l)
      (vector-set! vec pos (car l))
      (copy-list-to-vector (cdr l) vec (1+ pos))))

  (define vec (make-vector (length l)))
  (copy-list-to-vector l vec 0)
  vec)

;;; Equality

(define (equal? a b)
  (cond ((pair? a)
         (and (pair? b) (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
        ((string? a)
         (and (string? b) (string-equal? a b)))
        (true (eq? a b))))

;;; IO

(define (write-substring writer str pos len)
  (writer str pos len))

(define (write-string writer str)
  (writer str 0 (string-length str)))

;;; String buffers

(define (make-string-buffer)
  (cons 0 (make-string 20)))

(define (string-buffer-write strbuf str str-pos len)
  (define buf-pos (car strbuf))
  (define buf-pos-after (+ buf-pos len))
  (when (> buf-pos-after (string-length (cdr strbuf)))
    (define new-buf-size (* 2 (string-length (cdr strbuf))))
    (while (> buf-pos-after new-buf-size)
      (set! new-buf-size (* 2 new-buf-size)))
    
    (define new-buf (make-string new-buf-size))
    (string-copy (cdr strbuf) 0 new-buf 0 buf-pos)
    (rplacd strbuf new-buf))

  (string-copy str str-pos (cdr strbuf) buf-pos len)
  (rplaca strbuf buf-pos-after))

(define (string-buffer-write-char strbuf ch)
  (define buf-pos (car strbuf))
  (when (= buf-pos (string-length (cdr strbuf)))
    (define new-buf (make-string (* 2 (string-length (cdr strbuf)))))
    (string-copy (cdr strbuf) 0 new-buf 0 buf-pos)
    (rplacd strbuf new-buf))

  (string-set! (cdr strbuf) buf-pos ch)
  (rplaca strbuf (1+ buf-pos)))

(define (string-buffer-writer strbuf)
  (lambda (str pos len) (string-buffer-write strbuf str pos len)))

(define (string-buffer-to-string strbuf)
  (define str (make-string (car strbuf)))
  (string-copy (cdr strbuf) 0 str 0 (car strbuf))
  str)

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
        (lambda (writer) (write-string writer str))))))

(define-char-printer print-newline #\Newline)
(define-char-printer print-double-quote #\")

(define (print-number writer num)
  (if (= num 0)
      (write-string writer "0")
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
        
        (write-substring writer buf pos (- buf-size pos)))))

(define (print-string writer str)
  (if *print-readably*
      (begin
        (print-double-quote writer)
        (write-string writer str)
        (print-double-quote writer))
      (write-string writer str)))


(define character-names '((#\Space . "Space")
                          (#\Newline . "Newline")))

(define (print-char writer ch)
  (if *print-readably*
      (begin
       (write-string writer "#\\")
       (define name (assoc ch character-names))
       (write-string writer (if name
                                (character-string ch)
                                (cdr name))))
      (write-string writer (character-string ch))))

(define (print-symbol writer sym)
  (define str (symbol-name sym))
  (write-substring writer str 0 (string-length str)))

(define special-printed-forms
  '((false . "false")
    (true . "true")
    (unspecified . "unspecified")
    (() . "()")))

(define (print-list writer l)
  (write-string writer "(")
  (print writer (car l))
  (set! l (cdr l))

  (while (pair? l)
    (write-string writer " ")
    (print writer (car l))
    (set! l (cdr l)))

  (unless (null? l)
    (write-string writer " . ")
    (print writer l))

  (write-string writer ")"))

(define (print writer obj)
  (cond ((pair? obj)
         (print-list writer obj))
        ((number? obj)
         (print-number writer obj))
        ((string? obj)
         (print-string writer obj))
        ((symbol? obj)
         (print-symbol writer obj))
        ((function? obj)
         (write-string writer "#<function>"))
        (true
         (define special (assoc obj special-printed-forms))
         (if special
             (write-string writer (cdr special))
             (error "cannot print object")))))

;;; Formatted IO

(define (formout-list writer control args)
  (define pos 0)
  (define write-from 0)
  (define control-len (string-length control))
  (define ch)

  (define (flush)
    (write-substring writer control write-from (- pos write-from)))

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
                       (print-newline writer))

                      ((or (eq? ch #\A) (eq? ch #\a))
                       (with-value (*print-readably* false)
                         (print writer (car args)))
                       (set! args (cdr args)))

                      ((or (eq? ch #\S) (eq? ch #\s))
                       (with-value (*print-readably* true)
                         (print writer (car args)))
                       (set! args (cdr args)))

                      ((or (eq? ch #\D) (eq? ch #\d))
                       (with-value (*print-radix* 10)
                         (print writer (car args)))
                       (set! args (cdr args)))

                      ((or (eq? ch #\X) (eq? ch #\x))
                       (with-value (*print-radix* 16)
                         (print writer (car args)))
                       (set! args (cdr args)))

                      ((or (eq? ch #\C) (eq? ch #\c))
                       (print-char writer (car args))
                       (set! args (cdr args)))

                      (true
                       (error "Unknown format character ~C" ch))))))))

  (flush))

(define (formout writer control . args)
  (formout-list writer control args))

(define (format-list control args)
  (define buf (make-string-buffer))
  (formout-list (string-buffer-writer buf) control args)
  (string-buffer-to-string buf))

(define (format control . args)
  (format-list control args))

;;; With formatted IO in place, define error support and handlers

(define (error message . args)
  (formout stderr "~A~%" (format-list message args))
  (error-halt message args))

(when-compiling
  (set! (arity-mismatch nparams nargs)
    (error "expected ~S arguments, got ~S" nparams nargs))

  (set! (handle-varargs nparams nargs args-base)
    (when (< nargs nparams)
        (error "expected at least ~S arguments, got ~S" nparams nargs))

    (define (make-varargs-list nargs l)
      (if (/= nargs nparams)
          (make-varargs-list (1- nargs)
                             (cons (raw-arg-ref args-base (1- nargs)) l))
          l))

    (make-varargs-list nargs ()))

  (define (apply func arg1 . args)
    (define (args-length args)
      (if (null? (cdr args)) (length (car args))
          (1+ (args-length (cdr args)))))

    (define (copy-final-args args args-base index)
      (unless (null? args)
        (raw-arg-set! args-base index (car args))
        (copy-final-args (cdr args) args-base (1+ index))))
      
    (define (copy-args args args-base index)
      (if (null? (cdr args)) (copy-final-args (car args) args-base index)
          (begin
            (raw-arg-set! args-base index (car args))
            (copy-args (cdr args) args-base (1+ index)))))

    (set! args (cons arg1 args))
    (define args-len (args-length args))
    (raw-apply-with-args args-len
      (lambda ()
        (copy-args args (raw-args-address) 0)
        (raw-apply-jump func args-len))))

  (define gensym-counter 0)
  (define (gensym)
    (intern (format "gensym-~D" (set! gensym-counter (1+ gensym-counter))))))

;;; Character istreams

;;; istreams are represented as a vector of 3 elements:
;;; - Read position in the buffer
;;; - End position of valid data in the buffer
;;; - The buffer
;;; - The reader function to get more data (false if no more data)

(define (make-reader-istream reader)
  (make-vector-from-list (list 0 0 (make-string 100) reader)))

(define (make-string-istream str)
  (make-vector-from-list (list 0 (string-length str) str false)))

(define (istream-refill istr)
  ;; Refill the buffer of an istream.  Returns false if there is no
  ;; more data available from the underlying reader function.
  (if (vector-ref istr 3)
      (begin
        ;; move data down within the buffer
        (define pos (vector-ref istr 0))
        (define len (- (vector-ref istr 1) pos))
        (define buf (vector-ref istr 2))
        (when (> pos 0)
          (string-copy buf pos buf 0 len))
        (vector-set! istr 0 0)
        (define readlen ((vector-ref istr 3) buf len
                                             (- (string-length buf) len)))
        (vector-set! istr 1 (+ len readlen))
        (if (> readlen 0) true
            (begin
              (vector-set! istr 3 false)
              false)))
      false))

(define (istream-eos? istr)
  (and (= (vector-ref istr 0) (vector-ref istr 1))
       (not (istream-refill istr))))

(define (read-char istr . accept-eos)
  (set! accept-eos (if (null? accept-eos) false (car accept-eos)))
  (define pos (vector-ref istr 0))
  (cond ((< pos (vector-ref istr 1))
         (vector-set! istr 0 (1+ pos))
         (string-ref (vector-ref istr 2) pos))
        ((istream-refill istr)
         (read-char istr accept-eos))
        (accept-eos false)
        (true (error "read-char off end of stream"))))

(define (consume-char istr)
  (define pos (vector-ref istr 0))
  (cond ((< pos (vector-ref istr 1))
         (vector-set! istr 0 (1+ pos)))
        ((istream-refill istr)
         (consume-char istr))
        (true (error "consume-char off end of stream"))))

(define (peek-char istr offset)
  (define pos (+ offset (vector-ref istr 0)))
  (cond ((< pos (vector-ref istr 1))
         (string-ref (vector-ref istr 2) pos))
        ((istream-refill istr)
         ;; FIXME: we may need to expand the buffered data to include
         ;; offset.  But for now, offset is always 0 or 1, and
         ;; therefore smaller than the buffer size
         (peek-char istr offset))
        (true false)))

(define stdin (make-reader-istream stdin-reader))

;;; Reader

;;; Reader syntax is currently intended to be consistent with the CL
;;; spec.

(define readtable (make-vector 128))

(defmacro rt-illegal -1)

(defmacro rt-alpha-uc 0)
(defmacro rt-alpha-lc 1)
(defmacro rt-digit 2)
(defmacro rt-constituent-misc 3)
(defmacro rt-constituent-max 3)

(defmacro rt-eos 4)
(defmacro rt-whitespace 5)
(defmacro rt-lparen 6)
(defmacro rt-rparen 7)
(defmacro rt-line-comment 8)
(defmacro rt-double-quote 9)
(defmacro rt-single-quote 10)
(defmacro rt-sharp-sign 11)
(defmacro rt-max 11)

(define digit-bases (make-vector (1+ rt-max)))

(begin
  (vector-set-range! readtable 0 128 rt-illegal)

  (dolist (n '(9 10 13 32))
    (vector-set! readtable n rt-whitespace))

  (dolist (ch-ct (list (cons #\( rt-lparen)
                       (cons #\) rt-rparen)
                       (cons #\; rt-line-comment)
                       (cons #\" rt-double-quote)
                       (cons #\' rt-single-quote)
                       (cons #\# rt-sharp-sign)))
    (vector-set! readtable (char-code (car ch-ct)) (cdr ch-ct)))

  (vector-set-range! readtable (char-code #\A) 26 rt-alpha-uc)
  (vector-set-range! readtable (char-code #\a) 26 rt-alpha-lc)
  (vector-set-range! readtable (char-code #\0) 10 rt-digit)
  (dolist (ch '(#\. #\* #\+ #\- #\? #\< #\> #\= #\/ #\! #\~))
    (vector-set! readtable (char-code ch) rt-constituent-misc))

  (vector-set-range! digit-bases 0 rt-max false)
  (vector-set! digit-bases rt-digit (char-code #\0))
  (vector-set! digit-bases rt-alpha-uc (- (char-code #\A) 10))
  (vector-set! digit-bases rt-alpha-lc (- (char-code #\a) 10)))

(define (rt-char-type ch)
  (if ch
      (begin
        (set! ch (char-code ch))
        (define ct (if (>= ch 128) rt-illegal (vector-ref readtable ch)))
        (when (= ct rt-illegal)
          (error "bad character ~C (~D)" ch (char-code ch)))
        ct)
      rt-eos))

(define (rt-constituent? ct)
  (<= ct rt-constituent-max))

(define (read-integer istr radix)
  ;; Reads an integer from the istream.  Returns false if an integer
  ;; could not be parsed, others returns the value, stopping after the
  ;; last character forming part of the number.
  (define first-digit (peek-char istr 0))
  (define negative (if (eq? #\- first-digit)
                       (begin
                         (consume-char istr)
                         (set! first-digit (peek-char istr 0))
                         true)
                       false))

  (define (digit-value ch)
    (define ct (rt-char-type ch))
    (define base (vector-ref digit-bases ct))
    (if base
        (begin
          (define val (- (char-code ch) base))
          (if (< val radix) val false))
        false))

  (define res (digit-value first-digit))
  (if res
      (begin
        (define (scan-integer)
          (consume-char istr)
          (define val (digit-value (peek-char istr 0)))
          (when val
            (set! res (+ (* radix res) val))
            (scan-integer)))
        (scan-integer)
        (if negative (- res) res))
      false))

(define (read-token istr)
  (define buf (make-string-buffer))
  (string-buffer-write-char buf (read-char istr))

  (define (scan-token)
    (define ch (peek-char istr 0))
    (when (rt-constituent? (rt-char-type ch))
      (string-buffer-write-char buf ch)
      (consume-char istr)
      (scan-token)))

  (scan-token)
  (string-buffer-to-string buf))

(define (interpret-token token)
  (define istr (make-string-istream token))
  (define num (read-integer istr 10))
  (if (and num (istream-eos? istr))
      num
      (begin
        (define special (rassoc-equal token special-printed-forms))
        (if special
            (car special)
            (intern token)))))

(define (consume-whitespace istr)
  (define ch (peek-char istr 0))
  (define ct (rt-char-type ch))
  (when (= ct rt-whitespace)
    (consume-char istr)
    (consume-whitespace istr)))

(define (read-list istr c)
  (consume-whitespace istr)
  (define ch (peek-char istr 0))
  (cond ((eq? #\) ch)
         (consume-char istr) 
         ())
        ((and (eq? #\. ch)
              (not (rt-constituent? (rt-char-type (peek-char istr 1)))))
         (consume-char istr)

         (until (read-maybe istr c))
         (define result (car c))

         ;; find and consume the closing )
         (define (find-rparen)
           (consume-whitespace istr)
           (define ch (peek-char istr 0))
           (if (eq? #\) ch)
               (consume-char istr)
               (begin
                 (when (read-maybe istr c)
                   (error "more than one object follows . in list"))
                 (find-rparen))))
         (find-rparen)
         
         result)
        (true
         (if (read-maybe istr c)
             (begin
               (define h (car c))
               (define t (read-list istr c))
               (cons h t))
             (read-list istr c)))))

(define (consume-line-comment istr)
  (define ch (read-char istr true))
  (when (and ch (not (eq? ch #\Newline)))
    (consume-line-comment istr)))

(define (read-string-literal istr)
  (define buf (make-string-buffer))

  (define (scan-string)
    (define ch (read-char istr))
    (unless (eq? ch #\")
      (when (eq? ch #\\)
        (set! ch (read-char istr)))
      (string-buffer-write-char buf ch)
      (scan-string)))

  (scan-string)
  (string-buffer-to-string buf))

(define (rassoc-equal key l)
  (cond ((null? l) false)
        ((equal? key (cdar l)) (car l))
        (true (rassoc-equal key (cdr l)))))

(define (read-char-literal istr)
  (if (rt-constituent? (rt-char-type (peek-char istr 1)))
      (begin
        ;; a character name token
        (define name (read-token istr))
        (define named-char (rassoc-equal name character-names))
        (unless named-char (error "unknown character name ~A" name))
        (car named-char))
      (read-char istr)))

(define (read-sharp-signed istr)
  (define ch (read-char istr))

  (define (read-radixed-integer istr radix)
    (define val (read-integer istr radix))
    (unless (and val 
                 (not (rt-constituent? (rt-char-type (peek-char istr 0)))))
      (error "bad digit ~C" (peek-char istr 0)))
    val)

  (cond ((eq? ch #\\) (read-char-literal istr))
        ((or (eq? ch #\x) (eq? ch #\X)) (read-radixed-integer istr 16))
        ((or (eq? ch #\b) (eq? ch #\B)) (read-radixed-integer istr 2))
        (true (error "unknown sharp sign sequence #~C" ch))))

(define (read-maybe istr c)
  ;; like read, but might not return a value (in cases such as
  ;; comments) returns false if no value was read, otherwise it puts
  ;; the value into the car of the second arg.  this is the first time
  ;; it hurts not to have multiple returns!
  (define ch (peek-char istr 0))
  (define ct (rt-char-type ch))
  (cond ((= ct rt-whitespace)
         (consume-char istr)
         false)
        ((rt-constituent? ct)
         (rplaca c (interpret-token (read-token istr))))
        ((= ct rt-lparen)
         (consume-char istr)
         (rplaca c (read-list istr c)))
        ((= ct rt-double-quote)
         (consume-char istr)
         (rplaca c (read-string-literal istr)))
        ((= ct rt-single-quote)
         (consume-char istr)
         (rplaca c (list 'quote (read istr))))
        ((= ct rt-sharp-sign)
         (consume-char istr)
         (rplaca c (read-sharp-signed istr)))
        ((= ct rt-line-comment)
         (consume-line-comment istr)
         false)
        ((= ct rt-eos)
         (error "unexpected end of stream while reading"))
        (true
         (error "don't know how to handle character ~C (~D -> ~D)"
                ch (char-code ch) ct))))

(define (read istr . eos-val)
  (define c (cons () ()))
  (define (attempt-read)
    (if (and (not (null? eos-val)) (istream-eos? istr))
        (car eos-val)
        (if (read-maybe istr c)
            (car c)
            (attempt-read))))
  (attempt-read))

;;; Files

(defmacro (with-open-file-for-reading var-pathname . body)
  (define fd (gensym))
  (quasiquote
    (begin
      (define (unquote fd)
              (open-file-for-reading (unquote (cadr var-pathname))))
      (define (unquote (car var-pathname))
              (make-reader-istream (make-file-reader (unquote fd))))
      (unquote-splicing body)
      (close-file (unquote fd)))))

;;; CL compatibility

(defmacro (let* bindings . body)
  (quasiquote (begin
                (unquote-splicing (mapfor (binding bindings)
                                          (if (pair? binding)
                                              (cons 'define binding)
                                              (list 'define binding))))
                (unquote-splicing body))))

(defmacro (labels funcs . body)
  (quasiquote (begin
                (unquote-splicing
                  (mapfor (func funcs)
                    (list* 'define (cons (car func) (cadr func)) (cddr func))))
                (unquote-splicing body))))

(defmacro (defmarco template . body)
  (quasiquote (defmacro (unquote template) (unquote-splicing body))))

(defmacro (defconstant name val)
  (quasiquote (defmacro (unquote name) (unquote val))))

(defmacro (funcall . form) form)
(defmacro (function f) f)

(defmacro (subject-language-boolean bool) bool)
(defmacro (subject-language-symbol-name sym) (list 'symbol-name sym))
(defmacro (subject-language-intern str) (list 'intern str))
(defmacro (string-symbolcase str) str)

(defmacro (read~ . args) (cons 'read args))

(define (format~ out . args)
  (cond ((eq? out true) (apply formout stdout args))
        ((eq? out false) (apply format args))
        (true (apply formout out args))))

;; Not a full destructuring-bind  
(defmacro (bind vars values . body)
  (quasiquote
    (apply (lambda (unquote vars) (unquote-splicing body)) (unquote values))))

