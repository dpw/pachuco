;;; Runtime and library of basic definitions

(defmacro (when-compiling . rest)
  (if compiling (cons 'definitions rest) '(definitions)))
(defmacro (when-interpreting . rest)
  (if compiling '(definitions) (cons 'definitions rest)))

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
(defmacro (cddddr x) (list 'cdr (list 'cdr (list 'cdr (list 'cdr x)))))

(defmacro (first x) (list 'car x))
(defmacro (second x) (list 'car (list 'cdr x)))
(defmacro (third x) (list 'car (list 'cdr (list 'cdr x))))
(defmacro (fourth x) (list 'car (list 'cdr (list 'cdr (list 'cdr x)))))

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

(reify (character? a))
(reify (character-code a))
(reify (code-character a))

(reify (string? a))
(reify (make-string a))
(reify (string-length a))

(reify (vector? a))
(reify (make-vector a))
(reify (vector-length a))

(when-interpreting
  ;; Provide function forms for some interpreter builtins
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

(define (min a . nums)
  (reduce a nums (lambda (a b) (if (< a b) a b))))

;; We only support left shifts, because that is all the compiler needs
(define (ash n count)
  (cond ((= count 0) n)
        ((> count 0) (ash (+ n n) (1- count)))
        (true (error "no support for right shifts"))))

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

;;; Compiled code support

;;; The real definition of error requires formatted output, which is a
;;; bit late in the day.  So put a stub definition in place here.
(define (error message . args)
  (error-halt message args))

(when-compiling
  (define (arity-mismatch nparams nargs)
    (error "expected ~S arguments, got ~S" nparams nargs))

  (define (handle-varargs nparams nargs args-base)
    (when (< nargs nparams)
        (error "expected at least ~S arguments, got ~S" nparams nargs))

    (define (make-varargs-list nargs l)
      (if (/= nargs nparams)
          (make-varargs-list (1- nargs)
                             (cons (raw-vec-ref args-base (1- nargs)) l))
          l))

    (make-varargs-list nargs ()))

  (define (apply func arg1 . args)
    (define (count-args args)
      (if (null? (cdr args)) (length (car args))
          (1+ (count-args (cdr args)))))

    (define (copy-final-args args args-base index)
      (unless (null? args)
        (raw-vec-set! args-base index (car args))
        (copy-final-args (cdr args) args-base (1+ index))))
      
    (define (copy-args args args-base index)
      (if (null? (cdr args)) (copy-final-args (car args) args-base index)
          (begin
            (raw-vec-set! args-base index (car args))
            (copy-args (cdr args) args-base (1+ index)))))

    (set! args (cons arg1 args))
    (define arg-count (count-args args))
    (raw-jump-with-arg-space raw-arg-count arg-count
      (lambda ()
        (copy-args args (raw-args-base) 0)
        (raw-apply-jump func arg-count)))))

;;; Lists

(define (length-aux l n)
  (if (null? l) n (length-aux (cdr l) (1+ n))))

(define (length l)
  (length-aux l 0))

(define (member? item l)
  (cond ((null? l) false)
        ((eq? item (car l)) true)
        (true (member? item (cdr l)))))

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

(defmacro (list* l1 . l)
  (define (aux l1 l)
    (if (null? l) l1
        (quasiquote (cons (unquote l1) (unquote (aux (car l) (cdr l)))))))
  (aux l1 l))

(define (copy-list l)
  (if (pair? l) (cons (car l) (copy-list (cdr l))) l))

(define (copy-tree l)
  (if (pair? l)
      (cons (copy-tree (car l)) (copy-tree (cdr l)))
      l))

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
        (rplaca (unquote l) (begin (unquote-splicing body)))
        ((unquote loopf) (cdr (unquote l)))))
    (define (unquote l) (unquote (second binding)))
    ((unquote loopf) (unquote l))
    (unquote l))))

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

(defmacro (nfilterfor binding . body)
  (define loopf (gensym))
  (define in (gensym))
  (define out (gensym))
  (define out-tail (gensym))
  (quasiquote (begin
     (define (unquote out) (cons () ()))
     (define ((unquote loopf) (unquote in) (unquote out-tail))
       (if (not (null? (unquote in)))
         ((unquote loopf) (cdr (unquote in))
           (if (begin (define (unquote (first binding)) (car (unquote in)))
                      (unquote-splicing body))
               (begin (rplacd (unquote out-tail) (unquote in))
                      (unquote in))
               (unquote out-tail)))
         (rplacd (unquote out-tail) ())))
     ((unquote loopf) (unquote (second binding)) (unquote out))
     (cdr (unquote out)))))

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
               (sort (filterfor (x (cdr l)) (not (pred x pivot))) pred)))))

(define (reverse l)
  (define (aux acc l)
    (if (null? l) acc
        (aux (cons (car l) acc) (cdr l))))
  (aux () l))

(define (sublist l start . end)
  (if (null? end) (copy-list (nthcdr start l))
      (begin
        (define (copy-partial-list l len)
          (cond ((= 0 len) ())
                ((not (null? l))
                 (cons (car l) (copy-partial-list (cdr l) (1- len))))
                (true
                 (error "sublist fell off the end of the list"))))
        (copy-partial-list (nthcdr start l) (- (first end) start)))))

;;; Symbols

(when-compiling
  (define (intern str)
    (or (findfor (sym interned-symbols)
                 (string-equal? str (symbol-name sym)))
        (begin
          (define sym (raw-make-symbol str))
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
  (raw-string-ref str index))

(define (string-set! str index ch)
  (check-string-index str index)
  (raw-string-set! str index ch)
  ch)

(define (string-copy src src-offset dest dest-offset len)
  (check-string-range src src-offset len)
  (check-string-range dest dest-offset len)
  (raw-string-copy src src-offset dest dest-offset len))

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
  (raw-vector-ref vec index))

(define (vector-set! vec index val)
  (check-vector-index vec index)
  (raw-vector-set! vec index val)
  val)

(define (vector-copy src src-offset dest dest-offset len)
  (check-vector-range src src-offset len)
  (check-vector-range dest dest-offset len)
  (raw-vector-copy src src-offset dest dest-offset len))

(define (vector-set-range! vec index len val)
  (while (/= len 0)
    (vector-set! vec index val)
    (set! index (1+ index))
    (set! len (1- len))))

(define (make-vector-from-list l)
  (define (copy-list-to-vector l vec pos)
    (unless (null? l)
      (vector-set! vec pos (car l))
      (copy-list-to-vector (cdr l) vec (1+ pos))))

  (define vec (make-vector (length l)))
  (copy-list-to-vector l vec 0)
  vec)

;;; I/O

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

  (define-syscall-read-write raw-write-substring "write")
  (define raw-stdout 1)
  (define raw-stderr 2)

  (define-syscall-read-write raw-read-substring "read")
  (define raw-stdin 0)

  (define (make-c-string str)
    (define len (string-length str))
    (define c-str (make-string (1+ len)))
    (string-copy str 0 c-str 0 len)
    (string-set! c-str len (code-character 0))
    (raw-string-address c-str 0))

  (define (syscall-open pathname flags mode)
    (checked-syscall "open" (make-c-string pathname)
                     (fixnum->raw flags) (fixnum->raw mode)))

  (define (syscall-close fd)
    (checked-syscall "close" (fixnum->raw fd)))

  ;; O_* values for Linux x86/x86_64

  (define (open-file-for-reading pathname)
    (syscall-open pathname 0 0))

  (define (close-file fd)
    (syscall-close fd)))


;;; Output streams

;;; ostreams are represented as a vector of 4 elements:

;;; - Write position in the buffer
;;; - The buffer
;;; - The output FD, or false for a buffer
;;;
;;; The position must always be before the end of the buffer.  This
;;; means that a buffer of length 1 gives us unbuffered output.

(define (make-fd-ostream fd size)
  (make-vector-from-list (list 0 (make-string size) fd)))

(define (make-buffer-ostream) 
  (make-fd-ostream false 100))

(define (buffer-ostream-to-string ostr)
  (define buf-pos (vector-ref ostr 0))
  (define str (make-string buf-pos))
  (string-copy (vector-ref ostr 1) 0 str 0 buf-pos)
  str)

(define (write-substring ostr str pos len)
  (define buf-pos (vector-ref ostr 0))
  (define buf-pos-after (+ buf-pos len))
  (define buf (vector-ref ostr 1))

  (if (< buf-pos-after (string-length buf))
      (string-copy str pos buf buf-pos len)
      (begin
        ;; new data won't fit
        (define fd (vector-ref ostr 2))
        (if fd
            (begin
              (when (/= buf-pos 0)
                ;; flush to the fd
                (raw-write-substring fd buf 0 buf-pos))
             
              ;; if the new data is bigger than half the buffer size, write
              ;; it directly, otherwise copy into the buffer
              (if (> (* 2 len) (string-length buf))
                  (begin
                    (raw-write-substring fd str pos len)
                    (set! buf-pos-after 0))
                  (begin
                    (string-copy str pos buf 0 len)
                    (set! buf-pos-after len))))
            (begin
              ;; grow the buffer
              (define new-buf-size (make-string (* 2 (string-length buf))))
              (while (> buf-pos-after new-buf-size)
                (set! new-buf-size (* 2 new-buf-size)))

              (define new-buf (make-string new-buf-size))
              (string-copy buf 0 new-buf 0 buf-pos)
              (string-copy str pos new-buf buf-pos len)
              (vector-set! ostr 1 new-buf)))))
  (vector-set! ostr 0 buf-pos-after))

(define (write-string ostr str)
  (write-substring ostr str 0 (string-length str)))

(define (write-character ostr ch)
  (define buf-pos (vector-ref ostr 0))
  (define buf (vector-ref ostr 1))
  (string-set! buf buf-pos ch)
  (set! buf-pos (1+ buf-pos))

  (when (= buf-pos (string-length buf))
    ;; buffer full
    (define fd (vector-ref ostr 2))
    (if fd
        (begin
          (raw-write-substring fd buf 0 buf-pos)
          (set! buf-pos 0))
        (begin
          ;; grow the buffer
          (define new-buf (make-string (* 2 (string-length buf))))
          (string-copy buf 0 new-buf 0 buf-pos)
          (vector-set! ostr 1 (set! buf new-buf)))))

  (vector-set! ostr 0 buf-pos))

(define (flush-ostream ostr)
  (define buf-pos (vector-ref ostr 0))
  (define fd (vector-ref ostr 2))
  (when (and fd (/= buf-pos 0))
    (raw-write-substring fd (vector-ref ostr 1) 0 buf-pos)
    (vector-set! ostr 0 0)))

(define stdout (make-fd-ostream raw-stdout 10000))
(define stderr (make-fd-ostream raw-stderr 1))

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
  (define res (gensym))
  (quasiquote
    (begin
      (define (unquote temp) (unquote (first binding)))
      (set! (unquote (first binding)) (unquote (second binding)))
      (define (unquote res) (begin (unquote-splicing body)))
      (set! (unquote (first binding)) (unquote temp))
      (unquote res))))

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

(define (string-range-equal? a astart b bstart len)
  (cond ((= 0 len) true)
        ((or (>= astart (string-length a))
             (>= bstart (string-length b))
             (not (eq? (string-ref a astart) (string-ref b bstart))))
         false)
        (true
         (string-range-equal? a (1+ astart) b (1+ bstart) (1- len)))))

(define (string-search haystack needle start)
  (define len (string-length needle))
  (cond ((> (+ start len) (string-length haystack)) false)
        ((string-range-equal? haystack start needle 0 len) start)
        (true (string-search haystack needle (1+ start)))))

(define (string-replace str old new)
  (define (string-replace-from str start)
    (define pos (string-search str old start))
    (if pos
        (begin
          (define rest (+ pos (string-length old)))
          (define res (string-concat (substring str 0 pos) new
                           (substring str rest (- (string-length str) rest))))
          (string-replace-from res (+ pos (string-length new))))
        str))
  (string-replace-from str 0))

;;; Equality

(define (equal? a b)
  (cond ((pair? a)
         (and (pair? b) (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
        ((string? a)
         (and (string? b) (string-equal? a b)))
        (true (eq? a b))))

;;; Printing and formatting

(define print-number-digits "0123456789abcdefghijklmnopqrstuvwxyz")

(define *print-radix* 10)
(define *print-readably* false)

(define (character-string ch)
  (define buf (make-string 1))
  (string-set! buf 0 ch)
  buf)

(define (print-number ostr num)
  (if (= num 0)
      (write-character ostr #\0)
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
        
        (write-substring ostr buf pos (- buf-size pos)))))

(define (print-string ostr str)
  (if *print-readably*
      (begin
        (write-character ostr #\")
        (write-string ostr str)
        (write-character ostr #\"))
      (write-string ostr str)))

(define character-names '((#\Space . "Space")
                          (#\Newline . "Newline")))

(define (print-character ostr ch)
  (if *print-readably*
      (begin
        (write-character ostr #\\)
        (define name (assoc ch character-names))
        (write-string ostr (if name (character-string ch) (cdr name))))
      (write-string ostr (character-string ch))))

(define (print-symbol ostr sym)
  (define str (symbol-name sym))
  (write-substring ostr str 0 (string-length str)))

(define special-printed-forms
  '((false . "false")
    (true . "true")
    (unspecified . "unspecified")
    (() . "()")))

(define (print-list ostr l)
  (write-character ostr #\()
  (print ostr (car l))
  (set! l (cdr l))

  (while (pair? l)
    (write-character ostr #\Space)
    (print ostr (car l))
    (set! l (cdr l)))

  (unless (null? l)
    (write-string ostr " . ")
    (print ostr l))

  (write-character ostr #\)))

(define (print ostr obj)
  (cond ((pair? obj)
         (print-list ostr obj))
        ((number? obj)
         (print-number ostr obj))
        ((string? obj)
         (print-string ostr obj))
        ((symbol? obj)
         (print-symbol ostr obj))
        ((function? obj)
         (write-string ostr "#<function>"))
        (true
         (define special (assoc obj special-printed-forms))
         (if special
             (write-string ostr (cdr special))
             (error "cannot print object")))))

;;; Formatted IO

(define (formout-list ostr control args)
  (define pos 0)
  (define write-from 0)
  (define control-len (string-length control))
  (define ch)

  (define (flush)
    (write-substring ostr control write-from (- pos write-from)))

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
                       (write-character ostr #\Newline))

                      ((or (eq? ch #\A) (eq? ch #\a))
                       (with-value (*print-readably* false)
                         (print ostr (car args)))
                       (set! args (cdr args)))

                      ((or (eq? ch #\S) (eq? ch #\s))
                       (with-value (*print-readably* true)
                         (print ostr (car args)))
                       (set! args (cdr args)))

                      ((or (eq? ch #\D) (eq? ch #\d))
                       (with-value (*print-radix* 10)
                         (print ostr (car args)))
                       (set! args (cdr args)))

                      ((or (eq? ch #\X) (eq? ch #\x))
                       (with-value (*print-radix* 16)
                         (print ostr (car args)))
                       (set! args (cdr args)))

                      ((or (eq? ch #\C) (eq? ch #\c))
                       (print-character ostr (car args))
                       (set! args (cdr args)))

                      (true
                       (error "Unknown format character ~C" ch))))))))

  (flush))

(define (formout ostr control . args)
  (formout-list ostr control args))

(define (format-list control args)
  (define buf (make-buffer-ostream))
  (formout-list buf control args)
  (buffer-ostream-to-string buf))

(define (format control . args)
  (format-list control args))

(set! (error message . args)
  (formout stderr "~A~%" (format-list message args))
  (error-halt message args))

(when-compiling
  (define gensym-counter 0)
  (define (gensym)
    (intern (format "gensym-~D" (set! gensym-counter (1+ gensym-counter))))))

;;; Input streams

;;; istreams are represented as a vector of 4 elements:
;;; - Read position in the buffer
;;; - End position of valid data in the buffer
;;; - The buffer
;;; - The reader function to get more data (false if no more data)

(define (make-fd-istream fd)
  (make-vector-from-list (list 0 0 (make-string 10000) fd)))

(define stdin (make-fd-istream raw-stdin))

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
        (define readlen (raw-read-substring (vector-ref istr 3) buf len
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

(define (read-character istr . accept-eos)
  (set! accept-eos (if (null? accept-eos) false (car accept-eos)))
  (define pos (vector-ref istr 0))
  (cond ((< pos (vector-ref istr 1))
         (vector-set! istr 0 (1+ pos))
         (string-ref (vector-ref istr 2) pos))
        ((istream-refill istr)
         (read-character istr accept-eos))
        (accept-eos false)
        (true (error "read-character off end of stream"))))

(define (consume-character istr)
  (define pos (vector-ref istr 0))
  (cond ((< pos (vector-ref istr 1))
         (vector-set! istr 0 (1+ pos)))
        ((istream-refill istr)
         (consume-character istr))
        (true (error "consume-character off end of stream"))))

(define (peek-character istr offset)
  (define pos (+ offset (vector-ref istr 0)))
  (cond ((< pos (vector-ref istr 1))
         (string-ref (vector-ref istr 2) pos))
        ((istream-refill istr)
         ;; FIXME: we may need to expand the buffered data to include
         ;; offset.  But for now, offset is always 0 or 1, and
         ;; therefore smaller than the buffer size
         (peek-character istr offset))
        (true false)))

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
    (vector-set! readtable (character-code (car ch-ct)) (cdr ch-ct)))

  (vector-set-range! readtable (character-code #\A) 26 rt-alpha-uc)
  (vector-set-range! readtable (character-code #\a) 26 rt-alpha-lc)
  (vector-set-range! readtable (character-code #\0) 10 rt-digit)
  (dolist (ch '(#\. #\* #\+ #\- #\? #\< #\> #\= #\/ #\! #\~ #\%))
    (vector-set! readtable (character-code ch) rt-constituent-misc))

  (vector-set-range! digit-bases 0 rt-max false)
  (vector-set! digit-bases rt-digit (character-code #\0))
  (vector-set! digit-bases rt-alpha-uc (- (character-code #\A) 10))
  (vector-set! digit-bases rt-alpha-lc (- (character-code #\a) 10)))

(define (rt-character-type ch)
  (if ch
      (begin
        (set! ch (character-code ch))
        (define ct (if (>= ch 128) rt-illegal (vector-ref readtable ch)))
        (when (= ct rt-illegal)
          (error "bad character ~C (~D)" ch (character-code ch)))
        ct)
      rt-eos))

(define (rt-constituent? ct)
  (<= ct rt-constituent-max))

(define (read-integer istr radix)
  ;; Reads an integer from the istream.  Returns false if an integer
  ;; could not be parsed, others returns the value, stopping after the
  ;; last character forming part of the number.
  (define first-digit (peek-character istr 0))
  (define negative (if (eq? #\- first-digit)
                       (begin
                         (consume-character istr)
                         (set! first-digit (peek-character istr 0))
                         true)
                       false))

  (define (digit-value ch)
    (define ct (rt-character-type ch))
    (define base (vector-ref digit-bases ct))
    (if base
        (begin
          (define val (- (character-code ch) base))
          (if (< val radix) val false))
        false))

  (define res (digit-value first-digit))
  (if res
      (begin
        (define (scan-integer)
          (consume-character istr)
          (define val (digit-value (peek-character istr 0)))
          (when val
            (set! res (+ (* radix res) val))
            (scan-integer)))
        (scan-integer)
        (if negative (- res) res))
      false))

(define (read-token istr)
  (define buf (make-buffer-ostream))
  (write-character buf (read-character istr))

  (define (scan-token)
    (define ch (peek-character istr 0))
    (when (rt-constituent? (rt-character-type ch))
      (write-character buf ch)
      (consume-character istr)
      (scan-token)))

  (scan-token)
  (buffer-ostream-to-string buf))

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
  (define ch (peek-character istr 0))
  (define ct (rt-character-type ch))
  (when (= ct rt-whitespace)
    (consume-character istr)
    (consume-whitespace istr)))

(define (read-list istr c)
  (consume-whitespace istr)
  (define ch (peek-character istr 0))
  (cond ((eq? #\) ch)
         (consume-character istr) 
         ())
        ((and (eq? #\. ch)
              (not (rt-constituent? (rt-character-type (peek-character istr 1)))))
         (consume-character istr)

         (until (read-maybe istr c))
         (define result (car c))

         ;; find and consume the closing )
         (define (find-rparen)
           (consume-whitespace istr)
           (define ch (peek-character istr 0))
           (if (eq? #\) ch)
               (consume-character istr)
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
  (define ch (read-character istr true))
  (when (and ch (not (eq? ch #\Newline)))
    (consume-line-comment istr)))

(define (read-string-literal istr)
  (define buf (make-buffer-ostream))

  (define (scan-string)
    (define ch (read-character istr))
    (unless (eq? ch #\")
      (when (eq? ch #\\)
        (set! ch (read-character istr)))
      (write-character buf ch)
      (scan-string)))

  (scan-string)
  (buffer-ostream-to-string buf))

(define (rassoc-equal key l)
  (cond ((null? l) false)
        ((equal? key (cdar l)) (car l))
        (true (rassoc-equal key (cdr l)))))

(define (read-character-literal istr)
  (if (rt-constituent? (rt-character-type (peek-character istr 1)))
      (begin
        ;; a character name token
        (define name (read-token istr))
        (define named-character (rassoc-equal name character-names))
        (unless named-character (error "unknown character name ~A" name))
        (car named-character))
      (read-character istr)))

(define (read-sharp-signed istr)
  (define ch (read-character istr))

  (define (read-radixed-integer istr radix)
    (define val (read-integer istr radix))
    (unless (and val 
                 (not (rt-constituent? (rt-character-type (peek-character istr
                                                                          0)))))
      (error "bad digit ~C" (peek-character istr 0)))
    val)

  (cond ((eq? ch #\\) (read-character-literal istr))
        ((or (eq? ch #\x) (eq? ch #\X)) (read-radixed-integer istr 16))
        ((or (eq? ch #\b) (eq? ch #\B)) (read-radixed-integer istr 2))
        (true (error "unknown sharp sign sequence #~C" ch))))

(define (read-maybe istr c)
  ;; like read, but might not return a value (in cases such as
  ;; comments) returns false if no value was read, otherwise it puts
  ;; the value into the car of the second arg.  this is the first time
  ;; it hurts not to have multiple returns!
  (define ch (peek-character istr 0))
  (define ct (rt-character-type ch))
  (cond ((= ct rt-whitespace)
         (consume-character istr)
         false)
        ((rt-constituent? ct)
         (rplaca c (interpret-token (read-token istr))))
        ((= ct rt-lparen)
         (consume-character istr)
         (rplaca c (read-list istr c)))
        ((= ct rt-double-quote)
         (consume-character istr)
         (rplaca c (read-string-literal istr)))
        ((= ct rt-single-quote)
         (consume-character istr)
         (rplaca c (list 'quote (read istr))))
        ((= ct rt-sharp-sign)
         (consume-character istr)
         (rplaca c (read-sharp-signed istr)))
        ((= ct rt-line-comment)
         (consume-line-comment istr)
         false)
        ((= ct rt-eos)
         (error "unexpected end of stream while reading"))
        (true
         (error "don't know how to handle character ~C (~D -> ~D)"
                ch (character-code ch) ct))))

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
      (define (unquote (car var-pathname)) (make-fd-istream (unquote fd)))
      (unquote-splicing body)
      (close-file (unquote fd)))))

;;; Command line access

(define (copy-c-string addr)
  (define (strlen addr)
    (define (aux offset)
        (if (= 0 (raw->fixnum (raw-1-vec-ref addr offset)))
            offset
            (aux (1+ offset))))
    (aux 0))

  (define len (strlen addr))
  (define str (make-string len))
  (raw-1-copy addr (raw-string-address str 0) len)
  str)

(define (c-string-array-to-list addr)
  (define (aux index)
    (define str-addr (raw-vec-ref addr index))
    (if (eq? 0 str-addr)
        ()
        (cons (copy-c-string str-addr) (aux (1+ index)))))
  (aux 0))

(define saved-command-line false)

(define (command-line)
  (unless saved-command-line
    (set! saved-command-line (c-string-array-to-list (c-global "lisp_argv"))))
  saved-command-line)

;;; Entry point function

(define (runtime-main)
  (main)
  (flush-ostream stdout))

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

(define (reduce~ initial l f) (reduce initial l f))

;; Not a full destructuring-bind  
(defmacro (bind vars values . body)
  (quasiquote
    (apply (lambda (unquote vars) (unquote-splicing body)) (unquote values))))

