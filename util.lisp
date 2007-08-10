;;; List-like buffers
;;; Represented as (head . tail)

(define (make-buffer)
  (let* ((buf (cons () ())))
    (cons buf buf)))

(define (buffer-add buf el)
  (let* ((newtail (cons el ())))
    (rplacd (cdr buf) newtail)
    (rplacd buf newtail)))

(define (buffer-concat buf1 buf2)
  (rplacd (cdr buf1) (cdar buf2))
  (rplacd buf1 (cdr buf2)))

(define (buffer-list buf)
  (cdar buf))


(define (lassoc item lalist)
  (if (null? lalist) false
      (or (assoc item (car lalist)) (lassoc item (cdr lalist)))))

(define (lapush key val lalist)
  (rplaca lalist (cons (cons key val) (car lalist))))