;;; common runtime definitions, even when bootstrapping from CL

(define (compound-symbol . pieces)
  (intern (string-flatten (mapfor (p pieces)
                            (cond ((string? p) (string-symbolcase p))
                                  ((symbol? p) (symbol-name p))
                                  (true (error "~S" p)))))))


;;; hash tables

(defconstant hashtable-initial-size 11)

(define (make-hashtable hashf equalf)
  (let* ((buckets (make-vector hashtable-initial-size)))
    (vector-set-range! buckets 0 hashtable-initial-size ())
    (list hashf equalf buckets 0)))

(defmarco (hashtable-hash ht val)
  (quasiquote (funcall (first (unquote ht)) (unquote val))))

(define (hashtable-ref ht key)
  (let* ((keyhash (hashtable-hash ht key))
         (buckets (third ht))
         (bucket (vector-ref buckets (mod keyhash (vector-length buckets))))
         (equalf (second ht))
         (match (findfor (entry bucket) (funcall equalf key (first entry)))))
    (and match (second match))))

(define (hashtable-set! ht key value)
  (when (>= (* 2 (fourth ht)) (vector-length (third ht)))
    (hashtable-grow ht))

  (let* ((keyhash (hashtable-hash ht key))
         (buckets (third ht))
         (bucket-index (mod keyhash (vector-length buckets)))
         (bucket (vector-ref buckets (mod keyhash (vector-length buckets))))
         (equalf (second ht))
         (match (findfor (entry bucket) (funcall equalf key (first entry)))))
    (if match
        (rplaca (cdr match) value)
        (begin
          (rplaca (cdddr ht) (1+ (fourth ht)))
          (vector-set! buckets bucket-index
                       (cons (list key value keyhash) bucket))))))

(define (hashtable-grow ht)
  (let* ((buckets (third ht))
         (new-buckets-len (1+ (* 2 (vector-length buckets))))
         (new-buckets (make-vector new-buckets-len)))
    (vector-set-range! new-buckets 0 (vector-length new-buckets) ())
    (labels ((rehash-bucket (index)
               (when (< index (vector-length buckets))
                 (dolist (entry (vector-ref buckets index))
                   (let* ((bucket-index (mod (third entry) new-buckets-len)))
                     (vector-set! new-buckets bucket-index
                          (cons entry (vector-ref new-buckets bucket-index)))))
                 (rehash-bucket (1+ index)))))
      (rehash-bucket 0))
    (rplaca (cddr ht) new-buckets)))

(define (string-hash str)
  (string-range-hash str 0 (string-length str)))

(define (string-range-hash str index len)
  (labels ((scan (len mult hash)
             (if (= len 0) hash
               (scan (1- len) (* 31 mult)
                     (+ hash (* mult (character-code
                                        (string-ref str (+ index len -1)))))))))
    (scan len 1 0)))
