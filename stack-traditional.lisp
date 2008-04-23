;;; Common parts of "traditional" x86 stack regimes.  That is, with
;;; %sp marking the bottom of the stack, and the parameters above the
;;; return address.

(define (copy-tail-call-args out-arg-count out-retaddr tmpreg out)
  (when (> out-arg-count 0)
    (set! out-arg-count (1- out-arg-count))
    (emit-mov out (mem %sp out-arg-count) tmpreg)
    (emit-mov out tmpreg (mem out-retaddr 1 out-arg-count))
    (copy-tail-call-args out-arg-count out-retaddr tmpreg out)))

(define (emit-adjust-frame-base out in-frame-base out-frame-base)
  (unless (= in-frame-base out-frame-base)
    (emit-add out (* value-size (- in-frame-base out-frame-base)) %sp)))

