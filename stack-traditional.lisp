;;; Common parts of "traditional" x86 stack regimes.  That is, with
;;; %sp marking the bottom of the stack, and the parameters above the
;;; return address.

(define (copy-tail-call-args out-arg-count out-retaddr tmpreg cg)
  (when (> out-arg-count 0)
    (set! out-arg-count (1- out-arg-count))
    (emit-mov cg (mem %sp out-arg-count) tmpreg)
    (emit-mov cg tmpreg (mem out-retaddr 1 out-arg-count))
    (copy-tail-call-args out-arg-count out-retaddr tmpreg cg)))

(define (emit-adjust-frame-base cg in-frame-base out-frame-base)
  (unless (= in-frame-base out-frame-base)
    (emit-add cg (* value-size (- in-frame-base out-frame-base)) %sp)))

