(when-compiling
  ;; gc-type table is a vector mapping from the low-order bits of a value 
  ;; to the appropriate type-specific GC routine
  (define gc-type-table)

  ;; the mask to apply to a value to get an index into gc-type-table
  (define gc-raw-tag-mask)

  ;; the limits of from-space and to-space
  (define gc-from-space false)
  (define gc-from-space-end false)
  (define gc-to-space false)
  (define gc-to-space-end false)

  (define (gc-live val)
    ;; handle a live value
    (if (and (<= gc-from-space val) (< val gc-from-space-end))
        ;; value could be a reference into from-space; dispatch to a
        ;; type-specific GC routine
        (funcall (raw-vector-ref gc-type-table 
                                 (raw->fixnum (raw-logand gc-raw-tag-mask val)))
                 val)
        ;; not a reference to from-space, therefore a reference into
        ;; to-space, to quoted data, or a fixnum.  we don't need to do
        ;; anything with those.
        val))

  (define (gc-test-copy val)
    ;; a test function that uses the GC machinery to destructively
    ;; copy objects

    ;; heap_alloc moves downwards, so from-space is above it, and
    ;; to-space is below it
    (set! gc-from-space (raw-c-global "heap_alloc"))
    (set! gc-from-space-end (raw-c-global "heap_end"))
    (set! gc-to-space (raw-c-global "heap"))
    (set! gc-to-space-end (raw-c-global "heap_alloc"))
    
    (define res (gc-live val))

    (set! gc-from-space (set! gc-from-space-end
      (set! gc-to-space (set! gc-to-space-end false))))

    res)

  (define (construct-gc-type-table . tag-bits-funcs)
    ;; contract gc-type-table passed on the type information
    (define max-bits (max$ 0 (mapfor (tbf tag-bits-funcs) (second tbf))))
    (define table-size (ash 1 max-bits))
    (set! gc-type-table (make-vector table-size))
    (set! gc-raw-tag-mask (fixnum->raw (1- table-size)))
    
    ;; fill in the table
    (dolist (tbf (cons (list 0 0 (function gc-unknown-type)) tag-bits-funcs))
      (define step (ash 1 (second tbf)))
      (define i (first tbf))
      (while (< i table-size)
        (vector-set! gc-type-table i (third tbf))
        (set! i (+ i step)))))

  (define (gc-unknown-type val)
    ;; handler for unknown tags
    (error "unknown type with tag ~D" 
           (raw->fixnum (raw-logand gc-raw-tag-mask val))))

  (defmacro (define-gc-types . type-gcs)
    ;; a convenience macro which takes a body full of type-gc
    ;; declarations, and produces a construct-gc-type-table form
    ;; taking advantage of compiler-constant
    (list* 'construct-gc-type-table
      (mapfor (type-gc type-gcs)
        (define type (first type-gc))
        (define tag (list 'compiler-constant (compound-symbol type "-tag")))
        (define bits
            (list 'compiler-constant (compound-symbol type "-tag-bits")))
        (quasiquote 
          (list (unquote tag) (unquote bits)
                ((unquote (second type-gc)) (unquote tag) (unquote bits)
                                        (unquote-splicing (cddr type-gc))))))))

  (defmacro (gc-number-type tag bits)
    ;; don't need to copy anything for a fixnum
    'identity)

  (defmacro (gc-address-type tag bits . copier)
    ;; a type that involves copying
    (define to-val (gensym))
    (quasiquote
      (begin
        (define raw-tag (fixnum->raw (unquote tag)))
        (define raw-tag-mask (fixnum->raw (1- (ash 1 (unquote bits)))))

        (lambda (val)
          ;; look at the first word of the object to see if it was
          ;; already copied
          (define addr (raw-- val raw-tag))
          (define (unquote to-val) (raw-ref addr))
          (if (and (<= gc-to-space (unquote to-val))
                   (< (unquote to-val) gc-to-space-end)
                   ;; We also need to check that the forwarding
                   ;; reference has the same tag as the object itself.
                   ;; Otherwise, it might just be a fixnum that
                   ;; happens to fall in the to-space address range.
                   (= raw-tag (raw-logand (unquote to-val) raw-tag-mask)))
              ;; the first word contains the forwarding reference
              (unquote to-val)
              ;; copy
              (begin (unquote-splicing copier)))))))

  (define-gc-types
    (number gc-number-type)

    (pair gc-address-type
      (define a (car val))
      (define copy (cons () ()))
      (raw-set! addr copy)
      (rplaca copy (gc-live a))
      (rplacd copy (gc-live (cdr val)))
      copy)

    (vector gc-address-type
      (define len (vector-length val))
      (define copy (make-vector len))
      (raw-set! addr copy)
      (gc-copy-vec addr
                   (raw-- copy (fixnum->raw (compiler-constant vector-tag)))
                   (1+ len) 1)
      copy)

    (string gc-address-type
      (define len (string-length val))
      (define copy (make-string len))
      (raw-set! addr copy)
      (raw-string-copy val 0 copy 0 len)
      copy)
    
    (symbol gc-address-type
      ;; This won't work for a symbol with its name slot set to the
      ;; symbol.  But that would be really perverse.
      (define copy (raw-make-symbol (gc-live (symbol-name val))))
      (raw-set! addr copy)
      copy)

    (closure gc-address-type
      (define code (raw-ref addr))
      (define size (raw-vec-ref code -1))
      (define copy-addr (raw-alloc closure-tag-bits size))
      (define copy (raw-+ copy-addr
                          (fixnum->raw (compiler-constant closure-tag))))
      (raw-set! addr copy)
      (raw-set! copy-addr code)
      (gc-copy-vec addr copy-addr size 1)
      copy)

    (box gc-address-type
      (define boxed (raw-box-ref val))
      (define copy (raw-make-box unspecified))
      (raw-set! addr copy)
      (raw-box-set! copy (gc-live boxed))
      copy))

  (define (gc-copy-vec src dest len i)
    (when (< i len)
      (raw-vec-set! dest i (gc-live (raw-vec-ref src i)))
      (gc-copy-vec src dest len (1+ i)))))
