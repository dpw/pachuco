;;; common runtime definitions, even when bootstrapping from CL

(define (compound-symbol . pieces)
  (intern (string-flatten (mapfor (p pieces)
                            (cond ((string? p) (string-symbolcase p))
                                  ((symbol? p) (symbol-name p))
                                  (true (error "~S" p)))))))
