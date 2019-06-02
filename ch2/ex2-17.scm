(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(print (last-pair (list 23 72 149 34)))
; (34)
