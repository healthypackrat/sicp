(define (same-parity first . rest)
  (define (iter items)
    (cond ((null? items) items)
          ((= (remainder (car items) 2) (remainder first 2))
           (cons (car items) (iter (cdr items))))
          (else
            (iter (cdr items)))))
  (iter (cons first rest)))

(print (same-parity 1 2 3 4 5 6 7))
; (1 3 5 7)

(print (same-parity 2 3 4 5 6 7))
; (2 4 6)
