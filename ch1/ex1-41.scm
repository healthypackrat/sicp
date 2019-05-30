(define (double f)
  (lambda (x) (f (f x))))

(define (inc n) (+ n 1))

(print (((double (double double)) inc) 5))
; => 21
