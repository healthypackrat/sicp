(define (make-accumulator sum)
  (lambda (amount)
    (set! sum (+ sum amount))
    sum))

(define (main argv)
  (define A (make-accumulator 5))

  (print (A 10))
  ; 15

  (print (A 10))
  ; 25

  0)
