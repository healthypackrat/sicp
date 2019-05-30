(define (compose f g)
  (lambda (x) (f (g x))))

(define (inc n) (+ n 1))

(print ((compose square inc) 6))
; => 49
