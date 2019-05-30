(define (iterative-improve enough? improve)
  (lambda (guess)
    (define (iter guess)
      (if (enough? guess)
          guess
          (iter (improve guess))))
    (iter guess)))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (average x y)
    (/ (+ x y) 2))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

(print (sqrt 2))
; => 1.4142156862745097

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  ((iterative-improve close-enough? f) first-guess))

(print (fixed-point cos 1.0))
; => 0.7390893414033928
