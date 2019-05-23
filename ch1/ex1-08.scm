(define (cube-root-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-root-iter (improve guess x)
                      x)))

(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* guess 2)) 3))

(define (good-enough? guess x)
  (< (abs (- (* guess guess guess) x)) 0.001))

(define (cube-root x)
  (cube-root-iter 1.0 x))

(print (cube-root 8))
(print (cube-root 1000))
