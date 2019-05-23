(define (sqrt-iter last-guess next-guess x)
  (if (good-enough? last-guess next-guess)
      next-guess
      (sqrt-iter next-guess (improve next-guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? last-guess next-guess)
  ; 予測値の差の変化率が微小ならOK
  (< (abs (/ (- last-guess next-guess) next-guess)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 (improve 1.0 x) x))

(print (sqrt 2))
(print (sqrt 0.2))
(print (sqrt 0.02))
(print (sqrt 0.002))
(print (sqrt 0.0002))
(print (sqrt 200000000000000000000))
