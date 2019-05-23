; 1.1.4 合成手続き

(define (square x) (* x x))

; (print (square 21))

; (print (square (+ 2 5)))

; (print (square (square 3)))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

; (print (sum-of-squares 3 4))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

; (print (f 5))

; 1.1.6 条件式と述語

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define (abs x)
  (if (< x 0)
      (- x)
      x))
