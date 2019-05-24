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

; 1.1.7 例: Newton法による平方根

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; (print (sqrt 9))
; (print (sqrt (+ 100 37)))
; (print (sqrt (+ (sqrt 2) (sqrt 3))))
; (print (square (sqrt 1000)))

; 1.2.1 線形再帰と反復

; (線形)再帰的プロセス
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

; (print (factorial 6))

; (線形)反復的プロセス
(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

; (print (factorial 6))
