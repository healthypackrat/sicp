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

; 1.2.2 木構造再帰

; 再帰的プロセス

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

; (print (fib 10))

; 反復的プロセス

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

; (print (fib 10))

; 例: 両替の計算

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; (print (count-change 100))

; 1.2.4 べき乗

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

; (print (expt 2 8))

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))

; (print (expt 2 8))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

; (print (expt 2 8))

; 1.2.5 最大公約数

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; (print (gcd 16 28))
; (print (gcd 206 40))
