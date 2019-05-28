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

; 1.2.6 素数性のテスト

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n) ; 脚注44
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

; Fermatテスト

(use srfi-27)

(define (random n)
  (random-source-randomize! default-random-source)
  (random-integer n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

; (print (prime? 561)) ; 561は素数ではない
; (print (fast-prime? 561 1)) ; しかし素数として判定される

; 1.3 高階手続きによる抽象

(define (cube x) (* x x x))

; 1.3.1 引数としての手続き

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

; (print (sum-integers 1 10))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

; (print (sum-cubes 1 10))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

; (print (* 8 (pi-sum 1 1000)))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

; (print (sum-cubes 1 10))

(define (indentity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

; (print (sum-integers 1 10))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

; (print (* 8 (pi-sum 1 1000)))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) ; 微小区間ごとの長方形の高さの和
     dx ; 長方形の幅
     ))

; (print (integral cube 0 1 0.01))

; (print (integral cube 0 1 0.001))

; 1.3.3 一般的方法としての手続き

; 区間二分法による方程式の零点の探索

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
            (error "Values are not of opposite sign" a b)))))

; (print (half-interval-method sin 2.0 4.0))
; => 3.14111328125

; (print (half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0))
; => 1.89306640625

; 関数の不動点の探索

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; (print (fixed-point cos 1.0))
; => 0.7390822985224023

; (print (fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0))
; => 1.2587315962971173

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

; (print (sqrt 2))
; => 1.4142135623746899
