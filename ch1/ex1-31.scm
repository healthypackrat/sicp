(define (inc n) (+ n 1))

; 再帰的プロセス版

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(print (factorial 6))

; piの計算

(define (pi-from-product n)
  (define (term i) (/ (* (- i 1) (+ i 1)) (* i i)))
  (define (next i) (+ i 2))
  (product term 3.0 next n))

(print (* 4 (pi-from-product 1000)))

; 反復的プロセス版

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (product identity 1 inc n))

(print (factorial 6))
