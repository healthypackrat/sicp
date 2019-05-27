(define (inc n) (+ n 1))

; 再帰的プロセス版

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(print (sum identity 1 inc 10))

(define (product term a next b)
  (accumulate * 1 term a next b))

(print (product identity 1 inc 6))

; 反復的プロセス版

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(print (sum identity 1 inc 10))

(define (product term a next b)
  (accumulate * 1 term a next b))

(print (product identity 1 inc 6))
