(define (average x y)
  (/ (+ x y) 2))

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

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (repeated f n)
  (if (= n 0)
      (lambda (x) x)
      (compose f (repeated f (- n 1)))))

(define (n-th-root-k n x k)
  (fixed-point ((repeated average-damp k)
                (lambda (y) (/ x (expt y (- n 1)))))
               1.0))

; 何回の平均緩和が必要か実験する

; k=1 で収束する
(print (n-th-root-k 2 2 1))
(print (n-th-root-k 3 2 1))

; k=2 で収束する
(print (n-th-root-k 4 2 2))
(print (n-th-root-k 5 2 2))
(print (n-th-root-k 6 2 2))
(print (n-th-root-k 7 2 2))

; k=3 で収束する
(print (n-th-root-k 8 2 3))
(print (n-th-root-k 9 2 3))
(print (n-th-root-k 10 2 3))
(print (n-th-root-k 11 2 3))
(print (n-th-root-k 12 2 3))
(print (n-th-root-k 13 2 3))
(print (n-th-root-k 14 2 3))
(print (n-th-root-k 15 2 3))

; k=4 で収束する
(print (n-th-root-k 16 2 4))

(define (n-th-root n x)
  (let ((k (floor (/ (log n) (log 2))))) ; 底をeから2に変換する
    (n-th-root-k n x k)))

(print (n-th-root 16 2))
