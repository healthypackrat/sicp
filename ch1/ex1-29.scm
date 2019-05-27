(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (term i)
    (if (even? i)
        (* 2 (y i))
        (* 4 (y i))))
  (define (next i) (+ i 1))
  (* (/ h 3.0)
     (+ (y 0)
        (sum term 1 next (- n 1))
        (y n)
        )))


(print (simpson cube 0 1 100))

(print (simpson cube 0 1 1000))
