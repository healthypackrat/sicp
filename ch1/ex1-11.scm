(define (f1 n)
  (if (< n 3)
      n
      (+ (f1 (- n 1))
         (* 2 (f1 (- n 2)))
         (* 3 (f1 (- n 3))))))

(print (f1 0))
(print (f1 1))
(print (f1 2))
(print (f1 3))
(print (f1 4))
(print (f1 5))

(define (f2 n)
  (f2-iter 2 1 0 n))

(define (f2-iter x y z n)
  (cond ((= n 0) z)
        ((= n 1) y)
        ((= n 2) x)
        (else (f2-iter (+ x (* y 2) (* z 3)) x y (- n 1)))))

(print (f1 0))
(print (f2 1))
(print (f2 2))
(print (f2 3))
(print (f2 4))
(print (f2 5))
