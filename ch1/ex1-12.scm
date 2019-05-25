(define (pascal n i)
  (if (or (= i 0) (= i n))
      1
      (+ (pascal (- n 1) (- i 1)) (pascal (- n 1) i))))

(print (list (pascal 0 0)))
(print (list (pascal 1 0) (pascal 1 1)))
(print (list (pascal 2 0) (pascal 2 1) (pascal 2 2)))
(print (list (pascal 3 0) (pascal 3 1) (pascal 3 2) (pascal 3 3)))
(print (list (pascal 4 0) (pascal 4 1) (pascal 4 2) (pascal 4 3) (pascal 4 4)))
