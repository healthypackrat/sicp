(define (deep-reverse x)
  (if (pair? x)
      (append (deep-reverse (cdr x))
              (list (deep-reverse (car x))))
      x))

(print (deep-reverse '((1 2) (3 4))))
; ((4 3) (2 1))

(print (deep-reverse '(1 (2 (3 (4 (5 (6 7))))))))
; ((((((7 6) 5) 4) 3) 2) 1)
