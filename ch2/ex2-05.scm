(define (numeric-cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (numeric-car x)
  (if (= (remainder x 2) 0)
      (+ 1 (numeric-car (/ x 2)))
      0))

(define (numeric-cdr x)
  (if (= (remainder x 3) 0)
      (+ 1 (numeric-cdr (/ x 3)))
      0))

(let ((x (numeric-cons 0 0)))
  (print (list (numeric-car x) (numeric-cdr x))))
; (0 0)

(let ((x (numeric-cons 1 1)))
  (print (list (numeric-car x) (numeric-cdr x))))
; (1 1)

(let ((x (numeric-cons 4 5)))
  (print (list (numeric-car x) (numeric-cdr x))))
; (4 5)
