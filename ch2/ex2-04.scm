(define (lambda-cons x y)
  (lambda (m) (m x y)))

(define (lambda-car z)
  (z (lambda (p q) p)))

(define (lambda-cdr z)
  (z (lambda (p q) q)))

(let ((x (lambda-cons 2 3)))
  (print (list (lambda-car x) (lambda-cdr x))))
; (2 3)
