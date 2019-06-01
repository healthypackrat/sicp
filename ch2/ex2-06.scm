(define (inc n) (+ n 1))

(define zero (lambda (f) (lambda (x) x)))

(print ((zero inc) 0))
; 0

(define one (lambda (f) (lambda (x) (f x))))

(print ((one inc) 0))
; 1

(define two (lambda (f) (lambda (x) (f (f x)))))

(print ((two inc) 0))
; 2

(define (add-1 num)
  (lambda (f) (lambda (x) (f ((num f) x)))))

(print (((add-1 zero) inc) 0))
; 1

(print (((add-1 one) inc) 0))
; 2

(print (((add-1 two) inc) 0))
; 3

(define (plus num1 num2)
  (lambda (f) (lambda (x) ((num1 f) ((num2 f) x)))))

(print (((plus one two) inc) 0))
; 3

(define one (lambda (f) (lambda (x) ((zero f) (f x)))))

(print ((one inc) 0))
; 1

(define two (lambda (f) (lambda (x) ((one f) (f x)))))

(print ((two inc) 0))
; 2
