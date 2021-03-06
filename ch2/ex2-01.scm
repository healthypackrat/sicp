(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (negative? d)
        (cons (/ (- n) g) (/ (- d) g))
        (cons (/ n g) (/ d g)))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(print-rat (make-rat (- 2) 3))
; -2/3

(print-rat (make-rat 2 (- 3)))
; -2/3

(print-rat (make-rat (- 2) (- 3)))
; 2/3
