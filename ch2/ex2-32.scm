(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        ; (print rest)
        (append rest
                (map (lambda (x) (cons (car s) x))
                     rest)))))

(print (subsets '()))
; (())

(print (subsets '(1)))
; (() (1))

(print (subsets '(1 2)))
; (() (2) (1) (1 2))

(print (subsets '(1 2 3)))
; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
