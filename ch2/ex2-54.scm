(define (equal? a b)
  (or (and (not (pair? a)) (not (pair? b)) (eq? a b))
      (and (pair? a) (pair? b)
           (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))))

(print (equal? '() '()))
; #t

(print (equal? 1 1))
; #t

(print (equal? 1 2))
; #f

(print (equal? '(this is a list) '(this is a list)))
; #t

(print (equal? '(this is a list) '(this (is a) list)))
; #f
