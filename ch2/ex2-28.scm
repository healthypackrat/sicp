(define (fringe x)
  (if (pair? x)
      (if (pair? (car x))
          (append (fringe (car x)) (fringe (cdr x)))
          (cons (car x) (fringe (cdr x))))
      x))

(print (fringe '()))
; ()

(print (fringe 1))
; 1

(print (fringe '(1)))
; (1)

(print (fringe '((1))))
; (1)

(define x (list (list 1 2) (list 3 4)))

(print (fringe x))
; (1 2 3 4)

(print (fringe (list x x)))
; (1 2 3 4 1 2 3 4)
