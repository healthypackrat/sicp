(define (square-tree tree)
  (cond ((null? tree) '())
        ((pair? tree) (cons (square-tree (car tree))
                            (square-tree (cdr tree))))
        (else (square tree))))

(print (square-tree '()))
; ()

(print (square-tree '(2)))
; (4)

(print (square-tree '((2))))
; ((4))

(print (square-tree (list 1
                          (list 2 (list 3 4) 5)
                          (list 6 7))))
; (1 (4 (9 16) 25) (36 49))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

(print (square-tree '()))
; ()

(print (square-tree '(2)))
; (4)

(print (square-tree '((2))))
; ((4))

(print (square-tree (list 1
                          (list 2 (list 3 4) 5)
                          (list 6 7))))
; (1 (4 (9 16) 25) (36 49))
