(define (tree-map proc tree)
  (cond ((null? tree) '())
        ((pair? tree) (cons (tree-map proc (car tree))
                            (tree-map proc (cdr tree))))
        (else (proc tree))))

(define (square-tree tree) (tree-map square tree))

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

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

(define (square-tree tree) (tree-map square tree))

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
