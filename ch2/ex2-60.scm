(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (append set1 set2))

(print (adjoin-set 3 '(1 2 3)))
; (3 1 2 3)

(print (union-set '(1 2 3) '(2 3 4)))
; (1 2 3 2 3 4)
