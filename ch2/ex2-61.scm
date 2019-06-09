(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(print (adjoin-set 3 '()))
; (3)

(print (adjoin-set 3 '(3)))
; (3)

(print (adjoin-set 3 '(4)))
; (3 4)

(print (adjoin-set 3 '(2 4 6)))
; (2 3 4 6)
