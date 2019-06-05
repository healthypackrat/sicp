(define (reverse sequence)
  (fold-right (lambda (this rest) (append rest (list this))) '() sequence))

(print (reverse (list 1 4 9 16 25)))
; (25 16 9 4 1)

(define (reverse sequence)
  (fold-left (lambda (rest this) (cons this rest)) '() sequence))

(print (reverse (list 1 4 9 16 25)))
; (25 16 9 4 1)
