(define (reverse x)
  (if (null? x)
      x
      (append (reverse (cdr x)) (list (car x)))))

(print (reverse (list 1 4 9 16 25)))
; (25 16 9 4 1)
