(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(print (fold-right / 1 (list 1 2 3)))
; 3/2
(print (/ 1 (/ 2 (/ 3 1))))
; 3/2

(print (fold-left / 1 (list 1 2 3)))
; 1/6
(print (/ (/ (/ 1 1) 2) 3))
; 1/6

(print (fold-right list '() (list 1 2 3)))
; (1 (2 (3 ())))
(print (list 1 (list 2 (list 3 '()))))
; (1 (2 (3 ())))

(print (fold-left list '() (list 1 2 3)))
; (((() 1) 2) 3)
(print (list (list (list '() 1) 2) 3))
; (((() 1) 2) 3)
