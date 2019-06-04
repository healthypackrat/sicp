(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                ; (print (list this-coeff higher-terms))
                (+ this-coeff (* higher-terms x)))
              0
              coefficient-sequence))

(print (horner-eval 2 (list 1 3 0 5 0 1)))
; 79

; ((((1x + 0)x + 5)x + 0)x + 3)x + 1

; 1 + (3 + (0 + (5 + (0 + 1x)x)x)x)x

; (accumulate op 0 (1 3 0 5 0 1))
; (op 1 (accumulate op 0 (3 0 5 0 1)))
; (op 1 (op 3 (accumulate op 0 (0 5 0 1))))
; (op 1 (op 3 (op 0 (accumulate op 0 (5 0 1)))))
; (op 1 (op 3 (op 0 (op 5 (accumulate op 0 (0 1))))))
; (op 1 (op 3 (op 0 (op 5 (op 0 (accumulate op 0 (1)))))))
; (op 1 (op 3 (op 0 (op 5 (op 0 (op 1 (accumulate op 0 ())))))))
; (op 1 (op 3 (op 0 (op 5 (op 0 (op 1 0))))))
; (op 1 (op 3 (op 0 (op 5 (op 0 1)))))
; (op 1 (op 3 (op 0 (op 5 2))))
; (op 1 (op 3 (op 0 (op 5 2))))
; (op 1 (op 3 (op 0 9)))
; (op 1 (op 3 18))
; (op 1 39)
; 79
