(define (cont-frac n d k)
  (define (cf i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (cf (+ i 1))))))
  (cf 1))

(define (calc-e cont-frac-func tolerance)
  (define (calc k)
    (+ 2
       (cont-frac-func (lambda (i) 1.0)
                       (lambda (i)
                         (if (= (remainder i 3) 2)
                             (/ (+ i i 2.0) 3.0)
                             1.0))
                       k)))
  (define (iter k)
    (let ((current (calc k))
          (next (calc (+ k 1))))
      (cond ((< (abs (- current next)) tolerance)
             (print next)
             (print (+ k 1)))
            (else (iter (+ k 1))))))
  (iter 1))

(use math.const)

(print e)
; 2.718281828459045

(calc-e cont-frac 0.0001)
; 2.718279569892473
; 8
