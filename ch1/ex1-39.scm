(define (cont-frac n d k)
  (define (cf i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (cf (+ i 1))))))
  (cf 1))

(define (print-result calc-func tolerance)
  (define (iter k)
    (let ((current (calc-func k))
          (next (calc-func (+ k 1))))
      (cond ((< (abs (- current next)) tolerance)
             (print next)
             (print (+ k 1)))
            (else (iter (+ k 1))))))
  (iter 1))

(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1)
                   x
                   (- (* x x))))
             (lambda (i)
               (- (* i 2) 1))
             k))

(use math.const)

(print (tan (/ pi 4)))
; 0.9999999999999999

; tan(pi/4)が1.0に近づくことを確認
(print-result (lambda (n) (tan-cf (/ 3.141592 4) n)) 0.0001)
; 0.999999659731547
; 5
