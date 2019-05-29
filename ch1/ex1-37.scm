; 再帰的プロセス版

(define (cont-frac n d k)
  (define (cf i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (cf (+ i 1))))))
  (cf 1))

; 反復的プロセス版

(define (cont-frac-iter n d k)
  (define (cf result i)
    (if (= i 0)
        result
        (cf (/ (n i) (+ (d i) result)) (- i 1))))
  (cf (/ (n k) (d k)) (- k 1)))

; 差が許容値以下になるまで計算して、結果と計算回数を表示

(define (calc-phi cont-frac-func tolerance)
  (define (calc k)
    (cont-frac-func (lambda (i) 1.0)
                    (lambda (i) 1.0)
                    k))
  (define (iter k)
    (let ((current (calc k))
          (next (calc (+ k 1))))
      (cond ((< (abs (- current next)) tolerance)
             (print next)
             (print (+ k 1)))
            (else (iter (+ k 1))))))
  (iter 1))

(define phi (/ (+ 1 (sqrt 5)) 2))

(print (/ 1 phi))
; 0.6180339887498948

(calc-phi cont-frac 0.0001)
; 0.6180555555555556
; 11

(calc-phi cont-frac-iter 0.0001)
; 0.6180555555555556
; 11
