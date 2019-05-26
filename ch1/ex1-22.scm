(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

; 現在時刻のマイクロ秒を返す
(define (runtime)
    (use srfi-11)
    (let-values (((a b) (sys-gettimeofday)))
    (+ (* a 1000000) b)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes n count)
  (cond ((= count 0))
        ((prime? n) (timed-prime-test n)
                    (search-for-primes (+ n 1) (- count 1)))
        (else (search-for-primes (+ n 1) count))))

(search-for-primes 1000 3)
(search-for-primes 10000 3)
(search-for-primes 100000 3)
(search-for-primes 1000000 3)

; 1009 *** 11
; 1013 *** 10
; 1019 *** 10
; 10007 *** 30
; 10009 *** 53
; 10037 *** 39
; 100003 *** 112
; 100019 *** 134
; 100043 *** 111
; 1000003 *** 329
; 1000033 *** 330
; 1000037 *** 315

; nが2桁増えると実行時間が1桁増える
