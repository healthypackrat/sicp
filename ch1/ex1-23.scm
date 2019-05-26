(define (smallest-divisor n)
  (find-divisor n 2))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

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

; 1009 *** 9
; 1013 *** 7
; 1019 *** 7
; 10007 *** 27
; 10009 *** 23
; 10037 *** 19
; 100003 *** 76
; 100019 *** 60
; 100043 *** 58
; 1000003 *** 253
; 1000033 *** 203
; 1000037 *** 198
