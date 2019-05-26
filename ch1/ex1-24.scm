(use srfi-27)

(define (random n)
  (random-source-randomize! default-random-source)
  (random-integer n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

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
  (if (fast-prime? n 1)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes n count)
  (cond ((= count 0))
        ((fast-prime? n 1) (timed-prime-test n)
                    (search-for-primes (+ n 1) (- count 1)))
        (else (search-for-primes (+ n 1) count))))

(search-for-primes 1000 3)
(search-for-primes 10000 3)
(search-for-primes 100000 3)
(search-for-primes 1000000 3)

; 1009 *** 13
; 1013 *** 11
; 1019 *** 12
; 10007 *** 28
; 10009 *** 18
; 10037 *** 17
; 100003 *** 20
; 100019 *** 19
; 100043 *** 14
; 1000003 *** 21
; 1000033 *** 15
; 1000037 *** 24
