(define (filtered-accumulate pred combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((pred a) (combiner (term a)
                            (filtered-accumulate pred combiner null-value term (next a) next b)))
        (else (filtered-accumulate pred combiner null-value term (next a) next b))))

(use math.prime)

(define (prime? n)
  (small-prime? n))

(define (inc n) (+ n 1))

(define (sum-of-squares-of-primes a b)
  (filtered-accumulate prime? + 0 square a inc b))

(print (sum-of-squares-of-primes 1 100))

(define (product-of-mutual-primes n)
  (define (pred i) (= (gcd i n) 1))
  (filtered-accumulate pred * 1 identity 1 inc (- n 1)))

(print (product-of-mutual-primes 10))
