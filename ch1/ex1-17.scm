(use gauche.test)

(define (double n) (+ n n))

(define (halve n) (/ n 2))

(define (fast-product a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-product a (halve b))))
        (else (+ a (fast-product a (- b 1))))))

(test* "0 * 1" 0 (fast-product 0 1))
(test* "1 * 0" 0 (fast-product 1 0))
(test* "1 * 1" 1 (fast-product 1 1))
(test* "2 * 1" 2 (fast-product 2 1))
(test* "2 * 2" 4 (fast-product 2 2))
(test* "2 * 3" 6 (fast-product 2 3))

(test-end)
