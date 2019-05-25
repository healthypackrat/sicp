(use gauche.test)

(define (double n) (+ n n))

(define (halve n) (/ n 2))

(define (fast-product a b)
  (fast-product-iter 0 a b))

(define (fast-product-iter result a b)
  (cond ((= b 0) result)
        ((even? b) (fast-product-iter result (double a) (halve b)))
        (else (fast-product-iter (+ result a) a (- b 1)))))

(test* "0 * 1" 0 (fast-product 0 1))
(test* "1 * 0" 0 (fast-product 1 0))
(test* "1 * 1" 1 (fast-product 1 1))
(test* "2 * 1" 2 (fast-product 2 1))
(test* "2 * 2" 4 (fast-product 2 2))
(test* "2 * 3" 6 (fast-product 2 3))

(test-end)
