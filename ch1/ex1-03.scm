(use gauche.test)

(define (sum-of-squares-of-biggers x y z)
  (cond ((and (>= y x) (>= z x)) (+ (square y) (square z)))
        ((and (>= z y) (>= x y)) (+ (square z) (square x)))
        ((and (>= x z) (>= y z)) (+ (square x) (square y)))))

(test* "y and z" 25 (sum-of-squares-of-biggers 2 3 4))
(test* "z and x" 25 (sum-of-squares-of-biggers 4 2 3))
(test* "x and y" 25 (sum-of-squares-of-biggers 3 4 2))

(test-end)
