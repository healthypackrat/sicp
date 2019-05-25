(define (fast-expt b n)
  (fast-expt-iter 1 b n))

(define (fast-expt-iter result b n)
  (cond ((= n 0) result)
        ((even? n) (fast-expt-iter result (square b) (/ n 2)))
        (else (fast-expt-iter (* result b) b (- n 1)))))

(print (fast-expt 2 0))
(print (fast-expt 2 1))
(print (fast-expt 2 2))
(print (fast-expt 2 8))
(print (fast-expt 2 9))
