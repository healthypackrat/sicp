(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (matrix-*-vector cols row))
         m)))

(define x '((1 2 3) (4 5 6) (7 8 9)))

(print (matrix-*-vector x '(1 1 1)))
; (6 15 24)

(print (transpose x))
; ((1 4 7) (2 5 8) (3 6 9))

(print (matrix-*-matrix x '((1 0 0) (0 1 0) (0 0 1))))
; ((1 2 3) (4 5 6) (7 8 9))
