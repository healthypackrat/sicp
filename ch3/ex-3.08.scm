(define (make-f)
  (let ((evaluated #f))
    (lambda (x)
      (if evaluated
          0
          (begin
            (set! evaluated #t)
            x)))))

(define (main argv)
  (define f1 (make-f))
  (define f2 (make-f))

  (let* ((l (f1 0))
         (r (f1 1)))
    (print (+ l r)))

  (let* ((r (f2 1))
         (l (f2 0)))
    (print (+ l r)))

  0)
