(define (make-monitored f)
  (let ((count 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) count)
            ((eq? x 'reset-count)
             (set! count 0)
             count)
            (else
              (set! count (+ count 1))
              (f x))))))

(define (main argv)
  (define s (make-monitored sqrt))

  (print (s 100))
  ; 10

  (print (s 'how-many-calls?))
  ; 1

  (print (s 'reset-count))
  ; 0

  (print (s 4))
  ; 2

  (print (s 9))
  ; 3

  (print (s 16))
  ; 4

  (print (s 'how-many-calls?))
  ; 3

  0)
