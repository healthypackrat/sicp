; from ch3support.scm
(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

(define random-init 7)

(define rand
  (let ((x random-init))
    (define (generate)
      (set! x (rand-update x))
      x)
    (define (reset new-value)
      (set! x new-value)
      x)
    (define (dispatch m)
      (cond ((eq? m 'generate)
             (generate))
            ((eq? m 'reset)
             reset)
            (else
              (error "Unknown message -- RAND:" m))))
    dispatch))

(define (main argv)
  (print (rand 'generate))
  ; 88
  (print (rand 'generate))
  ; 116

  (print ((rand 'reset) random-init))
  ; 7

  (print (rand 'generate))
  ; 88
  (print (rand 'generate))
  ; 116

  0)
