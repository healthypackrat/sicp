(use srfi-27)

(random-source-randomize! default-random-source)

(define (random range)
  (* (random-real) range))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (test)
    (P (random-in-range x1 x2) (random-in-range y1 y2)))
  (let ((area (* (- x2 x1) (- y2 y1))))
    (* (monte-carlo trials test) area)))

(define (main argv)
  (use math.const)

  (print (* 3 3 pi))
  ; 28.274333882308138

  (print (inexact (estimate-integral
                    (lambda (x y) (<= (+ (square (- x 5)) (square (- y 7))) (square 3)))
                    2 8 4 10 100000)))

  (print (inexact (estimate-integral
                    (lambda (x y) (<= (+ (square x) (square y)) (square 1)))
                    -1 1 -1 1 100000)))
  0)
