(define (make-point x y) (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (make-segment p q) (cons p q))

(define (start-segment s) (car s))

(define (end-segment s) (cdr s))

(define (midpoint-segment s)
  (let ((p (start-segment s))
        (q (end-segment s)))
    (let ((x (/ (+ (x-point p) (x-point q)) 2))
          (y (/ (+ (y-point p) (y-point q)) 2)))
      (make-point x y))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(let ((p (make-point 2 4))
      (q (make-point 4 8)))
  (let ((s (make-segment p q)))
    (print-point (midpoint-segment s))))
; (3,6)
