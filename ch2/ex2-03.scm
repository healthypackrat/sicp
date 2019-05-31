(define (make-point x y) (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (make-segment p q) (cons p q))

(define (start-segment s) (car s))

(define (end-segment s) (cdr s))

(define (make-rectangle s t) (cons s t))

(define (horizontal-segment r) (car r))

(define (vertical-segment r) (cdr r))

(define (distance s)
  (let ((p (start-segment s))
        (q (end-segment s)))
    (sqrt (+ (square (- (x-point p) (x-point q)))
             (square (- (y-point p) (y-point q)))))))

(define (perimeter r)
  (let ((s (horizontal-segment r))
        (t (vertical-segment r)))
    (* (+ (distance s) (distance t)) 2)))

(define (area r)
  (let ((s (horizontal-segment r))
        (t (vertical-segment r)))
    (* (distance s) (distance t))))

(let ((r (make-rectangle (make-segment (make-point 0 0) (make-point 8 0))
                         (make-segment (make-point 8 0) (make-point 8 4)))))
  (print (perimeter r)) ; 24
  (print (area r))) ; 32
