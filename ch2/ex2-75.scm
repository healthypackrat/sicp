(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
            (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

(use math.const)

(define z (make-from-mag-ang (sqrt 2) (/ pi 4)))

(print (z 'real-part))
; 1.0000000000000002

(print (z 'imag-part))
; 1.0

(print (z 'magnitude))
; 1.4142135623730951

(print (z 'angle))
; 0.7853981633974483
