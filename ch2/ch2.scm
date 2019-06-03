; 2.1.1 例: 有理数の算術演算

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))

; (print-rat one-half)
; 1/2

(define one-third (make-rat 1 3))

; (print-rat (add-rat one-half one-third))
; 5/6

; (print-rat (mul-rat one-half one-third))
; 1/6

; (print-rat (add-rat one-third one-third))
; 6/9

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

; (print-rat (add-rat one-third one-third))
; 2/3

; 2.2.1 並びの表現

; リスト演算

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

; (print (list-ref squares 3))
; 16

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))

; (print (length odds))
; 4

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

; (print (length odds))
; 4

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

; (print (append squares odds))
; (1 4 9 16 25 1 3 5 7)

; (print (append odds squares))
; (1 3 5 7 1 4 9 16 25)

; リストの写像

(define (scale-list items factor)
  (if (null? items)
      '()
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

; (print (scale-list (list 1 2 3 4 5) 10))
; (10 20 30 40 50)

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

; (print (map abs (list -10 2.5 -11.6 17)))
; (10 2.5 11.6 17)

; (print (map (lambda (x) (* x x)) (list 1 2 3 4)))
; (1 4 9 16)

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

; (print (scale-list (list 1 2 3 4 5) 10))
; (10 20 30 40 50)
