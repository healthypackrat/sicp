(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

; (print (withdraw 25))
; 75

; (print (withdraw 25))
; 50

; (print (withdraw 60))
; Insufficient funds

; (print (withdraw 15))
; 35

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

; (print (new-withdraw 25))
; 75

; (print (new-withdraw 25))
; 50

; (print (new-withdraw 60))
; Insufficient funds

; (print (new-withdraw 15))
; 35

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

; (print (W1 50))
; 50

; (print (W2 70))
; 30

; (print (W2 40))
; Insufficient funds

; (print (W1 40))
; 10

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define acc (make-account 100))

; (print ((acc 'withdraw) 50))
; 50

; (print ((acc 'withdraw) 60))
; Insufficient funds

; (print ((acc 'deposit) 40))
; 90

; (print ((acc 'withdraw) 60))
; 30
