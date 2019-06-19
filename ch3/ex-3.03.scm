(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (incorrect-password x)
    "Incorrect password")
  (define (dispatch p m)
    (if (eq? p password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        incorrect-password))
  dispatch)

(define (main argv)
  (define acc (make-account 100 'secret-password))

  (print ((acc 'secret-password 'withdraw) 40))
  ; 60

  (print ((acc 'some-other-password 'deposit) 50))
  ; Incorrect password

  0)
