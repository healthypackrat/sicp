(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (cond ((eq? m 'authorize) (eq? p password))
          ((eq? p password)
           (cond ((eq? m 'withdraw) withdraw)
                 ((eq? m 'deposit) deposit)
                 (else (error "Unknown request -- MAKE-ACCOUNT" m))))
          (else
            (lambda _ "Incorrect password"))))
  dispatch)

(define (make-joint account password new-password)
  (define (dispatch p m)
    (if (eq? p new-password)
        (account password m)
        (lambda _ "Incorrect password")))
  (if (account password 'authorize)
      dispatch
      (error "Incorrect password")))

(define (main argv)
  (define peter-acc (make-account 100 'open-sesame))

  (define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))

  (print ((paul-acc 'rosebud 'withdraw) 10))
  ; 90

  (print ((paul-acc 'open-sesame 'withdraw) 10))
  ; Incorrect password

  (print ((peter-acc 'rosebud 'withdraw) 10))
  ; Incorrect password

  0)
