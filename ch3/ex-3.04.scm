(define (call-the-cops) "call-the-cops")

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define count 0)
  (define (incorrect-password x)
    (set! count (+ count 1))
    (if (>= count 7)
        (call-the-cops)
        "Incorrect password"))
  (define (dispatch p m)
    (if (eq? p password)
        (begin
          (set! count 0)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unknown request -- MAKE-ACCOUNT"
                             m))))
        incorrect-password))
  dispatch)

(define (main argv)
  (define acc (make-account 100 'secret-password))

  (print ((acc 'secret-password 'withdraw) 40))
  ; 60

  (print ((acc 'some-other-password 'deposit) 50))
  ; Incorrect password

  ((acc 'some-other-password 'deposit) 50)
  ((acc 'some-other-password 'deposit) 50)
  ((acc 'some-other-password 'deposit) 50)
  ((acc 'some-other-password 'deposit) 50)
  ((acc 'some-other-password 'deposit) 50)

  (print ((acc 'some-other-password 'deposit) 50))
  ; call-the-cops

  0)
