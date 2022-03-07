#lang racket
(require berkeley)

; Modify the make-account procedure of Exercise 01 by adding another local state variable so that, if
; an account is accessed more than seven consecutive times with an incorrect password, it invokes the 
; procedure call-the-cops.

(define (make-account balance password)
  ; Initialize counter of wrong passwords
  (define count-incorrect 0)
  (define (print-msg msg)
    (lambda
      (args)
      msg
    )
  )
  ; Ask for password, and return #t or #f where the 
  ; password is correct or incorrect
  (define (check-password pass)
    (lambda
      (procedure-true)
      (if (eq? pass password)
        (begin 
          (set! count-incorrect 0)
          (lambda 
            (args)
            (procedure-true args)
          )
        )
        (begin
          (set! count-incorrect (+ count-incorrect 1))
          (if (>= count-incorrect 7)
            (print-msg "I called the cops")
            (print-msg "Incorrect password")
          )
        )
      )
    )
  )
  ; Augment the amount in the account
  (define (withdraw amount)
    (if (>= balance amount)
      (begin 
        (set! balance (- balance amount))
        balance
      )
      "Insufficient funds"
    )
  )

  ; Reduce the amount in the account
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance
  )

  ; Dispatch one of the methods given
  ; the operation name: "m"
  (define (dispatch m pass)
    ; Check if the account password is correct
    ((check-password pass)
      (cond 
        ; Return one procedure or the other
        ; based on the operation name
        ((eq? m 'withdraw) withdraw)
        ((eq? m 'deposit) deposit)
        (else 
          (error "Unknown request: MAKE-ACCOUNT" m)
        )
      )
    )
  )
  ; Return the procedure dispatch
  ; that handles the message passing
  dispatch ; Value that represents the bank-account object
)

(provide make-account)

(define acc (make-account 100 '1234))
((acc 'withdraw '1) 10)
((acc 'withdraw '1) 10)
((acc 'withdraw '1) 10)
((acc 'withdraw '1) 10)
((acc 'withdraw '1) 10)
((acc 'withdraw '1) 10)
((acc 'withdraw '1) 10)
; I called the cops

((acc 'withdraw '1234) 10)
; Reset count
((acc 'withdraw '1) 10)
((acc 'withdraw '1234) 10)
; Reset count
((acc 'withdraw '1) 10) ; 1
((acc 'withdraw '1) 10) ; 2 
((acc 'withdraw '1) 10) ; 3 
((acc 'withdraw '1) 10) ; 4 
((acc 'withdraw '1) 10) ; 5 
((acc 'withdraw '1) 10) ; 6 
((acc 'withdraw '1) 10) ; 7 
; I called the cops