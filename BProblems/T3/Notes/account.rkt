#lang racket
(require berkeley)

; Load OOP library
(require "../../../lib/obj.rkt" rackunit)

(define-class (account balance)
  ; Add money to account
  (method (deposit amount)
    ; Update the balance
    (set! balance (+ amount balance))
    balance
  )
  ; Substract money from the account
  (method (withdraw amount)
    (if (< balance amount)
      "Insufficient funds"
      (begin
        ; Update the balance
        (set! balance (- balance amount))
        balance
      )
    )
  ) 
)

 (define-class (checking-account init-balance)
    (parent (account init-balance))
    (method 
      (write-check amount)
      (ask self 'withdraw (+ amount 0.10)) 
    )
 )


;; TEST

(define Matt-Account (instantiate account 1000))
(ask Matt-Account 'deposit 100)
