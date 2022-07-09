#lang racket
(require berkeley)

(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
    (begin 
      ; Update the value of balance
      (set! balance (- balance amount))
      ; Return the value
      balance
    )
    "Insufficient funds"
  )
)

; (withdraw 25)
; ; 75
; (withdraw 25)
; ; 50
; (withdraw 60)
; ; "Insufficient funds"
; (withdraw 15)
; ; 35

;;;;;;;;;;;;;;;;;
; balance as a local variable
;;;;;;;;;;;;;;;;;

(define new-withdraw
  ; Bind balance
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
        (begin 
          (set! balance (- balance amount))
          balance
        )
        "Insufficient funds"
      )
    )
  )
)

; (new-withdraw 25)
; ; 75
; (new-withdraw 25)
; ; 50
; (new-withdraw 60)
; ; "Insufficient funds"
; (new-withdraw 15)
; ; 35

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
      (begin 
        (set! balance (- balance amount))
        balance
      )
      "Insufficient funds"
    )
  )
)

; (define W1 (make-withdraw 100))
; (define W2 (make-withdraw 100))

; (W1 50)
; ; 50
; (W2 70)
; ; 30
; (W2 40)
; ; "Insufficient funds"
; (W1 40)
; ; 10

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACCOUNT WITH MESSAGE PASSING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-account balance)
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
  (define (dispatch m)
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
  ; Return the procedure dispatch
  ; that handles the message passing
  dispatch ; Value that represents the bank-account object
)

(define acc (make-account 100))

; ((acc 'withdraw) 50)
; ; 50
; ((acc 'withdraw) 60)
; ; "Insufficient funds"
; ((acc 'deposit) 40)
; ; 90
; ((acc 'withdraw) 60)
; ; 30

