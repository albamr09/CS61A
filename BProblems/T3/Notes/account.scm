; Load OOP library
(load "../../../lib/obj.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BANCK ACCOUNT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  ; Initialize parent class
  (parent (account init-balance))
  ; Local variable initialized inside the class
  (instance-vars (check-fee 0.10))
  ; Write a check for the given amount and pay a fee
  (method (write-check amount)
    ; Remove an additional check fee from the amount
    (ask self 'withdraw (+ amount check-fee)) 
  )
  ; Update the check fee
  (method (set-fee! fee)
    (set! check-fee fee)
  )
)

;; TEST

(define Matt-Account (instantiate account 1000))
(print (ask Matt-Account 'deposit 100))
; 1100
(print (ask Matt-Account 'withdraw 100))
; 1000

(define Hal-Account (instantiate checking-account 1000))
(print (ask Hal-Account 'balance))
; 1000
(print (ask Hal-Account 'deposit 100))
; 1100
(print (ask Hal-Account 'withdraw 50))
; 1050
(print (ask Hal-Account 'write-check 30))
; 1019.9

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WORKERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class (worker)
  ; Unique to the instance
  (instance-vars (hunger 0))
  ; Shared between all of the instances
  (class-vars (work-done 0))
  (method (work)
    ; Update the hunger
    (set! hunger (+ hunger 1))
    ; Update the work accomplished
    (set! work-done (+ work-done 1))
    'whistle-while-you-work 
  )
)

(define brian (instantiate worker))
; BRIAN
(define matt (instantiate worker))
; MATT
(print (ask matt 'work))
; WHISTLE-WHILE-YOU-WORK
(print (ask matt 'work))
; WHISTLE-WHILE-YOU-WORK
(print (ask matt 'hunger))
; 2
(print (ask matt 'work-done))
; 2
(print (ask brian 'work))
; WHISTLE-WHILE-YOU-WORK
(print (ask brian 'hunger))
; 1
(print (ask brian 'work-done))
; 3
(print (ask worker 'work-done))
; 3
