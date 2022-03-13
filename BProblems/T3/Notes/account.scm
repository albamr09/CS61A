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
(ask Matt-Account 'deposit 100)
; 1100
(ask Matt-Account 'withdraw 100)
; 1000

(define Hal-Account (instantiate checking-account 1000))
(ask Hal-Account 'balance)
; 1000
(ask Hal-Account 'deposit 100)
; 1100
(ask Hal-Account 'withdraw 50)
; 1050
(ask Hal-Account 'write-check 30)
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
(ask matt 'work)
; WHISTLE-WHILE-YOU-WORK
(ask matt 'work)
; WHISTLE-WHILE-YOU-WORK
(ask matt 'hunger)
; 2
(ask matt 'work-done)
; 2
(ask brian 'work)
; WHISTLE-WHILE-YOU-WORK
(ask brian 'hunger)
; 1
(ask brian 'work-done)
; 3
(ask worker 'work-done)
; 3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ALL WORKERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class (worker)
  ; Unique to the instance
  (instance-vars (hunger 0))
  ; Shared between all of the instances
  (class-vars 
    (all-workers '())
    (work-done 0)
  )
  ; On instantiation update the list of all workers 
  ; to include this instance
  (initialize (set! all-workers (cons self all-workers)))
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
(ask brian 'all-workers)
; '(BRIAN MATT)
(ask matt 'all-workers)
; '(BRIAN MATT)
(ask worker 'all-workers)
; '(BRIAN MATT)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ECHO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class (echo-previous)
  ; Initiate the first previous message
  (instance-vars (previous-message 'first-time))
  ; Method called when no other method matches the 
  ; message received by the class
  (default-method
    ; Save the previous message in a 
    ; aux variable
    (let ((result previous-message))
      ; Update the previous message to the current one
      (set! previous-message message)
      result
    )
  )
)

(define echo (instantiate echo-previous))
(ask echo 'hi!!)
; first-time
(ask echo 'nice)
; hi!!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class (TA)
  ; This class is a child of worker
  (parent (worker))
  ; Override the work method
  (method (work)
    ; Call the parent's method
    (print (usual 'work))
    ; Functionality of this method
    '(Let me help you with that box and pointer diagram)
  )
  (method (grade-exam) 'A+) 
)

(define beloved_ta (instantiate TA))
(ask beloved_ta 'work)
; WHISTLE-WHILE-YOU-WORK
; Let me help you with that box and pointer diagram

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MULTIPLE SUPERCLASSES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class (singer)
  (parent (worker))
  (method (sing) '(tra-la-la)) 
)

(define-class (singer-TA)
  ; More than one parent
  (parent (singer) (TA)) 
)

(define-class (TA-singer)
  ; More than one parent
  (parent (TA) (singer)) 
)

(define Matt (instantiate singer-TA))
(define Chris (instantiate TA-singer))

(ask Matt 'grade-exam)
; A+
(ask Matt 'sing)
; (TRA-LA-LA)
(ask Matt 'work)
; Matt is mainly a singer, so it inherits the work method from work, instead of from TA
; WHISTLE-WHILE-YOU-WORK
(ask Chris 'work)
; Chris is mainly a TA, so it uses its overriden work method
; WHISTLE-WHILE-YOU-WORK
; (LET ME HELP YOU WITH THAT BOX AND POINTER DIAGRAM)

