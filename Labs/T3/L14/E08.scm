; Explain in detail why the deadlock-avoidance method described above, (i.e., the accounts are numbered,
; and each process attempts to acquire the smaller-numbered account first) avoids deadlock in the exchange 
; problem. 

; The deadlock was described as follows:

; Imagine that Peter attempt to exchange a1 with a2 while Paul concurrently attempts to exchange a2 with a1.

; 1. Suppose that Peter’s process reaches the point where it has entered a serialized procedure protecting a1 
; 2. Just after that, Paul’s process enters a serialized procedure protecting a2. 
; 3. Now Peter cannot proceed (to enter a serialized procedure protecting a2) until Paul exits the serialized procedure protecting a2. 
; 4. Similarly, Paul cannot proceed until Peter exits the serialized procedure protecting a1.

; We can say that the main reason there was a deadlock was beacause Paul attempted to access a2 before it accesed a1. Supposing the accounts
; were numbered, then a1 would have and id equal to 1 and a2 an id equal to 2. And if we restrict the access for it to be ordered by the account id,
; then Paul would have to adquire a1 before trying with a2, and Peter would be able to adquire a2 because Paul did not adquire it.

; Rewrite serialized-exchange to incorporate this idea. (You will also need to modify make-account so that each account
; is created with a number, which can be accessed by sending an appropriate message.)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAKE ACCOUNT
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-account balance id)

  (define (withdraw amount)
    (if (>= balance amount)
      (begin 
        (set! 
          balance
          (- balance amount)
        )
        balance
      )
      "Insufficient funds"
    )
  )
  
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance
  )
  
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond 
        ((eq? m 'withdraw) withdraw)
        ((eq? m 'deposit) deposit)
        ((eq? m 'balance) balance) 
        ((eq? m 'serializer) balance-serializer)
        ; Add id to account
        ((eq? m 'id) id) 
        (else
          (error "Unknown request: MAKE-ACCOUNT" m)
        )
      )
    )
    dispatch
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SERIALIZED-EXCHANGE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (serialized-exchange account1 account2)
  (let 
    (
      (serializer1 (account1 'serializer))
      (serializer2 (account2 'serializer))
    )
    (
      (serializer1 
        (serializer2 exchange)
      )
      account1
      account2
    )
  )
)
