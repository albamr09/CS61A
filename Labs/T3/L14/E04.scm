; Ben Bitdiddle worries that it would be better to implement the bank account as follows (where the
; commented line has been changed):

(define (make-account balance)

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
  
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond 
        ((eq? m 'withdraw) (protected withdraw))
        ((eq? m 'deposit) (protected deposit))
        ((eq? m 'balance)
          ; serialized access to balance
          ((protected
            (lambda () balance))
          )
        ) 
        (else
          (error "Unknown request: MAKE-ACCOUNT" m)
        )
      )
    )
    dispatch
  )
)

; because allowing unserialized access to the bank balance can result in anomalous behavior. Do you agree? Is there
; any scenario that demonstrates Ben’s concern?

; The only concern with the balance variable would be if we wanted to modify it somehow and we were accessing the wrong data. However by following the laws of data abstraction we would not call the 'balance method in order to modify this variable with respect to the class, that is the account. Instead we would use the withdraw or deposit procedure which are indeed serialized. Reading procedures do not produce inconsistencies because they consist (ideally) of only one operation (this is not completely true, because first we have to access the variable to be able to read it).
