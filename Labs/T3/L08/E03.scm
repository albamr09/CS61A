(load "E02.scm")
; Consider the bank account objects created by make-account, with the password modification described
; in Exercise 01. Suppose that our banking system requires the ability to make joint accounts. Define a 
; procedure make-joint that accomplishes this make-joint should take three arguments. 

; - The first is a password-protected account. 
; - The second argument must match the password with which the account was defined in order for the make-joint 
; operation to proceed. 
; - The third argument is a new password make-joint is to create an additional access to the original account using 
; the new password. 

; For example, if peter-acc is a bank account with password open-sesame, then

; (define paul-acc
;   (make-joint peter-acc 'open-sesame 'rosebud)
; )

; will allow one to make transactions on peter-acc using the name paul-acc and the password rosebud. 
; You may wish to modify your solution to Exercise 01 to accommodate this new feature.

(define (make-joint account account-password joint-password)
  (define joint-account (make-account 0 joint-password))
  ; Dispatch one of the methods given
  ; the operation name: "m"
  ; (define (dispatch m pass)
  ;   ((joint-account 'joint pass)
  ;     (account m account-password)
  ;   )
  ; )
  (define (dispatch m pass)
    (let
      ((result (joint-account 'check-pass pass)))
      (cond
        ((equal? (result '()) "Incorrect password")
          result
        )
        ((equal? (result '()) "I called the cops")
          result
        )
        (else
          (account m account-password)
        )
      )
    )
  )
  dispatch
) 

; (define acc (make-account 150 '1234))
; (define joint-acc (make-joint acc '1234 '123))
; 
; ((joint-acc 'withdraw '12) 50)
; ((joint-acc 'withdraw '12) 50)
; ((joint-acc 'withdraw '12) 50)
; ((joint-acc 'withdraw '12) 50)
; ((joint-acc 'withdraw '12) 50)
; ((joint-acc 'withdraw '12) 50)
; ((joint-acc 'withdraw '12) 50)
; ; I called the cops
; (acc 'balance '1234)
; ; 150
; ((joint-acc 'withdraw '123) 50)
; ; 100
; (acc 'balance '1234)
; ; 100
; ((joint-acc 'withdraw '123) 50)
; (joint-acc 'balance '123)
; ; 50
; (acc 'balance '1234)
; ; 50
