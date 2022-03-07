#lang racket
(require berkeley)
(require "E02.scm")
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
  (define (dispatch m pass)
    ; Call check-password with the joint account password
    (account m account-password)
    ; ((joint-account 'check-password pass)
    ;   ; proc-true on E02, pass it the original account password
    ; )
  )
  ; Return the procedure dispatch
  ; that handles the message passing
  dispatch
) 

(define acc (make-account 100 '1234))
(define joint-acc (make-joint acc '1234 '123))

((joint-acc 'withdraw '123) 50)