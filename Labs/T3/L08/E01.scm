; Modify the make-account procedure so that it creates password-protected accounts. 
; That is, make-account should take a symbol as an additional argument, as in

; (define acc (make-account 100 'secret-password))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACCOUNT WITH MESSAGE PASSING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-account balance password)
  ; Ask for password, and return #t of #f if the 
  ; password is correct
  (define (check-password)
    (display "Enter password: ")
    (let
      ((pass (read)))
      (eq? pass password)
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
  (define (dispatch m)
    ; Check if the account password is correct
    (if (check-password)
      (cond 
        ; Return one procedure or the other
        ; based on the operation name
        ((eq? m 'withdraw) withdraw)
        ((eq? m 'deposit) deposit)
        (else 
          (error "Unknown request: MAKE-ACCOUNT" m)
        )
      )
      "Incorrect password"
    )
  )
  ; Return the procedure dispatch
  ; that handles the message passing
  dispatch ; Value that represents the bank-account object
)

; Do it on the interpreter, to read from stinput the password
; (define acc (make-account 100 'secret-password))
; ((acc 'withdraw) 50)
; ((acc 'deposit) 150)
; ((acc 'withdraw) 550)
