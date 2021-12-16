;  valentine strategy that stops at 17 unless you have 
; a heart in your hand, in which case it stops at 19.

; Load function to obtain best score
; (load "./01-main-best-total.scm")

; Check whether the hand has an ace, no matter the suit
(define (has-ace? hand)
  (or
    (member? 'AH hand)
    (member? 'AS hand)
    (member? 'AD hand)
    (member? 'AC hand)
  )
)

(define (valentine customer-hand-so-far dealer-up-card)
  ; If there is a heart in the hand
  (if (has-ace? customer-hand-so-far)
    ; Return whether the total is less than 19
    (< (best-total customer-hand-so-far) 19)
    ; Return whether the total is less than 17
    (< (best-total customer-hand-so-far) 17)
  )
)
