;  valentine strategy that stops at 17 unless you have 
; a heart in your hand, in which case it stops at 19.

; Load function to obtain best score
(load "./01-main-best-total.scm")

; Check whether the hand has heart, no matter the suit
(define (has-heart? hand)
  (cond
    ; Return false is no card was a heart
    ((empty? hand) #f)
    ; If the current card is a heart return true
    ((equal? (last (first hand)) 'h) #t)
    ; Else keep searching
    (else (has-heart? (bf hand)))
  )
)

(define (valentine customer-hand-so-far dealer-up-card)
  ; If there is a heart in the hand
  (if (has-heart? customer-hand-so-far)
    ; Return whether the total is less than 19
    (< (best-total customer-hand-so-far) 19)
    ; Return whether the total is less than 17
    (< (best-total customer-hand-so-far) 17)
  )
)
