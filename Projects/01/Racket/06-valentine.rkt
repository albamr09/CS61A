#lang racket
(require berkeley)
(require sugar)

; Load function to obtain best score
(require "./01-main-best-total.rkt")

; Valentine strategy that stops at 17 unless you have 
; a heart in your hand, in which case it stops at 19.

; Check whether the hand has heart, no matter the suit
(define (has-heart? hand)
  (cond
    ; Return false is no card was a heart
    ((empty? hand) #f)
    ; If the current card is a heart return true
    ((or 
      (equal? (last (first hand)) 'h)
      (equal? (last (first hand)) 'H)) 
      #t
    )
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

; Exports
(provide valentine)
