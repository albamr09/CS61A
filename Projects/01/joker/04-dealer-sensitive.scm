; Define a strategy named dealer-sensitive that “hits” (takes a card) if (and only if)
; the dealer has an ace, 7, 8, 9, 10, or picture card showing, and the customer has less than
; 17, or the dealer has a 2, 3, 4, 5, or 6 showing, and the customer has less than 12.

; Load function to obtain best score
(load "./01-main-best-total.scm")

(define (dealer-sensitive customer-hand-so-far dealer-up-card)
  ; Return whether the conditions to draw a new cards are met
  (or
    (and
      ; The dealer has a 7, 8, 9, 10 or picture card
      (member? (bl dealer-up-card) '(7 8 9 10 J Q K A))
      ; is the total score of the cards less than 17
      (< (best-total customer-hand-so-far) 17)
    )
    (and
      ; The dealer has a 2, 3, 4, 5, 6
      (member? (bl dealer-up-card) '(2 3 4 5 6))
      ; is the total score of the cards less than 12
      (< (best-total customer-hand-so-far) 12)
    )
  )
)

