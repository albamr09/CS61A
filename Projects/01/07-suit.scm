; Function suit-strategy that takes three arguments: a suit (h, s, d, or c), 
; a strategy to be used if your hand doesn't include that suit, and a strategy 
; to be used if your hand does include that suit. It should return a strategy
; that behaves accordingly

(define (has-suit hand suit)
  (cond
    ; If no card of the suit has been false return false
    ((empty? hand) #f)
    ; If the current card is of the suit return true
    ((equal? (last (first hand)) suit) #t)
    ; Keep analyzing the hand by deleting the first card
    (else (has-suit (bf hand) suit))
  )
)

(define (suit-strategy suit strategy-include strategy-exclude)
  (lambda 
    ; Formal parameters
    (customer-hand-so-far dealer-up-card)
    ; If the customer's hand has the suit
    (if (has-suit customer-hand-so-far suit)
      ; Execute the given strategy
      (strategy-include customer-hand-so-far dealer-up-card)
      ; Execute the alternative strategy
      (strategy-exclude customer-hand-so-far dealer-up-card)
    )
  )
)


