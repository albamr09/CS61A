; Write a procedure reckless that takes a strategy as 
; its argument and returns another strategy. This new strategy should
; take one more card than the original would.

(define (reckless strategy)
  (lambda
    ; Formal parameters
    (customer-hand-so-far dealer-up-card)
    ; stand if the old strategy would stand 
    ; on the butlast of the customer’s hand.
    (strategy (bl customer-hand-so-far) dealer-up-card)
  )
)
