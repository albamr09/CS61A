; Strategy where the customes takes a
; card if and only if the total so far is less than 17.

(define (stop-at-17 customer-hand-so-far dealer-up-card)
  ; Return whether the total is less than 17
  (< (best-total customer-hand-so-far) 17)
)
