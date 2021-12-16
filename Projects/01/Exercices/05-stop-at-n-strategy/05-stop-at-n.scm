; Function stop-at. (stop-at n) should return a
; strategy that keeps hitting until a hand’s total is n or more

; Load function to obtain best score
; (load "./01-main-best-total.scm")

(define (stop-at n)
  (lambda
    ; Arguments to pass to the anonymous function
    (customer-hand-so-far dealer-up-card)
    ; Return whether the total is less than n
    (< (best-total customer-hand-so-far) n)
  )
)
