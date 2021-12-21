#lang racket
(require berkeley)
(require sugar)

; Function majority that takes three strategies as arguments and produces a
; strategy as a result, such that the result strategy always decides whether or not to “hit”
; by consulting the three argument strategies, and going with the majority

(define (is-true? strategy customer-hand-so-far dealer-up-card)
  ; Return 1 if the strategy return true, and 0 otherwise
  (if (strategy customer-hand-so-far dealer-up-card)
    1
    0
  )
)

(define (majority first-strategy second-strategy third-strategy)
  (lambda 
    ; Formal parameters
    (customer-hand-so-far dealer-up-card)
    ; Return whether or not the majority (2) of the strategies return true
    (>= 
      (+ 
        ; Sum the results of running each strategy
        (is-true? first-strategy customer-hand-so-far dealer-up-card) 
        (is-true? second-strategy customer-hand-so-far dealer-up-card)
        (is-true? third-strategy customer-hand-so-far dealer-up-card)
      )
      2
    )
  )
)

; Exports
(provide majority)
