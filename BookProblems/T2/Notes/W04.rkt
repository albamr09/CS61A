#lang racket
(require berkeley)


; Constructors

; Concatenate rank with first letter of the suit
(define (make-card rank suit)
  (word rank (first suit)) 
)

; Concatenate cards
(define make-hand se)

; Selectors
(define rank butlast)
(define suit last)

; Return last card
(define one-card last)
; Return all but last card
(define remaining-cards butlast)

; Operations
(define (total hand)
  ; If hand is empty
  (if (empty? hand)
    ; Do no sum anything
    0
    (+ 
      ; Get value of last card
      (rank (one-card hand))
      ; Continue iterating though the hand
      ; by removing the last card
      (total (remaining-cards hand))
    )
  )
)

; Obtain score
(total 
  ; Append all cards
  (make-hand 
    ; Create each card
    (make-card 3 'heart)
    (make-card 10 'club)
    (make-card 4 'diamond) 
  )
)
; 17
