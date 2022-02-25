; Load OOP
(load "../../../lib/obj.scm")

; We are going to use objects to represent decks of cards. You are given the list ordered-deck 
; containing 52 cards in standard order.


; You are also given a function to shuffle the elements of a list:

(define (shuffle deck)
  (if (null? deck)
    '()
    (let 
      ((card (list-ref deck (random (length deck)))))
      (cons card (shuffle (delete card deck))) 
    )
  )
)

; A deck object responds to two messages: deal and empty?. It responds to deal by 
; returning the top card of the deck, after removing that card from the deck; if the 
; deck is empty, it responds to deal by returning (). It responds to empty? by returning 
; #t or #f, according to whether all cards have been dealt. Write a class definition for 
; deck. When instantiated, a deck object should contain a shuffled deck of 52 cards.

(define-class (deck cards)
  ; Shuffle the cards when the object is created
  (initialize 
    (set! cards (shuffle cards))
  )
  (method (deal)
    ; If the deck is not emtpy
    (if (ask self 'empty?)
      "The deck is empty"  
      ; Select the first card on the deck
      (begin
        (let
          ; Make a copy of the deck
          ((original-deck cards))
          ; Update the deck
          (set! cards (cdr cards))
          ; Return the first card of the deck
          (car original-deck)
        )
      )
    )
  )
  (method (empty?)
    ; Check if the list of 
    ; cards is empty
    (empty? cards)
  )
)

(define ordered-deck '(AH 2H 3H QH KH AS 2S QC KC))
(define test-deck (instantiate deck ordered-deck))
(ask test-deck 'empty?)
; #f
(ask test-deck 'deal)
(ask test-deck 'deal)
(ask test-deck 'deal)
(ask test-deck 'deal)
(ask test-deck 'deal)
(ask test-deck 'deal)
(ask test-deck 'deal)
(ask test-deck 'deal)
(ask test-deck 'deal)
(ask test-deck 'empty?)
; t
(ask test-deck 'deal)
; The deck is empty
