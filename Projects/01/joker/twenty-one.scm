(load "./01-main-best-total.scm")

(define (twenty-one strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond 
      ; Customer wins if the dealer's cards sum more than 21
      ((> (best-total dealer-hand-so-far) 21) 1)
      ; If the dealer's cards sum less than 17, the dealer takes another card 
	    ((< (best-total dealer-hand-so-far) 17)
	      (play-dealer 
          ; Array of the customer's cards
          customer-hand
          ; Take new card from the deck
	        (se dealer-hand-so-far (first rest-of-deck))
          ; Update the deck removing the card taken by the dealer
	        (bf rest-of-deck)
        )
      )
      ; Dealer wins if the sum of its cards is greater than the customer's and less than 21
	    ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1)
      ; Tie between customer and dealer when their cards sum the same
	    ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0)
      ; Customer wins
	    (else 1))
  )

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond 
      ; Customer looses if the it's cards sum more than 21
      ((> (best-total customer-hand-so-far) 21) -1)
      ; Strategy tells whether the customer keeps playing
	    ((strategy customer-hand-so-far dealer-up-card)
        ; If the customer keeps playing
	      (play-customer 
          ; Take new card and add it to the customer's cards
          (se customer-hand-so-far (first rest-of-deck))
          ; The card the dealer is showing
	        dealer-up-card
          ; Update the deck by removing one card
	        (bf rest-of-deck)
        )
      )
	    (else
	      (play-dealer 
          ; Array of the customer's cards
          customer-hand-so-far
          ; The dealer adds a card to it's array 
	        (se dealer-up-card (first rest-of-deck))
          ; Update the deck by removing the first card
	        (bf rest-of-deck)
        )
      )
    )
  )

  (let 
    ; Create deck with the make-deck function
    ((deck (make-deck)))
    ; The customer starts playing
    (play-customer 
      ; Array of the customer's  cards
      (se (first deck) (first (bf deck)))
      ; The card that the dealer shows
	    (first (bf (bf deck)))
      ; Update the deck by removing the three cards chosen
	    (bf (bf (bf deck)))
    )
  )
)

(define (make-ordered-deck)
  (define (make-suit s)
    ; For every element in the list: A, 2, ...
    ; Append 'H, 'S, 'D or 'C after each element 
    ; to create a hand
    (every 
      (lambda 
        (rank) 
        (word rank s)
      ) 
      '(A 2 3 4 5 6 7 8 9 10 J Q K)
    ) 
  )

  ; Concatenate all of the cards from each hand
  ; created with make-suit
  (se 
    (make-suit 'H) 
    (make-suit 'S) 
    (make-suit 'D) 
    (make-suit 'C)
    ; Add two jokers
    '(J J)
  ) 
)

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
        ; Concatenate
	      (se 
          ; First of the unshuffled deck
          (first in) 
          ; Shuffle again the rest of the cards
          (shuffle (se (bf in) out) (- size 1))
        )
        ; Keep shuffling
	      (move-card 
          ; Update unshuffled cards by removing the first card
          (bf in) 
          ; Update shuffled cards by adding the selected card
          (se (first in) out) 
          ; Shuffle the next card
          (- which 1)
        ) 
      )
    )
    ; If size = 0, nothing to shuffle
    (if (= size 0)
      ; Return the deck
	    deck
      ; Else keep shuffling
    	(move-card 
        ; unshuffled cards
        deck 
        ; shuffled cards
        '()
        ; How many cards to shuffle
        (random size)
      ) 
    )
  )

  (shuffle 
    ; List of all the cards
    (make-ordered-deck) 
    ; Number of cars on the deck
    52
  ) 
)
