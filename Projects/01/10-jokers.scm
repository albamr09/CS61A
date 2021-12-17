; Returns true if the card is an joker
(define (is-joker? card)
  (equal? (bl card) 'J)
)

(define (has-joker? hand)
  (define (has-joker-helper hand index) 
    (cond
      ((empty? hand) 0)
      ((is-joker? (first hand)) index)
      (else (has-joker-helper (bf hand) (+ index 1)))
    )
  )
  (has-joker-helper hand 1)
)

(define (try-jokers hand)
  (let
    (
     (joker-index (has-joker? hand))
    )
    (if (= joker-index 0)
      ; Obtain the total normally
      (best-total-helper hand (count hand) 0)
    )
  )
)


(define (generate-combinations hand comb-hand i number start-number)
  (cond
    (
      (and 
        (= (count hand) (count comb-hand)) 
        (= numer 12)
      )
    )
    ((= number 12) (se (generate-combinations hand i 1 (+ start-number 1))))
    (else )
  )
)


(define (replace-all-jokers hand comb-hand i)
  (cond
    ((= (count hand) (count ace-hand))
    )
    ; On the first pass all aces are swapped for 1
    ((is-joker? (item i hand))
      (generate-all-hands
        hand
        (se
          ace-hand
          ; Append 1, J (for joker) and the suit of the card: 'h', 's', etc.
          (word 1 J (last (item i hand)))
        )
        ; Update index
        (+ i 1)
      )
    )
    (else
      ; Keep going forwards through the hand
      (generate-all-hands
        hand
        ; Append the item i of hand to ace-hand
        (se ace-hand (item i hand))
        ; Update index
        (+ i 1)
      )
    )
  )
)

; Goes through the hand backwards until there is
; a 11 card to replace if for a 1
; - hand: most up to date hand
; - ace-hand: copy of hand we are going to modify
; - i: current index, goes from the end to the start
; - best: best score up until now

(define (reverse-ace hand ace-hand i)
  (cond
    ; If no 11 is found (all of them have been replaced)
    ; Return best overall score
    ((= i 0))
    ; Swap aces for 1
    ; On the second we start swapping the before 11-worth aces for 1-worth aces
    ((= (bl (item i hand)) 'J)
      ; Call generate-all-hands to generate all of the combinations from this position
      (generate-all-hands
        ; modified hand with 11 and no 'a'
        hand
        ; Remove the 11 and
        ; Append 1 to the new hand
        (se
          (bl ace-hand)
          ; Append 1 to the suit of the card: 'h', 's', etc.
          (word 1 (last (item i hand)))
        )
        ; Keep generating from the next index
        (+ 1 i)
        ; Save the best score until now
        best
      )
    )
    ; Keep searching for first 11-ace
    (else
      (reverse-ace
        hand
        ; Remove the element
        (bl ace-hand)
        ; Continue going backwards
        (- i 1)
        ; Save the best score until now
        best
      )
    )
  )
)

