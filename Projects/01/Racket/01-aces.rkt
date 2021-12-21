#lang racket
(require berkeley)
(require sugar)

; Load function to obtain best score of a hand
(require "./01-best-total.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Generate all of the possible ace combinations, as an ace can be 
; worth 1 or 11. Remember the order of the 11 and 1's do not matter, 
; only the number of 11 and 1's present matter
; i.e. we do not calculate (11h 2h 1d) and (1d 2h 11h), only one of them
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Example: (as 2h ad)
; 1. generate-all-hands: 
;   ace-hand = (11s 2h 11d), best = 13
; 2. reverse-ace
;   ace-hand = (11s 2h 1d), best = 14
; 3. reverse-ace
;   ace-hand = (1s 2h 1d), best 14
; 4. reverse-ace:
;   return 14, there are no 11 left

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Returns true if the card is and ace
(define (is-ace? card)
  (or (equal? (first card) 'A) (equal? (first card) 'a))
)

; Replaces a value on a concrete index in the list lst
(define (replace lst index value)
  (se
    (sublist lst 0 (- index 1))
    value
    (sublist lst index (count lst))
  )
)

; Obtains the score of hand and it continues going 
; backwards through the hand
; - hand: most up to date hand
; - seen-hand: cards we have already seen
; - i: current index
; - best: best score up until now

(define (get-best-score hand seen-hand i best)
  (let
    (
      ; Start the search with 
      ; n = total number of cards 
      ; to get the best score with any number of cards set n = 1
      ; and best = 0
     (total (best-total-helper hand (count hand) 0))
    )
    (if (> total best)
      ; Run the reverse search
      (reverse-ace
        ; modified hand
        hand
        ; cards we have already seen/modified
        seen-hand
        ; Pass last index to go backwards
        (- i 1)
        ; Update best = total
        total
      )
      (reverse-ace
        ; modified hand
        hand
        ; cards we have already seen/modified
        seen-hand
        ; Pass last index to go backwards
        (- i 1)
        ; Do not update best
        best
      )
    )
  )
)

; Goes through the hand backwards until there is 
; a 11 card to replace if for a 1
; - hand: most up to date hand
; - seen-hand: cards we have already seen
; - i: current index, goes from the end to the start
; - best: best score up until now

(define (reverse-ace hand seen-hand i best)
  (cond
    ; If no 11 is found (all of them have been replaced)
    ; Return best overall score
    ((<= i 0) 
      ; If best = 0, then there was no best 
      ; score less than 21, return 22 so the player looses
      (if (= best 0) 
        22
       ; else return best
        best
      )
    )
    ; Swap 11 cards for 1 cards
    ((equal? (bl (item i hand)) 11)
      ; Once we swap the cards we update the best score if needed
      (get-best-score 
        ; Swap cards to update the hand
        (replace
          hand
          i
          (word 1 (last (item i hand)))
        )
        ; Cards we have already seen/modified
        seen-hand
        ; Keep generating from the next index
        i 
        ; Save the best score until now
        best
      )
    )
    ; Keep searching for first 11-ace
    (else
      (reverse-ace 
        ; Most up to date hand
        hand
        ; Save i element that is not 11
        (se (item i hand) seen-hand)
        ; Continue going backwards
        (- i 1) 
        ; Save the best score until now
        best
      )
    )
  )
)

; Goes through the hand and
; 1. In the first pass it replaces all A for 11
; 2. Then it obtains the score
; 3. Then it calls reverse-ace to replace all 11 for 1
; - hand: cards to be modified
; - seen-hand: cards we have already seen/modified
; - i: current index, goes from the start to the end
; - best: best score up until now

(define (generate-all-hands hand seen-hand i best)
  (cond
    ((> i (count hand)) 
      ; Get score
      (get-best-score 
        ; Modified hand
        hand
        ; Cards we have already seen/modified
        seen-hand
        ; Current index
        i
        ; Save best score
        best
      )
    )
    ; Swap aces of the form 'A'suit for 11
    ((is-ace? (item i hand))
      (generate-all-hands 
        ; Update hand by swapping the A for 11
        (replace
          hand
          i
          (word 11 (last (item i hand)))
        )
        ; Cards we have already seen/modified
        seen-hand
        ; Update index
        (+ 1 i) 
        ; Save best score until now
        best
      )
    )
    (else
      ; Keep going forwards through the hand
      (generate-all-hands 
        hand 
        seen-hand
        ; Update index
        (+ 1 i) 
        ; Save the best score until now
        best
      )
    )
  )
)

; (trace generate-all-hands)
; (trace get-best-score)

; Exports
(provide generate-all-hands)
