#lang racket
(require berkeley)
(require sugar)

; Load function to obtain all the joker combinations
(require "./10-jokers.rkt")

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

; This method checks if any aces were found on the 
; hand, if so it runs the reverse searching
; else it generates the joker combinations and
; obtains the best score
; - hand: most up to date hand
; - best: best score up until now
; - exists-ace: if false no ace is in the hand

(define (check-any-ace hand best exists-ace)
    ; If there was no ace, obtain the best value for the
    ; joker combinations
    (if (equal? exists-ace #f)
      (let
        ((best (generate-joker-helper hand best)))
        ; If best = 0, there was no best less than 21, so return
        ; 22 to loose
        (if (= best 0)
          22
          best
        )
      )
      ; Else do reverse search, but first update 
      ; the score if needed
      (reverse-ace 
        ; Modified hand
        hand 
        ; Seen cards
        '() 
        ; Index to the end to start backwards
        ; searching
        (- (count hand) 1) 
        ; Update best if needed
        (generate-joker-helper hand best)
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
      (reverse-ace
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
        ; Get the best score of all possible 
        ; joker combinations
        (generate-joker-helper
          ; Most up to date hand
          (replace
            hand
            i
            (word 1 (last (item i hand)))
          )
          ; Most up to date best score
          best
        )
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

(define (generate-all-hands hand i best exists-ace)
  (cond
    ((> i (count hand)) 
      ; Check if there was any ace
      ; before searching backwards
      (check-any-ace hand best exists-ace)
    )
    ; Swap aces of the form "Asuit" for 11
    ((is-ace? (item i hand))
      (generate-all-hands 
        ; Update hand by swapping the A for 11
        (replace
          hand
          i
          (word 11 (last (item i hand)))
        )
        ; Update index
        (+ 1 i) 
        ; Save best score until now
        best
        ; Update the boolean of exists ace
        #t
      )
    )
    (else
      ; Keep going forwards through the hand
      (generate-all-hands 
        hand 
        ; Update index
        (+ 1 i) 
        ; Save the best score until now
        best
        ; Save the boolean of exists ace
        exists-ace
      )
    )
  )
)

; Exports 
(provide generate-all-hands)

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ; Test for generating combinations of ace and jokers
; (trace replace-all-jokers)
; (trace generate-all-hands)
; (trace generate-combinations-backwards)
; ; Should return 19
; (generate-all-hands '(8s j) 1 0 #f)
; ; Should return 21
; (generate-all-hands '(as j) 1 0 #f)
; ; Should return 16
; (generate-all-hands '(qh 6d) 1 0 #f)
; ; Should return 21, because is the best combination
; (generate-all-hands '(J as 19s) 1 0 #f)
; ; Should return 22 because there is no combination than returns less than 21
; (generate-all-hands '(J as 20s) 1 0 #f)