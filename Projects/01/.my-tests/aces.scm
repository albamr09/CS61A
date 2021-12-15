; Load function to obtain best score of a hand
(load "./best-total.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ATTENTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This version generates all possible combinations, so it does not take 
; into account that no matter the order of 11 and 1's the results is the same
; i.e. 
;  (11s 2h 1d), best = 13
; is the same as:
; (1s 2h 11d)
; However in this version we generate both
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Generate all of the possible ace combinations, as an ace can be 
; worth 1 or 11
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Example: (as 2h ad)
; 1. generate-all-hands: 
;   ace-hand = (11s 2h 11d), best = 13
; 2. reverse-ace
;   ace-hand = (11s 2h 1d), best = 13
; 3. generate-all-hands:
;   ace-hand = (11s 2h 1d), best = 14
; 4. reverse-ace
;   ace-hand = (1s)
; 5. generate-all-hands:
;   ace-hand = (1s 2h 11d), best = 14
; 6. reverse-ace:
;   ace-hand = (1s 2h 1d), best = 14
; 7. generate-all-hands:
;   ace-hand = (1s 2h 1d), best 14
; 8. reverse-ace:
;   return 14, there are no 11 left

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Returns true if the card is and ace
(define (is-ace? card)
  (equal? (first card) 'A)
)

; Goes through the hand backwards until there is 
; a 11 card to replace if for a 1
; - hand: most up to date hand
; - ace-hand: copy of hand we are going to modify
; - i: current index, goes from the end to the start
; - best: best score up until now

(define (reverse-ace hand ace-hand i best)
  (cond
    ; If no 11 is found (all of them have been replaced)
    ; Return best overall score
    ((= i 0) best)
    ; Swap aces for 1
    ; On the second we start swapping the before 11-worth aces for 1-worth aces
    ((= (bl (item i hand)) 11)
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

; Goes through the hand and
; 1. In the first pass it replaces all A for 11
; 2. Then it obtains the score
; 3. Then it calls reverse-ace to replace all 11 for 1
; - hand: original hand with aces
; - ace-hand: hand we are going to modify to add 11 or 1 instead of A
; - i: currenti index, goes from the start to the end
; - best: best score up until now

(define (generate-all-hands hand ace-hand i best)
  (cond
    ((= (count hand) (count ace-hand)) 
      ; Get score
      (let
        (
          ; Start the search with 
          ; n = total number of cards 
          ; to get the best score with any number of cards set n = 1
          ; and best = 0
         (total (best-total-helper ace-hand (count hand) 0))
        )
        (if (> total best)
          ; Run the reverse search
          (reverse-ace
            ; Reverse using the modified values
            ace-hand
            ; List to modify (swap 11 for 1)
            ace-hand
            ; Pass last index to go backwards
            (- i 1)
            ; Update best = total
            total
          )
          (reverse-ace
            ; Reverse using the modified values
            ace-hand
            ; List to modify (swap 11 for 1)
            ace-hand
            ; Pass last index to go backwards
            (- i 1)
            ; Do not update best
            best
          )
        )
      )
    )
    ; Swap aces for 11
    ; On the first pass all aces are swapped for 11
    ((is-ace? (item i hand))
      (generate-all-hands 
        hand 
        (se 
          ace-hand 
          ; Append 11 and the suit of the card: 'h', 's', etc.
          (word 11 (last (item i hand)))
        ) 
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
        ; Append the item i of hand to ace-hand
        (se ace-hand (item i hand)) 
        ; Update index
        (+ 1 i) 
        ; Save the best score until now
        best
      )
    )
  )
)
