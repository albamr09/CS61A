; Load function to obtain best score of a hand
(load "./01-best-total.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Generate all of the possible joker combinations, as an joker can be 
; worth from 1 to 11. Remember the order of the values, 
; only the number of them
; i.e. we do not calculate (1J 2J) and (2J 1J), only one of them
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Example of the generation of the combinations, given a list of the form
; (J J). First we generate the starting input (1J 1J), so the value of 
; both of the jokers is 1. Now, suppose the maximum value of a joker is 3
; we proceed as follows:
; 1. (1J 1J)
; 2. (1J 2J)
; 3. (1J 3J)
;--
; 4. (2J 2J)
; 5. (2J 3J)
;--
; 6. (3J 3J)
; And each time a combination is generated, we calculated its score

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Suppose the maximum value of a joker is 2 instead of 11
; Example: (J 3s J)
; 1. replace-all-jokers, value = 1
;   hand = (1j 3s 1j)
; 2. generate-combinations-backwards
;   hand = (1j 3s 2j), best = 5
; 3. find-joker, i = 2, best = 6
;   hand = (1j 3s 2j)
; 4. find-joker, i = 1, best = 6
;   hand = (1j 3s 2j)
; 5. replace-all-jokers, value = 2
;   hand = (2j 3s 2j)
; 6. generate-combinations-backwards
;   hand = (2j 3s 2j), best = 6
; 7. find-joker, i = 2, best = 7
;   hand = (2j 3s 2j)
; 8. find-joker, i = 1, best = 7
;   hand = (2j 3s 2j)
; 9. find-joker, i = 0, best = 7
;   hand = (2j 3s 2j)
; Return best = 7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Returns true if the card is an joker
(define (is-joker? card)
  (equal? (last card) 'J)
)

; Replaces a value on a concrete index in the list lst
(define (replace lst index value)
  (se
    (sublist lst 0 (- index 1))
    value
    (sublist lst index (count lst))
  )
)

; The modified joker's card is of the form 5j
; where 5 is the value, because it can be 11j
; we do not use the 'first' method as it would 
; select only 1
(define (get-value-joker card)
  ; So we select the character before the last
  ; character
  (bl card)
)

; When searching backwards, if we find a joker whose value < 11
; we have to set all of the jokers after to its value plus one
; and generate all of the combinations, whose value is equal or greater
; to it
; - hand: most up to date hand
; - i: current index
; - best: best score up until now

(define (find-joker hand i best)
  (cond 
    ; If i = 0, the were no jokers left to generate combinations from
    ((= i 0) best)
    ; If it is joker and its value is less than 11, generate all of the combinations from
    ; this point on
    ((and (is-joker? (item i hand)) (< (get-value-joker (item i hand)) 11))
      (generate-combinations-backwards 
        ; Modifify the hand so all of the jokers, from this joker found on, 
        ; have the same value as the joker found + 1
        (se
           ; Concatenate the cards before the joker and the cards
           ; after the joker found (itself included)
           (sublist hand 0 (- i 1))
           ; Change all the values of the jokers afther the joker
           ; found (itself included)
           (replace-all-jokers 
             ; List that is modified
             (sublist hand (- i 1) (count hand)) 
             ; Starting index in the sublist
             1 
             ; Value to replace with: value of joker found + 1
             (+ 1 (get-value-joker (item i hand)))
           )
        )
        ; Start from the end generating backwards
        (count hand) 
        ; Save best score until now
        best
      )
    )
    ; Else keep searching
    (else 
      (find-joker 
        hand 
        ; Update index
        (- i 1) 
        ; Save best score
        best
      )
    )
  )
)

(define (update-best hand i best)
  (let
    ; Start the search with 
    ; n = total number of cards 
    ; to get the best score with any number of cards set n = 1
    ; and best = 0
    ((total (best-total-helper hand (count hand) 0)))
    ; If total > best, return total and update best = total
    (if (> total best)
      total
      ; Else do not update best
      best
    )
  )
)

; This method updates the value of a joker until its value
; reached the maximum, then it calls the find-joker to 
; search backwards for a new joker
; - hand: most up to date hand
; - i: current index
; - best: best score up until now

(define (generate-combinations-backwards hand i best)
  (cond
    ; If we have reached the start of the hand then
    ; no jokers were found on the hand
    ; so return the best score without obtaining any combination
    ((= i 0) (best-total-helper hand (count hand) 0))
    ; If not all values for this joker's index were generated
    ((is-joker? (item i hand))
      ; If the value of the joker is less then 11
      (if (< (get-value-joker (item i hand)) 11)
        ; Add one to the joker, until reaching the max
        (generate-combinations-backwards 
          (replace 
            ; List
            hand 
            ; Index
            i 
            ; New value
            (word
              ; Update it by adding one
              (+ (get-value-joker (item i hand)) 1)
              'J
            )
          )
          ; Same index, because we update the same joker
          i
          ; Because a new hand is generated every time
          ; we have to always check if we need to update
          ; the score
          (update-best hand i best)
        )
        ; else find joker, update the best if necessary
        ; if the joker's value is 11, we have to
        ; check if best needs updating here
        (find-joker hand (- i 1) (update-best hand i best))
      )
    )
    ; If it is not a joker continue going backwards
    (else 
      (generate-combinations-backwards hand (- i 1) best)
    )
  )
)

; This method replaces all of the jokers' in the hand values
; to the value passed as argument
; - hand: most up to date hand
; - i: current index
; - value: new value for the jokers

(define (replace-all-jokers hand i value)
  (cond
    ; If we have reached the end of the hand, 
    ; return the modified hand
    ((< (count hand) i) 
      hand
    )
    ; If we find a joker, replace its value for the
    ; new value
    ((is-joker? (item i hand))
      (replace-all-jokers
        ; Replace the value at index i
        (replace 
          hand 
          i 
          ; New value
          (word value 'J)
        )
        ; Update index
        (+ i 1)
        ; Value to use for replacing
        value
      )
    )
    (else
      ; Keep going forwards through the hand
      (replace-all-jokers
        hand
        ; Update index
        (+ i 1)
        ; Value to use for replacing
        value
      )
    )
  )
)

(define (generate-joker-helper hand best)
  (generate-combinations-backwards
    ; Pass as starting input all the jokers
    ; with the value = 1
    (replace-all-jokers hand 1 1)
    ; Start from the end
    (count hand)
    ; Save the best score
    best
  )
)

(define (generate-joker hand)
  (generate-joker-helper hand 0)
)

