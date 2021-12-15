;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Obtain the best score less than 21 from all the possible combinations
; of all possible number of cards from the hand
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Get the score of a given card
(define (value-card card)
  (cond
    ; If it is a number return the number
    ; All is a number except the last letter
    ((number? (bl card)) (bl card)) 
    ; The K, Q, J are worth 10
    (else 10)
  )
)

; Sum all of the scores of the elements of the hand or subhand
(define (sum-list hand acc)
  (cond
    ((= (count hand) 0) acc)
    (else 
      (sum-list 
        (bf hand) 
        (+ acc (value-card (first hand)))
      )
    )
  )
)

(define (combination-n-from-k hand n k i best)
  (cond
    ; Prevent overflow of the index
    ; if n + offset is > the size of the list return all the combinations
    ; up until this point
    ((< (count hand) (+ n (- i 1))) 
     best
    )
    (else 
      (let
        (
         (total 
            (sum-list 
              (se 
                ; First item in the list
                (item k hand) 
                ; Sublist from offset i, to n + (i - 1)
                ; where n is the size of the subset
                ; and i is the offset
                (sublist 
                  hand 
                  i 
                  (+ (- i 1) n)
                )
              )
              0
            )
          )
        ) 
        (if (and (<= total 21) (> total best))
          ; If the total =< 21 and > best, then update best
          (combination-n-from-k
            ; Complete list
            hand
            ; Size of combination
            n
            ; Where to start in the list
            k
            ; Offset to append, 1 refers to second element in the list
            (+ i 1)
            ; Update the best value to the total
            total
          )
          ; If the total > 21 or <= best, then do not update best
          (combination-n-from-k
            ; Complete list
            hand
            ; Size of combination
            n
            ; Where to start in the list
            k
            ; Offset to append, 1 refers to second element in the list
            (+ i 1)
            ; Update the best value to the total
            best
          )
        )
      )
    )
  )
)

(define (best-total-helper hand n best)
  (if (< (count hand) n)
    ; If all the sizes have been tried return the best sum less or equal to 21
    best
    ; Else keep searching for the best sum
    (best-total-helper
      hand
      ; Update size n
      (+ 1 n)
      ; Obtain the combination with n elements and the best value, not greater than 21
      (combination-n-from-k hand n 1 1 best)
    )
  )
)
