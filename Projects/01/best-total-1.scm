; The procedure should return the largest possible total that’s less than or equal to
; 21, if possible

(define (is-ace? card)
  (equal? (first card) 'A)
)

(define (value-card card)
  (if (number? (first card))
    (first card)
    10
  )
)

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
  (if (= (count hand) n)
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

(define (best-total hand)
  ; Start the search with n = 1 and best = 0
  (best-total-helper hand 1 0)
)

(best-total '(ad as 9h)) ; here one counts as 11 and the other as 1
; 21

