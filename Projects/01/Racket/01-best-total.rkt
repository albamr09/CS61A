#lang racket
(require berkeley)
(require sugar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Obtain the best score less than 21 from all the possible combinations
; of all possible number of cards from the hand
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; How are the  combinations generated? Suppose the size n is set to 1, 
; and the input is (1 2 3)
; 1. We generate the combinations of size 1
; (1) (2) (3)
; 2. Then we generate the combinations of size 2. For that we start by 
; generating the combinations of size 2 for 1, which means k = 1. We also 
; have to determine an offset, that will indicate from which index to con
; catenate (n - 1) elements of the list
; So for k = 1, offset = 1
; (1 2)
; and for k = 1, offset = 2
; (1 3)
; We set offset = 3. Because now n + (offset - 1) > size of list, we generate 
; combinations for the next k, so k = 2, and offset = 2 
; (is not a relative offset)
; (2 3)
; We set offset = 3. Now, again n + (offset - 1) > size of list, so we set k = k + 1 = 3
; However now n + (k - 1) > size of list. So we have finished generating combinations
; for n = 2
; 3. Next we generate combinations of size 3, with k = 1, offset = 1
; (1 2 3)
; If we set offset = offset + 1, we have n + (offset - 1) > size of list. And if then,
; we set k = k + 1, we have n + (k - 1) > size of list.
; 4. Finally, because we set n = 4, n > size of list. So the generation process is
; finished

; Note that our conditions use (offset - 1) and (k - 1). 

; The fist one is because when we generate our sublists of size n, 
; we generate the sublist from offset to (offset - 1) + n, because 
; we are not using the k element, so we generate a sublist 
; of size n - 1. 
; Think of it as an sliding window, with a step of 1. If we update the offset,
; adding one to it and we generate the sublist, if its upper limit
; (offset - 1) + n > size of list, then we have reached the end of the list, 
; in other words, the sliding window collides with the end wall of the list.

; The second one is because we start with k = 1, instead of k = 0. And so 
; if we used k + n, this would indicate the actual combination upper limit is 
; over by one.

; For example
; If we want to generate combinations of size n = 2, we start with k = 1. So 
; the upper limit is k + n = 1 + 2 = 3. But that is not true, the upper limit,
; meaning the index of the last element in the sliding window, is 2.
; If k = 2, then k + n = 2 + 2 = 4, then again this is false, because the upper
; limit is actually 3.
; Thus we define our upper limit with (k - 1) + n. And so 
; if we want to generate the combinations for a given k, and (k - 1) + n > size
; of list, this would mean that the upper limit exceeds the upper limit of the list
; or that our sliding window overflows the list. Hence we finished generating
; all of the  combinations for a concrete n.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Example of how the combinations are generated. 
; Give the list (1 2 3), the combinations are generated as follow
; (1)
; (2)
; (3)
; ---
; (1 2)
; (1 3)
; (2 3)
; ---
; (1 2 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Example of generation of combinations and calculation of best score
; Given the input (1s 2s 3s)
; 1. best-total-helper hand = (1s 2s 3s), n = 1, best = 0
; 2. combination-n-from-k, n = 1, k = 1, offset = 1, best = 0
; 2.1. sum-list, hand = (1s)
; -> 1
; 3. combination-n-from-k, n = 1, k = 2, offset = 2, best = 1
; 3.1. sum-list, hand = (2s)
; -> 2
; 4. combination-n-from-k, n = 1, k = 3, offset = 3, best = 2
; 4.1. sum-list, hand = (3s)
; -> 3
; 5. combination-n-from-k, n = 1, k = 4, offset = 4, best = 2
; -> k > size of hand: #t
; 6. best-total-helper hand = (1s 2s 3s), n = 1, best = 3
; 7. combination-n-from-k, n = 2, k = 1, offset = 1, best = 3
; 7.1. sum-list, hand = (1s 2s)
; -> 3 
; 8. combination-n-from-k, n = 2, k = 1, offset = 2, best = 3
; 8.1. sum-list, hand = (1s 3s)
; -> 4 
; 9. combination-n-from-k, n = 2, k = 1, offset = 3, best = 4
; -> size of hand < (n + (offset - 1)): #t
; -> size of hand < (n + (k - 1)): #f
; 10. combination-n-from-k, n = 2, k = 2, offset = 2, best = 4
; 10.1. sum-list, hand = (2s 3s)
; -> 5 
; 11. combination-n-from-k, n = 2, k = 2, offset = 3, best = 5
; -> size of hand < (n + (offset - 1)): #t
; -> size of hand < (n + (k - 1)): #t
; 12. best-total-helper hand = (1s 2s 3s), n = 2, best = 5
; 13. combination-n-from-k, n = 3, k = 1, offset = 1, best = 5
; 13.1. sum-list, hand = (1s 2s 3s)
; -> 6 
; 14. combination-n-from-k, n = 3, k = 1, offset = 2, best = 6
; -> size of hand < (n + (offset - 1)): #t
; -> size of hand < (n + (k - 1)): #f
; 15. combination-n-from-k, n = 3, k = 2, offset = 2, best = 6
; -> size of hand < (n + (offset - 1)): #t
; -> size of hand < (n + (k - 1)): #t
; 16. best-total-helper hand = (1s 2s 3s), n = 2, best = 6
; -> 6

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

; This method obtains the sum of elements in the hand
; and updates the best score if necessary
; - hand: hand to generate combinations from
; - n: size of the combinations
; - k: index of the element we are generating the combinations
; - offset: index of offset
; - best: best score up until now

(define (update-best hand n k offset best)
  (let
    ((total 
      (sum-list 
        (se 
          ; First item in the list
          (item k hand) 
          ; Sublist from offset to n + (offset - 1)
          (sublist 
            hand 
            ; From offset
            offset 
            ; Sublist of n - 1 elements
            (+ (- offset 1) n)
          )
        ) 
      ; Initial total sum of the list
      0)
    )) 
    ; If the total calculated does not exceed 21 and 
    ; is greater than the current best
    (if (and (<= total 21) (> total best))
      ; return total, so best = total
      total
      ; else do not update total
      best
    )
  )
)

; This method generates all of the combinations of size n
; from a given hand
; - hand: hand to generate combinations from
; - n: size of the combinations
; - k: index of the element we are generating the combinations
; - offset: index of offset
; - best: best score up until now

(define (combination-n-from-k hand n k offset best)
  (cond
    ; When the size of the combinations is one
    ; we need to generate then differently
    ((= n 1)
      ; If the index we are generating from > size
      ; of the hand, we finished with this n
      (if (> k (count hand))
        ; Return the best value
        best
        ; Else keep generating combinations
        (combination-n-from-k
          ; Complete list
          hand
          ; Size of combination
          n
          ; Where to start in the list
          (+ k 1)
          ; Offset to append, 1 refers to second element in the list
          (+ k 1)
          ; Update best score if necessary
          (update-best hand n k offset best)
        )
      )
    )
    ; Prevent overflow of the index
    ; if n + (offset - 1) > size of the hand
    ((< (count hand) (+ n (- offset 1))) 
      ; Prevent overflow of the index
      ; if n + (k - 1) > size of the hand
      (if (< (count hand) (+ n (- k 1)))
        ; Return the best value
        best 
        ; Else keep generating combinations
        (combination-n-from-k
          ; Complete list
          hand
          ; Size of combination
          n
          ; Where to start in the list
          (+ k 1)
          ; Offset to append, 1 refers to second element in the list
          (+ k 1)
          ; Save best score
          best
        )
      ) 
    )
    ; Calculate the score of the current combination
    (else 
      (combination-n-from-k
        ; Complete list
        hand
        ; Size of combination
        n
        ; Where to start in the list
        k
        ; Offset to append, 1 refers to second element in the list
        (+ offset 1)
        ; Update best score if necessary
        (update-best hand n k offset best)
      )
    )
  )
)

; This method tries all of the combinations of size from n (usually 1)
; to the length of the hand and returns the best score obtained
; - hand: list to generate combinations from
; - n: current size of combination
; - best: best score up until now

(define (best-total-helper hand n best)
  ; Go from n to length of hand
  (if (< (count hand) n)
    ; If all the sizes have been tried return the 
    ; best sum less or equal to 21
    best
    ; Else keep searching for the best 
    ; sum with a bigger number of cards
    (best-total-helper
      hand
      ; Update size n
      (+ 1 n)
      ; Obtain the best score < 21, of all of the possible 
      ; combinations of size n
      (combination-n-from-k hand n 1 1 best)
    )
  )
)

; (trace best-total-helper)

; Exports
(provide best-total-helper)