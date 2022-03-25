;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; E07
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Exercise 7. Write bubble-sort!

;;;;;;;;;;;;;;;;;;;;
; Use the following algorithm for your definition: 
; 1.  Go through the array, looking at two adjacent elements at a time, 
;     starting with elements 0 and If the earlier element is larger than the 
;     later element, swap them. Then look at the next overlapping pair (0 and 1, 
;     then 1 and 2, etc.). 
; 2.  Recursively bubble-sort all but the last element (which is now the largest 
;     element). 
; 3. Stop when you have only one element to sort.
;;;;;;;;;;;;;;;;;;;;

(define (bubble-sort! vec)
  ; - count: number of times a swap took place
  (define (loop i n count)
    (cond
      ; If i < 1, then i = 0, and we cannot compare with the previous element
      ; so we have finished going through the vector backwards
      ((> i n)
        ; We check if there was any swapping done
        (if (= count 0)
          ; If not we return the ordered vector
          vec
          ; If there was, we go through the vector again to check
          ; if it is ordered, or to keep ordering
          (loop 0 (- n 1) 0)
        )
      )
      ; If the current element vec[i + 1] < vec[i], then we interchange them
      ((< (vector-ref vec (+ i 1)) (vector-ref vec i))
        ; Swap them
        (let
          ((temp (vector-ref vec i)))
          (vector-set! vec i (vector-ref vec (+ i 1)))
          (vector-set! vec (+ i 1) temp)
        )
        ; Keep going through the vector
        (loop (+ i 1) n (+ count 1))
      )
      ; If not, do not update anything
      (else
        ; Keep going through the vector
        (loop (+ i 1) n count)
      )
    )
  )
  (trace loop)
  ; Start sorting with the count of swaps to zero
  ; We do (vector-length vec) - 2, because
  ; 1. First -1 because the vector start in 0, so the index upper limit must be the 
  ;     size of the vector minus 1
  ; 2. Second -1 because we are comparing an element v[i] to the next v[i+1], so we
  ;     want to stop comparing when i+1 exceeds the upper limit, which is one iteration
  ;     before than when i reaches it
  (loop 0 (- (vector-length vec) 2) 0)
)

(print (bubble-sort! (vector 86 2 26 8 3)))
; (1 2)
; (print (bubble-sort! (vector 2 8 39 2 9 0 1 8 43 92 31 1)))
; (0 1 1 2 2 8 8 9 31 39 43 92)

; What is the order of the algorithm?

; We will analyze how many steps are required, worse case scenario. That is to say, how many
; steps are required to order a vector that requires a swap for every element:
; Suposse we have a list ordered in reverse such as:
; (4 3 2 1)
; So in the first iteration of the loop, after 4 steps, we reach the next state:
; (3 2 1 4)
; In the second iteration of the loop, after 3 steps, we reach the next state:
; (2 1 3 4)
; In the third iteration of the loop, after 2 steps, we reach the next state:
; (1 2 3 4)
; And finally we iterate again through the whole vector, so we perform one step
; to check there is only one element left to order

; So, the number of steps are 4 + 3 + 2 + 1 = 10 steps in total. 
; By the formula of the sum of these type of sequences, for a vector of size n the 
; number of steps equal: n(n+1)/2 = (n^2 + n)/2
; By the theorem on the order of polynomials we conclude that the algorithm is Theta(n^2)
