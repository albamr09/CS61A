;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise 4.2.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;SICP 3.17 Write a correct version of count-pairs. (refer to E04_1)

; This method counts all of the lists given a list, including itself, so given the list ((1 2) (3 4) (5 (6))), the number of lists are 5

(define (count-pairs x)
  (if (pair? x)
    ;; If it is a pair
    (+
      ; Add one to count the current pair: x
      1
      ; Count the number of pairs inside
      ; the list x
      (accumulate 
        +
        ; Convert each element of the list x
        ; into the # of pairs that each element
        ; contains
        (map
          (lambda
            (element)
            (count-pairs element)
          )
          x
        )
      )
    )
    ;; If it is not a pair, convert it to 0, so 
    ;; it adds nothing
    0
  )
)


;;;;;;;;;;;;;;;;;;;;
;; Example
;; Given the list ((1 2) (3 4)), the expected result is 3
;; [count ((1 2) (3 4))] = 1 + [count (1 2)] + [count (3 4)] = 1 + 1 + 1
;; [count (1 2)] = 1 + [count 1] + [count 2] = 1 + 0 + 0 = 1
;; [count 1] = 0
;; [count 2] = 0
;; [count (3 4)] = 1 + [count 3] + [count 4] = 1 + 0 + 0 = 1
;; [count 3] = 0
;; [count 4] = 0
;;;;;;;;;;;;;;;;;;;;

; (trace count-pairs)

(print (count-pairs (list 1)))
; 1
(print (count-pairs (list (list 1 2) (list 3 4) (list 5 6))))
; 4
(print (count-pairs (list (list 1 2) (list 3 4) (list 5 (list 6)))))
; 5

