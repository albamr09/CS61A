#lang racket
(require berkeley)

; Modify the cc procedure so that its kinds-of-coins parameter, instead of being an integer, is a 
; sentence that contains the values of the coins to be used in making change.

; The coins should be tried in the sequence they appear in the sentence. For 
; the count-change procedure to work the same in the revised program as in the 
; original, it should call cc as follows:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Modified version to take string as input instead of integer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (count-change amount) 
  (cc 
    amount 
    ; Kinds of coins we have left to try
    '(50 25 10 5 1)
  )
)

(define (cc amount kinds-of-coins)
  (cond 
    ; If the total amount is zero there is one way of change
    ((= amount 0) 1)
    ((or 
        ; If the amount is negative
        (< amount 0) 
        ; There are no more type of coins to try in the list
        (= (count kinds-of-coins) 0)
      ) 
      0
    )
    (else 
      ; Sum 
      (+ 
          ; the ways of making change without the first coin
          (cc 
            amount
            ; Remove the first coin from the list
            (bf kinds-of-coins)
          )
          ; the ways of making change of the amount - the value of the first coin
          (cc 
            (- amount
              ; Substract the first coin from the list from the amount
              (first kinds-of-coins)
            ) 
            kinds-of-coins
          )
      )
    )
  )
)

(count-change 100)
; 292
