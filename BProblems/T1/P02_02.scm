;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Number of ways to make change given an amount and n kinds of coins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (count-change amount) 
  (cc 
    amount 
    ; Kinds of coins we have left to try
    5
  )
)

(define (cc amount kinds-of-coins)
  (cond 
    ; If the total amount is zero there is one way of change
    ((= amount 0) 1)
    ; If the amount is negative or 
    ; there are 0 kinds of coins left to try
    ; there are no way of change
    ((or (< amount 0) (= kinds-of-coins 0)) 0)
    (else 
      ; Sum 
      (+ 
          ; the ways of making change without the first coin
          (cc 
            amount
            (- kinds-of-coins 1)
          )
          ; the ways of making change of the amount - the value of the first coin
          (cc 
            (- amount
              (first-denomination kinds-of-coins)
            ) 
            kinds-of-coins
          )
      )
    )
  )
)

; Takes as input the number of kinds
; of coins available and returns the denomination of the first kind

(define (first-denomination kinds-of-coins)
    (cond 
      ((= kinds-of-coins 1) 1)
      ((= kinds-of-coins 2) 5)
      ((= kinds-of-coins 3) 10)
      ((= kinds-of-coins 4) 25)
      ((= kinds-of-coins 5) 50)
    )
)

(count-change 100)
; 292
