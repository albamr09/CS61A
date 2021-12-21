#lang racket
(require berkeley)

; Identify two ways to change the program to reverse the order in which coins 
; are tried, that is, to change the program so that pennies are 
; tried first, then nickels, then dimes, and so on. 

; We use the same program as the one in "./BProblems/T1/P02_02.scm"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; First way to change the order: Simply reverse the order in the procedure first-denomination
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (count-change-alt-1 amount) 
  (cc-alt-1 
    amount 
    ; Kinds of coins we have left to try
    5
  )
)

(define (cc-alt-1 amount kinds-of-coins)
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
          (cc-alt-1 
            amount
            (- kinds-of-coins 1)
          )
          ; the ways of making change of the amount - the value of the first coin
          (cc-alt-1 
            (- amount
              (first-denomination-alt-1 kinds-of-coins)
            ) 
            kinds-of-coins
          )
      )
    )
  )
)

; Takes as input the number of kinds
; of coins available and returns the denomination of the first kind

(define (first-denomination-alt-1 kinds-of-coins)
    (cond 
      ((= kinds-of-coins 1) 50)
      ((= kinds-of-coins 2) 25)
      ((= kinds-of-coins 3) 10)
      ((= kinds-of-coins 4) 5)
      ((= kinds-of-coins 5) 1)
    )
)

(count-change-alt-1 100)
; 292

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Second way: reverse the way kinds-of-coins is updated
; - set the kind-of-coins variable to one
; - increase the kind-of-coins variable each time
; - Finish if kind-of-coins = 6 (the max value)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (count-change-alt-2 amount) 
  (cc-alt-2
    amount 
    ; Kinds of coins we start with (still have not tried any)
    1
  )
)

(define (cc-alt-2 amount kinds-of-coins)
  (cond 
    ; If the total amount is zero there is one way of change
    ((= amount 0) 1)
    ; If the amount is negative or 
    ; we have reached past the type of coin number five 
    ; there is no way of change
    ((or (< amount 0) (= kinds-of-coins 6)) 0)
    (else 
      ; Sum 
      (+ 
          ; the ways of making change without the first coin
          (cc-alt-2 
            amount
            (+ kinds-of-coins 1)
          )
          ; the ways of making change of the amount - the value of the first coin
          (cc-alt-2 
            (- amount
              (first-denomination-alt-2 kinds-of-coins)
            ) 
            kinds-of-coins
          )
      )
    )
  )
)

; Takes as input the number of kinds
; of coins available and returns the denomination of the first kind

(define (first-denomination-alt-2 kinds-of-coins)
    (cond 
      ((= kinds-of-coins 1) 1)
      ((= kinds-of-coins 2) 5)
      ((= kinds-of-coins 3) 10)
      ((= kinds-of-coins 4) 25)
      ((= kinds-of-coins 5) 50)
    )
)

(count-change-alt-2 100)
; 292
