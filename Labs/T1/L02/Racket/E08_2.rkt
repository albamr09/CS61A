#lang racket
; Load  the product function from last excercise
(require "./E08_1.rkt")

; Show how to define factorial in terms of product. 


(define (identity x) x)

(define (factorial n)
  (product identity 1 inc n)
)

(factorial 1)
; 1
(factorial 2)
; 2
(factorial 3)
; 6

; Also use product to compute approximations to π using the formula: pi/4 = 
; (2*4*4*6*6...)/(3*3*5*5*7*7...)

; If odd add 1
(define (set-even n) 
  (if (even? n)
    n
    (+ 1 n)
  )
)

; If even add 1
(define (set-odd n)
  (if (even? n)
    (+ 1 n)
    n
  )
)

; Obtain pi by dividing the product of the sequences
; 2,4,4,6,6,8,8 and 3,3,5,5,7,7. Then multiply it by 4
; by the formula discussed previously

(define (estimate-pi)
  (* 4(/ 
    (product set-even 2.0 inc 100)
    (product set-odd 2.0 inc 100)
  ))
)

(estimate-pi)
