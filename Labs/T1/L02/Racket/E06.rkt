#lang racket
(require "./E05.rkt")

; Find the values of the following expressions where t is defined as in question 5 above, and s is defined as follows:


(define (s x) 
  (+ 1 x)
)

; Sums 1 three times
((t s) 0) 
;; 3

; Applies 3 times a function that sums 1 three times
(
 (t (t s)) 
 0
) 
;; 9

; Apply t three times, where t is a lambda that applies a function three times
; so (t t) applies a function 27 times, meaning, ((t t) s) sums 1, 27 times
(((t t) s) 0)
;; 27
