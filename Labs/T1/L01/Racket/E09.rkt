#lang racket

(require berkeley)

; Write a predicate ordered? that takes a sentence of numbers as its argument and returns a
; true value if the numbers are in ascending order, or a false value otherwise.

;; Exercise 9.
(define (ordered? sent)
  (cond
    ; If the list only has one element, end with tru
    ((<= (count sent) 1) #t)
    ; If the first element <= the second first, continue removing the first element
    ((<= (first sent) (first (bf sent))) (ordered? (bf sent)))
    ; Else the first element > second first, so return false
    (else #f)
  )
)
(ordered? '(1 2 3)) ; #t
(ordered? '(2 1 3)) ; #f
(ordered? '(2)) ; #t
