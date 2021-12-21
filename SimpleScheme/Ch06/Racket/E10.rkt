#lang racket

(require berkeley)

; Write a procedure that takes as its argument a sentence containing two
; numbers. It should return a sentence containing the same two numbers, but in ascending
; order:

(define (sort2 numbers)
  (
    if (<= (first numbers) (last numbers))
    (se numbers)
    (se (last numbers) (first numbers))
  )
)

(sort2 '(5 7))
; (5 7)
(sort2 '(7 5))
; (5 7)
