#lang racket

(require berkeley)

; Write a procedure squares that takes a sentence of 
; numbers as its argument and returns a sentence of the 
; squares of the numbers.

;; Exercise 7.
(define (squares sent)
  (if (empty? sent) 
    '()
    (se (square (first sent)) (squares (bf sent)))
  )
)

(squares '(1 2 3)) ;; (1 4 9)
