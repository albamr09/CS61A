#lang racket
(require berkeley)

; Define a procedure reverse that takes a list
; as argument and returns a list of the same elements in re-
; verse order:

; Note: Your solution should reverse lists, not sentences! That is, you should 
; be using cons, car, and cdr, not first, sentence, etc.

(define (reverse-helper lst reversed)
  (if (null? lst)
    reversed
    (reverse-helper 
      (cdr lst) 
      (cons (car lst) reversed)
    )
  )
)

(define (reverse lst)
  (reverse-helper lst '())
)


(reverse (list 1 4 9 16 25))
; (25 16 9 4 1)
