#lang racket

(require berkeley)

; Write a procedure that takes a sentence as its argument and returns a
; sentence of the first letters in each of the sentence’s words:

(define (initials sent)
  (if (empty? sent)
    '()
    (se (first (first sent)) (initials (bf sent)))
  )
)

(initials '(if i needed someone))
; (I I N S)
