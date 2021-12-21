#lang racket

(require berkeley)

; Write a procedure that takes a sentence as its argument and returns
; another sentence containing only the numbers in the argument:

(define (numbers sent)
  (if (empty? sent)
    '()
    (se 
      (if (number? (first sent))
        (first sent)
        '()
      )
      (numbers (bf sent))
    )
  )
)

(numbers '(76 trombones and 110 cornets))
; (76 110)
