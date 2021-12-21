#lang racket

(require berkeley)

; Write a procedure insert-and that takes a sentence of items and returns a new
; sentence with an “and” in the right place:
(define (insert-and sent)
    (
        se (bl sent) 'and (last sent)
    )
)

(insert-and '(john bill wayne fred joey))
; (JOHN BILL WAYNE FRED AND JOEY)
