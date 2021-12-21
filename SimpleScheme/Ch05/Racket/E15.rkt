#lang racket

(require berkeley)

; Write a procedure first-two that takes a word as its argument, returning a
; two-letter word containing the first two letters of the argument.

(define (first-two wd)
    (
        word (first wd) (item 2 wd)
    )
)

(first-two 'ambulatory)
; AM
