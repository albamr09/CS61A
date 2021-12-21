#lang racket

(require berkeley)

; Sometimes you must choose the singular or the plural of a word: 1 book but
; 2 books. Write a procedure that takes two arguments, a number and a
; singular noun, and combines them appropriately:

(define (thismany amount wd)
  (
    if (> amount 1)
    (se amount (word wd 's))
    (se amount wd)
  )
)

(thismany 1 'partridge)
; (1 PARTRIDGE)
(thismany 3 'french-hen)
; (3 FRENCH-HENS)
