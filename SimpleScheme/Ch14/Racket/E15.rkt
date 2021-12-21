#lang racket
(require berkeley)

; Write , a procedure that takes two sentences of numbers as arguments.
; Each sentence must consist of numbers in increasing order. should return a single
; sentence containing all of the numbers, in order. (We’ll use this in the next chapter as
; part of a sorting algorithm.)

(define (merge an bn)
  (cond
    ; If one of the lists ends, we return the remainig of the other
    ((empty? an) (se bn))
    ((empty? bn) (se an))
    ; Check if the first element of an < bn, if so append to final list
    ; and remove the element from an
    ((<= (first an) (first bn)) (se (first an) (merge (bf an) bn)))
    ; else append bn and remove the element from bn
    (else (se (first bn) (merge an (bf bn))))
  )
)

(merge '(4 7 18 40 99) '(3 6 9 12 24 36 50))
; (3 4 6 7 9 12 18 24 36 40 50 99)
