#lang racket

(require berkeley)

; Exercise 2.
; Max Sum of Squares

; Define a procedure that takes three numbers as arguments 
; and returns the sum of the squares of the two larger numbers

; Find the two larger elements by recursion
(define (find-larger-helper an larger second)
  (cond
    ; If empty return the two largest
    ((empty? an) (se larger second))
    ; If the first element of an > larger update larger to it and second to larger
    ((> (first an) larger) (find-larger-helper (bf an) (first an) larger))
    ; If the first element of an > second update second to it and leave larger the same
    ((> (first an) second) (find-larger-helper (bf an) larger (first an)))
    ; Else do not update larger and second
    (else (find-larger-helper (bf an) larger second))
  )
)

(define (find-larger an)
  (find-larger-helper an (first an) (first (bf an)))
)

; Sum the square of each element
(define (square-and-sum-list an)
  ; If empty sum 0
  (if (empty? an)
    0
    ; else sum the square of the first element
    (+ (square (first an)) (square-and-sum-list (bf an)))
  )
)

;; Exercise 2.
(define (max-sum-squares x y z)
  (square-and-sum-list (find-larger (se x y z)))
)

(max-sum-squares 1 2 3) ; 13
