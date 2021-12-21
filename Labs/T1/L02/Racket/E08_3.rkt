#lang racket
(require berkeley)

; Accumulate that combines a collection of terms, using 
; some general accumulation function:
; (accumulate combiner null-value term a next b)

; Increment by 1
(define (inc n) (+ 1 n))
; Sum two numbers
(define (sum x y) (+ x y))

; Combiner a collection of elements 
; combiner: How to combiner every two elements
; null-value: initial value of result
; term: how to transform each term of the sequence
; a: lower bound
; next: how to get the next term of the sequence
; b: upper bound

(define (accumulate combiner null-value term a next b)
  ; Iterative procedure
  (define (iter a result)
    ; If the lower bound > upper bound return result
    (if (> a b)
      result
      ; Else continue
      (iter 
        ; Obtainer next value of sequence
        (next a) 
        ; Combine the current result with the 
        ; transformation of the current term
        (combiner result (term a))
      )
    )
  )
  ; First invocation of iterative method
  (iter a null-value)
)

; Sum all numbers from 5 to 10
(accumulate sum 0 inc 5 inc 10)
