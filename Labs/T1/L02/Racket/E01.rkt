#lang racket

(require berkeley)

; Exercise 1.
; Type each of the following into Scheme, and note the results. See if 
; you can predict the results before letting Scheme do the computation

; Define function
(lambda (x) (+ x 3))

; Evaluate function
(
 ; Operator
 (lambda (x) (+ x 3)) 
 ; Argument
 7
)

; Add two numbers 
(define (make-adder num) 
  ; Returns a function with num equal to the num argument
  (lambda (x) (+ x num))
)
; Evaluate x + num, where x = 7 and num = 3
((make-adder 3) 7) 
 

; Add 3 to a number
(define plus3 (make-adder 3)) 
(plus3 7) 
 

; Sqrt normal definition
(define (square x) (* x x)) 
(square 5) 
 
; Sqrt with lambda
(define sq 
  ; Return function that takes x as argument
  (lambda 
    (x) 
    (* x x)
  )
)
(sq 5) 
 
; Abstraction that applies a function f to 3 and 5
(define (try f) 
  (f 3 5)
)
; Add
(try +) 
; Concatenate characters 3 and 5 
(try word)
