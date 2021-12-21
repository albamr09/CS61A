#lang racket
(require berkeley)

; Write a procedure iterative-improve that takes two procedures
; as arguments: a method for telling whether a guess is good
; enough and a method for improving a guess. iterativeimprove 
; should return as its value a procedure that takes a
; guess as argument and keeps improving the guess until it is
; good enough. Rewrite the sqrt procedure of Section 1.1.7
; and the fixed-point procedure of Section 1.3.3 in terms of
; iterative-improve.

; improve: function that obtains the next value from the prior
; good-enough?: function that establishes if the algorithm should stop and return the guess
(define (iterative-improve improve good-enough?)
  (lambda
    (guess)
    (let
      ; List of values
      (
        ; Obtain the next guess applying the function on the current guess 
        (next (improve guess))
      )
      ; If the prior guess and the current guess are close enough
      (if (good-enough? guess next)
        ; Return next
        next
        ; Continue recursively with next as our new guess
        ((iterative-improve improve good-enough?) next)
      )
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; Definition for sqrt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

; Helper function
(define (average x y) (/ (+ x y) 2))

; Definition of sqrt
(define (sqrt x)
  (define tolerance 0.00001)
  (
   (iterative-improve 
      ; Sqrt function
      (lambda (y) (average y (/ x y))) 
      ; close-enough? function
      (lambda 
        (v1 v2) 
        (< (abs (- v1 v2)) tolerance)
      )
    ) 
    ; Argument for iterative-improve
    1.0
  )
)

(sqrt 4.0)
(sqrt 9.0)
(sqrt 121.0)
(sqrt 225.0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; Definition for fixed-point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (
   (iterative-improve 
      f 
      ; close-enough? function
      (lambda 
        (v1 v2) 
        (< (abs (- v1 v2)) tolerance)
      )
    ) 
    ; Argument for iterative-improve
    first-guess
  )
)

; y = cos(y)
(fixed-point cos 1.0)
; .7390822985224023

; y = sin(y) + cos(y)
(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)
; 1.2587315962971173
