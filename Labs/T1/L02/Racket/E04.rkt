#lang racket
(require berkeley)

; For each of the following expressions, what must f be in order for 
; the evaluation of the expression to succeed, without causing an error? For each 
; expression, give a definition of f such that evaluating the expression will not 
; cause an error, and say what the expression's value will be, given your definition.

;; Exercise 4.

;; Define a constant
(define (f1)
  1
)
;; Value:
f1
 
;; Regular function without arguments
(define (f2)
  (+ 1 1)
)

;; Value:
(f2)
 
;; Regular function
(define (f3 x) 
  (+ x x)
)
;; Value:
(f3 3)
 
;; (f4) returns the lambda function
;; ((f4)) evaluates the lambda function returned by (f4)
(define (f4) 
  (lambda 
    ; No arguments
    ()
    ; Return function
    (+ 1 1)
  )
)
;; Value:
((f4))
 
;; (f5) returns a lambda function with no arguments
;; ((f5)) the previous lambda function returns a lambda function with x as argument
;; (((f5))) Evaluates the inner lambda function
;; (((f5)) 3) Evaluates the inner lamda function with 3 as argument
(define (f5) 
  (lambda
    ; No arguments
    ()
    ; Body is another anonymous function
    (lambda
      ; Takes x as argument
      (x)
      ; Body of function
      (+ x x)
    )
  )
)
;; Value:
(((f5)) 3)
