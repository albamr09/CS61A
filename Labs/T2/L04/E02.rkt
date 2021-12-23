#lang racket
(require berkeley)


(define x (cons 4 5))
(define y (cons 'hello 'goodbye))
(define z (cons x y))

; Predict the result of each of these before you try it.

; z = ((4 5) (hello goodbye))

; (cdr (car z))
; Error, there is no second element, it is not a pair
(car (cons 8 3))
; 8
(car z)
; (4 5)
; (car 3)
; Error, it is not a pair
