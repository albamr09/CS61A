; Suppose that the following definitions have been provided.

(define x (cons 1 3))
(define y 2)

; A CS 61AS student, intending to change the value of x to a pair with car 
; equal to 1 and cdr equal to 2, types the expression (set! (cdr x) y) instead 
; of (set-cdr! x y) and gets an error. Explain why.