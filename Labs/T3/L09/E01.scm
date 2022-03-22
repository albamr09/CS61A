;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; E01
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Suppose that the following definitions have been provided.

(define x (cons 1 3))
(define y 2)

; A CS 61AS student, intending to change the value of x to a pair with car 
; equal to 1 and cdr equal to 2, types the expression (set! (cdr x) y) instead 
; of (set-cdr! x y) and gets an error. Explain why.

; The expression (set! (cdr x) y) gives and error, because the second element of (1. 3), 3, is not a variable
; so the declaration would be equal to (set! 3 2), which is semantically incorrect.

