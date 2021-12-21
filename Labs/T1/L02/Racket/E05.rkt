#lang racket
(require berkeley)

(provide t)

(define (1+ x)
  (+ 1 x)
)

; Find the values of the following expressions where 1+ is a primitive procedure that adds one to its argument, and t is defined as follows:

(define (t f) 
  (lambda 
    ; Formal parameter
    (x) 
    ; Body
    ; Apply f over f(f(x))
    (f 
      ; Apply f over f(x)
      (f 
        ; Apply f over x
        (f x)
      )
    )
  )
) 
 
;; Exercise 5.
;; Work these out before trying them on the computer.
(
 ; Function that sums 1
 (t 1+) 
 0
) 
;; 3
(
 ; (t 1+) sums 3
 ; (t (t 1+)) sums 3 three times
 (t (t 1+)) 
 0
) 
;; 9 
(
 ; t applies a function three times
 ; (t t) applies t three times, so (t t) applies a function 3^3 times 
 ((t t) 1+) 
 0
)
;; 27
