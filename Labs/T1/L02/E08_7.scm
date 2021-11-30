; Write a procedure that takes as inputs a procedure that computes f and a
; positive integer n and returns the procedure that computes
; the nth repeated application of f . Your procedure should be able to be used as follows:
; ((repeated square 2) 5)
; 625

(define (repeated f n)
  (lambda
    (x)
    ; If the number of times to apply is one
    (if (= n 1)
      ; Return value of x under f
      (f x)
      ; Else apply f recursively
      (
       ; Function that composes n times
       ; Decrease n once
       (repeated f (- n 1)) 
       ; Pass as argument the value of x when you apply f
       ; so x is updated
       (f x)
      )
    )
   )
  )
)

((repeated square 2) 5)
; 625

((repeated square 3) 2)
; ((2^2)^2)^2= (2^4)^2 = 2^8 = 256
((repeated square 1) 2)
; 4
((repeated square 2) 2)
; 16
