#lang racket
(require berkeley)

; Define a procedure double that takes a procedure of one argument as argument and returns 
; a procedure that applies the original procedure twice. For example, if inc is 
; a procedure that adds 1 to its argument, then (double inc) should be a procedure that adds 2. 

(define (double f)
  ; Returns a function
  (lambda 
    ; Formal parameter
    (x)
    ; Apply f twice
    (f 
      ; Apply f once
      (f x)
    )
  )
)


(define (inc x) (+ 1 x))


(
  (
    ; Apply two times a function that applies four times: so 4^2 = 16 times
    (double 
      ; Function that applies two times a function that applies two times: so 4 times
      (double double)
    ) 
    inc) 5
)
