; Write a procedure make-safe that you can use this way:
; It should take two arguments, a function and a type-checking predicate, and 
; return a new function that returns #f if its argument doesn’t satisfy the predicate.

(define (make-safe f condition)
  ; Return an anonymous function
  (lambda 
    ; Formal parameters of the function
    (input)
    ; If the condition is met by the input data
    (if (condition input)
      ; Apply the function f
      (f input)
      ; Else return false
      #f
    )
  )
)
 
(define safe-sqrt 
  ; Type check
  (make-safe sqrt number?)
)

(safe-sqrt 'hello)
; #f
(safe-sqrt 4)
; 2
