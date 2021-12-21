; Define a procedure cubic that can be used together with the newtons-method 
; procedure in expressions of the form (newtons-method (cubic a b c) 1)
; to approximate zeros of the cubic x3 + ax2 + bx + c.

; Load Newton's Method
(load "./E08_5_1.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; Cubic function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(define (cubic a b c)
  ; Return a function
  (lambda
    ; Formal parameter
    (x)
    ; Function
    (+ 
      ; x^3
      (* x x x) 
      ; ax²
      (* a x x) 
      ; bx
      (* b x) 
      ; c
      c)
  )
)

(newtons-method 
  ; Function definition
  (cubic -6 11 -6)
  ; First guess
  -1.0
)

; Solutions: 1, 2, 3

(newtons-method 
  ; Function definition
  (cubic -2 -1 2)
  ; First guess
  5.0
)

; Solutions: 1, -1, 2

(newtons-method 
  ; Function definition
  (cubic -23 142 -120)
  ; First guess
  5.0
)

; Solutions: 1, 10, 12

(newtons-method 
  ; Function definition
  (cubic -6 11 -6)
  ; First guess
  -1.0
)

; Solutions: 2,1,3

