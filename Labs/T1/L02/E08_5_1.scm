; Newton's Method

; Load files necessary for newton's method
(load "../../../BProblems/T1/P03_03.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; Derivative
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

; Limit value
(define dx 0.00001)

;  Newton's Method
(define (deriv g)
  (lambda 
    ; Formal parameters
    (x) 
    ; derivative (x) = [g(x + dx) - g(x)] / dx
    (/ (- (g (+ x dx)) (g x)) dx)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; Newton's Method
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(define (newton-transform g)
  (lambda 
    ; Formal parameter
    (x) 
    ; f(x) = x - [g(x)/(Dg(x))]
    (- x (/ (g x) ((deriv g) x)))
  )
)

(define (newtons-method g guess)
  ; Solution for g(x)=0 is x when f(x) = x - [g(x)/(Dg(x))]
  (fixed-point 
    ; Get the "transformed" version of g, to apply Newton's method
    (newton-transform g) guess
  )
)
