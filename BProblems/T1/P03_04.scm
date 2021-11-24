; Load last section functions
(load "./P03_03.scm")

; Return a function that gets the average between x and f(x)
(define (average-damp f)
  (lambda 
    ; Formal parameter
    (x) 
    ; Return function
    (average x (f x))
  )
)

((average-damp square) 10)
; 55

; Square function with fixed point method
(define (sqrt x)
  ; y^2 = x, so y = x / y. But this does not converge so we average y and x/y
  (fixed-point 
    ; Returns a function that is a transformation of y = x/y so that it converges
    (average-damp (lambda (y) (/ x y)))
    ; First guess for x
    1.0
  )
)

; y^3 = x, so y = x / y^2. Again we average y and x/y^2
(define (cube-root x)
  (fixed-point 
    (average-damp (lambda (y) (/ x (square y))))
    ; First guess
    1.0
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; Derivative
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

; Limit value
(define dx 0.00001)

(define (deriv g)
  (lambda 
    ; Formal parameters
    (x) 
    ; derivative (x) = [g(x + dx) - g(x)] / dx
    (/ (- (g (+ x dx)) (g x)) dx)
  )
)

; Cube function
(define (cube x) (* x x x))

; Derivative of cube at 5
((deriv cube) 5)
; 75.00014999664018


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

; Sqrt definition with Newton's Method
(define (sqrt x)
  (newtons-method
    ; Find y where x - y^2 = 0
    ; Because we want x = y^2
    (lambda (y) (- (square y) x)) 
    ; First guess
    1.0
  )
)

(sqrt 9)
(sqrt 4)
(sqrt 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; Even more abstract version
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

; - g: normal function to apply fixed point
; - transform: how to transform the function f (i.e. with newton-transform for Newton's Method)
; - guess: first guess for fixed-point method
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess)
)

; Sqrt definition with average-damp
(define (sqrt x)
  (fixed-point-of-transform
    ; Regular function y = x / y
    ; y^2 = x, so y is the root of x
    (lambda (y) (/ x y)) 
    ; Transform function for the method to converge
    average-damp 
    ; First guess
    1.0
  )
)

(sqrt 9)
(sqrt 4)
(sqrt 2)

; Sqrt definition with Newton's Method
(define (sqrt x)
  (fixed-point-of-transform
    ; Regular function for square
    ; y^2 - x = 0
    (lambda (y) (- (square y) x)) 
    ; How to transform the function
    newton-transform 
    ; First guess
    1.0
  )
)

(sqrt 9)
(sqrt 4)
(sqrt 2)


