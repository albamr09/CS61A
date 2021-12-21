#lang racket
(require berkeley)

(provide average fixed-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; Half interval method to find roots
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

; Get average of two numbers
(define (average x y) (/ (+ x y) 2))

; Main search of root method
(define (search f neg-point pos-point)
  (let 
    ; Variables
    ((midpoint (average neg-point pos-point)))
    ; Body
    (if (close-enough? neg-point pos-point)
      ; If close enough return midpoint
      midpoint
      (let 
        ; Obtain new value for midpoint
        ((test-value (f midpoint)))
        (cond 
          ; If positive search on the interval given by (neg-point, midpoint)
          ((positive? test-value) (search f neg-point midpoint))
          ; If negative search on the interval given by (midpoint, pos-point)
          ((negative? test-value) (search f midpoint pos-point))
          ; If zero, the root is midpoint
          (else midpoint))
      )
    )
  )
)

; Error tolerance
(define (close-enough? x y) (< (abs (- x y)) 0.001))

; Check that the values a and b are negative and positive or viceversa
(define (half-interval-method f a b)
  (let 
    (
      ; Get image of a and b under f
      (a-value (f a))
      (b-value (f b))
    )
    (cond 
      ; a > 0 and b < 0 then search on the interval (b, a)
      ((and (negative? a-value) (positive? b-value)) (search f a b))
      ; a < 0 and b > 0 then search on the interval (a, b)
      ((and (negative? b-value) (positive? a-value)) (search f b a))
      ; Else error
      (else (error "Values are not of opposite sign" a b))
    )
  )
)

; sin(x) = 0 
(half-interval-method sin 2.0 4.0)
; 3.14111328125

; x^3 - 2x - 3 = 0
(half-interval-method 
  (lambda (x) (- (* x x x) (* 2 x) 3))
  1.0
  2.0
)
; 1.89306640625

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; Finding fixed points functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

; Error tolerance
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    ; Difference between points less than error tolerance
    (< (abs (- v1 v2)) tolerance)
  )
  (define (try guess)
    (let 
      ; Get image of guess under f
      ((next (f guess)))
      ; If the first value is close enough to the current value
      (if (close-enough? guess next)
        next
        ; Try again
        (try next))
    )
  )
  ; Call try function with guess from arguments
  (try first-guess))

; y = cos(y)
(fixed-point cos 1.0)
; .7390822985224023

; y = sin(y) + cos(y)
(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)
; 1.2587315962971173

; Use the fixed point method to define a custom sqrt function
; y^2 = x, so y = x / y. But this does not converge so we average y and x/y
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0)
)

(sqrt 9)



