#lang racket

(require berkeley)

; Constructor
(define (make-interval a b) (cons a b))

; Selectors

; Select first element of list
(define (lower-bound x) (car x))
; Select second element of list
(define (upper-bound x) (cdr x))


; Opeartions

(define (add-interval x y)
  ; The sum is the sum of the bounds
  (make-interval 
    ; Sum two lower bounds
    (+ (lower-bound x) (lower-bound y))
    ; Sum two upper bounds
    (+ (upper-bound x) (upper-bound y))
  )
)

(define (mul-interval x y)
  (let 
    (
      ; Multiply all of the bounds
      (p1 (* (lower-bound x) (lower-bound y)))
      (p2 (* (lower-bound x) (upper-bound y)))
      (p3 (* (upper-bound x) (lower-bound y)))
      (p4 (* (upper-bound x) (upper-bound y)))
    )
    (make-interval 
      ; The lower bound is the minimum of all the multiplications
      (min p1 p2 p3 p4)
      ; The upper bound is the maximum of all the multiplications
      (max p1 p2 p3 p4)
    )
  )
)

; Multiply the first by the reciprocal of the second
(define (div-interval x y)
  (mul-interval
    ; First interval
    x 
    ; Reciprocal of the interval
    ; Bounds are reversed and set to their reciprocals
    (make-interval 
      ; Reciprocal of the upper bound
      (/ 1.0 (upper-bound y))
      ; Reciprocal of the lower bound
      (/ 1.0 (lower-bound y))
    )
  )
)
