#lang racket
(require berkeley)

; Import interval data type
(require "./E07_1.rkt")

; Ben Bitdiddle, an expert systems programmer, looks over Alyssa's shoulder and comments 
; that it is not clear what it means to divide by an interval that spans zero. Modify Alyssa's 
; code to check for this condition and to signal an error if it occurs.

; Note: Spans zero means that one bound is <= zero and the other is >= zero!

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
  (if
    (and 
      (>= (upper-bound y) 0) 
      (<= (lower-bound y) 0) 
    )
    (error "The interval spans zero!")
    ; Else compute interval
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
)

; (div-interval (make-interval 20 30) (make-interval 10 12))
; (div-interval (make-interval -5 5) (make-interval 10 20))
; (div-interval (make-interval -5 5) (make-interval -5 5))
; Error
