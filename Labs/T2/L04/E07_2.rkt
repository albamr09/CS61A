#lang racket
(require berkeley)

; Import interval data type
(require "./E07_1.rkt")

; Using reasoning analogous to Alyssa’s, de-
; scribe how the difference of two intervals may be com-
; puted. Define a corresponding subtraction procedure, called
; sub-interval.

(define (sub-interval x y)
  (make-interval
    (- (lower-bound x) (lower-bound y))
    (- (upper-bound x) (upper-bound y))
  )
)

;; TEST

(define x (make-interval 0 1))
(define y (make-interval 1 2))

(sub-interval x y)
