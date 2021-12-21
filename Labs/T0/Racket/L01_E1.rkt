#lang racket

(require berkeley)

;; Exercise 1b.
(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y))
)

; This should output 25
(sum-of-squares 3 4)
