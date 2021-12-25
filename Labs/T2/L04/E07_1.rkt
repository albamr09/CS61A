#lang racket
(require berkeley)

; Alyssa’s program is incomplete because she
; has not specified the implementation of the interval ab-
; straction. Here is a definition of the interval constructor:
; Define selectors upper-bound and lower-bound to complete
; the implementation.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Interval Data Object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Constructor

(define (make-interval a b) (cons a b))

; Selectors

; - i: interval
(define (lower-bound i)
  (car i)
)

; - i: interval
(define (upper-bound i)
  (cdr i)
)

; Exports
(provide make-interval lower-bound upper-bound)
