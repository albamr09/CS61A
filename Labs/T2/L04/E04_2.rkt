#lang racket
(require berkeley)
; Load point and segment
(require "./E04_1.rkt")

; Implement a representation for rectangles in a plane. 

; In terms of your constructors and selectors, create procedures that compute the 
; perimeter and the area of a given rectangle. 

; Now implement a different representation for rectangles. 

; Can you design your system with suitable abstraction
; barriers, so that the same perimeter and area procedures
; will work using either representation?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Rectangle data type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Constructor
; - s1: first segment
; - s2: second segment
(define (make-rectangle s1 s2))

; Selectors
(define (b r)

)
(define (h r))

; Operations
(define (perimeter r)
  ; Perimeter = b+b+h+h
  (+ 
    (* (b r) 2)
    (* (h r) 2)
  )
)

(define (area r)
  ; area = b*h
  (*
    (b r)
    (h r)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Rectangle data type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Constructor
; - o: origin of the rectangle
; - b: base of the rectangle
; - h: height of the rectangle
(define (make-rectangle o b h)
  (cons o b h)
)

; Selectors
(define (b r)
  (car (cdr r))
)
(define (h r)
  (cdr (cdr r))
)
