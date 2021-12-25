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
; - s1: upper segment
; - s2: lower segment
; --------------------------- <-- s2
; |                         |
; |                         |
; |                         |
; |                         |
; --------------------------- <-- s1

(define (make-rectangle o b h)
  ; There is not restriction 
  ; for the correctness of the
  ; rectangle, sadly
  (cons 
    ; s1
    (make-segment
      ; p1 = origin
      o
      ; p2
      (make-point 
        ; x = origin + offset of b
        (+ (x-point o) b)
        (y-point o)
      )
    )
    ; s2
    (make-segment
      ; p1
      (make-point
        (x-point o)
        ; y = origin + offset of h
        (+ (y-point o) h)
      )
      (make-point
        ; x = origin + offset of b
        (+ (x-point o) b)
        ; y = origin + offset of h
        (+ (y-point o) h)
      )
    )
  )
)

; Selectors

(define (b r)
  ; abs(x1 - x2)
  (abs (-
    ; From s1 = (p1, p2), select x from p1 = (x, y)
    (x-point (start-segment (car r)))
    ; From s1 = (p1, p2), select x from p2 = (x, y)
    (x-point (end-segment (car r)))
  ))
)

(define (h r)
  ; abs(y1 - y2)
  (abs (-
    ; From s1 = (p1, p2), select y from p1 = (x, y)
    (y-point (start-segment (car r)))
    ; From s2 = (p1, p2), select y from p2 = (x, y)
    (y-point (start-segment (cdr r)))
  ))
)

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

; (define (make-rectangle o b h)
;   (cons o (cons b h))
; )
; 
; ; Selectors

; (define (b r)
;   (car (cdr r))
; )

; (define (h r)
;   (cdr (cdr r))
; )


;; TEST

(area (make-rectangle (make-point 0 0) 4 5))
; 20

(perimeter (make-rectangle (make-point 0 0) 4 5))
; 18
