#lang racket
(require berkeley)

; Consider the problem of representing line segments in a plane. 

; Each segment is represented as a pair of points: a starting point and 
; an ending point. Define a constructor make-segment and selectors start-segment 
; and end-segment that define the representation of segments in terms of points. 

; Furthermore, a point can be represented as a pair of numbers: the x coordinate and the y coordinate. 
; Accordingly, specify a constructor make-point and selectors x-point and y-point that define this representation. 

; Finally, using your selectors and constructors, define a procedure midpoint-segment that takes a line segment as
; argument and returns its midpoint (the point whose coordinates are the average of the coordinates of the endpoints).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Segment data type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Constructor

(define (make-segment a b) 
  (cons a b)
)

; Selectors

(define (start-segment a) (car a))
(define (end-segment a) (cdr a))

; Operations

(define (midpoint-segment s)
  (make-point
    ; x: average of x's
    (/ 
      (+ 
        (x-point (start-segment s))
        (x-point (end-segment s))
      ) 
      2
    )
    ; y: average of y's
    (/ 
      (+ 
        (y-point (start-segment s))
        (y-point (end-segment s))
      ) 
      2
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Point data type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Constructor

(define (make-point a b)
  (cons a b)
)

; Selectors

(define (x-point a) (car a))
(define (y-point a) (cdr a))

; Operations

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
)


; Exports
(provide make-segment start-segment end-segment make-point x-point y-point print-point midpoint-segment)

;;; TEST

;(define p1 (make-point 1 2))
;(define p2 (make-point 5 2))
;(print-point p1)
;; (1, 2)
;
;(define s (make-segment p1 p2))
;(print-point (midpoint-segment s))
;; (3, 2)
