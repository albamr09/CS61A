#lang racket
(require berkeley)


; Import tag system
(require "../../P04_01/tags.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SHAPE TAD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Constant
(define pi 3.141592654)


;;;;;;;;;;;;;;;
;; Constructors
;;;;;;;;;;;;;;;

(define (make-square side)
  ; Determine the type of the shape
  ; by tagging it square
  (attach-tag 'square side)
)

(define (make-circle radius)
  ; Determine the type of the shape
  ; by tagging it circle
  (attach-tag 'circle radius)
)

;;;;;;;;;;;;;;;
;; Operators
;;;;;;;;;;;;;;;

(define (area shape)
  (cond 
    ; If the shape is a square
    ((eq? (type-tag shape) 'square)
      ; The area is side² (strip shape of the tag 
      ; and get the value)
      (* (contents shape) (contents shape))
    )
    ; If the shape is a circle
    ((eq? (type-tag shape) 'circle)
      ; The area is pi* side² (strip shape of the tag 
      ; and get the value)
      (* pi (contents shape) (contents shape))
    )
    (else (error "Unknown shape -- AREA"))
  )
)

(define (perimeter shape)
  (cond 
    ; If the shape is a square
    ((eq? (type-tag shape) 'square)
      ; The perimeter is the sum of the 
      ; side (remove tag with "contents")
      (* 4 (contents shape))
    )
    ((eq? (type-tag shape) 'circle)
      ; The perimeter is 2pi*side 
      ; (remove tag with "contents")
      (* 2 pi (contents shape))
    )
    (else (error "Unknown shape -- PERIMETER"))
  )
)

;; TEST

;(define square5 (make-square 5))
;
;(area square5)
;; 25
;(perimeter square5)
;; 20
;(define circle3 (make-circle 3))
;
;(area circle3)
;(perimeter circle3)
