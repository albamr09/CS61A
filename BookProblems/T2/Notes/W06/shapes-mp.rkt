#lang racket
(require berkeley)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Message Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-square side)
  ; Anonymous function that acts
  ; as a dispatcher
  (lambda 
    ; The message is the operation name
    (message)
    (cond 
      ; Obtain the area of the square
      ((eq? message 'area) (* side side))
      ; Obtain the perimeter of the square
      ((eq? message 'perimeter) (* 4 side))
      ((eq? message 'type) 'square)
      (else (error "Unknown message"))
    )
  )
)

(define (make-circle radius)
  ; Anonymous function that acts
  ; as a dispatcher
  (lambda 
    ; The message is the operation name
    (message)
    (cond 
      ((eq? message 'area) (* pi radius radius))
      ((eq? message 'perimeter) (* 2 pi radius))
      ((eq? message 'type) 'circle)
      (else (error "Unknown message"))
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic Interfaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (operate op obj)
  ; Send "op" message to 
  ; the data object obj
  (obj op)
)

(define (area shape)
  ; Send operation name
  ; area to the dispatcher of shape
  (operate 'area shape)
)

(define (perimeter shape)
  ; Send operation name
  ; perimeter to the dispatcher of shape
  (operate 'perimeter shape)
)

;; TEST

;(define square5 (make-square 5))
;square5
;(square5 'area)
;; 25
;(square5 'perimeter)
;; 20
;(define circle3 (make-circle 3))
;circle3
;(circle3 'area)
;(circle3 'perimeter)
;(area square5)
;(area circle3)
