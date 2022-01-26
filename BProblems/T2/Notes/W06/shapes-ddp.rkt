#lang racket
(require berkeley)

; Import tag system
(require "../../P04_01/tags.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface to rest of the system 
;; (operation-type table)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(put 
  ; Operation name
  'square 
  ; Type of arg
  'area 
  ; Procedure definition
  (lambda (s) (* s s))
)

(put 'circle 'area (lambda (r) (* pi r r)))
(put 'square 'perimeter (lambda (s) (* 4 s)))
(put 'circle 'perimeter (lambda (r) (* 2 pi r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic procedures 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (area shape)
  (operate 'area shape)
)

(define (perimeter shape)
  (operate 'perimeter shape))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary procedures 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Select the procedure to apply from the operation-type table
; depending on:
; - op: operation name
; - obj: data object (actually it's type given by the tag)
(define (operate op obj) 
  (let 
    ; Obtain the procedure corresponding to the operation
    ; name and the tag of the type
    ((proc (get (type-tag obj) op)))
    ; If there exists a procedure for the pair
    (if proc
      ; Apply the procedure
      (proc (contents obj))
      ; Else raise error
      (error "Unknown operator for type")
    )
  )
)
