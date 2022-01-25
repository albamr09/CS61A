#lang racket
(require berkeley)

; Import packages
(require "./rectangular.rkt")
(require "./polar.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Complex number TAD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (apply-generic op arg) (arg op))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Generic selectors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (real-part z) 
  ; op name: real-part
  ; args z (this comes from make-from-mag-ang or make-from-real-imag
  ; the procedure applied depends on which one is called)
  (apply-generic 'real-part z)
)

(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))



;;;;;;;;;;;;;;;;
;;;; TEST
;;;;;;;;;;;;;;;;

;; Complex number, rectangular representation
;(define c1 (make-from-real-imag 1 3))
;
;(real-part c1)
;; 1
;(imag-part c1)
;; 3
;(magnitude c1)
;; 3.16277
;(angle c1)
;; 71 (degrees) 1.24904571695 (rad)
;
;; Complex number, polar representation
;(define c2 (make-from-mag-ang 3.16277 1.24904571695))
;
;(real-part c2)
;; 1
;(imag-part c2)
;; 3
;(magnitude c2)
;; 3.16277
;(angle c2)
;; 71 (degrees) 1.24904571695 (rad)

