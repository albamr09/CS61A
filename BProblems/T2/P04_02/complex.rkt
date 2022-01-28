#lang racket
(require berkeley)

; Import tag system
(require "../P04_01/tags.rkt")
; Import table
(require "./table.rkt")
; Import apply
(require "./apply.rkt")
; Import packages
(require "./rectangular-pkg.rkt")
(require "./polar-pkg.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Complex number TAD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Generic constructors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-from-real-imag x y)
  ; In this case get returns an anonymous procedure
  ; which takes as input x and y (check out rectangular-pkg.rkt,
  ; how put stores a lambda function)
  ((get 'make-from-real-imag 'rectangular) x y)
)

(define (make-from-mag-ang r a)
  ; In this case get returns an anonymous procedure
  ; which takes as input r and a (check out polar-pkg.rkt,
  ; how put stores a lambda function)
  ((get 'make-from-mag-ang 'polar) r a)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Generic selectors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (real-part z) 
  ; op name: real-part
  ; args z (from this we determine the representation type)
  (apply-generic 'real-part z)
)

(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))


;;;;;;;;;;;;;;;;;;;;
;;;; TEST

;(install-rectangular-package)
;(install-polar-package)
;
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
;;
;; Complex number, polar representation
;(define c2 (make-from-mag-ang 3.16277 1.24904571695))
;;
;(real-part c2)
;; 1
;(imag-part c2)
;; 3
;(magnitude c2)
;; 3.16277
;(angle c2)
;; 71 (degrees) 1.24904571695 (rad)

