#lang racket
(require berkeley)

; Import tag system
(require "../P04_01/tags.rkt")
; Import packages
(require "./rectangular-pkg.rkt")
(require "./polar-pkg.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Complex number TAD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Apply the procedure associated with an op (operation name)
; and the type of the arguments (rectangular or polar)
(define (apply-generic op . args)
  (let 
    ; Obtain the representation type 
    ; from the arguments
    ((type-tags (map type-tag args)))
    (let 
      ; Obtain the procedure definition
      ; from the representation type tag
      ; and the operation name
      ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error 
          "No method for these types: APPLY-GENERIC"
          (list op type-tags)
        )
      )
    )
  )
)

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
