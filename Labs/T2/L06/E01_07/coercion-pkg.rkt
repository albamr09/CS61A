#lang racket
(require berkeley)

; Import tag system
(require "../../../../BProblems/T2/P04_01/tags.rkt")
; Import new apply generic
(require "./apply.rkt")
; Import tables
(require "../../../../BProblems/T2/P04_02/table.rkt")
(require "../../../../BProblems/T2/P05_02/table-coercion.rkt")
; Import arithmetic package
(require "./arithmetic-pkg.rkt")

; Modify apply-generic so that it doesn’t try coercion
; if the two arguments have the same type. 

(define (install-coercion-package)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Import procedures from the arithmetic package
  ;; Installing creates the operation-type table for the 
  ;; arithmetic package
  ;; (see arithmetic-pgk.rkt)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (install-arithmetic-pkg)

  ;;;;;;;;;;;;;;;;;
  ;;; Coercion Operations
  ;;;;;;;;;;;;;;;;;
  
  ; Transform a ordinary number to a complex number
  (define (scheme-number->complex n)
    ; Obtain procedure from operation table
    ((get 'make-from-real-imag 'complex) 
      ; Strip n of the tag, get the value
      (contents n) 
      ; Imaginary part is zero
      0
    )
  )

  ; Identity conversion: a type to itself
  (define (scheme-number->scheme-number n) n)
  (define (complex->complex z) z)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Interface to rest of the system
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (put-coercion 
    ; Convert from
    'scheme-number 
    ; to
    'complex
    ; Procedure that applies conversion
    scheme-number->complex
  )

  (put-coercion 
    'scheme-number 'scheme-number
    scheme-number->scheme-number
  )

  (put-coercion 
    'complex 'complex 
    complex->complex
  )

  'done
)

(install-coercion-package)


; Exports
(provide install-coercion-package)

;;;;;;;;;;;;;;;;;;;;;;;;
;; TEST

; ; Complex number (rectangular representation)
; (define (make-complex-from-real-imag x y)
;   ((get 'make-from-real-imag 'complex) x y)
; )
; 
; ; Scheme number
; (define (make-scheme-number n)
;   ((get 'make 'scheme-number) n)
; )
; 
; ; Define exponientiation operation:
; (define (exp x y) (apply-generic 'exp x y))
; 
; (define complex-num-a (make-complex-from-real-imag 0 0))
; (define complex-num-b (make-complex-from-real-imag 1 3))
; (define scheme-num-a (make-scheme-number 0))
; (define scheme-num-b (make-scheme-number 9))
; 
; ; Exponent on two integers
; (exp scheme-num-a scheme-num-b)
; ; Exponent on two complex number
; (exp complex-num-a complex-num-b)
