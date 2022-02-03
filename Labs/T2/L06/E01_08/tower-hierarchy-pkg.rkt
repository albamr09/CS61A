#lang racket
(require berkeley)

; Import tag system
(require "../../../../BProblems/T2/P04_01/tags.rkt")
; Import new apply generic
(require "../E01_07/apply.rkt")
; Import tables
(require "../../../../BProblems/T2/P04_02/table.rkt")
(require "../../../../BProblems/T2/P05_02/table-coercion.rkt")
; Import arithmetic package
(require "./arithmetic-pkg.rkt")

; Suppose you are designing a generic arithmetic system for dealing with the tower of types shown in
; Figure 2.25: integer, rational, real, complex. For each type (except complex), design a procedure 
; that raises objects of that type one level in the tower. Show how to install a
; generic raise operation that will work for each type (except complex).

(define (install-tower-hierarchy-package)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Import procedures from the arithmetic package
  ;; Installing creates the operation-type table for the 
  ;; arithmetic package
  ;; (see ../../../../BProblems/T2/P05_01)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (install-arithmetic-pkg)

  ;;;;;;;;;;;;;;;;;
  ;;; Coercion Operations
  ;;;;;;;;;;;;;;;;;
  

  ; Transform a ordinary number to a rational number
  (define (scheme-number->rational n)
    ; Obtain procedure from operation table
    ((get 'make 'rational) 
      ; Strip n of the tag, get the value
      (contents n) 
      ; Denominator is zero
      1
    )
  )

  ; Transform a rational number to real number
  (define (rational->real n)
    ; Obtain procedure from operation table
    ((get 'make 'real) 
      ; Strip n of the tag, get the value
      ; multiply by 1.0 to get decimal value
      (/ 
        (* (apply-generic 'numer n) 1.0)
        (apply-generic 'denom n)
      )
    )
  )

  ; Transform real number to a complex number
  (define (real->complex n)
    ; Obtain procedure from operation table
    ((get 'make-from-real-imag 'complex) 
      ; Strip n of the tag, get the value
      (contents n) 
      ; Imaginary part is zero
      0
    )
  )


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Interface to rest of the system
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (put 
    ; Operation
    'raise
    ; From
    '(scheme-number)
    ; To rational
    scheme-number->rational
  )

  (put 
    'raise
    '(rational)
    rational->real
  )

  (put 
    'raise
    '(real) 
    real->complex
  )

  'done
)

; Exports
(provide install-tower-hierarchy-package)
