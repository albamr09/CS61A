#lang racket
(require berkeley)

; Import tag system
(require "../P04_01/tags.rkt")
; Import new apply generic
(require "./apply.rkt")
; Import table
(require "../P04_02/table.rkt")
(require "./table-coercion.rkt")
; Import arithmetic package
(require "../P05_01/arithmetic-pkg.rkt")

(define (install-coercion-package)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Import procedures from the arithmetic package
  ;; Installing creates the operation-type table for the 
  ;; arithmetic package
  ;; (see ../P05_01/)
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

  'done
)


; Exports
(provide install-coercion-package)

;;;;;;;;;;;;;;;;;;;;;
;;; TEST 
;;;;;;;;;;;;;;;;;;;;;

; (install-coercion-package)

; ; Get procedure to convert
; (define (scheme-number->complex n)
;   ((get-coercion 'scheme-number 'complex) n) 
; )
; 
; ; Procedure to create scheme number
; (define (make-scheme-number n)
;   ((get 'make 'scheme-number) n)
; )
; 
; ; Define scheme number n
; (define n (make-scheme-number 3))
; 
; ; Convert n to complex
; (scheme-number->complex n)
