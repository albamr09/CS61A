#lang racket
(require berkeley)

; Import tag system
(require "../../../../BProblems/T2/P04_01/tags.rkt")
; Import table
(require "../../../../BProblems/T2/P04_02/table.rkt")

; We do not define internal procedures, given we are using primitive
; expressions as +, -, etc.

(define (install-scheme-number-package)

  ; Determine the type of x, append the tag scheme-number
  (define (tag x) (attach-tag 'scheme-number x))

  ;;;;;;;;;;;;;;;;;;;;;;;;
  ; Constructor
  ;;;;;;;;;;;;;;;;;;;;;;;;

  (put 
    ; Operation name
    'make 
    ; Argument type
    'scheme-number 
    ; Procedure
    (lambda 
      (x) 
      ; Determine the type of x
      ; by tagging it scheme-number
      (tag x)
    )
  )
  
  ;;;;;;;;;;;;;;;;;;;;;;;;
  ; Arithmetic operations
  ;;;;;;;;;;;;;;;;;;;;;;;;

  (put 
    ; Operation name
    'add 
    ; Argument types
    '(scheme-number scheme-number)
    ; Procedure
    (lambda 
      (x y) 
      ; Determine the type of the result (+ x y)
      ; by tagging it scheme-number
      (tag (+ x y))
    )
  )

  (put 'sub '(scheme-number scheme-number)
    (lambda (x y) (tag (- x y)))
  )

  (put 'mul '(scheme-number scheme-number)
    (lambda (x y) (tag (* x y)))
  )

  (put 'div '(scheme-number scheme-number)
    (lambda (x y) (tag (/ x y)))
  )

  ;;;;;;;;;;;;;;;
  ;; Generic procedures
  ;;;;;;;;;;;;;;;

  (put 'equ? '(scheme-number scheme-number)
    (lambda (x y) (= x y))
  )

  (put '=zero? '(scheme-number)
    (lambda (x) (= x 0))
  )
  
  ; Finish installing
  'done
)


; Exports
(provide install-scheme-number-package)
