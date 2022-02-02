#lang racket
(require berkeley)

; Import tag system
(require "../../../../BProblems/T2/P04_01/tags.rkt")
; Import table
(require "../../../../BProblems/T2/P04_02/table.rkt")

; We do not define internal procedures, given we are using primitive
; expressions as +, -, etc.

(define (install-real-package)

  ; Determine the type of x, append the tag real
  (define (tag x) (attach-tag 'real x))

  ;;;;;;;;;;;;;;;;;;;;;;;;
  ; Constructor
  ;;;;;;;;;;;;;;;;;;;;;;;;

  (put 
    ; Operation name
    'make 
    ; Argument type
    'real 
    ; Procedure
    (lambda 
      (x) 
      ; Determine the type of x
      ; by tagging it real
      ; make sure it is on decimal form
      ; by multiplying it by 1.0
      (tag (* x 1.0))
    )
  )
  
  ;;;;;;;;;;;;;;;;;;;;;;;;
  ; Arithmetic operations
  ;;;;;;;;;;;;;;;;;;;;;;;;

  (put 
    ; Operation name
    'add 
    ; Argument types
    '(real real)
    ; Procedure
    (lambda 
      (x y) 
      ; Determine the type of the result (+ x y)
      ; by tagging it real
      (tag (+ x y))
    )
  )

  (put 'sub '(real real)
    (lambda (x y) (tag (- x y)))
  )

  (put 'mul '(real real)
    (lambda (x y) (tag (* x y)))
  )

  (put 'div '(real real)
    (lambda (x y) (tag (/ x y)))
  )

  ;;;;;;;;;;;;;;;
  ;; Generic procedures
  ;;;;;;;;;;;;;;;

  (put 'equ? '(real real)
    (lambda (x y) (= x y))
  )

  (put '=zero? '(real)
    (lambda (x) (= x 0))
  )
  
  ; Finish installing
  'done
)


; Exports
(provide install-real-package)
