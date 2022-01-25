#lang racket
(require berkeley)

; Import tag system
(require "../P04_01/tags.rkt")

(define (install-polar-package)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Internal procedures
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;
  ; Constructors
  ;;;;;;;;;;;;;;

  ; From real and imaginaty part (needs to be converted)
  (define (make-from-real-imag x y)
    (cons 
      (sqrt (+ (square x) (square y)))
      (atan y x)
    )
  )
  ; From magnitude and angle
  (define (make-from-mag-ang r a) (cons r a))

  ;;;;;;;;;;;;;;
  ; Selectors
  ;;;;;;;;;;;;;;

  ; Convert the polar representation to a real part
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  ; Convert the polar representation to an imaginary part
  (define (imag-part z) (* (magnitude z) (sin (angle z))))

  (define (magnitude z) (car z))
  (define (angle z) (cdr z))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Interface to the rest of the system
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (tag x) (attach-tag 'polar x))

  ;;;
  ; Put is not yet defined
  ;;;
  ; Put is not yet defined
  ; (put ⟨op⟩ ⟨type⟩ ⟨item⟩) 
  ; installs the ⟨item⟩ in the table, indexed
  ; by the ⟨op⟩ and the ⟨type⟩.

  ;    operation   tag          procedure 
  ;                             internal definition
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 
    ; Operation
    'make-from-real-imag 
    ; Tag
    'polar
    ; Procedure
    (lambda (x y) (tag (make-from-real-imag x y)))
  )
  (put 
    'make-from-mag-ang 
    'polar
    (lambda (r a) (tag (make-from-mag-ang r a)))
  )
  
  ; Finish installing
  'done
)

; Export
(provide install-polar-package)
