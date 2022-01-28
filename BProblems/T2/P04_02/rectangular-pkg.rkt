#lang racket
(require berkeley)

; Import tag system
(require "../P04_01/tags.rkt")
; Import table
(require "./table.rkt")

(define (install-rectangular-package)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Internal procedures
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;
  ; Constructors
  ;;;;;;;;;;;;;;

  ; From real and imaginaty part
  (define (make-from-real-imag x y) (cons x y))
  ; From magnitude and angle
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a)))
  )

  ;;;;;;;;;;;;;;
  ; Selectors
  ;;;;;;;;;;;;;;

  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))

  ; Convert the rectangular representation to a magnitude
  (define (magnitude z)
    (sqrt 
      (+ 
        (square (real-part z))
        (square (imag-part z))
      )
    )
  )
  ; Convert the rectangular representation to an angle
  (define (angle z)
    (atan 
      (imag-part z) 
      (real-part z)
    )
  )


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Interface to the rest of the system
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (tag x) (attach-tag 'rectangular x))

  ;;;
  ; Put is not yet defined
  ;;;

  ; (put ⟨op⟩ ⟨type⟩ ⟨item⟩) 
  ; installs the ⟨item⟩ in the table, indexed
  ; by the ⟨op⟩ and the ⟨type⟩.

  ;    operation   tag           procedure
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 
    ; Operation
    'make-from-real-imag 
    ; Tag
    'rectangular
    ; Procedure
    (lambda (x y) (tag (make-from-real-imag x y)))
  )
  (put 
    'make-from-mag-ang 
    'rectangular
    (lambda (r a) (tag (make-from-mag-ang r a)))
  )
  
  ; Finish installing
  'done
)

; Export
(provide install-rectangular-package)
