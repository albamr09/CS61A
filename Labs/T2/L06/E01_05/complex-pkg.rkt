#lang racket
(require berkeley)

; Import tag system
(require "../../../../BProblems/T2/P04_01/tags.rkt")
; Import table
(require "../../../../BProblems/T2/P04_02/table.rkt")
; Apply generic
(require "../../../../BProblems/T2/P04_02/apply.rkt")
; Import rectangular and polar representation
(require "../../../../BProblems/T2/P04_02/rectangular-pkg.rkt")
(require "../../../../BProblems/T2/P04_02/polar-pkg.rkt")

(define (install-complex-package)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Imported procedures from rectangular and polar packages
  ;; (see ../P04_02/polar-pkg.rkt and ../P04_02/rectangular-pkg.rkt)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (install-rectangular-package)
  (install-polar-package)

  ;;;;;;;;;;;;;;;
  ;; Constructors
  ;;;;;;;;;;;;;;;

  ; Rectagular representation
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y)
  )
  
  ; Polar representation
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a)
  )

  ; Arithmetic operations
  ; Apply generic determines what
  ; procedure to call given the representation
  ; of the complex number z (rectangular or polar)

  (define (real-part z)
    (apply-generic 'real-part z)
  )

  (define (imag-part z)
    (apply-generic 'imag-part z)
  )

  (define (magnitude z)
    (apply-generic 'magnitude z)
  )

  (define (angle z)
    (apply-generic 'angle z)
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Internal procedures
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;
  ;; Arithmetic
  ;;;;;;;;;;;;;;;

  (define (add-complex z1 z2)
    (make-from-real-imag 
      (+ (real-part z1) (real-part z2))
      (+ (imag-part z1) (imag-part z2))
    )
  )

  (define (sub-complex z1 z2)
    (make-from-real-imag 
      (- (real-part z1) (real-part z2))
      (- (imag-part z1) (imag-part z2))
    )
  )

  (define (mul-complex z1 z2)
    (make-from-mag-ang 
      (* (magnitude z1) (magnitude z2))
      (+ (angle z1) (angle z2))
    )
  )

  (define (div-complex z1 z2)
    (make-from-mag-ang 
      (/ (magnitude z1) (magnitude z2))
      (- (angle z1) (angle z2))
    )
  )

  ;;;;;;;;;;;;;;;
  ;; Generic procedures
  ;;;;;;;;;;;;;;;

  (define (equ? z1 z2)
    (and
      (=
        (real-part z1)
        (real-part z2)
      )
      (=
        (imag-part z1)
        (imag-part z2)
      )
    )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Interface to rest of the system
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (tag z) (attach-tag 'complex z))
  
  ;;;;;;;;;;;;;;;
  ;; Constructors
  ;;;;;;;;;;;;;;;

  (put 
    ; Operation name
    'make-from-real-imag 
    ; Argument type tag
    'complex
    ; Determine the type of the data object result
    ; of make-from-real-imag by tagging it 
    ; complex
    (lambda (x y) (tag (make-from-real-imag x y)))
  )

  (put 'make-from-mag-ang 'complex
    (lambda (r a) (tag (make-from-mag-ang r a)))
  )

  ;;;;;;;;;;;;;;;
  ;; Arithmetic
  ;;;;;;;;;;;;;;;

  (put 
    ; Operation name
    'add 
    ; Argument type name
    '(complex complex)
    ; Determine the type of the result of 
    ; add-complex by tagging it complex
    (lambda (z1 z2) (tag (add-complex z1 z2)))
  )

  (put 'sub '(complex complex)
    (lambda (z1 z2) (tag (sub-complex z1 z2)))
  )

  (put 'mul '(complex complex)
    (lambda (z1 z2) (tag (mul-complex z1 z2)))
  )

  (put 'div '(complex complex)
    (lambda (z1 z2) (tag (div-complex z1 z2)))
  )

  ;;;;;;;;;;;;;;;
  ;; Generic procedures
  ;;;;;;;;;;;;;;;

  (put 'equ? '(complex complex)
    (lambda (z1 z2) (equ? z1 z2))
  )

  ; Finish installing
  'done
)


(provide install-complex-package)
