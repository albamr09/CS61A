#lang racket
(require berkeley)

; Import tag system
(require "./tags.rkt")
; Import selectors (rectangular and polar representation)
(require "./selectors.rkt")
; Import constructors (rectangular and polar representation)
(require "./constructors.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Complex number TAD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;
;; Generic Constructors
;;;;;

; We use the rectangular representation as the generic 
; representation

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y)
)

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a)
)


;;;;;
;; Generic Selectors
;;;;;

(define (real-part z)
  (cond 
    ; Use rectangular methods
    ((rectangular? z)
      ; Discard the tag with contents
      (real-part-rectangular (contents z))
    )
    ; Use polar methods
    ((polar? z)
      ; Discard the tag with contents
      (real-part-polar (contents z))
    )
    (else (error "Unknown type: REAL-PART" z))
  )
)

(define (imag-part z)
  (cond 
    ; Use rectangular methods
    ((rectangular? z)
      ; Discard the tag with contents
      (imag-part-rectangular (contents z))
    )
    ; Use polar methods
    ((polar? z)
      ; Discard the tag with contents
      (imag-part-polar (contents z))
    )
    (else (error "Unknown type: IMAG-PART" z))
  )
)

(define (magnitude z)
  (cond 
    ; Use rectangular methods
    ((rectangular? z)
      ; Discard the tag with contents
      (magnitude-rectangular (contents z))
    )
    ; Use polar methods
    ((polar? z)
      ; Discard the tag with contents
      (magnitude-polar (contents z))
    )
    (else (error "Unknown type: MAGNITUDE" z))
  )
)

(define (angle z)
  (cond 
    ; Use rectangular methods
    ((rectangular? z)
      ; Discard the tag with contents
      (angle-rectangular (contents z))
    )
    ; Use polar methods
    ((polar? z)
      ; Discard the tag with contents
      (angle-polar (contents z))
    )
    (else (error "Unknown type: ANGLE" z))
  )
)

;;;;;
;; Arithmetic Operations on Complex Numbers
;;;;;

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
