#lang racket
(require berkeley)


;;;;;;;;;;;;;;
; Constructors
;;;;;;;;;;;;;;

; From magnitude and angle
(define (make-from-mag-ang r a)
  ;;;;;;;;;;;
  ; Selectors
  ;;;;;;;;;;;
  (define (dispatch op)
    (cond 
      ((eq? op 'real-part)
        (* r (cos a))
      )
      ((eq? op 'imag-part)
        (* r (sin a))
      )
      ((eq? op 'magnitude) r)
      ((eq? op 'angle) a)
      (else (error "Unknown op: MAKE-FROM-REAL-IMAG" op))
    )
  )
  ; We return a procedure that takes
  ; the name of the procedure as argument
  dispatch
)


; Export
(provide make-from-mag-ang)
