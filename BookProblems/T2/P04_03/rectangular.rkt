#lang racket
(require berkeley)

;;;;;;;;;;;;;;
; Constructors
;;;;;;;;;;;;;;

; From real and imaginaty part
(define (make-from-real-imag x y)
  ;;;;;;;;;;;
  ; Selectors
  ;;;;;;;;;;;
  (define (dispatch op)
    (cond 
      ((eq? op 'real-part) x)
      ((eq? op 'imag-part) y)
      ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
      ((eq? op 'angle) (atan y x))
      (else (error "Unknown op: MAKE-FROM-REAL-IMAG" op)))
  )
  ; We return a procedure that takes
  ; the name of the procedure as argument
  dispatch
)

; Export
(provide make-from-real-imag)
