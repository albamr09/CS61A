#lang racket
(require berkeley)

; Read over the following code and enter them into Scheme.

;; Enter these definitions into Scheme:

; Constructor
(define (make-rational num den)
  (cons num den))

; Selectors
(define (numerator rat)
  (car rat)
)
(define (denominator rat)
  (cdr rat)
)

; Operations

(define (*rat a b)
  (make-rational 
    (* (numerator a) (numerator b))
	  (* (denominator a) (denominator b))
  )
)

(define (print-rat rat)
  (word 
    (numerator rat) 
    '/ 
    (denominator rat)
  )
)


;; Now try these:
(print-rat (make-rational 2 3))
(print-rat (*rat (make-rational 2 3) (make-rational 1 4)))

; Now define a procedure +rat to add two rational numbers, in the same style as *rat above.

(define (+rat a b)
  (make-rational
    ; Numerator
    (+ 
      (* (numerator a) (denominator b))
      (* (numerator b) (denominator a))
    )
    ; Denominator
    (* (denominator a) (denominator b))
  )
)

(print-rat (+rat 
  (make-rational 1 2)
  (make-rational 1 2)
))
; 4/4 = 2/2

(print-rat (+rat 
  (make-rational 1 2)
  (make-rational 2 3)
))

; 7/6
