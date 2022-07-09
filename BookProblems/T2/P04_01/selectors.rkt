#lang racket
(require berkeley)

;;;;;
;; Selectors
;;;;;

;;
;; Rectangular representation
;;

; It is straightforward, the complex number is of the form a + bi
; so the data structure is (real-number imaginary-number)

(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))

; Obtain magnitude from the real part and the imag-part
(define (magnitude-rectangular z)
  (sqrt 
    (+ 
      (square (real-part-rectangular z))
      (square (imag-part-rectangular z))
    )
  )
)

; Obtain angle from the real part and the imag-part
(define (angle-rectangular z)
  (atan 
    (imag-part-rectangular z)
    (real-part-rectangular z)
  )
)

;;
;; Polar representation
;;

; Obtain real part from the magnitude and the angle
(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z)))
)

; Obtain imaginary part from the magnitude and the angle
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z)))
)

; It is straightforward in the polar representation, the first element of the list
; is the magnitude and the second the angle
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))

; Exports
(provide real-part-rectangular imag-part-rectangular magnitude-rectangular angle-rectangular real-part-polar imag-part-polar magnitude-polar angle-polar)
