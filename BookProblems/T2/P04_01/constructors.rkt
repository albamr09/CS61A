#lang racket
(require berkeley)

; Import tag system
(require "./tags.rkt")

;;;;;
;; Constructors (with tags for each representation)
;;;;;

; Construct complex number equal to z

;;
;; Rectangular representation
;;

; Rectangular constructor for rectangular data
(define (make-from-real-imag-rectangular x y)
  ; Just attach the tag and create the structure
  (attach-tag 'rectangular (cons x y))
)
; Rectangular constructor for polar data
(define (make-from-mag-ang-rectangular r a)
  ; Attach the tag and convert the magnitude 
  ; and angle to the real and imaginary number
  (attach-tag 
    'rectangular
    (cons (* r (cos a)) (* r (sin a)))
  )
)

;;
;; Polar representation
;;

; Polar constructor for rectangular data
(define (make-from-real-imag-polar x y)
  (attach-tag 
    ; Add the type of the represnetation
    'polar
    ; Convert rectangular representation to polar
    (cons 
      (sqrt (+ (square x) (square y)))
      (atan y x)
    )
  )
)
; Polar constructor for polar data
(define (make-from-mag-ang-polar r a)
  ; Add the type of the representation and create
  ; the structure
  (attach-tag 'polar (cons r a))
)

; Exports
(provide make-from-real-imag-rectangular make-from-mag-ang-rectangular make-from-real-imag-polar make-from-mag-ang-polar)
