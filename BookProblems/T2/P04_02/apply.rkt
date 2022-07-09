#lang racket
(require berkeley)

; Import tag system
(require "../P04_01/tags.rkt")
; Import table
(require "./table.rkt")

; Apply the procedure associated with an op (operation name)
; and the type of the arguments (rectangular or polar)
(define (apply-generic op . args)
  (let 
    ; Obtain the representation type 
    ; from the arguments
    ((type-tags (map type-tag args)))
    (let 
      ; Obtain the procedure definition
      ; from the representation type tag
      ; and the operation name
      ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error 
          "No method for these types: APPLY-GENERIC"
          (list op type-tags)
        )
      )
    )
  )
)

; Exports 
(provide apply-generic)
