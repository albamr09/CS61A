#lang racket
(require berkeley)

; Import tag system
(require "../../../../BProblems/T2/P04_01/tags.rkt")
; Import tables
(require "../../../../BProblems/T2/P04_02/table.rkt")

(define (raise-generic . args)
  (let
    ; Obtain type of the data object
    ((type-tags (map type-tag args)))
    (let 
      ; Obtain concrete raise procedure for
      ; the type (by the tower hierarchy)
      ((proc (get 'raise type-tags)))
      ; If it exists, apply it
      (if proc
        (apply proc args)
        ; Else throw error
        (error 
          "No raise for the type"
          type-tags
        )
      )
    )
  )
)

; Exports
(provide raise-generic)
