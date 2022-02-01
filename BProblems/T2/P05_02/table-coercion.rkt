
#lang racket

(require berkeley)

; Two dimensional data to store the relation between two data types and the procedure to transform
; the first to the second

; Import our normal hash table
(require "../P04_02/table.rkt")

; Define the new put-coercion and get-coercion as
; the same as put and get
(define put-coercion put)
(define get-coercion get)

; Exports
(provide get-coercion put-coercion)
