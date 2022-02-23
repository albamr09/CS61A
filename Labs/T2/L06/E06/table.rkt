#lang racket

(require berkeley)

; Two dimensional data to store the relation between: operation names, data types and procedures

; Create
(define *the-table* (make-hash))
; Add
(define (put key value) (hash-set! *the-table* key value))
; Retrieve
(define (get key) (hash-ref *the-table* key #f))

; Exports
(provide put get *the-table*)