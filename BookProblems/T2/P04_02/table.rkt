#lang racket

(require berkeley)

; Two dimensional data to store the relation between: operation names, data types and procedures

; Create
(define *the-table* (make-hash))
; Add
(define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value))
; Retrieve
(define (get key1 key2) (hash-ref *the-table* (list key1 key2) #f))

; Exports
(provide put get)
