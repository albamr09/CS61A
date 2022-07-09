#lang racket
(require berkeley)

; This takes two arguments, a symbol and a list. If the symbol 
; is not contained in the list (i.e., is not eq? to any item in 
; the list), then memq returns false. Otherwise, it returns the 
; sublist of the list beginning with the first occurrence
; of the symbol

(define (memq item x)
  (cond 
    ; If the list is emtpy, item is not on the list
    ((null? x) false)
    ; If item = first element of the list, return x
    ((eq? item (car x)) x)
    ; Else keek searching
    (else 
      ; Update x, by removing the first element
      (memq item (cdr x))
    )
  )
)
