#lang racket
(require berkeley)

; Define a procedure last-pair that returns the list that 
; contains only the last element of a given (nonempty) list

(define (last-pair lst)
  ; If is one element in the list
  ; return the list
  (if (= (count lst) 1)
    lst
    ; Else keep going though the list
    ; and remove the first element
    ; of the list by selecting everything 
    ; but the first element (cdr)
    (last-pair (cdr lst))
  )
)

(last-pair (list 23 72 149 34))
; (34)
