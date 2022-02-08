#lang racket
(require berkeley)

; Give a Θ(n) implementation of union-set for sets 
; represented as ordered lists.


(define (union-set set1 set2)
  (cond
    ; If both are null, stop
    ((and (null? set1) (null? set2)) '())
    ; If set1 is emtpy, append the rest of set2
    ; to the union
    ((null? set1) set2)
    ; If set2 is emtpy, append the rest of set1
    ; to the union
    ((null? set2) set1)
    (else
      (let
        (
         ; Obtain first elements of both sets
         (x1 (car set1))
         (x2 (car set2))
        )
        (cond
          ; If they are equal, only insert one
          ; into union
          ((= x1 x2) 
            (cons 
              x1
              ; Continue comparing
              (union-set
                (cdr set1)
                (cdr set2)
              )
            )
          )
          ; If the element of set1 y less than the one from set2
          ; insert the lesser element, because it will not be in set2
          ((< x1 x2)
            (cons
              x1
              ; Continue comparing
              (union-set
                (cdr set1)
                set2
              )
            )
          )
          ; If the element of set2 y less than the one from set1
          ; insert the lesser element, because it will not be in set1
          (else
            (cons
              x2
              ; Continue comparing
              (union-set
                set1
                (cdr set2)
              )
            )
          )
        )
      )
    )
  )
)

(union-set (list 1 3) (list 2 3 4 8))
; (1 2 3 4 8)
