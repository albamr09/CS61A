#lang racket
(require berkeley)

; a. Define a procedure square-tree analogous to the square-list procedure. That is, 
; square-tree should behave as follows:

; > (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
; (1 (4 (9 16) 25) (36 49))

(define (square-tree tree)
  (map
    (lambda
      (x)
      (if (pair? x)
        ; If it is a subtree, 
        ; square the elements
        (square-tree x)
        ; Else square the root
        (square x)
      )
    )
    tree
  )
)

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
; (1 (4 (9 16) 25) (36 49))
