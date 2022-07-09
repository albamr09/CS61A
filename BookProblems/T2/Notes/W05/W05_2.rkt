#lang racket
(require berkeley)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TREE ADT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Constructor
(define make-my-tree cons)

; Selectors
(define datum-tree car)
(define children-tree cdr)

; Other procedures

; A node is a leaf it is has no children
(define (leaf? node)
  (null? (children-tree node))
)

;; Copy the structure of a tree 
;; given by tree, but each datum in the tree
;; is replaced by a function fn
;(define (treemap fn tree)
;  (make-my-tree 
;    ; Replace root of the tree
;    (fn (datum-tree tree))
;    (map 
;      ; For each child of the root of tree
;      (lambda 
;        (t) 
;        ; Change the root of the subtree
;        ; by the value given by the funtion
;        ; fn
;        (treemap fn t)
;      )
;      (children-tree tree) 
;    )
;  )
;)

; Treemap With mutual recursion more visible 
(define (treemap fn tree)
  (make-my-tree 
    ; Replace root of the tree
    (fn (datum-tree tree))
    ; Continue replacing for all of the 
    ; children of the tree
    (forest-map fn (children-tree tree))
  )
)

(define (forest-map fn forest)
  ; If the forest is null return an empty list
  (if (null? forest)
    '()
    (cons 
      ; Else apply tree map to the first child
      (treemap fn (car forest))
      ; And apply forest map to the list of children
      ; excluding the first child
      (forest-map fn (cdr forest))
    )
  )
)

; Exports
(provide make-my-tree datum-tree children-tree leaf? treemap)
