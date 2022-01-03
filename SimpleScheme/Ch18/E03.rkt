#lang racket
(require berkeley)

; Load tree ADT
(require "./E02.rkt")

; Load conventional interfaces
(require "../../BProblems/T2/P02_03.rkt")

; Write a depth procedure that takes a tree as argument and returns the largest
; number of nodes connected through parent-child links. That is, a leaf node has depth
; 1; a tree in which all the children of the root node are leaves has depth 2. Our world
; tree has depth 4 (because the longest path from the root to a leaf is, for example, world,
; country, state, city).

(define (leaf? node)
  ; Return true if the node
  ; has an empty list as children
  ; or the list is null
  (or 
    (not (pair? node))
    (null? (children node)) 
    (empty? (children node))
  )
)


(define (depth tree d)
  (cond
    ((null? tree) 0)
    ((leaf? tree) d)
    (else
      (max
        (depth (car (children tree)) (+ 1 d))
        (depth-forest (cdr (children tree)) (+ 1 d))
      )
    )
  )
)

(define (depth-forest forest d)
  (if (null? forest)
    d
    (max
      (depth (car forest) d)
      (depth-forest (cdr forest) d)
    )
  )
)

(define trivial-tree
  (make-node 
    'trivial_tree 
    (list
      (make-node 
      'child-1
        (list
          1
          2
          (make-node 
          'child-1
            (list 1 2)
          )
        )
      )
      (make-node 
      'child-1
        (list 1 2)
      )
    )
  )
)

;(children trivial-tree)
;(trace depth)
;(trace depth-forest)
(depth trivial-tree 0)

; (depth trivial-tree)
