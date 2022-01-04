#lang racket
(require berkeley)

; Load tree ADT
(require "./E02.rkt")

; Write a depth procedure that takes a tree as argument and returns the largest
; number of nodes connected through parent-child links. That is, a leaf node has depth
; 1; a tree in which all the children of the root node are leaves has depth 2. Our world
; tree has depth 4 (because the longest path from the root to a leaf is, for example, world,
; country, state, city).

(define (depth tree)
  ; Start the process
  (depth-helper tree 1)
)

(define (depth-helper tree total)
  (cond
    ; If the tree is empty, it has no depth
    ((null? tree) 0)
    ; If the current node is a leaf, return
    ; the accumulated depth
    ((leaf? tree) total)
    ; Else select the maximum 
    ; depth between the leftmost
    ; branch and the rest of the branches
    (else 
      (max
        ; Left-most branch (only one node)
        (depth-helper 
          (car (children-tree tree))
          ; Update the depth
          (+ 1 total)
        )
        ; Rest of branches (several nodes)
        (depth-forest 
          (cdr (children-tree tree))
          ; Do not update depth
          total
        )
      )
    )
  )
)

(define (depth-forest forest total)
  ; If there are no more siblings
  ; return the total depth until now
  (if (null? forest)
    total
    ; Else select the maximum 
    ; depth between the leftmost
    ; branch and the rest of the branches
    (max
      ; Left-most branch 
      (depth-helper
        (car forest) 
        ; Update depth
        (+ 1 total)
      )
      ; Rest of branches
      (depth-forest 
        (cdr forest) 
        ; Do not update depth
        total
      )
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define test-tree
;  (make-tree-node
;    'root
;    (list
;      (make-tree-node
;        'c1
;        nil
;      )
;      (make-tree-node
;        'c2
;        (list
;          (make-tree-node
;            'c21
;            null
;          )
;          (make-tree-node
;            'c22
;            null
;          )
;        )
;      )
;    )
;  )
;)
;
;(define test-tree1
;  (make-tree-node
;    'root
;    (list
;      (make-tree-node
;        'c1
;        nil
;      )
;      (make-tree-node
;        'c2
;        (list
;          (make-tree-node
;            'c21
;            null
;          )
;          (make-tree-node
;            'c22
;            (list
;              (make-tree-node
;                'c221
;                null
;              )
;              (make-tree-node
;                'c222
;                null
;              )
;            )
;          )
;        )
;      )
;    )
;  )
;)

;(trace depth)
;(trace depth-forest)
; (depth test-tree)
; 3
; (depth test-tree1)
; 4
