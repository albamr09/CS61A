#lang racket
(require berkeley)

; Load tre ADT
(require "./E02.rkt")
; Load accumulate interface
(require "../../BProblems/T2/P02_03.rkt")

; Write count-nodes, a procedure that takes a tree as argument and returns the
; total number of nodes in the tree.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; EXAMPLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Given a tree of the form (root (c1) (c2 (c21 c22)))

; > (count-nodes (root (c1) (c2 (c21 c22))))
; >> (map (root))
; << returns 1, because root is not a list
; >> (map (c1))
; << returns 1, because c1 is a leaf
; >> (map (c2))
; >>> (count-nodes (c2 (c21 c22)))
; >>>> (map (c2))
; <<<< returns 1, because c2 is not a list
; >>>> (map (c21))
; <<<< returns 1, because c21 is a leaf
; >>>> (map (c22))
; <<<< returns 1, because c22 is a leaf
; <<< 1 + 1 + 1 = 3 (accumulate on c2 and children of c2)
; << 1 + 1 + 3 = 5 (accumulate on root, c1 (c2 and children of c2))
; < 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (count-nodes tree)
  (accumulate
    +
    (map
      (lambda
        ; For each element of the list
        (node)
        ; If it is the datum or a leaf 
        (if (or (not (list? node)) (leaf? node))
          ; Return 1, because it only is one node
          1
          ; If the element is a list, then
          ; count the nodes of this subtree
          (count-nodes node)
        )
      )
      tree
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (define test-tree
;   (make-tree-node
;     'root
;     (list
;       (make-tree-node
;         'c1
;         nil
;       )
;       (make-tree-node
;         'c2
;         (list
;           (make-tree-node
;             'c21
;             (list
;               (make-tree-node
;                 'c21
;                 null
;               )
;               (make-tree-node
;                 'c22
;                 null
;               )
;             )
;           )
;           (make-tree-node
;             'c22
;             null
;           )
;         )
;       )
;     )
;   )
; )
; 
; (define test-tree1
;   (make-tree-node
;     'root
;     (list
;       (make-tree-node
;         'c1
;         nil
;       )
;       (make-tree-node
;         'c2
;         (list
;           (make-tree-node
;             'c21
;             null
;           )
;           (make-tree-node
;             'c22
;             null
;           )
;         )
;       )
;     )
;   )
; )
; 
; (count-nodes test-tree)
; ; 7
; (count-nodes test-tree1)
; ; 5

