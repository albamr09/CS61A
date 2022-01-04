#lang racket
(require berkeley)

; Load tree ADT
(require "./E02.rkt")
; Load interface
(require "../../BProblems/T2/P02_03.rkt")

; Write prune, a procedure that takes a tree as argument and returns a copy of the
; tree, but with all the leaf nodes of the original tree removed. (If the argument to
; is a one-node tree, in which the root node has no children, then should return #f
; because the result of removing the root node wouldn’t be a tree.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; EXAMPLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Given a tree of the form (root (c1) (c2 (c21 c22)))

; > (prune (root (c1) (c2 (c21 c22))))
; >> (map(root))
; << root, because it is a datum and not a leave
; >> (map((c1)))
; << null, because it is a leave
; >> (map((c2 (c21) (c22))))
; -----------------------------------------------
; >>> (prune (c2 (c21) (c22)))
; >>>> (map(c2))
; <<<< c2 because it is a datum and not a leave
; >>>> (map((c21)))
; <<<< null, because it is a leave
; >>>> (map((c22)))
; <<<< null, because it is a leave
; >>>> (filter(c2 null null)) 
; <<<< (c2)
; >>>> (make-tree-node c2 null)
; <<<< (c2)
; <<< (c2)
; -----------------------------------------------
; >> (filter (root null (c2)))
; << (root (c2))
; >> (make-tree-node root (c2))
; << (root (c2))
; < (root (c2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (not-null x)
  (not (null? x))
)

(define (prune tree)
  (if (null? tree) 
    ; If the tree is null return false
    #f
    (let
      ; List of nodes in the current level that are 
      ; not leaves
      ((non-leaves
        ; Filter the nodes that are null = nodes that are leaves
        (filter
          ; Predicate that filters null values
          not-null
          (map
            (lambda
              ; For each node of the current level of the tree
              (node)
              ; if it is a leaf (the list predicate for root, it is not a list)
              ; so the leaf procedure cannot be applied
              (if (and (list? node) (leaf? node))
                ; Return null if it is a leaf
                null
                ; Else check if this nodes is a list of nodes, datums are not
                ; lists of nodes
                (if (list? node)
                  ; if it is, analyse the nodes to prune the leaves
                  (prune node)
                  ; it not, return the node
                  node
                )
              )  
            )
            tree
          )
        )
      ))
      (make-tree-node (car non-leaves) (cdr non-leaves))
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define test-tree
  (make-tree-node
    'root
    (list
      (make-tree-node
        'c1
        (list
          (make-tree-node
            'c11
            nil
          )
          (make-tree-node
            'c12
            null
          )
        )
      )
      (make-tree-node
        'c2
        (list
          (make-tree-node
            'c22
            (list
              (make-tree-node
                'c221
                null
              )
            )
          )
        )
      )
    )
  )
)

; (trace prune)
; (prune test-tree)
; (root (c1) (c2 (c22)))




























