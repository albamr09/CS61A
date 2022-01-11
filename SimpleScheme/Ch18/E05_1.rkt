#lang racket
(require berkeley)

; Load tree ADT
(require "./E02.rkt")

; Write prune, a procedure that takes a tree as argument and returns a copy of the
; tree, but with all the leaf nodes of the original tree removed. (If the argument to
; is a one-node tree, in which the root node has no children, then should return #f
; because the result of removing the root node wouldn’t be a tree.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; EXAMPLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Given a tree of the form (root (c1) (c2 (c21 c22)))

; > (prune (root (c1) (c2 (c21 c22))))
; >> (leaf? (root (c1) (c2 (c21 c22))))
; << #f
; >>> (prune-forest ((c1) (c2 (c21 c22))))
; -------------------------------------------- first brach
; >>>> (prune (c1))
; >>>>> (leaf? (c1))
; <<<<< #t, so return '()
; -------------------------------------------- second brach
; >>>> (prune-forest (c2 (c21 c22))))
; >>>>> (prune (c2 (c21 c22))))
; >>>>>> (leaf? (c2 (c21 c22))))
; <<<<<< #f
; ------------------------------- second branch's children
; >>>>>>> (prune-forest (c21 c22))
; >>>>>>>> (prune (c21))
; >>>>>>>>> (leaf? (c21))
; <<<<<<<<< #t, so return '()
; ------------------------------- first child, second brach, returns '()
; >>>>>>>> (prune-forest (c22))
; >>>>>>>>> (prune (c22))
; >>>>>>>>>> (leaf? (c22))
; <<<<<<<<<< #t, so return '()
; ------------------------------- second child, second brach, returns '()
; <<<<<< (make-tree-node '() '())     -- make tree of the two c2 children
; <<<<< (make-tree-node 'c2 '())      -- make tree of c2 and the pruned subtrees of children
; <<<< (make-tree-node ('c2) null)    -- make tree of c2 and the rest of the children on 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (append-tree x y)
  (cond
    ((and (null? x) (null? y)) null)
    ((null? x) (list y))
    ((null? y) (list x))
    (else
      (append x y)
    )
  )
)

(define (prune tree)
  (if (null? tree) 
    ; If the tree is null return empty list
    null
    ; It it is a leaf return empty list, to not
    ; append it to pruned tree
    (if (leaf? tree)
       null
       ; Else create a node with the datum
       ; and the pruned children
       (make-tree-node
        (datum tree)
        (prune-forest (children tree))
       )
    )
  )
)


(define (prune-forest forest)
  ; If the children are null
  ; return the empty list
  (if (null? forest) 
      null
      ; Else make a tree with the pruned version
      ; of the first child
      (append-tree
       (prune (car forest))
       ; and the pruned subtrees corresponding to the rest
       ; of the children
       (prune-forest (cdr forest))
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
;         (list
;           (make-tree-node
;             'c11
;             null
;           )
;           (make-tree-node
;             'c12
;             null
;           )
;         )
;       )
;       (make-tree-node
;         'c2
;         (list
;           (make-tree-node
;             'c22
;             (list
;               (make-tree-node
;                 'c221
;                 null
;               )
;             )
;           )
;         )
;       )
;     )
;   )
; )

;(trace append-tree)
;(trace prune)
;(trace prune-forest)
(prune test-tree)
; (root (c1) (c2 (c22)))
